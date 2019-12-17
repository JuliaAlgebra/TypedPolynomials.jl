Base.one(::Type{P}) where {C, T, P <: Polynomial{C, T}} = Polynomial(one(T))
Base.one(p::Polynomial) = one(typeof(p))

Base.zero(::Type{Polynomial{C, T, A}}) where {C, T, A} = Polynomial(A())
Base.zero(t::PolynomialLike) = zero(typeof(t))

combine(t1::Term, t2::Term) = combine(promote(t1, t2)...)
combine(t1::T, t2::T) where {T <: Term} = Term(t1.coefficient + t2.coefficient, t1.monomial)
compare(t1::Term, t2::Term) = monomial(t1) > monomial(t2)

# Graded Lexicographic order
# First compare total degree, then lexicographic order
function isless(m1::Monomial{V}, m2::Monomial{V}) where {V}
    d1 = degree(m1)
    d2 = degree(m2)
    if d1 < d2
        return true
    elseif d1 > d2
        return false
    else
        return exponents(m1) < exponents(m2)
    end
end
function grlex(m1::Monomial{V}, m2::Monomial{V}) where {V}
    d1 = degree(m1)
    d2 = degree(m2)
    if d1 != d2
        return d1 - d2
    else
        if exponents(m1) == exponents(m2)
            return 0
        elseif exponents(m1) < exponents(m2)
            return -1
        else
            return 1
        end
    end
end
grlex(m1::Monomial, m2::Monomial) = grlex(promote(m1, m2)...)

jointerms(terms1::AbstractArray{<:Term}, terms2::AbstractArray{<:Term}) = mergesorted(terms1, terms2, compare, combine)
function jointerms!(output::AbstractArray{<:Term}, terms1::AbstractArray{<:Term}, terms2::AbstractArray{<:Term})
    resize!(output, length(terms1) + length(terms2))
    Sequences.mergesorted!(output, terms1, terms2, compare, combine)
end

(+)(p1::Polynomial, p2::Polynomial) = Polynomial(jointerms(terms(p1), terms(p2)))
(+)(p1::Polynomial, p2::Polynomial{<:UniformScaling}) = p1 + MP.mapcoefficientsnz(J -> J.λ, p2)
(+)(p1::Polynomial{<:UniformScaling}, p2::Polynomial) = MP.mapcoefficientsnz(J -> J.λ, p1) + p2
(+)(p1::Polynomial{<:UniformScaling}, p2::Polynomial{<:UniformScaling}) = MP.mapcoefficientsnz(J -> J.λ, p1) + p2
function MA.mutable_operate_to!(result::Polynomial, ::typeof(+), p1::Polynomial, p2::Polynomial)
    if result === p1 || result === p2
        error("Cannot call `mutable_operate_to!(output, +, p, q)` with `output` equal to `p` or `q`, call `mutable_operate!` instead.")
    end
    jointerms!(result.terms, terms(p1), terms(p2))
    result
end
function MA.mutable_operate_to!(result::Polynomial, ::typeof(*), p::Polynomial, t::AbstractTermLike)
    if iszero(t)
        MA.mutable_operate!(zero, result)
    else
        resize!(result.terms, nterms(p))
        for i in eachindex(p.terms)
            # TODO could use MA.mul_to! for indices that were presents in `result` before the `resize!`.
            result.terms[i] = p.terms[i] * t
        end
        return result
    end
end
function MA.mutable_operate_to!(result::Polynomial, ::typeof(*), t::AbstractTermLike, p::Polynomial)
    if iszero(t)
        MA.mutable_operate!(zero, result)
    else
        resize!(result.terms, nterms(p))
        for i in eachindex(p.terms)
            # TODO could use MA.mul_to! for indices that were presents in `result` before the `resize!`.
            result.terms[i] = t * p.terms[i]
        end
        return result
    end
end
(-)(p1::Polynomial, p2::Polynomial) = Polynomial(jointerms(terms(p1), (-).(terms(p2))))
(-)(p1::Polynomial, p2::Polynomial{<:UniformScaling}) = p1 - MP.mapcoefficientsnz(J -> J.λ, p2)
(-)(p1::Polynomial{<:UniformScaling}, p2::Polynomial) = MP.mapcoefficientsnz(J -> J.λ, p1) - p2
(-)(p1::Polynomial{<:UniformScaling}, p2::Polynomial{<:UniformScaling}) = MP.mapcoefficientsnz(J -> J.λ, p1) - p2
function MA.mutable_operate!(op::Union{typeof(+), typeof(-)}, p::Polynomial{T}, q::Polynomial) where T
    get1(i) = p.terms[i]
    function get2(i)
        t = q.terms[i]
        Term(MA.scaling_convert(T, MA.copy_if_mutable(t.coefficient)), MA.copy_if_mutable(t.monomial))
    end
    set(i, t::Term) = p.terms[i] = t
    push(t::Term) = push!(p.terms, t)
    compare_monomials(t::Term, j::Int) = grlex(q.terms[j].monomial, t.monomial)
    compare_monomials(i::Int, j::Int) = compare_monomials(get1(i), j)
    combine(i::Int, j::Int) = p.terms[i] = Term(MA.operate!(op, p.terms[i].coefficient, q.terms[j].coefficient), p.terms[i].monomial)
    combine(t::Term, j::Int) = Term(MA.operate!(op, t.coefficient, q.terms[j].coefficient), t.monomial)
    resize(n) = resize!(p.terms, n)
    # We can modify the coefficient since it's the result of `combine`.
    keep(t::Term) = !MA.iszero!(t.coefficient)
    keep(i::Int) = !MA.iszero!(p.terms[i].coefficient)
    MP.polynomial_merge!(
        nterms(p), nterms(q), get1, get2, set, push,
        compare_monomials, combine, keep, resize
    )
    return p
end

(==)(::Variable{N}, ::Variable{N}) where {N} = true
(==)(::Variable, ::Variable) = false
(==)(m1::Monomial{V}, m2::Monomial{V}) where {V} = exponents(m1) == exponents(m2)

# Multiplication is handled as a special case so that we can write these
# definitions without resorting to promotion:
MP.multconstant(α, v::Monomial) = Term(α, v)
MP.multconstant(α, v::Variable) = Term(α, Monomial(v))

(*)(v1::V, v2::V) where {V <: Variable} = Monomial{(V(),), 1}((2,))
(*)(v1::Variable, v2::Variable) = (*)(promote(v1, v2)...)

function MP.divides(m1::Monomial{V, N}, m2::Monomial{V, N}) where {V, N}
    reduce((d, exp) -> d && (exp[1] <= exp[2]), zip(m1.exponents, m2.exponents), init=true)
end
MP.divides(m1::Monomial, m2::Monomial) = divides(promote(m1, m2)...)
function MP.mapexponents(op, m1::M, m2::M) where M<:Monomial
    M(map(op, m1.exponents, m2.exponents))
end
MP.mapexponents(op, m1::Monomial, m2::Monomial) = mapexponents(op, promote(m1, m2)...)
#function MP.mapexponents_to!(output::M, op, m1::M, m2::M) where M<:Monomial
#    map!(op, output.exponents, m1.exponents, m2.exponents)
#    return output
#end
#function MP.mapexponents!(op, m1::M, m2::M) where M<:Monomial
#    map!(op, m1.exponents, m1.exponents, m2.exponents)
#    return m1
#end

function MA.mutable_operate_to!(output::Polynomial, ::typeof(*), p::Polynomial, q::Polynomial)
    empty!(output.terms)
    MP.mul_to_terms!(output.terms, p, q)
    sort!(output.terms, lt=(>))
    MP.uniqterms!(output.terms)
    return output
end
function MA.mutable_operate!(::typeof(*), p::Polynomial, q::Polynomial)
    return MA.mutable_operate_to!(p, *, MA.mutable_copy(p), q)
end

function MA.mutable_operate!(::typeof(zero), p::Polynomial)
    empty!(p.terms)
    return p
end
function MA.mutable_operate!(::typeof(one), p::Polynomial{T}) where T
    if isempty(p.terms)
        push!(p.terms, constantterm(one(T), p))
    else
        t = p.terms[1]
        p.terms[1] = Term(MA.one!(coefficient(t)), constantmonomial(t))
        resize!(p.terms, 1)
    end
    return p
end

# The exponents are stored in a tuple, this is not mutable.
# We could remove these methods since it is the default.
MA.mutability(::Type{<:Monomial}) = MA.NotMutable()
MA.mutability(::Type{<:Term}) = MA.NotMutable()
# The polynomials can be mutated.
MA.mutability(::Type{<:Polynomial}) = MA.IsMutable()

^(v::V, x::Integer) where {V <: Variable} = Monomial{(V(),), 1}((x,))

# dot(v1::AbstractVector{<:TermLike}, v2::AbstractVector) = dot(v1, v2)
# dot(v1::AbstractVector, v2::AbstractVector{<:TermLike}) = dot(v1, v2)
# dot(v1::AbstractVector{<:TermLike}, v2::AbstractVector{<:TermLike}) = dot(v1, v2)

# All of these types are immutable, so there's no need to copy anything to get
# a shallow copy.
Base.copy(x::TermLike) = x

# Terms are not mutable under the MutableArithmetics API
MA.mutable_copy(p::Polynomial) = Polynomial([Term(MA.copy_if_mutable(t.coefficient), t.monomial) for t in terms(p)])
Base.copy(p::Polynomial) = MA.mutable_copy(p)

adjoint(v::Variable) = v
adjoint(m::Monomial) = m
adjoint(t::Term) = Term(adjoint(coefficient(t)), monomial(t))
adjoint(x::Polynomial) = Polynomial(adjoint.(terms(x)))

function MP.mapcoefficientsnz_to!(output::Polynomial, f::Function, p::Polynomial)
    resize!(output.terms, nterms(p))
    for i in eachindex(p.terms)
        t = p.terms[i]
        output.terms[i] = Term(f(coefficient(t)), monomial(t))
    end
    return output
end
function MP.mapcoefficientsnz_to!(output::Polynomial, f::Function, p::AbstractPolynomialLike)
    return MP.mapcoefficientsnz_to!(output, f, polynomial(p))
end
