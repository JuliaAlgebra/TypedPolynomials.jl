one(::Type{Monomial{N, V}}) where {N, V} = Monomial{N, V}()
zero(::Type{Term{T, M}}) where {T, M} = Term{T, M}(0, M())
zero(t::TermLike) = zero(typeof(t))

# We reverse the order of comparisons here so that the result
# of x < y is equal to the result of Monomial(x) < Monomial(y)
isless(v1::AbstractVariable, v2::AbstractVariable) = name(v1) > name(v2)

isless(m1::MonomialLike, m2::MonomialLike) = isless(promote(m1, m2)...)

# Graded Lexicographic order
# First compare total degree, then lexicographic order
function isless(m1::M, m2::M) where {M <: Monomial}
    d1 = degree(m1)
    d2 = degree(m2)
    if d1 < d2
        return true
    elseif d1 > d2
        return false
    else
        for i in 1:length(variables(M))
            if m1.exponents[i] < m2.exponents[i]
                return true
            elseif m1.exponents[i] > m2.exponents[i]
                return false
            end
        end
    end
    false
end

isless(t1::Term, t2::Term) = t1.monomial < t2.monomial

function mergesorted(v1::AbstractArray, v2::AbstractArray, isless=Base.isless, combine=Base.(+))
    T = promote_type(eltype(v1), eltype(v2))
    result = Vector{T}(length(v1) + length(v2))
    i = 1
    i1 = 1
    i2 = 1
    while i1 <= length(v1) && i2 <= length(v2)
        x1 = v1[i1]
        x2 = v2[i2]
        if isless(x1, x2)
            result[i] = x1
            i1 += 1
        elseif isless(x2, x1)
            result[i] = x2
            i2 += 1
        else
            result[i] = combine(x1, x2)
            i1 += 1
            i2 += 1
        end
        i += 1
    end
    for j in i1:length(v1)
        result[i] = v1[j]
        i += 1
    end
    for j in i2:length(v2)
        result[i] = v2[j]
        i += 1
    end
    resize!(result, i - 1)
    result
end

combine(t1::Term, t2::Term) = combine(promote(t1, t2)...)
function combine(t1::T, t2::T) where {T <: Term}
    Term(t1.coefficient + t2.coefficient, t1.monomial)
end

function jointerms(terms1::AbstractArray{<:Term}, terms2::AbstractArray{<:Term})
    mergesorted(terms1, terms2, <, combine)
end

for op in [:+, :*, :-, :(==)]
    @eval $op(p1::PolynomialLike, p2::PolynomialLike) = $op(promote(p1, p2)...)
    @eval $op(p::PolynomialLike, x) = $op(promote(p, x)...)
    @eval $op(x, p::PolynomialLike) = $op(promote(x, p)...)
end

(+)(p1::TermLike, p2::TermLike) = Polynomial(p1) + Polynomial(p2)
(+)(p1::Polynomial, p2::Polynomial) = Polynomial(jointerms(p1.terms, p2.terms))

(-)(t::TermLike) = -1 * t
(-)(p1::TermLike, p2::TermLike) = Polynomial(p1) - Polynomial(p2)
(-)(p1::Polynomial, p2::Polynomial) = Polynomial(jointerms(p1.terms, (-).(p2.terms)))

(*)(v1::V, v2::V) where {V <: Variable} = Monomial{1, (V(),)}((2,))

@generated function (*)(m1::M, m2::M) where {M <: Monomial}
    vars = variables(M)
    :(Monomial{$(length(vars)), $vars}($(Expr(:tuple, [:(m1.exponents[$i] + m2.exponents[$i]) for i in 1:length(vars)]...))))
end

(*)(t1::Term, t2::Term) = Term(t1.coefficient * t2.coefficient, t1.monomial * t2.monomial)

# TODO: this is inefficient
(*)(p1::Polynomial, p2::Polynomial) = sum(p1.terms .* p2.terms.')

(*)(t::PolynomialLike) = t

(==)(v1::Variable, v2::Variable) = name(v1) == name(v2)
(==)(m1::M, m2::M) where {M <: Monomial} = variables(m1) == variables(m2) && exponents(m1) == exponents(m2)
(==)(t1::T, t2::T) where {T <: Term} = coefficient(t1) == coefficient(t2) && monomial(t1) == monomial(t2)
(==)(::TermLike, ::Void) = false
(==)(::Void, ::TermLike) = false

^(v::V, x::Integer) where {V <: Variable} = Monomial{1, (V(),)}((x,))

transpose(v::Variable) = v
transpose(m::Monomial) = m
transpose(t::Term) = Term(t.coefficient', t.monomial)
