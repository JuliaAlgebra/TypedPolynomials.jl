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

jointerms(terms1::AbstractArray{<:Term}, terms2::AbstractArray{<:Term}) = mergesorted(terms1, terms2, compare, combine)

(+)(p1::Polynomial, p2::Polynomial) = Polynomial(jointerms(terms(p1), terms(p2)))
(-)(p1::Polynomial, p2::Polynomial) = Polynomial(jointerms(terms(p1), (-).(terms(p2))))

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

function _mul(::Type{T}, p1::Polynomial, p2::Polynomial) where T<:Term
    ts = T[]
    for t1 in terms(p1)
        for t2 in terms(p2)
            push!(ts, t1 * t2)
        end
    end
    ts
end
function (*)(p1::Polynomial{S}, p2::Polynomial{T}) where {S, T}
    C = Base.promote_op(*, S, T)
    M = promote_type(monomialtype(p1), monomialtype(p2))
    MP.polynomial(_mul(termtype(M, C), p1, p2))
end

^(v::V, x::Integer) where {V <: Variable} = Monomial{(V(),), 1}((x,))

# dot(v1::AbstractVector{<:TermLike}, v2::AbstractVector) = dot(v1, v2)
# dot(v1::AbstractVector, v2::AbstractVector{<:TermLike}) = dot(v1, v2)
# dot(v1::AbstractVector{<:TermLike}, v2::AbstractVector{<:TermLike}) = dot(v1, v2)

# All of these types are immutable, so there's no need to copy anything to get
# a shallow copy.
Base.copy(x::TermLike) = x

Base.copy(p::Polynomial) = Polynomial(copy(terms(p)))

adjoint(v::Variable) = v
adjoint(m::Monomial) = m
adjoint(t::Term) = Term(adjoint(coefficient(t)), monomial(t))
adjoint(x::Polynomial) = Polynomial(adjoint.(terms(x)))
