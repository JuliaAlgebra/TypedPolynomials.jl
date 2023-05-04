# Graded Lexicographic order
# First compare total degree, then lexicographic order
function Base.isless(m1::Monomial{V}, m2::Monomial{V}) where {V}
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
function MP.grlex(m1::Monomial{V}, m2::Monomial{V}) where {V}
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
MP.grlex(m1::Monomial, m2::Monomial) = MP.grlex(promote(m1, m2)...)

(==)(::Variable{N}, ::Variable{N}) where {N} = true
(==)(::Variable, ::Variable) = false
(==)(m1::Monomial{V}, m2::Monomial{V}) where {V} = exponents(m1) == exponents(m2)

# Multiplication is handled as a special case so that we can write these
# definitions without resorting to promotion:

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
# We cannot mutate `m1` as tuples are immutables.
function MP.mapexponents_to!(::Monomial, op::F, m1::Monomial, m2::Monomial) where {F<:Function}
    return MP.mapexponents(op, m1, m2)
end
function MP.mapexponents!(op::F, m1::Monomial, m2::Monomial) where {F<:Function}
    return MP.mapexponents(op, m1, m2)
end

# The exponents are stored in a tuple, this is not mutable.
# We could remove these methods since it is the default.
MA.mutability(::Type{<:Monomial}) = MA.IsNotMutable()

^(v::V, x::Integer) where {V <: Variable} = Monomial{(V(),), 1}((x,))

# dot(v1::AbstractVector{<:TermLike}, v2::AbstractVector) = dot(v1, v2)
# dot(v1::AbstractVector, v2::AbstractVector{<:TermLike}) = dot(v1, v2)
# dot(v1::AbstractVector{<:TermLike}, v2::AbstractVector{<:TermLike}) = dot(v1, v2)

# All of these types are immutable, so there's no need to copy anything to get
# a shallow copy.
Base.copy(x::MonomialLike) = x

adjoint(v::Variable) = v
adjoint(m::Monomial) = m
