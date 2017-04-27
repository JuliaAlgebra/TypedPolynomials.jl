one(::Type{M}) where {M <: Monomial} = M()
one(m::MonomialLike) = one(typeof(m))
one(::Type{Term{T, M}}) where {T, M} = Term(one(T), M())
one(t::Term) = one(typeof(t))
one(::Type{P}) where {T, P <: Polynomial{T}} = Polynomial(one(T))
one(p::Polynomial) = one(typeof(p))

zero(::Type{V}) where {V <: Variable} = Term(0, Monomial(V()))
zero(::Type{M}) where {M <: Monomial} = Term(0, M())
zero(::Type{Term{T, M}}) where {T, M} = Term{T, M}(zero(T), M())
zero(::Type{Polynomial{T, A}}) where {T, A} = Polynomial{T, A}(A())
zero(t::PolynomialLike) = zero(typeof(t))

combine(t1::Term, t2::Term) = combine(promote(t1, t2)...)
combine(t1::T, t2::T) where {T <: Term} = Term(t1.coefficient + t2.coefficient, t1.monomial)
compare(t1::Term, t2::Term) = monomial(t1) < monomial(t2)

jointerms(terms1::AbstractArray{<:Term}, terms2::AbstractArray{<:Term}) = mergesorted(terms1, terms2, compare, combine)

(*)(v1::V, v2::V) where {V <: Variable} = Monomial{(V(),), 1}((2,))

function (*)(m1::Monomial{V, N}, m2::Monomial{V, N}) where {V, N}
    e1 = m1.exponents
    e2 = m2.exponents
    Monomial{V, N}(ntuple(i -> e1[i] + e2[i], Val{N}))
end

^(v::V, x::Integer) where {V <: Variable} = Monomial{(V(),), 1}((x,))

# All of these types are immutable, so there's no need to copy anything to get
# a shallow copy.
copy(x::PolynomialLike) = x
