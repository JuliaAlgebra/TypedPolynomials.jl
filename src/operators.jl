one(::Type{Monomial{V, N}}) where {N, V} = Monomial{V, N}()
zero(::Type{Term{T, M}}) where {T, M} = Term{T, M}(0, M())
zero(t::TermLike) = zero(typeof(t))

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
