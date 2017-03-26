one(::Type{Monomial{V, N}}) where {N, V} = Monomial{V, N}()
zero(::Type{Term{T, M}}) where {T, M} = Term{T, M}(0, M())
zero(t::TermLike) = zero(typeof(t))

combine(t1::Term, t2::Term) = combine(promote(t1, t2)...)
combine(t1::T, t2::T) where {T <: Term} = Term(t1.coefficient + t2.coefficient, t1.monomial)

jointerms(terms1::AbstractArray{<:Term}, terms2::AbstractArray{<:Term}) = mergesorted(terms1, terms2, <, combine)

(*)(v1::V, v2::V) where {V <: Variable} = Monomial{(V(),), 1}((2,))

@generated function (*)(m1::M, m2::M) where {M <: Monomial}
    vars = variables(M)
    :(Monomial{$vars, $(length(vars))}($(Expr(:tuple, [:(m1.exponents[$i] + m2.exponents[$i]) for i in 1:length(vars)]...))))
end

^(v::V, x::Integer) where {V <: Variable} = Monomial{(V(),), 1}((x,))
