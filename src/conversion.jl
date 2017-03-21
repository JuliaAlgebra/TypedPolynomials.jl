convert(::Type{Monomial{N, Vars}}, v::Variable) where {N, Vars} = convert(Monomial{N, Vars}, Monomial(v))
convert(T::Type{Term{T1, M1}}, x) where {T1, M1} = T(convert(T1, x), M1())
convert(::Type{Term{T, M1}}, m::Monomial) where {T, M1} = Term{T, M1}(one(T), convert(M1, m))
convert(::Type{Term{T, M}}, v::Variable) where {T, M} = Term{T, M}(one(T), convert(M, v))
convert(T::Type{Polynomial{T1, V1}}, p::Polynomial) where {T1, V1} = T(convert(V1, p.terms))
convert(T::Type{Polynomial{T1, V1}}, x) where {T1, V1} = convert(T, Polynomial(convert(T1, x)))

@generated function convert(::Type{Monomial{N1, V1}}, m::Monomial{N2, V2}) where {N1, V1, N2, V2}
    args = Any[0 for v in V1]
    i1 = 1
    i2 = 1
    while i1 <= N1 && i2 <= N2
        if V1[i1] == V2[i2]
            args[i1] = :(m.exponents[$i2])
            i2 += 1
        else
            i1 += 1
        end
    end
    if i2 < N2
        :(throw(InexactError()))
    else
        :(Monomial{N1, V1}($(Expr(:tuple, args...))))
    end
end

function convert(::Type{Term{T1, M1}}, t::Term{T2, M2}) where {T1, M1, T2, M2}
    Term{T1, M1}(convert(T1, t.coefficient), convert(M1, t.monomial))
end
