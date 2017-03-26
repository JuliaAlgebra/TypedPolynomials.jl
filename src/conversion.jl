convert(::Type{M}, v::Variable) where {Vars, M <: Monomial{Vars}} = convert(M, Monomial(v))
convert(T::Type{Term{T1, M1}}, x) where {T1, M1} = T(convert(T1, x), M1())
convert(::Type{Term{T, M1}}, m::Monomial) where {T, M1} = Term{T, M1}(one(T), convert(M1, m))
convert(::Type{Term{T, M}}, v::Variable) where {T, M} = Term{T, M}(one(T), convert(M, v))
convert(T::Type{Polynomial{T1, V1}}, p::Polynomial) where {T1, V1} = T(convert(V1, p.terms))
convert(T::Type{Polynomial{T1, V1}}, x) where {T1, V1} = convert(T, Polynomial(convert(T1, x)))

@generated function convert(::Type{Monomial{V1}}, m::Monomial) where {V1}
    :(convert(Monomial{V1, $(length(V1))}, m))
end

@generated function convert(::Type{Monomial{V1, N1}}, m::Monomial{V2}) where {V1, N1, V2}
    args = Any[0 for v in V1]
    i1 = 1
    i2 = 1
    while i1 <= length(V1) && i2 <= length(V2)
        if V1[i1] == V2[i2]
            args[i1] = (args[i1] == 0) ? :(exponent(m, $i2)) : :($(args[i1]) + exponent(m, $i2))
            i2 += 1
        else
            i1 += 1
        end
    end
    if i2 < length(V2)
        :(throw(InexactError()))
    else
        :(Monomial{V1, $(length(V1))}($(Expr(:tuple, args...))))
    end
end

function convert(::Type{Term{T1, M1}}, t::Term{T2, M2}) where {T1, M1, T2, M2}
    Term{T1, M1}(convert(T1, t.coefficient), convert(M1, t.monomial))
end

convert(::Type{AbstractTerm}, coeff, mono::Monomial) = Term(coeff, mono)
convert(::Type{AbstractPolynomial}, t::TermLike) = Polynomial(t)
convert(::Type{AbstractPolynomial}, v::AbstractVector{<:Term}) = Polynomial(v)
