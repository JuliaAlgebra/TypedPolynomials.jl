@generated function convert(::Type{Monomial{v1}}, m::Monomial) where {v1}
    args = Any[0 for v in v1]
    v2 = variables(m)
    i1 = 1
    i2 = 1
    while i1 <= length(v1) && i2 <= length(v2)
        if v1[i1] == v2[i2]
            args[i1] = :(exponents(m)[$i2])
            i1 += 1
            i2 += 1
        else
            i1 += 1
        end
    end
    if i2 <= length(v2)
        throw(InexactError())
    end
    :(Monomial{v1, $(Expr(:tuple, args...))}())
end

convert(::Type{Monomial}, v::Variable) = Monomial{(v,), (1,)}()
convert(::Type{M}, v::Variable) where {M <: Monomial} = convert(M, Monomial(v))

convert(::Type{AbstractPolynomial}, terms::Tuple{Vararg{Term}}) = Polynomial(terms)

convert(::Type{Term}, m::Monomial) = Term(1, m)
convert(::Type{Term{T, M}}, m::Monomial) where {T, M} = Term(one(T), convert(M, m))
convert(::Type{Term}, v::Variable) = Term(1, Monomial(v))
convert(::Type{Term{T, M}}, v::Variable) where {T, M} = convert(Term{T, M}, Monomial(v))
convert(::Type{Term{T, M}}, x) where {T, M} = Term{T, M}(convert(T, x))
