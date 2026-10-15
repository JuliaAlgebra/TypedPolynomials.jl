function Base.convert(::Type{M}, v::Variable) where {M<:Monomial}
    return convert(M, monomial(v))
end

function Base.convert(::Type{A}, a::SA.StarAlgebra) where {A<:_Algebra}
    return MP.algebra(MP.FullBasis{MP.Monomial}(variables(monomial_type(A))))
end
Base.convert(::Type{A}, a::A) where {A<:_Algebra} = a
Base.convert(::Type{Monomial}, v::Variable) = monomial(v)
Base.convert(::Type{Monomial}, m::Monomial) = m
Base.convert(::Type{M}, m::M) where {M<:Monomial} = m

function Base.convert(::Type{M}, m::Monomial) where {M<:Monomial}
    vars = variables(M)
    for v in variables(m)
        v in vars || throw(InexactError(:convert, M, m))
    end
    exps = map(vars) do v
        i = findfirst(isequal(v), variables(m))
        return isnothing(i) ? 0 : exponents(m)[i]
    end
    return M(exps)
end
