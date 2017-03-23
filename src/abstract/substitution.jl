@inline subs(v::AbstractVariable{Name}, s::Pair{<:AbstractVariable{Name}}) where {Name} = s.second
@inline subs(v::AbstractVariable{N1}, s::Pair{<:AbstractVariable{N2}}) where {N1, N2} = v

const Substitions = Tuple{Vararg{Pair{<:AbstractVariable}}}

variables(S::Type{<:Substitions}) = [variable(p) for p in S.parameters]
variable(::Type{<:Pair{V}}) where {V <: AbstractVariable} = V

@generated function subs(v::AbstractVariable, s::Substitions)
    expr = :(v)
    for (i, vartype) in enumerate(variables(s))
        if name(vartype) == name(v)
            expr = :(s[$i].second)
        end
    end
    expr
end

@generated function subs(m::AbstractMonomial{Vars}, s::Substitions) where {Vars}
    args = Expr[]
    for (i, var) in enumerate(Vars)
        push!(args, :(subs($var, s) ^ exponent(m, $i)))
    end
    Expr(:call, :(*), args...)
end

subs(t::AbstractTerm, s::Substitions) = coefficient(t) * subs(monomial(t), s)
