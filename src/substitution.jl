const Substitution{Name} = Pair{<:Variable{Name}}

## Variables
MP.substitute(st::MP.AbstractSubstitutionType, v::Variable{Name}, s::Substitution{Name}) where {Name} = s.second
MP.substitute(st::MP.AbstractSubstitutionType, v::Variable, s::Substitution) = v
# subs(x, x=>y, y=>1) should be y, not 1 so as soon as we see the right variable, we stop, in subs(x, x=>y, x=>1), "x=>1" is ignored.
MP.substitute(st::MP.AbstractSubstitutionType, v::Variable{Name}, s::Substitution{Name}, ::MP.AbstractSubstitution...) where {Name} = s.second

_remove_variable(t::Tuple{}, ::Type) = t
_remove_variable(t::Tuple{V,Vararg{Variable,N}}, ::Type{V}) where {V,N} = Base.tail(t)
_remove_variable(t::Tuple{V,Vararg{Variable,N}}, ::Type{W}) where {V,W,N} = tuple(first(t), _remove_variable(Base.tail(t), V)...)

function MA.promote_operation(::typeof(MP.substitute), ::Type{MP.Subs}, ::Type{Monomial{V,N}}, ::Type{Pair{Variable{Name},T}}) where {V,N,Name,T}
    U = MA.promote_operation(^, T, Int)
    VV = _remove_variable(V, Variable{Name})
    return Term{U,Monomial{VV,length(VV)}}
end
