const Substitution{Name} = Pair{<:Variable{Name}}

## Variables
MP.substitute(st::MP.AbstractSubstitutionType, v::Variable{Name}, s::Substitution{Name}) where {Name} = s.second
MP.substitute(st::MP.AbstractSubstitutionType, v::Variable, s::Substitution) = v
# subs(x, x=>y, y=>1) should be y, not 1 so as soon as we see the right variable, we stop, in subs(x, x=>y, x=>1), "x=>1" is ignored.
MP.substitute(st::MP.AbstractSubstitutionType, v::Variable{Name}, s::Substitution{Name}, ::MP.AbstractSubstitution...) where {Name} = s.second

_remove_variable(t::Tuple{}, ::Type) = t
function _remove_variable(t::Tuple{V,Vararg{Variable,N}}, ::Type{V}) where {V,N}
    Base.tail(t)
end
function _remove_variable(t::Tuple{V,Vararg{Variable,N}}, ::Type{W}) where {V,W,N}
    tuple(first(t), _remove_variable(Base.tail(t), W)...)
end

_mult_monomial_type(::Type{U}, ::Tuple{}) where {U} = U
_mult_monomial_type(::Type{U}, V::Tuple) where {U} = MA.promote_operation(*, U, Monomial{V,length(V)})

function MA.promote_operation(::typeof(MP.substitute), ::Type{MP.Subs}, ::Type{Monomial{V,N}}, ::Type{Pair{Variable{Name},T}}) where {V,N,Name,T}
    U = MA.promote_operation(*, T, T)
    VV = _remove_variable(V, Variable{Name})
    return _mult_monomial_type(U, VV)
end
