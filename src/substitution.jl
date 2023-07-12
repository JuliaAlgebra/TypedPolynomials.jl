const Substitution{Name} = Pair{<:Variable{Name}}

function MA.promote_operation(
    ::typeof(MP.substitute),
    ::Type{<:MP.AbstractSubstitutionType},
    ::Type{V},
) where {V<:Variable}
    return V
end

function MP.substitute(
    ::MP.AbstractSubstitutionType,
    v::Variable,
)
    return v
end

function MA.promote_operation(
    ::typeof(MP.substitute),
    ::Type{<:MP.AbstractSubstitutionType},
    ::Type{Variable{Name}},
    ::Type{Pair{Variable{Name},T}},
    args::Vararg{Type,N}
) where {Name,T,N}
    return T
end

function MP.substitute(
    ::MP.AbstractSubstitutionType,
    ::Variable{Name},
    s::Substitution{Name},
    ::Vararg{MP.AbstractSubstitution,N},
) where {Name,N}
    return s.second
end

function MA.promote_operation(
    ::typeof(MP.substitute),
    S::Type{<:MP.AbstractSubstitutionType},
    ::Type{V},
    ::Type{<:Pair{<:Variable}},
    args::Vararg{Type,N}
) where {V<:Variable,N}
    return MA.promote_operation(MP.substitute, S, V, args...)
end

function MP.substitute(
    s::MP.AbstractSubstitutionType,
    v::Variable,
    ::Substitution,
    args::Vararg{MP.AbstractSubstitution,N},
) where {N}
    return MP.substitute(s, v, args...)
end

# TODO remove below

_remove_variable(::Tuple{}, ::Type) = nothing
function _remove_variable(t::Tuple{V,Vararg{Variable,N}}, ::Type{V}) where {V,N}
    Base.tail(t)
end
function _remove_variable(t::Tuple{V,Vararg{Variable,N}}, ::Type{W}) where {V,W,N}
    tail = _remove_variable(Base.tail(t), W)
    if isnothing(tail)
        return
    else
        return tuple(first(t), tail...)
    end
end

_mult_monomial_type(::Type{U}, ::Tuple{}) where {U} = U
_mult_monomial_type(::Type{U}, V::Tuple) where {U} =
    MA.promote_operation(*, U, Monomial{V,length(V)})

function _promote_subs(
    ::Type{Monomial{V,N}},
    ::Type{Pair{Variable{Name},T}},
) where {V,N,Name,T}
    U = MA.promote_operation(*, T, T)
    VV = _remove_variable(V, Variable{Name})
    if isnothing(VV)
        # Variable not present
        return Monomial{V,N}
    else
        return _mult_monomial_type(U, VV)
    end
end

function MA.promote_operation(
    ::typeof(MP.substitute),
    ::Type{MP.Subs},
    ::Type{Monomial{V,N}},
    ::Type{Pair{Variable{Name},T}},
) where {V,N,Name,T}
    return _promote_subs(Monomial{V,N}, Pair{Variable{Name},T})
end

function MA.promote_operation(
    ::typeof(MP.substitute),
    ::Type{MP.Eval},
    ::Type{Monomial{V,N}},
    ::Type{Pair{Variable{Name},T}},
) where {V,N,Name,T}
    return _promote_subs(Monomial{V,N}, Pair{Variable{Name},T})
end
