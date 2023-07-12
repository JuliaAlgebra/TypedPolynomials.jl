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
