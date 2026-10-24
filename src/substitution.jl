function MA.promote_operation(
    ::typeof(MP.substitute),
    ::Type{S},
    ::Type{M},
    args::Vararg{Type,N},
) where {S<:MP.AbstractSubstitutionType,M<:MonomialLike,N}
    return Base.promote_op(MP.substitute, S, M, Tuple{args...})
end
