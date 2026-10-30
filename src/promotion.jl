@pure merge(v1::Tuple{Vararg{Variable}}) = v1

@pure function merge(v1::Tuple{Vararg{Variable}}, v2::Variable)
    for i = 1:length(v1)
        if v2 === v1[i]
            return v1
        elseif v2 > v1[i]
            return tuple(v1[1:(i-1)]..., v2, v1[i:end]...)
        end
    end
    return tuple(v1..., v2)
end

@pure function merge(v1::Tuple{Vararg{Variable}}, v2::Variable, vars::Variable...)
    merge(merge(v1, v2), vars...)
end

function Base.promote_rule(::Type{M1}, ::Type{M2}) where {M1<:Monomial,M2<:Monomial}
    vars = merge(variables(M1), variables(M2)...)
    return Monomial{typeof(vars),length(vars)}
end

MP.monomial_type(::Tuple{}) = Monomial{Tuple{},0}
function MP.monomial_type(vars::Tuple{Variable,Vararg{Variable}})
    sorted = merge((first(vars),), Base.tail(vars)...)
    return Monomial{typeof(sorted),length(sorted)}
end
function MP.monomial_type(::Type{V}) where {V<:Tuple{Vararg{Variable}}}
    return monomial_type(_instantiate(V))
end
