# We mark `isless()` as `@pure` because it's used in computing the correct
# (sorted) monomial order during type promotion, so its results must be
# assumed to be hyper-pure. Overloading this method in user code would be
# a bad idea.
@pure function isless(::Type{V1}, ::Type{V2}) where {V1 <: Variable, V2 <: Variable}
    name(V1) < name(V2)
end

# We should be able to get rid of this method as the fallback just
# redirects to `promote_rule(monomial_type(V1), monomial_type(V2))`
# but we should check first the implications with this `@pure` thing.
function promote_rule(::Type{V1}, ::Type{V2}) where {V1 <: Variable, V2 <: Variable}
    if V1 < V2
        Monomial{(V1(), V2()), 2}
    else
        Monomial{(V2(), V1()), 2}
    end
end

function _promote_monomial_noncommutative(::Type{Monomial{V1, N1}}, ::Type{Monomial{V2, N2}}) where {N1, V1, N2, V2}
    if V1 > V2
        _promote_monomial(Monomial{V2, N2}, Monomial{V1, N1})
    else
        varnames = shortest_common_supersequence(name.(V1), name.(V2))
        vars = Expr(:tuple, [Expr(:call, Expr(:curly, :Variable, Expr(:quote, n))) for n in varnames]...)
        quote
            Monomial{$vars, $(length(varnames))}
        end
    end
end

@pure merge(v1::Tuple{Vararg{Variable}}) = v1

@pure function merge(v1::Tuple{Vararg{Variable}}, v2::Variable)
    for i in 1:length(v1)
        if v2 === v1[i]
            return v1
        elseif v2 > v1[i]
            return tuple(v1[1:i-1]..., v2, v1[i:end]...)
        end
    end
    return tuple(v1..., v2)
end

@pure function merge(v1::Tuple{Vararg{Variable}}, v2::Variable, vars::Variable...)
    merge(merge(v1, v2), vars...)
end

@pure function promote_rule(::Type{<:Monomial{V1}}, ::Type{<:Monomial{V2}}) where {V1, V2}
    vars = merge(V1, V2...)
    Monomial{vars, length(vars)}
end
