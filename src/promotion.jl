# We mark `isless()` as `@pure` because it's used in computing the correct
# (sorted) monomial order during type promotion, so its results must be
# assumed to be hyper-pure. Overloading this method in user code would be
# a bad idea. 
@pure function isless(::Type{V1}, ::Type{V2}) where {V1 <: Variable, V2 <: Variable}
    name(V1) < name(V2)
end

function promote_rule(::Type{V1}, ::Type{V2}) where {V1 <: Variable, V2 <: Variable}
    if V1 < V2
        Monomial{(V1(), V2()), 2}
    else
        Monomial{(V2(), V1()), 2}
    end
end

function promote_rule(::Type{M}, ::Type{V}) where {V <: Variable, M <: Monomial}
    promote_rule(Monomial{(V(),), 1}, M)
end
promote_rule(V::Type{<:Variable}, M::Type{<:Monomial}) = promote_rule(M, V)

function promote_rule(::Type{Term{T, M}}, ::Type{V}) where {V <: Variable, T, M <: Monomial}
    Term{T, promote_type(V, M)}
end
promote_rule(V::Type{<:Variable}, T::Type{<:Term}) = promote_rule(T, V)

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

function promote_rule(::Type{Term{T, M2}}, ::Type{M1}) where {M1 <: Monomial, T, M2 <: Monomial}
    Term{T, promote_type(M1, M2)}
end
promote_rule(M::Type{<:Monomial}, T::Type{<:Term}) = promote_rule(T, M)

promote_rule(::Type{Term{T1, M1}}, ::Type{Term{T2, M2}}) where {T1, M1 <: Monomial, T2, M2 <: Monomial} = Term{promote_type(T1, T2), promote_type(M1, M2)}

function promote_rule(::Type{<:Polynomial{<:Any, T2}}, ::Type{T1}) where {T1 <: TermLike, T2 <: Term}
    T = promote_type(T1, T2)
    Polynomial{coefficienttype(T), T, Vector{T}}
end
promote_rule(T::Type{Term{T1, M1}}, P::Type{<:Polynomial}) where {T1, M1 <: Monomial} = promote_rule(P, T)
promote_rule(T::Type{<:Monomial}, P::Type{<:Polynomial}) = promote_rule(P, T)
promote_rule(T::Type{<:Variable}, P::Type{<:Polynomial}) = promote_rule(P, T)

function promote_rule(::Type{<:Polynomial{<:Any, T1}}, ::Type{<:Polynomial{<:Any, T2}}) where {T1 <: TermLike, T2 <: TermLike}
    T = promote_type(T1, T2)
    Polynomial{coefficienttype(T), T, Vector{T}}
end

function MP.promote_rule_constant(::Type{S}, ::Type{V}) where {S, V <: Variable}
    Term{S, Monomial{(V(),), 1}}
end

function MP.promote_rule_constant(::Type{S}, ::Type{M}) where {S, M <: Monomial}
    Term{S, M}
end

MP.promote_rule_constant(::Type{S}, t::Type{Term{T, M}}) where {T, M <: Monomial, S} = Term{promote_type(S, T), M}

function MP.promote_rule_constant(::Type{S}, ::Type{<:Polynomial{<:Any, T}}) where {S, T <: TermLike}
    R = promote_type(S, T)
    Polynomial{coefficienttype(R), R, Vector{R}}
end
