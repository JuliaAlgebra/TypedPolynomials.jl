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

@pure function merge(v1::Tuple{Vararg{Variable}}, v2::Variable)
    for i in 1:length(v1)
        if v2 == v1[i]
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
    # _promote_monomial(M1, M2)
end

function promote_rule(::Type{Term{T, M2}}, ::Type{M1}) where {M1 <: Monomial, T, M2 <: Monomial}
    Term{T, promote_type(M1, M2)}
end
promote_rule(M::Type{<:Monomial}, T::Type{<:Term}) = promote_rule(T, M)

function _promote_term(::Type{Term{T1, M1}}, ::Type{Term{T2, M2}}) where {T1, M1, T2, M2}
    Term{promote_type(T1, T2), promote_type(M1, M2)}
end

promote_rule(T1::Type{<:Term}, T2::Type{<:Term}) = _promote_term(T1, T2)

function promote_rule(::Type{<:Polynomial{T2}}, ::Type{T1}) where {T1 <: TermLike, T2 <: Term}
    T = promote_type(T1, T2)
    Polynomial{T, Vector{T}}
end
promote_rule(T::Type{<:Term}, P::Type{<:Polynomial}) = promote_rule(P, T)
promote_rule(T::Type{<:Monomial}, P::Type{<:Polynomial}) = promote_rule(P, T)
promote_rule(T::Type{<:Variable}, P::Type{<:Polynomial}) = promote_rule(P, T)

function promote_rule(::Type{Polynomial{T2, SVector{N, T2}}}, ::Type{T1}) where {T1 <: TermLike, T2 <: Term, N}
    T = promote_type(T1, T2)
    Polynomial{T, SVector{N, T}}
end

function promote_rule(::Type{<:Polynomial{T1}}, ::Type{<:Polynomial{T2}}) where {T1 <: TermLike, T2 <: TermLike}
    T = promote_type(T1, T2)
    Polynomial{T, Vector{T}}
end

function promote_rule(::Type{Polynomial{T1, SVector{1, T1}}}, ::Type{Polynomial{T2, SVector{1, T2}}}) where {T1, T2}
    T = promote_type(T1, T2)
    Polynomial{T, SVector{1, T}}
end

function promote_rule(::Type{V}, ::Type{S}) where {S, V <: Variable}
    Term{S, Monomial{(V(),), 1}}
end

function promote_rule(::Type{M}, ::Type{S}) where {S, M <: Monomial}
    Term{S, M}
end

function _promote_any_term(::Type{S}, ::Type{Term{T, M}}) where {S, T, M <: Monomial}
    Term{promote_type(S, T), M}
end

promote_rule(t::Type{<:Term}, s) = _promote_any_term(s, t)

function promote_rule(::Type{<:Polynomial{T}}, ::Type{S}) where {S, T <: TermLike}
    R = promote_type(S, T)
    Polynomial{R, Vector{R}}
end
