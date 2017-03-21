# promote_rule(::Type{S}, t::Type{<:PolynomialLike}) where {S} = promote_rule(t, S)

@generated function promote_rule(::Type{V1}, ::Type{V2}) where {V1 <: Variable, V2 <: Variable}
    if V1 < V2
        :(Monomial{2, (V1(), V2())})
    else
        :(Monomial{2, (V2(), V1())})
    end
end

function promote_rule(::Type{M}, ::Type{V}) where {V <: Variable, M <: Monomial}
    promote_rule(Monomial{1, (V(),)}, M)
end
promote_rule(V::Type{<:Variable}, M::Type{<:Monomial}) = promote_rule(M, V)

function promote_rule(::Type{Term{T, M}}, ::Type{V}) where {V <: Variable, T, M <: Monomial}
    Term{T, promote_type(V, M)}
end
promote_rule(V::Type{<:Variable}, T::Type{<:Term}) = promote_rule(T, V)

function _promote_monomial_noncommutative(::Type{Monomial{N1, V1}}, ::Type{Monomial{N2, V2}}) where {N1, V1, N2, V2}
    if V1 > V2
        _promote_monomial(Monomial{N2, V2}, Monomial{N1, V1})
    else
        varnames = shortest_common_supersequence(name.(V1), name.(V2))
        vars = Expr(:tuple, [Expr(:call, Expr(:curly, :Variable, Expr(:quote, n))) for n in varnames]...)
        quote
            Monomial{$(length(varnames)), $vars}
        end
    end
end

function _promote_monomial(::Type{Monomial{N1, V1}}, ::Type{Monomial{N2, V2}}) where {N1, V1, N2, V2}
    varnames = union(name.(V1), name.(V2))
    sort!(varnames)
    vars = Expr(:tuple, [Expr(:call, Expr(:curly, :Variable, Expr(:quote, n))) for n in varnames]...)
    quote
        Monomial{$(length(varnames)), $vars}
    end
end

@generated function promote_rule(::Type{M1}, ::Type{M2}) where {M1 <: Monomial, M2 <: Monomial}
    _promote_monomial(M1, M2)
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

@generated function promote_rule(::Type{Polynomial{T1, SVector{N1, T1}}}, ::Type{Polynomial{T2, SVector{N2, T2}}}) where {T1, N1, T2, N2}
    T = promote_type(T1, T2)
    :(Polynomial{$T, SVector{$(max(N1, N2)), $T}})
end

function promote_rule(::Type{V}, ::Type{S}) where {S, V <: Variable}
    Term{S, Monomial{1, (V(),)}}
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
