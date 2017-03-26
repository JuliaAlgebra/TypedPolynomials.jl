@generated function promote_rule(::Type{V1}, ::Type{V2}) where {V1 <: Variable, V2 <: Variable}
    if name(V1) < name(V2)
        :(Monomial{(V1(), V2())})
    else
        :(Monomial{(V2(), V1())})
    end
end

promote_rule(::Type{M}, ::Type{V}) where {V <: Variable, M <: Monomial} = promote_rule(Monomial{(V(),), (1,)}, M)
promote_rule(V::Type{<:Variable}, M::Type{<:Monomial}) = promote_rule(M, V)

function _promote_monomial(::Type{<:Monomial{V1}}, ::Type{<:Monomial{V2}}) where {V1, V2}
    varnames = union(name.(V1), name.(V2))
    sort!(varnames)
    vars = Expr(:tuple, [Expr(:call, Expr(:curly, :Variable, Expr(:quote, n))) for n in varnames]...)
    :(Monomial{$vars})
end

@generated function promote_rule(::Type{M1}, ::Type{M2}) where {M1 <: Monomial, M2 <: Monomial}
    _promote_monomial(M1, M2)
end

promote_rule(::Type{Term{T, M}}, ::Type{V}) where {V <: Variable, T, M <: Monomial} = Term{T, promote_type(V, M)}
promote_rule(V::Type{<:Variable}, T::Type{<:Term}) = promote_rule(T, V)

function promote_rule(::Type{Term{T, M2}}, ::Type{M1}) where {M1 <: Monomial, T, M2 <: Monomial}
    Term{T, promote_type(M1, M2)}
end
promote_rule(M::Type{<:Monomial}, T::Type{<:Term}) = promote_rule(T, M)

function _promote_term(::Type{Term{T1, M1}}, ::Type{Term{T2, M2}}) where {T1, M1, T2, M2}
    Term{promote_type(T1, T2), promote_type(M1, M2)}
end

promote_rule(T1::Type{<:Term}, T2::Type{<:Term}) = _promote_term(T1, T2)

function promote_rule(::Type{V}, ::Type{S}) where {S, V <: Variable}
    Term{S, Monomial{(V(),)}}
end

function promote_rule(::Type{M}, ::Type{S}) where {S, M <: Monomial}
    Term{S, M}
end

function _promote_any_term(::Type{S}, ::Type{Term{T, M}}) where {S, T, M <: Monomial}
    Term{promote_type(S, T), M}
end

promote_rule(t::Type{<:Term}, s) = _promote_any_term(s, t)
