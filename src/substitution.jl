@inline subs(v::Variable{Name}, s::Pair{Variable{Name}}) where {Name} = s.second
@inline subs(v::Variable{N1}, s::Pair{Variable{N2}}) where {N1, N2} = v

@generated function subs(v::V, s::NTuple{N, Pair{<:Variable}}) where {V <: Variable, N}
    expr = :(v)
    for (i, p) in enumerate(s.parameters)
        vartype = p.parameters[1]
        if vartype == V
            expr = :(s[$i].second)
        end
    end
    expr
end

@generated function subs(m::Monomial{N, V}, s::NTuple{M, Pair{<:Variable}}) where {N, V, M}
    args = Expr[]
    for (i, var) in enumerate(V)
        push!(args, :(subs($var, s) ^ m.exponents[$i]))
    end
    Expr(:call, :(*), args...)
end

function subs(t::Term, s::NTuple{N, Pair{<:Variable}}) where {N}
    t.coefficient * subs(t.monomial, s)
end

function remaining_variables(p::Type{<:Polynomial}, s::Type{<:NTuple{N, Pair{<:Variable}}}) where N
    subs_vars = Set{Symbol}([name(p.parameters[1]) for p in s.parameters])
    poly_vars = Set{Symbol}(collect(name.(variables(p))))
    setdiff(poly_vars, subs_vars)
end

function subs_complete(p::Polynomial{Term{T, M}}, s::NTuple{N, Pair{<:Variable}}) where {T, M, N}
    result = zero(Base.promote_op(+, T, T))
    for term in p.terms
        result += subs(term, s)
    end
    result
end

function subs_partial(p::Polynomial, s::NTuple{N, Pair{<:Variable}}) where N
    newterms = [subs(t, s) for t in p.terms]
    sort!(newterms)
    Polynomial(newterms)
end


@generated function subs(p::Polynomial, s::NTuple{N, Pair{<:Variable}}) where N
    v = remaining_variables(p, s)
    if isempty(v)
        quote
            subs_complete(p, s)
        end
    else
        quote
            subs_partial(p, s)
        end
    end
end

subs(v::PolynomialLike, s::Vararg{<:Pair{<:Variable}}) = subs(v, s)
subs(x, s::Vararg{Pair{<:Variable}}) = x

(p::Polynomial)(s::Vararg{Pair{<:Variable}}) = subs(p, s)
(p::Term)(s::Vararg{Pair{<:Variable}}) = subs(p, s)
(p::Monomial)(s::Vararg{Pair{<:Variable}}) = subs(p, s)
(p::Variable)(s::Vararg{Pair{<:Variable}}) = subs(p, s)
