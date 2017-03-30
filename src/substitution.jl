function remaining_variables(p::Type{<:Polynomial}, s::Type{<:Substitutions})
    subs_vars = Set{Symbol}(name.(variables(s)))
    poly_vars = Set{Symbol}(collect(name.(variables(p))))
    setdiff(poly_vars, subs_vars)
end

function subs_complete(p::Polynomial{<:Term{T}}, s::Substitutions) where {T}
    result = zero(Base.promote_op(+, T, T))
    for term in p.terms
        result += subs(term, s)
    end
    result
end

function subs_partial(p::Polynomial, s::Substitutions)
    newterms = [subs(t, s...) for t in p.terms]
    sort!(newterms)
    Polynomial(newterms)
end

@generated function subs(p::Polynomial, s::Substitutions)
    v = remaining_variables(p, s)
    if isempty(v)
        :(subs_complete(p, s))
    else
        :(subs_partial(p, s))
    end
end

subs(p::Polynomial, s::Substitution...) = subs(p, s)

(p::Polynomial)(s::Vararg{Pair{<:Variable}}) = subs(p, s)
(p::Term)(s::Vararg{Pair{<:Variable}}) = subs(p, s...)
(p::Monomial)(s::Vararg{Pair{<:Variable}}) = subs(p, s...)
(p::Variable)(s::Vararg{Pair{<:Variable}}) = subs(p, s...)
