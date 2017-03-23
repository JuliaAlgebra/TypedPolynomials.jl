function remaining_variables(p::Type{<:Polynomial}, s::Type{<:Substitions})
    subs_vars = Set{Symbol}(name.(variables(s)))
    poly_vars = Set{Symbol}(collect(name.(variables(p))))
    setdiff(poly_vars, subs_vars)
end

function subs_complete(p::Polynomial{<:Term{T}}, s::Substitions) where {T}
    result = zero(Base.promote_op(+, T, T))
    for term in p.terms
        result += subs(term, s)
    end
    result
end

function subs_partial(p::Polynomial, s::Substitions)
    newterms = [subs(t, s) for t in p.terms]
    sort!(newterms)
    Polynomial(newterms)
end

@generated function subs(p::Polynomial, s::Substitions)
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

(p::Polynomial)(s::Vararg{Pair{<:Variable}}) = subs(p, s)
(p::Term)(s::Vararg{Pair{<:Variable}}) = subs(p, s)
(p::Monomial)(s::Vararg{Pair{<:Variable}}) = subs(p, s)
(p::Variable)(s::Vararg{Pair{<:Variable}}) = subs(p, s)
