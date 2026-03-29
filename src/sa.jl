import StarAlgebras as SA

function SA.promote_with_map(
    m::Monomial{V,N},
    all_vars::Tuple{Vararg{Variable}},
    map::MP.ExponentMap,
) where {V,N}
    new_exps = map(m.exponents)
    M = length(all_vars)
    new_mono = Monomial{all_vars,M}(new_exps)
    return new_mono, map
end

function SA.promote_with_map(
    v::Variable,
    all_vars::Tuple{Vararg{Variable}},
    map::MP.ExponentMap,
)
    return SA.promote_with_map(Monomial(v), all_vars, map)
end

function SA.promote_with_map(
    t::MP.Term{C,<:Monomial},
    all_vars::Tuple{Vararg{Variable}},
    map::MP.ExponentMap,
) where {C}
    new_mono, _ = SA.promote_with_map(monomial(t), all_vars, map)
    return MP.Term(coefficient(t), new_mono), map
end

function SA.promote_with_map(
    p::MP.Polynomial{C,<:MP.Term{C,<:Monomial}},
    all_vars::Tuple{Vararg{Variable}},
    map::MP.ExponentMap,
) where {C}
    new_terms = [begin
        new_mono, _ = SA.promote_with_map(monomial(t), all_vars, map)
        MP.Term(coefficient(t), new_mono)
    end for t in terms(p)]
    return polynomial(new_terms), map
end
