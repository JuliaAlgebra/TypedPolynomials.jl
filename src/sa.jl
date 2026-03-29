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

