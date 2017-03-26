struct StagedTerm{M <: Monomial}
    coeff_expr::Expr
    monomial::M
end

isless(s1::StagedTerm, s2::StagedTerm) = s1.monomial < s2.monomial
function combine(s1::StagedTerm, s2::StagedTerm)
    @assert s1.monomial == s2.monomial
    StagedTerm(:($(s1.coeff_expr) + $(s2.coeff_expr)), s1.monomial)
end

@generated function jointerms(terms1::Tuple{Vararg{Term}}, terms2::Tuple{Vararg{Term}})
    v1 = [StagedTerm(:(terms1[$i].coefficient), monomial(t)) for (i, t) in enumerate(terms1.parameters)]
    v2 = [StagedTerm(:(terms2[$i].coefficient), monomial(t)) for (i, t) in enumerate(terms2.parameters)]

    newterms = mergesorted(v1, v2, <, combine)
    Expr(:tuple, [:(Term($(x.coeff_expr), $(x.monomial))) for x in newterms]...)
end

(+)(m1::M, m2::M) where {M <: Monomial} = Term{M}(c)
(+)(t1::Term{T1, M}, t2::Term{T2, M}) where {T1, T2, M} = Term{M}(coefficient(t1) + coefficient(t2))

(*)(v1::V, v2::V) where {V <: Variable} = Monomial{(v1,), (2,)}()
# @generated function (*)(::Power{V, E1}, ::Power{V, E2}) where {V, E1, E2}
#     :(Power{V, $(E1 + E2)}())
# end
(*)(p1::Power{V}, p2::Power{V}) where {V} = Power{V}(p1.exponent + p2.exponent)

@generated function (*)(m1::Monomial, m2::Monomial)
    newpowers = mergesorted(
        collect(powers(m1)),
        collect(powers(m2)),
        (p1, p2) -> name(variable(p1)) < name(variable(p2)),
        *)
    :(Monomial{$(Tuple(variable.(newpowers))), $(Tuple(exponent.(newpowers)))}())
end

function (==)(m1::Monomial, m2::Monomial)
    v1 = variables(m1)
    e1 = exponents(m1)
    v2 = variables(m2)
    e2 = exponents(m2)
    i1 = 1
    i2 = 1
    while true
        while i1 <= length(e1) && e1[i1] == 0
            i1 += 1
        end
        while i2 <= length(e2) && e2[i2] == 0
            i2 += 1
        end
        if i1 > length(e1) && i2 > length(e1)
            return true
        elseif i1 > length(e1) || i2 > length(e2)
            return false
        elseif v1[i1] != v2[i2]
            return false
        elseif e1[i1] != e2[i2]
            return false
        else
            i1 += 1
            i2 += 1
        end
    end
end
