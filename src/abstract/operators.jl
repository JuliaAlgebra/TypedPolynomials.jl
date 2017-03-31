# We reverse the order of comparisons here so that the result
# of x < y is equal to the result of Monomial(x) < Monomial(y)
@pure isless(v1::AbstractVariable, v2::AbstractVariable) = name(v1) > name(v2)
isless(m1::AbstractTermLike, m2::AbstractTermLike) = isless(promote(m1, m2)...)

# Graded Lexicographic order
# First compare total degree, then lexicographic order
function isless(m1::AbstractMonomial{V}, m2::AbstractMonomial{V}) where {V}
    d1 = degree(m1)
    d2 = degree(m2)
    if d1 < d2
        return true
    elseif d1 > d2
        return false
    else
        return exponents(m1) < exponents(m2)
    end
end

function isless(t1::AbstractTerm, t2::AbstractTerm)
    if monomial(t1) < monomial(t2)
        true
    elseif monomial(t1) == monomial(t2)
        coefficient(t1) < coefficient(t2)
    else
        false
    end
end

for op in [:+, :-, :*, :(==)]
    @eval $op(p1::AbstractPolynomialLike, p2::AbstractPolynomialLike) = $op(promote(p1, p2)...)
    @eval $op(p::AbstractPolynomialLike, x) = $op(promote(p, x)...)
    @eval $op(x, p::AbstractPolynomialLike) = $op(promote(x, p)...)
end

for op in [:+, :-]
    @eval $op(p1::AbstractTermLike, p2::AbstractTermLike) = $op(convert(AbstractPolynomial, p1), convert(AbstractPolynomial, p2))
    @eval $op(p1::AbstractPolynomial, p2::AbstractTermLike) = $op(p1, convert(AbstractPolynomial, p2))
    @eval $op(p1::AbstractTermLike, p2::AbstractPolynomial) = $op(convert(AbstractPolynomial, p1), p2)
end

(+)(p1::AbstractPolynomial, p2::AbstractPolynomial) = convert(AbstractPolynomial, jointerms(terms(p1), terms(p2)))

(-)(t::AbstractTermLike) = -1 * t
(-)(p1::AbstractPolynomial, p2::AbstractPolynomial) = convert(AbstractPolynomial, jointerms(terms(p1), (-).(terms(p2))))

(*)(t1::AbstractTerm, t2::AbstractTerm) = convert(AbstractTerm, coefficient(t1) * coefficient(t2), monomial(t1) * monomial(t2))
# TODO: this is inefficient
(*)(p1::AbstractPolynomial, p2::AbstractPolynomial) = sum(terms(p1) .* terms(p2).')
(*)(t::AbstractPolynomialLike) = t

@pure (==)(v1::AbstractVariable, v2::AbstractVariable) = name(v1) == name(v2)
(==)(m1::AbstractMonomial{V}, m2::AbstractMonomial{V}) where {V} = exponents(m1) == exponents(m2)
(==)(m1::AbstractMonomial, m2::AbstractMonomial) = variables(m1) == variables(m2) && exponents(m1) == exponents(m2)
(==)(t1::AbstractTerm, t2::AbstractTerm) = coefficient(t1) == coefficient(t2) && monomial(t1) == monomial(t2)
(==)(::AbstractTermLike, ::Void) = false
(==)(::Void, ::AbstractTermLike) = false

function compare_terms(p1::AbstractPolynomial, p2::AbstractPolynomial, op)
    i1 = 1
    i2 = 1
    t1 = terms(p1)
    t2 = terms(p2)
    while true
        while i1 <= length(t1) && coefficient(t1[i1]) == 0
            i1 += 1
        end
        while i2 <= length(t2) && coefficient(t2[i2]) == 0
            i2 += 1
        end
        if i1 > length(t1) && i2 > length(t2)
            return true
        end
        if i1 > length(t1) || i2 > length(t2)
            return false
        end
        if !op(t1[i1], t2[i2])
            return false
        end
        i1 += 1
        i2 += 1
    end
end

(==)(p1::AbstractPolynomial, p2::AbstractPolynomial) = compare_terms(p1, p2, (==))

isapprox(t1::AbstractTerm, t2::AbstractTerm; kwargs...) = isapprox(coefficient(t1), coefficient(t2); kwargs...) && monomial(t1) == monomial(t2)
isapprox(p1::AbstractPolynomial, p2::AbstractPolynomial; kwargs...) = compare_terms(p1, p2, (x, y) -> isapprox(x, y; kwargs...))

transpose(v::AbstractVariable) = v
transpose(m::AbstractMonomial) = m
transpose(t::T) where {T <: AbstractTerm} = T(coefficient(t)', monomial(t))
