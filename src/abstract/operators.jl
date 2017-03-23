# We reverse the order of comparisons here so that the result
# of x < y is equal to the result of Monomial(x) < Monomial(y)
isless(v1::AbstractVariable, v2::AbstractVariable) = name(v1) > name(v2)
isless(m1::AbstractMonomialLike, m2::AbstractMonomialLike) = isless(promote(m1, m2)...)
isless(t1::AbstractTerm, t2::AbstractTerm) = (monomial(t1), coefficient(t1)) < (monomial(t2), coefficient(t2))

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
    false
end

for op in [:+, :*, :-, :(==)]
    @eval $op(p1::AbstractPolynomialLike, p2::AbstractPolynomialLike) = $op(promote(p1, p2)...)
    @eval $op(p::AbstractPolynomialLike, x) = $op(promote(p, x)...)
    @eval $op(x, p::AbstractPolynomialLike) = $op(promote(x, p)...)
end

(+)(p1::AbstractTermLike, p2::AbstractTermLike) = convert(AbstractPolynomial, p1) + convert(AbstractPolynomial, p2)
(+)(p1::AbstractPolynomial, p2::AbstractPolynomial) = convert(AbstractPolynomial, jointerms(terms(p1), terms(p2)))

(-)(t::AbstractTermLike) = -1 * t
(-)(p1::AbstractTermLike, p2::AbstractTermLike) = convert(AbstractPolynomial, p1) - convert(AbstractPolynomial, p2)
(-)(p1::AbstractPolynomial, p2::AbstractPolynomial) = convert(AbstractPolynomial, jointerms(terms(p1), (-).(terms(p2))))

(*)(t1::AbstractTerm, t2::AbstractTerm) = convert(AbstractTerm, coefficient(t1) * coefficient(t2), monomial(t1) * monomial(t2))
# TODO: this is inefficient
(*)(p1::AbstractPolynomial, p2::AbstractPolynomial) = sum(terms(p1) .* terms(p2).')
(*)(t::AbstractPolynomialLike) = t

(==)(v1::AbstractVariable, v2::AbstractVariable) = name(v1) == name(v2)
(==)(m1::AbstractMonomial{V}, m2::AbstractMonomial{V}) where {V} = exponents(m1) == exponents(m2)
(==)(m1::AbstractMonomial, m2::AbstractMonomial) = variables(m1) == variables(m2) && exponents(m1) == exponents(m2)
(==)(t1::AbstractTerm, t2::AbstractTerm) = coefficient(t1) == coefficient(t2) && monomial(t1) == monomial(t2)
(==)(::AbstractTermLike, ::Void) = false
(==)(::Void, ::AbstractTermLike) = false

transpose(v::AbstractVariable) = v
transpose(m::AbstractMonomial) = m
transpose(t::T) where {T <: AbstractTerm} = T(coefficient(t)', monomial(t))
