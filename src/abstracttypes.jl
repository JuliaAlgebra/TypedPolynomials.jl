abstract type PolynomialLike end
abstract type TermLike <: PolynomialLike end
abstract type MonomialLike <: TermLike end
abstract type VariableLike <: MonomialLike end

function name(::VariableLike) end
degree(::VariableLike) = 1

degree(m::MonomialLike) = sum(exponents(m))
function variables(::MonomialLike) end
function exponents(::MonomialLike) end
function exponent(::MonomialLike, ::VariableLike) end

degree(t::TermLike) = degree(monomial(t))
variables(t::TermLike) = variables(monomial(t))
exponents(t::TermLike) = exponents(monomial(t))
exponent(t::TermLike, v::VariableLike) = exponent(monomial(t), v)
function coefficient(::TermLike) end
function monomial(::TermLike) end

function terms(::PolynomialLike) end
function variables(::PolynomialLike) end
