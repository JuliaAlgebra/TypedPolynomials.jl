abstract type PolynomialLike end
abstract type TermLike <: PolynomialLike end
abstract type MonomialLike <: TermLike end
abstract type VariableLike <: MonomialLike end

function name(::VariableLike) end

function variables(::MonomialLike) end
function exponents(::MonomialLike) end
function degree(::MonomialLike) end

function coefficient(::TermLike) end
function monomial(::TermLike) end
degree(t::TermLike) = degree(monomial(t))

function terms(::PolynomialLike) end
