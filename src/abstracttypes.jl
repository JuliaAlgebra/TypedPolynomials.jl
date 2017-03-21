abstract type AbstractVariable end
function name end
degree(::AbstractVariable) = 1

abstract type AbstractMonomial end
degree(m::AbstractMonomial) = sum(exponents(m))
function variables end
function exponents end
function exponent end

abstract type AbstractTerm end
degree(t::AbstractTerm) = degree(monomial(t))
variables(t::AbstractTerm) = variables(monomial(t))
exponents(t::AbstractTerm) = exponents(monomial(t))
exponent(t::AbstractTerm, v::AbstractVariable) = exponent(monomial(t), v)
function coefficient end
function monomial end

abstract type AbstractPolynomial end
function terms end
function variables end
