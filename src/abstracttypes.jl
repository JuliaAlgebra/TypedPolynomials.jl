abstract type AbstractPolynomial end
abstract type AbstractTerm end
abstract type AbstractMonomial end
abstract type AbstractVariable end

function name(::AbstractVariable) end
degree(::AbstractVariable) = 1

degree(m::AbstractMonomial) = sum(exponents(m))
function variables(::AbstractMonomial) end
function exponents(::AbstractMonomial) end
function exponent(::AbstractMonomial, ::AbstractVariable) end

degree(t::AbstractTerm) = degree(monomial(t))
variables(t::AbstractTerm) = variables(monomial(t))
exponents(t::AbstractTerm) = exponents(monomial(t))
exponent(t::AbstractTerm, v::AbstractVariable) = exponent(monomial(t), v)
function coefficient(::AbstractTerm) end
function monomial(::AbstractTerm) end

function terms(::AbstractPolynomial) end
function variables(::AbstractPolynomial) end
