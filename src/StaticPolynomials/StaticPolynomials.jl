module StaticPolynomials

using TypedPolynomials: mergesorted
import TypedPolynomials: AbstractVariable,
                         AbstractMonomial,
                         AbstractTerm,
                         AbstractPolynomial,
                         name,
                         exponents,
                         exponent,
                         variables,
                         coefficient,
                         monomial,
                         subs,
                         terms

import Base: literal_pow, +, -, *, /, ==, isless,
copy, promote_rule, convert

export @polyvar,
       Variable,
       Monomial,
       Term,
       Polynomial,
       name,
       exponents,
       variables,
       coefficient,
       monomial,
       terms,
       degree,
       subs

include("types.jl")
include("conversion.jl")
include("promotion.jl")
include("operators.jl")
include("substitution.jl")

end
