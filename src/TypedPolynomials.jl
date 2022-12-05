module TypedPolynomials

import MutableArithmetics
const MA = MutableArithmetics

using MultivariatePolynomials
const MP = MultivariatePolynomials

using MacroTools
import Base: *, +, -, /, ^, ==,
    promote_rule, show, isless, size, getindex,
    one, zero, iszero, @pure, copy, exponent, vec

using LinearAlgebra
import LinearAlgebra: dot, adjoint
export @polyvar,
       Variable,
       Monomial,
       Term,
       Polynomial,
       variables,
       terms,
       differentiate,
       antidifferentiate,
       subs

include("sequences.jl")
import .Sequences: shortest_common_supersequence, mergesorted

include("types.jl")
include("operators.jl")
include("substitution.jl")
include("calculus.jl")
include("conversion.jl")
include("promotion.jl")
include("call.jl")
include("macros.jl")

end # module
