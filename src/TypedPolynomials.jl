__precompile__()

module TypedPolynomials

using MultivariatePolynomials
const MP = MultivariatePolynomials

using MacroTools
import Base: *, +, -, /, ^, ==,
    promote_rule, show, isless, size, getindex,
    one, zero, iszero, isapprox, @pure, copy, exponent, vec
using Compat
using Compat.LinearAlgebra
import Compat.LinearAlgebra: dot, adjoint
export @polyvar,
       Variable,
       Monomial,
       Term,
       Polynomial,
       variables,
       terms,
       differentiate,
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
