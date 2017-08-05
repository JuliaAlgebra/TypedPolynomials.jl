module TypedPolynomials

using MultivariatePolynomials
const MP = MultivariatePolynomials

using MacroTools
import Base: *, +, -, /, ^, ==,
    promote_rule, show, isless, size, getindex,
    one, zero, iszero, transpose, isapprox, @pure, dot, copy, exponent, vec
export @polyvar,
       Variable,
       Monomial,
       Term,
       Polynomial,
       variables

include("sequences.jl")
import .Sequences: shortest_common_supersequence, mergesorted

#include("abstract/utils.jl")
include("abstract/types.jl")
include("abstract/operators.jl")
#include("abstract/show.jl")
include("abstract/substitution.jl")
include("abstract/calculus.jl")
include("types.jl")
include("operators.jl")
include("conversion.jl")
include("promotion.jl")
include("call.jl")
include("macros.jl")

end # module
