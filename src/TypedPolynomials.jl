module TypedPolynomials

using StaticArrays: SVector
using MacroTools
import Base: *, +, -, /, ^, ==,
    promote_rule, convert, show, isless, size, getindex,
    one, zero, transpose, isapprox, @pure, dot, copy
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
       subs,
       differentiate,
       mindeg,
       maxdeg,
       extdeg,
       nvars,
       monomials

include("sequences.jl")
import .Sequences: shortest_common_supersequence, mergesorted

include("abstract/utils.jl")
include("abstract/types.jl")
include("abstract/operators.jl")
include("abstract/show.jl")
include("abstract/substitution.jl")
include("abstract/calculus.jl")
include("types.jl")
include("operators.jl")
include("conversion.jl")
include("promotion.jl")
include("call.jl")
include("macros.jl")

end # module
