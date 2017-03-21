module TypedPolynomials

using StaticArrays: SVector
import Base: *, +, -, /, ^, ==,
    promote_rule, convert, show, isless, size, getindex,
    one, zero, transpose
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

include("sequences.jl")
import .Sequences: shortest_common_supersequence

include("abstracttypes.jl")
include("types.jl")
include("operators.jl")
include("conversion.jl")
include("promotion.jl")
include("show.jl")
include("substitution.jl")

include("StaticPolynomials/StaticPolynomials.jl")

end # module
