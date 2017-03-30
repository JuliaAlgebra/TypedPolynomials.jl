module TypedPolynomials

using StaticArrays: SVector
import Base: *, +, -, /, ^, ==,
    promote_rule, convert, show, isless, size, getindex,
    one, zero, transpose, isapprox
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
import .Sequences: shortest_common_supersequence, mergesorted

include("abstract/types.jl")
include("abstract/operators.jl")
include("abstract/show.jl")
include("abstract/substitution.jl")
include("types.jl")
include("operators.jl")
include("conversion.jl")
include("promotion.jl")
include("call.jl")

include("StaticPolynomials/StaticPolynomials.jl")

end # module
