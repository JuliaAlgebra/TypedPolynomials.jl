include("utils.jl")
using Pkg, NBInclude

const mvp_test = joinpath(dirname(pathof(MultivariatePolynomials)), "..", "test")
const Mod = TypedPolynomials

@testset "TypedPolynomials" begin
    # include("jump.jl")
    include("generators.jl")
    include("macros.jl")
    include("calculus.jl")
    include("promotion.jl")
    include("polynomials.jl")
    include("substitution.jl")
    # include("sequences.jl")
end

#@testset "MultivariatePolynomials interface tests" begin
include(joinpath(mvp_test, "utils.jl"))
include(joinpath(mvp_test, "commutativetests.jl"))
#end

@testset "Demo" begin
    @nbinclude(joinpath(@__DIR__, "..", "demo.ipynb"))
end
