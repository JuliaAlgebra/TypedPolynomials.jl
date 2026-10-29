module TestTypedPolynomials

include("utils.jl")

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

end

include("mvp.jl")
include("demo.jl")
