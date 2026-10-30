module TestDemo

using Test, NBInclude

@testset "Demo" begin
    @nbinclude(joinpath(@__DIR__, "..", "demo.ipynb"))
end

end
