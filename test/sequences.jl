import TypedPolynomials: shortest_common_supersequence

@testset "supersequences" begin
    # https://en.wikipedia.org/wiki/Shortest_common_supersequence_problem
    x = "abcbdab"
    y = "bdcaba"
    @inferred shortest_common_supersequence(x, y)
    z = shortest_common_supersequence(x, y)
    @test z == "abdcabdab"

    @test shortest_common_supersequence([:x, :x], [:x]) == [:x, :x]

    @test shortest_common_supersequence([1, 2, 3, 2, 1], [1, 1]) == [1, 2, 3, 2, 1]
    @inferred shortest_common_supersequence([1, 2, 3, 2, 1], [1, 1])
    @test eltype(shortest_common_supersequence([1, 2, 3, 2, 1], [1.0, 1.0])) == promote_type(Int, Float64)

    @test shortest_common_supersequence("xyx", "yx") == "xyx"
    @test shortest_common_supersequence("yx", "xyx") == "xyx"
    @test shortest_common_supersequence("xyx", "yxy") in ["xyxy", "yxyx"]
    @test shortest_common_supersequence("yxy", "xyx") in ["xyxy", "yxyx"]
    @test shortest_common_supersequence("xyx", "zx") in ["zxyx", "xzyx", "xyzx"]
    @test shortest_common_supersequence("zx", "xyx") in ["zxyx", "xzyx", "xyzx"]
    @test shortest_common_supersequence("xyxxx", "yxzx") in ["xyxzxx", "xyxxzx"]
    @test shortest_common_supersequence("yxzx", "xyxxx") in ["xyxzxx", "xyxxzx"]
    @test shortest_common_supersequence("xxyxy", "xzxy") in ["xzxyxy", "xxzyxy", "xxyzxy"]
    @test shortest_common_supersequence("xzxy", "xxyxy") in ["xzxyxy", "xxzyxy", "xxyzxy"]
end
