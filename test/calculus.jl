@testset "derivatives" begin
    @polyvar x y z

    @test @inferred(differentiate(x, x)) == 1
    @test @inferred(differentiate(x, y)) == 0
    @test @inferred(differentiate(y, x)) == 0
    @test @inferred(differentiate(x^2, x)) == 2x
    @test @inferred(differentiate(x^2, y)) == 0
    @test @inferred(differentiate(x^2 * y^3, y)) == 3 * x^2 * y^2
    @test @inferred(differentiate(x^2 * y^3, x)) == 2 * x * y^3
    @test @inferred(differentiate(x^2 * y^3, z)) == 0
    @test @inferred(differentiate(3x^2, x)) == 6x
    @test @inferred(differentiate(3x^2 * y^0, y)) == 0
    @test_broken @inferred(differentiate(5 * x^2 + 2 * x + 1, x)) == 10x + 2

    m = x^2
    @test @wrappedallocs(differentiate(m, x)) == 0
    @test @wrappedallocs(differentiate(m, y)) == 0
    m = x^2 * y^3
    @test @wrappedallocs(differentiate(m, x)) == 0
    @test @wrappedallocs(differentiate(m, y)) == 0
end
