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
    @test @inferred(differentiate(5 * x^2 + 2 * x + 1, x)) == 10x + 2

    @test @inferred(exponents(differentiate(x^0,x))==(0,))
    @test @inferred(exponents(differentiate(x^0*y*z^2,x))==(0,1,2))

    m = x^2
    @test @wrappedallocs(differentiate(m, x)) == 0
    @test @wrappedallocs(differentiate(m, y)) == 0
    m = x^2 * y^3
    @test @wrappedallocs(differentiate(m, x)) == 0
    @test @wrappedallocs(differentiate(m, y)) == 0

    @test @inferred(differentiate(1, x)) == 0
end

@testset "antiderivatives" begin
    @polyvar x y z

    @test @inferred(antidifferentiate(x, x)) == 1//2*x^2
    @test @inferred(antidifferentiate(x, y)) == x*y
    @test @inferred(antidifferentiate(y, x)) == x*y
    @test @inferred(antidifferentiate(x^2, x)) == 1//3*x^3
    @test @inferred(antidifferentiate(x^2, y)) == x^2*y
    @test @inferred(antidifferentiate(x^2 * y^3, y)) == x^2 * 1//4*y^4
    @test @inferred(antidifferentiate(x^2 * y^3, x)) == x^3 * 1//3*y^3
    @test @inferred(antidifferentiate(x^2 * y^3, z)) == x^2 * y^3 * z
    @test @inferred(antidifferentiate(3x^2, x)) == x^3
    @test @inferred(antidifferentiate(3x^2 * y^0, y)) == 3x^2 * y
    @test @inferred(antidifferentiate(3 * x^2 + 2 * x + 1, x)) == x^3 + x^2 + x

    @test @inferred(exponents(antidifferentiate(x^0,x))==(1,))
    @test @inferred(exponents(antidifferentiate(x^0*y*z^2,x))==(1,1,2))

    m = x^2
    @test @wrappedallocs(antidifferentiate(m, x)) == 0
    @test @wrappedallocs(antidifferentiate(m, y)) == 0
    m = x^2 * y^3
    @test @wrappedallocs(antidifferentiate(m, x)) == 0
    @test @wrappedallocs(antidifferentiate(m, y)) == 0

    @test @inferred(antidifferentiate(1, x)) == x
end
