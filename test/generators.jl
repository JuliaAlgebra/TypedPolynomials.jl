@testset "monomial generation" begin
    @polyvar x y z

    @test TypedPolynomials.monomial_powers(Val{3}(), 3) == [
        (3, 0, 0),
        (2, 1, 0),
        (2, 0, 1),
        (1, 2, 0),
        (1, 1, 1),
        (1, 0, 2),
        (0, 3, 0),
        (0, 2, 1),
        (0, 1, 2),
        (0, 0, 3)
    ]
    @test @inferred(monomials((x, y), 2)) == [x^2, x * y, y^2]
    @test @inferred(monomials((x, y), 0:2)) == [x^2, x * y, y^2, x, y, 1]
    @test @inferred(monomials((x, y), [1, 0, 2])) == [x^2, x * y, y^2, x, y, 1]
end
