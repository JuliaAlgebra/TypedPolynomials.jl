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

    srand(1)
    for vars in ((x,), (x, y), (x, y, z), (x, z))
        for degree in 0:5
            @test issorted(@inferred(monomials(vars, degree)), rev=true)
            @test issorted(@inferred(monomials(vars, 0:degree)), rev=true)
        end
    end
    @test_throws ArgumentError monomials((z, y), 2)
    @test_throws ArgumentError monomials((z, y), [1, 2])
end
