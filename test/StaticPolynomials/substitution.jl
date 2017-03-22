
@testset "complete substitution" begin
    @polyvar x y z

    p = x^2 * y + y^2 + x
    @test p(x=>1, y=>2) == 7
    @inferred p(x=>1, y=>2)

    subs1(p, x, y) = @allocated p(x=>1, y=>2)
    subs1(p, x, y)
    @test subs1(p, x, y) == 0

    @test (@inferred x(x=>5)) == 5
    @test (@inferred x(y=>2)) == x
    @test (@inferred x(x=>y)) == y
end

@testset "partial substitution" begin
    @polyvar x y z

    p = x^2 * y + y^2 + x
    p2 = @inferred p(x=>4)
    @test length(terms(p2)) == 3
    @test terms(p2)[1].coefficient == 4
    @test degree(terms(p2)[1].monomial) == 0
    @test terms(p2)[2].coefficient == 16
    @test degree(terms(p2)[2].monomial) == 1
    @test exponents(monomial(terms(p2)[2])) == (1,)
    @test terms(p2)[3].coefficient == 1
    @test degree(terms(p2)[3].monomial) == 2
    @test exponents(monomial(terms(p2)[3])) == (2,)
end
