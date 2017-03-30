
@testset "complete substitution" begin
    @polyvar x y z

    p = x^2 * y + y^2 + x
    @test (@inferred p(x=>1, y=>2)) == 7
    @test (@inferred p(x=>5)(y=>3)) == p(x=>5, y=>3)
    @test @wrappedallocs(p(x=>1, y=>2)) == 0
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

@testset "more inference" begin
    @test @inferred(subs(x, x=>1)) == 1
    @test @inferred(subs(x, x=>1, y=>2)) == 1
    @test @inferred(subs(x^2, x=>3.0)) == 9.0
    @test @inferred(subs(x^2 * y, x=>1)) == y
    @test @inferred(subs(x^2 * y, y=>5)) == 5x^2
    @test @inferred(subs(x^2 * y, y=>1.0, x=>2)) == 4
    @test @inferred(subs(1x, x=>2)) == 2
    @test @inferred(subs(x + 1, x=>1)) == 2
    @inferred(subs(x + 2y + z, y=>2.0, z=>π))
    @test_broken @inferred(subs(x + 2y + z, y=>2.0, z=>π)) == π + 4.0 + x
end