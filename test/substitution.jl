@testset "complete substitution" begin
    @polyvar x y z

    p = x^2 * y + y^2 + x
    @test (@inferred p(x=>1, y=>2)) == 7
    @test (@inferred p(x=>5)(y=>3)) == p(x=>5, y=>3)
    @test @wrappedallocs(p(x=>1, y=>2)) == 0 # Works on Julia v0.6 but fails on Julia v0.7
    @test (@inferred x(x=>5)) == 5
    @test (@inferred x(y=>2)) == x
    @test (@inferred x(x=>y)) == y

end

@testset "partial substitution" begin
    @polyvar x y z

    p = x^2 * y + y^2 + x
    p2 = @inferred p(x=>4)
    @test length(terms(p2)) == 3
    @test terms(p2)[1].coefficient == 1
    @test deg(terms(p2)[1].monomial) == 2
    @test exponents(monomial(terms(p2)[1])) == (2,)
    @test terms(p2)[2].coefficient == 16
    @test deg(terms(p2)[2].monomial) == 1
    @test exponents(monomial(terms(p2)[2])) == (1,)
    @test terms(p2)[3].coefficient == 4
    @test deg(terms(p2)[3].monomial) == 0
end

@testset "more inference" begin
    @polyvar x y z

    @test @inferred(subs(x, x=>1)) == 1
    @test @inferred(subs(x, x=>1, y=>2)) == 1
    @test @inferred(subs(x^2, x=>3.0)) == 9.0
    @test @inferred(subs(x^2 * y, x=>1)) == y
    @test @inferred(subs(x^2 * y, y=>5)) == 5x^2
    @test @inferred(subs(x^2 * y, y=>1.0, x=>2)) == 4
    @test @inferred(subs(1x, x=>2)) == 2
    @test @inferred(subs(x + 1, x=>1)) == 2
    @test @inferred(subs(x + 2y + z, y=>2.0, z=>π)) ≈ π + 4.0 + x
end

@testset "more call overloads" begin
    @polyvar x y z
    t = 5x^2 * z * 3 * y * z
    @test @inferred(t(x=>2, y=>2)) == @inferred(15 * 2^2 * z^2 * 2)
    m = z * z * z
    @test @inferred(m(x=>5)) == z^3
    @test @inferred(m(z=>2)) == 8
end

@testset "monomial substitution" begin
    @polyvar x y z

    @test @inferred(subs(x^2 * y, y=>z^2)) == x^2 * z^2
end

@testset "tuple substitution" begin
    @polyvar x y z

    @test @inferred(subs(x^2 * y * z^3, (x, y)=>(2, 3))) == (2^2 * 3 * z^3)
    @test @inferred(subs(5x, (x,)=>(6,))) == 30
    @test @inferred(subs(x * y * z^2, (y, x) => (z, z))) == z^4
    @test @inferred(subs(x^2 * y, (x, y) => (z^3, 5))) == 5 * z^6

    @testset "call overloads" begin
        @test @inferred(x((x,)=>(3,))) == 3

        m = z^1 * y^2
        @test @inferred(m((y, z) => (2, 3))) == 2^2 * 3

        t = 3 * m
        @test @inferred(t((y, z) => (2, 3))) == 3 * 2^2 * 3

        p = 3 * m + 1
        @test @inferred(p((y, z) => (2, 3))) == 3 * 2^2 * 3 + 1
    end
end

@testset "swapping variables" begin
    @polyvar x y z

    @test subs(x^2 * y, x=>y^2, y=>x) == y^4 * x
    # From example 1 in MultivariatePolynomials README
    p = 2x + 3.0x*y^2 + y
    @test p((x, y)=>(y, x)) == 2y + 3y*x^2 + x
    @test p((x, y)=>[1, 2]) == 16
end

@testset "non-variable substitution" begin
    @polyvar x y z

    @test @inferred(subs(5, x=>2)) == 5
    @test @inferred(subs(11, (x, y)=>(y, x))) == 11
end


