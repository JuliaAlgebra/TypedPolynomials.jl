@polyvar x y z

@testset "variables" begin
    @test x * x == x^2
    @test x^1 == x
    @test x^0 == 1
    @test (@wrappedallocs x * x) == 0
    @test (@wrappedallocs x^2) == 0
    @test (@wrappedallocs x^1) == 0
    @test (@wrappedallocs x^0) == 0
end

@testset "orderings" begin
    @test Monomial(y) < Monomial(x)
    @test Monomial(x) > Monomial(y)
    @test Monomial(y) < x
    @test y < Monomial(x)
    @test y < x
    @test x > y
    @test x > Monomial(y)
    @test Monomial(x) > y

    @test x == x
    @test x == Monomial(x)
    @test x == 1x
    @test x != y
    @test x != Monomial(y)
    @test x != 1y
    @test x != nothing
    @test x != 0
end

@testset "monomials" begin
    m = x^2
    @test isa(m, Monomial)
    @test variables(m) == (x,)
    @test exponents(m) == (2,)

    m = x * y
    @test isa(m, Monomial)
    @test variables(m) == (x, y)
    @test exponents(m) == (1, 1)

    m = y * x
    @test isa(m, Monomial)
    @test variables(m) == (x, y)
    @test exponents(m) == (1, 1)

    m = y * x^2 * y^3
    @test isa(m, Monomial)
    @test variables(m) == (x, y)
    @test exponents(m) == (2, 4)

    @test x * y < x^2

    f(x, y) = @allocated ((x * y) < x^2)
    f(x, y)
    @test f(x, y) == 0
end

@testset "terms" begin
    t = @inferred 3x
    @test isa(t, Term)
    @test variables(t) == (x,)
    @test coefficient(t) == 3
    @test monomial(t) == Monomial(x)

    t = @inferred x * 3
    @test isa(t, Term)
    @test variables(t) == (x,)
    @test coefficient(t) == 3
    @test monomial(t) == Monomial(x)

    t = @inferred (2x) * y * x
    @test isa(t, Term)
    @test variables(t) == (x, y)
    @test coefficient(t) == 2
    @test exponents(t) == (2, 1)

    m1 = x * y * z^3 * x^2 * y
    m2 = x^3 * y^3 * z^2
    t1 = 2m1
    t2 = 2m2
    @test (@wrappedallocs t1 < t2) == 0
end

@testset "polynomials" begin
    p = @inferred x + 1
    @test isa(p, Polynomial)
    @test p.terms[2] == Term(x)
    @test variables(p) == (x,)

    p = @inferred x^2 + x * x
    @test isa(p, Polynomial)
    @test length(terms(p)) == 1
    @test coefficient(terms(p)[1]) == 2

    p = @inferred (1 + x) + (y + 2)
    @test coefficient(terms(p)[1]) == 3
    @test exponents(terms(p)[1]) == (0, 0)
    @test variables(p) == (x, y)
    @test coefficient(terms(p)[2]) == 1
    @test exponents(terms(p)[2]) == (0, 1)
    @test coefficient(terms(p)[3]) == 1
    @test exponents(terms(p)[3]) == (1, 0)

    p = @inferred x^2 + y + x * x + 3 * x * y + x * y^2
    @test length(terms(p)) == 4
    @test coefficient.(terms(p)) == [1, 3, 2, 1]
    @test exponents.(terms(p)) == [(0, 1), (1, 1), (2, 0), (1, 2)]

    @test (@wrappedallocs x^2 + y + x * x + 3 * x * y + x * y) <= 640
    @test (@wrappedallocs x^2 + 1) <= 128
end

function testmonomials(var, degree)
    [var^i for i in 0:degree]
end

@testset "monomial vector" begin
    ms = @inferred testmonomials(x, 3)
    @test eltype(ms) == Monomial{1, (x,)}
end
