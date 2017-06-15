@polyvar x y z

@testset "variables" begin
    @test @inferred(x * x) == @inferred(x^2)
    @test @inferred(x^1) == x
    @test @inferred(x^0) == 1
    @test (@wrappedallocs x * x) == 0
    @test (@wrappedallocs x^2) == 0
    @test (@wrappedallocs x^1) == 0
    @test (@wrappedallocs x^0) == 0
    @test @inferred(zero(x)) == @inferred(zero(typeof(x)))
    @test @inferred(one(x)) == 1
    @test @inferred(zero(x)) == @inferred(0 * x)
    @test nvars(x) == 1
    @test nvars(y) == 1
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

    @test x > y
    @test x^1 > y
    @test x^1 > y^1
    @test 1x^1 > 1y^1
    @test x^1 > 1y^1
    @test 1x^1 > y^1
    @test y < x
    @test y < x^1
    @test y^1 < x
    @test 1y^1 < x
    @test 1y^1 < 1x^1
    @test 1x < 2x
    @test 2x > 1x
end

@testset "monomials" begin
    m = x^2
    @test isa(m, Monomial)
    @test variables(m) == (x,)
    @test exponents(m) == (2,)
    @test exponent(m, x) == 2
    @test exponent(m, y) == 0
    @test exponent(m, z) == 0
    @test nvars(m) == 1

    m = x * y
    @test isa(m, Monomial)
    @test variables(m) == (x, y)
    @test exponents(m) == (1, 1)
    @test exponent(m, x) == 1
    @test exponent(m, y) == 1
    @test exponent(m, z) == 0
    @test nvars(m) == 2

    m = y * x
    @test isa(m, Monomial)
    @test variables(m) == (x, y)
    @test exponents(m) == (1, 1)
    @test exponent(m, x) == 1
    @test exponent(m, y) == 1
    @test exponent(m, z) == 0

    m = y * x^2 * y^3
    @test isa(m, Monomial)
    @test variables(m) == (x, y)
    @test exponents(m) == (2, 4)
    @test exponent(m, x) == 2
    @test exponent(m, y) == 4

    @test x * y < x^2

    @test @wrappedallocs((x * y) < x^2) == 0

    @test @inferred(zero(m)) == @inferred(zero(typeof(m)))
    @test zero(m) == 0 * m

    @test @inferred(Monomial{tuple(), 0}()) == @inferred(x^0)

    @test @inferred(Monomial{(x, y)}([1, 2])) == Monomial{(x, y)}((1, 2))
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
    @test exponent(t, x) == 2
    @test exponent(t, y) == 1

    @test @inferred(zero(t)) == @inferred(zero(typeof(t)))
    @test zero(t) == 0 * t
    @test @inferred(one(t)) == @inferred(one(typeof(t)))
    @test 1 == one(t) == 1 * t^0

    m1 = x * y * z^3 * x^2 * y
    m2 = x^3 * y^3 * z^2
    t1 = 2m1
    t2 = 2m2
    @test (@wrappedallocs t1 < t2) == 0

    @test -(2x^2 * y ) == -1 * (2y * x^2)

    @test TypedPolynomials.monomialtype(5 * x * y) == Monomial{(x, y), 2}
end

@testset "polynomials" begin
    p = @inferred x + 1
    @test isa(p, Polynomial)
    @test p.terms[2] == Term(x)
    @test variables(p) == (x,)

    @test @inferred(zero(p)) == @inferred(zero(typeof(p)))
    @test zero(p) == 0 * p
    @test @inferred(one(p)) == @inferred(one(typeof(p)))
    @test one(p) == 1

    @test mindeg(p) == 0
    @test maxdeg(p) == 1
    @test extdeg(p) == (0, 1)
    @test nvars(p) == 1

    p = @inferred x^2 + x * x
    @test isa(p, Polynomial)
    @test length(terms(p)) == 1
    @test coefficient(terms(p)[1]) == 2

    @test mindeg(p) == 2
    @test maxdeg(p) == 2
    @test extdeg(p) == (2, 2)
    @test nvars(p) == 1

    p = @inferred (1 + x) + (y + 2)
    @test coefficient(terms(p)[1]) == 3
    @test exponents(terms(p)[1]) == (0, 0)
    @test variables(p) == (x, y)
    @test coefficient(terms(p)[2]) == 1
    @test exponents(terms(p)[2]) == (0, 1)
    @test coefficient(terms(p)[3]) == 1
    @test exponents(terms(p)[3]) == (1, 0)

    @test mindeg(p) == 0
    @test maxdeg(p) == 1
    @test extdeg(p) == (0, 1)
    @test nvars(p) == 2

    p = @inferred x^2 + y + x * x + 3 * x * y + x * y^2
    @test length(terms(p)) == 4
    @test coefficient.(terms(p)) == [1, 3, 2, 1]
    @test exponents.(terms(p)) == [(0, 1), (1, 1), (2, 0), (1, 2)]

    @test mindeg(p) == 1
    @test maxdeg(p) == 3
    @test extdeg(p) == (1, 3)
    @test nvars(p) == 2

    @test (@wrappedallocs x^2 + y + x * x + 3 * x * y + x * y) <= 1376
    @test (@wrappedallocs x^2 + 1) <= 384

    @test (1 + x) * (x + 3) == 3 + 4x + x^2
    @test (2.0 + x) * (y + 1) == 2 + 2y + x + x * y
    @test (x + 1) - 1 == x

    @test @inferred(Polynomial(x) * x^2) == x^3
    @test @inferred((x + y) * x^2) == @inferred(x^3 + x^2 * y)

    @test @inferred(Polynomial([y, x])) == x + y
end

@testset "equality" begin
    @test 1 + x == 1 + x
    @test 1 + x == x + 1
    @test 1 + (1 + 1e-16) * x ≈ 1 + x
    @test 2 + 3 + x != 3 + x
    @test 2 + 3 + x == 5 + x
    @test x * y + 2 != y
    @test x * y + 2 == x * y + 2 * x^0
    @test x != nothing
    @test x^2 != nothing
    @test 3x != nothing
    @test x + 1 != nothing
    @test nothing != x
    @test nothing != x^2
    @test nothing != 3x
    @test nothing != x + 1

    @test Polynomial([1, 0 * x, y]) == Polynomial([1, y])
    @test Polynomial([1, 1 * x, y]) != Polynomial([1, y])
    @test Polynomial([1, y]) == Polynomial([1, 0 * x, y])
    @test Polynomial([1, y]) != Polynomial([1, 1 * x, y])
end

@testset "ordering" begin
    @test x > y
    @test Monomial(x) > Monomial(y)
    @test y < x
    @test Monomial(y) < Monomial(x)
    @test x^2 > x
    @test y^2 > x
    @test y^2 < x^2
    @test 3x < 5x
    @test 3x^2 > 5x
    @test 5x < x^2
    @test 1x^3 > 10y^3
end

@testset "linear algebra" begin
    @test @inferred([x, y]' * [1 2; 3 4] * [x, y]) == x^2 + 5 * x * y + 4 * y^2
    @test @inferred([x, y]' * [1 2; 3 4]) == [(x + 3y) (2x + 4y)]
    @test @inferred([x, y]' * [-1, 3]) == 3y - x
end


function testmonomials(var, degree)
    [var^i for i in 0:degree]
end

@testset "monomial vector" begin
    ms = @inferred testmonomials(x, 3)
    @test eltype(ms) == Monomial{(x,), 1}
end

@testset "operators" begin
    @polyvar x y z

    @test !iszero(x)
    @test !iszero(x^2)
    @test !iszero(5x)
    @test !iszero(5x + 1)
    @test iszero(0 * x)
    @test iszero(0 * x + 0)

    @test transpose(x) == x
    @test transpose(x^2) == x^2
    @test transpose(Term([1 2; 3 4], x)) == Term([1 2; 3 4]', x)

    @test @inferred(dot(1, x)) == 1x
    @test @inferred(dot(x, 5)) == 5x
    @test @inferred(dot(Term([1, 2], x), Term([3, 4], y))) == [1, 2]' * [3, 4] * x * y
end

@testset "linear algebra with tuples" begin
    @polyvar x[1:2] y z

    @test x .+ x == 2 .* x
    @test x .- y == (x[1] - y, x[2] - y)

    @test vecdot(x, x) == x[1]^2 + x[2]^2
    @test vecdot(x, [y, z]) == x[1]*y + x[2]*z
    @test vecdot([y, z], x) == x[1]*y + x[2]*z

    xv = @inferred(vec(x))
    @test @inferred(dot(xv, xv)) == x[1]^2 + x[2]^2
    @test @inferred(dot(xv, [y, z])) == x[1]*y + x[2]*z
    @test @inferred(dot([y, z], xv)) == x[1]*y + x[2]*z

    @test @inferred([1 2; 3 4] * xv) == [x[1] + 2x[2], 3x[1] + 4x[2]]
    @test @inferred(dot(xv, [1 2; 3 4] * xv)) == x[1]^2 + 5x[1]*x[2] + 4x[2]^2
end

struct FakeScalar
end

@testset "Term construction shortcut" begin
    @polyvar x y z

    # Verify that our shortcut that (*)(x, m::MonomialLike) = Term(x, m) works
    @test typeof(@inferred(FakeScalar() * x)) == Term{FakeScalar, Monomial{(x,), 1}}
    @test typeof(@inferred(FakeScalar() * x^2)) == Term{FakeScalar, Monomial{(x,), 1}}
    @test typeof(@inferred(x * FakeScalar())) == Term{FakeScalar, Monomial{(x,), 1}}
    @test typeof(@inferred(x^2 * FakeScalar())) == Term{FakeScalar, Monomial{(x,), 1}}
end
