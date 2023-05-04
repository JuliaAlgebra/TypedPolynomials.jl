@testset "promotion" begin
    @polyvar x y z

    @test typeof(@inferred promote(1, y)) == NTuple{2, Term{Int, Monomial{(y,), 1}}}
    @test typeof(@inferred promote(1, y^2)) == NTuple{2, Term{Int, Monomial{(y,), 1}}}
    @test typeof(@inferred promote(1, 2y)) == NTuple{2, Term{Int, Monomial{(y,), 1}}}
    @test typeof(@inferred promote(1, polynomial(2y))) == NTuple{2, Polynomial{Int, Term{Int, Monomial{(y,), 1}}, Vector{Term{Int, Monomial{(y,), 1}}}}}
    @test typeof(@inferred promote(1, polynomial([2y]))) == NTuple{2, Polynomial{Int, Term{Int, Monomial{(y,), 1}}, Vector{Term{Int, Monomial{(y,), 1}}}}}

    @test typeof(@inferred promote(x, 1)) == NTuple{2, Term{Int, Monomial{(x,), 1}}}
    @test typeof(@inferred promote(x, y)) == NTuple{2, Monomial{(x, y), 2}}
    @test typeof(@inferred promote(y, x)) == NTuple{2, Monomial{(x, y), 2}}
    @test typeof(@inferred promote(x, y^2)) == NTuple{2, Monomial{(x, y), 2}}
    @test typeof(@inferred promote(x, 2y)) == NTuple{2, Term{Int, Monomial{(x, y), 2}}}
    @test typeof(@inferred promote(x, polynomial(2y))) == NTuple{2, Polynomial{Int, Term{Int, Monomial{(x, y), 2}}, Vector{Term{Int, Monomial{(x, y), 2}}}}}
    @test typeof(@inferred promote(x, polynomial([2y]))) == NTuple{2, Polynomial{Int, Term{Int, Monomial{(x, y), 2}}, Vector{Term{Int, Monomial{(x, y), 2}}}}}

    @test typeof(@inferred promote(x^2, 1)) == NTuple{2, Term{Int, Monomial{(x,), 1}}}
    @test typeof(@inferred promote(x^2, y)) == NTuple{2, Monomial{(x, y), 2}}
    @test typeof(@inferred promote(y, x^2)) == NTuple{2, Monomial{(x, y), 2}}
    @test typeof(@inferred promote(x^2, y^2)) == NTuple{2, Monomial{(x, y), 2}}
    @test typeof(@inferred promote(x^2, 2y)) == NTuple{2, Term{Int, Monomial{(x, y), 2}}}
    @test typeof(@inferred promote(x^2, polynomial(2y))) == NTuple{2, Polynomial{Int, Term{Int, Monomial{(x, y), 2}}, Vector{Term{Int, Monomial{(x, y), 2}}}}}
    @test typeof(@inferred promote(x^2, polynomial([2y]))) == NTuple{2, Polynomial{Int, Term{Int, Monomial{(x, y), 2}}, Vector{Term{Int, Monomial{(x, y), 2}}}}}

    @test typeof(@inferred promote(2x, 1)) == NTuple{2, Term{Int, Monomial{(x,), 1}}}
    @test typeof(@inferred promote(2x, y)) == NTuple{2, Term{Int, Monomial{(x, y), 2}}}
    @test typeof(@inferred promote(2x, y^2)) == NTuple{2, Term{Int, Monomial{(x, y), 2}}}
    @test typeof(@inferred promote(2x, 2y)) == NTuple{2, Term{Int, Monomial{(x, y), 2}}}
    @test typeof(@inferred promote(2x, polynomial(2y))) == NTuple{2, Polynomial{Int, Term{Int, Monomial{(x, y), 2}}, Vector{Term{Int, Monomial{(x, y), 2}}}}}
    @test typeof(@inferred promote(2x, polynomial([2y]))) == NTuple{2, Polynomial{Int, Term{Int, Monomial{(x, y), 2}}, Vector{Term{Int, Monomial{(x, y), 2}}}}}

    @test typeof(@inferred promote(polynomial(2x), 1)) == NTuple{2, Polynomial{Int, Term{Int, Monomial{(x,), 1}}, Vector{Term{Int, Monomial{(x,), 1}}}}}
    @test typeof(@inferred promote(polynomial(2x), y)) == NTuple{2, Polynomial{Int, Term{Int, Monomial{(x, y), 2}}, Vector{Term{Int, Monomial{(x, y), 2}}}}}
    @test typeof(@inferred promote(polynomial(2x), y^2)) == NTuple{2, Polynomial{Int, Term{Int, Monomial{(x, y), 2}}, Vector{Term{Int, Monomial{(x, y), 2}}}}}
    @test typeof(@inferred promote(polynomial(2x), 2y)) == NTuple{2, Polynomial{Int, Term{Int, Monomial{(x, y), 2}}, Vector{Term{Int, Monomial{(x, y), 2}}}}}
    @test typeof(@inferred promote(polynomial(2x), polynomial(2y))) == NTuple{2, Polynomial{Int, Term{Int, Monomial{(x, y), 2}}, Vector{Term{Int, Monomial{(x, y), 2}}}}}
    @test typeof(@inferred promote(polynomial(2x), polynomial([2y]))) == NTuple{2, Polynomial{Int, Term{Int, Monomial{(x, y), 2}}, Vector{Term{Int, Monomial{(x, y), 2}}}}}

    @test typeof(@inferred(promote(Monomial{tuple(), 0}(), x))) == NTuple{2, Monomial{(x,), 1}}
    @test typeof(@inferred(promote(x, Monomial{tuple(), 0}()))) == NTuple{2, Monomial{(x,), 1}}
end

@testset "conversion" begin
    @polyvar x y z

    @test convert(Monomial{(x, y)}, x) == Monomial{(x, y)}((1, 0))
    @test_throws InexactError convert(Monomial{(x,)}, y)
    @test_throws InexactError convert(Monomial{(x,)}, x * y)
end

@testset "variable_union_type" begin
    @polyvar x y z

    function test(p)
        @test MultivariatePolynomials.variable_union_type(p) == TypedPolynomials.Variable
        @test MultivariatePolynomials.variable_union_type(typeof(p)) == TypedPolynomials.Variable
    end
    test(x)
    test(y)
    test(z)
    test(x*y*z)
    test(2x)
    test(2y)
    test(2z)
    test(x + y + z)
end
