if !@isdefined(TypedPolynomials)
    include("utils.jl")
end

@testset "promotion" begin
    @polyvar x y z

    @test typeof(@inferred promote(1, y)) == NTuple{2,term_type(monomial_type((y,)), Int)}
    @test typeof(@inferred promote(1, y^2)) == NTuple{2,term_type(monomial_type((y,)), Int)}
    @test typeof(@inferred promote(1, 2y)) == NTuple{2,term_type(monomial_type((y,)), Int)}
    @test typeof(@inferred promote(1, polynomial(2y))) ==
          NTuple{2,polynomial_type(monomial_type((y,)), Int)}
    @test typeof(@inferred promote(1, polynomial([2y]))) ==
          NTuple{2,polynomial_type(monomial_type((y,)), Int)}

    @test typeof(@inferred promote(x, 1)) == NTuple{2,term_type(monomial_type((x,)), Int)}
    @test typeof(@inferred promote(x, y)) == NTuple{2,Monomial{typeof((x, y)),2}}
    @test typeof(@inferred promote(y, x)) == NTuple{2,Monomial{typeof((x, y)),2}}
    @test typeof(@inferred promote(x, y^2)) == NTuple{2,Monomial{typeof((x, y)),2}}
    @test typeof(@inferred promote(x, 2y)) ==
          NTuple{2,term_type(monomial_type((x, y)), Int)}
    @test typeof(@inferred promote(x, polynomial(2y))) ==
          NTuple{2,polynomial_type(monomial_type((x, y)), Int)}
    @test typeof(@inferred promote(x, polynomial([2y]))) ==
          NTuple{2,polynomial_type(monomial_type((x, y)), Int)}

    @test typeof(@inferred promote(x^2, 1)) == NTuple{2,term_type(monomial_type((x,)), Int)}
    @test typeof(@inferred promote(x^2, y)) == NTuple{2,Monomial{typeof((x, y)),2}}
    @test typeof(@inferred promote(y, x^2)) == NTuple{2,Monomial{typeof((x, y)),2}}
    @test typeof(@inferred promote(x^2, y^2)) == NTuple{2,Monomial{typeof((x, y)),2}}
    @test typeof(@inferred promote(x^2, 2y)) ==
          NTuple{2,term_type(monomial_type((x, y)), Int)}
    @test typeof(@inferred promote(x^2, polynomial(2y))) ==
          NTuple{2,polynomial_type(monomial_type((x, y)), Int)}
    @test typeof(@inferred promote(x^2, polynomial([2y]))) ==
          NTuple{2,polynomial_type(monomial_type((x, y)), Int)}

    @test typeof(@inferred promote(2x, 1)) == NTuple{2,term_type(monomial_type((x,)), Int)}
    @test typeof(@inferred promote(2x, y)) ==
          NTuple{2,term_type(monomial_type((x, y)), Int)}
    @test typeof(@inferred promote(2x, y^2)) ==
          NTuple{2,term_type(monomial_type((x, y)), Int)}
    @test typeof(@inferred promote(2x, 2y)) ==
          NTuple{2,term_type(monomial_type((x, y)), Int)}
    @test typeof(@inferred promote(2x, polynomial(2y))) ==
          NTuple{2,polynomial_type(monomial_type((x, y)), Int)}
    @test typeof(@inferred promote(2x, polynomial([2y]))) ==
          NTuple{2,polynomial_type(monomial_type((x, y)), Int)}

    @test typeof(@inferred promote(polynomial(2x), 1)) ==
          NTuple{2,polynomial_type(monomial_type((x,)), Int)}
    @test typeof(@inferred promote(polynomial(2x), y)) ==
          NTuple{2,polynomial_type(monomial_type((x, y)), Int)}
    @test typeof(@inferred promote(polynomial(2x), y^2)) ==
          NTuple{2,polynomial_type(monomial_type((x, y)), Int)}
    @test typeof(@inferred promote(polynomial(2x), 2y)) ==
          NTuple{2,polynomial_type(monomial_type((x, y)), Int)}
    @test typeof(@inferred promote(polynomial(2x), polynomial(2y))) ==
          NTuple{2,polynomial_type(monomial_type((x, y)), Int)}
    @test typeof(@inferred promote(polynomial(2x), polynomial([2y]))) ==
          NTuple{2,polynomial_type(monomial_type((x, y)), Int)}

    @test typeof(@inferred(promote(Monomial{Tuple{},0}(), x))) ==
          NTuple{2,Monomial{typeof((x,)),1}}
    @test typeof(@inferred(promote(x, Monomial{Tuple{},0}()))) ==
          NTuple{2,Monomial{typeof((x,)),1}}
end

@testset "monomial_type for variable tuples" begin
    @polyvar x y z

    @test @inferred(monomial_type((x,))) == Monomial{typeof((x,)),1}
    @test @inferred(monomial_type((x,))) == monomial_type(x)
    @test @inferred(monomial_type((x, y))) == Monomial{typeof((x, y)),2}
    @test @inferred(monomial_type((x, y))) == typeof(x * y)
    @test @inferred(monomial_type((y, x))) == Monomial{typeof((x, y)),2}
    @test @inferred(monomial_type((x, y, z))) == Monomial{typeof((x, y, z)),3}
    @test @inferred(monomial_type((x, y, z))) == typeof(x * y * z)
    @test @inferred(monomial_type((z, x, y))) == Monomial{typeof((x, y, z)),3}

    @test @inferred(monomial_type(typeof((x,)))) == Monomial{typeof((x,)),1}
    @test @inferred(monomial_type(typeof((x, y)))) == Monomial{typeof((x, y)),2}
    @test @inferred(monomial_type(typeof((x, y, z)))) == Monomial{typeof((x, y, z)),3}

    # Test that `monomial_type` on Tuple types is constant-folded,
    # so that branches on it are eliminated at compile time.
    _mt_branch(::Type{V}) where {V} =
        monomial_type(V) === Monomial{typeof((x, y)),2} ? Int : String
    @test @inferred(_mt_branch(typeof((x, y)))) === Int
end

@testset "conversion" begin
    @polyvar x y z

    @test convert(Monomial{typeof((x, y))}, x) == Monomial{typeof((x, y))}((1, 0))
    @test_throws InexactError convert(Monomial{typeof((x,))}, y)
    @test_throws InexactError convert(Monomial{typeof((x,))}, x * y)
end

@testset "variable_union_type" begin
    @polyvar x y z

    function test(p)
        @test MultivariatePolynomials.variable_union_type(p) == TypedPolynomials.Variable
        @test MultivariatePolynomials.variable_union_type(typeof(p)) ==
              TypedPolynomials.Variable
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
