@testset "promotion" begin
    @polyvar x y z

    @test typeof(@inferred promote(1, y)) == NTuple{2, Term{Int, Monomial{(y,), 1}}}
    @test typeof(@inferred promote(1, y^2)) == NTuple{2, Term{Int, Monomial{(y,), 1}}}
    @test typeof(@inferred promote(1, 2y)) == NTuple{2, Term{Int, Monomial{(y,), 1}}}
    @test typeof(@inferred promote(1, Polynomial(2y))) == NTuple{2, Polynomial{Term{Int, Monomial{(y,), 1}}, Vector{Term{Int, Monomial{(y,), 1}}}}}
    @test typeof(@inferred promote(1, Polynomial([2y]))) == NTuple{2, Polynomial{Term{Int, Monomial{(y,), 1}}, Vector{Term{Int, Monomial{(y,), 1}}}}}

    @test typeof(@inferred promote(x, 1)) == NTuple{2, Term{Int, Monomial{(x,), 1}}}
    @test typeof(@inferred promote(x, y)) == NTuple{2, Monomial{(x, y), 2}}
    @test typeof(@inferred promote(y, x)) == NTuple{2, Monomial{(x, y), 2}}
    @test typeof(@inferred promote(x, y^2)) == NTuple{2, Monomial{(x, y), 2}}
    @test typeof(@inferred promote(x, 2y)) == NTuple{2, Term{Int, Monomial{(x, y), 2}}}
    @test typeof(@inferred promote(x, Polynomial(2y))) == NTuple{2, Polynomial{Term{Int, Monomial{(x, y), 2}}, SVector{1, Term{Int, Monomial{(x, y), 2}}}}}
    @test typeof(@inferred promote(x, Polynomial([2y]))) == NTuple{2, Polynomial{Term{Int, Monomial{(x, y), 2}}, Vector{Term{Int, Monomial{(x, y), 2}}}}}

    @test typeof(@inferred promote(x^2, 1)) == NTuple{2, Term{Int, Monomial{(x,), 1}}}
    @test typeof(@inferred promote(x^2, y)) == NTuple{2, Monomial{(x, y), 2}}
    @test typeof(@inferred promote(y, x^2)) == NTuple{2, Monomial{(x, y), 2}}
    @test typeof(@inferred promote(x^2, y^2)) == NTuple{2, Monomial{(x, y), 2}}
    @test typeof(@inferred promote(x^2, 2y)) == NTuple{2, Term{Int, Monomial{(x, y), 2}}}
    @test typeof(@inferred promote(x^2, Polynomial(2y))) == NTuple{2, Polynomial{Term{Int, Monomial{(x, y), 2}}, SVector{1, Term{Int, Monomial{(x, y), 2}}}}}
    @test typeof(@inferred promote(x^2, Polynomial([2y]))) == NTuple{2, Polynomial{Term{Int, Monomial{(x, y), 2}}, Vector{Term{Int, Monomial{(x, y), 2}}}}}

    @test typeof(@inferred promote(2x, 1)) == NTuple{2, Term{Int, Monomial{(x,), 1}}}
    @test typeof(@inferred promote(2x, y)) == NTuple{2, Term{Int, Monomial{(x, y), 2}}}
    @test typeof(@inferred promote(2x, y^2)) == NTuple{2, Term{Int, Monomial{(x, y), 2}}}
    @test typeof(@inferred promote(2x, 2y)) == NTuple{2, Term{Int, Monomial{(x, y), 2}}}
    @test typeof(@inferred promote(2x, Polynomial(2y))) == NTuple{2, Polynomial{Term{Int, Monomial{(x, y), 2}}, SVector{1, Term{Int, Monomial{(x, y), 2}}}}}
    @test typeof(@inferred promote(2x, Polynomial([2y]))) == NTuple{2, Polynomial{Term{Int, Monomial{(x, y), 2}}, Vector{Term{Int, Monomial{(x, y), 2}}}}}

    @test typeof(@inferred promote(Polynomial(2x), 1)) == NTuple{2, Polynomial{Term{Int, Monomial{(x,), 1}}, Vector{Term{Int, Monomial{(x,), 1}}}}}
    @test typeof(@inferred promote(Polynomial(2x), y)) == NTuple{2, Polynomial{Term{Int, Monomial{(x, y), 2}}, SVector{1, Term{Int, Monomial{(x, y), 2}}}}}
    @test typeof(@inferred promote(Polynomial(2x), y^2)) == NTuple{2, Polynomial{Term{Int, Monomial{(x, y), 2}}, SVector{1, Term{Int, Monomial{(x, y), 2}}}}}
    @test typeof(@inferred promote(Polynomial(2x), 2y)) == NTuple{2, Polynomial{Term{Int, Monomial{(x, y), 2}}, SVector{1, Term{Int, Monomial{(x, y), 2}}}}}
    @test typeof(@inferred promote(Polynomial(2x), Polynomial(2y))) == NTuple{2, Polynomial{Term{Int, Monomial{(x, y), 2}}, SVector{1, Term{Int, Monomial{(x, y), 2}}}}}
    @test typeof(@inferred promote(Polynomial(2x), Polynomial([2y]))) == NTuple{2, Polynomial{Term{Int, Monomial{(x, y), 2}}, Vector{Term{Int, Monomial{(x, y), 2}}}}}
end

@testset "conversion" begin
    @polyvar x y z
    
    @test convert(Monomial{(x, y)}, x) == Monomial{(x, y)}((1, 0))
    @test_throws InexactError convert(Monomial{(x,)}, y)
    @test_throws InexactError convert(Monomial{(x,)}, x * y)
end
