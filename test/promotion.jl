@testset "promotion" begin
    @polyvar x y z

    @test typeof(@inferred promote(1, y)) == NTuple{2, Term{Int, Monomial{1, (y,)}}}
    @test typeof(@inferred promote(1, y^2)) == NTuple{2, Term{Int, Monomial{1, (y,)}}}
    @test typeof(@inferred promote(1, 2y)) == NTuple{2, Term{Int, Monomial{1, (y,)}}}
    @test typeof(@inferred promote(1, Polynomial(2y))) == NTuple{2, Polynomial{Term{Int, Monomial{1, (y,)}}, Vector{Term{Int, Monomial{1, (y,)}}}}}
    @test typeof(@inferred promote(1, Polynomial([2y]))) == NTuple{2, Polynomial{Term{Int, Monomial{1, (y,)}}, Vector{Term{Int, Monomial{1, (y,)}}}}}

    @test typeof(@inferred promote(x, 1)) == NTuple{2, Term{Int, Monomial{1, (x,)}}}
    @test typeof(@inferred promote(x, y)) == NTuple{2, Monomial{2, (x, y)}}
    @test typeof(@inferred promote(y, x)) == NTuple{2, Monomial{2, (x, y)}}
    @test typeof(@inferred promote(x, y^2)) == NTuple{2, Monomial{2, (x, y)}}
    @test typeof(@inferred promote(x, 2y)) == NTuple{2, Term{Int, Monomial{2, (x, y)}}}
    @test typeof(@inferred promote(x, Polynomial(2y))) == NTuple{2, Polynomial{Term{Int, Monomial{2, (x, y)}}, SVector{1, Term{Int, Monomial{2, (x, y)}}}}}
    @test typeof(@inferred promote(x, Polynomial([2y]))) == NTuple{2, Polynomial{Term{Int, Monomial{2, (x, y)}}, Vector{Term{Int, Monomial{2, (x, y)}}}}}

    @test typeof(@inferred promote(x^2, 1)) == NTuple{2, Term{Int, Monomial{1, (x,)}}}
    @test typeof(@inferred promote(x^2, y)) == NTuple{2, Monomial{2, (x, y)}}
    @test typeof(@inferred promote(y, x^2)) == NTuple{2, Monomial{2, (x, y)}}
    @test typeof(@inferred promote(x^2, y^2)) == NTuple{2, Monomial{2, (x, y)}}
    @test typeof(@inferred promote(x^2, 2y)) == NTuple{2, Term{Int, Monomial{2, (x, y)}}}
    @test typeof(@inferred promote(x^2, Polynomial(2y))) == NTuple{2, Polynomial{Term{Int, Monomial{2, (x, y)}}, SVector{1, Term{Int, Monomial{2, (x, y)}}}}}
    @test typeof(@inferred promote(x^2, Polynomial([2y]))) == NTuple{2, Polynomial{Term{Int, Monomial{2, (x, y)}}, Vector{Term{Int, Monomial{2, (x, y)}}}}}

    @test typeof(@inferred promote(2x, 1)) == NTuple{2, Term{Int, Monomial{1, (x,)}}}
    @test typeof(@inferred promote(2x, y)) == NTuple{2, Term{Int, Monomial{2, (x, y)}}}
    @test typeof(@inferred promote(2x, y^2)) == NTuple{2, Term{Int, Monomial{2, (x, y)}}}
    @test typeof(@inferred promote(2x, 2y)) == NTuple{2, Term{Int, Monomial{2, (x, y)}}}
    @test typeof(@inferred promote(2x, Polynomial(2y))) == NTuple{2, Polynomial{Term{Int, Monomial{2, (x, y)}}, SVector{1, Term{Int, Monomial{2, (x, y)}}}}}
    @test typeof(@inferred promote(2x, Polynomial([2y]))) == NTuple{2, Polynomial{Term{Int, Monomial{2, (x, y)}}, Vector{Term{Int, Monomial{2, (x, y)}}}}}

    @test typeof(@inferred promote(Polynomial(2x), 1)) == NTuple{2, Polynomial{Term{Int, Monomial{1, (x,)}}, Vector{Term{Int, Monomial{1, (x,)}}}}}
    @test typeof(@inferred promote(Polynomial(2x), y)) == NTuple{2, Polynomial{Term{Int, Monomial{2, (x, y)}}, SVector{1, Term{Int, Monomial{2, (x, y)}}}}}
    @test typeof(@inferred promote(Polynomial(2x), y^2)) == NTuple{2, Polynomial{Term{Int, Monomial{2, (x, y)}}, SVector{1, Term{Int, Monomial{2, (x, y)}}}}}
    @test typeof(@inferred promote(Polynomial(2x), 2y)) == NTuple{2, Polynomial{Term{Int, Monomial{2, (x, y)}}, SVector{1, Term{Int, Monomial{2, (x, y)}}}}}
    @test typeof(@inferred promote(Polynomial(2x), Polynomial(2y))) == NTuple{2, Polynomial{Term{Int, Monomial{2, (x, y)}}, SVector{1, Term{Int, Monomial{2, (x, y)}}}}}
    @test typeof(@inferred promote(Polynomial(2x), Polynomial([2y]))) == NTuple{2, Polynomial{Term{Int, Monomial{2, (x, y)}}, Vector{Term{Int, Monomial{2, (x, y)}}}}}
end
