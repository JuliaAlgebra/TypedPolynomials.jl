if !@isdefined(TypedPolynomials)
    include("utils.jl")
end

module M

using Test
using TypedPolynomials: @polyvar, Variable
import MultivariatePolynomials as MP

const G = MP.Graded{MP.LexOrder}

const scalar_result = @polyvar scalar constant = true
const indexed_result = @polyvar indexed[1:3] constant = true
const multiple_result = @polyvar c cs[1:2] constant = true
const nonconstant_result = @polyvar nonconstant constant = false
@polyvar default_nonconstant

@testset "macro tests" begin
    @testset "constant bindings" begin
        for name in (:scalar, :indexed, :c, :cs)
            @test isconst(@__MODULE__, name)
        end
        @test !isconst(@__MODULE__, :nonconstant)
        @test !isconst(@__MODULE__, :default_nonconstant)
        @test scalar_result === scalar
        @test scalar_result isa Variable{:scalar,G}
        @test indexed_result === indexed
        @test indexed_result isa Tuple{Vararg{Variable}}
        @test length(indexed_result) == 3
        @test multiple_result === (c, cs)
        @test cs isa Tuple{Vararg{Variable}}
        @test nonconstant_result === nonconstant
        @test_throws ErrorException macroexpand(
            @__MODULE__,
            :(@polyvar invalid constant = 1),
        )
        @test_throws ErrorException macroexpand(
            @__MODULE__,
            :(@polyvar invalid unsupported = true),
        )
    end

    @testset "monomial order" begin
        @polyvar x y
        @test MP.ordering(x) == MP.Graded(MP.LexOrder())
        @test MP.ordering(x * y) == MP.Graded(MP.LexOrder())
        @test Variable{:x}() === x
        @polyvar a b monomial_order = MP.LexOrder
        @test typeof(a) == Variable{:a,MP.LexOrder}
        @test MP.ordering(a) == MP.LexOrder()
        @test MP.ordering(a * b^2) == MP.LexOrder()
        @test MP.ordering(2a + b) == MP.LexOrder()
        @test a * b^2 < a^2
        @test x * y^2 > x^2
        @test MP.monomials((a, b), 0:2) == [1, b, b^2, a, a * b, a^2]
        @test MP.monomials((x, y), 0:2) == [1, y, x, y^2, x * y, x^2]
        @test MP.terms(a + a * b^2 + b^3) == [b^3, a, a * b^2]
        @polyvar c[1:2] monomial_order = MP.Graded{MP.InverseLexOrder}
        @test MP.ordering(c[1] * c[2]) == MP.Graded(MP.InverseLexOrder())
        @test Variable{:x,MP.LexOrder}() != x
        @test_throws ArgumentError MP.ordering(a * x)
    end

    @testset "macros" begin
        @polyvar x
        @test typeof(x) == Variable{:x,G}

        @polyvar x y z
        @test typeof(x) == Variable{:x,G}
        @test typeof(y) == Variable{:y,G}
        @test typeof(z) == Variable{:z,G}

        @polyvar a[1:3]
        @test typeof(a) == Tuple{
            Variable{Symbol("a[1]"),G},
            Variable{Symbol("a[2]"),G},
            Variable{Symbol("a[3]"),G},
        }

        @polyvar b c[1:2]
        @test typeof(b) == Variable{:b,G}
        @test typeof(c) == Tuple{Variable{Symbol("c[1]"),G},Variable{Symbol("c[2]"),G}}
    end

    @testset "macro assignments" begin
        x, (y1, y2) = @polyvar(x, y[1:2])
        @test typeof(x) == Variable{:x,G}
        @test typeof(y1) == Variable{Symbol("y[1]"),G}
        @test typeof(y2) == Variable{Symbol("y[2]"),G}

        a = @polyvar x
        @test typeof(a) == Variable{:x,G}
    end
end

end
