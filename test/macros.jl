if !@isdefined(TypedPolynomials)
    include("utils.jl")
end

module M

using Test
using TypedPolynomials: @polyvar, Variable

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
        @test scalar_result isa Variable{:scalar}
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

    @testset "macros" begin
        @polyvar x
        @test typeof(x) == Variable{:x}

        @polyvar x y z
        @test typeof(x) == Variable{:x}
        @test typeof(y) == Variable{:y}
        @test typeof(z) == Variable{:z}

        @polyvar a[1:3]
        @test typeof(a) == Tuple{
            Variable{Symbol("a[1]")},
            Variable{Symbol("a[2]")},
            Variable{Symbol("a[3]")},
        }

        @polyvar b c[1:2]
        @test typeof(b) == Variable{:b}
        @test typeof(c) == Tuple{Variable{Symbol("c[1]")},Variable{Symbol("c[2]")}}
    end

    @testset "macro assignments" begin
        x, (y1, y2) = @polyvar(x, y[1:2])
        @test typeof(x) == Variable{:x}
        @test typeof(y1) == Variable{Symbol("y[1]")}
        @test typeof(y2) == Variable{Symbol("y[2]")}

        a = @polyvar x
        @test typeof(a) == Variable{:x}
    end
end

end
