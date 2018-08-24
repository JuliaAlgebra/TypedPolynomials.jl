module M

using Test
using TypedPolynomials: @polyvar, Variable

@testset "macro tests" begin
    @testset "macros" begin
        @polyvar x
        @test typeof(x) == Variable{:x}

        @polyvar x y z
        @test typeof(x) == Variable{:x}
        @test typeof(y) == Variable{:y}
        @test typeof(z) == Variable{:z}

        @polyvar a[1:3]
        @test typeof(a) == Tuple{Variable{Symbol("a[1]")}, Variable{Symbol("a[2]")}, Variable{Symbol("a[3]")}}

        @polyvar b c[1:2]
        @test typeof(b) == Variable{:b}
        @test typeof(c) == Tuple{Variable{Symbol("c[1]")}, Variable{Symbol("c[2]")}}
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
