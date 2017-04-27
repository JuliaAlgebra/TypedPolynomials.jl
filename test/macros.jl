module M

using Base.Test
using TypedPolynomials: @polyvar, Variable

@testset "macros" begin
    @polyvar x
    @test typeof(x) == Variable{:x}

    @polyvar x y z
    @test typeof(x) == Variable{:x}
    @test typeof(y) == Variable{:y}
    @test typeof(z) == Variable{:z}

    @polyvar a[1:5]
    @test typeof(a1) == Variable{:a1}
    @test typeof(a2) == Variable{:a2}
    @test typeof(a3) == Variable{:a3}
    @test typeof(a4) == Variable{:a4}
    @test typeof(a5) == Variable{:a5}

    @polyvar b c[1:2]
    @test typeof(b) == Variable{:b}
    @test typeof(c1) == Variable{:c1}
    @test typeof(c2) == Variable{:c2}
end

@testset "macro assignments" begin
    x, (y1, y2) = @polyvar(x, y[1:2])
    @test typeof(x) == Variable{:x}
    @test typeof(y1) == Variable{:y1}
    @test typeof(y2) == Variable{:y2}

    a = @polyvar x
    @test typeof(a) == Variable{:x}
end

end

