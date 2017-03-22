module sptests

using Base.Test
using TypedPolynomials: StaticPolynomials

macro wrappedallocs(expr)
    @assert expr.head == :call
    f = expr.args[1]
    args = expr.args[2:end]
    argnames = [gensym() for a in args]
    quote
        function g($(argnames...))
            @allocated $(Expr(:call, esc(f), argnames...))
        end
        $(Expr(:call, :g, [esc(a) for a in args]...))
    end
end


@testset "variables" begin
    @polyvar x y

    @test x * x == x^2
    @test x^1 == x
#     @test x^0 == 1
    @test (@wrappedallocs x * x) == 0
    @test (@wrappedallocs x^2) == 0
    @test (@wrappedallocs x^1) == 0
#     @test (@wrappedallocs x^0) == 0
end

@testset "orderings" begin
    @polyvar x y z

#     @test Monomial(y) < Monomial(x)
#     @test Monomial(x) > Monomial(y)
#     @test Monomial(y) < x
#     @test y < Monomial(x)
    @test y < x
    @test x > y
#     @test x > Monomial(y)
#     @test Monomial(x) > y
end

@testset "monomials" begin
    @polyvar x y z

    @test typeof(@inferred x * x) == Monomial{(x,), (2,)}
    @test typeof(@inferred x * y) == Monomial{(x, y), (1, 1)}
    @test typeof(@inferred y * x) == Monomial{(x, y), (1, 1)}
    @test typeof(@inferred x * z * y) == Monomial{(x, y, z), (1, 1, 1)}
end

# include("substitution.jl")

end
