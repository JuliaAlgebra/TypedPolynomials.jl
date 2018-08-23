using MultivariatePolynomials
using TypedPolynomials

using Test
using Random
using Pkg
using LinearAlgebra
using NBInclude

"""
    @wrappedallocs(expr)

Given an expression, this macro wraps that expression inside a new function
which will evaluate that expression and measure the amount of memory allocated
by the expression. Wrapping the expression in a new function allows for more
accurate memory allocation detection when using global variables (e.g. when
at the REPL).

For example, `@wrappedallocs(x + y)` produces:

```julia
function g(x1, x2)
    @allocated x1 + x2
end
g(x, y)
```

You can use this macro in a unit test to verify that a function does not
allocate:

```
@test @wrappedallocs(x + y) == 0
```
"""
macro wrappedallocs(expr)
    argnames = [gensym() for a in expr.args]
    quote
        function g($(argnames...))
            @allocated $(Expr(expr.head, argnames...))
        end
        $(Expr(:call, :g, [esc(a) for a in expr.args]...))
    end
end



const mvp_test = joinpath(dirname(pathof(MultivariatePolynomials)), "..", "test")
const Mod = TypedPolynomials

@testset "TypedPolynomials" begin
    # include("jump.jl")
    include("generators.jl")
    include("macros.jl")
    include("calculus.jl")
    include("promotion.jl")
    include("polynomials.jl")
    include("substitution.jl")
    include("sequences.jl")
end

@testset "MultivariatePolynomials interface tests" begin
    include(joinpath(mvp_test, "utils.jl"))
    include(joinpath(mvp_test, "commutativetests.jl"))
end

@testset "Demo" begin
    @nbinclude(joinpath(@__DIR__, "..", "demo.ipynb"))
end
