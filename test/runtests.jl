using TypedPolynomials
using Base.Test
using StaticArrays: SVector

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

include("promotion.jl")
include("polynomials.jl")
include("substitution.jl")
include("sequences.jl")
