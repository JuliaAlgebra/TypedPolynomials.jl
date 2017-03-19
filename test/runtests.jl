using TypedPolynomials
using Base.Test

@testset "promotion" begin
    @polyvar x y z

    # @show promote(Monomial(x), Monomial(x))
    @show TypedPolynomials.promote_rule(typeof(Monomial(x)), typeof(Monomial(y)))
    # @show promote(Monomial(x), Monomial(y))
    # m1, m2 = promote(x, y)
    # m1 = Monomial{1, (x,)}((1,))
    # m2 = Monomial{1, (y,)}((1,))
    # @show TypedPolynomials._promote_monomial(typeof(m1), typeof(m2))
    # @show m1 m2
end

# include("polynomials.jl")
# include("substitution.jl")
include("sequences.jl")
