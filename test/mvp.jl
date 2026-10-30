import TypedPolynomials
import MultivariatePolynomials

const mvp_test = joinpath(dirname(pathof(MultivariatePolynomials)), "..", "test")
const Mod = TypedPolynomials

include(joinpath(mvp_test, "utils.jl"))
include(joinpath(mvp_test, "commutativetests.jl"))
