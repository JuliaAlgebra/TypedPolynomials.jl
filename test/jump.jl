using JuMP: @variable, Model

@testset "JuMP variables" begin
    @polyvar x y z
    m = Model()
    @variable(m, q[1:2])

    p = @inferred q[2] * y + q[1] * x
    @test @inferred(q' * [x, y]) == p
    @test @inferred([x, y]' * q) == p

    @test @inferred((x + y) * p) isa Polynomial{JuMP.AffExpr}

    m = Model()
    @variable(m, Q[1:2, 1:2])
    @test @inferred([x, y]' * Q * [x, y]) == Q[1, 1] * x^2 + Q[2, 1] * x * y + Q[1, 2] * x * y + Q[2, 2] * y^2
end
