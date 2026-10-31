using Test
import TypedPolynomials as TP
import MultivariatePolynomials as MP
import StarAlgebras as SA

@testset "Compact polynomial type display" begin
    TP.@polyvar x y
    for variables in ((), (x,), (x, y)), T in (Int, Float64, ComplexF64)
        monomial = MP.monomial(variables, ntuple(_ -> 2, length(variables)))
        term = MP.term(one(T), monomial)
        polynomial = SA.algebra_element(term)
        coefficients =
            SA.SparseCoefficients((term.index,), (MP.coefficient(term),), MP.ordering(term))
        single = SA.AlgebraElement(coefficients, parent(term))
        V = typeof(variables)
        @test polynomial isa TP.Polynomial{T}
        @test polynomial isa TP.Polynomial{T,V}
        @test term isa TP.Term{T}
        @test term isa TP.Term{T,V}
        @test single isa TP.Polynomial{T}
        @test single isa TP.SingleTermPolynomial{T,V}
        @test !(polynomial isa TP.SingleTermPolynomial)
        for (value, name) in
            ((polynomial, "Polynomial"), (term, "Term"), (single, "SingleTermPolynomial"))
            expected = "TypedPolynomials.$name{$T, $V}"
            @test sprint(show, typeof(value)) == expected
            @test sprint(show, Tuple{typeof(value)}) == "Tuple{$expected}"
            @test sprint(show, Vector{typeof(value)}) == "Vector{$expected}"
        end
    end
    @test sprint(show, typeof(x + x)) != sprint(show, typeof(y + y))
    for (family, name) in (
        (TP.Polynomial, "Polynomial"),
        (TP.Term, "Term"),
        (TP.SingleTermPolynomial, "SingleTermPolynomial"),
    )
        @test sprint(show, family) == "TypedPolynomials.$name"
        @test sprint(show, family{Float64}) == "TypedPolynomials.$name{Float64}"
        V = Tuple{TP.Variable{:x}}
        @test sprint(show, family{Float64,V}) == "TypedPolynomials.$name{Float64, $V}"
        bounded = family{T,V} where {T<:Real}
        @test sprint(show, bounded) == "TypedPolynomials.$name{<:Real, $V}"
        @test sprint(show, Tuple{family{Float64}}) ==
              "Tuple{TypedPolynomials.$name{Float64}}"
    end
    first_type = typeof(x + x)
    second_type = typeof(y + y)
    union_display = sprint(show, Union{first_type,second_type})
    @test startswith(union_display, "Union{")
    @test occursin(sprint(show, first_type), union_display)
    @test occursin(sprint(show, second_type), union_display)
    @test sprint(show, Union{}) == "Union{}"
    @test isempty(filter(m -> m.name == :show, Test.detect_unbound_args(TP)))
end
