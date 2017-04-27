differentiate(::V, ::V) where {V <: AbstractVariable} = 1
differentiate(v::AbstractVariable, ::AbstractVariable) = 0

@pure inmonomial(V) = false
@pure inmonomial(V, Vars...) = name(V) == name(Vars[1]) || inmonomial(V, Base.tail(Vars)...)

function differentiate(m::AbstractMonomial{Vars}, v::V) where {Vars, V <: AbstractVariable}
    if inmonomial(V, Vars...)
        _diff(m, v)
    else
        0
    end
end

_diff(m::AbstractMonomial, v::AbstractVariable) = _diff(m, exponents(m), v)

function _diff(m::AbstractMonomial{Vars}, 
               exponents::NTuple{N, Integer},
               v::AbstractVariable) where {Vars, N}
    vi = findfirst(var -> name(var) == name(v), Vars)
    @assert vi != 0
    exponents[vi] * Monomial{Vars, N}(ntuple(i -> begin
        if i == vi
            exponents[i] - 1
        else
            exponents[i]
        end
    end, Val{N}))
end

differentiate(t::AbstractTerm, v::AbstractVariable) = coefficient(t) * differentiate(monomial(t), v)
differentiate(p::AbstractPolynomial, v::AbstractVariable) = Polynomial(differentiate.(terms(p), v))

# Fallback for everything else
differentiate(x, v::AbstractVariable) = 0