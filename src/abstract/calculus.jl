differentiate(::V, ::V) where {V <: AbstractVariable} = 1
differentiate(v::AbstractVariable, ::AbstractVariable) = 0

@pure inmonomial(V) = false
@pure inmonomial(V, Vars...) = name(V) == name(Vars[1]) || inmonomial(V, Base.tail(Vars)...)

function differentiate(m::AbstractMonomial{Vars}, v::V) where {Vars, V <: AbstractVariable}
    if inmonomial(V, Vars...)
        _diff(v, powers(m)...)
    else
        0
    end
end

_diff(v::V, p::Tuple{V, Any}, p2...) where {V <: AbstractVariable} = p[2] * v^(p[2] - 1) * _diff(v, p2...)
_diff(v::AbstractVariable, p::Tuple{AbstractVariable, Any}, p2...) = p[1]^p[2] * _diff(v, p2...)
_diff(v::AbstractVariable) = 1

differentiate(t::AbstractTerm, v::AbstractVariable) = coefficient(t) * differentiate(monomial(t), v)
differentiate(p::AbstractPolynomial, v::AbstractVariable) = Polynomial(differentiate.(terms(p), v))

# Fallback for everything else
differentiate(x, v::AbstractVariable) = 0