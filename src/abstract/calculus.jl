MP.differentiate(::V, ::V) where {V <: TypedVariable} = 1
MP.differentiate(v::TypedVariable, ::TypedVariable) = 0

@pure inmonomial(V) = false
@pure inmonomial(V, Vars...) = name(V) == name(Vars[1]) || inmonomial(V, Base.tail(Vars)...)

function MP.differentiate(m::TypedMonomial{Vars}, v::V) where {Vars, V <: TypedVariable}
    if inmonomial(V, Vars...)
        _diff(m, v)
    else
        0
    end
end

_diff(m::TypedMonomial, v::TypedVariable) = _diff(m, exponents(m), v)

function _diff(m::TypedMonomial{Vars},
               exponents::NTuple{N, Integer},
               v::TypedVariable) where {Vars, N}
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
