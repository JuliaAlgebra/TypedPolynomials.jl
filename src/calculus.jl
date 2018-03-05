MP.differentiate(::V, ::V) where {V <: Variable} = 1
MP.differentiate(v::Variable, ::Variable) = 0

@pure inmonomial(V) = false
@pure inmonomial(V, Vars...) = name(V) == name(Vars[1]) || inmonomial(V, Base.tail(Vars)...)

function MP.differentiate(m::Monomial{Vars}, v::V) where {Vars, V <: Variable}
    if inmonomial(V, Vars...)
        _diff(m, v)
    else
        0
    end
end

_diff(m::Monomial, v::Variable) = _diff(m, exponents(m), v)

function find_variable_index(var::Variable, vars::Tuple, i=1)
    if name(var) == name(first(vars))
        return i
    else
        return find_variable_index(var, Base.tail(vars), i + 1)
    end
end

find_variable_index(v::Variable, ::Tuple{}, i) = error("Could not find variable $v")

function _diff(m::Monomial{Vars},
               exponents::NTuple{N, Integer},
               v::Variable) where {Vars, N}
    vi = find_variable_index(v, Vars)
    # vi = findfirst(var -> name(var) == name(v), Vars)
    exponents[vi] * Monomial{Vars, N}(ntuple(i -> begin
        if i == vi
            exponents[i] - 1
        else
            exponents[i]
        end
    end, Val{N}()))
end
