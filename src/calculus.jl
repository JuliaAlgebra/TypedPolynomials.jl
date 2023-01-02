MP.differentiate(::V, ::V) where {V <: Variable} = 1
MP.differentiate(v::Variable, ::Variable) = 0

@pure inmonomial(V) = false
@pure inmonomial(V, Vars...) = (V === first(Vars)) || inmonomial(V, Base.tail(Vars)...)

function MP.differentiate(m::Monomial{Vars}, v::V) where {Vars, V <: Variable}
    if inmonomial(v, Vars...)
        _diff(m, v)
    else
        0
    end
end

_diff(m::Monomial, v::Variable) = _diff(m, exponents(m), v)

function find_variable_index(var::Variable, vars::Tuple, i=1)
    if var === first(vars)
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
            (exponents[i] == 0) ? 0 : exponents[i] - 1
        else
            exponents[i]
        end
    end, Val{N}()))
end

MP.antidifferentiate(v1::V, v2::V) where {V <: Variable} = v1*v2/2
MP.antidifferentiate(v1::Variable, v2::Variable) = v1 * v2

function MP.antidifferentiate(m::Monomial{Vars}, v::V) where {Vars, V <: Variable}
    if inmonomial(v, Vars...)
        return _antidiff(m, v)
    else
        # Insert `v` in the monomial
        return m * v
    end
end

_antidiff(m::Monomial, v::Variable) = _antidiff(m, exponents(m), v)

function _antidiff(::Monomial{Vars},
               exponents::NTuple{N, Integer},
               v::Variable) where {Vars, N}
    vi = find_variable_index(v, Vars)
    new_m = Monomial{Vars,N}(
        ntuple(i -> begin
                if i == vi
                    (exponents[i] == 0) ? 1 : exponents[i] + 1
                else
                    exponents[i]
                end
            end,
            Val{N}()
        )
    )
    # Remark : as `exponents` are imposed to be `Integer` according to
    # the method signature, we can use the Rational{Int} type here for the
    # coefficient
    return (1 // (exponents[vi] + 1)) * new_m
end
