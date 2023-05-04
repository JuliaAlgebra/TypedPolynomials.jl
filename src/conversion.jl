Base.convert(::Type{M}, v::Variable) where {Vars, M <: Monomial{Vars}} = convert(M, Monomial(v))
Base.convert(::Type{Monomial}, v::Variable) = Monomial(v)

Base.convert(T::Type{Monomial{V}}, m::Monomial) where {V} = convert(Monomial{V, nvariables(T)}, m)

@pure function matchindices(::Type{Monomial{V1, N1}}, ::Type{Monomial{V2, N2}}) where {V1, N1, V2, N2}
    i2 = 1
    inds = ntuple(i -> begin
        if i2 > N2
            0
        elseif V1[i] === V2[i2]
            i2 += 1
            i2 - 1
        else
            0
        end
    end, Val{N1}())
    if i2 <= N2
        throw(InexactError(:matchindices, Monomial{V1, N1}, Monomial{V2, N2}))
    end
    inds
end

function Base.convert(::Type{Monomial{V1, N1}}, m::Monomial) where {V1, N1}
    inds = matchindices(Monomial{V1, N1}, typeof(m))
    exps = ntuple(i -> begin
        @inbounds ii = inds[i]
        if ii == 0
            0
        else
            m.exponents[ii]
        end
    end, Val{N1}())
    Monomial{V1, N1}(exps)
end

function Base.convert(::Type{Variable}, mono::Monomial)
    var = _mono2var(mono, powers(mono)...)
    var === nothing && throw(InexactError(:convert, Variable, mono))
    return var
end

function _checknovar(mono) end
function _checknovar(mono, ve, ves...)
    if iszero(ve[2])
        _checknovar(mono, ves...)
    else
        throw(InexactError(:convert, Variable, mono))
    end
end
function _mono2var(mono, ve, ves...)
    if iszero(ve[2])
        _mono2var(mono, ves...)
    elseif isone(ve[2])
        _checknovar(mono, ves...)
        ve[1]
    else
        throw(InexactError(:convert, Variable, mono))
    end
end
_mono2var(mono) = throw(InexactError(:convert, Variable, mono))
