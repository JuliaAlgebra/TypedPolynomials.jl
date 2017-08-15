Base.convert(::Type{M}, v::Variable) where {Vars, M <: Monomial{Vars}} = convert(M, Monomial(v))
Base.convert(::Type{Term{T, M1}}, m::Monomial) where {T, M1} = Term(one(T), convert(M1, m))
Base.convert(::Type{Term{T, M}}, v::Variable) where {T, M} = Term(one(T), convert(M, v))
Base.convert(T::Type{Polynomial{C1, T1, V1}}, p::Polynomial) where {C1, T1, V1} = T(convert(V1, p.terms))
# Break ambibuity
Base.convert(T::Type{Polynomial{C1, T1, V1}}, x::MP.AbstractTermLike) where {C1, T1, V1} = convert(T, Polynomial(convert(T1, x)))

MP.convertconstant(T::Type{Term{T1, M1}}, α) where {T1, M1} = T(convert(T1, α), M1())
MP.convertconstant(T::Type{Polynomial{C1, T1, V1}}, α) where {C1, T1, V1} = convert(T, Polynomial(convert(T1, α)))

Base.convert(T::Type{Monomial{V}}, m::Monomial) where {V} = convert(Monomial{V, nvariables(T)}, m)

@pure function matchindices(::Type{Monomial{V1, N1}}, ::Type{Monomial{V2, N2}}) where {V1, N1, V2, N2}
    i2 = 1
    inds = ntuple(i -> begin
        if i2 > N2
            0
        elseif V1[i] == V2[i2]
            i2 += 1
            i2 - 1
        else
            0
        end
    end, Val{N1})
    if i2 <= N2
        throw(InexactError())
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
    end, Val{N1})
    Monomial{V1, N1}(exps)
end

Base.convert(::Type{Term{T1, M1}}, t::Term) where {T1, M1} = Term{T1, M1}(convert(T1, t.coefficient), convert(M1, t.monomial))
#MP.term(coeff, mono::Monomial) = Term(coeff, mono)
MP.polynomial(t::TermLike) = Polynomial(t)
MP.polynomial(v::AbstractVector{<:Term}, ::MP.SortedUniqState) = Polynomial(v)
function MP.polynomial(p::P, ::Type{C}) where {P<:TypedPolynomialLike, C}
    convert(polynomialtype(P, C), p)
end
