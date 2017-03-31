convert(::Type{M}, v::Variable) where {Vars, M <: Monomial{Vars}} = convert(M, Monomial(v))
convert(T::Type{Term{T1, M1}}, x) where {T1, M1} = T(convert(T1, x), M1())
convert(::Type{Term{T, M1}}, m::Monomial) where {T, M1} = Term{T, M1}(one(T), convert(M1, m))
convert(::Type{Term{T, M}}, v::Variable) where {T, M} = Term{T, M}(one(T), convert(M, v))
convert(T::Type{Polynomial{T1, V1}}, p::Polynomial) where {T1, V1} = T(convert(V1, p.terms))
convert(T::Type{Polynomial{T1, V1}}, x) where {T1, V1} = convert(T, Polynomial(convert(T1, x)))

convert(T::Type{Monomial{V}}, m::Monomial) where {V} = convert(Monomial{V, numvariables(T)}, m)

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

function convert(::Type{Monomial{V1, N1}}, m::Monomial) where {V1, N1}
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

function convert(::Type{Term{T1, M1}}, t::Term{T2, M2}) where {T1, M1, T2, M2}
    Term{T1, M1}(convert(T1, t.coefficient), convert(M1, t.monomial))
end

convert(::Type{AbstractTerm}, coeff, mono::Monomial) = Term(coeff, mono)
convert(::Type{AbstractPolynomial}, t::TermLike) = Polynomial(t)
convert(::Type{AbstractPolynomial}, v::AbstractVector{<:Term}) = Polynomial(v)
