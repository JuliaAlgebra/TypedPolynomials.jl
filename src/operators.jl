one(::Type{V}) where {V <: Variable} = Monomial{(V(),), 1}()
one(::Type{M}) where {M <: Monomial} = M()
one(m::MonomialLike) = one(typeof(m))
one(::Type{Term{T, M}}) where {T, M} = Term(one(T), M())
one(t::Term) = one(typeof(t))
one(::Type{P}) where {T, P <: Polynomial{T}} = Polynomial(one(T))
one(p::Polynomial) = one(typeof(p))

zero(::Type{V}) where {V <: Variable} = Polynomial([Term(0, Monomial(V()))])
zero(::Type{M}) where {M <: Monomial} = Polynomial([Term(0, M())])
zero(::Type{Term{T, M}}) where {T, M} = Polynomial([Term{T, M}(zero(T), M())])
zero(::Type{Polynomial{T, A}}) where {T, A} = zero(T)
zero(t::PolynomialLike) = zero(typeof(t))

combine(t1::Term, t2::Term) = combine(promote(t1, t2)...)
combine(t1::T, t2::T) where {T <: Term} = Term(t1.coefficient + t2.coefficient, t1.monomial)
compare(t1::Term, t2::Term) = monomial(t1) < monomial(t2)

jointerms(terms1::AbstractArray{<:Term}, terms2::AbstractArray{<:Term}) = mergesorted(terms1, terms2, compare, combine)

# Multiplication is handled as a special case so that we can write these
# definitions without resorting to promotion:
(*)(x, v::MonomialLike) = Term(x, Monomial(v))
(*)(v::MonomialLike, x) = Term(x, Monomial(v))

for T1 in [Variable, Monomial, Term, Polynomial]
    for T2 in [Variable, Monomial, Term, Polynomial]
        if T1 != T2
            @eval (*)(x1::$T1, x2::$T2) = (*)(promote(x1, x2)...)
        end
    end
end

(*)(v1::V, v2::V) where {V <: Variable} = Monomial{(V(),), 1}((2,))
(*)(v1::Variable, v2::Variable) = (*)(promote(v1, v2)...)

function (*)(m1::Monomial{V, N}, m2::Monomial{V, N}) where {V, N}
    e1 = m1.exponents
    e2 = m2.exponents
    Monomial{V, N}(ntuple(i -> e1[i] + e2[i], Val{N}))
end

(*)(v1::Monomial, v2::Monomial) = (*)(promote(v1, v2)...)
(*)(t1::Term, t2::Term) = Term(coefficient(t1) * coefficient(t2), monomial(t1) * monomial(t2))
(*)(p1::P1, p2::P2) where {P1 <: Polynomial, P2 <: Polynomial} = (*)(promote(p1, p2)...)

function add!(p::Polynomial, t::TermLike)
    for i in 1:length(p.terms)
        ti = p.terms[i]
        if t == ti
            p.terms[i] = Term(coefficient(t) + coefficient(ti), monomial(t))
            return
        elseif t < ti
            insert!(p.terms, i, t)
            return
        end
    end
    push!(p.terms, t)
    return
end

function (*)(p1::P, p2::P) where {P <: Polynomial}
    C = coefftype(termtype(P))
    M = monomialtype(termtype(P))
    result = Polynomial([Term(zero(C), M())])
    for t1 in terms(p1)
        for t2 in terms(p2)
            expected = result + t1 * t2
            add!(result, t1 * t2)
            @assert result == expected
        end
    end
    result
end

(*)(x, t::Term) = Term(x * coefficient(t), monomial(t))
(*)(t::Term, x) = Term(coefficient(t) * x, monomial(t))
(*)(p::Polynomial, x) = (*)(promote(p, x)...)
(*)(x, p::Polynomial) = (*)(promote(x, p)...)

^(v::V, x::Integer) where {V <: Variable} = Monomial{(V(),), 1}((x,))

# All of these types are immutable, so there's no need to copy anything to get
# a shallow copy.
copy(x::PolynomialLike) = x
