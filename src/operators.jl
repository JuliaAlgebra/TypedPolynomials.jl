one(::Type{Monomial{N, V}}) where {N, V} = Monomial{N, V}()
zero(::Type{Term{T, M}}) where {T, M} = Term{T, M}(0, M())
zero(t::TermLike) = zero(typeof(t))

isless(::Type{Variable{N1}}, ::Type{Variable{N2}}) where {N1, N2} = N1 < N2
isless(v1::Variable, v2::Variable) = typeof(v1) < typeof(v2)

# Graded Lexicographic order
# First compare total degree, then lexicographic order
isless(m1::Monomial, m2::Monomial) = isless(promote(m1, m2)...)

function isless(m1::M, m2::M) where {M <: Monomial}
    d1 = degree(m1)
    d2 = degree(m2)
    if d1 < d2
        return true
    elseif d1 > d2
        return false
    else
        for i in 1:length(variables(M))
            if m1.exponents[i] < m2.exponents[i]
                return true
            elseif m1.exponents[i] > m2.exponents[i]
                return false
            end
        end
    end
    false
end

isless(t1::Term, t2::Term) = t1.monomial < t2.monomial

function jointerms(terms1::AbstractArray{<:Term}, terms2::AbstractArray{<:Term})
    T = promote_type(eltype(terms1), eltype(terms2))
    terms = Vector{T}(length(terms1) + length(terms2))
    i = 1
    i1 = 1
    i2 = 1
    deletions = 0
    while i1 <= length(terms1) && i2 <= length(terms2)
        t1 = convert(T, terms1[i1])
        t2 = convert(T, terms2[i2])
        if t1.monomial < t2.monomial
            terms[i] = t1
            i1 += 1
        elseif t1.monomial > t2.monomial
            terms[i] = t2
            i2 += 1
        else
            terms[i] = Term(t1.coefficient + t2.coefficient,
                            t1.monomial)
            i1 += 1
            i2 += 1
            deletions += 1
        end
        i += 1
    end
    for j in i1:length(terms1)
        terms[i] = terms1[j]
        i += 1
    end
    for j in i2:length(terms2)
        terms[i] = terms2[j]
        i += 1
    end
    resize!(terms, length(terms) - deletions)
end

for op in [:+, :*, :-, :(==)]
    @eval $op(p1::PolynomialLike, p2::PolynomialLike) = $op(promote(p1, p2)...)
    @eval $op(p::PolynomialLike, x) = $op(promote(p, x)...)
    @eval $op(x, p::PolynomialLike) = $op(promote(x, p)...)
end

(+)(p1::TermLike, p2::TermLike) = Polynomial(p1) + Polynomial(p2)
(+)(p1::Polynomial, p2::Polynomial) = Polynomial(jointerms(p1.terms, p2.terms))

(-)(t::TermLike) = -1 * t
(-)(p1::TermLike, p2::TermLike) = Polynomial(p1) - Polynomial(p2)
(-)(p1::Polynomial, p2::Polynomial) = Polynomial(jointerms(p1.terms, (-).(p2.terms)))

(*)(v1::V, v2::V) where {V <: Variable} = Monomial{1, (V(),)}((2,))

@generated function (*)(v1::V1, v2::V2) where {V1 <: Variable, V2 <: Variable}
    if V1() < V2()
        :(Monomial{2, (V1(), V2())}((1, 1)))
    else
        :(Monomial{2, (V2(), V1())}((1, 1)))
    end
end

@generated function (*)(m1::M, m2::M) where {M <: Monomial}
    vars = variables(M)
    :(Monomial{$(length(vars)), $vars}($(Expr(:tuple, [:(m1.exponents[$i] + m2.exponents[$i]) for i in 1:length(vars)]...))))
end

(*)(t1::Term, t2::Term) = Term(t1.coefficient * t2.coefficient, t1.monomial * t2.monomial)

# TODO: this is inefficient
(*)(p1::Polynomial, p2::Polynomial) = sum(p1.terms .* p2.terms.')

(*)(t::PolynomialLike) = t

(==)(v1::Variable, v2::Variable) = name(v1) == name(v2)
(==)(m1::M, m2::M) where {M <: Monomial} = variables(m1) == variables(m2) && exponents(m1) == exponents(m2)
(==)(t1::T, t2::T) where {T <: Term} = coefficient(t1) == coefficient(t2) && monomial(t1) == monomial(t2)

^(v::V, x::Integer) where {V <: Variable} = Monomial{1, (V(),)}((x,))

transpose(v::Variable) = v
transpose(m::Monomial) = m
transpose(t::Term) = Term(t.coefficient', t.monomial)
