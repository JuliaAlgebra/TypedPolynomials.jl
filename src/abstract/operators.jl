# We reverse the order of comparisons here so that the result
# of x < y is equal to the result of Monomial(x) < Monomial(y)
#@pure isless(v1::TypedVariable, v2::TypedVariable) = name(v1) > name(v2)
#isless(m1::TypedTermLike, m2::TypedTermLike) = isless(promote(m1, m2)...)

# Graded Lexicographic order
# First compare total degree, then lexicographic order
function isless(m1::TypedMonomial{V}, m2::TypedMonomial{V}) where {V}
    d1 = deg(m1)
    d2 = deg(m2)
    if d1 < d2
        return true
    elseif d1 > d2
        return false
    else
        return exponents(m1) < exponents(m2)
    end
end

#function isless(t1::TypedTerm, t2::TypedTerm)
#    if monomial(t1) < monomial(t2)
#        true
#    elseif monomial(t1) == monomial(t2)
#        coefficient(t1) < coefficient(t2)
#    else
#        false
#    end
#end

#for op in [:+, :-]
#    @eval $op(p1::TypedTermLike, p2::TypedTermLike) = $op(polynomial(p1), polynomial(p2))
#    @eval $op(p1::TypedPolynomial, p2::TypedTermLike) = $op(p1, polynomial(p2))
#    @eval $op(p1::TypedTermLike, p2::TypedPolynomial) = $op(polynomial(p1), p2)
#end

#(-)(t::TypedTermLike) = -1 * t

# We call Polynomial since it is already sorted and polynomial sort them
(+)(p1::TypedPolynomial, p2::TypedPolynomial) = Polynomial(jointerms(terms(p1), terms(p2)))
(-)(p1::TypedPolynomial, p2::TypedPolynomial) = Polynomial(jointerms(terms(p1), (-).(terms(p2))))

@pure (==)(::TypedVariable{N}, ::TypedVariable{N}) where {N} = true
@pure (==)(::TypedVariable, ::TypedVariable) = false
(==)(m1::TypedMonomial{V}, m2::TypedMonomial{V}) where {V} = exponents(m1) == exponents(m2)
#function (==)(t1::TypedTerm, t2::TypedTerm)
#    c1 = coefficient(t1)
#    c2 = coefficient(t2)
#    if iszero(c1) && iszero(c2)
#        true
#    else
#        c1 == c2 && monomial(t1) == monomial(t2)
#    end
#end
#(==)(::TypedPolynomialLike, ::Void) = false
#(==)(::Void, ::TypedPolynomialLike) = false

#function compare_terms(p1::TypedPolynomial, p2::TypedPolynomial, op)
#    i1 = 1
#    i2 = 1
#    t1 = terms(p1)
#    t2 = terms(p2)
#    while true
#        while i1 <= length(t1) && coefficient(t1[i1]) == 0
#            i1 += 1
#        end
#        while i2 <= length(t2) && coefficient(t2[i2]) == 0
#            i2 += 1
#        end
#        if i1 > length(t1) && i2 > length(t2)
#            return true
#        end
#        if i1 > length(t1) || i2 > length(t2)
#            return false
#        end
#        if !op(t1[i1], t2[i2])
#            return false
#        end
#        i1 += 1
#        i2 += 1
#    end
#end

#(==)(p1::TypedPolynomial, p2::TypedPolynomial) = compare_terms(p1, p2, (==))

#isapprox(t1::TypedTerm, t2::TypedTerm; kwargs...) = isapprox(coefficient(t1), coefficient(t2); kwargs...) && monomial(t1) == monomial(t2)
#isapprox(p1::TypedPolynomial, p2::TypedPolynomial; kwargs...) = compare_terms(p1, p2, (x, y) -> isapprox(x, y; kwargs...))

#transpose(v::TypedVariable) = v
#transpose(m::TypedMonomial) = m
#transpose(t::T) where {T <: TypedTerm} = coefficient(t)' * monomial(t)
#transpose(p::TypedPolynomial) = polynomial([transpose(t) for t in terms(p)])

#dot(p1::TypedPolynomialLike, p2::TypedPolynomialLike) = p1' * p2
#dot(x, p::TypedPolynomialLike) = x' * p
#dot(p::TypedPolynomialLike, x) = p' * x

#iszero(v::TypedVariable) = false
#iszero(m::TypedMonomial) = false
#iszero(t::TypedTerm) = iszero(coefficient(t))
iszero(p::TypedPolynomial) = all(iszero, terms(p))

## Amazingly, this works! Thanks, StaticArrays.jl!
#"""
#Convert a tuple of variables into a static vector to allow array-like usage.
#The element type of the vector will be Monomial{vars, length(vars)}.
#"""
#vec(vars::Tuple{Vararg{<:TypedVariable}}) = [vars...]
## vec(vars::Tuple{Vararg{<:TypedVariable}}) = SVector(vars)
