struct Variable{Name} <: AbstractVariable{Name}
end

checksorted(x::Tuple{Any}, cmp) = true
checksorted(x::Tuple{}, cmp) = true
checksorted(x::Tuple, cmp) = cmp(x[1], x[2]) && checksorted(Base.tail(x), cmp)

struct Monomial{V, N} <: AbstractMonomial{V}
    exponents::NTuple{N, Int}

    Monomial{V, N}(exponents::NTuple{N, Int}) where {V, N} = (@assert checksorted(V, >); new{V, N}(exponents))
    Monomial{V}(exponents::NTuple{N, Int}) where {V, N} = (@assert checksorted(V, >); new{V, N}(exponents))
end

Monomial{V, N}() where {N, V} = Monomial{V, N}(ntuple(_ -> 0, Val{N}))
Monomial{V}() where {V} = Monomial{V, 0}()
Monomial{V}(exponents::T) where {V, N, T <: Tuple{Vararg{Any, N}}} = Monomial{V, N}(exponents)
Monomial(v::Variable) = Monomial{(v,), 1}((1,))
Monomial{V}(exponents::AbstractVector{<:Integer}) where {V} = Monomial{V, length(V)}(NTuple{length(V), Int}(exponents))

exponents(m::Monomial) = m.exponents
exponent(m::Monomial, i::Integer) = m.exponents[i]

struct Term{CoeffType, M <: Monomial} <: AbstractTerm{CoeffType, M}
    coefficient::CoeffType
    monomial::M
end
Term(m::Monomial) = Term(1, m)
Term(v::Variable) = Term(Monomial(v))

coefficient(t::Term) = t.coefficient
monomial(t::Term) = t.monomial

immutable Polynomial{T <: Term, V <: AbstractVector{T}} <: AbstractPolynomial
    terms::V
end
Polynomial(terms::V) where {T <: Term, V <: AbstractVector{T}} = Polynomial{T, V}(terms)
Polynomial(term::Term) = Polynomial(SVector(term))
Polynomial(x) = Polynomial(Term(x))

terms(p::Polynomial) = p.terms
variables(::Type{<:Polynomial{T}}) where {T} = variables(T)
variables(p::Polynomial) = variables(typeof(p))

const MonomialLike = Union{<:Variable, <:Monomial}
const TermLike = Union{<:MonomialLike, <:Term}
const PolynomialLike = Union{<:TermLike, <:Polynomial}

# Based on fillZfordeg!() from MultivariatePolynomials.jl by Benoit Legat
# https://github.com/blegat/MultivariatePolynomials.jl/blob/d85ad85de413afa20fc8f5354c980387218ced2c/src/mono.jl#L186-L259
function monomial_powers{N}(::Val{N}, degree)
    result = Vector{NTuple{N, Int}}()
    powers = zeros(Int, N)
    powers[1] = degree
    while true
        push!(result, NTuple{N, Int}(powers))
        if powers[end] == degree
            break
        end
        total = 1
        for j in (N - 1):-1:1
            if powers[j] != 0
                powers[j] -= 1
                powers[j+1] += total
                break
            else
                total += powers[j+1]
                powers[j+1] = 0
            end
        end
    end
    result
end

function monomials(vars::Tuple{Vararg{<:Variable}}, degree::Integer)
    checksorted(vars, >) || throw(ArgumentError("Variables must be in order"))
    [Monomial{vars}(p) for p in monomial_powers(Val{length(vars)}(), degree)]
end

function monomials(vars::Tuple{Vararg{<:Variable}}, degrees::AbstractArray)
    checksorted(vars, >) || throw(ArgumentError("Variables must be in order"))
    Monomial{vars, length(vars)}[Monomial{vars}(p) for d in sort(degrees, rev=true)
        for p in monomial_powers(Val{length(vars)}(), d)]
end
