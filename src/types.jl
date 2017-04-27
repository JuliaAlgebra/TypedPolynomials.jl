struct Variable{Name} <: AbstractVariable{Name}
end

macro polyvar(names...)
    exprs = [
        quote
            $(esc(name)) = Variable{$(esc(Expr(:quote, name)))}()
        end
        for name in names
    ]
    Expr(:block, exprs...)
end

checksorted(x::Tuple{Any}, cmp) = true
checksorted(x::Tuple{}, cmp) = true
checksorted(x::Tuple, cmp) = cmp(x[1], x[2]) && checksorted(Base.tail(x), cmp)

struct Monomial{V, N} <: AbstractMonomial{V}
    exponents::NTuple{N, Int}

    Monomial{V, N}(exponents::NTuple{N, Int}) where {V, N} = (@assert checksorted(V, >); new{V, N}(exponents))
end
Monomial{V, N}() where {N, V} = Monomial{V, N}(ntuple(_ -> 0, Val{N}))
Monomial{V}() where {V} = Monomial{V, 0}()
Monomial{V}(exponents::T) where {V, N, T <: Tuple{Vararg{Any, N}}} = Monomial{V, N}(exponents)
Monomial(v::Variable) = Monomial{(v,), 1}((1,))

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
