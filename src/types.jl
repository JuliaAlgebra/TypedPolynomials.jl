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

struct Monomial{V, N} <: AbstractMonomial{V}
    exponents::NTuple{N, Int}
end
Monomial{V, N}() where {N, V} = Monomial{V, N}(ntuple(_ -> 0, Val{N}))
Monomial{V}() where {V} = Monomial{V, 0}()
Monomial{V}(exponents::T) where {V, N, T <: Tuple{Vararg{Any, N}}} = Monomial{V, N}(exponents)
Monomial(v::Variable) = Monomial{(v,), 1}((1,))

exponents(m::Monomial) = m.exponents
exponent(m::Monomial, i::Integer) = m.exponents[i]
@generated function exponent(m::Monomial{Vs}, v::V) where {Vs, V <: Variable}
    for (i, var) in enumerate(Vs)
        if typeof(var) == V
            return :(m.exponents[$i])
        end
    end
    :(0)
end

struct Term{CoeffType, M <: Monomial} <: AbstractTerm{CoeffType, M}
    coefficient::CoeffType
    monomial::M
end
Term(m::Monomial) = Term(1, m)
Term(v::Variable) = Term(Monomial(v))
Term(x) = Term{T, Monomial{tuple(), 0}}(x, Monomial{tuple(), 0}())

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
