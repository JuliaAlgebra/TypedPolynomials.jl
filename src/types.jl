struct Variable{Name} <: VariableLike
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

name(::Type{Variable{Name}}) where {Name} = Name
name(v::Variable) = name(typeof(v))

struct Monomial{N, V} <: MonomialLike
    exponents::NTuple{N, Int}
end
Monomial{N, V}() where {N, V} = Monomial{N, V}(ntuple(_ -> 0, Val{N}))
Monomial(v::Variable) = Monomial{1, (v,)}((1,))

exponents(m::Monomial) = m.exponents
variables(::Type{Monomial{N, V}}) where {N, V} = V
variables(m::Monomial) = variables(typeof(m))
degree(m::Monomial) = sum(exponents(m))
@generated function exponent(m::Monomial{N, Vs}, v::V) where {N, Vs, V <: Variable}
    for (i, var) in enumerate(Vs)
        if typeof(var) == V
            return :(m.exponents[$i])
        end
    end
    :(0)
end

struct Term{T, MonomialType <: Monomial} <: TermLike
    coefficient::T
    monomial::MonomialType
end
Term(m::Monomial) = Term(1, m)
Term(v::Variable) = Term(Monomial(v))
Term(x) = Term{T, Monomial{0, tuple()}}(x, Monomial{0, tuple()}())

variables(::Type{Term{T, M}}) where {T, M} = variables(M)
variables(t::Term) = variables(typeof(t))
coefficient(t::Term) = t.coefficient
monomial(t::Term) = t.monomial
exponents(t::Term) = exponents(monomial(t))

immutable Polynomial{T <: Term, V <: AbstractVector{T}} <: PolynomialLike
    terms::V
end
Polynomial(terms::V) where {T <: Term, V <: AbstractVector{T}} = Polynomial{T, V}(terms)
Polynomial(term::Term) = Polynomial(SVector(term))
Polynomial(x) = Polynomial(Term(x))

variables(::Type{<:Polynomial{T}}) where {T} = variables(T)
variables(p::Polynomial) = variables(typeof(p))
terms(p::Polynomial) = p.terms
