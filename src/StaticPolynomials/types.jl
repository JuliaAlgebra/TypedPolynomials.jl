struct Variable{Name} <: AbstractVariable{Name}
end

copy(v::Variable) = v

macro polyvar(names...)
    exprs = [
        quote
            $(esc(name)) = Variable{$(esc(Expr(:quote, name)))}()
        end
        for name in names
    ]
    Expr(:block, exprs...)
end

struct Power{V}
    exponent::Int

    function Power{V}(e::Int) where {V}
        new{typeassert(V, Variable)}(e)
    end
end

variable(p::Power{V}) where {V} = V
exponent(p::Power) = p.exponent

struct Monomial{Vars, Exponents} <: AbstractMonomial{Vars}
    function Monomial{V, E}() where {V, E}
        @assert length(V) == length(E)
        new{typeassert(V, Tuple{Vararg{Variable}}),
            typeassert(E, Tuple{Vararg{Int}})}()
    end
end

exponents(m::Monomial{V, E}) where {V, E} = E

@generated function powers(::Type{Monomial{V, E}}) where {V, E}
    Expr(:tuple, [:($(Power{v}(e))) for (v, e) in zip(V, E)]...)
end

struct Term{T, M <: Monomial} <: AbstractTerm{T, M}
    coefficient::T
end

Term{M}(c::T) where {T, M <: Monomial} = Term{T, M}(c)
Term(c::T, m::M) where {T, M <: Monomial} = Term{T, M}(c)

coefficient(t::Term) = t.coefficient
monomial(::Type{Term{T, M}}) where {T, M} = M()
monomial(t::Term) = monomial(typeof(t))

struct Polynomial{Terms <: Tuple{Vararg{Term}}} <: AbstractPolynomial
    terms::Terms
end

terms(p::Polynomial) = p.terms

function literal_pow(^, v::V, ::Type{Val{x}}) where {V <: Variable, x}
    Monomial{(v,), (x,)}()
end

const MonomialLike = Union{<:Variable, <:Monomial}
const TermLike = Union{<:MonomialLike, <:Term}
const PolynomialLike = Union{<:TermLike, <:Polynomial}
