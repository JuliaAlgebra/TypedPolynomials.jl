module StaticPolynomials

using TypedPolynomials: mergesorted
import TypedPolynomials: AbstractVariable,
    AbstractMonomial,
    AbstractTerm,
    AbstractPolynomial,
    name,
    exponents,
    exponent,
    variables

import Base: literal_pow, +, -, *, /, ==, isless,
copy, promote_rule, convert

export @polyvar,
       Variable,
       Monomial,
       Term,
       Polynomial,
       name,
       exponents,
       variables,
       coefficient,
       monomial,
       terms,
       degree,
       subs

struct Variable{Name} <: AbstractVariable
end

copy(v::Variable) = v

name(v::Variable{Name}) where {Name} = Name

macro polyvar(names...)
    exprs = [
        quote
            $(esc(name)) = Variable{$(esc(Expr(:quote, name)))}()
        end
        for name in names
    ]
    Expr(:block, exprs...)
end

struct Power{Var, Exponent}
    function Power{V, E}() where {V, E}
        new{typeassert(V, Variable), typeassert(E, Int)}()
    end
end

variable(p::Power{V}) where {V} = V
exponent(p::Power{V, E}) where {V, E} = E

struct Monomial{Powers} <: AbstractMonomial
    function Monomial{P}() where {P}
        new{typeassert(P, Tuple{Vararg{Power}})}()
    end
end

variables(m::Monomial{Powers}) where {Powers} = variable.(Powers)
exponents(m::Monomial{Powers}) where {Powers} = exponent.(Powers)
powers(::Type{Monomial{Powers}}) where {Powers} = Powers
powers(m::Monomial) = powers(typeof(m))

struct Term{M <: Monomial, T} <: AbstractTerm
    coefficient::T
end

struct Polynomial{Terms <: Tuple{Vararg{Term}}}
    terms::Terms
end

function literal_pow(^, v::V, ::Type{Val{x}}) where {V <: Variable, x}
    Monomial{(Power{v, x}(),)}()
end

const MonomialLike = Union{<:Variable, <:Monomial}
const TermLike = Union{<:MonomialLike, <:Term}
const PolynomialLike = Union{<:TermLike, <:Polynomial}

promote_rule(::Type{<:Variable}, ::Type{<:Variable}) = Monomial
promote_rule(::Type{<:Variable}, ::Type{<:Monomial}) = Monomial

convert(::Type{Monomial}, v::Variable) = Monomial{(Power{v, 1}(),)}()


(*)(v1::V, v2::V) where {V <: Variable} = Monomial{(Power{v1, 2}(),)}()
@generated function (*)(::Power{V, E1}, ::Power{V, E2}) where {V, E1, E2}
    :(Power{V, $(E1 + E2)}())
end
@generated function (*)(m1::Monomial, m2::Monomial)
    newpowers = mergesorted(
        collect(powers(m1)),
        collect(powers(m2)),
        (p1, p2) -> name(variable(p1)) < name(variable(p2)),
        *)
    :(Monomial{$(Tuple(newpowers))}())
end

for op in [:+, :*, :-, :(==)]
    @eval $op(p1::PolynomialLike, p2::PolynomialLike) = $op(promote(p1, p2)...)
    @eval $op(p::PolynomialLike, x) = $op(promote(p, x)...)
    @eval $op(x, p::PolynomialLike) = $op(promote(x, p)...)
end

(==)(v1::V, v2::V) where {V <: Variable} = true
(==)(v1::Variable, v2::Variable) = false
function (==)(m1::Monomial, m2::Monomial)
    v1 = variables(m1)
    e1 = exponents(m1)
    v2 = variables(m2)
    e2 = exponents(m2)
    i1 = 1
    i2 = 1
    while true
        while i1 <= length(e1) && e1[i1] == 0
            i1 += 1
        end
        while i2 <= length(e2) && e2[i2] == 0
            i2 += 1
        end
        if i1 > length(e1) && i2 > length(e1)
            return true
        elseif i1 > length(e1) || i2 > length(e2)
            return false
        elseif v1[i1] != v2[i2]
            return false
        elseif e1[i1] != e2[i2]
            return false
        else
            i1 += 1
            i2 += 1
        end
    end
end

end
