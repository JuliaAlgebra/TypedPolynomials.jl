module StaticPolynomials

using TypedPolynomials: mergesorted
import TypedPolynomials: AbstractVariable,
                         AbstractMonomial,
                         AbstractTerm,
                         AbstractPolynomial,
                         name,
                         exponents,
                         exponent,
                         variables,
                         coefficient,
                         monomial,
                         subs,
                         terms

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

# struct Power{Var, Exponent}
#     function Power{V, E}() where {V, E}
#         new{typeassert(V, Variable), typeassert(E, Int)}()
#     end
# end
#
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

# powers(::Type{Monomial{Powers}}) where {Powers} = Powers
# powers(m::Monomial) = powers(typeof(m))

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

promote_rule(::Type{V1}, ::Type{V2}) where {V1 <: Variable, V2 <: Variable} = Monomial{(V1(), V2())}
promote_rule(::Type{V}, ::Type{M}) where {V <: Variable, M <: Monomial} = promote_rule(Monomial{(V(),), (1,)}, M)

@generated function promote_rule(::Type{M1}, ::Type{M2}) where {M1 <: Monomial, M2 <: Monomial}
    varnames = union(name.(variables(M1)), name.(variables(M2)))
    sort!(varnames)
    vars = Expr(:tuple, [Expr(:call, Expr(:curly, :Variable, Expr(:quote, n))) for n in varnames]...)
    :(Monomial{$(vars)})
end

@generated function convert(::Type{Monomial{v1}}, m::Monomial) where {v1}
    args = Any[0 for v in v1]
    v2 = variables(m)
    i1 = 1
    i2 = 1
    while i1 <= length(v1) && i2 <= length(v2)
        if v1[i1] == v2[i2]
            args[i1] = :(exponents(m)[$i2])
            i1 += 1
            i2 += 1
        else
            i1 += 1
        end
    end
    if i2 <= length(v2)
        throw(InexactError())
    end
    :(Monomial{v1, $(Expr(:tuple, args...))}())
end

convert(::Type{Monomial}, v::Variable) = Monomial{(v,), (1,)}()
convert(::Type{M}, v::Variable) where {M <: Monomial} = convert(M, Monomial(v))

convert(::Type{AbstractPolynomial}, terms::Tuple{Vararg{Term}}) = Polynomial(terms)

struct StagedTerm{M <: Monomial}
    coeff_expr::Expr
    monomial::M
end

isless(s1::StagedTerm, s2::StagedTerm) = s1.monomial < s2.monomial
function combine(s1::StagedTerm, s2::StagedTerm)
    @assert s1.monomial == s2.monomial
    StagedTerm(:($(s1.coeff_expr) + $(s2.coeff_expr)), s1.monomial)
end

@generated function jointerms(terms1::Tuple{Vararg{Term}}, terms2::Tuple{Vararg{Term}})
    v1 = [StagedTerm(:(terms1[$i].coefficient), monomial(t)) for (i, t) in enumerate(terms1.parameters)]
    v2 = [StagedTerm(:(terms2[$i].coefficient), monomial(t)) for (i, t) in enumerate(terms2.parameters)]

    newterms = mergesorted(v1, v2, <, combine)
    Expr(:tuple, [:(Term($(x.coeff_expr), $(x.monomial))) for x in newterms]...)
end

(+)(m1::M, m2::M) where {M <: Monomial} = Term{M}(c)
(+)(t1::Term{T1, M}, t2::Term{T2, M}) where {T1, T2, M} = Term{M}(coefficient(t1) + coefficient(t2))

(*)(v1::V, v2::V) where {V <: Variable} = Monomial{(v1,), (2,)}()
# @generated function (*)(::Power{V, E1}, ::Power{V, E2}) where {V, E1, E2}
#     :(Power{V, $(E1 + E2)}())
# end
(*)(p1::Power{V}, p2::Power{V}) where {V} = Power{V}(p1.exponent + p2.exponent)

@generated function (*)(m1::Monomial, m2::Monomial)
    newpowers = mergesorted(
        collect(powers(m1)),
        collect(powers(m2)),
        (p1, p2) -> name(variable(p1)) < name(variable(p2)),
        *)
    :(Monomial{$(Tuple(variable.(newpowers))), $(Tuple(exponent.(newpowers)))}())
end

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

subs(v::PolynomialLike, s::Vararg{<:Pair{<:Variable}}) = subs(v, s)
subs(x, s::Vararg{Pair{<:Variable}}) = x

(p::Polynomial)(s::Vararg{Pair{<:Variable}}) = subs(p, s)
(p::Term)(s::Vararg{Pair{<:Variable}}) = subs(p, s)
(p::Monomial)(s::Vararg{Pair{<:Variable}}) = subs(p, s)
(p::Variable)(s::Vararg{Pair{<:Variable}}) = subs(p, s)

end
