module TypedPolynomials

using StaticArrays: SVector
import Base: *, +, ^,
    promote_rule, convert, show, isless, size, getindex,
    one, zero
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
       degree

include("sequences.jl")
import .Sequences: shortest_common_supersequence

abstract type PolynomialLike end
abstract type TermLike <: PolynomialLike end
abstract type MonomialLike <: TermLike end
abstract type VariableLike <: MonomialLike end

immutable Variable{Name} <: VariableLike
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

immutable Monomial{N, V} <: MonomialLike
    exponents::NTuple{N, Int64}
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

immutable Term{T, MonomialType <: Monomial} <: TermLike
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

convert(::Type{Monomial{N, Vars}}, v::Variable) where {N, Vars} = convert(Monomial{N, Vars}, Monomial(v))
convert(T::Type{Term{T1, M1}}, x) where {T1, M1} = T(convert(T1, x), M1())
convert(::Type{Term{T, M1}}, m::Monomial) where {T, M1} = Term{T, M1}(one(T), convert(M1, m))
convert(::Type{Term{T, M}}, v::Variable) where {T, M} = Term{T, M}(one(T), convert(M, v))
convert(T::Type{Polynomial{T1, V1}}, p::Polynomial) where {T1, V1} = T(convert(V1, p.terms))
convert(::Type{<:Polynomial{T1}}, t::TermLike) where {T1} = Polynomial(convert(T1, t))

@generated function convert(::Type{Monomial{N1, V1}}, m::Monomial{N2, V2}) where {N1, V1, N2, V2}
    args = Any[0 for v in V1]
    i1 = 1
    i2 = 1
    while i1 <= N1 && i2 <= N2
        if V1[i1] == V2[i2]
            args[i1] = :(m.exponents[$i2])
            i2 += 1
        else
            i1 += 1
        end
    end
    if i2 < N2
        :(throw(InexactError()))
    else
        :(Monomial{N1, V1}($(Expr(:tuple, args...))))
    end
end

function convert(::Type{Term{T1, M1}}, t::Term{T2, M2}) where {T1, M1, T2, M2}
    Term{T1, M1}(convert(T1, t.coefficient), convert(M1, t.monomial))
end


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

show(io::IO, v::Variable{Name}) where Name = print(io, Name)
function show(io::IO, t::Term)
    if t.coefficient == 0
        print(io, "0")
    elseif t.coefficient == 1
        print(io, t.monomial)
    else
        print(io, t.coefficient)
        if !all(t.monomial.exponents .== 0)
            print(io, t.monomial)
        end
    end
end

format_exponent(e) = e == 1 ? "" : "^$e"
function show(io::IO, m::Monomial)
    if all(m.exponents .== 0)
        print(io, "1")
    else
        for (i, v) in enumerate(variables(m))
            if m.exponents[i] != 0
                print(io, v, format_exponent(m.exponents[i]))
            end
        end
    end
end

function show(io::IO, p::Polynomial)
    if isempty(p.terms)
        print(io, "0")
    else
        print(io, p.terms[1])
        for i in 2:length(p.terms)
            print(io, " + ", p.terms[i])
        end
    end
end

@generated function promote_rule(::Type{V1}, ::Type{V2}) where {V1 <: Variable, V2 <: Variable}
    if V1 < V2
        :(Monomial{2, (V1(), V2())})
    else
        :(Monomial{2, (V2(), V1())})
    end
end

function promote_rule(::Type{V}, ::Type{M}) where {V <: Variable, M <: Monomial}
    promote_rule(Monomial{1, (V(),)}, M)
end

function promote_rule(::Type{V}, ::Type{Term{T, M}}) where {V <: Variable, T, M <: Monomial}
    Term{T, promote_type(V, M)}
end

function _promote_monomial(::Type{Monomial{N1, V1}}, ::Type{Monomial{N2, V2}}) where {N1, V1, N2, V2}
    if V1 > V2
        _promote_monomial(Monomial{N2, V2}, Monomial{N1, V1})
    else
        varnames = shortest_common_supersequence(name.(V1), name.(V2))
        vars = Expr(:tuple, [Expr(:call, Expr(:curly, :Variable, Expr(:quote, n))) for n in varnames]...)
        quote
            Monomial{$(length(varnames)), $vars}
        end
    end
end

@generated function promote_rule(t1::Type{M1}, t2::Type{M2}) where {M1 <: Monomial, M2 <: Monomial}
    _promote_monomial(M1, M2)
end

function promote_rule(::Type{M1}, ::Type{Term{T, M2}}) where {M1 <: Monomial, T, M2 <: Monomial}
    Term{T, promote_type(M1, M2)}
end

function promote_rule(::Type{Term{T1, M1}}, ::Type{Term{T2, M2}}) where {T1, M1, T2, M2}
    Term{promote_type(T1, T2), promote_type(M1, M2)}
end

function promote_rule(::Type{T1}, ::Type{<:Polynomial{T2}}) where {T1 <: TermLike, T2 <: Term}
    T = promote_type(T1, T2)
    Polynomial{T, Vector{T}}
end

function promote_rule(::Type{T1}, ::Type{Polynomial{T2, SVector{N, T2}}}) where {T1 <: TermLike, T2 <: Term, N}
    T = promote_type(T1, T2)
    Polynomial{T, SVector{N, T}}
end

function promote_rule(::Type{<:Polynomial{T1}}, ::Type{<:Polynomial{T2}}) where {T1 <: TermLike, T2 <: TermLike}
    T = promote_type(T1, T2)
    Polynomial{T, Vector{T}}
end

@generated function promote_rule(::Type{Polynomial{T1, SVector{N1, T1}}}, ::Type{Polynomial{T2, SVector{N2, T2}}}) where {T1, N1, T2, N2}
    T = promote_type(T1, T2)
    :(Polynomial{T, SVector{max(N1, N2), T}})
end

function promote_rule(::Type{S}, ::Type{Term{T, M}}) where {S, T, M <: Monomial}
    Term{promote_type(S, T), M}
end

function promote_rule(::Type{S}, ::Type{<:Polynomial{T}}) where {S, T <: Term}
    R = promote_type(S, T)
    Polynomial{R, Vector{R}}
end

function promote_rule(::Type{S}, ::Type{Polynomial{T, SVector{N, T}}}) where {S, T <: Term, N}
    R = promote_type(S, T)
    Polynomial{R, SVector{N, R}}
end

# function promote_rule(::)

# promote_rule(::Type{M}, ::Type{M}) where {M <: Monomial} = M

# @generated function promote_rule(::Type{V}, ::Type{Monomial{N, Vars}}) where {V <: Variable, N, Vars}
#     if V in Vars
#         :(Monomial{N, Vars})
#     else
#         :(Monomial{$(N + 1), $(Tuple(sort(vcat(collect(Vars), [V]))))})
#     end
# end

# @generated function promote_rule(::Type{M1}, ::Type{M2}) where {M1 <: Monomial, M2 <: Monomial}
#     vars = Tuple(sort(union(variables(M1), variables(M2))))
#     quote
#         Monomial{$(length(vars)), $(vars)}
#     end
# end

# function promote_rule(::Type{<:MonomialLike}, ::Type{<:MonomialLike})
#     Monomial
# end

# function promote_rule(::Type{Term{T1, M}}, ::Type{Term{T2, M}}) where {T1, T2, M}
#     Term{promote_type(T1, T2), M}
# end

# function promote_rule(::Type{<:TermLike}, ::Type{<:TermLike})
#     Term
# end

# @generated function promote_rule(::Type{Term{T, Mono1}}, ::Type{Term{T, Mono2}}) where {T, Mono1, Mono2}
#     vars = Tuple(sort(collect(union(Set(variables(Mono1)),
#                                     Set(variables(Mono2))))))
#     quote
#         Term{T, Monomial{$(length(vars)), $(vars)}}
#     end
# end

# function promote_rule(::Type{<:PolynomialLike}, ::Type{<:PolynomialLike})
#     Polynomial
# end

# @generated function promote_rule(::Type{Polynomial{T1, V1}}, ::Type{Polynomial{T2, V2}}) where {T1, T2, V1, V2}
#     termtype = promote_type(T1, T2)
#     quote
#         Polynomial{$termtype, Vector{$termtype}}
#     end
# end

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

function (+)(term1::T1, term2::T2) where {T1 <: Term, T2 <: Term}
    T = promote_type(T1, T2)
    t1, t2 = promote(term1, term2)
    if t1.monomial < t2.monomial
        Polynomial([t1, t2])
    elseif t1.monomial > t2.monomial
        Polynomial([t2, t1])
    else
        Polynomial([T(t1.coefficient + t2.coefficient, t1.monomial)])
    end
end

(+)(p1::Polynomial, p2::Polynomial) = Polynomial(jointerms(p1.terms, p2.terms))
(+)(t1::TermLike, t2::TermLike) = Term(t1) + Term(t2)
(+)(t1::PolynomialLike, t2::PolynomialLike) = +(promote(t1, t2)...)
(+)(p::PolynomialLike, x::Any) = p + Polynomial(Term(x))
(+)(x::Any, p::PolynomialLike) = Polynomial(Term(x)) + p

@generated function (*)(m1::Monomial{N1, V1}, m2::Monomial{N2, V2}) where {N1, V1, N2, V2}
    vars = Tuple(sort(collect(union(Set(V1), Set(V2)))))
    args = []
    for (i, v) in enumerate(vars)
        i1 = findfirst(V1, v)
        i2 = findfirst(V2, v)
        if i1 != 0 && i2 != 0
            push!(args, :(m1.exponents[$i1] + m2.exponents[$i2]))
        elseif i1 != 0
            push!(args, :(m1.exponents[$i1]))
        else
            @assert i2 != 0
            push!(args, :(m2.exponents[$i2]))
        end
    end
    Expr(:call, :(Monomial{$(length(args)), $vars}), Expr(:tuple, args...))
end

(*)(t1::TermLike, t2::TermLike) = *(promote(t1, t2)...)
(*)(t1::Term, t2::Term) = Term(t1.coefficient * t2.coefficient, t1.monomial * t2.monomial)
(*)(t1::Term, m2::MonomialLike) = Term(t1.coefficient, t1.monomial * Monomial(m2))
(*)(m1::MonomialLike, t2::Term) = Term(t2.coefficient, Monomial(m1) * t2.monomial)
(*)(x::Any, t::Term) = Term(x * t.coefficient, t.monomial)
(*)(t::Term, x::Any) = Term(t.coefficient * x, t.monomial)
(*)(t::TermLike, x::Any) = Term(t) * x
(*)(x::Any, t::TermLike) = x * Term(t)
(*)(t::TermLike) = t

@generated function (*)(v1::V1, v2::V2) where {V1 <: Variable, V2 <: Variable}
    if V1() < V2()
        :(Monomial{2, (V1(), V2())}((1, 1)))
    else
        :(Monomial{2, (V2(), V1())}((1, 1)))
    end
end
(*)(v1::V, v2::V) where {V <: Variable} = Monomial{1, (V(),)}((2,))

^(v::V, x::Integer) where {V <: Variable} = Monomial{1, (V(),)}((x,))

include("substitution.jl")

end # module
