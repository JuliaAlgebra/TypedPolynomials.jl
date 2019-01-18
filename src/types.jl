struct Variable{Name} <: AbstractVariable
end

MP.name(::Type{Variable{N}}) where {N} = N
MP.name(v::Variable) = name(typeof(v))
MP.name_base_indices(v::Variable) = name_base_indices(typeof(v))
function MP.name_base_indices(v::Type{Variable{N}}) where N
    name = string(N)
    splits = split(string(N), r"[\[,\]]\s*", keepempty=false)
    if length(splits) == 1
        return name, Int[]
    else
        return splits[1], parse.(Int, splits[2:end])
    end
end


MP.variables(v::Variable) = (v,)
MP.variables(::Type{V}) where {V <: Variable} = (V(),)
Base.hash(v::Variable, u::UInt) = hash(name(v), u)

checksorted(x::Tuple{Any}, cmp) = true
checksorted(x::Tuple{}, cmp) = true
checksorted(x::Tuple, cmp) = cmp(x[1], x[2]) && checksorted(Base.tail(x), cmp)



struct Monomial{V, N} <: AbstractMonomial
    exponents::NTuple{N, Int}

    function Monomial{V, N}(exponents::NTuple{N, Int}=ntuple(_ -> 0, Val{N}())) where {V, N}
        @assert checksorted(V, >)
        new{V, N}(exponents)
    end
    Monomial{V}(exponents::NTuple{N, Integer}=()) where {V, N} = Monomial{V, N}(exponents)
    Monomial{V}(exponents::AbstractVector{<:Integer}) where {V} = Monomial{V}(NTuple{length(V), Int}(exponents))
end

Monomial(v::Variable) = monomialtype(v)((1,))
MP.monomial(v::Variable) = Monomial(v)

MP.variables(::Type{<:Monomial{V}}) where {V} = V
MP.variables(m::Monomial) = variables(typeof(m))
MP.nvariables(::Type{<:Monomial{V}}) where {V} = length(V)
MP.nvariables(m::Monomial) = nvariables(typeof(m))
MP.monomialtype(::Type{V}) where V<:Variable = monomialtype(V())
MP.monomialtype(v::Variable) = Monomial{(v,), 1}

MP.exponents(m::Monomial) = m.exponents
MP.exponent(m::Monomial, i::Integer) = m.exponents[i]
_exponent(v::V, p1::Tuple{V, Integer}, p2...) where {V <: Variable} = p1[2]
_exponent(v::Variable, p1::Tuple{Variable, Integer}, p2...) = _exponent(v, p2...)
_exponent(v::Variable) = 0
MP.exponent(m::Monomial, v::Variable) = _exponent(v, powers(m)...)



struct Term{CoeffType, M <: Monomial} <: AbstractTerm{CoeffType}
    coefficient::CoeffType
    monomial::M
end
Term(v::Variable) = Term(Monomial(v))
Term(x, v::Variable) = Term(x, Monomial(v))
Term(m::Monomial) = Term(1, m)

MP.coefficient(t::Term) = t.coefficient
MP.monomial(t::Term) = t.monomial
coefftype(::Type{<:Term{C}}) where {C} = C
MP.termtype(::Union{Term{C, M}, Type{<:Term{C, M}}}, ::Type{T}) where {C, M, T} = Term{T, M}
MP.termtype(::Union{M, Type{M}}, ::Type{T}) where {M<:Monomial, T} = Term{T, M}
MP.monomialtype(::Type{<:Term{C, M}}) where {C, M} = M
MP.monomialtype(t::Term) = monomialtype(typeof(t))
MP.variables(t::Union{Term, Type{<:Term}}) = variables(monomialtype(t))
MP.nvariables(t::Union{Term, Type{<:Term}}) = nvariables(monomialtype(t))

struct Polynomial{CoeffType, T <: Term{CoeffType}, V <: AbstractVector{T}} <: AbstractPolynomial{CoeffType}
    terms::V

    Polynomial{C, T, V}(terms::AbstractVector{T}) where {C, T, V} = new{C, T, V}(terms)
    Polynomial{C, T, V}(p::Polynomial{C, T, V}) where {C, T, V} = p
end
Polynomial(terms::AbstractVector{T}) where {C, T <: Term{C}} = Polynomial{C, T, typeof(terms)}(terms)
Polynomial(t::AbstractVector) = Polynomial(Term.(t))
Polynomial(x) = Polynomial(Term(x))
MP.termtype(::Type{<:Polynomial{C, T}}) where {C, T} = T
changeeltype(::Type{<:Vector}, ::Type{T}) where T = Vector{T}
function MP.polynomialtype(::Type{<:Polynomial{C, T, V}}, ::Type{NewC}) where {C, T, V, NewC}
    NewT = termtype(T, NewC)
    Polynomial{NewC, NewT, changeeltype(V, NewT)}
end
MP.polynomialtype(::Type{Term{C, M}}) where {C, M} = Polynomial{C, Term{C, M}, Vector{Term{C, M}}}
Polynomial(term::TT) where TT<:Term = Polynomial(iszero(term) ? TT[] : [term])

MP.terms(p::Polynomial) = p.terms
MP.variables(::Union{Polynomial{C, T}, AbstractArray{<:Polynomial{C, T}}, Type{<:Polynomial{C, T}}}) where {C, T} = variables(T)
MP.nvariables(::Union{Polynomial{C, T}, AbstractArray{<:Polynomial{C, T}}, Type{<:Polynomial{C, T}}}) where {V, N, C, M<:Monomial{V, N}, T<:Term{C, M}} = N
const MonomialLike = Union{Variable, Monomial}
const TermLike = Union{MonomialLike, Term}
const PolynomialLike = Union{TermLike, Polynomial}

MP.variables(::Union{AbstractVector{T}, Type{<:AbstractVector{T}}}) where {T <: PolynomialLike} = variables(T)
MP.nvariables(::Union{AbstractVector{T}, Type{<:AbstractVector{T}}}) where {T <: PolynomialLike} = nvariables(T)
MP.constantmonomial(p::PolynomialLike) = Monomial{variables(p), nvariables(p)}()
MP.constantmonomial(::Type{TT}) where {TT<:PolynomialLike} = Monomial{variables(TT), nvariables(TT)}()


# Based on fillZfordeg!() from MultivariatePolynomials.jl by Benoit Legat
# https://github.com/blegat/MultivariatePolynomials.jl/blob/d85ad85de413afa20fc8f5354c980387218ced2c/src/mono.jl#L186-L259
function monomial_powers(::Val{N}, degree) where N
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

function MP.monomials(vars::Tuple{Vararg{<:Variable}}, degree::Integer, filter::Function=m->true)
    checksorted(vars, >) || throw(ArgumentError("Variables must be in order"))
    Monomial{vars, length(vars)}[Monomial{vars}(p) for p in monomial_powers(Val{length(vars)}(), degree) if filter(Monomial{vars}(p))]
end

function MP.monomials(vars::Tuple{Vararg{<:Variable}}, degrees::AbstractArray, filter::Function=m->true)
    checksorted(vars, >) || throw(ArgumentError("Variables must be in order"))
    if isempty(degrees)
        # Otherwise, the following error is thrown: "ArgumentError: argument to Flatten must contain at least one iterator"
        Monomial{vars, length(vars)}[]
    else
        Monomial{vars, length(vars)}[Monomial{vars}(p) for d in sort(degrees, rev=true)
            for p in monomial_powers(Val{length(vars)}(), d) if filter(Monomial{vars}(p))]
    end
end

MP.similarvariable(::Union{PolynomialLike, Type{<:PolynomialLike}}, ::Type{Val{N}}) where N = Variable{N}()
