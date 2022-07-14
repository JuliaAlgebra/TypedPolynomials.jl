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
MP.degree(m::Monomial, v::Variable) = _exponent(v, powers(m)...)

const MonomialLike = Union{Variable, Monomial}

MP.constantmonomial(M::Union{<:MonomialLike,Type{<:MonomialLike}}) = Monomial{variables(M), nvariables(M)}()
MP.variable_union_type(::Type{<:MonomialLike}) = Variable
MP.similarvariable(::Type{Variable}, ::Type{Val{N}}) where N = Variable{N}()

# Use default `MP.Term`
MP.termtype(::Union{Type{M}}, ::Type{T}) where {M<:Monomial,T} = MP.Term{T,M}

# Use default `MP.Polynomial` with `Vector`
MP.polynomialtype(::Type{MP.Term{C,M}}) where {C,M<:Monomial} = Polynomial{C,MP.Term{C,M},Vector{MP.Term{C, M}}}
MP.polynomialtype(::Type{MP.Term{C,M} where C}) where {M<:Monomial} = Polynomial

MP.variables(::Union{Term{C,M},Type{Term{C,M}},Polynomial{C,Term{C,M}},Type{<:Polynomial{C,Term{C,M}}}}) where {C,M<:Monomial} = MP.variables(M)
MP.nvariables(::Union{Term{C,M},Type{Term{C,M}},Polynomial{C,Term{C,M}},Type{<:Polynomial{C,Term{C,M}}}}) where {C,M<:Monomial} = MP.nvariables(M)
MP.variables(::Union{AbstractVector{PT},Type{<:AbstractVector{PT}}}) where {C,M<:Monomial,PT<:Union{MonomialLike,Term{C,M},Polynomial{C,Term{C,M}}}} = variables(PT)
MP.nvariables(::Union{AbstractVector{PT},Type{<:AbstractVector{PT}}}) where {C,M<:Monomial,PT<:Union{MonomialLike,Term{C,M},Polynomial{C,Term{C,M}}}} = nvariables(PT)

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
