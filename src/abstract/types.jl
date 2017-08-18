abstract type TypedVariable{Name} <: AbstractVariable end
@pure MP.name(::Type{<:TypedVariable{N}}) where {N} = N
@pure MP.name(v::TypedVariable) = name(typeof(v))
@pure MP.variables(v::TypedVariable) = (v,)
@pure MP.variables(::Type{V}) where V<:TypedVariable = (V(),)
Base.hash(::TypedVariable{N}, u::UInt) where N = hash(N, u)

abstract type TypedMonomial{Variables} <: AbstractMonomial end
#MP.deg(m::TypedMonomial) = sum(exponents(m))
@pure MP.variables(::Type{<:TypedMonomial{V}}) where {V} = V
@pure MP.variables(m::TypedMonomial) = variables(typeof(m))
@pure MP.nvariables(::Type{<:TypedMonomial{V}}) where {V} = length(V)
@pure MP.nvariables(m::TypedMonomial) = nvariables(typeof(m))

abstract type TypedTerm{CoeffType, MonomialType} <: AbstractTerm{CoeffType} end
MP.monomialtype(::Type{<:TypedTerm{C, M}}) where {C, M} = M
MP.monomialtype(t::TypedTerm) = monomialtype(typeof(t))
MP.variables(T::Type{<:TypedTerm}) = variables(monomialtype(T))
MP.variables(t::TypedTerm) = variables(monomialtype(t))
#MP.powers(m::TypedMonomial) = tuplezip(variables(m), exponents(m))
MP.nvariables(t::TypedTerm{C, M}) where {C, M} = nvariables(M)

abstract type TypedPolynomial{CoeffType} <: AbstractPolynomial{CoeffType} end
#MP.nvariables(p::TypedPolynomial) = maximum(nvariables, terms(p))
# termtype(::Type{<:TypedPolynomial{T}}) where {T} = T
# termtype(p::TypedPolynomial) = termtype(typeof(p))
# variables(T::Type{<:TypedPolynomial}) = variables(termtype(T))
# variables(p::TypedPolynomial) = variables(termtype(p))

const TypedMonomialLike = Union{<:TypedVariable, <:TypedMonomial}
const TypedTermLike = Union{<:TypedMonomialLike, <:TypedTerm}
const TypedPolynomialLike = Union{<:TypedTermLike, <:TypedPolynomial}
