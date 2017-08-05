abstract type TypedVariable{Name} <: AbstractVariable end
@pure MP.name(::Type{<:TypedVariable{N}}) where {N} = N
@pure MP.name(v::TypedVariable) = name(typeof(v))
@pure MP.deg(::TypedVariable) = 1
@pure MP.nvars(::TypedVariable) = 1
@pure variables(v::TypedVariable) = (v,)

abstract type TypedMonomial{Variables} <: AbstractMonomial end
MP.deg(m::TypedMonomial) = sum(exponents(m))
@pure variables(::Type{<:TypedMonomial{V}}) where {V} = V
@pure variables(m::TypedMonomial) = variables(typeof(m))
@pure MP.nvars(::Type{<:TypedMonomial{V}}) where {V} = length(V)
@pure MP.nvars(m::TypedMonomial) = nvars(typeof(m))

abstract type TypedTerm{CoeffType, MonomialType} <: AbstractTerm{CoeffType} end
MP.monomialtype(::Type{<:TypedTerm{C, M}}) where {C, M} = M
MP.monomialtype(t::TypedTerm) = monomialtype(typeof(t))
variables(T::Type{<:TypedTerm}) = variables(monomialtype(T))
variables(t::TypedTerm) = variables(monomialtype(t))
#MP.powers(m::TypedMonomial) = tuplezip(variables(m), exponents(m))
MP.nvars(t::TypedTerm{C, M}) where {C, M} = nvars(M)

abstract type TypedPolynomial{CoeffType} <: AbstractPolynomial{CoeffType} end
MP.nvars(p::TypedPolynomial) = maximum(nvars, terms(p))
# termtype(::Type{<:TypedPolynomial{T}}) where {T} = T
# termtype(p::TypedPolynomial) = termtype(typeof(p))
# variables(T::Type{<:TypedPolynomial}) = variables(termtype(T))
# variables(p::TypedPolynomial) = variables(termtype(p))

const TypedMonomialLike = Union{<:TypedVariable, <:TypedMonomial}
const TypedTermLike = Union{<:TypedMonomialLike, <:TypedTerm}
const TypedPolynomialLike = Union{<:TypedTermLike, <:TypedPolynomial}

MP.vars(p::TypedPolynomialLike) = variables(p)
