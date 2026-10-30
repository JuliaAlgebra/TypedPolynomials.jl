struct Variable{Name} <: AbstractVariable end

MP.name(::Type{Variable{N}}) where {N} = N
MP.name(v::Variable) = name(typeof(v))
MP.name_base_indices(v::Variable) = name_base_indices(typeof(v))
function MP.name_base_indices(v::Type{Variable{N}}) where {N}
    name = string(N)
    splits = split(string(N), r"[\[,\]]\s*", keepempty = false)
    if length(splits) == 1
        return name, Int[]
    else
        return splits[1], parse.(Int, splits[2:end])
    end
end


MP.variables(v::Variable) = (v,)
MP.variables(::Type{V}) where {V<:Variable} = (V(),)
Base.hash(v::Variable, u::UInt) = hash(name(v), u)

checksorted(x::Tuple{Any}, cmp) = true
checksorted(x::Tuple{}, cmp) = true
checksorted(x::Tuple, cmp) = cmp(x[1], x[2]) && checksorted(Base.tail(x), cmp)

# Variable names remain encoded in the tuple type; arithmetic is shared with MP.
const Monomial{V<:Tuple{Vararg{Variable}},N} = MP.BasisElement{MP.Monomial,V,NTuple{N,Int}}
const MonomialLike = Union{Variable,Monomial}
const _Algebra = SA.StarAlgebra{<:MP.Variables{MP.Monomial,<:Tuple{Vararg{Variable}}}}
const Term{T} = SA.Term{T,<:_Algebra}
const Polynomial{T} = SA.AlgebraElement{T,<:_Algebra}

_instantiate(::Type{Tuple{}}) = ()
function _instantiate(::Type{T}) where {T<:Tuple}
    return (Base.tuple_type_head(T)(), _instantiate(Base.tuple_type_tail(T))...)
end

MP.variables(::Type{<:Monomial{V}}) where {V} = _instantiate(V)
MP.nvariables(::Type{<:Monomial{V}}) where {V} = fieldcount(V)
MP.monomial_type(::Type{V}) where {V<:Variable} = Monomial{Tuple{V},1}
MP.monomial_type(v::Variable) = monomial_type(typeof(v))
function MP.monomial(vars::Tuple{Vararg{Variable}}, exps::NTuple{N,Int}) where {N}
    @assert length(vars) == N
    @assert checksorted(vars, >)
    return MP.BasisElement(MP.Variables{MP.Monomial}(vars), exps)
end
MP.monomial(v::Variable) = MP.BasisElement{MP.Monomial}(v)
Monomial(v::Variable) = monomial(v)
function Monomial{V,N}(exps::NTuple{N,Int} = ntuple(_ -> 0, Val(N))) where {V,N}
    return monomial(_instantiate(V), exps)
end
function Monomial{V}(exps::Union{Tuple,AbstractVector} = ()) where {V}
    return Monomial{V,fieldcount(V)}(NTuple{fieldcount(V),Int}(exps))
end

MP.is_commutative(::Type{<:Variable}) = true
MP.ordering(::Type{<:Variable}) = MP.Graded(MP.LexOrder())
MP.ordering(::Type{<:Tuple{Vararg{Variable}}}) = MP.Graded(MP.LexOrder())
MP.ordering(::Tuple{Vararg{Variable}}) = MP.Graded(MP.LexOrder())
MP.constant_monomial(::Type{M}) where {M<:Monomial} = M()
MP.variable_union_type(::Type{<:MonomialLike}) = Variable
MP.similar_variable(::Type{Variable}, ::Type{Val{N}}) where {N} = Variable{N}()

Base.:(==)(::Variable{N}, ::Variable{N}) where {N} = true
Base.:(==)(::Variable, ::Variable) = false
Base.:^(v::Variable, n::Integer) = monomial((v,), (Int(n),))
MA.mutability(::Type{<:Monomial}) = MA.IsNotMutable()
Base.copy(m::Monomial) = m

function MP.divides(a::Monomial, b::Monomial)
    a, b = SA.promote_bases(a, b)
    return all(map(<=, exponents(a), exponents(b)))
end

function MP.monomials(
    vars::Tuple{Vararg{Variable}},
    degree::Integer,
    filter::Function = m->true,
)
    return MP.monomials(vars, [degree], filter)
end
function MP.monomials(
    vars::Tuple{Vararg{Variable}},
    degrees::AbstractArray,
    filter::Function = m->true,
)
    checksorted(vars, >) || throw(ArgumentError("Variables must be in order"))
    return MP._monomials(vars, degrees, filter)
end
