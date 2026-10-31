# Keep the existing coefficient-only families while exposing the variable tuple.
const _Algebra{V<:Tuple{Vararg{Variable}}} = SA.StarAlgebra{<:MP.Variables{MP.Monomial,<:V}}
const Term{T,V<:Tuple{Vararg{Variable}}} = SA.Term{T,<:_Algebra{V}}
const Polynomial{T,V<:Tuple{Vararg{Variable}}} = SA.AlgebraElement{T,<:_Algebra{V}}
const SingleTermPolynomial{T,V<:Tuple{Vararg{Variable}}} = SA.AlgebraElement{
    T,
    <:_Algebra{V},
    <:SA.SparseCoefficients{K,T,Tuple{K},Tuple{T},MP.Graded{MP.LexOrder}} where {K},
}

# Aliases live outside StarAlgebras, where the underlying types are defined.
# Inspect parameters rather than binding T and V in show: partially specified
# families can leave those static parameters unbound during dispatch.
function _display_parameter(type, index)
    while type isa TypeVar
        type = type.ub
    end
    return Base.unwrap_unionall(type).parameters[index]
end

function _show_polynomial_type(io::IO, name, type)
    if type isa Union
        return invoke(show, Tuple{IO,Type}, io, type)
    end
    T = _display_parameter(type, 1)
    algebra = _display_parameter(type, 2)
    object = _display_parameter(algebra, 1)
    V = _display_parameter(object, 2)
    while V isa TypeVar
        V = V.ub
    end
    unknown_variables = V === Tuple{Vararg{Variable}}
    print(io, "TypedPolynomials.", name)
    if T isa TypeVar && T.ub === Any && unknown_variables
        return
    end
    print(io, "{")
    if T isa TypeVar
        print(io, "<:")
        show(io, T.ub)
    else
        show(io, T)
    end
    if !unknown_variables
        print(io, ", ")
        show(io, V)
    end
    return print(io, "}")
end

function Base.show(io::IO, type::Type{<:Polynomial})
    return _show_polynomial_type(io, "Polynomial", type)
end

function Base.show(io::IO, type::Type{<:SingleTermPolynomial})
    return _show_polynomial_type(io, "SingleTermPolynomial", type)
end

function Base.show(io::IO, type::Type{<:Term})
    return _show_polynomial_type(io, "Term", type)
end
