const Substitution{Name} = Pair{<:AbstractVariable{Name}}
const Substitutions = Tuple{Vararg{Substitution}}
const MultiSubstitution{N} = Pair{<:Tuple{Vararg{<:AbstractVariable, N}}, <:Tuple{Vararg{Any, N}}}

## Variables
subs(v::AbstractVariable{Name}, s::Substitution{Name}) where {Name} = s.second
subs(v::AbstractVariable{N1}, s::Substitution{N2}) where {N1, N2} = v

## Monomials
subs(s::Substitutions, p::Tuple{AbstractVariable, Integer}) = subs(p[1], s...)^p[2]
subs(s::Substitutions, p::Tuple{AbstractVariable, Integer}, p2...) = subs(s, p) * subs(s, p2...)
subs(m::AbstractMonomial, s::Substitutions) = subs(s, powers(m)...)

## Terms
subs(t::AbstractTerm, s::Substitutions) = coefficient(t) * subs(monomial(t), s)

## Polynomials
function subs(p::AbstractPolynomial, s::Substitutions)
    ts = terms(p)
    @assert length(ts) >= 1
    r1 = subs(ts[1], s)
    R = Base.promote_op(+, typeof(r1), typeof(r1))
    result::R = convert(R, r1)
    for i in 2:length(ts)
        result += subs(ts[i], s)
    end
    result
end

## Varargs catchers
subs(v::AbstractVariable{N}, s1::Substitution{N}, s2::Substitution...) where {N} = s1.second
subs(v::AbstractVariable, s1::Substitution, s2::Substitution...) = subs(v, s2...)
subs(p::AbstractPolynomial, s::Substitution...) = subs(p, s)
subs(m::AbstractMonomial, s::Substitution...) = subs(m, s)
subs(t::AbstractTerm, s::Substitution...) = subs(t, s)

## Everything else
subs(x, s::Substitution...) = x

"""
    subs(polynomial, (x, y)=>(1, 2))

is equivalent to:

    subs(polynomial, (x=>1, y=>2))
"""
function subs(p::AbstractPolynomialLike, s::MultiSubstitution)
    subs(p, pairzip(s))
end
