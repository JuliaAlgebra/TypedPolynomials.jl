const Substitution{Name} = Pair{<:AbstractVariable{Name}}
const Substitutions = Tuple{Vararg{Substitution}}
const MultiSubstitution{N} = Pair{<:Tuple{Vararg{<:AbstractVariable, N}}, <:Tuple{Vararg{Any, N}}}

## Variables
subs(v::AbstractVariable{Name}, s::Substitution{Name}) where {Name} = s.second
subs(v::AbstractVariable{N1}, s::Substitution{N2}) where {N1, N2} = v

## Monomials
# This is a slightly gross workaround. I would like to do:
# subs(s, powers(m)...)
# but so far I haven't been able to find a length-stable way to do
# zip(variables(m), exponents(m)) without using a generated function
subs(s::Substitutions, exp, i::Integer, v::AbstractVariable) = subs(v, s...)^exp[i]
subs(s::Substitutions, exp, i::Integer, v1::AbstractVariable, v2::AbstractVariable...) = 
    subs(s, exp, i, v1) * subs(s, exp, i + 1, v2...)
subs(m::AbstractMonomial, s::Substitutions) = subs(s, exponents(m), 1, variables(m)...)

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
subs(v::AbstractVariable, s1::Substitution, s2::Substitution...) = subs(subs(v, s1), s2...)
subs(p::AbstractPolynomial, s::Substitution...) = subs(p, s)
subs(m::AbstractMonomial, s::Substitution...) = subs(m, s)
subs(t::AbstractTerm, s::Substitution...) = subs(t, s)

## Everything else
subs(x, s::Substitution...) = x

"""
pairzip((a, b), (c, d)) gives (a=>c, b=>d)

This function was written by Fengyang Wang and shared on the Julia discourse
forum: https://discourse.julialang.org/t/type-stable-zip-to-pairs/3390/2
"""
pairzip(::Tuple{}, ::Tuple{}) = ()
pairzip(::Tuple{}, ::Tuple) = throw(ArgumentError("args must be equal in length"))
pairzip(::Tuple, ::Tuple{}) = throw(ArgumentError("args must be equal in length"))
pairzip(t::Tuple, u::Tuple) = (t[1] => u[1], pairzip(Base.tail(t), Base.tail(u))...)
pairzip(p::Pair{<:Tuple, <:Tuple}) = pairzip(p.first, p.second)

"""
    subs(polynomial, (x, y)=>(1, 2))

is equivalent to:

    subs(polynomial, (x=>1, y=>2))
"""
function subs(p::AbstractPolynomialLike, s::MultiSubstitution)
    subs(p, pairzip(s))
end
