const TypedSubstitution{Name} = Pair{<:TypedVariable{Name}}

## Variables
MP.substitute(st::MP.AbstractSubstitutionType, v::TypedVariable{Name}, s::TypedSubstitution{Name}) where {Name} = s.second
MP.substitute(st::MP.AbstractSubstitutionType, v::TypedVariable, s::TypedSubstitution) = v
# subs(x, x=>y, y=>1) should be y, not 1 so as soon as we see the right variable, we stop, in subs(x, x=>y, x=>1), "x=>1" is ignored.
MP.substitute(st::MP.AbstractSubstitutionType, v::TypedVariable{Name}, s::TypedSubstitution{Name}, ::MP.AbstractSubstitution...) where {Name} = s.second

## Monomials
#subs(s::Substitutions, p::Tuple{AbstractVariable, Integer}) = subs(p[1], s...)^p[2]
#subs(s::Substitutions, p::Tuple{AbstractVariable, Integer}, p2...) = subs(s, p) * subs(s, p2...)
#subs(m::AbstractMonomial, s::Substitutions) = subs(s, powers(m)...)
#
### Terms
#subs(t::AbstractTerm, s::Substitutions) = coefficient(t) * subs(monomial(t), s)
#
### Polynomials
#function subs(p::AbstractPolynomial, s::Substitutions)
#    ts = terms(p)
#    @assert length(ts) >= 1
#    r1 = subs(ts[1], s)
#    R = Base.promote_op(+, typeof(r1), typeof(r1))
#    result::R = convert(R, r1)
#    for i in 2:length(ts)
#        result += subs(ts[i], s)
#    end
#    result
#end
#
### Varargs catchers
#subs(v::AbstractVariable{N}, s1::Substitution{N}, s2::Substitution...) where {N} = s1.second
#subs(v::AbstractVariable, s1::Substitution, s2::Substitution...) = subs(v, s2...)
#subs(p::AbstractPolynomial, s::Substitution...) = subs(p, s)
#subs(m::AbstractMonomial, s::Substitution...) = subs(m, s)
#subs(t::AbstractTerm, s::Substitution...) = subs(t, s)
#
### Everything else
#subs(x, s::Substitutions) = x
#subs(x, s::Substitution...) = x
#
#"""
#    subs(polynomial, (x, y)=>(1, 2))
#
#is equivalent to:
#
#    subs(polynomial, (x=>1, y=>2))
#"""
#subs(p, s::MultiSubstitution) = subs(p, pairzip(s))
#
## inefficient but convenient method to allow subs(p, (x, y)=>[1, 2])
#subs(p, s::Pair{<:Tuple{Vararg{<:AbstractVariable}}, <:AbstractVector}) = subs(p, s.first => Tuple(s.second))
