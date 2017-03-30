const Substitution{Name} = Pair{<:AbstractVariable{Name}}
const Substitutions = Tuple{Vararg{Substitution}}
variables(S::Type{<:Substitutions}) = [variable(p) for p in S.parameters]
variable(::Type{<:Pair{V}}) where {V <: AbstractVariable} = V

subs(v::AbstractVariable{Name}, s::Substitution{Name}) where {Name} = s.second
subs(v::AbstractVariable{N1}, s::Substitution{N2}) where {N1, N2} = v

# This is a slightly gross workaround. I would like to do:
# subs(s, powers(m)...)
# but so far I haven't been able to find a length-stable way to do
# zip(variables(m), exponents(m)) without using a generated function
subs(s::Substitutions, exp, i::Integer, v::AbstractVariable) = subs(v, s...)^exp[i]
subs(s::Substitutions, exp, i::Integer, v1::AbstractVariable, v2::AbstractVariable...) = 
    subs(s, exp, i, v1) * subs(s, exp, i + 1, v2...)

subs(v::AbstractVariable, s1::Substitution, s2::Substitution...) = subs(subs(v, s1), s2...)

subs(m::AbstractMonomial, s::Substitutions) = subs(s, exponents(m), 1, variables(m)...)
subs(m::AbstractMonomial, s::Substitution...) = subs(m, s)
subs(t::AbstractTerm, s::Substitutions) = coefficient(t) * subs(monomial(t), s)
subs(t::AbstractTerm, s::Substitution...) = subs(t, s)

subs(x, s::Substitution...) = x


