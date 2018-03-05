const Substitution{Name} = Pair{<:Variable{Name}}

## Variables
MP.substitute(st::MP.AbstractSubstitutionType, v::Variable{Name}, s::Substitution{Name}) where {Name} = s.second
MP.substitute(st::MP.AbstractSubstitutionType, v::Variable, s::Substitution) = v
# subs(x, x=>y, y=>1) should be y, not 1 so as soon as we see the right variable, we stop, in subs(x, x=>y, x=>1), "x=>1" is ignored.
MP.substitute(st::MP.AbstractSubstitutionType, v::Variable{Name}, s::Substitution{Name}, ::MP.AbstractSubstitution...) where {Name} = s.second
