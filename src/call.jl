(p::Polynomial)(s::Substitution...) = subs(p, s)
(p::Term)(s::Substitution...) = subs(p, s)
(p::Monomial)(s::Substitution...) = subs(p, s)
(p::Variable)(s::Substitution...) = subs(p, s...)
