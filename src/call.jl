(p::Polynomial)(s...) = subs(p, s...)
(p::Term)(s...) = subs(p, s...)
(p::Monomial)(s...) = subs(p, s...)
(p::Variable)(s...) = subs(p, s...)
