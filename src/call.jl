(p::Polynomial)(s...) = MP.substitute(MP.Eval(), p, s)
(t::Term)(s...)       = MP.substitute(MP.Eval(), t, s)
(m::Monomial)(s...)   = MP.substitute(MP.Eval(), m, s)
(v::Variable)(s...)   = MP.substitute(MP.Eval(), v, s)
