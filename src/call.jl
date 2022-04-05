(m::Monomial)(s...)   = MP.substitute(MP.Eval(), m, s)
(v::Variable)(s...)   = MP.substitute(MP.Eval(), v, s)
