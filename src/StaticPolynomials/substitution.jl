subs(v::PolynomialLike, s::Vararg{<:Pair{<:Variable}}) = subs(v, s)
subs(x, s::Vararg{Pair{<:Variable}}) = x

(p::Polynomial)(s::Vararg{Pair{<:Variable}}) = subs(p, s)
(p::Term)(s::Vararg{Pair{<:Variable}}) = subs(p, s)
(p::Monomial)(s::Vararg{Pair{<:Variable}}) = subs(p, s)
(p::Variable)(s::Vararg{Pair{<:Variable}}) = subs(p, s)
