_makevar(name::Symbol) = :($(esc(name)) = Variable{$(esc(Expr(:quote, name)))}())
_makevar(expr::Expr) = Expr(:block, _makevar.(_expand(expr))...)

_expand(s::Symbol) = s
function _expand(expr::Expr)
    @capture(expr, var_Symbol[lb_Int:ub_Int]) || error("Unsupported syntax. Expected x[a:b] for literal integers a and b")
    [Symbol(var, i) for i in lb:ub]
end

_splice(s::Symbol) = esc(s)
_splice(s::AbstractVector{Symbol}) = Expr(:tuple, esc.(s)...)

"""
Construct polynomial variables and bind them to local variables of the same
name. Usage:

Create a single variable named x:

    @polyvar(x)

Create several variables at the same time:

    @polyvar(x, y, z)

Create a list of variables x1, x2, x3, x4:

    @polyvar(x[1:4])

Create variables x, y, a1, a2, a3:

    @polyvar(x, y, a[1:3])

You can also assign the results of the macro to a tuple:

    vars = @polyvar(x[1:5])
    @assert vars[1] == x1
    @assert typeof(vars[1]) == Variable{:x1}


"""
macro polyvar(names...)
    Expr(:block, _makevar.(names)..., Expr(:tuple, _splice.(_expand.(names))...))
end

macro polyvar(name)
    Expr(:block, _makevar(name), _splice(_expand(name)))
end

