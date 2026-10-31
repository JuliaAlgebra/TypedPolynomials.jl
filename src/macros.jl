_new_variable(name::Symbol) = :(Variable{$(esc(Expr(:quote, name)))}())
_assign_new_variable(name::Symbol) = :($(esc(name)) = $(_new_variable(name)))

function _split(expr::Expr)
    @capture(expr, var_Symbol[lb_Int:ub_Int]) ||
        error("Unsupported syntax. Expected x[a:b] for literal integers a and b")
    var, lb, ub
end

function _assign_new_variable(expr::Expr)
    var, lb, ub = _split(expr)
    :($(esc(var)) = $(Expr(:tuple, [_new_variable(Symbol("$var[$i]")) for i = lb:ub]...)))
end

_return_name(s::Symbol) = esc(s)
function _return_name(expr::Expr)
    var, lb, ub = _split(expr)
    esc(var)
end

"""
Construct polynomial variables and bind them to local variables of the same
name. Usage:

Create a single variable named x:

    @polyvar(x)

Create several variables at the same time:

    @polyvar(x, y, z)

Create a tuple of variables x = (x1, x2, x3, x4):

    @polyvar(x[1:4])

Create variables x, y, a = (a1, a2, a3):

    @polyvar(x, y, a[1:3])

At global scope, declare every created binding constant with `constant = true`:

    @polyvar x y a[1:3] constant = true

`constant` must be a literal `true` or `false` and defaults to `false`.
A single declaration still returns its binding; multiple declarations return
a tuple of the bindings.

You can also assign the results of the macro to a tuple:

    vars = @polyvar(x[1:5])
    @assert typeof(vars[1]) == Variable{:x1}


"""
macro polyvar(args...)
    names = Any[]
    constant = false
    for arg in args
        if Base.Meta.isexpr(arg, :(=))
            arg.args[1] == :constant ||
                error("Unrecognized keyword argument `$(arg.args[1])`")
            constant = arg.args[2]
            constant isa Bool ||
                error("Keyword argument `constant` must be a literal `true` or `false`")
        else
            push!(names, arg)
        end
    end
    assignments = _assign_new_variable.(names)
    if constant
        assignments = [Expr(:const, expr) for expr in assignments]
    end
    result = if length(names) == 1
        _return_name(only(names))
    else
        Expr(:tuple, _return_name.(names)...)
    end
    return Expr(:block, assignments..., result)
end
