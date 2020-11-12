# TypedPolynomials

[![Build Status](https://github.com/JuliaAlgebra/TypedPolynomials.jl/workflows/CI/badge.svg?branch=master)](https://github.com/JuliaAlgebra/TypedPolynomials.jl/actions?query=workflow%3ACI)
[![codecov.io](http://codecov.io/github/JuliaAlgebra/TypedPolynomials.jl/coverage.svg?branch=master)](http://codecov.io/github/JuliaAlgebra/TypedPolynomials.jl?branch=master)

TypedPolynomials.jl provides an implementation of the multivariate polynomial interface from [MultivariatePolynomials.jl](https://github.com/JuliaAlgebra/MultivariatePolynomials.jl) using *strongly typed* variables. That is, in this package, the identity of a variable is encoded by its type, so variables `x` and `y` are of different types. This allows us to use the type system to handle certain operations, like computing the intersection of two monomials' variables, at compile-time.

### Features

* Handling variables at the type level makes constructing variables, monomials, and terms more efficient than with [DynamicPolynomials.jl](https://github.com/JuliaAlgebra/DynamicPolynomials.jl).
* Despite the heavy use of the type system, this package has no `@generated` functions and should be compatible with static compilation (though this has not yet been tested).

### Caveats

* There is no distinction in this package between a variable's *name* and its identity. That is, two variables named `x` are exactly the same object, regardless of how they were created.
* For problems with large numbers of variables, or for which the set of variables is not known at compile-time, you may see better performance overall with DynamicPolynomials.jl, e.g. [#32](https://github.com/JuliaAlgebra/TypedPolynomials.jl/issues/32). This may change in the future.

# Usage

The easiest way to create variables is the `@polyvar` macro:

```julia
julia> @polyvar x y z  # Declare three `Variable`s named x, y, and z and assign local variables with the same names
(x, y, z)

julia> typeof(x)
TypedPolynomials.Variable{:x}

julia> typeof(y)
TypedPolynomials.Variable{:y}
```

Multiplying variables creates a `Monomial{V}` where `V` is the vector of variables contained in the monomial:

```julia
julia> x * y
xy

julia> typeof(x * y)
TypedPolynomials.Monomial{(x, y),2}

julia> typeof(x^2)
TypedPolynomials.Monomial{(x,),1}
```

Multiplying a monomial (or variable) by anything other than another `Variable` or `Monomial` creates a `Term`:

```julia
julia> 3 * x
3x

julia> typeof(3 * x)
TypedPolynomials.Term{Int64,TypedPolynomials.Monomial{(x,),1}}

julia> typeof(3.0 * x^2 * y)
TypedPolynomials.Term{Float64,TypedPolynomials.Monomial{(x, y),2}}
```

Addition or subtraction of terms, monomials, and/or variables creates a `Polynomial`:

```julia
julia> x + y
x + y

julia> typeof(x + y) <: Polynomial
true

julia> x + 3y^2 + z/2 + x^3
x^3 + 3.0y^2 + x + 0.5z
```

## More Examples

### Differentiation and Substitution

```julia
using TypedPolynomials
using Test
@polyvar x y # assigns x (resp. y) to a variable of name x (resp. y)
p = 2x + 3.0x*y^2 + y
@test differentiate(p, x) == 3y^2 + 2 # compute the derivative of p with respect to x
@test differentiate.(p, (x, y)) == (3y^2 + 2, 6*x*y + 1) # compute the gradient of p
@test p((x, y)=>(y, x)) == 2y + 3y*x^2 + x  # replace any x by y and y by x
@test p(y=>x^2) == 2x + 3x*(x^4) + x^2 # replace any occurence of y by x^2
@test p(x=>1, y=>2) == 2 * 1 + 3 * 1 * 2^2 + 2 # evaluate p at [1, 2]
```

### Vectors of Variables

The `@polyvar` macro can also create a tuple of variables automatically:

```julia
using TypedPolynomials
A = rand(3, 3)
@polyvar x[1:3] # assign x to a tuple of variables x1, x2, x3
p = sum(x .* x) # x_1^2 + x_2^2 + x_3^2
p(x[1]=>2, x[3]=>3) # x_2^2 + 13
p(x=>A*vec(x)) # corresponds to dot(A*x, A*x), need vec to convert the tuple to a vector
```
