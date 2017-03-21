show(io::IO, v::AbstractVariable) = print(io, name(v))

function show(io::IO, t::AbstractTerm)
    c = coefficient(t)
    if c == 0
        print(io, "0")
    elseif c == 1
        print(io, monomial(t))
    else
        print(io, c)
        if !all(exponents(monomial(t)) .== 0)
            print(io, monomial(t))
        end
    end
end

format_exponent(e) = e == 1 ? "" : "^$e"
function show(io::IO, m::AbstractMonomial)
    exps = exponents(m)
    if all(exps .== 0)
        print(io, "1")
    else
        for (i, v) in enumerate(variables(m))
            if exps[i] != 0
                print(io, v, format_exponent(exps[i]))
            end
        end
    end
end

function show(io::IO, p::AbstractPolynomial)
    ts = terms(p)
    if isempty(ts)
        print(io, "0")
    else
        print(io, ts[1])
        for i in 2:length(ts)
            print(io, " + ", ts[i])
        end
    end
end
