show(io::IO, v::Variable{Name}) where Name = print(io, Name)
function show(io::IO, t::Term)
    if t.coefficient == 0
        print(io, "0")
    elseif t.coefficient == 1
        print(io, t.monomial)
    else
        print(io, t.coefficient)
        if !all(t.monomial.exponents .== 0)
            print(io, t.monomial)
        end
    end
end

format_exponent(e) = e == 1 ? "" : "^$e"
function show(io::IO, m::Monomial)
    if all(m.exponents .== 0)
        print(io, "1")
    else
        for (i, v) in enumerate(variables(m))
            if m.exponents[i] != 0
                print(io, v, format_exponent(m.exponents[i]))
            end
        end
    end
end

function show(io::IO, p::Polynomial)
    if isempty(p.terms)
        print(io, "0")
    else
        print(io, p.terms[1])
        for i in 2:length(p.terms)
            print(io, " + ", p.terms[i])
        end
    end
end
