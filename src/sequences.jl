module Sequences

export shortest_common_supersequence

"""
Given arrays (or strings) a and b, returns a table of size
(length(a) + 1) x (length(b) + 1)
Each entry i,j in the table is the length of the longest
common subsequence among a[1:(i-1)] and b[1:(j-1)]
Adapted from https://github.com/WestleyArgentum/Subsequences.jl
Copyright 2015 WestleyArentum and other contributors, released
under the MIT "Expat" License.
"""
function longest_common_subsequence_table(a, b)
    lengths = zeros(Int, length(a) + 1, length(b) + 1)
    for i in 1:length(a)
        ai = a[i]
        for j in 1:length(b)
            if ai == b[j]
                lengths[i+1, j+1] = lengths[i, j] + 1
            else
                lengths[i+1, j+1] = max(lengths[i+1, j], lengths[i, j+1])
            end
        end
    end
    lengths
end

"""
Given arrays (or strings) a and b, returns a table of size
(length(a) + 1) x (length(b) + 1)
Each entry i,j in the table is the length of the shortest
common supersequence among a[1:(i-1)] and b[1:(j-1)]
"""
function shortest_common_supersequence_table(a, b)
    table = longest_common_subsequence_table(a, b)
    for i in 1:size(table, 1)
        for j in 1:size(table, 2)
            table[i, j] = (i - 1) + (j - 1) - table[i, j]
        end
    end
    table
end

"""
Given arrays a and b, returns the shortest
common supersequence of a and b.
See: https://www.ics.uci.edu/~eppstein/161/960229.html
"""
function shortest_common_supersequence(a::Union{NTuple, AbstractVector}, b::Union{NTuple, AbstractVector})
    table = shortest_common_supersequence_table(a, b)
    result = promote_type(eltype(a), eltype(b))[]
    i = size(table, 1) - 1
    j = size(table, 2) - 1
    while i > 0 || j > 0
        if i == 0
            push!(result, b[j])
            j -= 1
        elseif j == 0
            push!(result, a[i])
            i -= 1
        elseif a[i] == b[j]
            push!(result, a[i])
            i -= 1
            j -= 1
        elseif table[i, j + 1] > table[i + 1, j]
            push!(result, b[j])
            j -= 1
        else
            push!(result, a[i])
            i -= 1
        end
    end
    reverse!(result)
    result
end

function shortest_common_supersequence(a::AbstractString, b::AbstractString)
    scs = shortest_common_supersequence(collect(a), collect(b))
    join(scs)
end

end
