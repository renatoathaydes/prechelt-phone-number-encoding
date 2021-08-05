#=
# Port of Peter Norvig's Common Lisp program from http://norvig.com/java-lisp.html.
#
# - Julia version: 1.6.2
# - Author: Renato Athaydes
# - Date: 2021-07-24
=#
const emptyStrings = String[]

function printTranslations(io, dict, num, digits, start=1, words=String[])
    if start > length(digits)
       return println(io, num, ": ", join(words, " "))
    end
    foundWord = false
    n = BigInt(1)
    for i in start:length(digits)
        Base.GMP.MPZ.mul_si!(n, 10)
        Base.GMP.MPZ.add_ui!(n, nthDigit(digits, i))
        for word in get(dict, n, emptyStrings)
            foundWord = true
            printTranslations(io, dict, num, digits, i + 1, [words; word])
        end
    end
    if !foundWord &&
        !(!isempty(words) && length(words[end]) == 1 && isdigit(words[end][begin]))
        printTranslations(io, dict, num, digits, start + 1, [words; string(nthDigit(digits, start))])
    end
end

function loadDictionary(file)::Dict{BigInt, Vector{String}}
    local dict = Dict{BigInt, Vector{String}}()
    for word in eachline(file)
        push!(get!(dict, wordToNumber(word)) do; String[] end, word)
    end
    dict
end

function nthDigit(digits::String, i::Int64)
    UInt(digits[i]) - UInt('0')
end

function charToDigit(ch::Char)
    ch = lowercase(ch)
    ch == 'e' && return 0
    ch in ('j', 'n', 'q') && return 1
    ch in ('r', 'w', 'x') && return 2
    ch in ('d', 's', 'y') && return 3
    ch in ('f', 't') && return 4
    ch in ('a', 'm') && return 5
    ch in ('c', 'i', 'v') && return 6
    ch in ('b', 'k', 'u') && return 7
    ch in ('l', 'o', 'p') && return 8
    ch in ('g', 'h', 'z') && return 9
    throw(DomainError(ch, "Not a letter"))
end

function wordToNumber(word::String)
    n = BigInt(1)
    for ch in word
        if isletter(ch) && isascii(ch)
            Base.GMP.MPZ.mul_si!(n, 10)
            Base.GMP.MPZ.add_ui!(n, charToDigit(ch))
        end
    end
    n
end

# patch in method to add integer to BigInt in-place
@eval Base.GMP.MPZ begin
    add_ui!(x::BigInt, a::BigInt, b) = (ccall((:__gmpz_add_ui, :libgmp), Cvoid, (mpz_t, mpz_t, Clong), x, a, b); x)
    add_ui!(x::BigInt, b) = add_ui!(x, x, b)
end

# function main()
    dict = open(isempty(ARGS) ? "tests/words.txt" : ARGS[begin]) do file
        loadDictionary(file)
    end
    io = IOBuffer()
    open(length(ARGS) < 2 ? "tests/numbers.txt" : ARGS[begin+1]) do file
        for num in eachline(file)
            printTranslations(io, dict, num, filter(isdigit, num))
            print(String(take!(io)))
        end
    end
    close(io)

# end

# main()