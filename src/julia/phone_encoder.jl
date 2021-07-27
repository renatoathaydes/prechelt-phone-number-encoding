#=
# Port of Peter Norvig's Common Lisp program from http://norvig.com/java-lisp.html.
#
# - Julia version: 1.6.2
# - Author: Renato Athaydes
# - Date: 2021-07-24
=#
const emptyStrings = String[]

function printTranslations(num, digits, start=1, words=String[])
    if start > length(digits)
       return println(num, ": ", join(words, " "))
    end
    foundWord = false
    n = BigInt(1)
    for i in start:length(digits)
        n = n * 10 + nthDigit(digits, i)
        for word in get(dict, n, emptyStrings)
            foundWord = true
            printTranslations(num, digits, i + 1, [words; word])
        end
    end
    if !foundWord &&
        !(!isempty(words) && length(words[end]) == 1 && isdigit(words[end][begin]))
        printTranslations(num, digits, start + 1, [words; string(nthDigit(digits, start))])
    end
end

function loadDictionary(file)::Dict{BigInt, Vector{String}}
    local dict = Dict{BigInt, Vector{String}}()
    for word in eachline(file)
        push!(get!(dict, wordToNumber(word)) do; String[] end, word)
    end
    dict
end

function nthDigit(digits::String, i::Int64)::UInt
    UInt(digits[i]) - UInt('0')
end

const charDigits = Dict{Char, UInt}(
    'e' => 0,
    'j' => 1, 'n' => 1, 'q'=> 1,
    'r' => 2, 'w' => 2, 'x' => 2,
    'd' => 3, 's' => 3, 'y' => 3,
    'f' => 4, 't' => 4,
    'a' => 5, 'm' => 5,
    'c' => 6, 'i' => 6, 'v' => 6,
    'b' => 7, 'k' => 7, 'u' => 7,
    'l' => 8, 'o' => 8, 'p' => 8,
    'g' => 9, 'h' => 9, 'z' => 9,
)

function charToDigit(ch::Char)::UInt
    ch = lowercase(ch)
    get(charDigits, ch) do
        throw(DomainError(ch, "Not a letter"))
    end
end

function wordToNumber(word::String)::BigInt
    n = BigInt(1)
    for ch in word
        if isletter(ch) && isascii(ch)
            n = n * 10 + charToDigit(ch)
        end
    end
    n
end


dict = open(isempty(ARGS) ? "tests/words.txt" : ARGS[begin]) do file
    loadDictionary(file)
end

open(length(ARGS) < 2 ? "tests/numbers.txt" : ARGS[begin+1]) do file
    for num in eachline(file)
        printTranslations(num, filter(isdigit, num))
    end
end
