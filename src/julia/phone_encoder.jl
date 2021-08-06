#=
# Port of Peter Norvig's Common Lisp program from http://norvig.com/java-lisp.html.
#
# - Julia version: 1.6.2
# - Author: Renato Athaydes
# - Date: 2021-07-24
=#
const emptyStrings = String[]

const KEYS = begin
    m = [
         'j' 'r' 'd' 'f' 'a' 'c' 'b' 'l' 'g'
         'n' 'w' 's' 't' 'm' 'i' 'k' 'o' 'h'
         'q' 'x' 'y' '_' '_' 'v' 'u' 'p' 'z'
        ]
    map('a':'z') do c
        n = 1
        for col in eachcol(m) if c in col return n else n += 1 end end
        0
    end
end

function printTranslations(dict, num, digits, start=1, words=String[])
    if start > length(digits)
       return println(num, ": ", join(words, " "))
    end
    foundWord = false
    n = BigInt(1)
    for i in start:length(digits)
        n = n * 10 + nth_digit(digits, i)
        for word in get(dict, n, emptyStrings)
            foundWord = true
            printTranslations(dict, num, digits, i + 1, [words; word])
        end
    end
    if !foundWord &&
        !(!isempty(words) && length(words[end]) == 1 && isdigit(words[end][begin]))
        printTranslations(dict, num, digits, start + 1, [words; string(nth_digit(digits, start))])
    end
end

nth_digit(digits, i) = digits[i] - '0'

letter2digit(i) = KEYS[i > 0x00060 ? i - 0x00060 : i - 0x00040]

function word2number(word)
    n = BigInt(1)
    for ch in word
        if isletter(ch) && isascii(ch)
            n = n * 10 + letter2digit(UInt32(ch))
        end
    end
    n
end

function main()
    dict = open(isempty(ARGS) ? "tests/words.txt" : ARGS[begin]) do file
        d = Dict{BigInt, Vector{String}}()
        for word in eachline(file)
            push!(get!(d, word2number(word), String[]), word)
        end
        d
    end

    open(length(ARGS) < 2 ? "tests/numbers.txt" : ARGS[begin+1]) do file
        for num in eachline(file)
            printTranslations(dict, num, filter(isdigit, num))
        end
    end
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
