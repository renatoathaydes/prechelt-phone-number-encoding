import std.stdio : write, writeln, File;
import std.conv : to;
import std.outbuffer : OutBuffer;
import std.ascii : isAlpha, isDigit;
import std.algorithm.iteration : filter;
import std.container.array : Array;

// @safe:

static const letters = [
    ['e', 'E'],
    ['J', 'N', 'Q', 'j', 'n', 'q'],
    ['R', 'W', 'X', 'r', 'w', 'x'],
    ['D', 'S', 'Y', 'd', 's', 'y'],
    ['F', 'T', 'f', 't'],
    ['A', 'M', 'a', 'm'],
    ['C', 'I', 'V', 'c', 'i', 'v'],
    ['B', 'K', 'U', 'b', 'k', 'u'],
    ['L', 'O', 'P', 'l', 'o', 'p'],
    ['G', 'H', 'Z', 'g', 'h', 'z'],
];

static const digitByLetter = buildDigitByLetter();

ubyte[char] buildDigitByLetter()
{
    ubyte[char] result;
    static foreach (i, row; letters)
    {
        static foreach (letter; row)
        {
            result[letter] = cast(ubyte) i;
        }
    }
    return result;
}

struct Key
{
    ubyte[] value;
    size_t hash;

    size_t toHash() const pure nothrow => hash;

    bool opEquals(const Key other) const @safe nothrow pure
    {
        return value == other.value;
    }
}

size_t hasher(const ubyte b, const size_t prev) @safe @nogc pure {
    return b ^ (prev << 4);
}

unittest {
    Key k;
    ubyte[3] value = [5,6,2];
    size_t hash = 0;
    hash = hasher(cast(ubyte) 1, hash);
    k.value = value[0 .. 1];
    k.hash = hash;
    assert("m".wordToNumber == k);
    hash = hasher(cast(ubyte)2, hash);
    k.value = value[0 .. 2];
    k.hash = hash;
    assert("mi".wordToNumber == k);
    hash = hasher(cast(ubyte)3, hash);
    k.value = value[0 .. 3];
    k.hash = hash;
    assert("mir".wordToNumber == k);
    writeln(k);
}

private Key wordToNumber(string word)
{
    size_t hash = 0;
    ubyte[] value = new ubyte[word.length];
    size_t index;
    foreach (c; word)
    {
        if (c.isAlpha)
        {
            auto b = digitByLetter[c];
            value[index] = b;
            hash = hasher(b, hash);
            index++;
        }
    }
    return Key(value[0 .. index], hash);
}

private bool lastItemIsDigit(in Array!string words)
{
    if (words.empty)
        return false;
    const back = words.back();
    return back.length == 1 && back[0].isDigit;
}

void printTranslations(in string[][Key] dict, ISolutionHandler shandler,
    string number, string digits, Array!string words)
{
    if (digits.length == 0)
    {
        shandler.put(number, words);
        return;
    }
    bool foundWord = false;
    Key n;
    size_t hash = 0;
    ubyte[64] keyValue;
    foreach (i, c; digits)
    {
        auto b = cast(ubyte) (c - '0');
        keyValue[i] = b;
        hash = hasher(b, hash);
        n.value = keyValue[0 .. i + 1];
        n.hash = hash;
        auto foundWords = n in dict;
        if (foundWords !is null)
        {
            foundWord = true;
            foreach (word; *foundWords)
            {
                words.insertBack(word);
                dict.printTranslations(shandler, number, digits[i + 1 .. $], words);
                words.removeBack();
            }
        }
    }
    if (!foundWord && !words.lastItemIsDigit)
    {
        words.insertBack(digits[0 .. 1]);
        dict.printTranslations(shandler, number, digits[1 .. $], words);
        words.removeBack();
    }
}

string[][Key] loadDictionary(string path) @trusted
{
    auto file = new File(path);
    string[][Key] result;
    foreach (line; file.byLine)
    {
        auto word = line.idup;
        auto n = word.wordToNumber;
        auto words = n in result;
        if (words is null)
        {
            result[n] = [word];
        }
        else
        {
            *words ~= word;
        }
    }
    return result;
}

interface ISolutionHandler
{
    void flush();
    void put(string number, in Array!string words);
}

final class Printer : ISolutionHandler
{
    import std.array : Appender;

    private enum capacity = 10 * 4_096;

    private Appender!(char[]) buf;

    this() {
        buf.reserve(capacity);
    }

    void flush()
    {
        write(buf[]);
        buf.shrinkTo(0);
    }

    void put(string number, in Array!string words)
    {
        buf ~= number;
        buf ~= ":";
        foreach (word; words)
        {
            buf ~= ' ';
            buf ~= word;
        }
        buf ~= '\n';
        
        if (buf[].length > capacity - 256)
        {
            flush();
        }
    }
}

final class Counter : ISolutionHandler
{
    private int count;

    void flush() => writeln(count);

    void put(string number, in Array!string words)
    {
        count++;
    }
}

ISolutionHandler createSolutionHandler(string arg)
{
    switch (arg)
    {
    case "print":
        return new Printer;
    case "count":
        return new Counter;
    default:
        assert(0, "First argument must be 'print' or 'count'");
    }
}

void main(string[] args) @trusted
{
    auto shandler = args.length > 1 ? createSolutionHandler(args[1]) : new Printer;
    auto file = args.length > 2 ? args[2] : "tests/words.txt";
    auto numbers = args.length > 3 ? args[3] : "tests/numbers.txt";
    auto dict = loadDictionary(file);
    auto numbersFile = File(numbers);
    Array!string words;
    words.reserve(128);
    foreach (number; numbersFile.byLine)
    {
        auto num = number.idup;
        string digits = num.filter!(c => c.isDigit)
            .to!string;
        dict.printTranslations(shandler, num, digits, words);
    }
    shandler.flush();
}
