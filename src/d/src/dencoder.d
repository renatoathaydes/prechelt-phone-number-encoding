import std.stdio : write, writeln, File;
import std.conv : to;
import std.outbuffer : OutBuffer;
import std.ascii : isAlpha, isDigit;
import std.algorithm.iteration : filter;

@safe:

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

    this(ubyte[] value)
    {
        this.value = value;
        foreach (i, b; value)
        {
            hash ^= b << (8 * (i % size_t.sizeof));
        }
    }

    void append(ubyte b)
    {
        auto i = value.length;
        value ~= b;
        hash ^= b << (8 * (i % size_t.sizeof));
    }

    size_t toHash() const @safe pure nothrow => hash;

    bool opEquals(ref const typeof(this) s) const @safe pure nothrow => this.value == s.value;
}

private Key wordToNumber(string word)
{
    ubyte[] result;
    foreach (c; word)
    {
        if (c.isAlpha)
        {
            result ~= digitByLetter[c];
        }
    }
    return Key(result);
}

private bool lastItemIsDigit(string[] words)
{
    return words.length != 0 && words[$ - 1].length == 1 && words[$ - 1][0].isDigit;
}

void printTranslations(string[][Key] dict, ISolutionHandler shandler,
    string number, string digits, string[] words)
{
    if (digits.length == 0)
    {
        shandler.put(number, words);
        return;
    }
    auto key = Key(new ubyte[0]);
    bool foundWord = false;
    foreach (i, c; digits)
    {
        key.append(cast(ubyte)(c - '0'));
        string[]* foundWords = key in dict;
        if (foundWords !is null)
        {
            foundWord = true;
            foreach (word; *foundWords)
            {
                words ~= word;
                dict.printTranslations(shandler, number, digits[i + 1 .. $], words);
                words = words[0 .. $ - 1];
            }
        }
    }
    if (!foundWord && !words.lastItemIsDigit)
    {
        string digit = [digits[0]];
        words ~= digit;
        dict.printTranslations(shandler, number, digits[1 .. $], words);
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
    void put(string number, string[] words);
}

final class Printer : ISolutionHandler
{
    private OutBuffer buf = new OutBuffer();
    private int counter;

    this()
    {
        buf.reserve(4096);
    }

    void flush()
    {
        write(buf.toString());
        buf.clear();
    }

    void put(string number, string[] words)
    {
        buf.writefln("%s:%-( %s%)", number, words);
        counter++;
        if (counter >= 100)
        {
            counter = 0;
            flush();
        }
    }
}

final class Counter : ISolutionHandler
{
    private int count;

    void flush() => writeln(count);

    void put(string number, string[] words)
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
    auto words = new string[16];
    foreach (number; numbersFile.byLine)
    {
        auto num = number.idup;
        string digits = num.filter!(c => c.isDigit)
            .to!string;
        dict.printTranslations(shandler, num, digits, words[0 .. 0]);
    }
    shandler.flush();
}
