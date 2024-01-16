import std.stdio : write, writeln, File;
import std.conv : to;
import std.outbuffer : OutBuffer;
import std.ascii : isAlpha, isDigit;
import std.algorithm.iteration : filter;
import std.container.array;

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

unittest {
    Key k;
    ubyte[3] value = [5,6,2];
    size_t hash = 0;
    hash = hashOf(cast(ubyte) 1, hash);
    k.value = value[0 .. 1];
    k.hash = hash;
    assert("m".wordToNumber == k);
    hash = hashOf(cast(ubyte)2, hash);
    k.value = value[0 .. 2];
    k.hash = hash;
    assert("mi".wordToNumber == k);
    hash = hashOf(cast(ubyte)3, hash);
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
            hash = hashOf(b, hash);
            index++;
        }
    }
    return Key(value[0 .. index], hash);
}

private bool lastItemIsDigit(Array!string words)
{
    if (words.empty)
        return false;
    const back = words.back();
    return back.length == 1 && back[0].isDigit;
}

void printTranslations(string[][Key] dict, ISolutionHandler shandler,
    string number, string digits, Array!string words)
{
    if (digits.length == 0)
    {
        shandler.put(number, words);
        return;
    }
    bool foundWord = false;
    auto keyValue = new ubyte[number.length];
    Key n;
    size_t hash = 0;
    foreach (i, c; digits)
    {
        auto b = cast(ubyte) (c - '0');
        keyValue[i] = b;
        hash = hashOf(b, hash);
        n.value = keyValue[0 .. i + 1];
        n.hash = hash;
        string[]* foundWords = n in dict;
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
        string digit = [digits[0]];
        words.insertBack(digit);
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
    void put(string number, Array!string words);
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

    void put(string number, Array!string words)
    {
        buf.writefln("%s:%-( %s%)", number, words[]);
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

    void put(string number, Array!string words)
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
