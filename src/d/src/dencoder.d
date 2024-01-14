import std.int128;
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

size_t[char] buildDigitByLetter()
{
    size_t[char] result;
    static foreach (i, row; letters)
    {
        static foreach (letter; row)
        {
            result[letter] = i;
        }
    }
    return result;
}

private Int128 wordToNumber(string word)
{
    Int128 result = Int128(1L);
    foreach (c; word)
    {
        if (c.isAlpha)
        {
            result *= 10;
            result += digitByLetter[c];
        }
    }
    return result;
}

private bool lastItemIsDigit(Array!string words)
{
    if (words.empty)
        return false;
    const back = words.back();
    return back.length == 1 && back[0].isDigit;
}

void printTranslations(string[][Int128] dict, ISolutionHandler shandler,
    string number, string digits, Array!string words)
{
    if (digits.length == 0)
    {
        shandler.put(number, words);
        return;
    }
    bool foundWord = false;
    Int128 n = Int128(1L);
    foreach (i, c; digits)
    {
        n *= 10;
        n += c - '0';
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

string[][Int128] loadDictionary(string path) @trusted
{
    auto file = new File(path);
    string[][Int128] result;
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
