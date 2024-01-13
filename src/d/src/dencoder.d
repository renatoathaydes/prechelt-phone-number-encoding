import std.bigint;

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

private BigInt wordToNumber(string word)
{
  import std.ascii : isAlphaNum;

  BigInt result = 1;
  foreach (c; word)
  {
    if (c.isAlphaNum)
    {
      result = result * 10 + digitByLetter[c];
    }
  }
  return result;
}

private bool lastItemNotDigit(string[] words)
{
  import std.ascii : isDigit;

  return words.length == 0 || words[$ - 1].length != 1 || !words[$ - 1][0].isDigit;
}

void printTranslations(string[][BigInt] dict, string number, string digits, string[] words = [
])
{
  import std.stdio : writefln;
  import std.conv : to;

  if (digits.length == 0)
  {
    writefln("%s:%-( %s%)", number, words);
    return;
  }
  bool foundWord = false;
  BigInt n = 1;
  foreach (i, c; digits)
  {
    n = n * 10 + (c - '0');
    string[]* foundWords = n in dict;
    if (foundWords !is null)
    {
      foundWord = true;
      foreach (word; *foundWords)
      {
        dict.printTranslations(number, digits[i + 1 .. $], words ~ word);
      }
    }
  }
  if (!foundWord && words.lastItemNotDigit)
  {
    string digit = [digits[0]];
    dict.printTranslations(number, digits[1 .. $], words ~ digit);
  }
}

string[][BigInt] loadDictionary(string path) @trusted
{
  import std.stdio : File;

  auto file = new File(path);
  string[][BigInt] result;
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

version (unittest)
{
  import dshould;

  unittest
  {
    "a".wordToNumber.should.equal(BigInt(15));
  }

  unittest
  {
    [].lastItemNotDigit.should.equal(true);
    [""].lastItemNotDigit.should.equal(true);
    ["a"].lastItemNotDigit.should.equal(true);
    ["0"].lastItemNotDigit.should.equal(false);
    ["9"].lastItemNotDigit.should.equal(false);
  }
}
else
  void main(string[] args) @trusted
{
  import std.stdio : File;
  import std.algorithm.iteration : filter;
  import std.ascii : isDigit;
  import std.conv : to;

  auto file = args.length > 1 ? args[1] : "tests/words.txt";
  auto numbers = args.length > 2 ? args[2] : "tests/numbers.txt";
  auto dict = loadDictionary(file);
  auto numbersFile = File(numbers);
  foreach (number; numbersFile.byLine)
  {
    auto num = number.idup;
    string digits = num.filter!(c => c.isDigit)
      .to!string;
    dict.printTranslations(num, digits);
  }

}
