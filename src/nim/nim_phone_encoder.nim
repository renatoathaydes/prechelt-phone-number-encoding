import tables
import strutils
import sequtils
import strformat

template LOG(x: varargs[untyped]) =
  when defined(debug):
    echo "LOG: ", x

type
  Dictionary = TableRef[int, seq[string]]

func to_number(ch: char): int =
  case ch
  of 'e','E': 0
  of 'j','J','n','N','q','Q': 1
  of 'r','R','w','W','x','X': 2
  of 'd','D','s','S','y','Y': 3
  of 'f','F','t','T': 4
  of 'a','A','m','M': 5
  of 'c','C','i','I','v','V': 6
  of 'b','B','k','K','u','U': 7
  of 'l','L','o','O','p','P': 8
  of 'g','G','h','H','z','Z': 9
  else: raise ValueError.newException("Invalid character: " & ch)

func word_to_number(word: string): int =
  ## Convert a word to an phone-number-mapped integer hash
  result = 1
  for ch in word:
    if ch.isAlphaAscii:
      result = result * 10 + ch.to_number()

proc load_dict(filename: string): Dictionary =
  ## Load a dictionary from the filesystem
  result = newTable[int, seq[string]]()
  let fh = open(filename)
  for word in fh.lines:
    if word.len > 0:
      let key = word.word_to_number()
      discard result.hasKeyOrPut(key, @[])
      result[key].add word

proc translations(dict: Dictionary, digits: string, prev = 1, index = 0, can_be_digit = true): seq[seq[string]] =
  LOG digits
  LOG " ".repeat(index), "^", " ", prev
  if index >= digits.len:
    return @[]
  let isEnd = index == (digits.len - 1)
  let ch = digits[index]
  var hash = prev
  hash = hash * 10 + parseInt($ch)
  LOG "hash ", prev, " -> ", hash
  var found_word = false
  if dict.hasKey(hash):
    # found word(s)
    LOG "found word(s) ", dict[hash]
    found_word = true
    for word in dict[hash]:
      if isEnd:
        # No more digits to process.
        LOG "no more digits"
        result.add @[word]
      else:
        # More digits to process...
        LOG "more digits..."
        for x in dict.translations(digits, prev = 1, index = index + 1):
          result.add concat(@[word], x)

  for x in dict.translations(digits, prev = hash, index = index + 1, can_be_digit = false):
    found_word = true
    result.add x
  
  if can_be_digit and not found_word:
    LOG "going for digit", $ch
    if isEnd:
      # No more digits to process
      result.add @[$ch]
    else:
      # More digits to process...
      for x in dict.translations(digits, prev = 1, index = index + 1, can_be_digit = false):
        result.add concat(@[$ch], x)

func just_digits(phone: string): string =
  phone.filterIt(it in '0'..'9').join

iterator iter_translations(dict: Dictionary, phone: string): string =
  ## Give possible translations for a phone number
  let digits = phone.just_digits
  for x in dict.translations(digits):
    yield x.join(" ")

when isMainModule:
  import os
  let params = commandLineParams()
  let words_file = if params.len > 0: params[0] else: "tests/words.txt"
  let input_file = if params.len > 1: params[1] else: "tests/numbers.txt"

  let dict = load_dict(words_file)
  LOG dict
  let fh = open(input_file)
  for line in fh.lines:
    let phone = line.filterIt(it.isAlphaNumeric).join
    for phrase in dict.iter_translations(phone):
      echo &"{line}: {phrase}"
    
