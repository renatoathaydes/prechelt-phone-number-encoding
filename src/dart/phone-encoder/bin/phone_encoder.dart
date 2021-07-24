import 'dart:convert';
import 'dart:io';

import 'package:charcode/ascii.dart';

typedef Dictionary = Map<BigInt, List<String>>;

final atoz = List.generate($z + 1 - $a, (index) => $a + index).toSet();
final AtoZ = List.generate($Z + 1 - $A, (index) => $A + index).toSet();
final zeroToNine = List.generate($9 + 1 - $0, (index) => $0 + index).toSet();
final ten = BigInt.from(10);

void main(List<String> args) async {
  await _run(args.isNotEmpty ? args[0] : 'tests/words.txt',
      args.length > 1 ? args[1] : 'tests/numbers.txt');
}

Future<void> _run(String dict, String nums) async {
  final dictionary = await _loadDictionary(File(dict));
  await for (final num in File(nums).readLines()) {
    await _printTranslations(num, await num.digits.toList(), dictionary);
  }
}

Future<void> _printTranslations(String num, List<int> digits, Dictionary dict,
    [int start = 0, List<String> words = const []]) async {
  if (start >= digits.length) {
    print('$num: ${words.join(' ')}');
    return;
  }
  var foundWord = false;
  var n = BigInt.one;
  for (var i = start; i < digits.length; i++) {
    n = n * ten + BigInt.from(_nthDigit(digits, i));
    dict[n]?.forEach((word) {
      foundWord = true;
      _printTranslations(num, digits, dict, i + 1, words + [word]);
    });
  }
  if (!foundWord && !(words.isNotEmpty && words.last.isDigit)) {
    await _printTranslations(num, digits, dict, start + 1,
        words + [_nthDigit(digits, start).toString()]);
  }
}

Future<Dictionary> _loadDictionary(File dict) async {
  final table = <BigInt, List<String>>{};
  await for (var word in dict.readLines()) {
    table.update(word.toNumber(), (v) => v, ifAbsent: () => []).add(word);
  }
  return table;
}

int _nthDigit(List<int> digits, int i) => digits[i] - $0;

extension on File {
  Stream<String> readLines() async* {
    final lines =
        openRead().transform(ascii.decoder).transform(const LineSplitter());
    await for (final line in lines) {
      yield line;
    }
  }
}

extension on String {
  BigInt toNumber() {
    var n = BigInt.one;
    for (int char in runes) {
      final ch = char.lowerCase;
      if (ch != null && atoz.contains(ch)) {
        n = ten * n + BigInt.from(ch.toDigit());
      }
    }
    return n;
  }

  bool get isDigit => length == 1 && zeroToNine.contains(runes.first);

  Stream<int> get digits async* {
    for (var value in runes) {
      if (zeroToNine.contains(value)) yield value;
    }
  }
}

extension on int {
  int? get lowerCase {
    if (atoz.contains(this)) return this;
    if (AtoZ.contains(this)) return this - ($A - $a);
    return null;
  }

  int toDigit() {
    switch (this) {
      case $e:
        return 0;
      case $j:
      case $n:
      case $q:
        return 1;
      case $r:
      case $w:
      case $x:
        return 2;
      case $d:
      case $s:
      case $y:
        return 3;
      case $f:
      case $t:
        return 4;
      case $a:
      case $m:
        return 5;
      case $c:
      case $i:
      case $v:
        return 6;
      case $b:
      case $k:
      case $u:
        return 7;
      case $l:
      case $o:
      case $p:
        return 8;
      case $g:
      case $h:
      case $z:
        return 9;
      default:
        throw Exception('Not a digit: $this');
    }
  }
}
