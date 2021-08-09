use std::collections::HashMap;
use std::env::args;
use std::fs::File;
use std::io::{self, BufRead, BufWriter, Write};
use std::path::Path;

use lazy_static::lazy_static;
use num_bigint::{BigUint, ToBigUint};

type Dictionary = HashMap<BigUint, Vec<String>>;

lazy_static! {
    static ref ONE: BigUint = 1.to_biguint().unwrap();
    static ref TEN: BigUint =10.to_biguint().unwrap();
}

static DIGITS: [&str; 10] = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"];

/// Port of Peter Norvig's Lisp solution to the Prechelt phone-encoding problem.
///
/// Even though this is intended as a port, it deviates quite a bit from it
/// due to the very different natures of Lisp and Rust.
fn main() -> io::Result<()> {
    // drop itself from args
    let mut args: Vec<_> = args().skip(1).collect();
    let words_file: String = if !args.is_empty() { args.remove(0) } else { "tests/words.txt".into() };
    let input_file: String = if !args.is_empty() { args.remove(0) } else { "tests/numbers.txt".into() };

    let dict = load_dict(words_file)?;

    let stdout = io::stdout();
    let mut writer = BufWriter::new(stdout.lock());

    for line in read_lines(input_file)? {
        if let Ok(num) = line {
            let digits: Vec<_> = num.bytes()
                .filter(|ch| ch.is_ascii_alphanumeric())
                .collect();
            let mut words = Vec::new();
            print_translations(&num, &digits, 0, &mut words, &dict, &mut writer)?;
        }
    }
    Ok(())
}

fn print_translations<'dict, W: Write>(
    num: &str,
    digits: &[u8],
    start: usize,
    words: &mut Vec<&'dict str>,
    dict: &'dict Dictionary,
    writer: &mut BufWriter<W>,
) -> io::Result<()> {
    if start >= digits.len() {
        return print_solution(num, &words, writer);
    }
    let mut n = ONE.clone();
    let mut found_word = false;
    for i in start..digits.len() {
        n *= &*TEN;
        n += nth_digit(digits, i);
        if let Some(found_words) = dict.get(&n) {
            for word in found_words {
                found_word = true;
                words.push(word);
                print_translations(num, digits, i + 1, words, dict, writer)?;
                words.pop();
            }
        }
    }
    if !found_word && !words.last().map(|w| is_digit(w)).unwrap_or(false) {
        words.push(DIGITS[nth_digit(digits, start) as usize]);
        let res = print_translations(num, digits, start + 1, words, dict, writer);
        words.pop();
        res
    } else {
        Ok(())
    }
}

fn print_solution<W: Write>(num: &str, words: &[&str], writer: &mut BufWriter<W>) -> io::Result<()> {
    // do a little gymnastics here to avoid allocating a big string just for printing it
    write!(writer, "{}", num)?;
    write!(writer, ":")?;
    if words.is_empty() {
        writeln!(writer)?;
        return Ok(());
    }
    for word in words {
        write!(writer, " ")?;
        write!(writer, "{}", word)?;
    }
    writeln!(writer)?;
    Ok(())
}

fn load_dict(words_file: String) -> io::Result<Dictionary> {
    let mut dict = HashMap::with_capacity(100);
    let words = read_lines(words_file)?;
    for word in words.flatten() {
        let key = word_to_number(&word);
        let words = dict.entry(key).or_insert_with(|| Vec::new());
        words.push(word);
    }
    Ok(dict)
}

// The output is wrapped in a Result to allow matching on errors
// Returns an Iterator to the Reader of the lines of the file.
fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
    where P: AsRef<Path>, {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}

fn word_to_number(word: &str) -> BigUint {
    let mut n = ONE.clone();
    for ch in word.bytes().filter_map(char_to_digit) {
        n *= &*TEN;
        n += ch;
    }
    n
}

fn nth_digit(digits: &[u8], i: usize) -> u8 {
    let ch = digits.get(i).expect("index out of bounds");
    *ch - b'0' as u8
}

fn is_digit(string: &str) -> bool {
    string.len() == 1 && string.chars().next().unwrap().is_digit(10)
}

fn char_to_digit(ch: u8) -> Option<u8> {
    Some(match ch.to_ascii_lowercase() {
        b'e' => 0,
        b'j' | b'n' | b'q' => 1,
        b'r' | b'w' | b'x' => 2,
        b'd' | b's' | b'y' => 3,
        b'f' | b't' => 4,
        b'a' | b'm' => 5,
        b'c' | b'i' | b'v' => 6,
        b'b' | b'k' | b'u' => 7,
        b'l' | b'o' | b'p' => 8,
        b'g' | b'h' | b'z' => 9,
        _ => return None
    })
}
