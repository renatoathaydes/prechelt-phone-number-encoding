use std::collections::HashMap;
use std::env::args;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;
use std::fmt::{self, Display, Formatter};

use lazy_static::lazy_static;
use num_bigint::{BigUint, ToBigUint};

type Dictionary = HashMap<Vec<u8>, Vec<String>>;

lazy_static! {
    static ref ONE: BigUint = 1.to_biguint().unwrap();
    static ref TEN: BigUint =10.to_biguint().unwrap();
}

#[derive(Debug, Copy, Clone)]
enum WordOrDigit<'a> {
    Word(&'a str),
    Digit(u8),
}

impl Display for WordOrDigit<'_> {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        match self {
            WordOrDigit::Word(s) => s.fmt(formatter),
            WordOrDigit::Digit(d) => d.fmt(formatter),
        }
    }
}


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

    for line in read_lines(input_file)? {
        let num = line?;
        let digits: Vec<u8> = num.chars()
            .filter(char::is_ascii_digit)
            .map(|ch| ch as u8)
            .collect();
        print_translations(&num, &digits, &mut Vec::new(), &dict);
    }
    Ok(())
}

fn print_translations<'a>(
    num: &str,
    digits: &[u8],
    words: &mut Vec<WordOrDigit<'a>>,
    dict: &'a Dictionary,
) {
    if digits.is_empty() {
        print_solution(num, &words);
        return;
    }
    let mut found_word = false;
    for i in 0..digits.len() {
        let (key, rest_of_digits) = digits.split_at(i + 1);
        if let Some(found_words) = dict.get(key) {
            for word in found_words {
                found_word = true;
                words.push(WordOrDigit::Word(word));
                print_translations(num, rest_of_digits, words, dict);
                words.pop();
            }
        }
    }
    if found_word {
        return;
    }
    let last_is_digit = match words.last() {
        Some(WordOrDigit::Digit(_)) => true,
        _ => false,
    };
    if !last_is_digit {
        let digit = digits[0] - b'0';
        words.push(WordOrDigit::Digit(digit));
        print_translations(num, &digits[1..], words, dict);
        words.pop();
    }
}

fn print_solution(num: &str, words: &[WordOrDigit<'_>]) {
    // do a little gymnastics here to avoid allocating a big string just for printing it
    print!("{}", num);
    if words.is_empty() {
        println!(":");
        return;
    }
    print!(": ");
    let (head, tail) = words.split_at(words.len() - 1);
    for word in head {
        print!("{} ", word);
    }
    for word in tail { // only last word in tail
        println!("{}", word);
    }
}

fn load_dict(words_file: String) -> io::Result<Dictionary> {
    let mut dict: Dictionary = HashMap::with_capacity(100);
    for line in read_lines(words_file)? {
        let word = line?;
        let key = word_to_number(&word);
        let words = dict.entry(key).or_default();
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

fn word_to_number(word: &str) -> Vec<u8> {
    word.chars()
        .filter(char::is_ascii_alphabetic)
        .map(char_to_digit)
        .map(|d| d + b'0')
        .collect()
}

fn nth_digit(digits: &[u8], i: usize) -> BigUint {
    let ch = digits.get(i).expect("index out of bounds");
    ((*ch as usize) - ('0' as usize)).to_biguint().unwrap()
}

fn char_to_digit(ch: char) -> u8 {
    match ch.to_ascii_lowercase() {
        'e' => 0,
        'j' | 'n' | 'q' => 1,
        'r' | 'w' | 'x' => 2,
        'd' | 's' | 'y' => 3,
        'f' | 't' => 4,
        'a' | 'm' => 5,
        'c' | 'i' | 'v' => 6,
        'b' | 'k' | 'u' => 7,
        'l' | 'o' | 'p' => 8,
        'g' | 'h' | 'z' => 9,
        _ => panic!("invalid input: not a digit: {}", ch)
    }
}
