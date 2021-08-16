use std::collections::HashMap;
use std::env::args;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

use crypto_bigint::U192;

type Dictionary = HashMap<U192, Vec<String>, ahash::RandomState>;
const TEN: U192 = U192::from_u8(10);

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
        if let Ok(num) = line {
            let digits: Vec<u8> = num
                .bytes()
                .filter_map(|ch| match ch {
                    b'0'..=b'9' => Some(ch - b'0'),
                    _ => None,
                })
                .collect();
            print_translations(&num, &digits, 0, Vec::new(), &dict)?;
        }
    }
    Ok(())
}

fn print_translations(
    num: &str,
    digits: &[u8],
    start: usize,
    words: Vec<&String>,
    dict: &Dictionary,
) -> io::Result<()> {
    if start >= digits.len() {
        print_solution(num, &words);
        return Ok(());
    }
    let mut n = U192::ONE;
    let mut found_word = false;
    for i in start..digits.len() {
        let t = U192::from_u8(digits[i]);
        n = n.wrapping_mul(&TEN).wrapping_add(&t);
        if let Some(found_words) = dict.get(&n) {
            for word in found_words {
                found_word = true;
                let mut partial_solution = words.clone();
                partial_solution.push(word);
                print_translations(num, digits, i + 1, partial_solution, dict)?;
            }
        }
    }
    if !found_word && !words.last().map(|w| is_digit(w)).unwrap_or(false) {
        let mut partial_solution = words.clone();
        let digit = digits[start].to_string();
        partial_solution.push(&digit);
        print_translations(num, digits, start + 1, partial_solution, dict)
    } else {
        Ok(())
    }
}

fn print_solution(num: &str, words: &Vec<&String>) {
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
    let mut dict = HashMap::with_capacity_and_hasher(
        100,
        ahash::RandomState::default(),
    );
    let words = read_lines(words_file)?;
    for line in words {
        if let Ok(word) = line {
            let key = word_to_number(&word);
            let words = dict.entry(key).or_insert_with(|| Vec::new());
            words.push(word);
        }
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

fn word_to_number(word: &str) -> U192 {
    word
        .bytes()
        .filter_map(char_to_digit)
        .fold(U192::ONE, |n, d| {
            let d = U192::from_u8(d);
            n.wrapping_mul(&TEN).wrapping_add(&d)
        })
}

fn is_digit(string: &str) -> bool {
    string.len() == 1 && string.chars().next().unwrap().is_digit(10)
}

fn char_to_digit(mut ch: u8) -> Option<u8> {
    if ch >= b'a' {
        ch = ch - b'a' + b'A';
    }
    let digit = match ch {
        b'E' => 0,
        b'J' | b'N' | b'Q' => 1,
        b'R' | b'W' | b'X' => 2,
        b'D' | b'S' | b'Y' => 3,
        b'F' | b'T' => 4,
        b'A' | b'M' => 5,
        b'C' | b'I' | b'V' => 6,
        b'B' | b'K' | b'U' => 7,
        b'L' | b'O' | b'P' => 8,
        b'G' | b'H' | b'Z' => 9,
        _ => return None,
    };
    Some(digit)
}
