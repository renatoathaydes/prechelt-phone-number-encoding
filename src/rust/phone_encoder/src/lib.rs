use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead, Write};
use std::path::Path;

use rug::Integer;

pub type Dictionary = HashMap<Integer, Vec<String>>;

static DIGITS: [&str; 10] = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"];

pub fn print_translations<'dict>(
    num: &str,
    digits: &[char],
    start: usize,
    words: &mut Vec<&'dict str>,
    dict: &'dict Dictionary,
    writer: &mut impl Write,
) -> io::Result<()> {
    if start >= digits.len() {
        print_solution(num, words, writer);
        return Ok(());
    }
    let mut n = Integer::from(1u8);
    let mut found_word = false;
    for i in start..digits.len() {
        n *= 10u8;
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
        let digit = DIGITS[nth_digit(digits, start) as usize];
        words.push(digit);
        let res = print_translations(num, digits, start + 1, words, dict, writer);
        words.pop();
        res
    } else {
        Ok(())
    }
}

pub fn print_solution(num: &str, words: &[&str], writer: &mut impl Write) {
    writeln!(writer, "{}: {}", num, words.join(" ")).expect("Cannot print solution");
}

pub fn load_dict(words_file: String) -> io::Result<Dictionary> {
    let mut dict = HashMap::with_capacity(100);
    let words = read_lines(words_file)?;
    for word in words.flatten() {
        let key = word_to_number(&word);
        let words = dict.entry(key).or_insert_with(Vec::new);
        words.push(word);
    }
    Ok(dict)
}

// The output is wrapped in a Result to allow matching on errors
// Returns an Iterator to the Reader of the lines of the file.
pub fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
    where P: AsRef<Path>, {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}

fn word_to_number(word: &str) -> Integer {
    let mut n = Integer::from(1u8);
    for ch in word.chars() {
        if ch.is_alphabetic() {
            n *= 10u8;
            n += char_to_digit(ch);
        }
    }
    n
}

fn nth_digit(digits: &[char], i: usize) -> u8 {
    digits[i] as u8 - b'0'
}

fn is_digit(string: &str) -> bool {
    string.len() == 1 && string.chars().next().unwrap().is_digit(10)
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