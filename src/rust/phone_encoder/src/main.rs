use fnv::FnvHasher;
use std::collections::HashMap;
use std::env::args;
use std::fs::{self, File};
use std::hash::BuildHasherDefault;
use std::io::{self, BufRead, StdoutLock, Write};
use std::path::Path;

type HashMapFnv<K, V> = HashMap<K, V, BuildHasherDefault<FnvHasher>>;
use ibig::{ubig, UBig};

type Dictionary<'a> = HashMapFnv<UBig, Vec<&'a str>>;

/// Port of Peter Norvig's Lisp solution to the Prechelt phone-encoding problem.
///
/// Even though this is intended as a port, it deviates quite a bit from it
/// due to the very different natures of Lisp and Rust.
fn main() -> io::Result<()> {
    let _stdout = std::io::stdout();
    // drop itself from args
    let mut a = args();
    let (_, words_file, input_file) = (a.next(), a.next(), a.next());
    let words_file: String = words_file.unwrap_or_else(|| "tests/words.txt".to_owned());
    let input_file: String = input_file.unwrap_or_else(|| "tests/numbers.txt".to_owned());

    let dict_content = fs::read_to_string(words_file)?;
    let dict = load_dict(&dict_content);
    read_lines(input_file)?
        .filter_map(|l| l.ok())
        .for_each(|num| {
            let digits: Vec<_> = num.chars().filter(|ch| ch.is_alphanumeric()).collect();
            print_translations(&num, &digits, 0, Vec::new(), &dict, &mut _stdout.lock()).unwrap();
        });
    Ok(())
}

fn print_translations(
    num: &str,
    digits: &[char],
    start: usize,
    words: Vec<&str>,
    dict: &Dictionary,
    stdout_lock: &mut StdoutLock,
) -> io::Result<()> {
    if start >= digits.len() {
        print_solution(num, &words, stdout_lock);
        return Ok(());
    }
    let mut n = ubig!(1);
    let mut found_word = false;
    for i in start..digits.len() {
        n = &n * (ubig!(10)) + &nth_digit(digits, i);
        if let Some(found_words) = dict.get(&n) {
            for word in found_words {
                found_word = true;
                let mut partial_solution = words.clone();
                partial_solution.push(&word);
                print_translations(num, digits, i + 1, partial_solution, dict, stdout_lock)?;
            }
        }
    }
    if !found_word && !words.last().map(|w| is_digit(w)).unwrap_or(false) {
        let mut partial_solution = words.clone();
        let digit = nth_digit_string(digits, start);
        partial_solution.push(&digit);
        print_translations(num, digits, start + 1, partial_solution, dict, stdout_lock)
    } else {
        Ok(())
    }
}

fn print_solution(num: &str, words: &[&str], stdout_lock: &mut StdoutLock) {
    // do a little gymnastics here to avoid allocating a big string just for printing it
    // use write_all() where no formatting is required
    let _ = stdout_lock.write_all(num.as_bytes());
    if words.is_empty() {
        let _ = stdout_lock.write_all(b":\n");
        return;
    }
    let _ = stdout_lock.write_all(b": ");
    let (head, tail) = words.split_at(words.len() - 1);
    for word in head {
        let _ = write!(stdout_lock, "{} ", word);
    }
    for word in tail {
        // only last word in tail
        let _ = writeln!(stdout_lock, "{}", word);
    }
}

fn load_dict(content: &str) -> Dictionary {
    let mut dict = HashMapFnv::with_capacity_and_hasher(100, Default::default());
    content.lines().for_each(|word| {
        let key = word_to_number(&word);
        let words = dict.entry(key).or_insert_with(|| Vec::new());
        words.push(word);
    });
    dict
}

// The output is wrapped in a Result to allow matching on errors
// Returns an Iterator to the Reader of the lines of the file.
fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}

fn word_to_number(word: &str) -> UBig {
    let mut n = ubig!(1);
    for ch in word.chars() {
        if ch.is_alphabetic() {
            n = &n * (ubig!(10)) + char_to_digit(ch);
        }
    }
    n
}

fn nth_digit(digits: &[char], i: usize) -> UBig {
    let ch = digits.get(i).expect("index out of bounds");
    ((*ch as usize) - ('0' as usize)).into()
}

// no need for extra conversion
fn nth_digit_string(digits: &[char], i: usize) -> String {
    let ch = digits.get(i).expect("index out of bounds");
    ch.to_string()
}

fn is_digit(string: &str) -> bool {
    string.chars().next().map(|c| c.is_digit(10)) == Some(true)
}

fn char_to_digit(ch: char) -> UBig {
    match ch.to_ascii_lowercase() {
        'e' => ubig!(0),
        'j' | 'n' | 'q' => ubig!(1),
        'r' | 'w' | 'x' => ubig!(2),
        'd' | 's' | 'y' => ubig!(3),
        'f' | 't' => ubig!(4),
        'a' | 'm' => ubig!(5),
        'c' | 'i' | 'v' => ubig!(6),
        'b' | 'k' | 'u' => ubig!(7),
        'l' | 'o' | 'p' => ubig!(8),
        'g' | 'h' | 'z' => ubig!(9),
        _ => panic!("invalid input: not a digit: {}", ch),
    }
}
