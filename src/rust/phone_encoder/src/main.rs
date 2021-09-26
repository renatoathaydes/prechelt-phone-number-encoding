use std::collections::HashMap;
use std::env::args;
use std::fmt::{self, Display, Formatter};
use std::fs::File;
use std::io::{self, BufRead, BufWriter, StdoutLock, Write};
use std::path::Path;

type Dictionary = HashMap<Vec<u8>, Vec<String>, ahash::RandomState>;

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

#[derive(PartialEq, Eq)]
enum ResultsOption {
    Print,
    Count,
}

/// Port of Peter Norvig's Lisp solution to the Prechelt phone-encoding problem.
///
/// Even though this is intended as a port, it deviates quite a bit from it
/// due to the very different natures of Lisp and Rust.
fn main() -> io::Result<()> {
    // drop itself from args
    let mut args = args().skip(1);
    let res_opt = args.next().unwrap_or_else(|| "".to_string());
    let results_opt = match res_opt.as_ref() {
        "print" => ResultsOption::Print,
        "count" => ResultsOption::Count,
        _ => panic!("Bad first argument (expected 'print' or 'count')"),
    };
    let words_file = args.next().expect("Missing words file");
    let input_files: Vec<_> = args.collect();

    let mut solution_count = 0;
    let dict = load_dict(words_file)?;

    let stdout = io::stdout();
    let mut writer = BufWriter::new(stdout.lock());

    for input_file in input_files {
        for line in read_lines(input_file)? {
            let num = line?;
            let digits: Vec<u8> = num
                .chars()
                .filter(char::is_ascii_digit)
                .map(|ch| ch as u8)
                .collect();
            find_translations(
                &mut solution_count,
                results_opt == ResultsOption::Print,
                &num,
                &digits,
                &mut Vec::new(),
                &dict,
                &mut writer,
            );
        }
        if results_opt == ResultsOption::Count {
            println!("{}", solution_count);
            solution_count = 0;
        }
    }
    Ok(())
}

fn find_translations<'a>(
    solution_count: &mut usize,
    do_print: bool,
    num: &str,
    digits: &[u8],
    words: &mut Vec<WordOrDigit<'a>>,
    dict: &'a Dictionary,
    writer: &mut BufWriter<StdoutLock>,
) {
    if digits.is_empty() {
        if do_print {
            print_solution(num, words, writer);
        } else {
            *solution_count += 1;
        }
        return;
    }
    let mut found_word = false;
    for i in 0..digits.len() {
        let (key, rest_of_digits) = digits.split_at(i + 1);
        if let Some(found_words) = dict.get(key) {
            for word in found_words {
                found_word = true;
                words.push(WordOrDigit::Word(word));
                find_translations(
                    solution_count,
                    do_print,
                    num,
                    rest_of_digits,
                    words,
                    dict,
                    writer,
                );
                words.pop();
            }
        }
    }
    if found_word {
        return;
    }
    if !matches!(words.last(), Some(WordOrDigit::Digit(..))) {
        let digit = digits[0] - b'0';
        words.push(WordOrDigit::Digit(digit));
        find_translations(
            solution_count,
            do_print,
            num,
            &digits[1..],
            words,
            dict,
            writer,
        );
        words.pop();
    }
}

fn print_solution(num: &str, words: &[WordOrDigit<'_>], writer: &mut BufWriter<StdoutLock>) {
    write!(writer, "{}:", num).unwrap();
    for word in words {
        write!(writer, " {}", word).unwrap();
    }
    writeln!(writer).unwrap();
}

fn load_dict(words_file: String) -> io::Result<Dictionary> {
    let mut dict: Dictionary =
        HashMap::with_capacity_and_hasher(100, ahash::RandomState::default());

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
where
    P: AsRef<Path>,
{
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
        _ => panic!("invalid input: not a digit: {}", ch),
    }
}
