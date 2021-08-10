use std::collections::HashMap;
use std::env::args;
use std::fs::File;
use std::io::{self, BufRead, BufWriter, Write};
use std::path::Path;
use std::hash::{Hash, Hasher};

type Dictionary = HashMap<DigitBytes, Vec<String>>;

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
    let mut key = DigitBytes::new();
    let mut found_word = false;
    for i in start..digits.len() {
        key.push(nth_digit(digits, i));
        if let Some(found_words) = dict.get(&key) {
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

fn word_to_number(word: &str) -> DigitBytes {
    let mut key = DigitBytes::new();
    for ch in word.bytes().filter_map(char_to_digit) {
        key.push(ch);
    }
    key
}

fn nth_digit(digits: &[u8], i: usize) -> u8 {
    let ch = digits.get(i).expect("index out of bounds");
    *ch - b'0'
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

#[derive(Debug, Eq, Clone)]
struct DigitBytes {
    even: bool,
    full: bool,
    write_index: usize,
    bytes: [u8; 25],
}

impl DigitBytes {
    fn new() -> DigitBytes {
        DigitBytes { even: true, write_index: 0, bytes: [0; 25], full: false }
    }

    fn push(&mut self, b: u8) {
        if self.full {
            panic!("too many digits pushed")
        }
        if self.even {
            self.bytes[self.write_index] = b;
        } else {
            self.bytes[self.write_index] += b << 4;
            if self.write_index + 1 == self.bytes.len() {
                // can't increase index anymore otherwise will break Eq and Hash checks
                self.full = true;
            } else {
                self.write_index += 1;
            }
        }
        self.even = !self.even;
    }
}

impl PartialEq<Self> for DigitBytes {
    fn eq(&self, other: &Self) -> bool {
        self.full == other.full &&
        self.even == other.even &&
            self.bytes[0..=self.write_index] == other.bytes[0..=other.write_index]
    }
}

impl Hash for DigitBytes {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.bytes[0..=self.write_index].hash(state);
        self.even.hash(state);
        self.full.hash(state);
    }
}

#[cfg(test)]
mod digit_bytes_test {
    use crate::DigitBytes;
    use std::collections::HashMap;

    #[test]
    fn can_write() {
        let mut expected_bytes = [0; 25];
        let mut bytes = DigitBytes::new();
        assert_eq!(bytes.bytes, expected_bytes);
        bytes.push(9);
        bytes.push(2);
        expected_bytes[0] = 41;
        assert_eq!(bytes.bytes, expected_bytes);
        bytes.push(4);
        expected_bytes[1] = 4;
        assert_eq!(bytes.bytes, expected_bytes);
    }

    #[test]
    fn can_compare() {
        let mut a = DigitBytes::new();
        let mut b = DigitBytes::new();
        assert_eq!(a, b);
        assert_eq!(b, a);
        a.push(3);
        assert_ne!(a, b);
        assert_ne!(b, a);
        b.push(3);
        assert_eq!(a, b);
        assert_eq!(b, a);
        a.push(4);
        b.push(5);
        assert_ne!(a, b);
        assert_ne!(b, a);
        let mut a = DigitBytes::new();
        let mut b = DigitBytes::new();
        a.push(4);
        b.push(5);
        assert_ne!(a, b);
        assert_ne!(b, a);
    }

    #[test]
    fn can_see_difference_between_zero_and_e() {
        let mut a = DigitBytes::new();
        let mut b = DigitBytes::new();
        a.push(0);
        assert_ne!(a, b);
        assert_ne!(b, a);

        a.push(0);
        b.push(0);
        assert_ne!(a, b);
        assert_ne!(b, a);
    }

    #[test]
    fn can_use_in_hash_map() {
        let mut map = HashMap::<DigitBytes, u32>::new();
        let a = {
            let a = DigitBytes::new();
            map.insert(a.clone(), 1);
            a
        };
        assert!(map.contains_key(&a));
        assert_eq!(map[&a], 1);
        let b = {
            let mut b = a.clone();
            b.push(4);
            b
        };
        assert!(!map.contains_key(&b));

        let c = {
            let mut c = b.clone();
            for i in 1..10 { c.push(i); }
            c
        };

        map.insert(b.clone(), 2);
        assert!(map.contains_key(&b));
        assert!(!map.contains_key(&c));

        map.insert(c.clone(), 3);

        assert_eq!(map[&a], 1);
        assert_eq!(map[&b], 2);
        assert_eq!(map[&c], 3);
    }

    #[test]
    fn can_push_max_len() {
        let mut a = DigitBytes::new();
        for _ in 0..50 {
            a.push(0);
        }
        assert!(a.full);

        // make sure an instance filled with zero is not equal to an empty one
        let b = DigitBytes::new();
        assert_ne!(a, b);
        assert_ne!(b, a);
    }
}