use std::env::args;
use std::io::{self, BufWriter};

mod lib;

/// Port of Peter Norvig's Lisp solution to the Prechelt phone-encoding problem.
///
/// Even though this is intended as a port, it deviates quite a bit from it
/// due to the very different natures of Lisp and Rust.
fn main() -> io::Result<()> {
    // drop itself from args
    let mut args = args().skip(1);
    let words_file = args.next().unwrap_or_else(|| "tests/words.txt".into());
    let input_file = args.next().unwrap_or_else(|| "tests/numbers.txt".into());

    let dict = lib::load_dict(words_file)?;

    let stdout = io::stdout();
    let mut out = BufWriter::new(stdout.lock());

    for num in lib::read_lines(input_file)?.flatten() {
        let digits: Vec<_> = num.chars()
            .filter(|ch| ch.is_alphanumeric())
            .collect();
        lib::print_translations(&num, &digits, 0, Vec::new(), &dict, &mut out)?;
    }
    Ok(())
}
