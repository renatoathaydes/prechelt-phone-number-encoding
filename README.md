## Prechelt Phone Number Encoding

This project implements the phone number encoding described by [Lutz Prechelt](https://twitter.com/prechelt)
in his article for the COMMUNICATIONS OF THE ACM (October 1999/Vol. 42, No. 10), [Comparing Java vs. C/C++ Efficiency Differences to Interpersonal Differences](https://www.ebhakt.info/dl/Comparejavaandc_D9F7/compare_java_c.pdf),
later re-used for a more extended paper, [An empirical comparisonofC, C++, Java,Perl, Python, Rexx, and Tclfor asearch/string-processing program](http://page.mi.fu-berlin.de/prechelt/Biblio/jccpprtTR.pdf)
from March, 2000.

The same problem was later used by [Ron Garret](https://flownet.com/ron/) (aka Erann Gat) on
[Lisp as an alternative to Java](https://flownet.com/ron/papers/lisp-java/lisp-java.pdf), also from the year 2000.

The instructions given to participants of the latter study (which was slightly adapted from the original paper,
and used to implement my own solution in this repository) can be found at
[flownet.com](https://flownet.com/ron/papers/lisp-java/instructions.html).

[Peter Norvig](https://norvig.com/), after finding out about the Lisp paper, wrote his own
[Lisp program](http://www.norvig.com/java-lisp.html) to solve the problem.

His solution is included in this repository at [src/lisp/main.lisp](src/lisp/main.lisp) as a baseline for performance
measurements (see [benchmark.sh](benchmark.sh)).

## Structure of this repository

The solution I first came up with is in [src/java/Main.java](src/java/Main.java).

Later, I ported Norvig's Lisp solution to Java ([src/java/Main2.java](src/java/Main2.java))
and Rust ([src/rust/phone_encoder/src/main.rs](src/rust/phone_encoder/src/main.rs)).

In order to be able to benchmark the programs, I created a few utilities in Java and Rust:

* [src/java/util/GeneratePhoneNumbers.java](src/java/util/GeneratePhoneNumbers.java) generates input files with random phone numbers.
* [src/java/util/LineCount.java](src/java/util/LineCount.java) counts lines of code (LOC) on given files.
* [src/java/util/OutputChecker.java](src/java/util/OutputChecker.java) can check the output of a program for correctness.
* [src/rust/benchmark_runner](src/rust/benchmark_runner) is a Rust project that compiles a binary that can run programs and benchmark them.

To run the benchmarks, a shell script, [benchmark.sh](benchmark.sh), is used.

## Compiling and running

### Requirements

The benchmarks currently require the following tools:

* Java 16+.
* Rust and Cargo (tested with `rustc 1.53.0 (53cb7b09b 2021-06-17)`).
* [SBCL](https://lisp-lang.org/learn/getting-started/), Steel Bank Common Lisp compiler.

### Java

Run this command to compile the Java solutions:

```
$ javac src/java/*.java -d build
```

To compile the utilities, run:

```
$ javac src/java/util/*.java -d build
```

### Rust

```
$ cd src/rust/phone_encoder
$ cargo build --release
```

### Lisp

No compilation is required.

## Running

### Java

```
$ java -cp build Main dictionary.txt input.txt
```

> For all programs, you can use the smaller dictionary at [tests/words.txt](tests/words.txt)
> and input file [tests/numbers.txt](tests/numbers.txt) to print just a few results.

### Rust

```
$ cp src/rust/phone_encoder/target/release/phone_encoder .
$ ./phone_encoder dictionary.txt input.txt
```

### Lisp

```
$ sbcl --script src/lisp/main.lisp dictionary.txt input.txt
```

## Testing

To verify that your program works correctly execute it with arguments `dictionary.txt input.txt`, piping its output
to a file, say `prog_out.txt`, then run:

```
$ diff -q <(sort prog_out.txt) <(sort output.txt)
```

If `diff` exits with `0` and does not print anything, your program is good.

The benchmarks generate input files of different sizes by running my Java utility. If you want to try your solution
with larger inputs, try this, for example, to generate 50,000 random phone numbers:

```
$ java -cp build/util util.GeneratePhoneNumbers 50000 > phones_50_000.txt
```

To partially check your solution works with the larger inputs
(the checker cannot guarantee that the digit-substitution algorithm is perfectly correct),
run my Java utility as follows:

```
$ java -cp build/util util.OutputChecker dictionary.txt program_output.txt
```

## Benchmarks

I've published the benchmarks results on a [Google Docs Spreadsheet](https://docs.google.com/spreadsheets/d/14MFvpFaJ49XIA8K1coFLvsnIkpEQBbkOZbtTYujvatA/edit?usp=sharing).

To run it yourself, checkout this repository and run:

> See the requirements above first.

```
./benchmark.sh
```

## Adding a new program to the benchmarks

> I don't currently intend to accept pull requests to this repository because I used the programs in this repository on
> an article I wrote on my website and I don't want it to become stale.
>
> However, feel free to fork it and modify it as you see fit!

1. Add your program to the appropriate folder under `src/lang/` (e.g. `src/lisp/main.lisp`).
2. If it requires a new compiler, update README.md's `Requirements` section.
3. Edit [benchmark.sh](benchmark.sh) as follows:
    * if it requires compilation, add the compilation command after the `echo "Compiling Rust sources"` block.
    * add the command to run your program to the `COMMANDS` array.
   

## Solutions in other languages

I've found the following other solutions to Prechetl's phone number encoding so far:

* [Lua, by Andrew Yourtchenko](https://gist.github.com/ayourtch/752460)
* [Python, by Josiah Carlson](https://gist.github.com/josiahcarlson/752208)
* [Scala, by anatoli](https://github.com/tolks/scala-test-drive/blob/master/Matcher.scala)
* [Common Lisp, by Peter Norvig](http://www.norvig.com/java-lisp.html)
* [Haskell, several solutions](https://wiki.haskell.org/Phone_number)
