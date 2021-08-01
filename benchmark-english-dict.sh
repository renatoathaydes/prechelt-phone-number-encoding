#!/bin/bash

set -e

# Author: Renato Athaydes
#
# Add each program to be benchmarked to the COMMANDS array below!
# If compilation is required, also add instructions to do that before "Generating INPUTS"
#

if [ ! -f words.txt ]; then
  echo "Downloading English Words Dictionary."
  ./download_english_words.sh
fi

COMMANDS=(
  "java -cp build/java Main"          # Java 1
  "java -cp build/java Main2"         # Java 2
#  "sbcl --script src/lisp/main.lisp"  # Common Lisp
  "./phone_encoder"                   # Rust
#  "src/dart/phone-encoder/bin/phone_encoder.exe" # Dart
#  "julia src/julia/phone_encoder.jl"  # Julia
)

echo "Compiling Java sources"
rm -rf build || true
javac src/java/*.java -d build/java
javac src/java/util/*.java -d build/util

echo "Compiling Rust sources"
cd src/rust/phone_encoder && cargo build --release && cp target/release/phone_encoder ../../../
cd ../benchmark_runner && cargo build --release && cp target/release/benchmark_runner ../../../
cd ../../..

#echo "Compiling Dart sources"
#cd src/dart/phone-encoder && dart compile exe bin/phone_encoder.dart
#cd ../../..

echo "Generating inputs"

# check how input length affects runtime
INPUTS2=(phones_1_len_5.txt phones_1_len_10.txt phones_1_len_15.txt phones_1_len_20.txt phones_1_len_25.txt phones_1_len_27.txt phones_1_len_28.txt phones_1_len_29.txt)
echo  "91760" > phones_1_len_5.txt
echo  "9176068765" > phones_1_len_10.txt
echo  "917606876516188" > phones_1_len_15.txt
echo  "91760687651618841752" > phones_1_len_20.txt
echo  "9176068765161884175203318" > phones_1_len_25.txt
echo  "917606876516188417520331816" > phones_1_len_27.txt
echo  "9176068765161884175203318165" > phones_1_len_28.txt
echo  "91760687651618841752033181652" > phones_1_len_29.txt

CHECK_FILE="proc_out.txt"
DEFAULT_INPUT="input.txt"
DEFAULT_OUTPUT="output.txt"
DICTIONARY="words.txt"

echo "Checking all programs for correctness"
for CMD in "${COMMANDS[@]}"
do
  echo "Checking: $CMD"
  $CMD "dictionary.txt" $DEFAULT_INPUT > $CHECK_FILE
  diff -q <(sort $CHECK_FILE) <(sort $DEFAULT_OUTPUT)
  echo "OK"
done

echo "Benchmarking..."
for CMD in "${COMMANDS[@]}"
do
  echo "2 ===> $CMD"
  # shellcheck disable=SC2086
  for file in "${INPUTS2[@]}"; do ./benchmark_runner $CMD $DICTIONARY "$file"; done;
done

echo "Cleaning up"
rm "${INPUTS2[@]}" "$CHECK_FILE" phone_encoder benchmark_runner
