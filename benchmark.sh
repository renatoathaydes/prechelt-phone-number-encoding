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
  "sbcl --script src/lisp/main.lisp"  # Common Lisp
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
INPUTS=(phones_2000.txt phones_1000.txt phones_10_000.txt phones_50_000.txt phones_100_000_with_empty.txt)
rm "${INPUTS[@]}" > /dev/null 2>&1 || true
java -cp "build/util" util.GeneratePhoneNumbers 2000 false 12 > phones_2000.txt
java -cp "build/util" util.GeneratePhoneNumbers 1000 > phones_1000.txt
java -cp "build/util" util.GeneratePhoneNumbers 10000 > phones_10_000.txt
java -cp "build/util" util.GeneratePhoneNumbers 50000 > phones_50_000.txt
java -cp "build/util" util.GeneratePhoneNumbers 100000 "true" > phones_100_000_with_empty.txt

# a large phone number (length 23) for use with the large dictionary
echo  "91760687651618841752033" > phones_1.txt

CHECK_FILE="proc_out.txt"
DEFAULT_INPUT="input.txt"
DEFAULT_OUTPUT="output.txt"
DICTIONARY="dictionary.txt"

echo "Checking all programs for correctness"
for CMD in "${COMMANDS[@]}"
do
  echo "Checking: $CMD"
  $CMD $DICTIONARY $DEFAULT_INPUT > $CHECK_FILE
  diff -q <(sort $CHECK_FILE) <(sort $DEFAULT_OUTPUT)
  echo "OK"
done

echo "Benchmarking..."
for CMD in "${COMMANDS[@]}"
do
  echo "===> $CMD"
  # shellcheck disable=SC2086
#  for file in "${INPUTS[@]}"; do ./benchmark_runner $CMD $DICTIONARY "$file"; done;

  # final run with very large dictionary
  # shellcheck disable=SC2086
#  ./benchmark_runner $CMD words.txt phones_2000.txt

  # another run with very large dictionary and one single large phone number
  ./benchmark_runner $CMD words.txt phones_1.txt
done

echo "Cleaning up"
rm "${INPUTS[@]}" "$CHECK_FILE" phone_encoder benchmark_runner phones_1.txt
