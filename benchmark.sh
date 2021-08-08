#!/bin/bash

set -e

# Author: Renato Athaydes
#
# Add each program to be benchmarked to the COMMANDS array below!
# If compilation is required, also add instructions to do that before "Generating INPUTS"
#

OPENJDK_JAVA=~/.sdkman/candidates/java/16.0.1-open/bin/java
GRAALVM_JAVA=~/.sdkman/candidates/java/21.2.0.r16-grl/bin/java

if [ ! -f words.txt ]; then
  echo "Downloading English Words Dictionary."
  ./download_english_words.sh
fi

COMMANDS=(
  "$OPENJDK_JAVA -cp build/java1 Main"          # Java 1
  "$OPENJDK_JAVA -cp build/java2 Main2"         # Java 2
  "$GRAALVM_JAVA -cp build/java1 Main"          # Java 1
  "$GRAALVM_JAVA -cp build/java2 Main2"         # Java 2
  ./graalvm-main
  ./graalvm-main2
#  "sbcl --script src/lisp/main.lisp"  # Common Lisp
#  "./phone_encoder_std_hash"           # Rust
#  "./phone_encoder"                    # Rust (ahash)
#  "src/dart/phone-encoder/bin/phone_encoder.exe" # Dart
#  "julia src/julia/phone_encoder.jl"  # Julia
)

echo "Compiling Java sources"
rm -rf build || true
javac src/java/Main.java -d build/java1
javac src/java/Main2.java -d build/java2
javac src/java/util/*.java -d build/util

echo "Compiling Java native images"
native-image -cp build/java1 Main graalvm-main
native-image -cp build/java2 Main2 graalvm-main2

echo "Compiling Rust sources"
cd src/rust/phone_encoder && cargo build --release && cp target/release/phone_encoder ../../../
cd ../benchmark_runner && cargo build --release && cp target/release/benchmark_runner ../../../
cd ../../..

#echo "Compiling Dart sources"
#cd src/dart/phone-encoder && dart compile exe bin/phone_encoder.dart
#cd ../../..

echo "Generating inputs"
INPUTS=(phones_1000.txt phones_2000.txt phones_3000.txt phones_4000.txt  phones_5000.txt)
rm "${INPUTS[@]}" > /dev/null 2>&1 || true
java -cp "build/util" util.GeneratePhoneNumbers 1000 false 16 > phones_1000.txt
java -cp "build/util" util.GeneratePhoneNumbers 2000 false 16 > phones_2000.txt
java -cp "build/util" util.GeneratePhoneNumbers 3000 false 16 > phones_3000.txt
java -cp "build/util" util.GeneratePhoneNumbers 4000 false 16 > phones_4000.txt
java -cp "build/util" util.GeneratePhoneNumbers 5000 false 16 > phones_5000.txt

# a large phone number (length 23) for use with the large dictionary
#echo  "91760687651618841752033" > phones_1.txt

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

DICTIONARY="words.txt"

echo "Benchmarking..."
for CMD in "${COMMANDS[@]}"
do
  echo "===> $CMD"
  # shellcheck disable=SC2086
  for file in "${INPUTS[@]}"; do ./benchmark_runner "$file" $CMD $DICTIONARY "$file"; done;

  # final run with very large dictionary
  # shellcheck disable=SC2086
#  ./benchmark_runner phones_2000.txt $CMD words.txt phones_2000.txt

  # another run with very large dictionary and one single large phone number
#  ./benchmark_runner phones_1.txt $CMD words.txt phones_1.txt
done

echo "Cleaning up"
rm "${INPUTS[@]}" "$CHECK_FILE" phone_encoder benchmark_runner phones_1.txt
