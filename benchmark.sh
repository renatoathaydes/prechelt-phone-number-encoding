#!/bin/bash

set -e

# Author: Renato Athaydes
#
# Add each program to be benchmarked to the COMMANDS array below!
# If compilation is required, also add instructions to do that before "Generating INPUTS"
#

COMMANDS=(
  "java -Djava.util.logging.config.file=log-config/jul.properties -cp build/java Main"                               # Java (JUL)
  "java -Djava.util.logging.manager=org.apache.logging.log4j.jul.LogManager -cp build/java:log-config:log4j-libs/* Main"  # Java (Log4j2)
  "java -Djava.util.logging.config.file=log-config/jul-logback.properties -cp build/java:log-config:logback-libs/* Main"    # Java (Logback)
)

echo "Compiling Java sources"
rm -rf build || true
javac src/java/*.java -d build/java
javac src/java/util/*.java -d build/util

echo "Compiling Rust sources"
cd src/rust/phone_encoder && cargo build --release && cp target/release/phone_encoder ../../../
cd ../benchmark_runner && cargo build --release && cp target/release/benchmark_runner ../../../
cd ../../..

echo "Generating inputs"
INPUTS=(phones_1000.txt phones_10_000.txt phones_50_000.txt phones_100_000_with_empty.txt phones_200_000_with_empty.txt)
rm "${INPUTS[@]}" > /dev/null 2>&1 || true
java -cp "build/util" util.GeneratePhoneNumbers 1000 > phones_1000.txt
java -cp "build/util" util.GeneratePhoneNumbers 10000 > phones_10_000.txt
java -cp "build/util" util.GeneratePhoneNumbers 50000 > phones_50_000.txt
java -cp "build/util" util.GeneratePhoneNumbers 100000 "true" > phones_100_000_with_empty.txt
java -cp "build/util" util.GeneratePhoneNumbers 200000 "true" > phones_200_000_with_empty.txt

CHECK_FILE="proc_out.txt"
DEFAULT_INPUT="input.txt"
DEFAULT_OUTPUT="output.txt"
DICTIONARY="dictionary.txt"

echo "Checking all programs for correctness"
for CMD in "${COMMANDS[@]}"
do
  echo "Checking: $CMD"
  $CMD $DICTIONARY $DEFAULT_INPUT
  diff -q <(sort $CHECK_FILE) <(sort $DEFAULT_OUTPUT)
  echo "OK"
  rm $CHECK_FILE
done

echo "Benchmarking..."
for CMD in "${COMMANDS[@]}"
do
  echo "===> $CMD"
  # shellcheck disable=SC2086
  for file in "${INPUTS[@]}"; do ./benchmark_runner $CMD $DICTIONARY "$file"; done;
done

echo "Cleaning up"
rm "${INPUTS[@]}" "$CHECK_FILE" phone_encoder benchmark_runner
