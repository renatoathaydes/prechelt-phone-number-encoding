#!/bin/bash

set -e

# Author: Renato Athaydes
#
# Add each program to be benchmarked to the COMMANDS array below!
# If compilation is required, also add instructions to do that before "Generating INPUTS"
#

COMMANDS=(
  "java -Xms30m -Xmx60m -cp build/java Main2" # Java
  "sbcl --script src/lisp/main.fasl"  # Common Lisp
  "./rust"                            # Rust
)

echo "Compiling Java sources"
rm -rf build || true
javac src/java/*.java -d build/java
javac src/java/util/*.java -d build/util

echo "Compiling Rust sources"
cd src/rust/phone_encoder && cargo build --release && cp target/release/phone_encoder ../../../rust
cd ../benchmark_runner && cargo build --release && cp target/release/benchmark_runner ../../../
cd ../plotter && cargo build --release && cp target/release/plotter ../../../
cd ../../..

echo "Compiling Lisp sources"
cd src/lisp/
sbcl --noinform --eval "(compile-file \"main.lisp\")" --eval "(quit)"
cd ../..

echo "Generating inputs"

INPUTS=(phones_1_000.txt
        phones_200_000.txt
        phones_400_000.txt
        phones_600_000.txt)

# INPUTS to run with larger dictionary (must use 'count' arg due to too many solutions)
# phone length limited to 16 instead of 25
BIG_INPUTS=(phones_1000_16.txt
            phones_2000_16.txt)

# Using Standard dictionary
java -cp "build/util" util.GeneratePhoneNumbers 1000   25 > "${INPUTS[0]}"
java -cp "build/util" util.GeneratePhoneNumbers 200000 25 > "${INPUTS[1]}"
java -cp "build/util" util.GeneratePhoneNumbers 400000 25 > "${INPUTS[2]}"
java -cp "build/util" util.GeneratePhoneNumbers 600000 25 > "${INPUTS[3]}"

# Using larger dictionary, takes much longer to run so use smaller input sizes
java -cp "build/util" util.GeneratePhoneNumbers 1000 16 > "${BIG_INPUTS[0]}"
java -cp "build/util" util.GeneratePhoneNumbers 2000 16 > "${BIG_INPUTS[1]}"

CHECK_FILE="proc_out.txt"
DEFAULT_INPUT="input.txt"
DEFAULT_OUTPUT="output.txt"
COUNT_OUTPUT="count_output.txt"
DICTIONARY="dictionary.txt"
BIG_DICTIONARY="words.txt"

echo "Checking all programs for correctness"
for CMD in "${COMMANDS[@]}"
do
    echo "Checking: $CMD"
    # test print
    $CMD print $DICTIONARY $DEFAULT_INPUT > $CHECK_FILE
    diff -q <(sort $CHECK_FILE) <(sort $DEFAULT_OUTPUT)
    # test count
    $CMD count $DICTIONARY $DEFAULT_INPUT $DEFAULT_INPUT > $CHECK_FILE
    diff <(sort $CHECK_FILE) <(sort $COUNT_OUTPUT)
    echo "OK"
done

CSV_OUT="data.csv"

run_bench() {
    DATA=$(./benchmark_runner $*)
    echo "$DATA"
    echo "$DATA" >> "$CSV_OUT"
}

echo "Benchmarking..."
echo "Proc,Run,Memory(bytes),Time(ms)"
echo "Proc,Run,Memory(bytes),Time(ms)" > "$CSV_OUT"
for CMD in "${COMMANDS[@]}"
do
  echo "===> $CMD"
  # shellcheck disable=SC2086
  for (( i = 0; i < ${#INPUTS[*]}; ++i )); do
      run_bench "$i $CMD print $DICTIONARY ${INPUTS[$i]}"
  done
  ## run with all input files
  run_bench "${#INPUTS[@]} $CMD count $DICTIONARY ${INPUTS[@]}"
  ## run with larger dictionary
  for (( i = 0; i < ${#BIG_INPUTS[*]}; ++i )); do
      run_bench "$((1 + i + ${#INPUTS[*]})) $CMD count $BIG_DICTIONARY ${BIG_INPUTS[$i]}"
  done
  ## run with all input files
  run_bench "$((1 + ${#INPUTS[@]} + ${#BIG_INPUTS[@]})) $CMD count $BIG_DICTIONARY ${BIG_INPUTS[@]}"
done

echo "Generating plot"
./plotter "$CSV_OUT"

echo "Cleaning up"
rm "${INPUTS[@]}" "${BIG_INPUTS[@]}" "$CHECK_FILE" ./rust ./benchmark_runner ./plotter
