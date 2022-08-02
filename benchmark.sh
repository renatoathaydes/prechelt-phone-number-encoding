#!/bin/bash

set -e

# Author: Renato Athaydes
#
# Add each program to be benchmarked to the COMMANDS array below!
# If compilation is required, also add instructions to do that before "Generating INPUTS"
#

JVM_OPTIONS="-Xms20M -Xmx100M"

COMMANDS=(
  "java $JVM_OPTIONS -cp build/java Main"         # Java (Renato Athaydes)
  "java $JVM_OPTIONS -cp build/java MainFaster"   # Java (@davidaf3)
  "./lisp-phone-encoder"                          # Common Lisp (Renato Athaydes)
  "./rust"                                        # Rust (Renato Athaydes)
  "./phone_encoder_c"                             # C (@davidaf3)
  "./phone_encoder_go"                            # Go (@davidaf3)
  "./encoder_zig"                                 # Zig
)

echo "Compiling Java sources"
rm -rf build || true
javac src/java/*.java -d build/java
javac src/java/util/*.java -d build/util

echo "Compiling Lisp sources"
cd src/lisp/
sbcl --non-interactive --load build.lisp
mv lisp-phone-encoder ../../
cd ../..

echo "Compiling Rust sources"
cd src/rust/phone_encoder && cargo build --release && cp target/release/phone_encoder ../../../rust
cd ../plotter && cargo build --release && cp target/release/plotter ../../../
cd ../benchmark_runner && cargo build --release && cp target/release/benchmark_runner ../../../
cd ../../..

echo "Compiling C sources"
cd src/c && gcc -O3 phone_encoder.c word_tree.c vector.c -o ../../phone_encoder_c
cd ../..

echo "Compiling Go sources"
cd src/go && go build -o ../../phone_encoder_go
cd ../..

echo "Compiling Zig sources"
zig build-exe src/zig/encoder_zig.zig -O ReleaseFast

echo "Generating inputs"
PRINT_INPUTS=(phones_1000.txt phones_100_000.txt phones_500_000.txt phones_1_000_000_with_empty.txt)
COUNT_INPUTS=(phones_1000.txt phones_2000.txt)
java -cp "build/util" util.GeneratePhoneNumbers 1000 > phones_1000.txt
java -cp "build/util" util.GeneratePhoneNumbers 2000 > phones_2000.txt
java -cp "build/util" util.GeneratePhoneNumbers 100000 > phones_100_000.txt
java -cp "build/util" util.GeneratePhoneNumbers 500000 > phones_500_000.txt
java -cp "build/util" util.GeneratePhoneNumbers 1000000 "true" > phones_1_000_000_with_empty.txt

CHECK_FILE="proc_out.txt"
DEFAULT_INPUT="input.txt"
DEFAULT_OUTPUT="output.txt"
DE_DICTIONARY="dictionary.txt"
EN_DICTIONARY="words-quarter.txt"

echo "Checking all programs for correctness"
for CMD in "${COMMANDS[@]}"
do
  echo "Checking: $CMD"
  $CMD print $DE_DICTIONARY $DEFAULT_INPUT > $CHECK_FILE
  diff -q <(sort $CHECK_FILE) <(sort $DEFAULT_OUTPUT)
  $CMD count $DE_DICTIONARY $DEFAULT_INPUT > $CHECK_FILE
  if [ "$(cat $CHECK_FILE)" == "262" ]; then
      echo "OK"
  else
      echo "count check failed, expected 262 but got '$(cat $CHECK_FILE)'"
      exit 1
  fi
done

CSV_OUT="benchmark-result.csv"

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
  for file in "${PRINT_INPUTS[@]}"; do run_bench "$CMD print $DE_DICTIONARY $file"; done;
  for file in "${COUNT_INPUTS[@]}"; do run_bench "$CMD count $EN_DICTIONARY $file"; done;
done

echo "Generating plot"
./plotter "$CSV_OUT"

echo "Cleaning up"
rm "${PRINT_INPUTS[@]} ${COUNT_INPUTS[@]} $CHECK_FILE ./rust ./benchmark_runner ./lisp-phone-encoder ./phone_encoder_c ./phone_encoder_go ./plotter" > /dev/null 2>&1 || true
