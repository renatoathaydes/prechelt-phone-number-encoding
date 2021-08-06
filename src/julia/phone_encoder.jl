using Prechelt

run_encodings(
              isempty(ARGS) ? "tests/words.txt" : ARGS[begin],
              length(ARGS) < 2 ? "tests/numbers.txt" : ARGS[begin+1]
             )
