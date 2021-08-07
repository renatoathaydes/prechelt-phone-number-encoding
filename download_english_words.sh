#!/bin/bash

set -e
curl https://raw.githubusercontent.com/dwyl/english-words/master/words.txt --output tmp.txt

# the dictionary has 466k words, so we can skip every 5 words to get close to 100k
cat tmp.txt | awk "NR % 5 == 0 && NR < 500001" > words.txt
rm tmp.txt
