#!/bin/bash

set -e
curl https://raw.githubusercontent.com/dwyl/english-words/master/words.txt --output tmp.txt
tail -n 100000 tmp.txt > words.txt
rm tmp.txt
