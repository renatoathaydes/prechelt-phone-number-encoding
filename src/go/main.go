package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	var consumer SolutionConsumer
	var dictFileName string
	var numbersFileName string

	if len(os.Args) > 1 && os.Args[1] == "count" {
		consumer = NewSolutionCounter(nil)
	} else {
		consumer = NewSolutionPrinter(4000)
	}

	if len(os.Args) > 2 {
		dictFileName = os.Args[2]
	} else {
		dictFileName = "tests/words.txt"
	}

	if len(os.Args) > 3 {
		numbersFileName = os.Args[3]
	} else {
		numbersFileName = "tests/numbers.txt"
	}

	loadAndEncode(consumer, dictFileName, numbersFileName)
}

func loadPhones(phonesFile string) []string {
	phones := make([]string, 0, 1000000)
	f, err := os.Open(phonesFile)
	if err != nil {
		fmt.Printf("Error reading numbers file: %s\n", err)
		os.Exit(1)
	}

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		phones = append(phones, scanner.Text())
	}

	err = f.Close()
	if err != nil {
		fmt.Printf("Error closing numbers file: %s\n", err)
		os.Exit(1)
	}
	return phones
}

func loadAndEncode(consumer SolutionConsumer, dictFile string, phonesFile string) {
	pe := NewPhoneEncoder(dictFile, consumer)
	phones := loadPhones(phonesFile)
	for _, phone := range phones {
		pe.Encode(phone)
	}	
	consumer.OnEnd()
}