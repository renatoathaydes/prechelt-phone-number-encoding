package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"regexp"
	"strconv"
	"strings"
)

type SolutionConsumer interface {
	OnSolution(phoneNumber string, solutionMatrix [][]string)
	OnEnd()
}

type PhoneEncoder struct {
	dict           *WordTree
	minWordLength  int
	consumer       SolutionConsumer
	phoneNumberStr string
	phoneNumber    []int
}

var regex = regexp.MustCompile("[^a-zA-Z]")

func NewPhoneEncoder(dictFileName string, consumer SolutionConsumer) *PhoneEncoder {
	pe := &PhoneEncoder{}
	pe.consumer = consumer
	pe.dict, pe.minWordLength = loadDict(dictFileName)
	return pe
}

func NewPhoneEncoderDict(dict *WordTree, minWordLength int, consumer SolutionConsumer) *PhoneEncoder {
	pe := &PhoneEncoder{}
	pe.consumer = consumer
	pe.dict = dict
	pe.minWordLength = minWordLength
	return pe
}

func (pe *PhoneEncoder) Encode(phoneNumber string) {
	pe.loadPhoneNumber(phoneNumber)

	if len(pe.phoneNumber) == 1 && pe.minWordLength > 1 {
		pe.consumer.OnSolution(pe.phoneNumberStr, [][]string{{strconv.Itoa(pe.phoneNumber[0])}})
		return
	}

	if len(pe.phoneNumber) == 0 || (len(pe.phoneNumber) > 1 && len(phoneNumber) < pe.minWordLength) {
		return
	}

	pe.encodeRec(0, make([][]string, 0, 10), false)
}

func (pe *PhoneEncoder) encodeRec(digitPos int, partSolution [][]string, lastWasDigit bool) {
	words := pe.dict.FindWords(pe.phoneNumber, digitPos)
	wordsFound := len(words) > 0

	if !wordsFound {
		if lastWasDigit {
			return
		}

		words[1] = []string{strconv.Itoa(pe.phoneNumber[digitPos])}
	}

	for wordLen, wordList := range words {
		partSolution = append(partSolution, wordList)
		if digitPos + wordLen == len(pe.phoneNumber) {
			pe.consumer.OnSolution(pe.phoneNumberStr, partSolution)
		} else {
			pe.encodeRec(digitPos + wordLen, partSolution, !wordsFound)
		}
		partSolution = partSolution[:len(partSolution) - 1]
	}
}

func loadDict(fileName string) (*WordTree, int) {
	wt := NewWordTree(0)
	minWordLength := math.MaxInt

	f, err := os.Open(fileName)
	if err != nil {
		fmt.Printf("Error reading dictionary file: %s\n", err)
		os.Exit(1)
	}

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		text := scanner.Text()
		clean := cleanWord(text)
		wt.Add(text, clean)
		if len(clean) < minWordLength {
			minWordLength = len(clean)
		}
	}

	err = f.Close()
	if err != nil {
		fmt.Printf("Error closing dictionary file: %s\n", err)
		os.Exit(1)
	}

	return wt, minWordLength
}

func (pe *PhoneEncoder) loadPhoneNumber(phoneNumberStr string) {
	pe.phoneNumberStr = phoneNumberStr
	pe.phoneNumber = pe.phoneNumber[:0]
	for _, char := range phoneNumberStr {
		if char >= 48 && char <= 57 {
			pe.phoneNumber = append(pe.phoneNumber, int(char) - 48)
		}
	}
}

func cleanWord(word string) string {
	cleaned := strings.ToLower(word)
	return string(regex.ReplaceAllString(cleaned, ""))
}