package main

import (
	"fmt"
	"strings"
)

type SolutionPrinter struct {
	bufSize int
	buffer 	string
}

func NewSolutionPrinter(bufSize int) *SolutionPrinter {
	return &SolutionPrinter{bufSize, ""}
}

func (sp *SolutionPrinter) OnSolution(phoneNumber string, solutionMatrix [][]string) {
	sp.printSolutionRec(0, make([]string, len(solutionMatrix)), solutionMatrix, phoneNumber)
}

func (sp *SolutionPrinter) printSolutionRec(i int, partSolution []string, solutionMatrix [][]string, phoneNumber string) {
	if i == len(solutionMatrix) {
		sp.buffer += phoneNumber
		sp.buffer += ": "
		sp.buffer += strings.Join(partSolution, " ")
		sp.buffer += "\n"
		if len(sp.buffer) > sp.bufSize {
			fmt.Print(sp.buffer)
			sp.buffer = sp.buffer[:0]
		}
		return
	}

	for _, word := range solutionMatrix[i] {
		partSolution[i] = word;
		sp.printSolutionRec(i + 1, partSolution, solutionMatrix, phoneNumber)
	}
}

func (sp *SolutionPrinter) OnEnd() {
	fmt.Print(sp.buffer)
}
