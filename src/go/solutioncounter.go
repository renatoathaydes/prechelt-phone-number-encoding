package main

import (
	"fmt"
	"sync/atomic"
)

type SolutionCounter struct {
	count 	int64
	output 	*int64
}

func NewSolutionCounter(output *int64) *SolutionCounter {
	return &SolutionCounter{0, output}
}

func (sc *SolutionCounter) OnSolution(phoneNumber string, solutionMatrix [][]string) {
	sc.countSolutionRec(0, make([]string, len(solutionMatrix)), solutionMatrix)
}

func (sc *SolutionCounter) countSolutionRec(i int, partSolution []string, solutionMatrix [][]string) {
	if i == len(solutionMatrix) {
		sc.count++
		return
	}

	for _, word := range solutionMatrix[i] {
		partSolution[i] = word
		sc.countSolutionRec(i + 1, partSolution, solutionMatrix)
	}
}

func (sc *SolutionCounter) OnEnd() {
	if sc.output == nil {
		fmt.Println(sc.count)
	} else {
		atomic.AddInt64(sc.output, sc.count)
	}
}
