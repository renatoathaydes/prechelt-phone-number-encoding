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
	var newSolutions int64 = 1
	for _, row := range solutionMatrix {
		newSolutions *= int64(len(row))
	}
	sc.count += newSolutions
}

func (sc *SolutionCounter) OnEnd() {
	if sc.output == nil {
		fmt.Println(sc.count)
	} else {
		atomic.AddInt64(sc.output, sc.count)
	}
}
