package main

type WordTree struct {
	depth    int
	children [10]*WordTree
	words    []string
}

func NewWordTree(depth int) *WordTree {
	wt := &WordTree{}
	wt.depth = depth
	for i := range wt.children {
		wt.children[i] = nil
	}
	return wt
}

func (wt *WordTree) Add(word string, clean string) {
	wt.add(word, clean, 0)
}

func (wt *WordTree) add(word string, clean string, letterPos int) {
	if letterPos == len(clean) {
		wt.words = append(wt.words, word)
		return
	}

	nextIndex := letterToDigit(clean[letterPos])
	if wt.children[nextIndex] == nil {
		wt.children[nextIndex] = NewWordTree(wt.depth + 1)
	}
	wt.children[nextIndex].add(word, clean, letterPos + 1)
}

func (wt *WordTree) FindWords(phoneNumber []int, digitPos int) map[int][]string {
	words := make(map[int][]string)
	wt.findWordsRec(phoneNumber, digitPos, &words)
	return words
}

func (wt *WordTree) findWordsRec(phoneNumber []int, digitPos int, words *map[int][]string) {
	if digitPos == len(phoneNumber) {
		return
	}

	child := wt.children[phoneNumber[digitPos]]
	if child == nil {
		return
	}

	if len(child.words) > 0 {
		(*words)[child.depth] = child.words
	}
	child.findWordsRec(phoneNumber, digitPos+1, words)
}

func letterToDigit(letter byte) int {
	switch letter {
	case 'j', 'n', 'q':
		return 1
	case 'r', 'w', 'x':
		return 2
	case 'd', 's', 'y':
		return 3
	case 'f', 't':
		return 4
	case 'a', 'm':
		return 5
	case 'c', 'i', 'v':
		return 6
	case 'b', 'k', 'u':
		return 7
	case 'l', 'o', 'p':
		return 8
	case 'g', 'h', 'z':
		return 9
	}
	return 0
}
