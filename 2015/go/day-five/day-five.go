package main

import (
	"fmt"

	"github.com/gypsydave5/advent-of-code/2015/go/tools/by"
)

func main() {
	var niceCount int
	by.Line("day-five-input.txt", func(line string) {
		if isNicePartTwo(line) {
			niceCount++
		}
	})

	fmt.Println(niceCount)
}

func isNice(s string) bool {
	var last rune
	var vowelCount int
	var twice bool
	for _, r := range s {
		if isNaughtyPair(last, r) {
			return false
		}
		if isVowel(r) {
			vowelCount++
		}
		if r == last {
			twice = true
		}
		last = r
	}

	if vowelCount >= 3 && twice {
		return true
	}
	return false
}

func isNaughtyPair(a, b rune) bool {
	naughtyPairs := [...]string{"ab", "cd", "pq", "xy"}
	for i := range naughtyPairs {
		if string([]rune{a, b}) == naughtyPairs[i] {
			return true
		}
	}
	return false
}

func isVowel(r rune) bool {
	vowels := []rune{'a', 'e', 'i', 'o', 'u'}
	for i := range vowels {
		if r == vowels[i] {
			return true
		}
	}
	return false
}

func isNicePartTwo(line string) bool {
	pairs := []string{}
	var pair, gap bool

	for i, j, k := 0, 1, 2; k < len(line); i, j, k = i+1, j+1, k+1 {
		if line[i] == line[k] {
			gap = true
		}

		for _, p := range pairs {
			if p == line[j:k+1] {
				pair = true
			}
		}

		if gap && pair {
			return true
		}

		pairs = append(pairs, line[i:k])
	}

	return false
}
