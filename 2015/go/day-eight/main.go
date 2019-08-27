package main

import (
	"fmt"
	"strconv"

	"github.com/gypsydave5/advent-of-code/2015/go/tools/by"
)

func main() {
	lines := by.AllLines("input.txt")
	fmt.Println(one(lines))
	fmt.Println(two(lines))
}

func one(lines []string) int {
	var result int
	for _, line := range lines {
		result += len(line)
		s, _ := strconv.Unquote(line)
		result -= len(s)
	}
	return result
}
func two(lines []string) int {
	var result int
	for _, line := range lines {
		result -= len(line)
		s := strconv.Quote(line)
		result += len(s)
	}
	return result
}
