package main

import (
	"fmt"
	"strconv"

	"github.com/gypsydave5/advent-of-code/2015/go/tools/by"
)

func main() {
	var result int
	lines := by.AllLines("input.txt")
	for _, line := range lines {
		result += len(line)
		s, _ := strconv.Unquote(line)
		result -= len(s)
	}
	fmt.Println(result)
}
