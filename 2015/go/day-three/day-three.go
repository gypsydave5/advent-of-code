package main

import (
	"fmt"

	"github.com/gypsydave5/advent-of-code/2015/go/tools/by"
)

func main() {
	var x, y int
	var grid = make(map[int]map[int]int)
	grid[x] = make(map[int]int)
	grid[x][y]++
	by.Rune("day-three-input.txt", func(r rune) {
		switch r {
		case '^':
			y++
		case 'v':
			y--
		case '>':
			x++
		case '<':
			x--
		}
		_, present := grid[x]
		if !present {
			grid[x] = make(map[int]int)
		}
		grid[x][y]++
	})

	var result int

	for _, xs := range grid {
		for range xs {
			result++
		}
	}

	fmt.Printf("Houses Delivered To: %d\n", result)
}
