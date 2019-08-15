package main

import (
	"fmt"
	"io/ioutil"

	"github.com/gypsydave5/advent-of-code/2015/go/tools/by"
)

func main() {
	partTwo()
}

func partOne() {
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

func partTwo() {
	var sx, sy, rx, ry int
	var grid = make(map[int]map[int]int)
	grid[sx] = make(map[int]int)
	grid[sx][sy] += 2

	s, _ := ioutil.ReadFile("day-three-input.txt")
	for i, r := range s {
		var x, y *int
		if (i % 2) == 0 {
			x, y = &sx, &sy
		} else {
			x, y = &rx, &ry
		}
		switch r {
		case '^':
			*y++
		case 'v':
			*y--
		case '>':
			*x++
		case '<':
			*x--
		}
		_, present := grid[*x]
		if !present {
			grid[*x] = make(map[int]int)
		}
		grid[*x][*y]++
	}

	var result int

	for _, xs := range grid {
		for range xs {
			result++
		}
	}

	fmt.Printf("Houses Delivered To: %d\n", result)
}
