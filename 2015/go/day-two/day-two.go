package main

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/gypsydave5/advent-of-code/2015/go/tools/by"
)

func main() {
	var result, ribbon int
	by.Line("day-two-input.txt", func(line string) {
		dimensions := getDimensions(line)
		ribbonSideOne, ribbonSideTwo := minTwo(dimensions)
		ribbon += (ribbonSideOne + ribbonSideTwo) * 2
		ribbon += dimensions[0] * dimensions[1] * dimensions[2]
		sides := sides(dimensions)
		result += minVal(sides)
		for _, side := range sides {
			result += side * 2
		}
	})

	fmt.Printf("paper: %d\nribbon: %d\n", result, ribbon)
}

func getDimensions(line string) [3]int {
	var x, y, z int
	ds := strings.Split(line, "x")
	x, _ = strconv.Atoi(ds[0])
	y, _ = strconv.Atoi(ds[1])
	z, _ = strconv.Atoi(ds[2])
	return [3]int{x, y, z}
}

func sides(dimensions [3]int) [3]int {
	return [3]int{
		dimensions[0] * dimensions[1],
		dimensions[1] * dimensions[2],
		dimensions[2] * dimensions[0],
	}
}

func minVal(dims [3]int) int {
	var result = dims[0]
	for _, i := range dims[1:] {
		if i < result {
			result = i
		}
	}
	return result
}

func minTwo(dims [3]int) (int, int) {
	var one, two, spare int
	if dims[0] < dims[1] {
		one = dims[0]
		spare = dims[1]
	} else {
		one = dims[1]
		spare = dims[0]
	}

	if dims[2] < spare {
		two = dims[2]
	} else {
		two = spare
	}

	return one, two
}

func maxVal(dims [3]int) int {
	var result int
	for _, i := range dims {
		if i > result {
			result = i
		}
	}
	return result
}
