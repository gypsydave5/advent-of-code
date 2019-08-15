package main

import (
	"fmt"
	"io/ioutil"
)

func main() {
	bs, _ := ioutil.ReadFile("day-one-input.txt")
	var result int
	var basement int
	for i, r := range bs {
		switch r {
		case '(':
			result++
		case ')':
			result--
		}
		if result < 0 && basement == 0 {
			basement = i + 1
		}
	}

	fmt.Println(result)
	fmt.Println(basement)
}
