package main

import (
	"fmt"
	"strconv"
)

func main() {
	var input = "1321131112"
	for i := 0; i < 50; i++ {
		input = lookAndSay(input)
	}
	fmt.Println(len(input))
}

func lookAndSay(s string) string {
	var result []byte
	var count = 1
	var current = s[0]

	for i := range s[1:] {
		if current == s[1:][i] {
			count++
		} else {
			result = append(result, []byte(strconv.Itoa(count))...)
			result = append(result, current)
			count = 1
			current = s[1:][i]
		}
	}

	result = append(result, []byte(strconv.Itoa(count))...)
	result = append(result, current)
	return string(result)
}
