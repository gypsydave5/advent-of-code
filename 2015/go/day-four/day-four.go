package main

import (
	"crypto/md5"
	"fmt"
	"strconv"
)

const input = "iwrupvqb"
const example = "abcdef609043"

func main() {
	fmt.Println(findSixZeroes(input))
}

func findFiveZeroes(s string) string {
	var n = 1
	for {
		result := appendInt(s, n)
		my5 := md5.Sum([]byte(result))
		if hasFiveZeros(my5) {
			return result
		}
		n++
	}
}

func hasFiveZeros(b [md5.Size]byte) bool {
	for i := range b[:2] {
		if b[i] != 0 {
			return false
		}
	}
	if b[2] > 0x0f {
		return false
	}
	return true
}

func findSixZeroes(s string) string {
	var n = 1
	for {
		result := appendInt(s, n)
		my5 := md5.Sum([]byte(result))
		if hasSixZeros(my5) {
			return result
		}
		n++
	}
}

func hasSixZeros(b [md5.Size]byte) bool {
	for i := range b[:3] {
		if b[i] != 0 {
			return false
		}
	}
	return true
}

func appendInt(s string, i int) string {
	return s + strconv.Itoa(i)
}
