package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
)

func main() {
	bs, _ := ioutil.ReadFile("input.txt")
	var result int
	var n []byte
	for i := range bs {
		if bs[i] >= '0' && bs[i] <= '9' {
			n = append(n, bs[i])
			continue
		}
		if bs[i] == '-' {
			n = append(n, bs[i])
			continue
		}
		if len(n) > 0 {
			num, err := strconv.Atoi(string(n))
			if err != nil {
				fmt.Println(err)
			}
			result += num
			n = []byte{}
		}
	}
	if len(n) > 0 {
		num, _ := strconv.Atoi(string(n))
		result += num
	}
	fmt.Println(result)
}
