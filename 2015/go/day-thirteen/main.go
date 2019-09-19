package main

import (
	"fmt"
	"github.com/gypsydave5/advent-of-code/2015/go/tools/by"
)

type rule struct {
	subject string
	change  int
	object  string
}

var people = make(map[string]bool)

func main() {
	lines := by.AllLines("input.txt")

	var rules []rule
	for _, line := range lines {
		r := parse(line)
		people[r.object] = true
		people[r.subject] = true
		rules = append(rules, r)
	}
	var peeps []string
	for p, _ := range people {
		peeps = append(peeps, p)
	}

	var result int
	for _, p := range permute(peeps) {
		s := score(rules, p)
		if s > result {
			result = s
		}
	}
	fmt.Println(result)
}

func parse(s string) rule {
	var subject, object, sign string
	var change int
	s = s[:len(s)-1]

	format := "%s would %s %d happiness units by sitting next to %s"
	_, _ = fmt.Sscanf(s, format, &subject, &sign, &change, &object)
	if sign == "lose" {
		change = -change
	}
	return rule{subject, change, object}
}

func score(rules []rule, seats []string) int {
	var result int
	for i := range seats {
		for _, rule := range rules {
			n1, n2 := neighbours(i, seats)
			if seats[i] == rule.subject && (seats[n1] == rule.object || seats[n2] == rule.object) {
				result += rule.change
			}
		}
	}
	return result
}

func neighbours(n int, seats []string) (int, int) {
	return (n+len(seats)-1) % len(seats), (n+len(seats)+1) % len(seats)
}

func permute(arr []string) [][]string{
	var helper func([]string, int)
	res := [][]string{}

	helper = func(arr []string, n int){
		if n == 1{
			tmp := make([]string, len(arr))
			copy(tmp, arr)
			res = append(res, tmp)
		} else {
			for i := 0; i < n; i++{
				helper(arr, n - 1)
				if n % 2 == 1{
					tmp := arr[i]
					arr[i] = arr[n - 1]
					arr[n - 1] = tmp
				} else {
					tmp := arr[0]
					arr[0] = arr[n - 1]
					arr[n - 1] = tmp
				}
			}
		}
	}
	helper(arr, len(arr))
	return res
}