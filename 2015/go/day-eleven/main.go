package main

import "fmt"

func main() {
	current := "cqjxjnds"
	current = "cqjxxyzz"
	for {
		current = increment(current)
		if validPassword(current) {
			break
		}
	}

	fmt.Println(current)
}

func validPassword(s string) bool {
	return hasTwoPair(s) && hasThreeStraight(s) && hasNoForbiddenLetters(s)
}

func increment(s string) string {
	bs := []byte(s)
	b := innercrement(bs)
	if b {
		return string(append(bs, 'a'))
	}

	return string(bs)
}

func innercrement(bs []byte) bool {
	for i := len(bs) - 1; i >= 0; i-- {
		if bs[i] < 'z' {
			bs[i]++
			return false
		}
		if bs[i] == 'z' {
			bs[i] = 'a'
			if i == 0 {
				return true
			}
		}
	}
	return false
}

func hasThreeStraight(s string) bool {
	if len(s) < 3 {
		return false
	}
	for i := 2; i < len(s); i++ {
		if s[i-2] == s[i]-2 && s[i-1] == s[i]-1 {
			return true
		}
	}
	return false
}

func hasTwoPair(s string) bool {
	if len(s) < 4 {
		return false
	}

	pairCount := 0

	for i := 1; i < len(s); i++ {
		if s[i-1] == s[i] {
			pairCount++
			i++
		}
		if pairCount == 2 {
			return true
		}
	}
	return false
}

func hasNoForbiddenLetters(s string) bool {
	for i := range s {
		switch s[i] {
		case 'i', 'o', 'l':
			return false
		}
	}
	return true
}
