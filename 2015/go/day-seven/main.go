package main

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/gypsydave5/advent-of-code/2015/go/tools/by"
)

type dyadic struct {
	a string
	b string
	f func(a, b uint) uint
}

type monadic struct {
	a string
	f func(a uint) uint
}

var dValues = make(map[string]dyadic)
var mValues = make(map[string]monadic)
var values = make(map[string]uint)

func main() {
	lines := by.AllLines("input.txt")
	for _, l := range lines {
		parseLine(l)
	}

	a := getValue("a")
	fmt.Println(a)

	values = make(map[string]uint)
	parseLine(fmt.Sprintf("%d -> b", a))
	newA := getValue("a")
	fmt.Println(newA)
}

func lShift(n uint) func(uint) uint {
	return func(a uint) uint {
		return a << n
	}
}

func rShift(n uint) func(uint) uint {
	return func(a uint) uint {
		return a >> n
	}
}

func and(a, b uint) uint {
	return a & b
}

func or(a, b uint) uint {
	return a | b
}

func not(a uint) uint {
	return ^a
}

func assignment(a uint) uint {
	return a
}

func parseLine(l string) {
	var v, a, b string
	var n uint
	switch {
	case strings.Contains(l, "LSHIFT"):
		fmt.Sscanf(l, "%s LSHIFT %d -> %s", &a, &n, &v)
		mValues[v] = monadic{a, lShift(n)}
	case strings.Contains(l, "RSHIFT"):
		fmt.Sscanf(l, "%s RSHIFT %d -> %s", &a, &n, &v)
		mValues[v] = monadic{a, rShift(n)}
	case strings.Contains(l, "OR"):
		fmt.Sscanf(l, "%s OR %s -> %s", &a, &b, &v)
		dValues[v] = dyadic{a, b, or}
	case strings.Contains(l, "AND"):
		fmt.Sscanf(l, "%s AND %s -> %s", &a, &b, &v)
		dValues[v] = dyadic{a, b, and}
	case strings.Contains(l, "NOT"):
		fmt.Sscanf(l, "NOT %s -> %s", &a, &v)
		mValues[v] = monadic{a, not}
	default:
		fmt.Sscanf(l, "%s -> %s", &a, &v)
		mValues[v] = monadic{a, assignment}
	}
}

func getValue(v string) uint {
	if i, ok := values[v]; ok {
		return i
	}

	if m, ok := mValues[v]; ok {
		i := m.f(getValue(m.a))
		values[v] = i
		return i
	}

	if d, ok := dValues[v]; ok {
		i := d.f(getValue(d.a), getValue(d.b))
		values[v] = i
		return i
	}

	if i, err := strconv.Atoi(v); err == nil {
		values[v] = uint(i)
		return uint(i)
	}

	panic(fmt.Sprintf("WHAT? %s\n", v))
}
