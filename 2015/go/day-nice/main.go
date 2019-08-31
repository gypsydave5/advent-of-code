// Package main provides the win
package main

import (
	"fmt"

	"github.com/gypsydave5/advent-of-code/2015/go/tools/by"
)

type node struct {
	connections []edge
}

type edge struct {
	distance int
	node     string
}

var nodes = make(map[string]node)

func main() {
	lines := by.AllLines("input.txt")
	for _, line := range lines {
		parseLine(line)
	}
	toVisit := keys(nodes)
	origins := []edge{}
	for _, node := range toVisit {
		origins = append(origins, edge{0, node})
	}
	m := findMax(origins, toVisit, 0)
	fmt.Println(m)
}

func keys(m map[string]node) []string {
	result := []string{}
	for k := range m {
		result = append(result, k)
	}
	return result
}

func traverse(options []edge, remaining []string, distance int) int {
	min := 999999999
	if len(remaining) == 0 {
		return distance
	}
	for _, option := range options {
		if contains(remaining, option.node) {
			d := traverse(nodes[option.node].connections, remove(remaining, option.node), distance+option.distance)
			if d < min {
				min = d
			}
		}
	}
	return min
}

func findMax(options []edge, remaining []string, distance int) int {
	max := 0
	if len(remaining) == 0 {
		return distance
	}
	for _, option := range options {
		if contains(remaining, option.node) {
			d := findMax(nodes[option.node].connections, remove(remaining, option.node), distance+option.distance)
			if d > max {
				max = d
			}
		}
	}
	return max
}

func remove(ss []string, r string) []string {
	result := []string{}
	for _, s := range ss {
		if s != r {
			result = append(result, s)
		}
	}
	return result
}

func contains(ss []string, s string) bool {
	for _, si := range ss {
		if si == s {
			return true
		}
	}
	return false
}

func parseLine(line string) {
	var origin, destination string
	var distance int
	fs := "%s to %s = %d"
	fmt.Sscanf(line, fs, &origin, &destination, &distance)
	_, present := nodes[origin]
	if !present {
		nodes[origin] = node{[]edge{edge{distance, destination}}}
	} else {
		nodes[origin] = node{append(nodes[origin].connections, edge{distance, destination})}
	}
	_, present = nodes[destination]
	if !present {
		nodes[destination] = node{[]edge{edge{distance, origin}}}
	} else {
		nodes[destination] = node{append(nodes[destination].connections, edge{distance, origin})}
	}
}
