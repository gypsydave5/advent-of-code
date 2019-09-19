package main

import (
	"fmt"
	"testing"
)

func TestRead(t *testing.T) {
	cases := []struct {
		input    string
		expected int
	}{
		{"1", 1},
		{"-1", -1},
		{"\"hello\"", 0},
		{"[1, 2, 3]", 6},
		{"[1, 2, \"hello\", 3]", 6},
		{"[1, 2, \"hello\", [3], []]", 6},
		{`{"hello": 1}`, 1},
		{`{"hello": 1, "goodbye": 2}`, 3},
		{`{"hello": 1, "goodbye": [2,3,4]}`, 10},
		{`{"hello": 1, "goodbye": [2,3,"red"]}`, 6},
		{"[1, 2, \"red\", [3], []]", 6},
		{"[1, 2, \"red\"]", 3},
		{`[1, {"red": [2,3]}, {"orange": 4}]`, 5},
		{`[1,{"c":"red","b":2},3]`, 4},
		{`{"d":"red","e":[1,2,3,4],"f":5}`, 0},
		{`[-1,"red",-5]`, -6},
		{`[{"red": [2,3]}]`, 0},
		{`[-1,{"a":1}]`, 0},
	}

	for _, c := range cases {
		t.Run(fmt.Sprintf("%s->%d", c.input, c.expected), func(t *testing.T) {
			lex := newLexer(c.input)
			actual, _ := read(lex)

			if actual != c.expected {
				t.Errorf("got %v", actual)
			}
		})
	}
}
