package main

import (
	"fmt"
	"testing"
)

func TestIncrementString(t *testing.T) {
	cases := []struct {
		input    string
		expected string
	}{
		{"a", "b"},
		{"aa", "ab"},
	}

	for _, c := range cases {
		t.Run(fmt.Sprintf("%s -> %s\n", c.input, c.expected), func(t *testing.T) {
			actual := increment(c.input)
			if actual != c.expected {
				t.Errorf("Expected %q but got %q", c.expected, actual)
			}
		})
	}
}
