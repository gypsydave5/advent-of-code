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
		{"az", "ba"},
		{"z", "aa"},
		{"zzz", "aaaa"},
		{"zaz", "zba"},
	}

	for _, c := range cases {
		t.Run(fmt.Sprintf("%s->%s", c.input, c.expected), func(t *testing.T) {
			actual := increment(c.input)
			if actual != c.expected {
				t.Errorf("Expected %q but got %q", c.expected, actual)
			}
		})
	}
}

func TestHasThreeStraight(t *testing.T) {
	cases := []struct {
		input    string
		expected bool
	}{
		{"hijklmmn", true},
	}

	for _, c := range cases {
		t.Run(fmt.Sprintf("%q->%v", c.input, c.expected), func(t *testing.T) {
			actual := hasThreeStraight(c.input)
			if actual != c.expected {
				t.Errorf("Expected %v but got %v", c.expected, actual)
			}
		})
	}
}

func TestHasTwoPair(t *testing.T) {
	cases := []struct {
		input    string
		expected bool
	}{
		{"abbceffg", true},
	}

	for _, c := range cases {
		t.Run(fmt.Sprintf("%q->%v", c.input, c.expected), func(t *testing.T) {
			actual := hasTwoPair(c.input)
			if actual != c.expected {
				t.Errorf("Expected %v but got %v", c.expected, actual)
			}
		})
	}
}
