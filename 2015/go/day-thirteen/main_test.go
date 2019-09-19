package main

import (
	"github.com/google/go-cmp/cmp"
	"testing"
)

func TestParse(t *testing.T) {
	cases := []struct {
		input    string
		expected rule
	}{
		{
			"Alice would gain 2 happiness units by sitting next to Bob.",
			rule{"Alice", 2, "Bob"},
		},
		{
			"Alice would lose 82 happiness units by sitting next to David.",
			rule{"Alice", -82, "David"},
		},
	}

	for _, c := range cases {
		actual := parse(c.input)
		if actual != c.expected {
			t.Errorf("Expected %q but got %q", c.expected, actual)
		}
	}
}

func TestScore(t *testing.T) {
	cases := []struct {
		rules    []rule
		seating  []string
		expected int
	}{
		{[]rule{
			{"Alice", 5, "Bob"},
			{"Bob", 5, "Alice"},
			{"Alice", 5, "Dave"},
			{"Bob", 5, "Dave"},
			{"Dave", 5, "Alice"},
			{"Dave", 5, "Bob"},
		},
			[]string{"Alice", "Bob", "Dave"},
			30,
		},
		{
			[]rule{
				{"Alice", 1, "Bob"},
				{"Bob", 1, "Alice"},
				{"Alice", 1, "Dave"},
				{"Bob", 1, "Dave"},
				{"Dave", 1, "Alice"},
				{"Dave", 1, "Bob"},
			},
			[]string{"Alice", "Bob", "Dave"},
			6,
		},
	}

	for _, c := range cases {
		actual := score(c.rules, c.seating)

		if actual != c.expected {
			t.Errorf("Expected %d, but got %d", c.expected, actual)
		}
	}
}

func TestNeighbours(t *testing.T) {
	seats := []string{"A", "B", "C", "D", "E"}
	cases := []struct{
		seat, n1, n2 int
	} {
		{1, 0, 2},
		{0,4, 1},
		{4,3,0},
	}

	for _,  c := range cases{
		n1, n2 := neighbours(c.seat, seats)

		if (n1 != c.n1) || (n2 != c.n2) {
			t.Errorf("Expected %d %d, but got %d %d", c.n1, c.n2, n1, n2)
		}
	}
}

func TestPermute(t *testing.T) {
	cases := []struct{
		ss []string
		expected [][]string
	}{
		{
			[]string{},
			[][]string{},
		},
		{
			[]string{"a", "b"},
			[][]string{{"a", "b"}, {"b", "a"}},
		},
	}

	for _, c := range cases {
		result := permute(c.ss)
		if !cmp.Equal(result, c.expected) {
			t.Errorf("Expected %v, got %v", c.expected, result)
		}
	}
}

