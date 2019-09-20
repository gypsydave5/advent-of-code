package main

import (
	"fmt"
	"testing"
)

func TestParse(t *testing.T) {
	cases := []struct {
		input    string
		expected reindeer
	}{
		{
			"Vixen can fly 8 km/s for 8 seconds, but then must rest for 53 seconds.",
			reindeer{"Vixen", 8, 8, 8, 53, 0, 0},
		},
		{
			"Blitzen can fly 13 km/s for 4 seconds, but then must rest for 49 seconds.",
			reindeer{"Blitzen", 13, 4, 4, 49, 0, 0},
		},
	}
	for _, c := range cases {
		t.Run(fmt.Sprintf("%q->%v", c.input, c.expected), func(t *testing.T) {
			actual := parse(c.input)
			if actual != c.expected {
				t.Errorf("Expected %v but got %v", c.expected, actual)
			}
		})
	}
}

func TestReindeerTick(t *testing.T) {
	cases := []struct {
		reindeer reindeer
		expected reindeer
	}{
		{reindeer{"bob", 5, 4, 3, 2, 1, 0}, reindeer{"bob", 5, 4, 2, 2, 1, 5}},
		{reindeer{"bob", 5, 4, 0, 2, 2, 0}, reindeer{"bob", 5, 4, 0, 2, 1, 0}},
		{reindeer{"bob", 22, 4, 0, 2, 0, 0}, reindeer{"bob", 22, 4, 3, 2, 2, 22}},
	}

	for _, c := range cases {
		t.Run(fmt.Sprintf("%v->%v", c.reindeer, c.expected), func(t *testing.T) {
			c.reindeer.tick()
			if c.reindeer != c.expected {
				t.Errorf("Expected %v, got %v", c.expected, c.reindeer)
			}
		})
	}
}
