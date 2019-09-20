package main

import (
	"fmt"

	"github.com/gypsydave5/advent-of-code/2015/go/tools/by"
)

func main() {
	var reindeers []reindeer
	lines := by.AllLines("input.txt")
	for _, line := range lines {
		reindeers = append(reindeers, parse(line))
	}

	for i := 0; i < 2503; i++ {
		for i := range reindeers {
			reindeers[i].tick()
		}
	}

	var max int
	for _, r := range reindeers {
		if r.distance > max {
			max = r.distance
		}
	}

	fmt.Println(max)
}

type reindeer struct {
	name      string
	speed     int
	runTime   int
	runLeft   int
	sleepTime int
	sleepLeft int
	distance  int
}

func (r *reindeer) tick() {
	if r.sleepLeft == 0 {
		r.runLeft = r.runTime
		r.sleepLeft = r.sleepTime
	}
	if r.runLeft == 0 {
		r.sleepLeft--
		return
	}
	r.distance += r.speed
	r.runLeft--
}

func parse(s string) reindeer {
	var name string
	var speed, run, sleep int
	fs := "%s can fly %d km/s for %d seconds, but then must rest for %d seconds."
	fmt.Sscanf(s, fs, &name, &speed, &run, &sleep)
	return reindeer{name, speed, run, run, sleep, 0, 0}
}
