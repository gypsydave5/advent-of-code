package main

import (
	"fmt"
	"log"
	"strconv"
	"strings"

	"github.com/gypsydave5/advent-of-code/2015/go/tools/by"
)

type mode func(bool) bool

var (
	on mode = func(b bool) bool {
		return true
	}
	off mode = func(b bool) bool {
		return false
	}
	toggle mode = func(b bool) bool {
		return !b
	}
)

func main() {
	lines := by.AllLines("input.txt")
	numberOfLights := holdBackTheNight(lines)
	fmt.Println(numberOfLights)
}

func holdBackTheNight(lines []string) int {
	var lightShow = new([1000][1000]bool)
	for _, s := range lines {
		l := parseLine(s)
		turnOnTheLight(lightShow, l)
	}
	return countLights(lightShow)
}

func countLights(ls *[1000][1000]bool) int {
	var count int
	for x, ys := range ls {
		for y := range ys {
			if ls[x][y] {
				count++
			}
		}
	}

	return count
}

func turnOnTheLight(ls *[1000][1000]bool, l line) {
	for x := l.p1.x; x <= l.p2.x; x++ {
		for y := l.p1.y; y <= l.p2.y; y++ {
			ls[x][y] = l.mode(ls[x][y])
		}
	}
}

func parseLine(s string) line {
	var l line
	tokens := strings.Split(s, " ")
	l.p1 = parsePoint(tokens[len(tokens)-3])
	l.p2 = parsePoint(tokens[len(tokens)-1])
	switch tokens[1] {
	case "on":
		l.mode = on
	case "off":
		l.mode = off
	default:
		l.mode = toggle
	}

	return l
}

func parsePoint(s string) point {
	var p point
	ss := strings.Split(s, ",")
	if len(ss) != 2 {
		log.Fatal(ss)
	}
	p.x, _ = strconv.Atoi(ss[0])
	p.y, _ = strconv.Atoi(ss[1])
	return p
}

type point struct {
	x int
	y int
}

type line struct {
	mode mode
	p1   point
	p2   point
}
