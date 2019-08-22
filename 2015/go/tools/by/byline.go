package by

import (
	"bufio"
	"io/ioutil"
	"os"
	"strings"
)

func Line(fname string, f func(line string)) error {
	file, err := os.Open(fname)
	if err != nil {
		return err
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		f(scanner.Text())
		if err = scanner.Err(); err != nil {
			return err
		}
	}
	return nil
}

func AllLines(fname string) []string {
	bs, _ := ioutil.ReadFile(fname)
	lines := strings.Split(string(bs), "\n")
	return lines
}
