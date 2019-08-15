package by

import (
	"bufio"
	"os"
)

func Line(fname string, f func(line string)) error {
	file, err := os.Open("day-two-input.txt")
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
