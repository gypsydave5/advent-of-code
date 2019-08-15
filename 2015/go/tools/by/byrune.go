package by

import (
	"bufio"
	"os"
)

func Rune(fname string, f func(r rune)) error {
	file, err := os.Open(fname)
	if err != nil {
		return err
	}
	defer file.Close()

	b := bufio.NewReader(file)
	for x, _, err := b.ReadRune(); err == nil; x, _, err = b.ReadRune() {
		f(x)
	}

	return nil
}
