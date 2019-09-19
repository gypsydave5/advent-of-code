package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
	"text/scanner"

	"github.com/pkg/errors"
)

const badColour = "red"

func main() {
	bs, _ := ioutil.ReadFile("input.txt")
	lex := newLexer(string(bs))
	i, err := read(lex)
	if err != nil {
		fmt.Println(0)
	} else {
		fmt.Println(i)
	}
}

type lexer struct {
	scan  scanner.Scanner
	token rune // the current token
}

func (lex *lexer) next()        { lex.token = lex.scan.Scan() }
func (lex *lexer) text() string { return lex.scan.TokenText() }

func newLexer(s string) *lexer {
	l := &lexer{scan: scanner.Scanner{Mode: scanner.GoTokens}}
	l.scan.Init(strings.NewReader(s))
	l.next()
	return l
}

func (lex *lexer) consume(want rune) {
	if lex.token != want { // NOTE: Not an example of good error handling.
		panic(fmt.Sprintf("got %q, want %q", lex.text(), want))
	}
	lex.next()
}

func read(lex *lexer) (int, error) {
	switch lex.token {
	case scanner.String:
		s, _ := strconv.Unquote(lex.text()) // NOTE: ignoring errors
		lex.next()
		if s == badColour {
			return 0, errors.Errorf("BAD")
		}
		return 0, nil
	case scanner.Int:
		i, _ := strconv.Atoi(lex.text()) // NOTE: ignoring errors
		lex.next()
		return i, nil
	case '{':
		lex.next()
		i := readObject(lex)
		return i, nil
	case '[':
		lex.next()
		i := readArray(lex)
		return i, nil
	case '-':
		lex.next()
		x, _ := read(lex)
		return -x, nil
	}
	lex.next()
	return 0, nil
}

func readArray(lex *lexer) int {
	var result int
	// var err error
	for {
		if lex.token == ']' {
			lex.next()
			return result
		}
		r, _ := read(lex)
		result += r
	}
}

func readObject(lex *lexer) int {
	var result int
	var err error
	for {
		if lex.token == '}' {
			lex.next()
			if err != nil {
				return 0
			}
			return result
		}
		r, e := read(lex)
		if e != nil {
			err = e
		}
		result += r
	}

}
