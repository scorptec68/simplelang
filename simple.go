package main

import (
	"fmt"
	"io/ioutil"
	"os"
)

func main() {
	if len(os.Args) == 1 {
		fmt.Print("Missing filename to run")
		os.Exit(1)
	}
	filename := os.Args[1]

	inputBuf, err := ioutil.ReadFile(filename)
	if err != nil {
		fmt.Printf("Unable to read file %s: %s\n", filename, err)
		os.Exit(1)
	}

	l := lex(filename, string(inputBuf))

	parser := NewParser(l)
	program, err := parser.ParseProgram()
	if err != nil {
		fmt.Print(err)
		os.Exit(1)
	}

	interp := new(Interpreter)
	interp.InterpProgram(program)
	if err != nil {
		fmt.Print(err)
		os.Exit(1)
	}

	os.Exit(0)
}
