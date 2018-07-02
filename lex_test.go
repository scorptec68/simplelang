package simple_language

import "fmt"

func printTokens(l *lexer) {
	for {
		it := l.nextItem()
		fmt.Println("item:", it)
		if it.typ == itemEOF {
			break
		}
	}
}

func ExampleLexing1() {

	// test input
	inputStr := `var
       x: integer
    endvar
    run
       x = 3
    endrun`

    printTokens(lex("test", inputStr))
	/* Output:
item: <var>
item: "\n" (type 19)
item: "x" (type 21)
item: ":" (type 18)
item: <integer>
item: "\n" (type 19)
item: <endvar>
item: "\n" (type 19)
item: <run>
item: "\n" (type 19)
item: "x" (type 21)
item: "=" (type 3)
item: "3" (type 1)
item: "\n" (type 19)
item: <endrun>
item: EOF
	*/
}

func ExampleLexing2() {

	// test input
	inputStr := `var
       this-boy : string
    endvar
    run
       this-boy = "hi there"
    endrun`

	printTokens(lex("test", inputStr))
	/* Output:
item: <var>
item: "\n" (type 19)
item: "this-boy" (type 21)
item: ":" (type 18)
item: <string>
item: "\n" (type 19)
item: <endvar>
item: "\n" (type 19)
item: <run>
item: "\n" (type 19)
item: "this-boy" (type 21)
item: "=" (type 3)
item: "hi there" (type 2)
item: "\n" (type 19)
item: <endrun>
item: EOF
	*/
}
