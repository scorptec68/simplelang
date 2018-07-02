package simple_language

import "fmt"

func ExampleLexing() {

	// test input
	inputStr := `var
       x : integer
    endvar
    run
       x = 3
    endrun`

    l := lex("test", inputStr)
    for {
		it := l.nextItem()
		fmt.Println("item:", it)
		if it.typ == itemEOF {
			break
		}
	}
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
