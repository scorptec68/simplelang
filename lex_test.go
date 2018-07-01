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
	// Output: woof

}
