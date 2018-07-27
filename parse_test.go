package simple_language

import "fmt"

func ExampleParse1() {

	// test input
	inputStr := `
    var
       x: boolean
       y: boolean
    endvar

    run
       //x = true
       //y = true | false
	   y = x & (b | false)
    endrun`

	l := lex("test", inputStr)
	parser := NewParser(l)
	program, err := parser.ParseProgram()
	if err != nil {
		fmt.Print(err)
	} else {
		PrintProgram(program, 2)
	}
	/* Output:

	 */
}
