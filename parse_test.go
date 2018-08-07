package simple_language

import "fmt"

func ExampleParse1() {

	// test input
	inputStr := `var
       x: boolean
       y: boolean
       z: boolean
    endvar

    run
       //x = true
       //y = true | false
	   //y = x & (b | false)
       //z = y & false | x & (y | true)
	   //z = y & false | x & y | true
	   //z = (x | y & z) | (true & false | x)
	   if x & y 
		  z = y
		  z = true
	   elseif true
	      z = x | y
	   elseif x | true
	      z = y
	   else
		  z = false
	   endif
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
