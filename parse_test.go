package simple_language

import "fmt"

func ExampleParse1() {
	inputStr := `var
	   test: string
	   x: integer
	   y: integer
	   i: integer
	   doit: boolean
	   this-boy: boolean
	endvar
	
	run
	    this-boy = true
	    x = 3 + 1000000 + 2 +
	          4 + 6
	    x = 3
		test = "hello"
		    
		y = x + 3
		print("y =  " + strInt(x))
		
		if y > 3
			test = "super"
		elseif y < 3 
		    test = "wonder"
		else
		    test = "duper"
		endif
		
		print(test)
		test = test + "r"
		print(test)
		
		loop times 10
		    i = i + 1
		endloop
		
		print("i = " + strInt(i))
	endrun
	`

	// test input
	// inputStr := `var
	//    x: boolean
	//    y: boolean
	//    z: boolean
	//    a: integer
	//    b: integer
	//    c: string
	// endvar

	// run
	//x = true
	//y = true | false
	//y = x & (b | false)
	//z = y & false | x & (y | true)
	//z = y & false | x & y | true
	//z = (x | y & z) | (true & false | x)
	//    if x & y
	// 	  z = y
	// 	  z = true
	//    elseif true
	//       z = x | y
	//    elseif x | true
	//       z = y
	//    else
	// 	  z = false
	//    endif
	// loop x & y
	//    x = true
	// endloop
	//x = y | (z | true) & x
	//a = 2 / 3
	//a = 2 * 3
	//a = 3 + (b / a + 2) * 6
	//c = "hello" + " " + "there"
	//c = strInt(2 + 3 / 5)
	//print("hi" + " " + "there")
	//endrun`

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
