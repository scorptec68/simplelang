# simple-language
A simple language to test our writing lexers and parsers.


It will just operate on strings and integers and booleans. With if and loops. And a print command.

Example:

	var
	   test: string
	   x: integer
	   y: integer
	   i: integer
	   doit: boolean
	endvar
	
	run
		x = 3
		test = "hello"
		    
		y = x + 3
		print("y =  " + str(x))
		if y > 3
			test = "super"
		elseif y < 3 
		    test = "wonder"
		else
		    test = "duper"
		endif
		print(test)
		test = test - "r"
		print(test)
		
		loop 10
		    i = i + 1
		endloop
		print("i = " + str(i))
	endrun
	
EBNF

	<program> ::= <var-decl> <run-defn>
	
	<var-decl> ::= var {<decl>} endvar
	
	<decl> ::= <identifier> : <type>
	
	<type> ::= integer | string | boolean
	
	<run-defn> ::= run {<statement>} endrun
	
	<statement> ::= <assignment> | <loop> | <if> | <print>
	
	<assignment> ::= <identifier> = <expression>
	
	<expression> ::= ( <expression> ) | <int-expression> | <bool-expression> | <string-expression>
	
	<int-expression> ::= <integer> 
	     | <int-expression> <binary-int-operator> <int-expression>
	
	<string-expression> ::= <string> 
	     | <str-expression> <binary-str-operator> <str-expression> | str(<expression>)
	
	<bool-expression> ::= true | false | <bool-expression> <binary-bool-operator>  <bool-expression> | <unary-bool-operator> <bool-expression>
	          
	<binary-bool-operator> ::= & | \| | = | < | > | <= | >=
	<unary-bool-operator> ::= ~
	<binary-int-operator> ::= + | - | / | *
	<unary-int-operator> ::= -
	<binary-str-operator> ::= + | -
	
	
	<loop> ::= loop | loop <int-expression> endloop | loop <bool-expression> endloop
	
	<if> ::= if <bool-expression> {<statement>} {elseif <bool-expression> {<statement>}} [else {<statement>}] endif
	
	<print> ::= print(<string-expression>)
	
tokens

	var
	endvar
	run
	endrun
	integer
	string
	boolean
	loop
	endloop
	if
	endif
	else
	elseif
	+
	/
	-
	*
	true
	false
	<string>
	<identifier>
	print
	str
	(
	)
	: