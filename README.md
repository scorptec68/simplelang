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
	   this-boy: boolean
	endvar
	
	run
	    this-boy = true
	    x = 3 + 1000000 + 2 +
	          4 + 6
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
	
	<var-decl> ::= var \n {<decl>} endvar \n
	
	<decl> ::= <identifier> : <type> \n
	
	<type> ::= integer | string | boolean
	
	<run-defn> ::= run \n {<statement>} endrun \n
	
	<statement> ::= <assignment> | <loop> | <if> | <print>
	
	<assignment> ::= <identifier> = <expression> \n
	
	<expression> ::= ( <expression> ) | <int-expression> | <bool-expression> | <string-expression> | <identifier>
	
	<int-expression> ::= <integer> 
	     | <int-expression> <binary-int-operator> <int-expression>
	     | <unary-int-operator> <int-expression>
	
	<string-expression> ::= <string-literal> 
	     | <str-expression> <binary-str-operator> <str-expression> | str(<expression>)
	
	<bool-expression> ::= true | false | <bool-expression> <binary-bool-operator>  <bool-expression> | <unary-bool-operator> <bool-expression>
	          
	<binary-bool-operator> ::= & | \| | = | < | > | <= | >=
	<unary-bool-operator> ::= ~
	<binary-int-operator> ::= + | - | / | *
	<unary-int-operator> ::= -
	<binary-str-operator> ::= + | -
	
	
	<loop> ::= loop {<statement>} endloop | loop <int-expression> {<statement>} endloop | loop <bool-expression> {<statement>} endloop
	
	<if> ::= if <bool-expression> \n {<statement>} {elseif <bool-expression> \n {<statement>}} [else \n {<statement>}] endif \n
	
	<print> ::= print(<string-expression>) \n
	
tokens

	string-literal ::= "<any-char>"
	integer-literal ::= regex: [0-9][0-9]*
	boolean-literal ::= true | false
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
	<identifier> ::= regex: [a-zA-Z0-9_][a-zA-Z0-9_ ]*
	print
	str
	(
	)
	:
	\n
	
formatting

* requires 1 space between each token
* arbitrary leadup space for indentation
* \n token
* lexer can hide \n token if line ends in an operator

