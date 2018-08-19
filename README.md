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

		3 secs ...
		    
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
	
EBNF

	<program> ::= <var-decl> <run-defn>
	
	<var-decl> ::= var \n {<decl>} endvar \n
	
	<decl> ::= <identifier> : <type> \n
	
	<type> ::= integer | string | boolean
	
	<run-defn> ::= run \n {<statement>} endrun \n
	
	<statement> ::= <assignment> | <loop> | <if> | <print>
	
	<assignment> ::= <identifier> = <expression> \n
	
	<expression> ::= <int-expression> | <bool-expression> | <string-expression>
	
	<bool-expression> ::= <bool-term> {<or><bool-term>}
    <bool-term> ::= <bool-factor> {<and><bool-factor>}
    <bool-factor> ::= <bool-constant>|<unary-bool-operator><bool-factor>|<identifier>|
                       <lparen> <bool-expression> <rparen>
                       |<int-comparison>
    <int-comparison> ::= <int-expression> <int-comp> <int-expression>
    
    <int-expression> ::= <int-term> {<plus-or-minus> <int-term>}
    <int-term> ::= <int-factor> {<times-or-divide> <int-factor>}
    <int-factor> ::= <int-constant> | <identifier> | <unary-int-operator><int-factor> 
                      | <lparen><int-expression><rparen>
    
	<string-expression> ::= <str-term> {<binary-str-operator> <str-term>}
	<string-term> ::= <string-literal> | <identifier> | str(<expression>)
	                     | <lparen><string-expression><rparen>
    
	<loop> ::= loop {<statement>} endloop | loop <int-expression> {<statement>} endloop | loop <bool-expression> {<statement>} endloop
	
	<if> ::= if <bool-expression> \n {<statement>} {elseif <bool-expression> \n {<statement>}} [else \n {<statement>}] endif \n
	
	<print> ::= print(<string-expression>) \n
	
tokens

    <lparen> ::= '('
    <rparen> ::= ')'
    <bool-constant> ::= false|true
    <or> ::= '|'
    <and> ::= '&'
    <not> ::= '!'
    <plus-or-minus> ::= '+' | '-'
    <times-or-divide> ::= '*' | '/'
    <unary-int-operator> ::='-'
	<binary-int-comparator> ::= '=' | '<' | '>' | '<=' | '>='
	<unary-bool-operator> ::= '~'
	<binary-str-operator> ::= '+'

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

