package simple_language

import (
	"fmt"
	"strconv"
)

type ValueType int
type StatementType int
type LoopType int
type ExpressionType int
type IntExpressionType int
type IntOperatorType int
type IntComparatorType int
type BoolExpressionType int
type BoolOperatorType int
type StringExpressionType int
type StringOperatorType int

const (
	ValueInteger ValueType = iota
	ValueString
	ValueBoolean
	ValueNone
)

const (
	StmtLoop StatementType = iota
	StmtIf
	StmtAssignment
	StmtPrint
)

const (
	LoopForever LoopType = iota
	LoopWhile
	LoopTimes
)

const (
	ExprnIdentifier ExpressionType = iota
	ExprnInteger
	ExprnBoolean
	ExprnString
)

const (
	IntExprnValue IntExpressionType = iota
	IntExprnVariable
	IntExprnBinary
	IntExprnUnary
)

const (
	IntUnaryOpNegative IntOperatorType = iota
	IntBinaryOp
	IntBinaryOpAdd
	IntBinaryOpMinus
	IntBinaryOpTimes
	IntBinaryOpDivide
)

const (
	IntCompLessThan IntComparatorType = iota
	IntCompGreaterThan
	IntCompLessEquals
	IntCompGreaterEquals
	IntCompEquals
)

const (
	StringExprnValue StringExpressionType = iota
	StringExprnVariable
	StringExprnBinary
)

const (
	StringBinaryOpAdd StringOperatorType = iota
	StringBinaryOpMinus
)

type Program struct {
	variables *Variables // ?? make sense in Program or in Parser??
	stmtList  []*Statement
}

type Variables struct {
	values map[string]*Value
}

type Parser struct {
	variables *Variables

	lex       *lexer
	token     [3]item // three-token lookahead for parser.
	peekCount int
}

//-------------------------------------------------------------------------------

// nextItem returns the nextItem token from lexer or saved from peeking.
func (parser *Parser) nextItem() item {
	if parser.peekCount > 0 {
		parser.peekCount--
	} else {
		parser.token[0] = parser.lex.nextItem()
	}
	fmt.Println("-> token: ", parser.token[parser.peekCount])
	return parser.token[parser.peekCount]
}

func (parser *Parser) match(itemTyp itemType, context string) (err error) {
	item := parser.nextItem()
	fmt.Printf("-> matching on item: %v, got token: %v\n", itemTyp, item)
	if item.typ != itemTyp {
		return parser.Errorf("Expecting %v in %s but got \"%v\"", itemTyp, context, item.typ)
	}
	return nil
}

// backup backs the input stream up one token.
func (parser *Parser) backup() {
	parser.peekCount++
}

// backup2 backs the input stream up two tokens.
// The zeroth token is already there.
func (parser *Parser) backup2(t1 item) {
	parser.token[1] = t1
	parser.peekCount = 2
}

// backup3 backs the input stream up three tokens
// The zeroth token is already there.
func (parser *Parser) backup3(t2, t1 item) { // Reverse order: we're pushing back.
	parser.token[1] = t1
	parser.token[2] = t2
	parser.peekCount = 3
}

// peek returns but does not consume the nextItem token.
func (parser *Parser) peek() item {
	if parser.peekCount > 0 {
		return parser.token[parser.peekCount-1]
	}
	parser.peekCount = 1
	parser.token[0] = parser.lex.nextItem()
	return parser.token[0]
}

//-------------------------------------------------------------------------------

func printIndent(indent int) {
	for indent > 0 {
		fmt.Print("  ")
		indent--
	}
}

func printfIndent(indent int, format string, a ...interface{}) {
	printIndent(indent)
	fmt.Printf(format, a...)
}

func PrintProgram(prog *Program, indent int) {
	printfIndent(indent, "Program\n")
	PrintVariables(prog.variables, indent+1)
	PrintStatementList(prog.stmtList, indent+1)
}

func PrintVariables(vars *Variables, indent int) {
	printfIndent(indent, "Variables\n")
	fmt.Printf("%v\n", vars.values)
}

func PrintStatementList(stmtList []*Statement, indent int) {
	printfIndent(indent, "StatementList\n")
	for _, stmt := range stmtList {
		PrintOneStatement(stmt, indent+1)
	}
}

func PrintOneStatement(stmt *Statement, indent int) {
	printfIndent(indent, "Statement (type code: %v)\n", stmt.stmtType)

	switch stmt.stmtType {
	case StmtAssignment:
		PrintAssignmentStmt(stmt.assignmentStmt, indent+1)
	case StmtIf:
		PrintIfStmt(stmt.ifStmt, indent+1)
	case StmtLoop:
		PrintLoopStmt(stmt.loopStmt, indent+1)
	case StmtPrint:
		// PrintPrintStmt(stmt.printStmt, indent + 1)
	}
}

func PrintAssignmentStmt(assign *AssignmentStatement, indent int) {
	printfIndent(indent, "Assignment\n")

	printfIndent(indent, "lhs var = %s\n", assign.identifier)
	PrintExpression(assign.exprn, indent+1)
}

func PrintIfStmt(ifStmt *IfStatement, indent int) {
	printfIndent(indent, "If Statement\n")

	printfIndent(indent, "predicate\n")
	PrintBooleanExpression(ifStmt.boolExpression, indent+1)

	printfIndent(indent, "if stmts\n")
	PrintStatementList(ifStmt.stmtList, indent+1)

	// print the elseif parts
	for i, elseif := range ifStmt.elsifList {
		printfIndent(indent+1, "[%d] elsif\n", i)
		printElseIfStmt(elseif, indent+1)
	}

	if len(ifStmt.elseStmtList) > 0 {
		printfIndent(indent, "else stmts\n")
		PrintStatementList(ifStmt.elseStmtList, indent+1)
	}
}

func PrintLoopStmt(loopStmt *LoopStatement, indent int) {
	printfIndent(indent, "Loop Statement (%v)\n", loopStmt.loopType)
	switch loopStmt.loopType {
	case LoopWhile:
		PrintBooleanExpression(loopStmt.boolExpression, indent+1)
	case LoopTimes:
		//PrintIntExpression(loopStmt.intExpression, indent+1)
	}
	PrintStatementList(loopStmt.stmtList, indent+1)
}

func printElseIfStmt(elseif *ElseIf, indent int) {
	printfIndent(indent, "elsif expression\n")
	PrintBooleanExpression(elseif.boolExpression, indent+1)

	printfIndent(indent, "elsif stmts\n")
	PrintStatementList(elseif.stmtList, indent+1)
}

func PrintExpression(exprn *Expression, indent int) {
	printfIndent(indent, "Expression\n")
	switch exprn.exprnType {
	case ExprnBoolean:
		PrintBooleanExpression(exprn.boolExpression, indent+1)
	case ExprnInteger:
		PrintIntExpression(exprn.intExpression, indent+1)
	case ExprnString:
		// PrintStringExpression(exprn.stringExpression, indent + 1)
	}
}

func PrintBooleanExpression(exprn *BoolExpression, indent int) {
	printfIndent(indent, "Boolean Expression\n")
	PrintOrTerms(exprn.boolOrTerms, indent)
}

func PrintOrTerms(orTerms []*BoolTerm, indent int) {
	printfIndent(indent, "Or Terms\n")
	for i, term := range orTerms {
		PrintOrTerm(i, term, indent+1)
	}
}

func PrintOrTerm(i int, term *BoolTerm, indent int) {
	printfIndent(indent, "[%d]: term\n", i)
	PrintAndFactors(term.boolAndFactors, indent+1)
}

func PrintAndFactors(andFactors []*BoolFactor, indent int) {
	printfIndent(indent, "And Factors\n")
	for i, factor := range andFactors {
		PrintBoolFactor(i, factor, indent+1)
	}
}

func PrintBoolFactor(i int, factor *BoolFactor, indent int) {
	printfIndent(indent, "[%d]: factor\n", i)
	switch factor.boolFactorType {
	case BoolFactorNot:
		printfIndent(indent, "Not factor\n")
		PrintBoolFactor(i, factor.notBoolFactor, indent+1)
	case BoolFactorConst:
		printfIndent(indent, "Const factor: %t\n", factor.boolConst)
	case BoolFactorId:
		printfIndent(indent, "Id factor: %s\n", factor.boolIdentifier)
	case BoolFactorBracket:
		printfIndent(indent, "Bracket expression\n")
		PrintBooleanExpression(factor.bracketedExprn, indent+1)
	}
}

func PrintIntExpression(exprn *IntExpression, indent int) {
	printfIndent(indent, "Integer Expression\n")
	if len(exprn.plusTerms) > 0 {
		PrintPlusTerms(exprn.plusTerms, indent)
	}
	if len(exprn.minusTerms) > 0 {
		PrintMinusTerms(exprn.minusTerms, indent)
	}
}

func PrintPlusTerms(plusTerms []*IntTerm, indent int) {
	printfIndent(indent, "Plus Terms\n")
	for i, term := range plusTerms {
		PrintPlusTerm(i, term, indent+1)
	}
}

func PrintMinusTerms(minusTerms []*IntTerm, indent int) {
	printfIndent(indent, "Minus Terms\n")
	for i, term := range minusTerms {
		PrintMinusTerm(i, term, indent+1)
	}
}

func PrintPlusTerm(i int, term *IntTerm, indent int) {
	printfIndent(indent, "[%d]: plus term\n", i)
	if len(term.timesFactors) > 0 {
		PrintTimesFactors(term.timesFactors, indent+1)
	}
	if len(term.divideFactors) > 0 {
		PrintDivideFactors(term.divideFactors, indent+1)
	}
}

func PrintMinusTerm(i int, term *IntTerm, indent int) {
	printfIndent(indent, "[%d]: minus term\n", i)
	PrintTimesFactors(term.timesFactors, indent+1)
	PrintDivideFactors(term.divideFactors, indent+1)
}

func PrintTimesFactors(timesFactors []*IntFactor, indent int) {
	printfIndent(indent, "Times Factors\n")
	for i, factor := range timesFactors {
		PrintIntFactor(i, factor, indent+1)
	}
}

func PrintDivideFactors(divideFactors []*IntFactor, indent int) {
	printfIndent(indent, "Divide Factors\n")
	for i, factor := range divideFactors {
		PrintIntFactor(i, factor, indent+1)
	}
}

func PrintIntFactor(i int, factor *IntFactor, indent int) {
	printfIndent(indent, "[%d]: factor\n", i)
	switch factor.intFactorType {
	case IntFactorMinus:
		printfIndent(indent, "Minus factor\n")
		PrintIntFactor(i, factor.minusIntFactor, indent+1)
	case IntFactorConst:
		printfIndent(indent, "Const factor: %d\n", factor.intConst)
	case IntFactorId:
		printfIndent(indent, "Id factor: %s\n", factor.intIdentifier)
	case IntFactorBracket:
		printfIndent(indent, "Bracket expression\n")
		PrintIntExpression(factor.bracketedExprn, indent+1)
	}
}

func NewParser(l *lexer) *Parser {
	return &Parser{
		lex: l,
	}
}

func (parser *Parser) ParseProgram() (prog *Program, err error) {
	prog = new(Program)
	prog.variables, err = parser.parseVariables()
	if err != nil {
		return nil, err
	}
	parser.variables = prog.variables

	err = parser.match(itemRun, "program")
	if err != nil {
		return nil, err
	}

	err = parser.match(itemNewLine, "program")
	if err != nil {
		return nil, err
	}

	prog.stmtList, err = parser.parseStatementList()
	if err != nil {
		return nil, err
	}

	err = parser.match(itemEndRun, "program")
	if err != nil {
		return nil, err
	}
	return prog, nil
}

func (parser *Parser) parseVariables() (vars *Variables, err error) {
	vars = new(Variables)
	vars.values = make(map[string]*Value)

	item := parser.nextItem()
	if item.typ != itemVar {
		// no variables to process
		parser.backup()
		return vars, nil
	}

	err = parser.match(itemNewLine, "Var start")
	if err != nil {
		return nil, err
	}

	// we have potentially some variables (could be empty)
	for {
		item = parser.nextItem()
		switch item.typ {
		case itemEndVar:
			// end of variable declaration
			err = parser.match(itemNewLine, "Var End")
			if err != nil {
				return nil, err
			}
			return vars, nil
		case itemEOF:
			// end of any input which is an error
			err := parser.Errorf("Cannot find EndVar")
			return nil, err
		case itemIdentifier:
			idStr := item.val

			err = parser.match(itemColon, "Variable declaration")
			if err != nil {
				return nil, err
			}

			value, err := parser.parseValue()
			if err != nil {
				return nil, err
			}
			vars.values[idStr] = value

			err = parser.match(itemNewLine, "Variable declaration")
			if err != nil {
				return nil, err
			}
		default:
			return nil, parser.Errorf("Unexpected token: %s in variables section", item)
		}
	}
}

func (parser *Parser) parseValue() (value *Value, err error) {
	item := parser.nextItem()
	if !(item.typ == itemString || item.typ == itemBoolean || item.typ == itemInteger) {
		err := parser.Errorf("Expecting a variable type")
		return nil, err
	}
	switch item.typ {
	case itemString:
		value = &Value{valueType: ValueString, stringVal: item.val}
	case itemInteger:
		i, _ := strconv.Atoi(item.val)
		value = &Value{valueType: ValueInteger, intVal: i}
	case itemBoolean:
		b, _ := strconv.ParseBool(item.val)
		value = &Value{valueType: ValueBoolean, boolVal: b}
	}
	return value, nil
}

func (parser *Parser) lookupType(id string) ValueType {
	val, ok := parser.variables.values[id]
	if ok {
		return val.valueType
	}
	return ValueNone
}

func isStmtListEndKeyword(i item) bool {
	return i.typ == itemEndRun || i.typ == itemEndLoop || i.typ == itemEndIf ||
		i.typ == itemElse || i.typ == itemElseIf

}

func (parser *Parser) parseStatementList() ([]*Statement, error) {
	var stmtList []*Statement
	for {
		stmt, err := parser.parseStatement()
		if err != nil {
			return nil, err
		}
		stmtList = append(stmtList, stmt)
		item := parser.peek()
		if isStmtListEndKeyword(item) {
			return stmtList, nil
		}
	}
}

func (parser *Parser) Errorf(format string, a ...interface{}) error {
	item := parser.token[0]
	preamble := fmt.Sprintf("Error at line %d: ", item.line)
	return fmt.Errorf(preamble+format, a...)
	//return errors.New(fmt.Sprintf(preamble+format, a...))
}

func (parser *Parser) parseStatement() (stmt *Statement, err error) {
	var stmtType StatementType

	var assignStmt *AssignmentStatement
	var ifStmt *IfStatement
	var loopStmt *LoopStatement
	var printStmt *PrintStatement

	item := parser.nextItem()
	switch item.typ {
	case itemIdentifier:
		stmtType = StmtAssignment
		parser.backup()
		assignStmt, err = parser.parseAssignment()
		if err != nil {
			return nil, err
		}
	case itemIf:
		stmtType = StmtIf
		ifStmt, err = parser.parseIfStatement()
		if err != nil {
			return nil, err
		}
	case itemLoop:
		stmtType = StmtLoop
		loopStmt, err = parser.parseLoopStatement()
		if err != nil {
			return nil, err
		}
	/*
		case itemPrint:
			stmtType = StmtPrint
			printStmt, err = parser.parsePrintStatement()
			if err !=  nil {
				return nil, err
			}
	*/
	default:
		return nil, parser.Errorf("Missing leading statement token. Got %v", item)
	}

	return &Statement{stmtType, assignStmt, ifStmt, loopStmt, printStmt}, err
}

// Note: other parsers use panic/recover instead of returning an error

// Grammar
//	<loop> ::= loop \n {<statement>} endloop \n |
//             loop times <int-expression> \n {<statement>} endloop \n |
//             loop <bool-expression> \n {<statement>} endloop \n
//
func (parser *Parser) parseLoopStatement() (loopStmt *LoopStatement, err error) {
	loopStmt = new(LoopStatement)

	switch parser.peek().typ {
	case itemNewLine:
		// forever loop
		// just statements and no conditional part of loop construct
		loopStmt.loopType = LoopForever
	case itemLoopTimes:
		parser.nextItem() // move over the "times" keyword
		loopStmt.loopType = LoopTimes
		//loopStmt.intExpression, err = parser.parseIntExpression()
		if err != nil {
			return nil, err
		}

	default:
		// while loop
		loopStmt.loopType = LoopWhile
		loopStmt.boolExpression, err = parser.parseBoolExpression()
		if err != nil {
			return nil, err
		}
	}

	// now parse the newline and statement list...

	err = parser.match(itemNewLine, "loop")
	if err != nil {
		return nil, err
	}
	loopStmt.stmtList, err = parser.parseStatementList()
	if err != nil {
		return nil, err
	}

	err = parser.match(itemEndLoop, "loop")
	if err != nil {
		return nil, err
	}
	err = parser.match(itemNewLine, "loop")
	if err != nil {
		return nil, err
	}

	return loopStmt, nil
}

// Grammar
// <if> ::= if <bool-expression> \n {<statement>}
//    {elseif <bool-expression> \n {<statement>}} [else \n {<statement>}] endif \n
//
func (parser *Parser) parseIfStatement() (ifStmt *IfStatement, err error) {
	ifStmt = new(IfStatement)

	ifStmt.boolExpression, err = parser.parseBoolExpression()
	if err != nil {
		return nil, err
	}
	err = parser.match(itemNewLine, "if statement")
	if err != nil {
		return nil, err
	}
	ifStmt.stmtList, err = parser.parseStatementList()
	if err != nil {
		return nil, err
	}
	for {
		item := parser.nextItem()
		switch item.typ {
		case itemElseIf:
			elseIf, err := parser.parseElseIf()
			if err != nil {
				return nil, err
			}
			ifStmt.elsifList = append(ifStmt.elsifList, elseIf)
		case itemElse:
			err = parser.match(itemNewLine, "else")
			if err != nil {
				return nil, err
			}
			ifStmt.elseStmtList, err = parser.parseStatementList()
			if err != nil {
				return nil, err
			}
		case itemEndIf:
			err = parser.match(itemNewLine, "if")
			if err != nil {
				return nil, err
			}
			return ifStmt, nil
		default:
			return nil, parser.Errorf("Bad token in if statement")
		}
	}

}

// grammar:
//    elseif <bool-expression> \n {<statement>}
//
func (parser *Parser) parseElseIf() (elseIf *ElseIf, err error) {
	elseIf = new(ElseIf)
	elseIf.boolExpression, err = parser.parseBoolExpression()
	if err != nil {
		return nil, err
	}
	err = parser.match(itemNewLine, "elseif")
	if err != nil {
		return nil, err
	}
	elseIf.stmtList, err = parser.parseStatementList()
	if err != nil {
		return nil, err
	}
	return elseIf, nil
}

func (parser *Parser) parseAssignment() (assign *AssignmentStatement, err error) {
	assign = new(AssignmentStatement)
	idItem := parser.nextItem()
	assign.identifier = idItem.val

	err = parser.match(itemEquals, "Assignment")
	if err != nil {
		return nil, err
	}

	idType := parser.lookupType(assign.identifier)
	switch idType {
	case ValueBoolean:
		boolExprn, err := parser.parseBoolExpression()
		if err != nil {
			return nil, err
		}
		assign.exprn = new(Expression)
		assign.exprn.exprnType = ExprnBoolean
		assign.exprn.boolExpression = boolExprn
	case ValueInteger:
		intExprn, err := parser.parseIntExpression()
		if err != nil {
			return nil, err
		}
		assign.exprn = new(Expression)
		assign.exprn.exprnType = ExprnInteger
		assign.exprn.intExpression = intExprn
	case ValueString:
		// TODO
		return assign, nil
	default:
		return nil, parser.Errorf("Assignment to undeclared variable: %s", idItem.val)
	}

	err = parser.match(itemNewLine, "assignment")
	if err != nil {
		return nil, err
	}

	return assign, nil
}

//
// e.g: (a + 3 * (c - 4)) < 10 & (d & e | f)
//
// This function is broken
// Change grammar to:
//
//<bool-expression>::=<bool-term>{<or><bool-term>}
//<bool-term>::=<bool-factor>{<and><bool-factor>}
//<bool-factor>::=<bool-constant>|<not><bool-factor>|(<bool-expression>)|<int-comparison>
//
// Leave out int-comparison for 1st testing
//
//<int-comparison>::=<int-expression><int-comp><int-expression>
//
//<int-expression>::=<int-term>{<plus-or-minus><int-term>}
//<int-term>::=<int-factor>{<times-or-divice><int-factor>}
//<int-factor>::=<int-constant>|<minus><int-factor>|(<int-expression>)
//...
//<bool-constant>::= false|true
//<or>::='|'
//<and>::='&'
//<not>::='!'
//<plus-or-minus>::='+' | '-'
//<times-or-divide>::= '*' | '/'
//<minus>::='-'
//

//<bool-expression>::=<bool-term>{<or><bool-term>}
func (parser *Parser) parseBoolExpression() (boolExprn *BoolExpression, err error) {
	boolExprn = new(BoolExpression)

	// process 1st term
	boolTerm, err := parser.parseBoolTerm()
	if err != nil {
		return nil, parser.Errorf("Error parsing boolean term")
	}
	boolExprn.boolOrTerms = append(boolExprn.boolOrTerms, boolTerm)

	// optionally process others
	for parser.peek().typ == itemOr {
		err = parser.match(itemOr, "Boolean Expression")
		if err != nil {
			return nil, err
		}
		boolTerm, err = parser.parseBoolTerm()
		if err != nil {
			return nil, parser.Errorf("Error parsing boolean term")
		}
		boolExprn.boolOrTerms = append(boolExprn.boolOrTerms, boolTerm)
	}
	return boolExprn, nil
}

func (parser *Parser) parseIntExpression() (intExprn *IntExpression, err error) {
	intExprn = new(IntExpression)

	// process 1st term
	intTerm, err := parser.parseIntTerm()
	if err != nil {
		return nil, parser.Errorf("Error parsing integer term")
	}
	intExprn.plusTerms = append(intExprn.plusTerms, intTerm)

	// optionally process others
	var usingPlus bool
loop:
	for {
		switch parser.peek().typ {
		case itemPlus:
			usingPlus = true
		case itemMinus:
			usingPlus = false
		default:
			break loop
		}
		parser.nextItem()
		intTerm, err := parser.parseIntTerm()
		if err != nil {
			return nil, parser.Errorf("Error parsing int term")
		}
		if usingPlus {
			intExprn.plusTerms = append(intExprn.plusTerms, intTerm)
		} else {
			intExprn.minusTerms = append(intExprn.minusTerms, intTerm)
		}
	}
	return intExprn, nil
}

func (parser *Parser) parseIntTerm() (intTerm *IntTerm, err error) {
	intTerm = new(IntTerm)

	// process 1st factor
	intFactor, err := parser.parseIntFactor()
	if err != nil {
		return nil, parser.Errorf("Error parsing integer factor")
	}
	intTerm.timesFactors = append(intTerm.timesFactors, intFactor)

	// optionally process others
	var usingTimes bool
loop:
	for {
		switch parser.peek().typ {
		case itemTimes:
			usingTimes = true
		case itemDivide:
			usingTimes = false
		default:
			break loop
		}
		parser.nextItem()
		intFactor, err := parser.parseIntFactor()
		if err != nil {
			return nil, parser.Errorf("Error parsing int term")
		}
		if usingTimes {
			intTerm.timesFactors = append(intTerm.timesFactors, intFactor)
		} else {
			intTerm.divideFactors = append(intTerm.divideFactors, intFactor)
		}
	}
	return intTerm, nil
}

//<bool-term>::=<bool-factor>{<and><bool-factor>}
func (parser *Parser) parseBoolTerm() (boolTerm *BoolTerm, err error) {
	boolTerm = new(BoolTerm)

	// process 1st factor
	boolFactor, err := parser.parseBoolFactor()
	if err != nil {
		return nil, parser.Errorf("Error parsing boolean factor")
	}
	boolTerm.boolAndFactors = append(boolTerm.boolAndFactors, boolFactor)

	// optionally process others
	for parser.peek().typ == itemAnd {
		parser.nextItem()
		boolFactor, err = parser.parseBoolFactor()
		if err != nil {
			return nil, parser.Errorf("Error parsing boolean term")
		}
		boolTerm.boolAndFactors = append(boolTerm.boolAndFactors, boolFactor)
	}
	return boolTerm, err
}

//<bool-factor>::=<bool-constant>|<not><bool-factor>|(<bool-expression>)|<int-comparison>
func (parser *Parser) parseBoolFactor() (boolFactor *BoolFactor, err error) {
	boolFactor = new(BoolFactor)

	item := parser.nextItem()
	switch item.typ {
	case itemIdentifier:
		boolFactor.boolFactorType = BoolFactorId
		boolFactor.boolIdentifier = item.val
	case itemTrue:
		boolFactor.boolFactorType = BoolFactorConst
		boolFactor.boolConst = true
	case itemFalse:
		boolFactor.boolFactorType = BoolFactorConst
		boolFactor.boolConst = false
	case itemNot:
		boolFactor.boolFactorType = BoolFactorNot
		factor, err := parser.parseBoolFactor()
		if err != nil {
			return nil, parser.Errorf("Not missing factor")
		}
		boolFactor.notBoolFactor = factor
	case itemLeftParen:
		boolFactor.boolFactorType = BoolFactorBracket
		exprn, err := parser.parseBoolExpression()
		if err != nil {
			return nil, parser.Errorf("Can not process bracketed expression")
		}
		boolFactor.bracketedExprn = exprn

		err = parser.match(itemRightParen, "Bracketed expression")
		if err != nil {
			return nil, err
		}
	default:
		return nil, parser.Errorf("Invalid boolean factor")
	}
	return boolFactor, nil
}

func (parser *Parser) parseIntFactor() (intFactor *IntFactor, err error) {
	intFactor = new(IntFactor)

	item := parser.nextItem()
	switch item.typ {
	case itemIdentifier:
		intFactor.intFactorType = IntFactorId
		intFactor.intIdentifier = item.val
	case itemIntegerLiteral:
		intFactor.intFactorType = IntFactorConst
		intFactor.intConst, err = strconv.Atoi(item.val)
		if err != nil {
			return nil, parser.Errorf("Invalid integer literal")
		}
	case itemMinus:
		intFactor.intFactorType = IntFactorMinus
		intFactor.minusIntFactor, err = parser.parseIntFactor()
		if err != nil {
			return nil, parser.Errorf("Minus missing int factor")
		}
	case itemLeftParen:
		intFactor.intFactorType = IntFactorBracket
		intFactor.bracketedExprn, err = parser.parseIntExpression()
		if err != nil {
			return nil, parser.Errorf("Can not process bracketed expression")
		}

		err = parser.match(itemRightParen, "Bracketed expression")
		if err != nil {
			return nil, err
		}
	default:
		return nil, parser.Errorf("Invalid integer factor")
	}
	return intFactor, nil
}

type Value struct {
	valueType ValueType

	intVal    int
	stringVal string
	boolVal   bool
}

func (v *Value) String() string {
	switch v.valueType {
	case ValueBoolean:
		return fmt.Sprintf("<Boolean: %t>", v.boolVal)
	case ValueInteger:
		return fmt.Sprintf("<Integer: %d>", v.intVal)
	case ValueString:
		return fmt.Sprintf("<String: %s>", v.stringVal)
	case ValueNone:
		return "<none>"
	}
	return "<unknown>"
}

func (loopTyp LoopType) String() string {
	switch loopTyp {
	case LoopForever:
		return "forever"
	case LoopTimes:
		return "times"
	case LoopWhile:
		return "while"
	}
	return "uknown loop"
}

type Statement struct {
	stmtType StatementType

	assignmentStmt *AssignmentStatement
	ifStmt         *IfStatement
	loopStmt       *LoopStatement
	printStmt      *PrintStatement
}

type LoopStatement struct {
	loopType LoopType

	intExpression  *IntExpression
	boolExpression *BoolExpression
	stmtList       []*Statement
}

type IfStatement struct {
	boolExpression *BoolExpression
	stmtList       []*Statement
	elsifList      []*ElseIf
	elseStmtList   []*Statement
}

type ElseIf struct {
	boolExpression *BoolExpression
	stmtList       []*Statement
}

type AssignmentStatement struct {
	identifier string
	exprn      *Expression
}

type PrintStatement struct {
	exprn *StringExpression
}

type Expression struct {
	exprnType ExpressionType

	intExpression    *IntExpression
	boolExpression   *BoolExpression
	stringExpression *StringExpression
}

//<bool-expression>::=<bool-term>{<or><bool-term>}
//<bool-term>::=<bool-factor>{<and><bool-factor>}
//<bool-factor>::=<bool-constant>|<bool-identifier>|<not><bool-factor>|(<bool-expression>)|<int-comparison>
//<int-comparison>::=<int-expression><int-comp><int-expression>

type BoolExpression struct {
	boolOrTerms []*BoolTerm
}

type BoolTerm struct {
	boolAndFactors []*BoolFactor
}

type BoolFactorType int

const (
	BoolFactorConst BoolFactorType = iota
	BoolFactorId
	BoolFactorNot
	BoolFactorBracket
	BoolFactorIntComparison
)

type BoolFactor struct {
	boolFactorType BoolFactorType

	boolConst      bool
	boolIdentifier string
	notBoolFactor  *BoolFactor
	bracketedExprn *BoolExpression
	intComparison  IntComparison
}

type IntComparison struct {
	// integer comparisons: <, >, <=, >=, =
	intComparator IntComparatorType

	lhsIntExpression *IntExpression
	rhsIntExpression *IntExpression
}

//<int-expression>::=<int-term>{<plus-or-minus><int-term>}
//<int-term>::=<int-factor>{<times-or-divide><int-factor>}
//<int-factor>::=<int-constant>|<int-identifier>|<minus><int-factor>|(<int-expression>)

type IntExpression struct {
	plusTerms  []*IntTerm
	minusTerms []*IntTerm
}

type IntTerm struct {
	timesFactors  []*IntFactor
	divideFactors []*IntFactor
}

type IntFactorType int

const (
	IntFactorConst IntFactorType = iota
	IntFactorId
	IntFactorMinus
	IntFactorBracket
)

type IntFactor struct {
	intFactorType IntFactorType

	intConst       int
	intIdentifier  string
	minusIntFactor *IntFactor
	bracketedExprn *IntExpression
}

// <string-expression> ::= <str-term> {<binary-str-operator> <str-term>}
// <string-term> ::= <string-literal> | <identifier> | str(<expression>) | <lparen><string-expression><rparen>

type StringExpression struct {
	addTerms []*StringTerm
}

type StringTermType int

const (
	StringTermValue = iota
	StringTermId
	StringTermBracket
	StringTermStringedExprn
)

type StringTerm struct {
	strTermType StringTermType

	strVal         string
	identifier     string
	bracketedExprn *StringExpression
	stringedExprn  *Expression
}
