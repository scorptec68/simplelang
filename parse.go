package simple_language

import (
	"errors"
	"strconv"
	"fmt"
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
	stmtList []*Statement
}

type Variables struct {
	values map[string]*Value
}

type Parser struct {
	variables *Variables

	lex *lexer
	token     [3]item // three-token lookahead for parser.
	peekCount int
}

// nextItem returns the nextItem token.
func (p *Parser) nextItem() item {
	if p.peekCount > 0 {
		p.peekCount--
	} else {
		p.token[0] = p.lex.nextItem()
	}
	return p.token[p.peekCount]
}

func (parser *Parser) match(itemTyp itemType) (err error) {
	item := parser.nextItem()
	if item.typ != itemTyp {
		return fmt.Errorf("Expecting %s", itemTyp
	}
    return nil
}

// backup backs the input stream up one token.
func (p *Parser) backup() {
	p.peekCount++
}

// backup2 backs the input stream up two tokens.
// The zeroth token is already there.
func (p *Parser) backup2(t1 item) {
	p.token[1] = t1
	p.peekCount = 2
}

// backup3 backs the input stream up three tokens
// The zeroth token is already there.
func (p *Parser) backup3(t2, t1 item) { // Reverse order: we're pushing back.
	p.token[1] = t1
	p.token[2] = t2
	p.peekCount = 3
}

// peek returns but does not consume the nextItem token.
func (p *Parser) peek() item {
	if p.peekCount > 0 {
		return p.token[p.peekCount-1]
	}
	p.peekCount = 1
	p.token[0] = p.lex.nextItem()
	return p.token[0]
}

func printIndent(indent int) {
	for indent > 0 {
		fmt.Print("  ")
		indent--
	}
}

func PrintProgramIndent(prog *Program, indent int) {
	printIndent(indent)
	fmt.Printf("Program\n")
	PrintVariables(prog.variables, indent + 1)
	PrintStatementList(prog.stmtList, indent + 1)
}

func PrintVariables(vars *Variables, indent int) {
	printIndent(indent)
	fmt.Printf("Variables\n")
	fmt.Printf("%v", vars.values)
}

func PrintStatementList(stmtList []*Statement, indent int) {
	printIndent(indent)
    fmt.Printf("StatementList\n")
    for _, stmt := range stmtList {
    	PrintStatement(stmt, indent + 1)
	}
}

func PrintOneStatement(stmt *Statement, indent int) {
	printIndent(indent)
	fmt.Printf("Statement\n")

	switch stmt.stmtType {
	case StmtAssignment:
		PrintAssignementStmt(stmt.assignmentStmt, indent + 1)
	case StmtIf:
		PrintIfStmt(stmt.ifStmt, indent + 1)
	case StmtLoop:
		PrintLoopStmt(stmt.loopStmt, indent + 1)
	case StmtPrint:
		PrintPrintStmt(stmt.printStmt, indent + 1)
	}
}

func PrintAssignmentStmt(assign AssignmentStatement, indent int) {
	printIndent(indent)
	fmt.Printf("Assignment\n")

	printIndent(indent)
	fmt.Printf("lhs var = %s\n", assign.identifier)
	PrintExpression(assign.exprn, indent + 1)
}

func PrintExpression(exprn *Expression, indent int) {

}

func (parser *Parser) ParseProgram() (prog *Program, err error) {
	prog.variables, err = parser.parseVariables()
	if err != nil {
		return nil, err
	}
	parser.variables = prog.variables

	item := parser.nextItem()
	if item.typ != itemRun {
	    err := errors.New("Missing Run keyword")
	    return nil, err
	}

	prog.stmtList, err = parser.parseStatementList()
	if err != nil {
		return nil, err
	}

	item = parser.nextItem()
	if item.typ != itemEndRun {
		err := errors.New("Missing EndRun keyword")
		return nil, err
	}
	return prog, nil
}

func (parser *Parser) parseVariables() (vars *Variables, err error) {
    vars.values = make(map[string]*Value)

    item := parser.nextItem()
    if item.typ != itemVar {
    	// no variables to process
		parser.backup()
    	return vars, nil
	}

	// we have potentially some variables (could be empty)
	for {
		item = parser.nextItem()
		switch item.typ {
		case itemEndVar:
			// end of variable declaration
			return vars, nil
		case itemEOF:
			// end of any input which is an error
			err := errors.New("Cannot find EndVar")
			return nil, err
		case itemIdentifier:
            idStr := item.val

            err = parser.match(itemColon)
            if err != nil {
            	return nil, err
			}

			value, err := parser.parseValue()
			if err != nil {
				return nil, err
			}
	        vars.values[idStr] = value
		default:
			return nil, fmt.Errorf("Unexpected type: %s", item.typ)
		}
	}
	return vars, err
}

func (parser *Parser) parseValue() (value *Value, err error) {
	item := parser.nextItem()
	if !(item.typ == itemString || item.typ == itemBoolean || item.typ == itemInteger) {
		err := errors.New("Expecting a variable type")
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

func isEndKeyword(i item) bool {
	return i.typ == itemEndRun || i.typ == itemEndLoop || i.typ == itemEndIf;

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
		if isEndKeyword(item) {
			return stmtList, nil
		}
	}
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
	/*
	case itemIf:
		stmtType = StmtIf
		ifStmt, err = parser.parseIfStatement()
	case itemLoop:
		stmtType = StmtLoop
		loopStmt, err = parser.parseLoopStatement()
	case itemPrint:
		stmtType = StmtPrint
		printStmt, err = parser.parsePrintStatement()
	*/
	}

	return &Statement{stmtType, assignStmt, ifStmt, loopStmt, printStmt}, err
}

// Note: other parsers use panic/recover instead of returning an error

func (parser *Parser) parseAssignment() (assign *AssignmentStatement, err error) {
	idItem := parser.nextItem()
	assign.identifier = idItem.val

	opItem := parser.nextItem()
	if opItem.typ != itemEquals {
	    err	= errors.New("Missing expected equals assign")
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
		return assign, nil
	case ValueInteger:
		// TODO
		return assign, nil
	case ValueString:
		// TODO
		return assign, nil
	default:
		return nil, fmt.Errorf("Assignment to undeclared variable: %s", idItem.val)
	}
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
	// process 1st term
	boolTerm, err := parser.parseBoolTerm()
	if err != nil {
		return nil, errors.New("Error parsing boolean term")
	}
	boolExprn.boolOrTerms = append(boolExprn.boolOrTerms, boolTerm)

	// optionally process others
	for parser.peek().typ != itemNewLine {
		item := parser.nextItem()
		if item.typ != itemOr {
			return nil, errors.New("Missing or term")
		}
		boolTerm, err = parser.parseBoolTerm()
		if err != nil {
			return nil, errors.New("Error parsing boolean term")
		}
		boolExprn.boolOrTerms = append(boolExprn.boolOrTerms, boolTerm)
	}
	return boolExprn, nil
}

//<bool-term>::=<bool-factor>{<and><bool-factor>}
func (parser *Parser) parseBoolTerm() (boolTerm *BoolTerm, err error) {
	// process 1st factor
	boolFactor, err := parser.parseBoolFactor()
	if err != nil {
		return nil, errors.New("Error parsing boolean factor")
	}
	boolTerm.boolAndFactors = append(boolTerm.boolAndFactors, boolFactor)

	// optionally process others
	for parser.peek().typ != itemNewLine {
		item := parser.nextItem()
		if item.typ != itemAnd {
			return nil, errors.New("Missing and factor")
		}
		boolFactor, err = parser.parseBoolFactor()
		if err != nil {
			return nil, errors.New("Error parsing boolean term")
		}
		boolTerm.boolAndFactors = append(boolTerm.boolAndFactors, boolFactor)
	}
    return boolTerm, err
}

//<bool-factor>::=<bool-constant>|<not><bool-factor>|(<bool-expression>)|<int-comparison>
func (parser *Parser) parseBoolFactor() (boolFactor *BoolFactor, err error) {
    item := parser.nextItem()
    switch item.typ {
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
			return nil, errors.New("Not missing factor")
		}
		boolFactor.notBoolFactor = factor
	case itemLeftParen:
		boolFactor.boolFactorType = BoolFactorBracket
		exprn, err := parser.parseBoolExpression()
		if err != nil {
			return nil, errors.New("Can not process bracketed expression")
		}
		boolFactor.bracketedExprn = exprn
		item = parser.nextItem()
		if item.typ != itemRightParen {
			return nil, errors.New("Missing right parenthesis on exxpression")
		}
	default:
		return nil, errors.New("Invalid boolean factor")
	}
	return boolFactor, nil
}

/*
func (parser *Parser) parseIfStatement() (ifStmt *IfStatement, err error) {
	var elseifList []*ElseIf
	var elseStmtList []*Statement

    exprn, err := parser.parseBoolExpression()
    if err != nil {
    	return nil, err
	}

    stmtList, err := parser.parseStatementList()
	if err != nil {
		return nil, err
	}

	// test for next optional parts or the end
    item := parser.nextItem()

    // optional elseif
    if item.typ == itemElseIf {
		elseifList, err = parser.parseElseIfList()
		if err != nil {
			return nil, err
		}
		item = parser.nextItem()
	}

	// optional else
	if item.typ == itemElse {
		elseStmtList, err = parser.parseElse()
		if err != nil {
			return nil, err
		}
		item = parser.nextItem()
	}

	// mandatory endif
	if item.typ != itemEndIf {
	    err = errors.New("Missing endif")
	    return nil, err
	}

	return &IfStatement{exprn, stmtList, elseifList, elseStmtList}, nil
}
*/

type Value struct {
	valueType ValueType

	intVal int
	stringVal string
	boolVal bool
}

type Statement struct {
	stmtType StatementType

	assignmentStmt *AssignmentStatement
	ifStmt   *IfStatement
	loopStmt *LoopStatement
	printStmt *PrintStatement
}

type LoopStatement struct {
	loopType LoopType

	intExpression *IntExpression
	boolExpression *BoolExpression
	stmtList []*Statement
}

type IfStatement struct {
	boolExpression *BoolExpression
	stmtList []*Statement
    elsifList []*ElseIf
    elseStmtList []*Statement
}

type ElseIf struct {
	boolExpression BoolExpression
	stmtList []*Statement
}

type AssignmentStatement struct {
	identifier string
	exprn *Expression
}

type PrintStatement struct {
	exprn *StringExpression
}

type Expression struct {
	exprnType ExpressionType

	intExpression *IntExpression
	boolExpression *BoolExpression
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

    boolConst bool
    boolIdentifier string
    notBoolFactor *BoolFactor
    bracketedExprn *BoolExpression
    intComparison IntComparison
}

type IntComparison struct {
	// integer comparisons: <, >, <=, >=, =
	intComparitor IntComparatorType

	lhsIntExpression *IntExpression
	rhsIntExpression *IntExpression
}

//<int-expression>::=<int-term>{<plus-or-minus><int-term>}
//<int-term>::=<int-factor>{<times-or-divide><int-factor>}
//<int-factor>::=<int-constant>|<int-identifier>|<minus><int-factor>|(<int-expression>)

type IntExpression struct {
    plusTerms []*IntTerm
    minusTerms []*IntTerm
}

type IntTerm struct {
    timesFactors []*IntFactor
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

    intConst int
    intIdentifier string
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

	strVal string
	identifier string
	bracketedExprn *StringExpression
	stringedExprn *Expression
}
