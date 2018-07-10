package simple_language

import (
	"errors"
	"go/parser"
	"golang.org/x/tools/go/gcimporter15/testdata"
	"strconv"
)

type ValueType int
type StatementType int
type LoopType int
type ExpressionType int
type IntExpressionType int
type IntOperatorType int
type BoolExpressionType int
type BoolOperatorType int
type StringExpressionType int
type StringOperatorType int

const (
	ValueInteger ValueType = iota
    ValueString
    ValueBoolean
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
	BoolExprnValue BoolExpressionType = iota
	BoolExprnBinary
	BoolExprnUnary
)

const (
	BoolUnaryOpNot IntOperatorType = iota
	BoolBinaryOp
	BoolBinaryOpOr
	BoolBinaryOpAnd
)

const (
	StringExprnValue StringExpressionType = iota
	StringExprnBinary
)

const (
	StringBinaryOpAdd StringOperatorType = iota
	StringBinaryOpMinus
)

type Program struct {
	variables *Variables
	stmtList []Statement
}

type Variables struct {
	values map[string]*Value
}

type Parser struct {
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

func (parser *Parser) ParseProgram() (*Program, error) {
	var err error
	prog := new(Program)
	prog.variables, err = parser.parseVariables()
	if err != nil {
		return nil, err
	}
	item := parser.nextItem()
	if item.typ != itemRun {
	    err := errors.New("Missing Run keyword")
	    return nil, err
	}

	prog.stmtList, err = parser.parseStementList()
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

func (parser *Parser) parseVariables() (*Variables, error) {
    vars := new(Variables)
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
			item = parser.nextItem()
			if item.typ != itemColon {
				err := errors.New("Expecting colon")
				return nil, err
			}
			value, err := parser.parseValue()
			if err != nil {
				return nil, err
			}
	        vars.values[idStr] = value
		}
	}
}

func (parser *Parser) parseValue() (*Value, error) {
	var value Value
	item := parser.nextItem()
	if !(item.typ == itemString || item.typ == itemBoolean || item.typ == itemInteger) {
		err := errors.New("Expecting a variable type")
		return nil, err
	}
	switch item.typ {
	case itemString:
		value = Value{valueType: ValueString, stringVal: item.val}
	case itemInteger:
		i, _ := strconv.Atoi(item.val)
		value = Value{valueType: ValueInteger, intVal: i}
	case itemBoolean:
		b, _ := strconv.ParseBool(item.val)
		value = Value{valueType: ValueBoolean, boolVal: b}
	}
	return &value, nil
}

func isEndKeyword(i item) bool {
	return i.typ == itemEndRun || i.typ == itemEndLoop || i.typ == itemEndIf;

}

func (parser *Parser) parseStatementList() ([]Statement, error) {
	stmtList := []Statement{}
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

type Value struct {
	valueType ValueType

	intVal int
	stringVal string
	boolVal bool
}

type Statement struct {
	stmtType StatementType

	loopStmt LoopStatement
	ifStmt   IfStatement
	assignmentStmt AssignmentStatement
	printStmt PrintStatement
}

type LoopStatement struct {
	loopType LoopType

	intExpression IntegerExpression
	boolExpression BoolExpression
	stmtList []Statement
}

type IfStatement struct {
	boolExpression BoolExpression
	stmtList []Statement
    elsifList []ElseIf
    elseStmt Statement
}

type ElseIf struct {
	boolExpression BoolExpression
	stmtList []Statement
}

type AssignmentStatement struct {
	identifier string
	exprn Expression
}

type PrintStatement struct {
	exprn StringExpression
}

type Expression struct {
	exprnType ExpressionType

	identifier string
	intExpression IntegerExpression
	boolExpression BoolExpression
	stringExpression StringExpression
}

type IntegerExpression struct {
	intExprnType IntExpressionType

    intVal int
    lhsExprn IntegerExpression
    rhsExprn IntegerExpression
    operator IntOperatorType
}

type BoolExpression struct {
	boolExprnType BoolExpressionType

	boolValue bool
	lhsExprn BoolExpression
	rhsExprn BoolExpression
	operator BoolOperatorType
}

type StringExpression struct {
	strExprnType StringExpressionType

	strVal string
	lhsExprn StringExpression
	rhsExprn StringExpression
	operator StringOperatorType
}
