package simple_language

import (
	"errors"
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
	values map[string]Value
}

type Parser struct {
	lex *lexer
}

func (parser *Parser) ParseProgram() (prog *Program, err error) {
	prog = new(Program)
	prog.variables, err = parser.parseVariables()
	if err != nil {
		return
	}
	prog.stmtList, err = parser.parseStementList()
	if err != nil {
		return
	}
	return
}

func (parser *Parser) parseVariables() (vars *Variables, err error) {
	err = nil
    vars = new(Variables)
    vars.values = make(map[string]Value)
    item := parser.lex.nextItem()
    if item.typ != itemVar {
    	// no variables to process
    	return
	}
	// we have potentially some variables (could be empty)
	for {
		item = parser.lex.nextItem()
		switch item.typ {
		case itemEndVar:
			// end of variable declaration
			return
		case itemEOF:
			// end of any input which is an error
			err = errors.New("Cannot find EndVar")
			return
		case itemIdentifier:
            str := item.val
			item = parser.lex.nextItem()
			if item.typ != itemColon {
				err = errors.New("Expecting colon")
				return
			}
			item = parser.lex.nextItem()
			if !(item.typ == itemString || item.typ == itemBoolean || item.typ == itemInteger) {
				err = errors.New("Expecting a variable type")
				return
			}
			return

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
