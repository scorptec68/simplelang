package simple_language

import (
)

type Program struct {
	variables map[string]Value
	stmtList []Statement
}

type Value struct {
	valueType ValueType
	intVal int
	stringVal string
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
    elseStmt ElseStatement
}
