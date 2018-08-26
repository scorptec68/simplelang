package simple_language

type Interpreter struct {
	variables *Variables
}

// InterpProgram Interprets the program aka runs the program
// prog - the program parse tree to run
func (interp *Interpreter) InterpProgram(prog *Program) (err error) {
	interp.variables = prog.variables
	err = interp.interpStatementList(prog.stmtList)
	if err != nil {
		return err
	}
	return nil
}

func (interp *Interpreter) interpStatementList(stmtList []*Statement) (err error) {
	for i, stmt := range stmtList {
		err = interp.interpStatement(stmt)
		if err != nil {
			return err
		}
	}
	return nil
}

func (interp *Interpreter) interpStatement(stmt *Statement) (err error) {
	err = nil
	switch stmt.stmtType {
	case StmtAssignment:
		err = interp.interpAssignmentStmt(stmt.assignmentStmt)
	case StmtIf:
		err = interp.interpIfStmt(stmt.ifStmt)
	case StmtLoop:
		err = interp.interpLoopStmt(stmt.loopStmt)
	case StmtPrint:
		err = interp.interpPrintStmt(stmt.printStmt)
	}
	return err
}

func (interp *Interpreter) interpAssignmentStmt(assign *AssignmentStatement) (err error) {
	value, err := interp.interpExpression(assign.exprn)
	if err != nil {
		return err
	}
	interp.variables.values[assign.identifier] = value
	return nil
}

func (interp *Interpreter) interpExpression(exprn *Expression) (val *Value, err error) {
	val = new(Value)
	switch exprn.exprnType {
	case ExprnBoolean:
		val.valueType = ValueBoolean
		val.boolVal, err = interp.interpBoolExpression(exprn.boolExpression)
	case ExprnInteger:
		val.valueType = ValueInteger
		val.intVal, err = interp.interpIntExpression(exprn.intExpression)
	case ExprnString:
		val.valueType = ValueString
		val.intVal, err = interp.interpStringExpression(exprn.stringExpression)
	}
	if err != nil {
		return nil, err
	}
	return val, nil
}

func (interp *Interpreter) interpBoolExpression(boolExprn *BoolExpression) (val bool, err error) {
	for i, term := range boolExprn.boolOrTerms {
		val, err = interp.interpBoolTerm(term)
		if err != nil {
			return false, err
		}
		if val {
			return true, nil
		}
	}
	return false, nil
}

func (interp *Interpreter) interpBoolTerm(boolTerm *BoolTerm) (val bool, err error) {
	for i, factor := range boolTerm.boolAndFactors {
		val, err = interp.interpBoolFactor(factor)
		if err != nil {
			return false, err
		}
		if !val {
			return false, nil
		}
	}
	return true, nil
}

func (interp *Interpreter) interpBoolFactor(boolFactor *BoolFactor) (val bool, err error) {
	switch boolFactor.boolFactorType {
	case BoolFactorConst:
		return boolFactor.boolConst, nil
	case BoolFactorNot:
		val, err = interp.interpBoolFactor(boolFactor.notBoolFactor)
		return !val, err
	case BoolFactorBracket:
		return interp.interpBoolExpression(boolFactor.bracketedExprn)
	case BoolFactorId:
		value := interp.variables.values[boolFactor.boolIdentifier]
		return value.boolVal, nil
	case BoolFactorIntComparison:
		return interp.interpIntComparison(boolFactor.intComparison)
	}
	return false, nil
}

func (interp *Interpreter) interpIntComparison(intComparison *IntComparison) (bool, error) {
	lhs, err := interp.interpIntExpression(intComparison.lhsIntExpression)
	if err != nil {
		return false, err
	}
	rhs, err := interp.interpIntExpression(intComparison.lhsIntExpression)
	if err != nil {
		return false, err
	}
	switch intComparison.intComparator {
	case IntCompEquals:
		return lhs == rhs, nil
	case IntCompGreaterEquals:
		return lhs >= rhs, nil
	case IntCompGreaterThan:
		return lhs > rhs, nil
	case IntCompLessEquals:
		return lhs <= rhs, nil
	case IntCompLessThan:
		return lhs < rhs, nil
	}
	return false, nil
}

func (interp *Interpreter) interpIntExpression(intExpression *IntExpression) (int, error) {
	val := 0
	for i, term := range intExpression.plusTerms {
		plusVal, err := interp.interpIntTerm(term)
		if err != nil {
			return 0, err
		}
		val += plusVal
	}
	for i, term := range intExpression.minusTerms {
		minusVal, err := interp.interpIntTerm(term)
		if err != nil {
			return 0, err
		}
		val -= minusVal
	}
	return val, nil
}

func (interp *Interpreter) interpIntTerm(intTerm *IntTerm) (int, error) {
	val := 1
	for i, factor := range intTerm.timesFactors {
		timesVal, err := interp.interpIntFactor(factor)
		if err != nil {
			return 1, err
		}
		val *= timesVal
	}
	for i, factor := range intTerm.divideFactors {
		divideVal, err := interp.interpIntFactor(factor)
		if err != nil {
			return 1, err
		}
		val /= divideVal
	}
	return val, nil
}

func (interp *Interpreter) interpIntFactor(intFactor *IntFactor) (int, error) {
	switch intFactor.intFactorType {
	case IntFactorConst:
		return intFactor.intConst, nil
	case IntFactorBracket:
		return interp.interpIntExpression(intFactor.bracketedExprn)
	case IntFactorId:
		value := interp.variables.values[intFactor.intIdentifier]
		return value.intVal, nil
	case IntFactorMinus:
		value, err := interp.interpIntFactor(intFactor.minusIntFactor)
		return -value, nil
	}
	return 0, nil
}
