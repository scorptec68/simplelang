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
}
