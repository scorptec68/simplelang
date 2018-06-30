package simple_language

import (
	"fmt"
	"strings"
	"unicode"
	"unicode/utf8"
)

// item represents a token or text string returned from the scanner.
type item struct {
	typ  itemType // The type of this item.
	pos  Pos      // The starting position, in bytes, of this item in the input string.
	val  string   // The value of this item.
	line int      // The line number at the start of this item.
}

func (i item) String() string {
	switch {
	case i.typ == itemEOF:
		return "EOF"
	case i.typ == itemError:
		return i.val
	case i.typ > itemKeyword:
		return fmt.Sprintf("<%s>", i.val)
	case len(i.val) > 10:
		return fmt.Sprintf("%.10q...", i.val)
	}
	return fmt.Sprintf("%q", i.val)
}

// itemType identifies the type of lex items.
type (
	itemType int
	Pos int
	processResult int
)

const (
	resultMatch    processResult = iota
	resultNoMatch
	resultMatchError
)

const (
	itemError        itemType = iota // error occurred; value is text of error
	itemIntegerLiteral // integer value
	itemStringLiteral  // quotable string
	itemTrue          // true
	itemFalse         // false
	itemEquals        // '='
	itemNotEquals     // '#'
	itemLessEquals    // '<='
	itemGreaterEquals // '>='
	itemLessThan   // '<'
	itemGreaterThan // '>'
	itemAnd        // '&'
	itemOr         // '|'
	itemNot        // '~'
	itemPlus       // '+'
	itemMinus      // '-'
	itemTimes      // '*'
	itemDivide     // '/'
	itemLeftParen  // '('
	itemRightParen // ')'
	itemColon      // ':'
	itemEOF
	itemIdentifier // alphanumeric identifier
	itemSingleSpace  // ' '
	itemWhiteSpace   // run of spaces separating arguments
	// Keywords appear after all the rest.
	itemKeyword  // used only to delimit the keywords
	itemIf       // if keyword
	itemElse     // else keyword
	itemElseIf   // elseif keyword
	itemEndIf    // endif keyword
	itemLoop     // loop keyword
	itemEndLoop  // endloop keyword
	itemPrint    // print keyword
	itemStr      // str keyword
	itemBoolean  // boolean keyword
	itemString   // string keyword
	itemInteger  // integer keyword
)

var keywords = map[string]itemType{
	"if":       itemIf,
	"else":     itemElse,
	"elseif":   itemElseIf,
	"endif":    itemEndIf,
	"loop":     itemLoop,
	"endloop":  itemEndLoop,
	"print":    itemPrint,
	"str":      itemStr,
	"boolean":  itemBoolean,
	"string":   itemString,
	"integer":  itemInteger,
	"true":     itemTrue,
	"false":    itemFalse,
}

var symbols = map[string]itemType {
	"<":        itemLessThan,
	">":        itemGreaterThan,
	"<=":       itemLessEquals,
	">=":       itemGreaterEquals,
	"=":        itemEquals,
	"+":        itemPlus,
	"-":        itemMinus,
	"~":        itemNot,
	"#":        itemNotEquals,
	"*":        itemTimes,
	"/":        itemDivide,
	"&":        itemAnd,
	"|":        itemOr,
	":":        itemColon,
	"(":        itemLeftParen,
	")":        itemRightParen,
}

type processFn func(*lexer) processResult

const eof = -1

const (
	spaceChars      = " \t\r\n" // These are the space characters defined by Go itself.
)

// lexer holds the state of the scanner.
type lexer struct {
	name       string    // the name of the input; used only for error reports
	input      string    // the string being scanned
	pos        Pos       // current position in the input
	start      Pos       // start position of this item
	width      Pos       // width of last rune read from input
	items      chan item // channel of scanned items
	parenDepth int       // nesting depth of ( ) exprs
	line       int       // 1+number of newlines seen
}

//--------------------------------------------------------------------------------------------
// core rune processing

// next returns the next rune in the input.
func (l *lexer) next() rune {
	if int(l.pos) >= len(l.input) {
		l.width = 0
		return eof
	}
	r, w := utf8.DecodeRuneInString(l.input[l.pos:])
	l.width = Pos(w)
	l.pos += l.width
	if r == '\n' {
		l.line++
	}
	return r
}

// peek returns but does not consume the next rune in the input.
func (l *lexer) peek() rune {
	r := l.next()
	l.backup()
	return r
}

// backup steps back one rune. Can only be called once per call of next.
func (l *lexer) backup() {
	l.pos -= l.width
	// Correct newline count.
	if l.width == 1 && l.input[l.pos] == '\n' {
		l.line--
	}
}

// ignore skips over the pending input before this point.
func (l *lexer) ignore() {
	l.start = l.pos
}

//--------------------------------------------------------------------------------------------

// accept consumes the next rune if it's from the valid set.
func (l *lexer) accept(valid string) bool {
	if strings.ContainsRune(valid, l.next()) {
		return true
	}
	l.backup()
	return false
}

func (l *lexer) acceptNot(valid string) bool {
	if !strings.ContainsRune(valid, l.next()) {
		return true
	}
	l.backup()
	return false
}

// acceptRun consumes a run of runes from the valid set.
func (l *lexer) acceptRun(valid string) {
	for strings.ContainsRune(valid, l.next()) {
	}
	l.backup()
}

func (l *lexer) acceptNotRun(valid string) bool {
	for !strings.ContainsRune(valid, l.next()) {
		if l.width == 0 {
			// reached eof
			return false
		}
	}
	l.backup()
	return true
}

//--------------------------------------------------------------------------------------------
// items channel functions

// emit passes an item back to the client.
func (l *lexer) emit(t itemType) {
	l.items <- item{t, l.start, l.input[l.start:l.pos], l.line}
	l.start = l.pos
}


// errorf returns an error token
func (l *lexer) errorf(format string, args ...interface{}) {
	l.items <- item{itemError, l.start, fmt.Sprintf(format, args...), l.line}
}

// nextItem returns the next item from the input.
// Called by the parser, not in the lexing goroutine.
func (l *lexer) nextItem() item {
	return <-l.items
}

// drain drains the output so the lexing goroutine will exit.
// Called by the parser, not in the lexing goroutine.
func (l *lexer) drain() {
	for range l.items {
	}
}

//--------------------------------------------------------------------------------------------

// lex creates a new scanner for the input string.
func lex(name, input string) *lexer {
	l := &lexer{
		name:       name,
		input:      input,
		items:      make(chan item),
		line:       1,
	}
	go l.run()
	return l
}



/*
 * Eat up any whitespace to the next non-whitespace
 * Return true if found a non-whitespace rune to process
 * Return false if didn't find anything and reached EOF
 */
func processWhitespace(l *lexer) bool {
	for {
		rune := l.next()
		if rune == eof {
			return false
		}
		if !strings.ContainsRune(spaceChars, rune) {
			l.backup()
			l.ignore()
			return true
		}
	}
}

/*
 * Process a comment
 * Return true if successfully processed a comment
 * Return false if this was not a comment and other processing should be done.
 */
func processComment(l *lexer) processResult {
	rune := l.next()
	if rune == '/' {
		rune := l.next();
		if rune == '/' {
			// eat up until \n
			for l.next() != '\n' {
			}
			l.ignore()
			return resultMatch
		} else {
			l.backup()
			l.backup()
			return resultNoMatch
		}
	} else {
		l.backup()
		return resultNoMatch
	}
}

/*
 * Process a symbol/operator of 1 or 2 runes
 * Return true if got a match and emitted symbol
 * Return false otherwise
 */
func processSymbol(l *lexer) processResult {
	rune1 := l.next()
    rune2 := l.next()
    key1 := string(rune1)
    key12 := string(rune1) + string(rune2)
    if item, ok := symbols[key12]; ok {
        l.emit(item)
        return resultMatch
	} else if item,ok := symbols[key1]; ok {
		l.backup()
		l.emit(item)
		return resultMatch
	}
	// no 1 or 2 char symbol matches
	l.backup()
	l.backup()
	return resultNoMatch
}

func processStringLiteral(l *lexer) processResult {
    rune := l.next()
    if rune == '"' {
    	// now look for matching "
    	if l.acceptNotRun("\"") {
			l.emit(itemStringLiteral)
			return resultMatch
		} else {
    		l.errorf("Could not find string terminstor")
    		return resultMatchError
		}
	}
	return resultNoMatch
}

func processNumericLiteral(l *lexer) processResult {
	r := l.next()
	if r != '+' && r != '-' && !('0' <= r && r <= '9') {
	    l.backup()
	    return resultNoMatch
	}

	// Optional leading sign.
	l.accept("+-")

	// Is it hex?
	digits := "0123456789"
	if l.accept("0") && l.accept("xX") {
		digits = "0123456789abcdefABCDEF"
	}
	l.acceptRun(digits)

	// Next thing mustn't be alphanumeric.
	if isAlphaNumeric(l.peek()) {
		l.next()
		l.errorf("bad number syntax: %q", l.input[l.start:l.pos])
		return resultMatchError
	}
	l.emit(itemIntegerLiteral)
	return resultMatch
}

func processKeyword(l *lexer) processResult {
	// extract word up to space or end of line
	// test word in the keywords list
	return resultNoMatch
}

func processIdentifier(l *lexer) processResult {
	// extract word up to space or end of line
	// test word in the keywords list
	return resultNoMatch
}


var processFunctions = []processFn{processComment, processSymbol, processStringLiteral, processNumericLiteral,
                                   processKeyword, processIdentifier}

// run runs the state machine for the lexer.
func (l *lexer) run() {
	for {
		if !processWhitespace(l) {
			break
		}
		found := false
		for _, processFunc := range processFunctions {
		    result := processFunc(l)
		    switch result {
			case resultMatch:
				found = true
				break
			case resultMatchError:
				break
			}
		}
		if !found {
			l.errorf("Invalid token")
			break
		}
	}
	close(l.items)
}

// isSpace reports whether r is a space character.
func isSpace(r rune) bool {
	return r == ' ' || r == '\t'
}

// isEndOfLine reports whether r is an end-of-line character.
func isEndOfLine(r rune) bool {
	return r == '\r' || r == '\n'
}

// isAlphaNumeric reports whether r is an alphabetic, digit, or underscore.
func isAlphaNumeric(r rune) bool {
	return r == '_' || unicode.IsLetter(r) || unicode.IsDigit(r)
}
