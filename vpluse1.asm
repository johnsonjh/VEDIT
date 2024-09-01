	.PAGE
	.TITLE	'E1'
;************************************************
;*						*
;*	Expression Evaluator Routines		*
;*						*
;************************************************
;
; Copyright (C) 1987 by CompuView Products, Inc.
;			1955 Pauline
;			Ann Arbor, MI 48103
;
;	Written by: Tom Burt from TINY BASIC
;
;	Last Change: Tom - Aug 20, 1985 - VGET:  .vm = BLMVEN
;			 - Oct. 3, 1985 - TSTCTL (.c=CTRLZ) bug
;			   Mar. 9, 1986 - Routines moved in from C3:
;		           EVLSTR,STQVAL,STCHL,VALCHL,VALHL,VALPNT,LDCHL
;			 - Mar 24, 1986 - XPRBC added (@HL points to string)
;					- ENXCHR, ENXSCN, EGTSCN
;			 - Apr 11 - NUMFLG renamed XNMFLG, RMINUS renamed XRMINU
;				    PNDFLG reference deleted
;			 - Apr. 24 - Bugs from ENXCHR etc. change
;				   - GETNUM return HL-> immediate expression delimiter
;				     even if a space.
;			 - May  08 - YGET, opt
;			 - June 02 - TGET (new) for next tab pos
;			 - Aug. 20 - WE-> EW, WV, WH for window coordinates
;			   Sept 05 - EVLSTR, 8086 bug
;		     Ted - Oct. 13 - EFGET uses INFLCH()
;		     Tom - Dec. 02 - WDGET(), RTGET(), LMGET()
;		     Ted - Mar. 31, 1987 - Added .wn, .wt, .wz, .ml, .,mr, .ms
;
;**************************************************************************
;
; Expression evaluator routines, taken from Li-Chen Wang's Tiny Basic 1976
;	Adapted to run under Vedit by Tom Burt, CompuView, April, 1984.
;	Remainder operator (%) & logical operators &, ^, ~
;
; Uses 24-bit arithmetic for addition and subtraction.  Returns 24-bit
;	value in CHL (C is merely the sign).  Returns (OVFERR) nonzero if
;	overflow occurred during evaluation.
;
;	Multiplication/division/remainder overflow in 16 bits.  Addition
;	and subtraction don't really overflow until 24 bits are used, but
;	(OVFERR) is set on return if the sign is not all 1's or 0's.
;
;***************************************************************************
;
; GETNUM - Return BC = |(CHL)| = |expession e| from string @ES:HL, default = 0.
;	   Return HL -> delimiting character.
;	   DE saved.
;
				;{CMDNUM,GOLINE,VSTPAR,DOTCMD}
ATOI:
;6	PUSH	DS		;Use DS: buffer
;6	POP	ES
				;{GETDEC}
	BPROC	GETNUM
GETNUM:	PUSH	D
	STES	EVLGET,H	;Set string pointer
	CALL	EXPR		;CHL = expession value e
	LXI	D,CHL		;Store into (CHL)
	CALL	STCHL
	CALL	CHKSGN		;HL = |e|
	MOV	B,H
	MOV	C,L		;BC = |e|

	LES	H,EVLGET	;HL-> past delimiter
	DCX$	H		;HL-> recognized delimiter
	MVI	A,SPACE

..1:	DCX$	H		;Examine preceeding char
	CMP%ES	M		;Space?
	BEQ	..1		;Yes, keep backing up

	INX$	H		;HL-> expression delimiter
	POP	D		;Restore caller's DE
	RET
	EPROC	GETNUM
;
; EXPR - Evaluates arithmetical & logical expressions pointed to by (EVLGET>.
;	 Returns result in %CHL, (EVLGET) -> delimiter.	[3/24/86]
;
;	 C is all sign bits, all 0's or all 1's, else overflow.
;	 Sets HL zero & (OVFLOW) non zero if overflow occurs.
;	 Temporary overflow allowed in addition and subtraction upto 24 bits.
;
;	 Ignores blanks.  Scan pointer advanced past evaluated characters.
;	 (EVLCHR) contains delimiting character.
;
;	Expressions are integers, including #, and variables, including
;	Q-registers and edit-point offset ".", perhaps combined with
;	arithmetic operators {+,-,*,/,%} and perhaps surrounded and
;	nested with parentheses.  (% is the "remainder" operator).
;
;	Also relational operators are recognized: <e1><relop><e2>.
;		Relops: <, <=, =, <>, >=, >.
;	Returns CHL = 1 for true, 0 for false.
;
;	Also recognizes the logical operators ~, &, ^ (NOT, AND, OR)
;		and the suffix ', the 1's complement operator.
;
;	In the following discussion, En indicates a subroutine call as well
;	as being part of the Bakhaus-Nauer formulation.
;
; <EXPRR>::=  <XPRAND> [^ <XPRAND>]*
; <XPRAND>::= <XPRNOT> [& <XPRNOT>]*
; <XPRNOT>::= {<E1> | ~<E1>}
; <E1>::=     {<E2> | <E2><relop><E2>}
; <E2>::=     [{+,-}] <E3> [{+,-} <E3>]*
; <E3>::=     <E4>    [{*,/,%} <E4>]*
; <E4>::=     { e | e' }  where e is one of:
;		  Numeric register (variable)  {Q0-Q9}		or
;		  Integer					or
;		  (<EXPRR>).
;
; Where { } indicates that exactly one of the set inside must be used.
;	[ ] indicates an optional item.
;	[ ]* indicates an indefinite # of occurrences of optional item.
;	Commas are separators and are not members of any of the above sets.
;
				;{GETNUM, GETDEC, EVLSTR}
EXPR:	CLR	OVFERR		;Clear overflow flag
	STA	XNMFLG
	LXI	H,0000		;Set CHL to 00
	MOV	C,L		;This sets default value to zero
	CALL	ENXSCN		;Get next scan chr (get scan line if needed)
	STA	XRMINU		;Possible numerical sign
;
;	For sake of speed, just return if simple command or control char
;
	CPI	SPACE		;Is it a control char?
	RC			;Yes, just return
	ANI	5FH		;Make upper case
	CPI	'Q'		;Numerical variable?
	BEQ	EXPRR		;Yes, continue with evaluation
	CPI	'A'		;Command?
	BLT	EXPRR		;No, continue with evaluation
	CPI	'Z'+1		;Command?
	RC			;Yes return
;
EXPRR:				;{EXPR4}
	CALL	XPRAND		;CHL = value of 1st expression, e1
;
;	Logical operator OR: ^
;
XPROR:	CALL	EGTSCN		;A = terminator of 1st expression
	CPI	'^'		;Is it the OR operator ^ ?
	RNZ			;No, all done, return CHL = e1
	CALL	ENXSCN		;Yes, advance past the OR operator
	LXI	D,XPRAND
	CALL	NXTBDE		;BDE = 2d expression = e2, CHL = e1
	MOV	A,B
	ORA	C
	MOV	C,A
	MOV	A,D
	ORA	H
	MOV	H,A
	MOV	A,E
	ORA	L
	MOV	L,A		;CHL = e1 OR e2
	JMPR	XPROR		;Look for more OR terms
;
;	Logical operator AND: &
;
XPRAND:	CALL	XPRNOT		;CHL = 1st expression, e1
XPAND1:	CALL	EGTSCN		;A = symbol terminating e1
	CPI	'&'		;Is it the logical operator AND (&) ?
	RNZ			;No, return CHL = e1
	CALL	ENXSCN		;Yes, advance past the LOP &
	LXI	D,XPRNOT
	CALL	NXTBDE		;BDE = 2d expression e2, CHL = e1
	MOV	A,B
	ANA	C
	MOV	C,A
	MOV	A,D
	ANA	H
	MOV	H,A
	MOV	A,E
	ANA	L
	MOV	L,A		;CHL = e1 AND e2
	JMPR	XPAND1		;Check for more AND terms
;
;	Logical operator NOT: ~
;
XPRNOT:	CALL	EGTSCN		;A = current scan character
	CPI	'~'		;Is it the logical operator NOT (~) ?
	BNE	EXPR1		;No, just evaluate the expression
	CALL	ENXSCN		;Yes, advance past the LOP ^
	CALL	EXPR1		;CHL = the expression value e
;	JMP	NOTCHL		;CHL <= NOT CHL.
;
; NOTCHL - Return CHL = not CHL.
;
				;{XPRNOT^,EGET}
NOTCHL:	MOV	A,C
	ORA	H
	ORA	L		;Zero?
	JNZ	ZROCHL		;No, return CHL = 0
	INR	L		;Yes, return CHL = 1
	RET
;
EXPR1:	CALL	EXPR2		;CHL = value of 1st expression, e1
	CALL	CHKREL		;Relational operator(s) coming up?
	RNZ			;No, all done
	PUSH	H		;Save HL, presumes GETEXC saves C of CHL
	LXI	H,RELOPS-1	;HL -> ahead of table of RelOps & addresses
	CALL	GETEXC		;HL -> executive routine for current RELOP
	XTHL			;TOS -> executive routine, HL restored
	LXI	D,NEGXP2
	CALL	NXTBDE		;BDE = -e2, CHL = e1
	CALL	ADD24		;CHL = e1 - e2, flags S,Z set/reset
	MVI	A,1		;For setting CHL = 1 => TRUE, flags still set
	JMP	ZROCHL		;CHL<= 0 = FALSE, return to one of following:
;
;	Relational operator evaluation.
;
XP1GE:	RM			;Greater than or equal:  >= ?
	MOV	L,A		;Yes, set CHL = 1 for TRUE
	RET
;
XP1GT:	RM			;Greater than:  >  ?
XP1NE:	RZ			;Yes if Not Equal: <>
	MOV	L,A		;Set CHL = 1 for TRUE
	RET
;
XP1LE:	MOV	L,A		;Less than or equal:  <=  ?
	RM			;Yes, less than, return TRUE
	RZ			;Yes, equal, return TRUE
	MOV	L,H		;No, CHL <= 0 => FALSE
	RET
;
XP1LT:	RP			;Less than:  <  ?
	MOV	L,A		;Yes, set CHL = 1 for TRUE
	RET
;
XP1EQ:	RNZ			;Equal:  =  ?
	MOV	L,A		;Yes, set CHL = 1 for TRUE
	RET
;
;	Term Processing.
;
;
				;{EXPR1}
NEGXP2:	CALL	EXPR2		;CHL = value of next expression
	JMP	CHGSGN		;Make it negative
				;{EXPR}
EXPR2:	CALL	EGTSCN		;Get current scan char (skipping blanks)
	CPI	'-'		;Negative number?
	BNE	XP21		;No, skip next section
	LXI	H,0000H		;Set <expr1> = 0 and treat as subtraction
	MVI	C,0		;Upper bits of the 24-bit number
	JMPR	XP24		;A = '-'
;
XP21:	CPI	'+'		;Plus sign?
	BNE	XP22		;No
	CALL	ENXSCN		;Yes, skip past it
XP22:	CALL	EXPR3		;CHL <= term value
;
;	Check for optional additional terms.
;
XP23:	CALL	EGTSCN		;Get current scan char (skipping blanks)
	CPI	'+'		;Addition?
	JRNZ	XP24		;No, keep looking
	LXI	D,EXPR3A
	JMPR	XP25
;
XP24:	CPI	'-'		;Subtraction?
	JNZ	XP42		;No, return with current value in CHL
	LXI	D,NEGXP3	;e2 will be negated
XP25:	CALL	NXTBDE		;BDE = e2, CHL = e1
	CALL	ADD24		;CHL = e1 + e2
	JMPR	XP23		;Keep checking for more terms
;
;	Factor Processing, 16-bits, presumes C is 8-bit sign register for HL.
;
				;{XPR23}
NEGXP3:	CALL	EXPR3A		;CHL = next expression
	JMP	CHGSGN		;Negate it
;
				;{XP23,XP24}
EXPR3A:	CALL	ENXSCN		;Advance past sign
				;{EXPR2,EXPR3A^}
EXPR3:	CALL	EXPR4		;CHL = 1st factor f1
XP31:	CALL	EGTSCN		;Get current scan char, skipping blanks
	CPI	'*'		;Multiplication?
	JRNZ	XP34		;No, check for division
	CALL	NXTFAC		;HL= |f1|, DE= |f2|, B= sign of result-to-be
	MOV	A,H		;Is |f1| < 256?
	ORA	A
	JRZ	XP32		;Yes, no problems
	MOV	A,D		;No, then is |f2| < 256?
	ORA	D
	XCHG			;HL = |f2| < 256, so L = |f2|
	JNZ	SETOVF		;Oops, both are greater than 100H, Overflow
XP32:	MOV	A,L		;A = |one factor| < 256
	LXI	H,0000H		;Initialize new product = 0
	ORA	A		;Zero factor?
	JRZ	XP35		;Yes, done already
;
;	Multiplication
;
XP33:	DAD	D		;Multiply by adding one term
	JC	SETOVF		; . . . oops, overflow
	DCR	A		;the # times specified by the other
	JRNZ	XP33
	JMPR	XP35		;Adding done, now go handle signs
;
;	Division
;
XP34:	CPI	'/'		;Division?
	JRNZ	XP36		;No, maybe "remainder."
	CALL	NXTFAC		;HL= |f1|, DE= |f2|, B= sign of result-to-be
	MOV	A,D		;Check for divisor f2 = 0
	ORA	E
	JZ	SETOVF		;Oops, that's a nono
	PUSH	B		;Ok, still going strong, save sign watcher
	CALL	DVHLDE		;BC = new quotient, HL = remainder
	SHLD	LFTOVR		;Save remainder
	MOV	H,B		;Move quotient into HL for division result
	MOV	L,C
	POP	B		;Retrieve sign watcher B
;
;	Sign processing for multiplication and division
;
XP35:	MVI	C,0		;Positive sign for absolute arithmetic
	MOV	A,B		;Check whether result should be negative
	ORA	A
	CM	CHGSGN		;If so, negate CHL
	JMPR	XP31		;Go look for more terms
;
;	Remainder
;
XP36:	CPI	'%'		;Remainder?
	JRNZ	XP42		;No, not a mathematical operator, return HL
	CALL	NXTFAC		;HL= |f1|, DE= |f2|, B= sign of result-to-be
	MOV	A,D		;Check for divisor d2 = 0
	ORA	E
	JZ	SETOVF		;Oops, that's a nono
	PUSH	B		;Ok, still going strong, save sign watcher
	CALL	DVHLDE		;BC = |new quotient|, HL = |remainder|
	SHLD	LFTOVR		;Save remainder
	POP	B		;B = sign of remainder
	JMPR	XP35		;Use common sign processing code
;
;	Next factor processing for */%
;	Input:  CHL = f1.
;	Returns HL = |f1|, DE = |f2|, B = sign of result-to-be.
;
NXTFAC:	CALL	ENXSCN		;Advance scan pointer past {*/%}
	PUSH24			;Save f1
	CALL	EXPR4		;CHL = f2
	MVI	B,0		;Keep track of result's sign in B
	CALL	CHKSGN		;HL<=|f2|, Sign(f2) change flips B
	XCHG			;DE = |f2|
	POP24			;CHL = |f1|
	JMP	CHKSGN		;HL = |f1|, sign(f1) change flips B
;
;	Basic expression evaluation: integers, Q-registers, (EXPR)'s
;
				;{EXPR3 (3x)}
EXPR4:	CALL	EGTSCN		;;Make sure AL = non-blank char
	CALL	DIGCHK		;;Are we at a simple number?
	JRZ	XP40		;;Yes, skip over TSTV() for speed sake
	CALL	TSTV		;{Q-register, #, ", .}?
	JRNC	XP41		;Yes, CHL = value, return success
;
XP40:	CALL	TSTNUM		;Number?
	MOV	A,B		;A = # digits in the number
	ORA	A		;Were there any?
	JRZ	PARN		;No, see if this is the case of (<EXPR>)
;
XP41:	MVI	A,0FFH
	STA	XNMFLG		;Set number found flag
	JMPR	XP42		;Finish processing & return
;
PARN:	CALL	EGTSCN		;(<EXPR>)?
	CPI	'('
	JRNZ	ZROCHL		;No, get out
	CALL	ENXSCN		;Advance scan ponter past the "("
	CALL	EXPRR		;Entry point for internal recursion
	CALL	EGTSCN
	CPI	')'		;End of (<EXPR>)?
	JRNZ	ZROCHL		;No, get out
	CALL	ENXSCN		;Advance scan pointer to next nonblank char
;
;	Return CHL = result, process ', process overflow errors.
;
XP42:	CALL	EGTSCN		;A = symbol terminating the expression
	CPI	TICMRK		;Is it the 1's complement operator ' ?
	BNE	CHKOVF		;No, check for overflow errors
	CALL	ENXSCN		;Yes, advance past the complement operator
	MOV	A,C
	CMA
	MOV	C,A
	MOV	A,H
	CMA
	MOV	H,A
	MOV	A,L
	CMA
	MOV	L,A		;CHL <= CHL'
CHKOVF:	LDA	OVFERR		;See if any overflows have occurred earlier
	ORA	A
	BNE	ZROCHL		;Yes, just zero out CHL & return
	MOV	A,C		;Check for overflow on last operation
	ORA	A		;Sign bits must be all 0's or 1's
	RZ			;All zero, ok, return CHL = result
	INR	A
	RZ			;All 1's, also ok, return CHL = result
SETOVF:	MVI	A,0FFH		;Set overflow flag
	STA	OVFERR
ZROCHL:	LXI	H,0		;Set current result CHL <= 0
	MVI	C,0
	RET
;
; NXTBDE - Return in BDE results obtained from routine pointed to by DE.
;	   That routine is expected to return a 24-bit result in CHL.
;
;	    Saves CHL.
;
NXTBDE:	PUSH24			;Save CHL
	CALL	CALLDE		;Call (DE), result returned in CHL
	MOV	B,C
	XCHG			;BDE = result
	POP24			;Restore CHL
	RET
;
; ADD24 - Return CHL = 24-bit sum of CHL and BDE.
;	  Returns 'P', 'M', 'Z' & 'NZ'.
;
ADD24:	DAD	D
	MOV	A,B
	ADC	C
	MOV	C,A
	RNZ		;Flags set properly when 'NZ'
	ORA	H	;Check for true zero
	ORA	L
	RZ		;'Z' for true zero
	XRA	A
	INR	A	;'P' & 'NZ'
	RET
;
;***************************************************************************
;
; TSTV - Evaluate special numeric values (#, ^a, "a, Qr, .v).
;
;	 Enter: AL = current character, EVLGET-> next character.
;	 Return: 'NC' if value found and evaluated.  CHL = 17 bit numeric value.
;		 'C'  if special value not found.
;		 EVLGET -> next scan char.
;
;    Interprets and returns values for the following:
;	 # = MAXINT = 65535.
;	^a = ASCII value of the control character represented by caret-a.
;	"a = ASCII value of the character "a".
;	Qr = value stored in numerical register "r".
;	     Qa - Qz are read-only program variables.
;	     Q0-Qmax are user-definable variables.
;	.v = value returned by routine "v" listed in table VALTBL.
;
				;{EXPR4}
	BPROC	TSTV
TSTV:	CPI	'#'		;Max integer?
	BNE	TSTV2
	LXI	H,MAXINT	;Largest possible integer
				;{TSTCTL,Numerous valuator routines}
TSTV1:	MVI	C,0
	JMPR	TSTYES
;
TSTV2:	CPI	'.'		;Evaluator function?
	JRNZ	..4
	CALL	ENXCHR		;Yes, A = next command character
	LXI	D,VALBRK	;BREAK out routine for unknown evaluator code
	LXI	H,VALTBL	;Table of Value-returning routines
	CALL	DISPT2		;CHL = value, if control returns here
	XRA	A		;Set 'NC'
	RET

..4:	CPI	'"'		;ASCII char "a?
	JRNZ	..5
	CALL	ENXCHR		;Yes, A = next command character
	JMPR	TSTCT1		;Proceed as with CHKCTL

..5:	CALL	TSTCTL		;Control code ^a ?
	RZ
				;Q-register?
	ANI	5FH		;Convert to UC
	CPI	'Q'
	STC			;Set failure case
	RNZ			;No, return with carry set
	CALL	EGETQR		;Yes, get register number
	CALL	VALCHL		;CHL = value
TSTYES:	CALL	ENXCHR		;Advance scan pointer
	XRA	A		;Set 'NC' & 'Z' for success
	RET
	EPROC	TSTV
;
; TSTCTL - Check command buffer for / return value of ctrl char.
;	   (Source may be true ASCII ctrl code or caret-char). (10/3/85)
;	    'Z' if successful, else 'NZ'.
;
;	INPUT: A = current scan character.
;
				;{TSTV}
	BPROC	TSTCTL
TSTCTL:	CPI	20H		;CTRL char?
	JRC	TSTCT1		;Yes, process
	CPI	'^'		;Caret?
	RNZ			;No, then not control code, return 'NZ'
	CALL	ENXCUC		;Yes, A = next command character UC
	ADI	1-'A'		;A = ASCII value of the control code
				;{Numerous valuator routines}
TSTCT1:	MOV	L,A		;L = ASCII code
	MVI	H,0		;Zero MSB
	JMPR	TSTV1
	EPROC	TSTCTL
;	
; TSTNUM - Return CHL = integer value of string of digits pointed to by DE.
;	   Returns B = # digits.
;
;    Ignores blanks and interprets digits. Quits on non-digit.
;    Loses most significant bits of |numbers| > 65535.
;
				;{EXPR4,EGETQR}
	BPROC	TSTNUM
TSTNUM:	CALL	ZROCHL		;Set value = 0 initially
	MOV	B,H		;# digits encountered also = 0
	CALL	EGTSCN		;A = last scanned char, or 1st non-blank chr
..1:	CALL	DIGCHK		;Is this a digit?
	RNZ			;No, CHL = final value, B = # digits
	INR	B		;No overflow, increment digit count
	CALL	MULT10		;Multiply HL by 10, add A
	CALL	ENXCHR		;A = next scan character
	JMPR	..1	
	EPROC	TSTNUM
;
; CHKREL - Return 'Z' if current/next-nonblank command character is
;	   a relational operator, else return 'NZ'.
;
;	   BC, HL preserved.
;
				;{EXPRR}
CHKREL:	CALL	EGTSCN		;A = current or next-nonblank command char
	LXI	D,RELTBL	;DE -> table of relational operators: <=>
	JMP	LOOKCH		;Is A in {<=>} ?
				;Flags set
	.PAGE
;
; ENXCHR - Bump string scan ptr EVLGET to next char & return the char in %A.
;
ENXCHR:	PUSH	H
	LES	H,EVLGET
	MOV%ES	A,M
	STA	EVLCHR
	INX$	H
	SHLD	EVLGET
	POP	H
	RET
;
ENXCUC:	CALL	ENXCHR
	JMP	CONVUC		;Convert to UC
;
; EGTSCN - Return current char unless blank, in which case ENXSCN.
;
EGTSCN:	CPIB	EVLCHR,SPACE
	RNZ
;
; ENXSCN - Return next char that is not a blank/tab from string @(EVLGET).
;
ENXSCN:	CALL	ENXCHR
	CALL	BLKCHK		;Is it SPace or Tab?
	BEQ	ENXSCN		;Yes, scan over it
	RET
;
; EGETQR - Get numerical register number into REGNUM.
;	   Exit:  EVLGET-> past end of register #.
;
				;{TSTV,GETQRN}
EGETQR:	CALL	ENXCHR		;Advance to next scan char
	CALL	TSTNUM		;CHL = register number, 0 if none given
	MOV	A,L		;A = register #
	CPI	QRGMAX		;Valid register #?
	JNC	QRNBRK		;No, BREAK out
	STA	REGNUM		;Set REGNUM
;	JMPR	EBKSCN
;
; EBKSCN - Backup string pointer EVLGET.
;
				;{EGETQR above}
EBKSCN:	PUSH	H
	LHLD	EVLGET
	DCX$	H
	SHLD	EVLGET
	POP	H
	RET
	.PAGE
;
; CHKSGN - Check sign of 24-bit register CHL
;
;	   Returns 'NC' is when C is positive.
;	   Otherwise 'C', changes sign of CHL and flips state of B.
;
CHKSGN:	MOV	A,C
	ORA	A		;'NC'
	RP
;
; CHGSGN - Changes sign of CHL and flips state of B unconditionally.
;
CHGSGN:	MOV	A,B		;Flip B
	CMA
	MOV	B,A
				;{EXPR1}
CHGSG1:	MOV	A,L		;Complement CHL
	CMA
	ADI	1		;Begin converting to 2's complement form
	MOV	L,A
	MOV	A,H
	CMA
	ACI	0		;Continue converting to 2's complement
	MOV	H,A
	MOV	A,C
	CMA
	ACI	0		;Finally
	MOV	C,A
	STC			;'C'
	RET
;
; GITCHL - Get the 24-bit valued iteration count into CHL & set S,Z flags.
;
				;{CALC,XACMD,XSCMD}
GITCHL:	LXI	H,CHL		;HL -> 24-bit iteration count
	CALL	LDCHL		;CHL = value
;	JMP	CHKCHL		;Set sign flags.
;
; CHKCHL - Check sign bits of 16&8-bit value in CHL, setting S & Z flags.
;
				;{GITCHL^, PRTCHL}
CHKCHL:	MOV	A,H
	ORA	L		;16-bit value has been examined
	RZ			;Return 'Z' & 'P' when zero
	MOV	A,C		;Sign bits into A
	ORI	1		;S & 'NZ'
	RET
	.PAGE
;
; GETEXC - Return HL -> executive routine for current command string's
;	   (upcoming) operation, determined from table pointed to by HL+1.
;
;	   EVLGET & EVLCHR are updated.
;
;	   Reg C saved (for EXPR1).
;
;	   DE's table: string to be matched, 80H, executive address,
;		  ended by 'no-match' case:  80H, default address.
;
;	   String's in HL's table that have same first parts, must be
;	   ordered such that the longer string occurs first in the table.
;
				;{EXPR1}
	BPROC	GETEXC
GETEXC:	LES	D,EVLGET	;+DE -> past current command line char
	DCX$	D		;DE -> start of possible command operation
..1:	PUSH	D		;Save start of string in case of no match
..2:	LDX%ES	D		;A = next character in string
	INX$	D		;DE -> past current command line char
	INX$	H		;HL -> char in table being searched against
	CMP	M		;Match?
	JRZ	..2		;Yes, so far so good
	STA	EVLCHR		;No, may have matched, update EVLCHR in case
	MVI	A,80H		;A = table's End-Of-String marker
	CMP	M		;Reached end of table's string?
	BEQ	..4		;Yes, pick up executive routine's address
..3:	INX$	H		;No, scan past rest of string in table
	CMP	M		;Reached the end of table's current string?
	BNE	..3		;Not yet, keep looking
	INX$	H		;Advance HL past executive routine address
	INX$	H		;
	POP	D		;DE -> start of command-line string
	JMPR	..1		;Try table's next string
;
..4:	INX$	H		;Bump pointer past delimiter
	SDED	EVLGET		;+Success, GET PTR -> past last char matched
	POPA			;Adjust stack
	JMP	MVINHL		;HL -> executive routine, return to caller
	EPROC	GETEXC
	.PAGE
;
; EVLSTR - Return CHL = value of expression @HL.	[9/5/86]
;	   Returns DE -> expression terminating character.
;	   Sets NFOUND = # ASCII characters processed during evaluation.
;
				;{XQCMD,VGET}
	BPROC	EVLSTR
EVLSTR:	LES	D,EVLGET	;Save expression GET ptr
	PUSH	D
;6	PUSH	ES
	PUSH	H		;Save -> start of string
	STDS	EVLGET,H	;Set GET pointer
	CALL	EXPR		;CHL = evaluated input expression
	POP	D		;DE -> start of expression
	PUSH	H		;Save part of number
	LHLD	EVLGET		;HL -> past expression terminating char
	MVI	A,SPACE
	DCX$	H		;HL -> 1st non-blank non-numeric char past e
..1:	DCX$	H		;Was preceeding character a space?
	CMP	M
	BEQ	..1		;Yes, keep backing up
	INX$	H		;No, found end of expression, now advance
	PUSH	H		;Save -> 1st byte following expression
	DSUB	D		;HL = length of evaluated string
	SHLD	NFOUND		;So user can check via .n

	POP	D		;DE -> expression terminator
	POP	H		;CHL = expression value
;6	POP	ES
	XTHL			;Simulate normal evaluator ending condition
	MOV%ES	A,M		;to allow .v to be part of expression
	STA	EVLCHR
	INX$	H
	STES	EVLGET,H
	POP	H		;CHL = value
	RET
	EPROC	EVLSTR
;
; STQVAL - Store CHL into numerical register specified by REGNUM.
;
				;XKCMD^,XACMD,XSCMD,XQCMD}
STQVAL:	PUSH24			;Save CHL
	CALL	VALPNT		;HL -> numerical register
	XCHG			;DE -> numerical register
	POP24			;Restore CHL
;	JMP	STCHL		;Store CHL where DE ->
;
; STCHL - Save CHL into (DE) in extended 8080 format, DE++.
;
				;{STQVAL^,PROCMD}
STCHL:	XCHG			;HL ->, DE = 16-bit value
	CALL	STDEIN		;Store 16-bit value, HL++
	MOV	M,C		;Store sign bits
	XCHG			;Restore pointer, value registers
	RET
;
; VALCHL - Return CHL = register contents, DE-> register.
;
				;{TSTV, XSCMD, XTCMD, XACMD}
VALCHL:	CALL	VALPNT		;NO, HL -> register
	PUSH	H
	CALL	LDCHL		;CHL = value
	POP	D		;DE-> register
	RET
;
; VALHL - Return C = 0, HL = (HL).
;
				;{.evaluators}
VALHL:	MVI	C,0
	JMP	MVINHL		;CHL <= Value <- HL
;
; VALPNT - Return HL -> numerical register.
;
				;{VALCHL, XK & XG via STQVAL}
VALPNT:	LXI	H,VALREG	;HL -> start of numerical registers
	LDA	REGNUM		;A = register number
	JMP	AD3AHL		;Get -> Qn
;
; LDCHL - Move 24-bit number pointed to by HL into CHL.
;
				;{VALCHL,GITCHL}
LDCHL:	PUSH	D		;Save DE
	CALL	MVINDE		;DE = value, HL++
	MOV	C,M		;C = sign
	XCHG			;HL = value
	POP	D		;Restore DE
	RET
	.PAGE
;
;	Relational Operator Tables.
;
	DSEG	$
RELTBL:	DC	'<=>'		;The 3 leading symbols in RELOPS below
	DB	EOS		;End of table flag
;
;	Relational operators & executive routines.
;
RELOPS:	DC	'>='		;Greater than or equal
	DB	GTXDLM		;String delimiter
	DW	XP1GE
	DC	'>'		;Greater than
	DB	GTXDLM
	DW	XP1GT
	DC	'<>'		;Not equal
	DB	GTXDLM
	DW	XP1NE
	DC	'='		;Equal
	DB	GTXDLM
	DW	XP1EQ
	DC	'<='		;Less than or equal
	DB	GTXDLM
	DW	XP1LE
	DC	'<'		;Less than
	DB	GTXDLM
	DW	XP1LT
;	DB	GTXDLM		;No provision here for "no-match"
;	DW	RELERR		;Handled beforehand by CHKREL
	CSEG	$
	.PAGE
;****************************************
;*					*
;*	Valuator Routines		*
;*					*
;****************************************
;
;	B - Buffer number/name
;
BGET:	LDA	EDTNUM		;Get edit buffer number
				;{BGET^,MGET}
RETASC:	CALL	RTOASC		;Convert to display code
	JMPR	RETA
;
;	C - Get character at EDTPTR.
;
CGET:	LHLD	EDTPTR
	MOV	A,M
	JMPR	RETA
;
;	E - Check for EOF / Error Flag / Search Error / Write error
;
	BPROC	EGET
EGET:	CALL	ENXCUC		;A = next scan char UC
	CPI	'F'		;Checking for EOF?
	BNE	..ER		;No, check for error flag
;
;	.EF - Check for EOF.
;
	CALL	INFLCH		;EOF?
	LXI	H,0		;Presume no
	MOV	C,H		;CHL=0
	CZ	NOTCHL		;Yes, set CHL true
	JMP	TSTYES		;
;
;	.ER - Get error flag.
;
..ER:	CPI	'R'		;Error flag?
	BNE	..ES		;No, check for Search Error flag
	LHLD	ERRFLG		;HL <= error flag
	JMPR	RETHL
;
;	.ES - Get Search Error flag.
;
..ES:	CPI	'S'		;Search Error flag?
	BNE	..EW		;No, check for Write Error
	LDA	SRCERR
	JMPR	RETA
;
;	.EW - Get Write Error flag.
;
..EW:	CPI	'W'
	LDA	WTERFL
;
; IFRETF - Return 0 or 1 to TSTV().
;
				;{EWGET^,OGET}
IFRETF:	JNZ	VALBRK
	ORA	A
	JRZ	RETA
	MVI	A,1		;Change non-zero to "1"
	JMPR	RETA
	EPROC	EGET
;
;	F - Get # free bytes remaining.
;
FGET:	CALL	FREESP		;BC = # free bytes
	JMPR	RETBC
;
;	M - Macro name / margins.
;
	BPROC	MGET
MGET:	CALL	ENXCUC		;A = next scan char UC
	CPI	'L'		;Left margin?
	BNE	..MR		;;No, branch
	LDA	INDPOS
	JMPR	RETA
;
..MR:	CPI	'R'		;Right margin?
	BNE	..MS		;;No, branch
	LDA	WRAPCL		;;EP 7 value
	JMPR	RETA
;
..MS:	CPI	'S'		;Scroll value?
	BNE	MEGET		;;No, branch
	LDA	VSHZBG		;;Horizontal scroll value in visual mode
;
RETA:	JMP	TSTCT1		;;Return value in AL to TSTV().
;
MEGET:	CALL	EBKSCN		;;Backup EVLGET
	LDA	REGEXN
	JMPR	RETASC
	EPROC	MGET
;
;	N - Number of bytes matched.
;
NGET:	LHLD	NFOUND
	JMPR	RETHL
;
;	O - Output File Open Flag / Operating System type
;
	BPROC	OGET
OGET:	CALL	ENXCUC		;A = next scan char UC
	CPI	'S'		;Op system?
	BNE	..OF		;;No, branch
	IF	P8086, [
	IF	MSDOS, [
	MVI	A,2		;;Yes, MSDOS is type "2"
	] [
	MVI	A,1		;;Yes, CP/M-86 is type "1"
	]
	] [
	XRA	A		;;Yes, CP/M is type "0"
	]
	JMPR	RETA
;
..OF:	CPI	'F'
	LDA	OUTFLG
	JMPR	IFRETF		;Return 0 or 1
	EPROC	OGET
;
;	P - Get offset of EDTPTR.
;
PGET:	LHLD	EDTPTR
				;{VMGET}
PGET0:	LDED	TXTFLR
	DSUB	D
RETHL:	JMP	TSTV1
;
;	R - Return Value / Remainder.
;
	BPROC	RGET
RGET:	CALL	ENXCUC		;A = next scan char UC
	CPI	'M'		;Remainder?
	BNE	..RTGT		;No, branch
	LHLD	LFTOVR		;Yes, HL = |remainder|
	JMPR	RETHL
;
..RTGT:	CPI	'T'		;Register type?
	BNE	..RVGT		;No, branch
	CALL	GETERN		;Get register #
	CALL	KBUFFR		;A=buffer type
	JMPR	RETA
;
..RVGT:	CPI	'V'		;Return Value?
	JNZ	VALBRK		;No, error
	LHLD	RETVAL		;Yes, HL = Return Value
	JMPR	RETHL
	EPROC	RGET
;
;	T - Get next tab position.
;
TGET:	CALL	EBKSCN		;
	CALL	XGET		;Get current column in HL
	CALL	FNDTAB		;Get next tab in %A
	JMPR	RETA
;
;	Ur - Get length of text register r.
;
UGET:	CALL	GETERN		;Get register #
	CALL	GETRLN		;Yes, get BC = length
RETBC:	MOV	H,B		;{UGET^,FGET}
	MOV	L,C
	JMPR	RETHL
;
;	V - Return BLMVEN / evaluate expression @ EDTPTR.
;
	BPROC	VGET
VGET:	LES	H,EVLGET	;HL-> next command char
	MOV%ES	A,M		;Get it
	ANI	5FH		;Make UC
	CPI	'M'		;'.vm'?
	BEQ	..VM		;Yes, branch
;
;  Evaluate expression at EDTPTR, advancing EDTPTR, setting NFOUND.
;	EVLCHR, EVLPUT defined as though normal evaluator function.
;
	LHLD	EDTPTR		;HL -> text string
	CALL	EVLSTR		;CHL = expression value, DE -> past expr
				;NFOUND set by EVLSTR
	SDED	EDTPTR		;+Set new EDTPTR
	RET
;
;	VM - Return HL = [BLMVEN].
;
..VM:	CALL	ENXCHR		;Advance GET pointer
	LHLD	BLMVEN
	JMPR	PGET0
	EPROC	VGET
;
;	W - Window parameters.
;
	BPROC	WGET
WGET:	CALL	ENXCUC		;A = next scan char UC
	CPI	'H'		;Horizontal size?
	BNE	..WVGT		;No, check for vertical position
	LDA	WWLLEN		;Horizontal window size
	JMPR	JRETA
;
..WVGT:	CPI	'V'		;Vertical Size?
	BNE	..WXGT		;No, branch
	LDA	WWNLIN		;Vertical window size
	JMPR	JRETA

..WXGT:	CPI	'X'		;Horizontal Position?
	BNE	..WYGT		;No, branch
	LDA	WINHOR
	INR	A		;Convert 0 to 1
	JMPR	JRETA

..WYGT:	CPI	'Y'		;Vertical Position?
	BNE	..WAGT		;No, branch
	LDA	WINVER		;Vertical window size
	JMPR	JRETA

..WAGT:	CPI	'A'		;Normal Attribute?
	BNE	..WDGT		;No, branch
	LDA	WWFRAT
	JMPR	JRETA

..WDGT:	CPI	'D'		;Display type?
	BNE	..WEGT		;No, branch
	CALL	KDISPY		;A = {0,1,2,3} for {CRT,MM,IBMmono,IBMchromo}
	JMPR	JRETA

..WEGT:	CPI	'E'		;Erase attribute?
	BNE	..WNGT		;No, branch
	LDA	WWBKAT
	JMPR	JRETA

..WNGT:	CPI	'N'		;Current window name?
	BNE	..WSGT		;No, branch
	LDA	WWNAME
	JMPR	JRETA

..WSGT:	CPI	'S'		;Window status?
	BNE	..WTGT		;No, branch
	CALL	GETERN		;;REGASC = evaluated window name
	LDA	REGASC		;;AL = window name
	CALL	WSTCAL		;;Does the window exist?
	MVI	A,0		;;Assume No
	JRC	JRETA		;;No, return 00
	INR	A		;;Yes, return 1
	JMPR	JRETA

..WTGT:	CPI	'T'		;;Number of windows?
	BNE	..WZGT		;;No, branch
	CALL	MAXWIN		;;A = # windows
	JMPR	JRETA

..WZGT:	CPI	'Z'		;;Window zoom flag?
	JNZ	VALBRK		;;No, BREAK out
	LDA	WWZMFL		;;AL = zoom flag
;
JRETA:	JMP	TSTCT1		;;Return AL to TSTV()
	EPROC	WGET
;
;	X - Get column number at the edit point (starting with 1).
;
XGET:	LHLD	EDTPTR
	CALL	GETLGP
JRETHL:	JMP	TSTV1
;
;	Y - Get line # being edited (from beginning of file).
;
YGET:	CALL	CNTFL0		;HL = line #
	JMPR	JRETHL
;
	.PAGE
;
;	Valuator routines address table.
;
VALTBL:	DW	VALBRK
	DW	BGET
	DW	CGET
	DW	VALBRK
	DW	EGET
	DW	FGET
	DW	VALBRK
	DW	VALBRK
	DW	VALBRK
	DW	VALBRK
	DW	VALBRK
	DW	VALBRK
	DW	MGET
	DW	NGET
	DW	OGET
	DW	PGET
	DW	VALBRK
	DW	RGET
	DW	VALBRK
	DW	TGET
	DW	UGET
	DW	VGET
	DW	WGET
	DW	XGET
	DW	YGET
	DW	VALBRK
