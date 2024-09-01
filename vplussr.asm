	.TITLE	'VPLUS-SR'
	.PAGE
;****************************************
;*					*
;*	Search Routines			*
;*					*
;****************************************
;
; Copyright (C) 1986 by CompuView Products, Inc.
;			1955 Pauline Blvd.
;			Ann Arbor, MI 48103
;
;	Written by: Theodore J. Green
;
;	Last Change: Tom - Oct. 04, 1985 - 2.02 changes
;			 - Jan. 19, 1986 - 2.02a update - SEARDN,SEADN2 (#em)
;			 - Mar. 31 - TARSET a la Vedit
;			 - May  11 - Lookup table for decoding patterns
;				   - BOL & EOL patterns |< & |>
;			 - May  12 - Group delimiter |G parens, brackets, braces
;			 - May  13 - SEAR00:  F$$ give error
;				     (certainly not move EDTPTR to EOB!
;			 - May  16 - |L fixed for reverse search
;
;			 - June 13 - |P bug fixed (result of SVCMPT changing DE,HL values)
;			   Sept 17 - |< find last BOL at EOB.
;		     Ted - Nov. 12 - |> and |< fixed for reverse search, misc
;
; STSEAR - Set up for search by filtering search string into PATREG.
;
;		Converts lower case to upper case if SRCNSW is set.
;		Switches source buffer to text register if |Rr is found.
;		Converts '|,' to ',' for benefit of |Pr pattern.
;
				;{FCMD,SCMD,JLCMD,EMCMD,MGET}
STSEAR:	MVIW	SRCHCN,00	;Reset search count
	SHLD	SRFAIL		;Reset fail PTR
	CPI%ES	CMDGET,ESC	;Is next char ESC?
	RZ			;Yes, use previous string (or null)
	CALL	SETTRM		;No, set terminator
	MOV	B,A		;Save in B
	CLR	FNDFLG		;Turn off visual FIND flag
	STA	LASTCH		;For TRIMC
	LXI	H,TARGST	;HL-> string storage
	LXI	D,TARGST+TARGLN	;Max value for TARSET
	CALL	SETPAT		;Process the string
	MVI	M,EOF		;Terminate the string
;	JMP	ENDTRM
				;{STSEAR^,ICMD via INSSET,RICMD,YTCMD}
ENDTRM:	CLR$	SRCHFL		;Clear flag - terminator found
	RET
;
; SETPAT - Set pattern string from cmd buff, handling |R & |P.
;
;	   HL-> pattern destination, DE-> destination U.L.
;	   TERMCH = SAVTRM = end of source/destination patterns.
;
				;{STSEAR,USRCHK}
	BPROC	SETPAT
SETPAT:	CALL	NXTCHR		;Get next string char
	CMP	B		;Reached terminator?
	BEQ	..3		;Maybe yes, maybe no
..2:	CALL	TRIMC		;Convert "|," to just "," for |P pattern
	CALL	TARST0		;Put char into TARGST
	JMPR	SETPAT		;Get rest of string
..3:	PUSH	H		;Check for |Rr processing
	CMPMB	TERMCH,SAVTRM	;Has TERMCH been changed?
	POP	H		;(If so, POPCMD will restore at end of |Rr)
	LDA	CMDCHR		;Retrieve putative terminator
	BNE	..2		;Yes, keep processing
	RET			;No, end of string reached
	EPROC	SETPAT
;
; TRIMC - Convert "|," to "," for the |P pattern
;
;	INPUT: current char in A
;	OUTPUT: char in C
;
				;{SETPAT}
	BPROC	TRIMC
TRIMC:	MOV	C,A		;Save char
	LDA	WILDCH		;Get user defined wild card char
	CMP	C		;Same as current char?
	BNE	..3		;No, skip
;
	LDA	LASTCH		;Yes, and what about the previous char?
	CMP	C		;Also WILDCH?
	BNE	..2		;No,skip
;
	CLR$	LASTCH		;Yes, don't get caught by "||,"
	RET
;
..2:	PUSH	H
	CPIB$	PEEKCH,COMMA	;Do we have "|," (from NXTCHR) ?
	POP	H
	BNE	..3		;No, skip
;
	CALL	NXCMCH		;Yes, throw away WILDCH, get the comma
	MOV	C,A		;Return the comma to caller in C
;
..3:	MOV	A,C
	STA	LASTCH
	RET
	EPROC	TRIMC
;
; TARSET - Insert char in A into string <- HL.  Check HL against DE
;
				;{VFIND,VREPLC via ??? }
TARSET:	MOV	C,A		;Save char in C
				;{SETPAT}
TARST0:	CALL	CMHLDE		;Reached limit?
	CMC
	RC			;Yes, return 'C'
;
				;{FORCML}
CHKUCC:	TST$	SRCNSW		;Search to ignore upper/lower case?
	MOV	A,C		;Get char. back in A
	CNZ	CONVUC		;Yes, convert lower to upper case
	MOV	M,A		;No, save char in storage
	INX$	H		;Bump string PTR
	RET
;
; SETTRM - Check for and set terminating character.
;	   Return: Terminator in A, HL clobbered.
;
				;{ICMD,RICMD,STSEAR,YTCMD}
	BPROC	SETTRM
SETTRM:	TST$	ATTFLG		;Is terminator explicitly specified?
	LXI	H,TERMCH	;HL-> terminator variable
	MVI	A,ESC		;A = default terminator
	JRZ	..1		;No, use ESC
	CALL	NXCMCH		;Yes, get explicit terminator
..1:	MOV	M,A		;Set terminator
	STA	SRCHFL		;Flag - waiting for terminator
	STA	SAVTRM		;For NXTCHR,POPCMD handling of |Rr & TERMCH
	RET
	EPROC	SETTRM
	.PAGE
;
;	Search text buffer
;
				;{FCMD,SCMD,VFIND,VREPLC,VRPEXE via FNDEXE}
	BPROC	SEARTX
SEARTX:	LHLD	SRFAIL		;HL-> where last search failed
	MOV	A,H		;Is PTR set?
	ORA	L
	JRNZ	..0		;Yes, use it
	LHLD	EDTPTR		;No, start search at EDTPTR
..0:	XCHG			;;DE-> where to search in text
	MVIW$	SRFAIL,00	;Clear SRFAIL flag
				;Merge below
	EPROC	SEARTX
;
; SEARCH - Look for match between text <- by DE and pattern <- by HL.
;	   Presumes text and pattern are terminated by EOF (ctrl-z).
;
;	   On success, returns:
;		       'NC'
;			DE -> 1st matched byte.
;			HL -> past last matched byte.
;			BC = # bytes matched.
;			(NFOUND) <= BC = # bytes matched.
;	   Else returns 'C'.
;
;	   Treats "|" as an escape signifying that a special pattern is
;		   about to be specified.
;
				;{JPCMD, SEARTX above}
SEARTG:	LXI	H,TARGST	;(Search using TARGST)
;
	BPROC	SEARCH
SEARCH:
;;	MVI	C,EOF		;C = pattern/text terminator = EOF
;
;	Come here to start search over from first character
;
				;{SMNYCH, below}
SEARC0:	LXI	B,5F1AH		;;B = 5F - Assume Lower/ Upper case ignored
				;;C = EOF - pattern/text terminator
	TST$	SRCNSW		;Is upper /lower case ignored?
	JRNZ	..0		;Yes, branch
	MVI	B,0FFH		;No, set that ANA B has no effect
;
;	Use separate loop if first search char is wildcard
;
..0:	LDA	WILDCH		;A = wildcard "|"
	CMP	M		;Start with wildcard?
	BEQ	..4		;Yes, handle initial wildcard
	MOV	A,M		;Get pattern char
	CMP	C		;Null search pattern?
	JZ	SEAERR		;Yes give error (don't allow EDTPTR-> TXTCEL)
	JMPR	..2		;No, quickly skip upto 1st possible start
;
..1:	CMP	C		;Is non-matching char. an EOF?
	JRZ	SEAERR		;Yes, give error
	INX$	D		;No, DE-> next byte in text buffer
..2:	LDAX	D		;A = char from text buffer
	CPI	'a'		;Is char less than 'a'?
	JRC	..3		;Yes, branch
	CPI	'z'+1		;Is char greater than 'z'?
	JRNC	..3		;Yes, branch
	ANA	B		;No, convert to upper case if B = 5FH
..3:	CMP	M		;Match ?
	BNE	..1		;No, check next byte
	JMPR	..9		;Yes, check rest of string
;
..4:	INX$	H		;HL -> pattern code
	MOV	A,M
	CALL	CONVUC		;Make upper case
	CPI	'Y'		;|Y?
	BNE	..5		;No, branch
	INX$	H		;
	JMPR	SEARC0		;Yes, ignore an initial "|Y"
;
..5:	CPI	'L'		;Branch if pattern can match multiple chars
	BEQ	..8
	CPI	'<'
	BEQ	..8
	CPI	'>'
	BEQ	..8
	CPI	'M'
	BEQ	..8
	CPI	'N'
	BEQ	..8
	CPI	'P'
	BEQ	..8
	PUSH	PSW
	CPI	'W'		;Or even |W without fudging
	BNE	..6
	MVI	M,'B'		;Change |W -> |B
;
..6:	DCX$	H		;HL -> wildcard
	CALL	PMATCH		;Does text match pattern?
	BEQ	..7		;Yes, found first, look for entire string
	CMP	C		;At end of text string (EOF)?
	JRZ	SEAER0		;Yes, return failure
	INX$	D		;DE -> next text character
	JMP	..6
;
;	Adjustments due to code at ..5,4.
;
..7:	POP	PSW		;A = true pattern code (a propos |B,|W)
	MOV	M,A		;Restore true code into pattern string
..8:	DCX$	H		;HL -> wildcard in pattern string
;
;	Try to match entire string, save starting position in case of failure.
;
..9:	PUSH	H		;Save current position in pattern
	PUSH	D		;DE-> first char that matches
	CALL	MATCH		;Successful pattern match? (C & Z flags)
	JRC	SEARDN		;Yes, return success
	POP	D		;No, retrieve starting point in text
	POP	H		;and in pattern
				;Any text remaining ? (Z returned by MATCH.)
	JRZ	SEAERR		;Branch if EOF reached
	INX$	D		;Advance to next text character
	JMP	SEARC0		;Yes, keep searching
	EPROC	SEARCH
;
SEAER0:	POP	PSW		;(..5)
	MOV	M,A		;Restore pattern byte (|B <- |W maybe)
				;{SEARCH,-Fcmd}
SEAERR:	SDED$	SRFAIL		;Save -> start search after disk buffering
				;{EMCMD}
SEAER1:	MVIB$	SRCERR,1	;Set error flag
	STC			;Set 'C' for failure
	RET
;
SEARDN:	XCHG			;HL-> past matched text
	POP	D		;DE-> begin of string
	POP	PSW		;Throw away -> pattern bytes
;
				;{FBACK,MGET,RMCMD}  [1/19/86]
SEADN2:	PUSH	H		;Save pointer past EOM
	INCW$	SRCHCN		;Increment the search count
	POP	H
	CALL	SBHLDE		;BC = # matched bytes, 'NC'
	SBCD	NFOUND		;Save for users to access via QL
	CLR$	SRCERR		;Clear error flag
	RET			;Return with DE-> begin of string and 'NC'
;
; MATCH - Make one attempt to match pattern <- by HL with the text <- by DE.
;	  C = pattern/text terminator EOF, B = '|' wildcard.
;
;	  Presumes pattern/text are terminated by EOF = <ctrl-z>.
;
;*************************************************************************
;			Modifiers, beware:
;
;	  In interests of speed, SEARCH presumes this routine only increments
;	  HL once except for patterns (currently) {|L,|M,|N,|P}.
;
;**************************************************************************
;
;	  If successful, returns 'C' and DE -> past last matched byte.
;	  else :	'NC','Z' => failure, end of text, => pattern > text.
;			'NC','NZ'=> failure, text remaining, and
;			      A  =  1 => pattern < text
;			      A  =  2 => pattern > text
;	  		      A  =  3 => pattern-code caused failure.
;
MATCHT:	LDED$	EDTPTR		;{MGET}
MACHT0:	LXI	H,TARGST	;{-FCMD}
	MVI	C,EOF
;
	BPROC	MATCH
MATCH:	LDA	WILDCH		;{SEARCH}
	MOV	B,A
;
;	At end of text buffer?
;
	LDAX	D		;A = next text byte
	CMP	C		;End of text ?
	BEQ	..3		;Yes, now check for EOP (shaves 0.2 sec/27K)
;
;	At end of search string?
;
	MOV	A,M		;A = next pattern byte
	CMP	C		;End of pattern ?
	BEQ	..5		;Yes, set success & return
;
;	Pattern match?
;
	CMP	B		;Wildcard '|' ?
	BEQ	..2		;Yes, go process
;
;	No, match normal text char.
;
	TST$	SRCNSW		;Upper/lower case ignored?
	LDAX	D
	CNZ	CONVUC		;Yes, convert to upper case
	CMP	M		;Match?
	INX	H		;Advance pattern pointer
	INX	D		;Advance text pointer
	JRZ	MATCH		;Yes, keep on checking
	MVI	A,1		;A = 1 => pattern < text
	JP	..1
	INR	A		;A = 2 => pattern > text
..1:	ORA	A		;'NC' & 'NZ' => failed, text remaining
	RET
;
;	Do pattern matching.
;
..2:	CALL	PMATCH		;Does text match pattern?
	INX	H		;Advance pattern pointer
	INX	D		;Advance text pointer
	JRZ	MATCH		;Yes, keep on checking
	MVI	A,3		;A = 3 => pattern-code caused failure
	ORA	A		;'NC' & 'NZ' => failed, text remaining
	RET
;
;	Check for EOF Pattern |L or end of pattern.  Ok if so.
;
..3:	MOV	A,M		;A = pattern byte
	CMP	B		;WILDCH?
	BNE	..4		;No, skip
	INX$	H		;HL-> pattern code
	MOV	A,M		;Get it
	CALL	CONVUC
	CPI	'L'		;EOF matching pattern?
	BEQ	..31		;Yes, branch
	CPI	'<'		;BOL?
	BNE	..6		;No, search failed
..31:	INX$	H		;Advance pattern pointer only!
;
;	Disallow if already at end of buffer.  No trampolines for CPU today.
;
	PUSH	H
	LHLD	EDTPTR
	CALL	CMHLDE		;Already at end of buffer?
	POP	H
	MOV	A,M		;Char following |L needs to be EOF for success
	BEQ	..6		;Yes, already at buffer end, disallow
;
;	Check for end of pattern string marker EOF
;
..4:	CMP	C		;End of pattern (EOF) ?
;
;	If 'Z', return success
;
..5:	STC			;'C' for successful search
	RZ			;Yes, return success
;
;	Else return failure, out of text
;
..6:	XRA	A		;No, 'NC' & 'Z' for failed, out of text
	MVI	A,2		;A = 2 => pattern > text
	RET
	EPROC	MATCH
;
; PMATCH - Do pattern matching as specified by search string |x.   [11/12/86]
;
;	   INPUT:  HL-> wildcard '|' in pattern string
;		   DE-> current text byte.
;	   OUTPUT: 'Z' if success with DE & HL -> last bytes matched.
;		   else 'NZ'.
;
	BPROC	PMATCH
PMATCH:	INX$	H		;Advance pointer to pattern matching code
	MOV	A,M		;A = pattern-matching code
	CPI	'A'		;Duplicate some code for sake of speed
	BLT	..SYMB
	CPI	'Z'+1		;Capital letter?
	BLT	..DISP
	CPI	'a'		;Maybe lower case?
	BLT	..SYMB
	CPI	'z'+1		;Lower case letter?
	BGE	..SYMB
	ANI	5FH		;Convert to upper case
..DISP:	SUI	'A'-1		;Compute table entry
	PUSH	H
	LXI	H,PATTBL	;HL-> table of pattern match routines
	CALL	TBLADD		;Get routine address
	XTHL			;Retrieve HL, routine address onto stack
	LDAX	D		;Retrieve the char
	RET			;Branch to pattern match routine

..SYMB:	CPI	'<'		;Beginning of line?
	BEQ	SBOL
	CPI	'>'		;End of line (exclusive)?
	BEQ	SEOL
	CPI	'@'		;Corresponding group delimiter?
	JZ	SGRP2
	EPROC	PMATCH
;
WLDPAT:	DCX$	H		;Back up HL -> known wildcard in pattern
	LDAX	D		;Get text character
	CMP	M		;Is it '|' ?
	INX	H		;HL -> presumed '|' in pattern string
	RET
;
;	Beginning of line?
;
SBOL:	LDAX	D		;Not if at EOB
	CPI	EOF
	JZ	RET%NZ

	DCX$	D		;DE-> previous text char  (Not defined for 1st
				;char generally, but works for edit buffers)
	JMP	SLINCH		;'Z' if LF or FF
;;	RZ
;;
;;	DCX$	D		;LF-CR?
;;	XCHG
;;	CALL	CRLFPR		;If so, HL-> CR
;;	XCHG
;;	RET

;
;	End of line?
;	If successful, must DCX D to account for INX D at MATCH..2.
;
	BPROC	SEOL
SEOL:	CALL	DE%EOL		;;At end-of-line (LF, CR-LF, EOF)?
	RNZ			;;No, return 'NZ'
	DCX$	D		;;Yes, DE-> previous char
				;;Account for INX D at MATCH..2
;
	LDAX	D		;;Get previous char
	CPI	CR		;;CR?
	JMPR	C%Z%NZ		;;If CR - return 'NZ'; don't match between CR-LF
	EPROC	SEOL
;
;	Table of pattern match routines.
;
	DSEG	$
;
PATTBL:	DW	SLETCH		;A-> alphabetic
	DW	SBLKCH		;B-> blank or tab
	DW	SCTLCH		;C-> control char
	DW	SDIGCH		;D-> decimal digit
	DW	WLDPAT		;E   		treat as wild char
	DW	SALPCH		;F-> alphanumeric
	DW	SGROUP		;G-> group delimiter
	DW	WLDPAT		;H   		treat as wild char
	DW	WLDPAT		;I   		treat as wild char
	DW	WLDPAT		;J   		treat as wild char
	DW	WLDPAT		;K   		treat as wild char
	DW	SLINTM		;L-> line terminator
	DW	SMNYCH		;M-> find rest of string
	DW	SNOTCH		;N-> anything not matching next pattern/char
	DW	WLDPAT		;O   		treat as wild char
	DW	SDFSET		;P-> user defined pattern set
	DW	WLDPAT		;Q   		treat as wild char
	DW	WLDPAT		;R   not supported in Visual Mode ****************
	DW	SSEPCH		;S-> separator
	DW	STABLE		;T-> table lookup
	DW	SUCCH		;U-> upper case
	DW	SLCCH		;V-> lower case
	DW	SWHTCH		;W-> white space (multiple blanks/tabs)
	DW	ANYCHR		;X-> any char
	DW	SUNTIL		;Y-> upto and including
	DW	WLDPAT		;Z   not defined, treat as wild char
;
	CSEG	$
;
;	Individual match routines - 'Z' means match, 'NZ' means no match.
;
SCTLCH:	LDAX	D
	CPI	' '		;A control char?
				;{SUCCH}
SCTLC1:	JNC	RET%NZ		;No, return NZ
ANYCHR:	CMP	A
	RET			;Yes, return Z
;
SSEPCH:	CALL	SALPCH		;Is it alphanumeric?
;
;	Change Z to NZ and NZ to Z
;
C%Z%NZ:	JZ	RET%NZ		;Yes, return NZ
	CMP	A
	RET			;Else, return Z
;
SALPCH:	LDAX	D
				;{FCBVAL, msdos}
ALFCHK:	CALL	LETCHK		;Is it a letter?
	RZ			;Yes, return Z
	JMP	DIGCHK		;No, check for digit
;
	BPROC	SLINTM
SLINTM:	TST$	MINUS		;Reverse search?
	JRNZ	..REV		;Yes, branch
;
	CALL	DE%EOL		;;At end-of-line (LF, CR-LF, EOF)?
	JNZ	SLINCH		;;No, check for FF
	INX$	D		;;Assume CR-LF
	CPI	CR		;;Is this CR-LF?
	RZ			;;Yes, return 'Z' and DE+
	DCX$	D		;;No, restore
	CMP	A		;;Return 'Z'
	RET
;
..REV:	LDAX	D		;;Get char again
	CPI	0CH		;FF?
	RZ
	CPI	CR		;CR?
	RZ
	CPI	LF		;LF?
	RNZ
	DCX$	D
	LDAX	D
	INX$	D
	CPI	CR		;Between CR & LF?
	MVI	A,LF
	JMPR	C%Z%NZ		;If so, n.g.
	EPROC	SLINTM
;
SUCCH:	CALL	SLETCH		;Is it a letter?
	RNZ			;No ,return 'NZ'
	CPI	'Z'+1		;Is it upper case?
	JMPR	SCTLC1		;Return 'Z' if UC
;
SLCCH:	CALL	SLETCH		;Is it a letter?
	RNZ			;No ,return 'NZ'
	CPI	'Z'+1		;Is it lower case?
	JC	RET%NZ		;No, return 'NZ'
	CMP	A
	RET			;'Z'
;
;	Upto and including.
;
	BPROC	SUNTIL
SUNTIL:	INX$	H		;HL -> next byte from pattern register
	MOV	A,M		;A = next pattern byte
	CMP	B		;Is it the pattern wildcard '|' ?
	BEQ	..2		;Yes, go process it
	CMP	C		;At end of pattern register?
	JZ	RET%NZ		;Yes, return failure
				;No, begin skipping until match pattern byte
..1:	TST$	SRCNSW		;Ignore upper/lower case distinction?
	LDAX	D		;Get next text byte
	INX	D		;+Advance text pointer
	CNZ	CONVUC		;If yes, convert to upper case
	CMP	C		;At EOF?
	JZ	RET%NZ		;Yes, failure
	CMP	M		;Match with pattern byte?
	BNE	..1		;No, keep checking
	DCX	D		;Yes, DE -> matched byte
	RET			;Return 'Z' for success
;
..2:	PUSH	H		;HL -> start of the pattern to be matched
	PUSH	D		;DE -> 1st byte of the search string
	CALL	PMATCH
	BEQ	..3
	POP	D
	POP	H
	INX$	D
	CMP	C
	JZ	RET%NZ	
	JMPR	..2
..3:	POP	PSW		;Adjust stack
	POP	PSW
	LDAX	D
	CMP	A		;'Z'
	RET
	EPROC	SUNTIL
;
;
	BPROC	SWHTCH
SWHTCH:	CALL	SBLKCH		;Is it a tab or space?
	RNZ			;No, return non success 'NZ'
..1:	INX$	D		;Yes, DE -> next text char
	CALL	SBLKCH		;More space/tabs ?
	JRZ	..1		;Yes, keep going
	DCX$	D		;No, back DE -> last char matched
	CMP	A		;'Z' for success
	RET
	EPROC	SWHTCH
;
	BPROC	SNOTCH
SNOTCH:	INX$	H		;HL-> char not to match
	MOV	A,M		;A = next pattern byte
	CMP	B		;Wildcard '|' ?
	BEQ	..1		;Yes, go process
	TST$	SRCNSW		;No, normal char.  Upper/lower case ignored?
	LDAX	D		;A = text char
	CNZ	CONVUC		;Yes, convert to upper case
	CMP	M		;Match pattern char?
	JMP	C%Z%NZ		;Switch truth value returned

..1:	CALL	PMATCH		;Recursively check for pattern match
	JMP	C%Z%NZ		;Now go switch truth value
	EPROC	SNOTCH
;
SMNYCH:	INX$	H		;Advance pattern pointer to next pattern
	PUSH	B		;Save WILDCH & EOF
	CALL	SEARC0		;Call search recursively
	POP	B		;Restore WILDCH & EOF
	XCHG			;DE -> past last matched char, if any
	LHLD	EDTRWF		;HL -> EOF = end-of-pattern marker
	JC	RET%NZ		;Return 'NZ' failure
	DCX$	H		;Account for upcoming INX
	DCX$	D
	XRA	A		;Else return 'Z' for success
	RET
	.PAGE
;
;	|Pr - User defined set in register 'r'.
;
;	   where 'r' contains pattern strings p1,p2,...,pn
;	   The comma can be included in a set element by the pattern '|,'.
;	   The pattern '|Pr' may not be used in a user defined set.
;
	BPROC	SDFSET
SDFSET:	INX$	H		;HL -> text register number
	PUSH	H
	PUSH	D
	PUSH	B
	MOV	A,M		;A = text register number
	CALL	SETRN		;Set register r=REGNUM
	CALL	MCMD1		;Change scan buffer to the text register r
	XCHG			;ES:HL-> past pattern string
	BEQ	..2		;Quit if text register is empty
	MVI%ES	M,COMMA		;Change text terminator to ','
	INX$	H		;Advance PUT past terminator
	SHLD	CMDPUT
;
;	Try to match next pattern string from the set.
;
..1:	MVI	B,','		;B = pattern terminator
	LXI	H,DEFDMA	;Use for temporary pattern register
	LXI	D,DEFDMA+127
	CALL	SETPAT		;Put 1st set element into pattern register
	MVI	M,EOF		;Put terminator onto end of the pattern
	POP	B
	POP	D
	PUSH	D
	LXI	H,DEFDMA	;HL -> start of current set-pattern
	CALL	MATCH		;Match with current element of user's set?
	JRC	..3		;Yes, return success
	LDED$	CMDPUT		;Reached end of text register?
	LHLD	CMDGET
	CALL	CMHLDE
	PUSH	B
	JRC	..1		;No, check next element of set
;
	LES	H,CMDPUT	;Yes, restore text register's terminating EOF
	DCX$	H
	MVI%ES	M,EOF
..2:	CALL	POPCMD		;Restore previous scan buffer
	POP	B
	POP	D
	POP	H
	ORI	1		;'NZ' for failure
	RET
..3:	LES	H,CMDPUT	;Restore text register's terminating EOF
	DCX$	H
	MVI%ES	M,EOF
	DCX$	D		;D -> last matched char
	PUSH	D
	PUSH	B
	CALL	POPCMD		;Restore previous scan buffer
	POP	B
	POP	D
	POP	H		;Adjust stack
	POP	H		;HL -> r of |Pr
	XRA	A		;'Z' for success
	RET
	EPROC	SDFSET
;
;	Check if text char is in separator table.  This routine will never change
;
STABLE:	PUSH	D
	LDAX	D		;Get current text char
	LXI	D,SEPTBL	;Point to start of table
	CALL	LOOKCH		;'Z' if char is in the table
	POP	D		;Restore text pointer
	RET
;
;	A grouping delimiter?
;
SGROUP:	PUSH	D
	LDAX	D		;Get current text char
	LXI	D,GRPTBL	;DE-> table of (redundant) group terminators
				;Redundancy used by YM cmd
	CALL	LOOKCH		;'Z' if char is in the table
	POP	D		;Restore text pointer
	RET
;
;	Corresponding group delimiter?
;
SGRP2:	PUSH	D
	LDAX	D		;Get current text char
	LXI	D,GRPPAT	;DE-> current group-delimiter pair
				;1st = original, 2d = correspondant
	CALL	LOOKCH		;'Z' if char is in the table
	POP	D		;Restore text pointer
	RET
