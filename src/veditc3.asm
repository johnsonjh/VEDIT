	.TITLE	'VEDIT-C3'
	.PAGE
;************************************************
;*						*
;*	Command Mode Support Routines		*
;*						*
;************************************************
;
; Copyright (C) 1987 by CompuView Products, Inc.
;			1955 Pauline Blvd.
;			Ann Arbor, MI 48103
;
;	Written by: Theodore J. Green
;
;	Last Change: Ted - Dec. 05 - HELPEX() uses STRBUF[]
;		     Tom - Dec. 09 - ENDITR() bug
;		     Ted - Apr. 08 - Clean up ERRHND(), fix HELPEX() bug
;
; SKPLIN - Handles one/two parameter option for commands K,P,T.
;	   Returns HL -> highest specified character.
;	   In 2-parameter case, sets EDTPTR -> lowest specified character.
;
	IF	VPLUS, [
				;{RCCMD,GLOBX}+
SKPLIN:	CPIB$	NM2FLG,COMMA	;Comma => 2 parameters for this command
	BNE	FNDLIN		;Not the case, treat as iteration number
	LHLD	ITRCNT		;Yes, HL <= one parameter value
	LDED	ITRCN2		;+DE <= the other parameter value
	CALL	MNHLDE		;HL = lesser of the 2 values, DE the larger
	PUSH	D
	CALL	MNEPTX		;HL = minimum of (lesser+TXTFLR,TXTCEL)
	SHLD	EDTPTR		;Set edit point
	POP	H
;	JMP	MNEPTX		;HL = minimum of (larger+TXTFLR,TXTCEL)
;
; MNEPTX - Return HL = new edit pointer = minimum (HL+TXTFLR, TXTCEL).
;
				;{SKPLIN^, EJCMD}
MNEPTX:	LDED	TXTFLR		;+DE -> start of text buffer
	DAD	D		;Add to buffer offset
	XCHG			;DE-> desired position
	LHLD	TXTCEL		;HL -> past end of text window
	RC			;Return ceiling if overflow
	JMP	MNHLDE		;HL -> minimum of DE,HL
	]			;<IF VPLUS>
;
; FNDLIN - Set HL-> forward or backward # of lines.
;	   "0" sets pointer to begin of current line.
;	   "0" is converted to "-0".
;	   Return: 'C' if EOF reached or attempt to go past beginning.
;		   ITRCNT also updated.
;
				;{SKPLIN+,GLOBX+,LCMD,YSCMD,PPRANG}
				;{KCMD,TCMD,RCCMD}-
	BPROC	FNDLIN
FNDLIN:	LHLD	EDTPTR		;HL = edit point
				;{WCMD}
FNDLN2:	CALL	CHKITR		;BC = ITRCNT, is ITRCNT <= 00?
	INX	B		;Account for current line
	BEQ	BAKLIN		;Yes, go skip backwards
;
; Skip forward number of lines in BC.
;
;	Consider using NEXTLF
;
	LXI	D,01A0AH	;Get D = EOF, E = LF
..3:	DCX%BC			;Decrement iteration count
	RZ			;Count=00, return HL-> begin of line
;
..4:	MOV	A,M		;Get next char
	CMP	D		;Reached EOF?
	BEQ	FNDLN5		;Yes, return 'C' and HL-> EOF
	INX$	H		;Bump PTR (past LF)
	CMP	E		;Reached end of line?
	BNE	..4		;No, continue looking
	JMPR	..3		;Yes, check if desired line reached
	EPROC	FNDLIN
;
;	Allow "0L", but return 'C' if "-nL" reaches beginning
;
BAKLN2:	TSTW$	ITRCNT		;Was command "0L"?
	XCHG			;HL = TXTBGN
	RZ			;Yes, return 'NC'
	DCX$	B		;Adjust for INX$ B above
				;No, return 'C' and update ITRCNT
;
FNDLN5:	SBCD	ITRCNT		;Save remaining iteration count
	STC
	RET			;Return with 'C'
;
; Skip backward number of lines in BC.
;
	BPROC	BAKLIN
BAKLIN:	LDED	TXTFLR		;DE-> start of text window
..1:	DCX$	H		;Don't point to LF if looping
	CALL	PREVLF		;HL-> previous LF
	CALL	CMHLDE		;Before begin of text window?
	BLT	BAKLN2		;Yes, set EDTPTR to TXTBGN
	LOOP	..1		;Decrement BC and loop until == 0
	INX$	H		;HL-> past the last LF
	RET			;Return 'NC'
	EPROC	BAKLIN
;
; CHGPTR - Compute HL = new Edit pointer for 'C', 'D' commands.
;
				;{CCMD,DCMD}
	BPROC	CHGPTR
CHGPTR:	CALL	GETITR		;BC = # positions to move
	LDED$	TXTFLR		;DE-> text beginning
	LHLD	EDTPTR		;HL = edit PTR
	TST$	MINUS		;Is move negative?
	JRZ	..FORW		;No, branch
;
..BACK:	DSUB	B		;HL-> tentative new position
	JRC	..1		;Branch if underflow
	CALL	CMHLDE		;Is move valid?
	RNC			;Yes, return new EDTPTR
..1:	XCHG			;No, return TXTFLR
	RET
;
..FORW:	DAD	B		;Add the movement
	XCHG			;DE = tentative EDTPTR
	LHLD	TXTCEL
	RC			;Return TXTCEL if overflow
	JMP	MNHLDE		;or if invalid EDTPTR
	EPROC	CHGPTR
;
; CHKGLB - Return 'Z' if global search & more text on disk.
;
				;{FCMD,SCMD,BCMD,LCMD,ZCMD,EMCMD+,RMCMD+}
CHKGLB:	CPIB$	GLBFLG,'_'	;Performing global search?
	RNZ			;No, return 'NZ'
	CALL	BRKCHK		;Check for user abort
	TST$	MINUS		;Is move negative?
	JZ	NEXFIL		;No, read following section, 'Z' if success

	IFNOT	VPLUS, [
	JMP	PRVFIL		;Yes, read previous section, 'Z' if success
	]

	IF	VPLUS, [
	CALL	CHKBOT		;Yes, restore BOTCHR if necessary (-F cmd>
	JNZ	PRVFIL		;Just read text when no BOTCHR, 'Z' if success

	CALL	PRVFIL		;Read previous section, 'Z' if success
	RNZ			;Quit if nothing read
	CALL	SETBOT		;Reset fence EOF
	XRA	A		;'Z'
	RET
	]			;<IF VPLUS>
	.PAGE
;
; CHKITR - (BC=ITRCNT) LE 0?  'Z' if so.
;
				;{FNDLIN,ICMD,YICMD,YPCMD,XACMD-}
CHKITR:	CALL	GETITR		;BC = ITRCNT, 'Z' if zero
	RZ			;0?
	IFNOT	VPLUS, [
	CPIB$	RMINUS,'-'	;"-n" ?
	RET
	] [
	TST	MINUS		;ITRCNT < 0 ?
	JMP	C%Z%NZ		;'Z' if so
	]
;
; GETITR - Get the iteration count in BC and return
;	   'Z' if count is zero.
;
				;{PROCM6,CHGPTR,RDPREV,cmds A,J+,W,YA,EI,EO}
GETITR:	LBCD	ITRCNT		;+Get iteration count
GETIT1:	MOV	A,B		;Get the count
	ORA	C		;Is it zero
	RET			;Return 'Z' if zero
;
; DECITR - Decrement iteration count and return.
;	   'Z' if zero after decrement.
;
				;{APPEN4,FCMD,SCMD1,RDPVS1}
DECITR:	LBCD	ITRCNT		;+Get remaining count
	DCX$	B		;Decrement it
	SBCD	ITRCNT		;+Save new count
	CALL	BRKCHK		;Check for user abort
	JMPR	GETIT1		;Branch for zero test
	.PAGE
;
; OVERWT - Overwrite text at (DE) with new string at CMDGET
;	   for a maximum of BC bytes.  Update CMDGET and EDTPTR
;	   Return: 'C' if string terminator reached
;
				;{ICMD,IOVCMD}
OVERWT:	CALL	STNSNL		;Make sure screen rewritten for "S" command
	LES	H,CMDGET	;HL-> begin of insert string
	CALL	INSMOV		;Perform the move. 'C' if terminator
	SHLD	CMDGET		;Save new command GET PTR
	SDED$	EDTPTR		;Save new edit PTR
	RET
;
; INSLEN - Compute in BC size of string to insert.
;	   Return: Size includes end of command line, but not TERMCH.
;		   'Z' if TERMCH found, else 'NZ'.
;
				;{ICMD,RICMD}
	BPROC	INSLEN
INSLEN:	LDA	TERMCH		;Get terminating char
	MOV	E,A		;E = TERMCH
	LES	H,CMDGET	;HL-> begin of insert string
	LXI	B,0		;Init counter to 00
..1:	MOV%ES	A,M		;Get next char
	INX$	H		;Bump PTR
	CMP	E		;Is it the terminator?
	RZ			;Yes, BC = size
;
	INX$	B		;No, count the char
	PUSH	D		;Save terminator
	LDED	CMDPUT		;DE-> end of command line
	CALL	CMHLDE		;Have we reached end of command?
	POP	D		;Restore terminator
	BLT	..1		;No, keep scanning
	JMP	RET%NZ		;Yes, return 'NZ'
	EPROC	INSLEN
;
; INSMOV - Moves bytes from (HL) to (DE) for BC bytes.
;	   Check if real TERMCH reached.
;	   Return 'C' if terminator, HL and DE set.
;
				;{ICMD,OVERWT}
	BPROC	INSMOV
INSMOV:	JCXZ	..1		;Must have reache terminator if count == 0
	IFNOT	P8086, [
	CALL	MOVEBC		;Move the bytes
				;HL-> past last char moved
	] [
	CALL	MVXI		;No polling
	]

	PUSH	D		;-
	LDED	CMDPUT		;DE-> past end of command
	CALL	CMHLDE		;Reached end of command?
	POP	D		;-
	RZ			;'NC' => command did not contain TERMCH
				;Else another char - it must be TERMCH
..1:	INX$	H		;HL-> past terminator
	STC			;Set 'C' to indicate real TERMCH reached
	RET
	EPROC	INSMOV
	.PAGE
;
; INSCHR - Insert char in C into text buffer at EDTPTR.
;	   *BREAK* out if no memory space
;	   Regs:   DE is saved
;
				;{EICMD,YICMD sets up through ROUTAD}
INSCHR:	CALL	SV%BD		;Save regs
	LXI	H,EASAV1	;HL-> free byte (in DS for 8086)
	MOV	M,C		;Store the char
	LXI	B,1		;We have one char to insert
;	JMP	INSBLK
;
; INSBLK - Insert BC chars beginning at HL in text buffer at EDTPTR.
;	   Return: DE = new EDTPTR, *BREAK* out if no memory space
;
				;{EGCMD,INSCHR above}
INSBLK:	PUSH	H		;Save insert pointer
;6	PUSH	ES
	LHLD	EDTPTR		;HL-> begin of text to move up
	CALL	BUFFUP		;Move text up
				;Update all PTRs including EDTPTR
	JC	BREAK		;BREAK out if no memory space
;6	POP	ES
	POP	D		;Restore Ptr.  HL = EDTPTR
	XCHG			;Insert at EDTPTR
	JMP	MOVEBC		;Perform the insert
	.PAGE
;
; SCCMPS - Compress delete chars (0FFH) from modified portion of text buffer.
;
	BPROC	SCCMPS
SCCMPS:	TSTM	COMPFL		;Do we need to compress?
	MVI	M,00		;Clear the flag
	RZ			;No, return now
	SUB%BC	EDTPTR,TXTFLR	;BC = # bytes to compress. DE = TXTFLR
	MOV	H,D
	MOV	L,E		;HL-> begin of text buffer
;
..1:	JCXZ	..2		;Done compressing if count == 0
	MOV	A,M		;Get next char in text
	INX$	H		;Bump the Get PTR
	DCX$	B		;Decrement the count
	CPI	0FFH		;Is it the delete char?
	BEQ	..1		;Yes, just skip over it
	STAX	D		;No, save char at Put PTR
	INX$	D		;Bump the Put PTR
	JMP	..1		;Continue (Faster than JMPR)
;
..2:	SDED$	EDTPTR		;Save new text PTR
	JMPR	PACKTX		;Move upper text block down to EDTPTR
	EPROC	SCCMPS
;
; PACKTX - Pack down text after a delete type operation.
;	   Move text between DELPTR and TXTRWF to EDTPTR.
;
				;{SCCMPS,VDELET}
PACKTX:	LHLD	DELPTR		;HL-> past delete region

				;{DCMD,KCMD-}
PAKTX0:	XCHG			;DE-> past delete region
	LHLD	EDTPTR		;HL-> begin delete region

				;{GLOBHL+}
PKTX00:	CALL	MNHLDE		;Make sure HL < DE
	SHLD	EDTPTR		;Save final EDTPTR

				;{RDPREV}
PCKTX1:	CALL	SBHLDE		;BC = -# bytes to move down
	XCHG			;HL-> begin of text to move down
	JMP	BUFFDW		;OK if BC = 00

	.PAGE
;************************************************
;						*
;	Command Execution Stack Routines	*
;						*
;************************************************
;
; SETCMD - Make string (HL) ending in [00] the new command string.
;
				;{OPNSPX,VISMAC}
SETCMD:	MOV	D,H
	MOV	E,L		;DE-> string
	CALL	STRLEN		;HL = length
	DAD	D		;DE-> begin string, HL-> past string
;
; SETCM2 - Push current command pointers onto stack.  Set new command
;	   string pointed to by DE (GET) and HL (PUT).
;
				;{SETCMD^,SETPRM,SETRX,HCMD,SETPTH(F5)}
SETCM2:	CALL	PSHCMD		;Save current command buffer pointers
	XCHG			;HL = GET, DE = PUT
	MVI	A,0FFH		;Set execution-register off
;6	PUSH	DS
;6	POP	ES
;
				;{SETCM2^,POPCMD,MCMD}
SVCMPT:	STES	CMDGET,H	;Save GET Ptr
	STES$	CMDPUT,D	;Save PUT Ptr
	STA	REGEXN		;Save execution-register #
	RET
;
; PSHCMD - Save REGEXN, CMDPUT, CMDGET on stack REGSTK.
;	   BREAKs out if no stack space left.
;	   Registers BDH & ES preserved.
;
				;{MCMD,SETCM2}
PSHCMD:	CALL	SV%BDH		;Save registers
;6	PUSH	ES
	LDA	REGEXN		;Stack REGEXN
	LBCD$	CMDGET		;Stack CMDGET
	LDED$	CMDPUT		;Stack CMDPUT
;6	MOV	ES,CMDGET+2
	LXI	H,REGSTK	;Point to stack
;6	CALL	PSHCHK
;6	POP	ES
;6	RET
;
; PSHCHK - Save DE, BC & A onto HL's stack.  BREAK out if no room.
;
				;{PSHCMD^,PROCMD}
PSHCHK:	CALL	PUSHSK		;Save A, BC, DE on HL's stack
	LXI	H,ISFMSG	;HL-> stack error
	JC	MSGBRK		;Error if stack overflow
	RET			;Go back for next command
	.PAGE
;
; POPCMD - Restore previous command variable set from REGSTK, if any.
;	    (REGEXN, CMDGET, CMDPUT)
;	    Also checks for turning trace mode back on.
;
;	    Return 'C' if stack empty, else 'NC'.
;
				;{NXCMCH,JMCMD,BEGIN,SETRX,PRCPRM,SDFSET,CPOPCM}
	BPROC	POPCMD
POPCMD:	LDA	REGEXN		;Executing a macro?
	INR	A
	BEQ	..1		;No, skip next check

	TST$	SRCHFL		;Yes, string open?

	IF	VPLUS, [
	LXI	D,STROPM
	JNZ	MACBRK		;Yes, n.g., break
	] [
	JMP	BREAK
	]

..1:	CALL	POPREG		;Pop register parameters off stack
	RC			;Return 'C' if nothing executing
	CALL	SVCMPT		;Save new REGEXN, CMDGET and CMDPUT

	IF	VPLUS, [
	MOVB	TERMCH,SAVTRM	;For when popping |Rr
	MOVB	PARMFL,SAVPFL	;	"
	CLR$	SAVPFL		;	"
	TST$	TRDENB		;Has trace been disabled?
	RZ			;No, return ('NC')
	CALL	HOPCHK		;Yes, check whether time to re-enable it
	]

	ORA	A		;'NC'
	RET
	EPROC	POPCMD

	IF	VPLUS, [
;
; POPUNC - Pop unclosed clauses belonging to current macro from ITRSTK.
;
;		Input:  L = level of clause to be retained on the stack.
;			May be left undefined.
;			(Base level = 0, +1 for each [, -1 for each ])
;
;		Output:	HL -> ITRSTK
;			B = offset past clause of specified level (if L def)
;			D = offset past last clause NOT belonging to REGEXN
;			E = number of clauses from REGEXN currently on ITRSTK
;
;			'C' if specified level is > # REGEXN clauses on stack.
;
				;{JMCMD,JPCMD,NXCMCH}
	BPROC	POPUNC
POPUNC:	PUSH	H		;Save desired level
	LXI	H,ITRSTK	;HL-> stack-top offset
	PUSH	H
	MOV	D,M		;D = offset past top of stack
	MVI	E,0		;E = # clauses in stack for current macro
	LXI	B,-STAKSZ	;# bytes per stacked clause
	INX$	H		;HL-> before 1st clause
	MOV	A,D
	CALL	ADDAHL		;HL-> macro name of clause at top of ITRSTK
	MOV	A,D		;Get top-of-stack offset
;
..1:	ORA	A		;Stack empty?
	BEQ	..2		;Yes, quit
	LDA	REGEXN		;Get current macro name
	CMP	M		;Same as name in stacked clause?
	BNE	..2		;No, quit
	INR	E		;Yes, increment count of owned clauses
	DAD	B		;Decrement stack pointer to next lower clause
	MOV	A,D		;Lower top of stack offset
	ADD	C
	MOV	D,A
	JMPR	..1		;Keep checking
;
..2:	POP	H		;HL-> ITRSTK
	POP	B		;C = desired level to be retained, perhaps
	MOV	A,C		;Get # clauses desired
	ADD	A
	ADD	A
;6	ADD	AL,AL
	ADD	C		;x5 (or 9) to be consistent with offset value
	ADD	D		;A = offset to new top of stack
	MOV	B,A		;Return new top of stack offset in B
	MOV	A,E		;# clauses from REGEXN currently on stack
	CMP	C		;Less than specified level?
	RET			;Catches some cases of branching into clauses
	EPROC	POPUNC
	]			;<IF VPLUS>
	.PAGE
;
; HOPCHK - Check whether trace mode is to be turned back on.
;	    Allows trace function to treat 'M' as just a single commmand.
;
	IF	VPLUS, [
				;{POPCMD}
HOPCHK:	CMPMB	REGSTK,TRSAVE	;Same level as originating macro?
	RNZ			;No, return
	CLR$	TRDENB		;Yes, allow tracing
	JMP	SETTRF		;Turn trace flag back on
;
; GETSLM - Get search limits for searching command buffer & text registers.
;	   Returns DS:DE -> start of the buffer, HL -> past last filled byte.
;	   BC may be clobbered.
;
				;{JPCMD}
	BPROC	GETSLM
GETSLM:	LES	H,CMDPUT	;ES:HL -> past last byte in command buffer

	IFNOT	P8086, [
	LDED$	CMDBAS		;DE -> start of command buffer
	] [
;6	MOV	DX,EWP CMDBAS	;ES:DX-> start of command buffer when not REGEXN
	]

	LDA	REGEXN		;Executing a text register?
	STA	REGNUM		;Set for PNTREG
	INR	A
	IFNOT	P8086, [
	RZ			;No, use command buffer pointers
	]
;6	JZ	#1		;Yes, use command buffer pointers

	CALL	PNTREG		;Does text register exist?
	IFNOT	P8086, [
	RNZ			;Yes, ptrs set
	]
;6	JZ	#2		;No, branch

;6#1:	MOV	AX,ES		;Yes, swap into command register's segment
;6	PUSH	BX
;6	PUSH	DX
;6	CALL	SWAPSG
;6	POP	DX
;6	POP	BX
;6	RET
;6#2:
	LHLD	EDTRWF		;No, DS:HL -> EOF
	MOV	D,H
	MOV	E,L		;DE = HL
	RET
	EPROC	GETSLM
	]			;<IF VPLUS>
	.PAGE
;
; NOTSUC - Handle search not successful. (7/9/85)
;
	IF	VPLUS, [
				;{FCMD,SCMD}
NOTSUC:	CALL	CKMANY		;PNDFLG (#) & SRCHCN > 0 ?
	MVI	E,1		;Flag from NOTSUC
	JRC	ERRHND		;No, process error
	RET			;Yes, not an error
;
; CKMANY - Return 'C' for search error unless PNDFLG (#) and SRCNCN > 0.
;
				;{NOTSUC,EMCMD}
CKMANY:	CPIB$	PNDFLG,'#'	;Search for "ALL"?
	BNE	NTMANY		;No, error
	TSTW$	SRCHCN		;Was search count zero?
	RNZ			;No, not an error
NTMANY:	STC			;'C' for error
	RET
	]			;<IF VPLUS>

;
; NOTSUC - Handle search not successful. (7/24/85)
;
;	   PNDFLG (#) & SRCHCN > 0 => no error, just return.
;
	IFNOT	VPLUS, [
				;{FCMD,SCMD}
NOTSUC:	MVI	E,1		;Flag from NOTSUC
	CPIB$	PNDFLG,'#'	;Search for "ALL"?
	BNE	ERRHND		;No, process the error
	TSTW$	SRCHCN		;Was search count zero?
	RNZ			;No, not an error
;	JMPR	ERRHND
	]			;<IFNOT VPLUS>
;
; ERRHND - Process failure to fulfill F,L,S command.  [1/8/86]
;	   Reg E:  0 for entry from LCMD
;		   1 for entry from NOTSUC
;		   2 for entry from YMCMD
;
;			V P L U S    O N L Y
;
;	   Set error flag (.er).  Then if
;
;	   ES(8) = 2, just return, user will check errflg.
;
;
;			V E D I T    &    V P L U S
;
;	   ES(8) = 1  or COLFLG, quietly terminate current iteration loop
;		      if any.  (If no loop just return).
;	   ES(8) = 0  and not COLFLG, then display error message (DL) & break out.
;
				;{NOTSUC^,LCMD}
	BPROC	ERRHND
ERRHND:	TST	REPLFL		;Is this a [FIND] or [REPLACE]?
	JRZ	..1		;No, branch
	CPI	2		;Is this [REPLACE]-"Rest"?
	RZ			;Yes, leave EDTPTR after last replace
	MOVW	EDTPTR,FINDPT	;Restore EDTPTR to position before search
	RET			;That's all for [FIND] or [REPLACE]
;
..1:
	IF	VPLUS, [
	CALL	SETERR		;Set error flag
	CPIB$	COLSW,2		;Will user check errflg?
	RZ			;Yes, return
;
	CPIB$	COLFLG,':'	;Explicit end-loop-quietly parameter?
	JRZ	ENDITR		;Yes, do it
	TST$	COLSW		;Explicit end-loop-quietly environment switch set?
	JRNZ	ENDITR		;Yes, do it
	]			;<IF VPLUS>

	IFNOT	VPLUS, [
	TST$	COLFLG		;Explicit end-loop-quietly parameter?
	JRNZ	ENDITR		;Yes, do it
	]
;
;	Reset ROUTAD to window handler (don't print error)
;
	CALL	RSTCON		;Restore ROUTAD to LSTFLG, detach printer
	LXI	H,LMSG		;HL-> 'END OF BUFFER REACHED'
	DCR	E		;LCMD?
	MVI	A,0D0H		;MSGHND code
	JRM	..3		;Yes, error, BR8K
;
;	Handle search error / YM error
;
	LXI	D,GRPPAT+1	;;Assume YM error
	JRNZ	..2		;;Branch if YM error
	LXI	D,TARGST	;HL-> search string
..2:	LXI	H,CNFMSG	;HL-> 'CANNOT FIND...'
	LXI	B,SRCEOS	;Search string ends on SRCEOS char (1A)
	MVI	A,0DCH		;2 msgs, terminated by B & C, CRLF() fore/aft
				;Wait for keypress if VS mode
..3:	JMP	MSBR8K		;;Display message and BREAK out
	EPROC	ERRHND
	.PAGE
;
; SETERR - Set error flag.
;				;{ERRHND,JPCMD,EMCMD}
	IF	VPLUS, [
SETERR:	MVIB$	ERRFLG,1	;Set ERRFLG for failure
	RET
	]
;
; ENDITR - End any current iteration level.
;
	IFNOT	VPLUS, [
				;{ERRHND}
ENDITR:	TSTM	ITRSTK		;Is iteration level zero?
	RZ			;Yes, continue with next command
	CALL	POPSTK		;No, end this iteration level
	LDA	ITRMCR		;Get ) char
	MOV	B,A		;Put in B
	JMP	SCNGET		;Scan CMDGET past ) char.
	]			;<IFNOT VPLUS>
;
; ENDITR - End any current iteration level.
;
	IF	VPLUS, [
				;{JLCMD,PROCMD(;),ERRHND}
ENDITR:	CALL	CHKLVL		;Pop clauses from this command level.  Any?
	RC			;No, return

	MOV	A,D		;THEN clause?
	ORA	E
	BEQ	ENDITR

	DCX$	D		;ELSE clause?
	MOV	A,D
	ORA	E
	BEQ	ENDITR

	INX$	D		;;REPEAT clause, DE-> past opening bracket
	SDED$	CMDGET		;;Set GET to scan past REPEAT clause
	LDA	REGEXN		;;Retrieve register name for loop just popped
	CALL	PSHITR		;;So NXCMCH() won't overrun macro register
	CALL	SCNGIT		;;Advance GET past end of REPEAT clause
				;;BREAKs out from NXCMCH() if no end bracket
	JMP	POPITR		;;Re-pop REPEAT clause
	.PAGE
;
; SCNGIT - Scan CMDGET past end of this clause.
;
;	    Handles nested clauses.
;
;	    Will not work if a string contains an unbalanced # of
;	    clause-bracketing delimiters.
;
				;{ENDITR^,ENDITL,ELSEIF,JNCMD}
	BPROC	SCNGIT
SCNGIT:	CALL	SCNSET		;Get BC = (,) as configured by user
	MVI	D,1		;Initialize level counter
..1:	CALL	NXCMCH		;A = next command char
	CMP	B		;New level?
	BNE	..2		;No, check for )
	INR	D		;Yes, increment level counter
	JMPR	..1		;Keep scanning
..2:	CMP	C		;End of current level?
	BNE	..1		;No, keep scanning
	DCR	D		;Yes, decrement counter.  Level 0 yet?
	BNE	..1		;No, keep scanning
	RET
	EPROC	SCNGIT
	]			;<IF VPLUS>
;
; SCNGET - Move CMDGET past the character in B.
;
				;{ENDITR-^, SCMD}
	BPROC	SCNGET
SCNGET:	LES	H,CMDGET	;HL-> next command char
..1:	MOV%ES	A,M		;Get next char
	INX$	H		;++ptr
	CMP	B		;Is it terminator (or ']')?
	BNE	..1		;No, keep scanning
	SHLD	CMDGET		;Yes, set new Get Ptr
	RET
	EPROC	SCNGET
;
; CHKLVL - Pop top clause from ITRSTK if it belongs to this command level.
;	    Return 'NC' if clause was popped, else 'C'.
;
	IF	VPLUS, [
CHKLVL:	CALL	POPITR		;Any clauses on stack?
	RC			;No, return
	LXI	H,REGEXN	;Yes, HL -> current command buffer
	CMP	M		;Did it belong to this buffer?
	RZ			;Yes, return 'NC'
	CALL	PSHITR		;No, put it back onto the stack
	STC			;'C' for failure
	RET
	]			;<IF VPLUS>
;
; CWNTLV - Return HL = current level of clause nesting as determined by
;	    HL -> start of command string, BC = # chars to check.
;
	IF	VPLUS, [
				;{JPCMD}
	BPROC	CWNTLV
CWNTLV:	PUSH	B		;Save count
	CALL	SCNSET		;BC = []
	LXI	H,0		;Initially at level 0
..1:	LDAX	D		;Get current char
	INX$	D		;Bump pointer
	CMP	B		;'[' ?
	BEQ	..2		;Yes, go increment level #
	CMP	C		;']' ?
	BNE	..3		;No, skip
	DCX$	H		;Yes, decrement level #
	JMPR	..3
;
..2:	INX$	H		;Increment level #
..3:	XTHL			;HL = # chars left, TOS = level #
	DCX$	H		;Decrement bytes-to-check count
	MOV	A,H		;Done?
	ORA	L
	XTHL			;Retrieve level #
	BNE	..1		;No, keep checking
	POPA			;Yes, adjust stack
	RET
	EPROC	CWNTLV
	]			;<IF VPLUS>
;
; SCNSET - Get BC = user configured opening/closing clause brackets. ([]).
;
	IF	VPLUS, [
				;{SCNGIT,CWNTLV}
SCNSET:	LDA	ITRMCL		;
	MOV	B,A		;B = [
	LDA	ITRMCR		;
	MOV	C,A		;C = ]
	RET
	]			;<IF VPLUS>
;
; SCNSTR - Scan string pointed to by ES:HL for char in B.
;	   Search terminated unsucessfully by CR or ESC.
;
;	Return: HL -> past last char encountered.
;	        'C' if successful else 'NC'.
;
	IF	VPLUS, [
				;{PROCM5}
SCNSTR:	MOV%ES	A,M		;A = current char
	INX$	H		;HL -> next char
	CALL	CMPCE		;CR or ESC ?
	RZ			;Yes, return 'NC' for failure
	CMP	B		;Search char?
	BNE	SCNSTR		;No, try next char
	STC			;Yes, return 'C' for success
	RET
	]			;<IF VPLUS>
	.PAGE
;
; HELPEX - Perform on-line help.
;	   Enter: HL-> filename to use for help.
;		  VISFLG set if visual mode help.
;	   Retrn: Screen zooming is restored.  VISFLG is cleared!
;
				;{HCMD,EHCMD,VSHELP}
	BPROC	HELPEX
HELPEX:
	IFNOT	VPLUS, [
	CLR$	HELPFL		;Don't stop on "\"
	]

	CALL	OPNSPX		;Open help file
	JC	FNFBRK		;Error if file not found
;
	SHLD	AUXGET		;Set GET=PUT to force sector read
	SHLD	AUXPUT		;value arbitrary
	LDA	LVLHLP		;Let ^L clear the screen
	CALL	SETLST		;Set new LSTFLG
	MVIB$	HELPFF,1	;;Tabs expand to every 8
	CALL	STNSNL		;;Make sure screen rewritten

	IF	WINDOO, [
	CALL	MAXWIN		;Is there more than one window?
	CNZ	WIZOOM		;Yes, zoom the window
	]
;
	TST$	VISFLG		;Visual Help?
	JRNZ	..4		;Yes, branch to display 1st screen of menu
;
;	Command Mode - copy help argument to STRBUF[].
;
	CALL	USRSEL		;;Copy argument to STRBUF[]
	JMPR	..2		;;Merge below
;
;	Query user for string.
;
..1:	LDA	INPFLG		;;Get input decode flag
	MOV	B,A		;;Save in B
	PUSH	B
	LXI	D,STRBUF	;Use GP buffer for user response
	LXI	H,STRBEN	;HL-> past end of buffer
	CALL	GETSTR		;Get user response with CR-LF
				;HL-> past last input char
	POP	B		;;Restore INPFLG
	MOV	A,B
	STA	INPFLG
	JRC	..BRK		;BREAK out on <ctrl-c>
	JRZ	..1		;Start over for <ctrl-x> or <ctrl-u>
;
	DCX$	H
	DCX$	H		;;Backup over CR-LF
	MVI	M,00		;;Terminate input string
..2:	CPIB$	STRBUF,00	;;Is input string empty?
	BEQ	..4		;;yes, skip token search
;
;	Search for STRBUF[] in menu file.
;
..3:
	IFNOT	VPLUS, [
	MVIB$	HELPFL,1	;Set flag to stop on "\"
	]

	CALL	FNDBGN		;Did search succeed?
	JRC	..ERR		;No, give error
;
;	Display menu text until ^S, ^V, <EOL>\, or EOF.
;
..4:	STA	PRVFLG		;Save for case of <EOL>\
	CALL	AUXCHR		;Get next char from menu file
	MOV	C,A		;Save char
	JRC	..DONE		;Quit on EOF
	CPI	CTRLS		;^S?
	JRZ	..1		;Yes, stop and get user response
	CPI	CTRLV		;^V?
	JRZ	..7		;Yes, stop and get visual-function response
	CPI	'\'		;<EOL>\?
	JRNZ	..5		;No, branch

	IFNOT	VPLUS, [
	TST$	HELPFL		;Stop on "\"?
	JRZ	..5		;No, branch
	]

	CPIB$	PRVFLG,LF	;<EOL>\?
	JRZ	..DONE		;Yes, all done
;
..5:	CALL	PCHAR		;Display on screen
	JMPR	..4		;Continue
;
;	Decode visual-function, setup STRBUF w corresponding ASCII code name.
;
..7:	CALL	VSHEL2		;Set search based on visual function
	JRC	..DON1		;Branch if invalid visual-function
	CALL	CRLF		;Give an extra CR-LF
	JMPR	..3		;Merge above
;
..ERR:	LXI	H,HLPMSG	;HL-> NOT FOUND error message
	MVI	A,0D0H		;CRLF() fore/aft (cm), temporary pause (vm)
	CALL	MSGHND		;Display message
	JMPR	..DON1		;Skip wait-prompt
;
;	Terminate Help function.
;
..DONE:	CLR$	VISFLG		;;Prevent [CANCEL] messing up at PAUSE()-STAKEY()
	CALL	PAUSE		;Give "Press any key to continue"
..DON1:	CALL	RSTOUT		;Restore LSTFLG to previous
	CLR$	HELPFF
	LDA	WWNAME		;Get current window #
	CALL	SWWIND		;Rewrite all windows if needed
	JMP	CLSAUX		;Done
;
..BRK:	CALL	..DONE
	JMP	BREAKC
	EPROC	HELPEX
;
;
; FNDBGN - Search for \text\ in STRBUF[].
;	   Return: 'C' if not found.
;
				;{HELPEX}
	BPROC	FNDBGN
FNDBGN:	CALL	AUXCHR		;Get next char from AUX file
	RC			;'C' for failure
..1:	CPI	'\'		;Help-section delimiter?
	JRNZ	FNDBGN		;No, keep looking
;
	LXI	H,STRBUF-1	;HL-> ahead of HELP argument
..2:	INX$	H		;HL-> next argument char (upper case)
	MOV	A,M		;Get it
	CALL	CONVUC		;;Convert to UC
	MOV	M,A		;;Put back as UC
	ORA	A		;EOS?
	JRZ	..3		;Yes, branch
	PUSH	H
	CALL	AUXCHR		;Get next HELP char
	POP	H
	RC			;Return 'C' if EOF reached
	CALL	CONVUC		;Make upper case
	CMP	M		;Still matching?
	JRZ	..2		;Yes, loop
	JMPR	..1		;No, start over
;
..3:	CALL	AUXCHR		;Get next AUX char
	RC			;Error if EOS
	CPI	'\'		;Final delimiter?
	JRNZ	..1		;No, start over
	RET
	EPROC	FNDBGN
;
; USRSEL - Copy help parameter to STRBUF[].
;
				;{HELPEX}
	BPROC	USRSEL
USRSEL:	LXI	H,STRBUF	;;HL-> save buffer
	CALL	NXSCAN		;Get next non-blank char
	CALL	CMPCE		;Terminator (CR ESC)?
	BEQ	..9		;Yes, null string
;
;	Save response in STRBUF[]
;
..2:	MOV	M,A
	INX$	H
	CALL	NXCMCH
	CALL	CMPCE
	BNE	..2
;
..9:	MVI	M,0		;;Terminate with 00
	RET
	EPROC	USRSEL
;
; AUXCHR - Return A = next char from AUX file.	 [11/13/85]
;	   'C' and A = EOF if EOF char. reached
;	   'C' and A = 00 if empty sector (binary EOF) reached
;
				;{HCMD,YLCMD,FNDBGN}
	BPROC	AUXCHR
AUXCHR:	LDED$	AUXPUT		;DE = end PTR
	LHLD	AUXGET		;HL-> next char
	CALL	CMHLDE		;Buffer empty?
	JRZ	..1		;Yes, read next sector
;
	MOV	A,M		;Get next char
	INX$	H		;Bump pointer
	SHLD	AUXGET		;Save pointer
	CPI	EOF		;EOF?
	STC			;Assume yes
	RZ			;Return 'C' and A = EOF
	ORA	A		;'NC'
	RET
;
;	Read next sector of help file.
;
..1:	CALL	RSTDMA		;Reset DMA to DEFDMA
	CALL	AXREAD		;Read next sector into DEFDMA
				;'Z' if empty sector read
	SHLD	AUXPUT		;AUXPUT-> past sector end (or to EOF)
	SDED	AUXGET		;AUXGET-> sector start
	JRNZ	AUXCHR		;No, get 1st char
	XRA	A		;Yes, return A = 00
	STC			;& 'C'
	RET			;
	EPROC	AUXCHR

