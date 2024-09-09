	.PAGE
	.TITLE	'VEDIT-R1'
;****************************************
;*					*
;*	Text Register Support		*
;*					*
;****************************************
;
; Copyright (C) 1986 by CompuView Products, Inc.
;			1955 Pauline Blvd.
;			Ann Arbor, MI 48103
;
;	Written by: Theodore J. Green
;
;	Last Change: Ted - Feb. 11 - Extensive changes to use MALLOC and FREE
;			 - Feb. 14 - Use STAPRM()
;		     Tom - Mar. 09 - FREESP from C3 (correspond to VPLUS)
;		     Ted - Apr. 04 - New routine CPYBRK()
;			 - Apr. 07 - PNTREG() changes
;
;
; CLRREG - Clears the text register and frees the memory.
;
				;{RCCMD,RLCMD,MAKCPY}
CLRREG:	CALL	PNTREG		;DE-> register, BC = length
	XCHG			;HL-> register
	CALL	FREE		;Free the memory space
	LHLD	REGPTR		;HL-> register's table entries
	CALL	ZER%HL		;Zero out PTR; HL++
	JMP	ZER%HL		;Zero out the length
;
; MAKCPY - Make copy of text in text register.
;
;	   Enter:   HL and DE -> ends of text to copy.
;		    REGAPP = "+" if text to be appended to register.
;
;	   Return: 'NC' if text was copied, 'C' if no room.
;		    BC = length of text added.
;
				;{RCCMD,RICMD}
BRKCPY:	CALL	MAKCPY		;Dead even in byte count w 2 routines
	RNC			;Slightly more esthetically pleasing this way
	JMP	BREAK
				;{BEGIN,VCPTXT}
MAKCPY:	CALL	POSDIF		;Set HL > DE, BC = HL - DE
	CALL	SV%BDH		;Save registers
;
	PUSH	D		;Save -> begin of text to copy
	PUSH	B		;Save length
	TSTM	REGAPP		;Append?
	MVI	M,00		;Clear flag
	JRZ	..1		;No, branch
;
	CALL	PNTREG		;DE-> register BC = register length
				;HL-> past end of register = append location
	JMPR	..2		;Merge
;
..1:	CALL	CLRREG		;Clear out existing T-REG
	LXI	H,0000		;Allocate space anywhere
;
..2:	POP	B		;BC = size of new text
	CALL	MALLHL		;Allocate the memory, HL-> free memory
	JRC	MAKER1		;Not enough space, give error return
				;Will POP H, 'C' then return thru SV%BDH()
;
	XCHG			;DE-> allocated space for T-REG
	LHLD	REGPTR		;HL-> register's table entry
	CALL	TSTWIN		;Does register already contain text?
	JRNZ	..3		;Yes, don't need to change PTR
	CALL	STDEIN		;No, set new PTR to text register
	JMPR	..4
;
..3:	INX$	H		;HL-> length
	INX$	H
..4:	CALL	ADDMBC		;(HL) = (HL) + BC Update length
;
;	Make a copy of the text.
;
	TSTM	RIFLG		;Is this an RI command?
	MVI	M,00		;Clear flag
	POP	H		;HL-> block of text to copy to text reg
				;Note: DE-> allocated space
	JRZ	..5		;No, branch
;
;	For RICMD - use CMDGET which has already been adjusted
;
	LHLD	CMDGET		;HL-> text for RI command
;
..5:	JMP	RTLDIR		;Perform  L D I R
				;Return thru SV%BDH
;
MAKER1:	POP	H
	STC			;Set 'C' for error return
	RET
;
; GETTXT - Copy the text register to current edit point.
;	   Entry:  HL-> where to insert text copy.
;	   Return: 'NC' if text copied, 'C' if no room left.
;
				;{RGCMD,VINTXT}
GETTXT:	PUSH	H		;Save -> where to insert text reg
	CALL	PNTREG		;Get text register pointer & length
				;DE-> T-REG, BC = size of T-REG
	POP	H		;HL-> where to insert text
	PUSH	D		;Save text reg address
	CALL	BUFFUP		;Move text buffer up
	JRC	MAKER1		;Branch if no space
;
;	Copy from the text register to main text.
;
	XCHG			;DE-> where to insert
	POP	H		;HL-> Text reg address
	JMP	RTLDIR		;Perform the move
;
; CHKEXE - Check if REGNUM is being executed
;	   Return: Give error and abort if executing
;
				;{RCCMD, RLCMD, RICMD}
CHKEXE:	CMPMB	REGNUM,REGEXN	;Is it current register?
	JRZ	..2		;Yes, give error
	MOV	D,A		;Save register number being checked
	LXI	H,REGSTK	;Check if it's on the stack
	MOV	C,M		;Get length of the stack
	INX$	H		;HL-> max length
..1:	MOV	A,C		;Get length
	SUI	5		;Decrement it
	MOV	C,A
	RC
	INX$	H		;Nope, point to next entry
	INX$	H
	INX$	H
	INX$	H
	INX$	H		;HL-> REGEXE.
	MOV	A,M		;Check if this entry
	CMP	D		;
	BNE	..1		;No, keep looking
;
..2:	LXI	H,MCRMSG	;HL-> error message
	JMP	MSGBRK		;Give error and BREAK
;
; REGLEN - Return BC = length of text registers, 'Z' if zero
;
				;{UCMD,WRTSTA}
REGLEN:	MVI	B,REGMAX	;Number of registers
	LXI	D,0000		;Init counter
	LXI	H,REGTBL+2	;HL-> first length in T-REG table
..1:	CALL	ADINDE		;Add (HL) to DE
	INX$	H
	INX$	H
	INX$	H
	INX$	H		;HL-> next length
	DJNZ	..1		;Loop for all registers
	MOV	B,D
	MOV	C,E		;Return length in BC
	MOV	A,C
	ORA	B		;Return 'Z' if BC == 00
	RET
	.PAGE
;
; GETREG - Get text register table values for register %A
; PNTREG - Get text register table values for REGNUM
;
;	   Enter: REGNUM =  text register # to use.
;	   Retrn: REGPTR -> text register table entry.
;		  DE     -> text register.
;		  BC,HL  =  text register length.
;
GETREG:	STA	REGNUM
PNTREG:	CALL	PNTRG1		;Point to table entry
	CALL	LD5%HL		;DE = address of text register
				;BC = HL = Length.  A = garbage.
	MOV	H,D
	MOV	L,E		;HL-> register
	DAD	B		;HL-> end of register
	MOV	A,B
	ORA	C		;'Z' if register is empty
	RET
;
; Point to text register table indexed by REGNUM
;
				;{PNTREG}
PNTRG1:	LXI	D,REGTBL	;Index into register pointer table
	LDA	REGNUM
	MOV	L,A
	MVI	H,0		;Mult. by 4
	DAD	H
	DAD	H
	DAD	D		;HL -> entry
	SHLD	REGPTR
	RET
;
; GETCRN - Get text register number in command mode and set REGNUM
;
				;{MCMD,RCMDs,XCMDs}
GETCRN:	XRA	A		;Get zero
..1:	STA	REGAPP		;Set/reset appending flag
	CALL	NXCMCH		;Get next character
;	CPI	' '		;Ignore spaces
;	BEQ	GETCR2
	CPI	'+'		;Appending?
	BEQ	..1		;Yes, set flag, skip any spaces
;
	SUI	'0'		;Make binary
	BLT	GETCR4		;Bad register number, use 0
	CPI	REGMAX		;Good register number?
	BLT	GETCR5		;Yes ...
				;{AUTEXE}
GETCR4:	XRA	A		;Bad register number, use 0
GETCR5:	STA	REGNUM		;Save register number
	RET
	.PAGE
;
; Get marker number to use and set REGNUM.
;
GETMNM:	CALL	SV%BDH		;Save registers
	LXI	H,MRKMSG
	JMPR	GETRN0		;Merge
;
; GETCRN - Get text register number in visual mode and set REGNUM.
;
GETRNM:	CALL	SV%BDH		;Save registers
	LXI	H,REGMSG
;
; Set up status line to ask for register number.
;
GETRN0:	CALL	STAPRM		;Prompt for register/marker #
	CALL	STADON		;Close status line
	XRA	A		;Get zero
..1:	STA	REGAPP		;Set/reset appending flag
..2:	CALL	GETKEY		;Get next key char
				;BC = function code, or C = char
	TST$	CNCLFL		;Was [CANCEL] pressed?
	JNZ	CANCEL		;Yes perform CANCEL (stack is reset)
;
	MOV	A,B		;Was function key pressed?
	ORA	A
	JRNZ	GETCR4		;Yes, set REGNUM = 0 to use default register
	MOV	A,C		;No, get char back
	CPI	'+'		;Append?
	BEQ	..1		;Yes, set append flag
;
	SUI	'0'		;Make binary
	BLT	..2		;Wait for good number
	CPI	REGMAX
	BGE	..2		;Wait for good number
	JMPR	GETCR5		;Save value in REGNUM
;
; RTOASC - Convert Text Register # in A to ASCII.	[3/5/86]
;	   Can also convert user # to ASCII
;	   This is for VEDIT only. Would be use for EU command
;
;	   Returns NULL for command buffer (255)
;	   Returns '@' for MAINRG and any undefined value
;
RTOASC:	CPI	10		;Digit?
	BGE	..1		;No, skip
	ADI	'0'
..1:	RET			;

