	.PAGE
	.TITLE	'VEDIT-B1'
;****************************************
;*					*
;*	I N I T I A L I Z A T I O N	*
;*					*
;****************************************
;
; Copyright (C) 1986 by CompuView Products, Inc.
;			1955 Pauline Blvd.
;			Ann Arbor, MI 48103
;
;	Written by: Theodore J. Green
;
;	Last Change: Tom - Feb. 20, 1986 - Broken out from MAIN
;		     Ted - Mar. 20, 1986 - New routine SVCMPT()
;			 - Mar. 26 - INITRT() changed
;			 - Apr. 08 - Misc
;
;	NOTE: Everything up to "STACK:" will be overlayed with storage.
;
;	Execute file VEDIT.INI in text register 0.
;
				;{BEGIN}
AUTEXE:	TST$	INIFLG		;Look for VEDIT.INI?
	RZ			;No, return now
	LXI	H,VDTMSG	;HL-> "VEDIT.INI".
	CALL	OPNSPX		;Open VEDIT.INI file
	RC			;Return if file not found
	CALL	GETCR4		;Set for register 0.
	CALL	RLCMD0		;Read the file into register 0.
	JMP 	MCMD1		;Setup to execute register.
				;Register will be executed at MAIN.
;
; PRCPRM - Process invocation line parameters.
;
				;{BEGIN}
PRCPRM:	CALL	SETPRM		;Point GET/PUT at VEDIT parameter line
	CALL	SETIOX		;Setup edit I/O FCB's
	JMP	POPCMD
;
;
; SETPRM - Set GET/PUT -> to VEDIT command-line parameters. [02/20/86]
;
				;{PRCPRM}
SETPRM:	LXI	H,DEFDMA	;HL-> VEDIT parameter line
	MOV	E,M		;Get # chars in the line
	MVI	D,0		;DE = # chars in the line
	INX$	H		;HL-> start of the string
	XCHG			;DE-> start of the string = new GET ptr
	DAD	D		;HL-> past end of the string
	MVI	M,CR		;Ensure filename terminator
	INX$	H		;HL = new PUT ptr
	JMP	SETCM2
	.PAGE
;
; BEGIN - Initialize variables, screen, and start editing
;
;	  NOTE:	Much of this area is overwritten with storage area *******
;
BEGIN:	XTSTACK			;Setup temporary stack pointer
	CALL	DOSINI		;Init DOS dependent stuff; call SAVUSR()
				;Handle CP/M, MP/M selection
	CALL	INITRT		;Init main variables and text markers
				;TXTRWF needs to be set before MALLOC()
				;Resets INFLG which overlays DOSINI
;
;	Init text register pointers and lengths.
;
	LXI	H,REGTBL	;HL-> text register table
	MVI	B,(REGTEN-REGTBL)/2  ;Zero out PTRs and lengths
..1:	CALL	ZER%HL		;Zero (HL) and HL++
	DJNZ	..1		;Loop for all registers
;
	LHLD	BASE+6		;Get address of BDOS
	MVI	L,0		;Clear low byte
	SHLD	MIDBAS		;MALLOC() needs this set
;
	LXI	B,80		;Start with command buffer of 80 bytes
				;HL->
	CALL	MALLHL		;HL-> Begin of allocated space
	SHLD	CMDBAS		;Save -> to allocated space
	SHLD	CMDGET		;Init GET PTR
	DCX$	D		;Adjust
	SDED	CMDMAX		;Save -> last byte of command space
;
	CPIB$	DEFFCB+1,' '	;Is a file specified?
	BEQ	..2		;No, don't go into visual mode
	TST$	ATVSSW		;Begin in visual mode?
	JRZ	..2		;No, branch
	MVI	M,'V'		;Yes, enter "V" command
	INX$	H		;Bump PTR
..2:	SHLD	CMDPUT		;Save CMDPUT
;
;	Allocate space for LINTBL, SCRCNT and SCRFCB
;	Note that these pointers DO NOT ADJUST during FREE()
;
	LBCD	PYLINE
	MVI	B,00		;BC = number of physical lines
	CALL	MALLOC		;Allocate space
	SHLD	LINTBL		;Set LINTBL
	CALL	MALLOC
	SHLD	SCRCNT		;Set SCRCNT
	CALL	MALLOC
	CALL	MALLOC		;Need PYLINE words, tricky huh
	SHLD	SCRFCB		;Set SCRFCB
	SHLD	MAXMEM		;Pointers above MAXMEM do not adjust
;
;	Make KEYTBL into text register
;
	MVIB$	REGNUM,REGKEY	;The decode table is treated as a register
	LDED$	KEYTEN		;DE-> past end of table
	LXI	H,KEYTBL	;HL-> begin of table
	CALL	MAKCPY		;Create the text register
	CALL	RSTKEY		;Set pointers to KEYTBL, reset KEYPTR
;
;	Make print headers into text registers
;
	IF	WORDP, [
	MVIB$	REGNUM,REGHED	;Set register # for header
	LXI	H,PXHEAD	;HL-> begin of table
	LXI	D,PXSUBH	;DE-> past end of table
	CALL	MAKCPY		;Create the text register
	MVIB$	REGNUM,REGSHD	;Set register # for sub-header
	LXI	H,PXSUBH	;HL-> begin of table
	LXI	D,PXFOOT	;DE-> past end of table
	CALL	MAKCPY		;Create the text register
	MVIB$	REGNUM,REGFOT	;Set register # for footer
	LXI	H,PXFOOT	;HL-> begin of table
	LXI	D,PXHEND	;DE-> past end of table
	CALL	MAKCPY		;Create the text register
	]
;
;	Initialize screen driver and put up sign-on message
;
	CALL	VSTART		;Setup hardware
				;CRT - enable status line, keypad mode
	CALL	RSTCON		;Set to internal console output
	CALL	WININI		;Init window parameters, clear screen
;
	LXI	H,VERSMS	;HL-> VEDIT version
	CALL	PCRMSG		;Print it
	LXI	H,INIMS1	;HL-> init message
	CALL	PCRMSG		;Print it
;
;
;	NOTE -	following initializes variables stored at OVERLY.  WATCH OUT
;
	MOVB	TERMCH+1,WILDCH	;Setup wildcard char from INSTALL location
	MVIB$	TARGST,SRCEOS	;Empty search string
;
;	Init Text markers and BLMVEN to 0000 => HOME
;
	LXI	H,PNTTBL	;Point to table
	MVI	B,PNTMAX+1	;Number of entries plus BLMVEN
..3:	CALL	ZER%HL		;Value of 0000 will set to HOME
	DJNZ	..3		;Loop for all markers
;
;	Begin File Operations
;
STACK:				;This is the top of operating stack
;
	XOSTACK			;Set operation stack NOW
	CALL	PRCPRM		;Process invocation line parameters
;
	IF	FULL, [
	CALL	AUTEXE		;Load VEDIT.INI into R0, set CMD ptrs to execute it
	]

	CALL	OPNIOX		;Open I/O files for editing, do auto-read
;
TOCMD:				;{CLSERR,RESTRT,EZCMD}
BEGIN6:	LXI	SP,STACK	;Make sure stack set correctly
	CALL	MAIN		;Do editing
	IF	POLLING, [
	MVIM	CHKKEY,0C9H	;@@Allow typeahead to CPM
	]
	CALL	CLOSER		;Close files
				;{EQCMD}
EXITV:	CALL	VEXIT		;Reset hardware, position cursor bottom line
	JMP	EXIT		;Return to CP/M
	.PAGE
;
;	Init main variables.
;
				;{BEGIN,RESTRT}
INITRT:	MVIB$	CNTBFL,1	;Make sure visual line # will be counted

	LHLD	TXTBAS		;HL-> begin text buffer
	SHLD	TXTRWF
	SHLD	TXTCEL
	SHLD	EDTPTR
	SHLD	TXTFLR
	MVI	M,EOF		;Init buffer to empty
	DCX$	H		;HL-> before buffer
	MVI	M,LF		;Make sure we have a LF
;	JMP	FILINI
;
; FILINI - Initialize all file flags and variables
;
				;{INITRT^,OPNIOX}
FILINI:	LXI	H,INITTB	;HL-> initial values
	LXI	D,FIRINI	;DE-> variables to be set
	MVI	C,LASINI-FIRINI ;C = # bytes to be moved
	CALL	MOVE		;Move values to variables
;
	CLR	REVFLG
	STA	INFLG		;This one in overlay space
	STA	OUTFLG
	STA	RENFLG
	STA	AUXFLG
				;{CLOSER,PRVSEC}
CNTINI:	MVIW$	CNTOUT,0000	;Clear # of lines written out
	RET
