	.PAGE
	.TITLE	'VEDIT-CP'
;************************************************
;*						*
;*		Print Routines			*
;*						*
;************************************************
;
; Copyright (C) 1986 by CompuView Products, Inc.
;			1955 Pauline Blvd.
;			Ann Arbor, MI 48103
;
;	Written by: Theodore J. Green
;
;	Last Change: Ted - Feb. 05, 1986
;
PCMD:	CALL	NXCMCH		;Get second char
	ANI	5FH		;Upper case it
	CPI	'R'		;Print?
	JZ	PRCMD
	CPI	'P'
	JZ	PRCMD
	CPI	'S'		;Settings?
	JNZ	C2BR8K		;No, bad command
;
;	Display/change print settings
;
PSCMD:	LXI	D,PPXTBL	;DE-> parameter table
	MVI	B,PPXTNM	;B = # of entries
	JMP	ESCMD1		;Merge into ES command
;
;	EO - Output text to line printer.
;
EOCMD:	CALL	PRRANG		;HL and DE -> range to print
	JMP	LSTBLK		;Print text from (DE) to (HL)
;
;
;	Perform the paginated print
;
PRCMD:	CALL	PRRANG		;Determine begin and end PTR
				;PTRs are saved in PPBEGN and PPEND
;	TST$	MINUS		;Non-paginated print?
;	JNZ	EOCMD		;Yes, branch
;
	CALL	LSTSET		;Setup ROUTAD and LSTFLG for printing
	MVIW$	NXPGNM,1	;Reset page number
;
;	Print top of a new page
;
PRCMD1:	LDED$	PPEND		;DE = End PTR
	LHLD	PPBEGN		;HL = Begin PTR
	CALL	CMHLDE		;Reached the end?
	JRNC	PRCMD9		;Yes, all done (Branch if ==)
;
	MVIB$	PPLNCN,0	;Reset line count
	MOVW	PAGENM,NXPGNM	;Set new page number
	INCW$	NXPGNM		;Increment (next) page number
	CALL	PRHEAD		;Print top margin and header
	CALL	PRSUBH		;Print subheader and header margin
	CALL	PRCKFO		;Reached footer margin already?
	JC	PRERRM		;Yes, give error
;
;	Print main body of text
;
PRCMD2:	CALL	PRCKFO		;Reached bottom of page?
	JRC	PRCMD5		;Yes, branch to print footer
;
	LHLD	PPBEGN		;HL-> text to print
	CALL	DOTCMD		;Is this a dot command?
	JRZ	PRCMD2		;Yes, PPBEGN-> next line
;
	CALL	PRPHYS		;Print physical indent
	MVIW$	LOGPOS,1	;Reset so tabs expand correctly
	LHLD	PPBEGN		;HL-> begin of text again
	MOV	D,H		;Save BEGIN PTR in DE
	MOV	E,L
	CALL	NEXTLF		;HL-> next LF
	JRC	PRCMD3		;Branch if EOF
	INX$	H		;HL-> past LF
PRCMD3:	PUSH	H
	CALL	TYPLST		;Print the line from (DE) to (HL)
				;Note - will print NULL lines after EOF reached
;
	LXI	H,PPLNCN	;HL-> line counter
	INR	M		;Increment it
	LDA	PPXLS		;Get line spacing
	DCR	A		;One less
	MOV	B,A		;Put into B
	CALL	PRSPAC		;Perform double spacing if desired
;
	POP	H
	SHLD	PPBEGN		;Update BEGIN PTR
	LDED$	PPEND		;DE = End PTR
	CALL	CMHLDE		;Reached the end?
	JRC	PRCMD2		;No, loop
;
;	Print bottom of page, footer, etc
;
PRCMD5:	CALL	PRPAGE		;
	JMPR	PRCMD1		;Loop back
;
PRCMD9:	JMP	ENDLST		;Reset ROUTAD and LSTFLG
;
;
PRERRM:	LXI	H,PPMERR	;HL-> error message
	JMP	MSGBRK		;Give error and BREAK out
;
;
; PRPAGE - Skip to bottom of page and print footer.
;
PRPAGE:	MVI	B,0FFH		;B = value to reach bottom of page
	CALL	PRSPAC		;Space to bottom of page
	LDA	PPXFM		;A = footer margin
	CALL	PRTBLK		;Print blank lines
	JMP	PRFOOT		;Print entire footer
;
; PRSPAC - Print %B blank lines or until bottom of page reached
;
PRSPAC:	MOV	A,B		;Is count zero now?
	ORA	A
	RZ			;Yes, return now
	CALL	PRCKFO		;Reached bottom of page?
	RC			;Yes, return
	CALL	CRLF		;Print blank line
	LXI	H,PPLNCN	;HL-> line counter
	INR	M		;Increment it
	DCR	B		;Decrement count
	JMPR	PRSPAC		;Loop until B = 0
;
; PRCKFO - Check if reached line on which to begin footer margin
;	   Return: 'C' if line reached.
;
PRCKFO:	LDA	PPXPL 		;A = page length
	LXI	H,PPXFM
	SUB	M
	LXI	H,PPXBM
	SUB	M		;A = line number on which to start margin
	MOV	C,A		;Save in C
	LDA	PPLNCN		;A = # line printed so far
	CMP	C		;Reached footer margin yet?
	CMC			;Return 'C' if footer margin reached
	RET
;
; PRPHYS - Print spaces which make up physical indent
;
PRPHYS:	LDA	PPXPI		;A = physical indent
	JMP	OUTNSP		;Print the spaces
;
;
;
PRFOOT:	LDA	PPXBM		;A = bottom margin
	LHLD	PPFOOT		;HL-> footer
PRFOO1:	DCR	A		;Account for footer line
	RM			;Return if .BM == 0
	PUSHA			;Save
	CALL	PSHEAD		;Print footer
	POPA			;A = blank lines for .BM
	JMP	PRTBLK		;Print them
;
PRSUBH:	LDA	PPXHM		;A = header margin
	LHLD	PPSUBH		;HL-> sub header
	JMPR	PRFOO1		;Merge above
;
PRHEAD:	LDA	PPXTM		;A = top margin
	DCR	A		;Account for header line
	RM			;Return if no header line
	CALL	PRTBLK		;Blank lines for top margin
	LHLD	PPHEAD		;HL-> header
	JMP	PSHEAD		;Print header
;
; PRTBLK - Print %A blank lines, updating PPLNCN.
;
PRTBLK:	ORA	A		;Is count zero?
	RZ			;Yes, done
	CALL	CRLF		;Send blank line
	LXI	H,PPLNCN	;HL-> line counter
	INR	M		;Increment count
	DCR	A
	JMPR	PRTBLK
;
; PRRANF - Determine command range and set PPBEGN and PPEND.
;	   Return: HL = PPEND, DE = PPBEGN.
;
PRRANG:	CALL	GETITR		;Is the command "0EO"?
	LDED$	TXTBGN		;Assume yes, DE-> begin of print
	LHLD	EDTPTR		;	     HL-> end of print
	JRZ	PRRAN1		;Yes, print from begin of text
	CALL	FNDLIN		;HL-> one block end point
	LDED	EDTPTR		;+DE-> other end point
PRRAN1:	CALL	POSDIF		;Make sure HL > DE
	SHLD	PPEND		;Save end PTR
	SDED	PPBEGN		;+Save Begin PTR
	RET
;
; DOTCMD - Dot command processor for all V-PRINT dot commands in file
;	   Enter: HL-> beginning of line
;	   Exit:  'Z' and 'NC' if dot command processed, HL-> begin of next line
;		  'NZ' and 'NC' if not dot command, HL unchanged
;
DOTCMD:	MOV	A,M		;Get char at line begin
	CPI	'.'		;Is it begin of dot command?
	RNZ			;No, return
;
	INX$	H		;HL-> command letters
	PUSH	H		;Save
	INX$	H
	INX$	H		;HL-> past command
	SHLD	DOTBGN		;Save as -> to parameters
	POP	H
	PUSH	H
	CALL	NEXTLF		;HL-> LF at end of line
	JRC	DOTCM1		;Branch if EOF
	INX$	H		;HL-> past LF
DOTCM1:	SHLD	PPBEGN		;Save PTR for further printing
	POP	H		;Restore
;
	CALL	MVINHL		;HL = two command letters
	MOV	A,H		;Convert both letters to UC
	ANI	05FH
	MOV	D,A
	MOV	A,L
	ANI	05FH
	MOV	E,A		;DE = command to search for
	LXI	H,DOTDEC	;HL-> decode table
	CALL	LOOKTB		;Look for DE in table (HL)
	JRC	DOTCM8		;Branch if not found
;
;	Get any following parameter
;
	PUSH	H		;Save
	LHLD	DOTBGN		;HL-> possible parameter
	CALL	ATOI		;Convert to decimal
	TST$	XNMFLG		;Was exlicit number found?
	JRNZ	DOTCM4		;Yes, branch
	LXI	B,1		;Else default to 1
DOTCM4:	SBCD	DOTPAR		;+Save it
	POP	H		;Restore address
	CALL	CALLHL		;Call the routine
;
DOTCM8:	CMP	A		;Set 'Z'
	RET
;
;	Routines to process individual DOT commands
;
DOTPA:	CPIB$	DOTPAR,1	;Is this a page # change?
	JRZ	DOTPA1		;No, start a new page
	MOVW	NXPGNM,DOTPAR	;Yes, set new page number
;
DOTPA1:	MVI	B,0FFH		;B = value to reach bottom of page
	CALL	PRSPAC		;Space to bottom of page
	RET			;This will force new page to start
;
DOTCP:	LDA	PPLNCN		;Get current line count
	MOV	B,A		;Save it
	LDA	DOTPAR		;Get parameter
	ADD	B		;A = desired line number
	STA	PPLNCN		;Save as line count
	CALL	PRCKFO		;Would this reach page bottom?
	JRC	DOTPA		;Yes, start a new page
	MOV	A,B		;Get old value back
	STA	PPLNCN		;Restore it
	RET			;All is OK
;
DOTSP:	LHLD	DOTPAR		;Get parameter
	LDA	PPXLS		;Get line spacing value
	MOV	E,A		;DE = spacing value
	MVI	D,00
	CALL	MULTIP		;HL = result
	MOV	A,L
DOTSP1:	MOV	B,A		;Put count in B
	JMP	PRSPAC		;Print blank lines or until bottom of page
;
DOTTF:	LHLD	DOTBGN		;HL-> past dot command
	CALL	SKIPWT		;Get first non-blank char
	CALL	MVINHL		;L = tab fill char
	MOV	A,L
	STA	TABFIL		;Set new tab fill
	RET
;
DOTBL:	LDA	DOTPAR		;Get 8 bit parameter
	JMPR	DOTSP1		;Merge above
;
;	DOT commands to set new header lines
;
DOTHE:	MVI	A,REGHED	;A = header's register #
	JMPR	DTHEAD		;Set new header
;
DOTSH:	MVI	A,REGSHD	;A = sub-header's register #
	JMPR	DTHEAD		;Set new subheader
;
DOTFO:	MVI	A,REGFOT	;A = footer's register #
;
; DTHEAD - Setup new header line in storage, if valid
;
DTHEAD:	STA	REGNUM		;Set register number to change
	LHLD	DOTBGN		;HL-> text following dot command
	CALL	SKIPWT		;HL-> first non-blank
	MOV	B,M		;Get delimiter
	MVI	C,3		;Need three fields
	INX$	H		;HL-> begin of first field
	LXI	D,ACTBUF	;Use ACTBUF as work line
DTHEA1:	MOV	A,M		;Get next char
	INX$	H		;++ptr
	CMP	B		;Delimiter?
	JRZ	DTHEA2		;Yes, branch
	STAX	D		;No, save char
	INX$	D		;++ptr2
	CALL	CMPCLE		;Reached premature end of line?
	JRNZ	DTHEA1		;No, continue
;
;	ERROR - Bad header specified
;
	LXI	H,HDERRM	;HL-> error message
	JMP	MSGBRK		;Give error and BREAK out
;
DTHEA2:	XRA	A		;Get zero
	STAX	D		;Put zero into work line
	INX$	D		;++ptr2
	DCR	C		;Have three fields been processed?
	JRNZ	DTHEA1		;No, continue
;
;	Copy new header to appropriate text register
;
	LXI	H,ACTBUF	;HL-> work area
				;DE-> past end of work area
	JMP	MAKCPY		;Copy text to the register area
;
;	DOT commands which change simple parameter
;
DOTTM:	MVI	A,1		;Offset in PPXTBL
	JMPR	DOTSET
;
DOTHM:	MVI	A,2		;Offset in PPXTBL
	JMPR	DOTSET
;
DOTFM:	MVI	A,3		;Offset in PPXTBL
	JMPR	DOTSET
;
DOTBM:	MVI	A,4		;Offset in PPXTBL
	JMPR	DOTSET
;
DOTPL:	MVI	A,5		;Offset in PPXTBL
	JMPR	DOTSET
;
DOTPI:	MVI	A,6		;Offset in PPXTBL
	JMPR	DOTSET
;
DOTHI:	MVI	A,7		;Offset in PPXTBL
	JMPR	DOTSET
;
DOTRM:	MVI	A,8		;Offset in PPXTBL
	JMPR	DOTSET
;
DOTPW:	MVI	A,9		;Offset in PPXTBL
	JMPR	DOTSET
;
DOTLS:	MVI	A,10		;Offset in PPXTBL
	JMPR	DOTSET
;
DOTSS:	LXI	D,1		;Change to single spacing
DOTSS1:	MVI	A,10		;Offset in PPXTBL
	JMPR	DOTSE0
;
DOTDS:	LXI	D,2		;Change to double spacing
	JMPR	DOTSS1
;
; DOTSET - Change formatting numeric parameter
;	   A =  parameter number to change
;	   DOTPAR = new value
;
DOTSET:	LDED$	DOTPAR		;DE = new value
DOTSE0:	LXI	H,PPXTBL-1	;HL-> parameter table
	CALL	ADDAHL		;HL-> desired parameter
	MOV	C,M		;Save old value in C
	CPIB$	XRMINU,'-'	;Subtract from current value?
	JRNZ	DOTSE1		;No, branch
	MOV	A,C
	SUB	E
	MOV	M,A		;Save new value
	RET
;
DOTSE1:	CPIB$	XRMINU,'+'	;Add to current value?
	MOV	A,C
	JRNZ	DOTSE2		;No, branch
	ADD	E
	MOV	E,A
DOTSE2:	MOV	M,E		;Save new value
	RET
;
;
;
PSHEAD:	XCHG			;Save -> header in DE
	LXI	H,ACTBUF	;HL-> use ACTLIN as work area
	MVI	A,SPACE		;Fill work area with spaces
	LXI	B,255		;Max length is 255
	CALL	FILL		;Wow
	XCHG			;HL-> header
;
;	Process first field
;
PSHEA1:	CALL	PSFILD		;Expand first field
	PUSH	H		;Save -> second field
	LXI	H,BSTACK	;HL-> expanded field
	LXI	D,ACTBUF	;DE-> work area
	CALL	STRZCP		;Copy the string without the [00]
	POP	H		;HL-> second field
;
;	Process centered field
;
	CALL	PSFILD		;Expand second field
	PUSH	H		;Save -> third field
	LXI	H,BSTACK	;HL-> expanded field
	CALL	STRLEN		;HL = string length
	LDA	PPXPW		;Get page width .PW
	SUB	L
	RAR			;A = indent to center
	LXI	H,ACTBUF	;HL-> work area
	CALL	ADDAHL		;HL-> where to save centered text
	XCHG			;DE-> work area
	LXI	H,BSTACK	;HL-> expanded field
	CALL	STRZCP		;Copy the string without the [00]
	POP	H		;HL-> third field
;
;	Process third field
;
	CALL	PSFILD		;Expand second field
	LXI	H,BSTACK	;HL-> expanded field
	CALL	STRLEN		;HL = string length
	LDA	PPXPW		;Get page width .PW
	SUB	L
	LXI	H,ACTBUF	;HL-> work area
	CALL	ADDAHL		;HL-> where to save centered text
	XCHG			;DE-> work area
	LXI	H,BSTACK	;HL-> expanded field
	CALL	STRCPY		;Copy the string WITH the [00]
;
;	Print the header line
;
	LDA	PPXHI		;A = Header indent
	CALL	OUTNSP		;Print the spaces
	LXI	H,ACTBUF	;HL-> fully expanded header
	CALL	PRTSTR		;Send to printer
	MVI	A,1
	JMP	PRTBLK		;Send CRLF and update PPLNCN
;
; PSFILD - Process one header field, expanding "#" and "%".
;	   Enter: HL-> header field
;	   Retrn: BSTACK contains expanded field, terminated in [00]
;		  HL-> past [00]
;
PSFILD:	LXI	D,BSTACK	;DE-> destination
PSFIL1:	MOV	A,M		;Get next char
	INX$	H		;Bump PTR
	CPI	'#'		;Expand for page number?
	JRZ	PSFLPG		;Yes, branch
;	CPI	'%'		;Expand for file name?
;	JRZ	PSFLFN		;Yes, branch
	STAX	D		;No, store normal char (or [00])
	INX$	D		;Bump PTR
	ORA	A		;Was [00] reached?
	JRNZ	PSFIL1		;No, continue
	RET			;Yes, return
;
PSFLPG:	PUSH	H		;Save -> field
	LBCD$	PAGENM		;BC = page number
	CALL	ITOA		;HL-> ASCII string
PSFLP1:	MOV	A,M		;Get next char
	INX$	H		;Bump PTR
	CPI	SPACE		;Leading space?
	JRZ	PSFLP1		;Yes, throw it out
	JRC	PSFLP2		;Done when [00] reached
	STAX	D		;Save in BSTACK
	INX$	D		;Bump PTR
	JMPR	PSFLP1		;Continue
;
PSFLP2:	POP	H		;HL-> rest of field
	JMPR	PSFIL1		;Process rest of field
;
;
; DOTDEC - Decode table for DOT commands at begin of line
;
DOTDEC:	DC	'PA'
	DW	DOTPA
	DC	'BP'
	DW	DOTPA
	DC	'SP'
	DW	DOTSP
	DC	'HE'
	DW	DOTHE
	DC	'SH'
	DW	DOTSH
	DC	'FO'
	DW	DOTFO
	DC	'HM'
	DW	DOTHM
	DC	'TM'
	DW	DOTTM
	DC	'BM'
	DW	DOTBM
	DC	'FM'
	DW	DOTFM
	DC	'PI'
	DW	DOTPI
	DC	'HI'
	DW	DOTHI
	DC	'RM'
	DW	DOTRM
	DC	'PL'
	DW	DOTPL
	DC	'PW'
	DW	DOTPW
	DC	'LS'
	DW	DOTLS
	DC	'SS'
	DW	DOTSS
	DC	'DS'
	DW	DOTDS
	DC	'CP'
	DW	DOTCP
	DC	'TF'
	DW	DOTTF
	DB	00,00		;End of table
;
;
;
;
PPMERR:	DC	'CANNOT PRINT - PAGE LAYOUT ERROR' [00]
HDERRM:	DC	'BAD .HE, .SH OR .FO COMMAND' [00]
;
PPBEGN:	DSW	1		;PTR to text yet to be printed
PPEND:	DSW	1		;PTR past end of text to be printed
PPLNCN:	DS	1		;Count of lines printed on page
PAGENM:	DSW	1		;Current page number
NXPGNM:	DSW	1		;Next page number
;
DOTBGN:	DSW	1		;PTR to parameters following dot command
DOTPAR:	DSW	1		;Parameter following dot command
;
PPXTBL:
PPXTM:	DB	4		;Top margin includes header
PPXHM:	DB	3		;Header margin includes subheader
PPXFM:	DB	3		;Footer margin
PPXBM:	DB	5		;Bottom margin includes footer
PPXPL:	DB	66		;Page length
PPXPI:	DB	10		;Physical indent
PPXHI:	DB	10		;Header indent
PPXRM:	DB	72		;Right margin
PPXPW:	DB	72		;Page width
PPXLS:	DB	1		;Single spacing
PPXEND:
PPXTNM	=	PPXEND - PPXTBL

