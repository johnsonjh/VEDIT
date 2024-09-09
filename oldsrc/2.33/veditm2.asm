	.PAGE
	.TITLE	'VEDIT-M2'
;************************************************
;*						*
;*	Memory Mapped Logical Functions		*
;*						*
;************************************************
;
; Copyright (C) 1986 by CompuView Products, Inc.
;			1955 Pauline Blvd.
;			Ann Arbor, MI 48103
;
;	Written by: Theodore J. Green
;
;	Last Change: Ted - Sept. 9, 1986 - Attribute in UPDONE()
;		     Tom - Sep. 10 - RVBCUR macro for IBM PC.
;		     Ted - Oct. 16 - VSEXIT() restores original color for PC
;
; VSTART - Perform any hardware dependent initialization.
;	   Send out enough CR-LF to put DOS cursor on bottom line
;
				;{BEGIN}
	BPROC	VSTART
VSTART:
;
	IFNOT	IBMPC, [
	LDA	PYLINE		;Number of screen lines
	SUI	5		;Must be at least 5 lines on screen
..1:	CALL	CRLF		;(Saves A)
	DCR	A
	JRNZ	..1
	]
;
;	Initialize Hardware for Visual entry.
;
	CALL	HCRSOF		;Turn any hardware cursor off
SCRINI:	JMPR	#END
	DS	18		;Total of 20 init bytes
#END:	RET
	EPROC	VSTART
;
; VEXIT - Get ready to exit VEDIT.
;
				;{BEGIN}
VEXIT:	LD16	H,PYLINE	;L = last screen line
	CALL	PHYST0		;Position cursor for clean exit
	CALL	HCRSON		;Turn any hardware cursor back on
;
	IF	P8086, [
	IF	IBMPC OR IBMEMU, [
	MVI	A,SPACE		;Write a space
	MVI	L,ORGCOL	;Get original screen attribute
	MVI	H,0		;Page 0
;6	MOV	AH,9		;Write char/attribute
	INT	10H
	]
	]
	RET

	.PAGE
	IF	WINDOO, [
;
; UPDONE - Show cursor position with reverse video character
;
				;{VEND}
UPDONE:	CALL	CHKCUR		;Update cursor pos
	MOVW	UPDCUR,PHYVER	;Save cursor pos
	CALL	SCRIN		;Bank select the screen in
	LHLD	SCRPNT		;Get current cursor pos

	IFNOT	P8086, [
	MOV	A,M		;Get character at cursor pos
	STA	UPDSVC		;Save it
	XRI	80H		;Switch top bit
	MOV	M,A		;Put secondary cursor on screen
	] [
	CALL	XGTCHR		;Get char/attribute at cursor pos
;6	MOV	UPDSVC,AX	;Save it
;6	RVBCUR			;Get Reverse-Block cursor
	LXI	B,1		;Only inject 1 char
	CALL	XSTCHR		;Put secondary cursor on screen
	]
	RET			;Return to command mode
;
				;{VEDCMD,WINOUT}
UPDRTC: TSTW	UPDCUR		;Is old cursor on screen?
	RZ			;No, return
	CALL	PHYSET		;Yes, position on screen
	CALL	SCRIN		;Bank select the screen in
	LHLD	SCRPNT		;Get current cursor pos

	IFNOT	P8086, [
	LDA	UPDSVC		;A = character there
	MOV	M,A		;Restore char on screen
	] [
;6	MOV	AX,UPDSVC	;AL = char, AH = its attribute
	LXI	B,1		;Only injecting 1 char
	CALL	XSTCHR		;Restore char on screen
	]

	MVIW$	UPDCUR,0000	;Clear position/flag
	RET
	]			;<IF WINDOO>
	.PAGE
;********************************************************
;*							*
;*	Screen and Line Clear Routines			*
;*							*
;********************************************************
;
; CLRSCR - Clear entire screen.
;
				;{VEXIT}
CLRSCR:	CALL	WIZOOM		;CALL WIZOOM would clear screen
				;{PRTFF,YWCLOS,WININI}
CLRWIN:	MOVB	WWUSLN,WWNLIN	;Ensure all lines are cleared
	CALL	WINHOM		;Home cursor
	JMPR	WINEOS		;Clear the entire window
;
; CLRSC2 - Clear rest of window from begin of next line # in Reg. A.
;
				;{UPDSCR}
CLRSC2:
	LXI	H,WWUSLN	;HL-> # screen lines not clear
	CMP	M		;Is screen below last line clear?
	RNC			;Yes, return
				;This prevents problems at end of window
;6	IF	IBMPC OR IBMEMU
;6	PUSH	AX
;6	CALL	SCROFF		;Turn screen off if Color 25x80
;6	POP	AX
;6	ENDIF

	PUSHA			;Save # of last used line
	INR	A		;Get line # of first line to clear
	MOV	L,A		;Put in L
	CALL	WINST0		;Position to column 00 of next line
	CALL	WINEOS		;Perform the EOS

;6	IF	IBMPC OR IBMEMU
;6	CALL	SCRON		;Turn screen back on if Color 25x80
;6	ENDIF

	POPA			;# last used line
	STA	WWUSLN		;Set window WWUSLN
	RET
;
; WINEOS - Clear to end of window from cursor position
;	   This is done by using multiple EOLs.
;
	BPROC	WINEOS
WINEOS:	CALL	SAVCUR		;Save cursor position
	LDA	WWUSLN		;A = last line that needs clearing
	LXI	H,WINVER	;HL-> current line
	SUB	M		;Compute # lines to clear
	RC			;Do nothing if WWUSLN < WINVER
	INR	A		;Adjust
	MOV	B,A		;Save count in B
	MOV	A,M		;Get WINVER
	STA	WWUSLN		;Update WWUSLN for EOS
;
	LD16	H,WINVER	;HL = desired write pos
..2:	PUSH	B		;Save count in B
	CALL	WINSET		;Position the cursor
	CALL	WIEOL2		;Erase the line
				;Note - first EOL may be partial line
	LD16	H,WINVER	;HL = previous pos
	INR	L		;Move to next line
	MVI	H,00		;Now start from left margin
	POP	B		;B = remaining count
	DJNZ	..2		;Loop
	EPROC	WINEOS
;
RESCUR:	LHLD	SAVWIN		;Restore to position saved in SAVCUR()
	JMP	WINSET
;
SAVCUR:	LD16	H,WINVER
	SHLD	SAVWIN
	RET
;
;
; WINEOL - Clear to end of window from cursor position.
;
WINEOL:	LXI	H,WINVER	;Get current cursor line #
	LDA	WWUSLN		;Get # window lines used
	CMP	M		;Is rest of current line clear?
	RC			;Yes, return
;
;	Do an EOL by sending out spaces to end of line.
;
				;{WINEOS.WINEOL^,WISCLR}
WIEOL2:	LDA	WWENCO		;Get last physical column for window
				;{PHYEOL}
WIEOL3:	LXI	H,PHYHOR	;HL-> current physical column
	SUB	M		;Compute # pos. to clear
	RC			;DON'T if PHYHOR past end of window
	INR	A		;Adjust
	MOV	C,A		;Put count in BC
	MVI	B,00		;High order = 00
	CALL	SCRIN		;Make sure screen bank selected in
	LHLD	SCRPNT		;HL-> first pos. to clear
	LDA	FILLCH		;Clear with Space char

	IFNOT	P8086, [
	JMP	FILL		;Fill with spaces
	] [
;6	MOV	AH,WWBKAT
	JMP	XSTCHR
	]
;
; PHYEOL - Clear to end of screen from cursor position.
;
				;{STACLR}
PHYEOL:	LD8	PYLLEN		;Get display line length
	DCR	A		;A = last column counting from 0
	JMPR	WIEOL3		;Merge above
	.PAGE
;********************************************************
;*							*
;*	Hardware Window Scroll Routines			*
;*							*
;********************************************************
;
; WISFWD - Scroll window forward and clear bottom line
;
				;{WINOUT,WTSCRL}
	BPROC	WISFWD
WISFWD:	CALL	SCRIN		;

;6	IF	IBMPC OR IBMEMU
;6	CALL	SCROFF
;6	CALL	#1
;6	JMP	SCRON
;6	ENDIF

..1:	MVI	A,1
..2:	LXI	H,WWNLIN
	CMP	M
	BGE	WISCLR		;Branch if all lines moved up
	MOV	L,A		;L = "TO" line #
	PUSHA			;Save line #
	PUSHA			;Save again
	CALL	WINST0		;SCRPNT-> begin "TO" line
	LHLD	SCRPNT
	POPA			;A = line #
	PUSH	H		;Save Destination
	INR	A		;Next line
	MOV	L,A		;L = "FROM" line #
	CALL	WINST0		;SCRPNT-> begin "FROM" line
	LHLD	SCRPNT		;HL-> source
	POP	D		;DE-> destination
	LD16	B,WWLLEN	;BC = window line length
	MVI	B,00		;Only 8 bit count
;6	ADD	CX,CX		;IBM PC -- Mult. by 2
;6	PUSH	DS
;6	PUSH	ES
;6	POP	DS
	CALL	RTLDIR		;Make the window line copy
;6	POP	DS
	POPA			;A = original line #
	INR	A		;Next line
	JMPR	..2		;Loop
	EPROC	WISFWD
;
WISCLR:	MOV	L,A
	CALL	WINST0		;Address bottom window line
	JMP	WIEOL2		;Erase it
;
; WISLBK - Scroll window backward and clear top line
;
	BPROC	WISLBK
WISLBK:	LDA	WWNLIN
	CALL	SCRIN		;

;6	IF	IBMPC OR IBMEMU
;6	PUSH	AX
;6	CALL	SCROFF
;6	POP	AX
;6	CALL	#1
;6	JMP	SCRON
;6	ENDIF

..1:	CPI	2
	BLT	WISCLR		;Branch if all lines moved down
	MOV	L,A		;L = "TO" line #
	PUSHA			;Save line #
	PUSHA			;Save again
	CALL	WINST0		;SCRPNT-> begin "TO" line
	LHLD	SCRPNT
	POPA			;A = line #
	PUSH	H		;Save Destination
	DCR	A		;Previous line
	MOV	L,A		;L = "FROM" line #
	CALL	WINST0		;SCRPNT-> begin "FROM" line
	LHLD	SCRPNT		;HL-> source
	POP	D		;DE-> destination
	LD16	B,WWLLEN	;BC = window line length
	MVI	B,00		;Only 8 bit count
;6	ADD	CX,CX		;IBM PC -- Mult. by 2
;6	PUSH	DS
;6	PUSH	ES
;6	POP	DS
	CALL	RTLDIR		;Make the window line copy
;6	POP	DS
	POPA			;A = original line #
	DCR	A		;Previous line
	JMPR	..1		;Loop
	EPROC	WISLBK
	.PAGE
;********************************************************
;*							*
;*	Cursor Positioning Routines			*
;*							*
;********************************************************
;
; PHYSET - Set the screen pointer SCRPNT from the desired
;	   Vert. & Horz. position in L and H.
;
				;Cursor position for PIICEON
PHYST0:	MVI	H,00		;Set to column 00
PHYSET:	PUSH	D		;- 8086
	LD16	D,PHYVER	;Get current position
	CALL	CMHLDE		;Already at desired position?
	POP	D		;- 8086
	RZ			;Yes, return now
;
				;{UPDRTC}
PHYFRC:	ST16	PHYVER,H	;Save physical cursor pos
				;{CLRLIN - don't change PHYVER,PHYRST - force}
PICSET:	PUSH	B		;Save character in C
	MOV	B,H		;Save column in B
	MOV	A,L		;Get row #
	CALL	VERSC1		;HL-> begin of desired line
	MOV	A,B		;Get column pos
	CALL	ADDAHL		;Add column pos

	IF	VWRD, [
	MOV	A,B
	CALL	ADDAHL
	]

	SHLD	SCRPNT		;Save as screen pointer
	POP	B		;Restore char. in C
	RET

