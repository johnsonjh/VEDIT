	.PAGE
	.TITLE	'VEDIT-M2'
;************************************************
;*						*
;*	Memory Mapped Logical Functions		*
;*						*
;************************************************
;
; Copyright (C) 1987 by CompuView Products, Inc.
;			1955 Pauline Blvd.
;			Ann Arbor, MI 48103
;
;	Written by: Theodore J. Green
;
;	Last Change: Ted - Sept. 9, 1986 - Attribute in UPDONE()
;		     Tom - Sep. 10 - RVBCUR macro for IBM PC.
;		     Ted - Oct. 28 - VSEXIT() restores original color for PC
;			 - Jan. 26 - VSTART and VSEXIT for CCPM86
;			 - Feb. 18 - 8080 MM support
;
; VSTART - Perform any hardware dependent initialization.
;	   Send out enough CR-LF to put DOS cursor on bottom line
;
				;{BEGIN}
	BPROC	VSTART
VSTART:
;
	IF	IBMPC & CCPM86, [
	CALL	MPMCHK		;;Is this CCP/M-86?
	JRZ	#2		;;No, branch
	] [
	IFNOT	IBMPC, [
	LDA	PYLINE		;Number of screen lines
	SUI	5		;Must be at least 5 lines on screen
..1:	CALL	CRLF		;(Saves A)
	DCR	A
	JRNZ	..1
	]
	]
;
;	Initialize Hardware for Visual entry.
;
..2:	CALL	HCRSOF		;Turn any hardware cursor off
SCRINI:	JMPR	..END
	DS	18		;Total of 20 init bytes
..END:	RET
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

;6	IF	IBMPC

;6	IF	CCPM86
;6	CALL	SCRIN		;;Set ES, lock console
;6	MOV	AL,SPACE	;Write a space
;6	MOV	AH,ORGCOL
;6	CALL	XRSTCH		;;Write char/attribute
;6				;;Unlock console
;6	ELSE
;6
;6	MOV	AL,SPACE	;Write a space
;6	MOV	BL,ORGCOL	;Get original screen attribute
;6	MOV	BH,0		;Page 0
;6	MOV	CX,1		;;Just a single byte
;6	MOV	AH,9		;Write char/attribute
;6	INT	10H
;6	ENDIF			;<IF CCPM86 - ELSE>
;6	ENDIF			;<IF IBMPC>
;6
;6	IF	TIPC
;6	MOV	ES,SCRBAS	;;Setup for screen segment
;6	MOV	DI,1800H	;;Offset to attribute latch
;6	MOV	AH,ORGCOL	;;AH = original screen attribute
;6	MOV	EBP [DI],AH	;;Restore screen attribute
;6	ENDIF
	] [
	CALL	SCROUT		;;Bank select screen out
	]			;<IF P8086 - ELSE>
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
	CALL	XSREAD		;Get char/attribute at cursor pos
;6	MOV	UPDSVC,AX	;Save it
;6	RVBCUR			;Get Reverse-Block cursor
	LXI	B,1		;Only inject 1 char
	CALL	XSWRIT		;Put secondary cursor on screen
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
	CALL	XSWRIT		;Restore char on screen
	]
				;{CLRWIN}
UPDRST:	MVIW$	UPDCUR,0000	;Clear position/flag
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
CLRWIN:	CALL	UPDRST		;;Flag that update cursor gone
	MOVB	WWUSLN,WWNLIN	;Ensure all lines are cleared
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
	PUSHA			;Save # of last used line
	INR	A		;Get line # of first line to clear
	MOV	L,A		;Put in L
	CALL	WINST0		;Position to column 00 of next line
	CALL	WINEOS		;Perform the EOS
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
				;{WINEOS,WINEOL^,WISCLR}
WIEOL2:	LDA	WWENCO		;Get last physical column for window
	LXI	H,PHYHOR	;HL-> current physical column
	SUB	M		;Compute # pos. to clear
	RC			;DON'T if PHYHOR past end of window
	INR	A		;Adjust
	MOV	C,A		;Put count in BC
	MVI	B,00		;High order = 00
	LDA	FILLCH		;Clear with Space char
	CALL	SCRIN		;Make sure screen bank selected in

	IFNOT	P8086, [
	MOV	L,A		;;Save fill char
	TST$	WWBKAT		;;Reverse video erase-attribute ?
	MOV	A,L		;;AL = fill char
	JRZ	..2		;;No, branch
	ORI	80H		;;Yes, get reverse bit
..2:	LHLD	SCRPNT		;HL-> first pos. to clear
	JMP	FILL		;Fill with spaces
	] [
	LHLD	SCRPNT		;HL-> first pos. to clear
;6	MOV	AH,WWBKAT	;AH = erase-attribute
	JMP	XSWRIT
	]
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

;6	IF	IBMPC
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

;6	IF	VWRD
;6	ADD	CX,CX		;IBM PC -- Mult. by 2
;6	ENDIF
;6
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

;6	IF	IBMPC
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

;6	IF	VWRD
;6	ADD	CX,CX		;IBM PC -- Mult. by 2
;6	ENDIF
;6
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
PHYST0:	MVI	H,00		;Set to column 00
PHYSET:
	IFNOT	P8086, [
	PUSH	D		;- 8086
	LD16	D,PHYVER	;Get current position
	CALL	CMHLDE		;Already at desired position?
	POP	D		;- 8086
	] [
;6	CMP	BX,WPT PHYVER	;Already at desired position?
	]

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

;6	IF	VWRD, [
;6	MOV	A,B
;6	CALL	ADDAHL
;6	]

	SHLD	SCRPNT		;Save as screen pointer
	POP	B		;Restore char. in C
	RET

