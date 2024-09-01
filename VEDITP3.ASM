	.PAGE
	.TITLE	'VEDIT-P3'
;************************************************
;*						*
;*	PIICEON / TDL  Dependent Routines	*
;*						*
;************************************************
;
; Copyright (C) 1986 by CompuView Products, Inc.
;			1955 Pauline Blvd.
;			Ann Arbor, MI 48103
;
;	Written by: Theodore J. Green
;
;	Last Change: Ted - Mar. 22 - Windows
;			   Apr. 28 - Optimize scrolling
;			   May  18 - CLRSCR() now calls WIZOOM()
;			   July 28 - Clear screen with WWBKAT attribute
;			   Oct. 09 - Changes for new WINHOR and WINSET()
;			   Dec. 02 - KDISPY()
;
; VSTART - Perform any hardware dependent initialization.
;	   Send out enough CR-LF to put DOS cursor on bottom line
;
				;{BEGIN}
VSTART:	LDA	PYLINE		;Number of screen lines
	SUI	5		;Must be at least 5 lines on screen
..1:	CALL	CRLF		;(Saves A)
	DCR	A
	JRNZ	..1
	RET
;
; VEXIT - Get ready to exit VEDIT.  Screen may be cleared.
;
				;{BEGIN}
VEXIT:	LHLD	PYLINE		;L = last screen line
	JMP	PHYST0		;Position cursor for clean exit
	.PAGE
;************************************************
;*						*
;*	PIICEON and TDL Drivers			*
;*						*
;************************************************
;
;
	IF	PICVRS, [
VDBCTL	=	PICBAS		;VDB control port
VDBDAT	=	PICBAS + 1	;VDB data port
VDBXCF	=	00000000B	;X cursor set flag
VDBYCF	=	11000000B	;Y cursor set flag
ADDRG	=	PICBAS+2	;CRTC address reg
INTRG	=	PICBAS+3	;CRTC internal reg
TMPLRG	=	PICBAS+2	;Low order screen begin storage reg
TMPHRG	=	PICBAS+3	;High order screen begin storage reg
	]
	IF	TDLVRS, [
VDBCTL	=	0E0H		;VDB control port
VDBDAT	=	VDBCTL + 1	;VDB data port
VDBXCF	=	00000000B	;X cursor set flag
VDBYCF	=	11000000B	;Y cursor set flag
	]
;
; OUTCHR - Output the character to the Video Board.
;	   Enter: Char in C.
;	   Regs: DE and BC saved.
;
OUTCHA:	MOV	C,A
				;{STAOUT,WINOUT}
OUTCHR:	TST$	ATTRIB		;Is any attribute set?
	MOV	A,C		;Get char. in A
	JRZ	..2		;No, branch
	ORI	80H		;Yes display char in reverse video
..2:	OUT	VDBDAT		;Send char to video board
	LXI	H,PHYHOR	;HL-> physical cursor pos
	INR	M		;Piiceon cursor moves right
	RET
	.PAGE
	IF	WINDOO, [
;
; UPDONE - Show cursor position with reverse video character
;
				;{VLOOP}
UPDONE:	CALL	CHKCUR		;Update cursor pos
	MOVW	UPDCUR,PHYVER	;Save cursor pos
	IN	VDBDAT		;Get screen character
	STA	UPDSVC
	XRI	80H		;Switch top bit
	PUSHA
	CALL	PHYRST		;Move cursor back
	POPA
	OUT	VDBDAT		;Send char to video board
	RET
;
				;{VEDCMD}
UPDRTC: TSTW	UPDCUR		;Is old cursor on screen?
	RZ			;No, return
	CALL	PHYFRC		;Yes, position on screen
	LDA	UPDSVC		;A = character there
	OUT	VDBDAT		;Send char to video board
	CALL	PHYRST		;Move cursor back
				;{CLRWIN}
UPDRST:	MVIW$	UPDCUR,0000	;Clear position/flag
	RET
	]			;<IF WINDOO>
	.PAGE
;********************************************************
;*							*
;*	Screen Clear and Cursor Routines.		*
;*							*
;********************************************************
;
; CLRSCR - Clear entire screen.
;
				;{VEXIT,VFLDIR}
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
	LHLD	WINVER		;HL = desired write pos
..2:	PUSH	B		;Save count in B
	CALL	WINSET		;Position the cursor
	CALL	WIEOL2		;Erase the line
				;Note - first EOL may be partial line
	LHLD	WINVER		;HL = previous pos
	INR	L		;Move to next line
	LDA	HZSCBG		;Get leftmost column of virtual window
	MOV	H,A
	POP	B		;B = remaining count
	DJNZ	..2		;Loop
;
RESCUR:	LHLD	SAVWIN		;Restore to position saved in SAVCUR()
	JMP	WINSET
;
SAVCUR:	LHLD	WINVER
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
WIEOL1:	CALL	WIEOL2		;Erase the line
	JMPR	PHYRST		;Reset to desired cursor pos
;
;	Do an EOL by sending out spaces to end of line.
;
				;{WINEOS.WINEOL}
WIEOL2:	LDA	WWENCO		;Get last physical column for window
WIEOL3:	LXI	H,PHYHOR	;HL-> current physical column
	SUB	M		;Compute # pos. to clear
	RC			;DON'T if PHYHOR past end of window
	INR	A		;Adjust
	MOV	B,A		;Put count in B
	JMP	FILLIN		;Fill line with spaces
;
; PHYEOL - Clear to end of screen from cursor position.
;
;;PHYEOL:	LDA	PYLLEN		;Get display line length
;;	DCR	A		;A = last column counting from 0
;;	CALL	WIEOL3		;Merge above
;
PHYRST:	LHLD	PHYVER		;Restore physical cursor pos
	JMP	PICSET		;Force cursor positioning
;
	.PAGE
;
; WISFWD - Scroll window forward and clear bottom line
;
				;{WINOUT,WTSCRL}
WISFWD:
	IF	WINDOO, [
;	CALL	MAXWIN		;B = # windows on screen
				;'Z' if only one text window
;	JZ	CRTFSL		;Hardware scroll entire screen forward
	] [
	JMP	CRTFSL		;Scroll screen forward
	]
;
;**********Need to STACLR if status line on bottom of screen
;
	IF	WINDOO, [
..1:	MVI	A,1
..2:	LXI	H,WWNLIN
	CMP	M
	BGE	WISCLR		;Branch if all lines moved up
	MOV	D,A		;D = window line # to write to
	INR	A		;Next line
	MOV	E,A		;E = window line # to read from
	CALL	PICMOV		;Move the window line up
	JMPR	..2		;Loop
;
WISCLR:	MOV	L,A
	CALL	WINST0		;Address bottom window line
	JMP	WIEOL1		;Erase it
;
; WISLBK - Scroll window backward and clear top line
;
WISLBK:	LDA	WWNLIN
..1:	CPI	2
	BLT	WISCLR		;Branch if all lines moved down
	MOV	D,A		;D = window line # to write to
	DCR	A		;Previous line
	MOV	E,A		;E = window line # to read from
	CALL	PICMOV		;Move the window line down
	JMPR	..1		;Loop
;
;	Copy window line on screen from line # (E) to line # (D)
;
PICMOV:	PUSHA
	PUSH	D
	MOV	L,E
	CALL	WINST0		;Address begin of line
	CALL	READLN		;Read the line
	POP	D
	MOV	L,D
	CALL	WINST0
	CALL	WRITLN
	POPA
	RET
;
READLN:	CALL	PICSUP
	INIR			;Read the screen in buffer
	RET
;
WRITLN:	CALL	PICSUP
	OUTIR
	RET
;
PICSUP:	LXI	H,PICLIN	;HL-> line buffer
	LDA	WWLLEN		;Get window line length
	MOV	B,A		;Put in B
	MVI	C,VDBDAT
	RET
	]			;<IF WINDOO>
	.PAGE
	IF	PICVRS, [
;
;	CLRLIN - Clear the invisible line before screen "ON" screen.
;
CLRLIN:	LHLD	PYLINE		;L = # of screen lines
	INR	L		;Set for invisible line
	MVI	H,00		;Column 00 of invisible line
	CALL	PICSET		;Position the cursor off screen
	MVI	B,2048-1920	;Get count of invisible chars
;
; FILLIN - Send spaces to screen.  Use WWBKAT attribute.
;
FILLIN:	TST$	WWBKAT		;Is background attribute set?
	MVI	A,SPACE		;Assume No, fill with space char
	JRZ	..1		;No, branch
	MVI	A,SPACE+80H	;Yes, use reverse video space
..1:	OUT	VDBDAT		;Send to board
	DJNZ	..1		;Clear all invisible chars
	RET
;
; CRTFSL - Scroll the PIICEON board forward.
;
CRTFSL:	PUSH	D		;Save DE
	CALL	CLRLIN		;Clear the invisible line first
	CALL	GETBGN		;HL = screen begin address
	LXI	B,80		;Value to scroll forward
	DAD	B		;Compute new begin address
	LXI	D,2048		;Max. value before wrap
	CALL	CMHLDE		;Does new value need wrap?
	JRC	..1		;No, set new screen begin
	LXI	B,-2048		;Yes, get wrap value
	DAD	B		;Wrap the begin address
..1:	POP	D		;Restore DE
	JMPR	SETSCL		;Set new screen begin
;
; CRTBSL - Scroll the PIICEON board backward.
;
	IFNOT	WINDOO, [
WISLBK:
	]
CRTBSL:	CALL	CLRLIN		;Clear the invisible line first
	CALL	GETBGN		;HL = screen begin address
	LXI	B,-80		;Value to scroll backwards
	DAD	B		;Compute new begin address
	JRC	SETSCL		;Branch if no underflow
	LXI	B,2048		;Value for wrap around
	DAD	B		;Wrap the begin address
;
; SETSCL - Set the screen begin and save value in PIICEON storage registers.
;
SETSCL:CALL	STWTBR		;Will need new status line
	MVI	A,0B4H		;Get bits to select CRTC
	OUT	VDBCTL		;Select the CRTC
	MVI	A,12		;Start address high reg
	OUT	ADDRG
	MVI	C,INTRG		;Port for CRTC internal register
	OUTP	H
	INR	A		;Start addr. low register
	OUT	ADDRG
	OUTP	L
	MVI	A,0B5H		;Get bits to select Storage Register
	OUT	VDBCTL		;Select the Storage Reg
	MOV	A,L		;Get low order address
	OUT	TMPLRG		;Save in register
	MOV	A,H		;Get high order address
	OUT	TMPHRG		;Save in register
	JMP	PHYRST		;Reset cursor pos
;
GETBGN:	IN	TMPLRG		;Get low order screen begin
	MOV	L,A
	IN	TMPHRG		;Get high order screen begin
	MOV	H,A
	RET
	]
;
; PHYSET - Address the physical screen cursor.
;	   Enter with L = vertical row #.  Top row = 1.
;	   H = horizontal column #.  Left column = 0.
;
				;{WINSET,WTSCRL,ADDSTA}
	IF	TDLVRS, [
				;Cursor position for TDL
PHYSET:	ERROR			;No longer correct
	SHLD	PHYVER		;Save as simulated cursor pos
	MOV	A,L		;Get row #
	DCR	L		;Adjust so that top left is 0,0
	MVI	A,VDBXCF	;Get set X cursor mask
	ORA	H		;Add in the column
	OUT	VDBCTL		;Set the X position
	MVI	A,VDBYCF	;Get set y cursor mask
	ORA	L		;Add in line number
	OUT	VDBCTL		;Set the Y position
	RET
	]
;
	IF	PICVRS, [
				;Cursor position for PIICEON
PHYST0:	MVI	H,00		;Set to column 00
PHYSET:	PUSH	D		;- 8086
	LDED	PHYVER		;Get current position
	CALL	CMHLDE		;Already at desired position?
	POP	D		;- 8086
	RZ			;Yes, return now
;
PHYFRC:	SHLD	PHYVER		;Save physical cursor pos
				;{CLRLIN - don't change PHYVER,PHYRST - force}
PICSET:	PUSH	D		;Save DE
	XCHG			;Save position in DE
	CALL	PICPOS		;HL= cursor position
	IN	VDBCTL+2	;Read the low order scroll value
	MOV	E,A		;Place in E
	IN	VDBCTL+3	;Read the high order scroll value
	MOV	D,A		;DE = PIICEON scroll value
	DAD	D		;Add to cursor pos
	LXI	D,2048		;Get max pos
	CALL	CMHLDE		;Does cursor pos. need wrap?
	JRC	..1		;No, branch
	LXI	D,-2048		;Yes, get wrap value
	DAD	D		;Wrap the cursor pos
..1:	MOV	A,L		;Get low order byte
	ANI	7FH		;Strip off top bit
	OUT	VDBCTL		;Send to control port
	MOV	A,L		;Get whole byte again
	RLC			;Bit 7 into carry
	MOV	A,H		;Get high order byte
	RAL			;Rotate with carry
	ORI	0C0H		;Bits to load high order cursor
	OUT	VDBCTL		;Send to control port
	POP	D		;Restore DE
	RET
;
PICPOS:	MOV	A,E		;Get cursor line #
	LXI	H,LINETB	;HL-> cursor position table
	CALL	TBLADD		;Get address from LINETB[A-1]
	MOV	A,D		;Get cursor column #
	JMP	ADDAHL		;Compute PIICEON cursor pos
;
; LINETB - Gives PIICEON cursor position for begin of each line.
;
LINETB:	DW	00,80,160,240,320,400,480,560,640,720
	DW	800,880,960,1040,1120,1200,1280,1360,1440
	DW	1520,1600,1680,1760,1840,1920
	]
;;
;; KDISPY - Return A = 1 for non-IBM MM display.
;;
				;;{WDGET}
KDISPY:	MVI	A,1
	RET
;
HCRSOF:	RET			;;Fewer modules need reassembly
