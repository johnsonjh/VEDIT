	.PAGE
	.TITLE	'VEDIT-M3'
;************************************************
;*						*
;*	Memory Mapped Dependent Routines	*
;*						*
;************************************************
;
; Copyright (C) 1986 by CompuView Products, Inc.
;			1955 Pauline Blvd.
;			Ann Arbor, MI 48103
;
;	Written by: Theodore J. Green
;
;	Last Change: Ted - June 01, 1986 - Windows
;		     Tom - Dec. 02, 1986 - KDISPY()
;		     Ted - Mar. 16, 1987 - TRS-80-II bank select
;

	DSEG	$
BANKFL:	DB	0		;;Screen "bank" selected flag
SCRPNT:	DSW	1		;Screen address for next char
HOLD:	DS	1		;Character "under" cursor
	CSEG	$

;
; OUTCHR - Output character to screen.  C = char
;
OUTCHA:	MOV	C,A		;Save char in C
				;(STAOUT,WINOUT)
OUTCHR:	TST$	BANKFL		;Is screen selected in?
	JRNZ	..1		;Yes, branch
	CALL	SCRIN		;No, bank select the screen in
;
..1:	TST$	ATTRIB		;Is any attribute set?
	MOV	A,C		;Get char. in A
	JRZ	..2		;No, branch
	ORI	80H		;Yes display char in reverse video
..2:	LHLD	SCRPNT		;HL-> where on screen to write
	MOV	M,A		;Put character on screen
	INX$	H		;Bump screen pointer
	SHLD	SCRPNT		;Save screen pointer
	INCM	PHYHOR		;Physical write pos. moves right
	RET
;
;	Note: If your screen memory overlays the edit buffer, be sure
;		to CALL SCROUT at the end of the OUTCHR() routine.
;
;	POLT VTI Patch - Change LHLD SCRPNT above to CALL POLY
;
POLY:	MOV	A,C
	ORI	080H
	MOV	C,A
	LHLD	SCRPNT
	RET
;
;
; CURSON - Place the cursor on the screen, saving the
;	   Character there.
;
				;{GETKEY}
CURSON:	LDA	CURTYP		;Get the cursor flag. (Bit 7 if cursor On)
	ORA	A		;Is the cursor already ON?
	RM			;Yes, return
	PUSH	PSW		;No, save the cursor type
	CALL	SCRIN		;Bank select the screen in
	LHLD	SCRPNT		;Get current cursor pos
	MOV	A,M		;Get character at cursor pos
	STA	HOLD		;Save it
	POP	PSW		;Get the cursor type
	JNZ	..1		;Branch if not underline
	LDA	CURSCH		;Get the cursor char
	JMP	CURSO5		;Put cursor on screen
;
..1:	ORI	80H		;Set cursor ON bit
	STA	CURTYP		;Save cursor flag
	CPI	83H		;Is cursor hardware attribute type?
	JZ	CURSO3		;Yes, branch
	MOV	A,M		;No, get the screen char. again
	ORI	80H		;Make into reverse video
	JMP	CURSO5		;Put cursor on screen
;
;	This is initially set up for the type of hardware attribute
;	as found on the SSM VB-3 board.
;
CURSO3:	MVI	A,07H		;Get the attribute byte.  Note:  Change
				;the "07H" for different cursor types
CURSO4:	LXI	B,1000H		;Offset to the hardware attributes
;	DAD	B		;HL-> attribute byte for char on screen
	NOP			;Space for above DAD B
				;{CURSON,CUROFF}
CURSO5:	MOV	M,A		;Put on screen, (or change attribute)
	JMP	SCROUT		;Bank select the screen out
;
; CUROFF - Restore character under the cursor or turn cursor off.
;
				;{GETCHR}
CUROFF:	LDA	CURTYP		;Get the cursor type
	ANI	7FH		;Ignore On/Off bit
	CPI	2		;Is it blinking?
	RNC			;No, leave it alone
				;{GETCHR,VSHELP}
CUROF1:	LXI	H,CURTYP	;HL-> cursor flag
	MOV	A,M		;Get the flag
	ANI	7FH		;Set flag for cursor OFF
	MOV	M,A		;Save flag
	LHLD	SCRPNT		;HL -> cursor pos
	CALL	SCRIN		;Bank select the screen in
	CPI	3		;Is cursor hardware attribute type?
	JZ	CUROF2		;Yes, branch
	LDA	HOLD		;No, get char. under cursor
	JMP	CURSO5		;Branch to put on screen
;
CUROF2:	MVI	A,03H		;Get the attribute byte
	JMP	CURSO4		;Change attribute byte, de-select screen
;
	DS	16		;User patch space
	.PAGE
;
;	Routines for bank select and hardware cursor control.
;	All registers must be saved, including PSW!
;
	.IFN	MEMVER -2, [
;
;	The Bank Select and Hardware Cursor Select routines can
;	generally be implemented by changing the "POP PSW" and "RET"
;	to the code needed to send the proper bytes to output ports
;	in the patch space provided and then doing a "POP PSW" and
;	a "RET".
;
SCRIN:	PUSH	PSW		;Save AF
	MVI	A,1		;Get non zero
	STA	BANKFL		;Set screen bank select flag
	POP	PSW		;Restore AF
	RET			;Just return if no bank select
	DS	14		;Patch space for bank select screen in
;
SCROUT:	PUSH	PSW		;Save AF
	XRA	A		;Get a zero
	STA	BANKFL		;Clear screen bank select flag
	POP	PSW		;Restore AF
	RET			;Default to no bank select
	DS	14		;Patch space for bank select screen out
;
;	The hardware cursor select OFF and ON are only called at entry
;	to and exit from visual mode, respectively.
;
HCRSOF:	RET			;Default to no hardware cursor off
	DS	14		;Patch space to turn hardware cursor off
;
HCRSON:	RET			;Default to no hardware cursor on
	DS	14		;Patch space to turn hardware cursor on
	]
;
	.IFE	MEMVER -2, [
;
	.IFE	TRSVER -1 ,[
;
SCRIN:	DI			;Prevent interrupts during screen select
	PUSH	PSW		;Save registers
	MVI	A,1		;Get non zero
	STA	BANKFL		;Set screen bank select flag
	MVI	A,081H		;Get screen select code
	OUT	0FFH		;Send to control port
	POP	PSW		;Restore PSW
	RET
	DS	7		;Keep alignment
;
SCROUT:	PUSH	PSW		;Save registers
	XRA	A		;Get a zero
	STA	BANKFL		;Clear screen bank select flag
	MVI	A,01H		;Get screen de-select code
	OUT	0FFH		;Send to control port
	POP	PSW		;Restore PSW
	EI			;Enable interrupts again
	RET
	DS	7		;Keep alignment
;
HCRSOF:	PUSH	PSW		;Save registers
	MVI	A,14		;Get set cursor position
	OUT	0FCH		;Select function
	MVI	A,3FH		;Value for cursor off screen
	OUT	0FDH		;Write new cursor pos
	POP	PSW
	RET
	DS	4		;Keep alignment
;
HCRSON:	RET			;Not used for Model II
	DS	14		;Keep alignment
	]
;
;
;
	.IFE	TRSVER -2, [
SCRIN:	PUSH	PSW		;Save registers
	PUSH	H
	LXI	H,BANKFL	;HL-> Bank select on/off flag
	MOV	A,M		;Get current value
	MVI	M,1		;Make sure flag is set
	ORA	A		;Is screen already selected?
	JRNZ	SCRIN2		;Yes, don't select again
;
	PUSH	D
	PUSH	B
	MVI	B,15H		;Open the screen for access
SCRIN1:	CALL	40H
	POP	B		;Restore registers
	POP	D
;
SCRIN2:	POP	H
	POP	PSW		;Restore registers
	RET
;
SCROUT:	PUSH	PSW		;Save flags
	PUSH	H		;Save registers
	LXI	H,BANKFL	;HL-> Bank select on/off flag
	MOV	A,M		;Get current value
	MVI	M,0		;Make sure flag is OFF
	ORA	A		;Is screen selected?
	JRZ	SCRIN2		;No, don't need to de-select it
;
	PUSH	D
	PUSH	B
	MVI	B,16H		;Close access to screen
	JMP	SCRIN1		;Merge in above
	DS	6		;Keep alignment
;
HCRSOF:	PUSH	PSW		;Save registers
	MVI	A,14		;Get set cursor position
	OUT	0FCH		;Select function
	MVI	A,3FH		;Value for cursor off screen
	OUT	0FDH		;Write new cursor pos
	POP	PSW
	RET
	DS	4		;Keep alignment
;
HCRSON:	RET			;Not used for Model II
	DS	14		;Keep alignment
	]
	]
;
; VERSCR - Return in HL screen address for begin of screen line.
;
VERSC1:	PUSH	D
	MOV	E,A
	DCR	E		;Align for zero offset
	MVI	D,0		;Put multiplier in DE
	LHLD	MMLLEN		;HL = length of screen line
	CALL	MULTIP		;HL = offset
	LDED	SCRBAS		;+DE = base address of screen
	DAD	D		;HL = address for desired line
	POP	D
	RET
;;
;; KDISPY - Return A = 1 for MM (non IBM) display device.
;;
				;;{WDGET}
KDISPY:	MVI	A,1
	RET

