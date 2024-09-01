	.PAGE
	.TITLE	'VEDIT-V4'
;************************************************
;*						*
;*	Character Screen Output			*
;*						*
;************************************************
;
; Copyright (C) 1987 by CompuView Products, Inc.
;			1955 Pauline Blvd.
;			Ann Arbor, MI 48103
;
;	Written by: Theodore J. Green
;
;	Last Change: Ted - Oct. 13, 1986 - Windows
;		     Tom - Oct. 15, UPEEB() restore RMINUS
;			   Oct. 17, UPDVIS() restore REGNUM
;		     Ted - Oct. 23, WINOUT(), SWWIND(), misc
;			 - Oct. 24, WRTCON(), YWCREA(), YWDELE(), misc
;			 - Oct. 25, Change structure for YWDELE()
;			 - Oct. 26, Allow YWDr, misc.
;			 - Oct. 27, DEZOOM()+, WINOUT(), UPDVIS(), UPDOTH()
;			 - Nov. 05, WINSET - Fix out-of-range bug; Misc
;			 - Nov. 25, WIZOOM - user/auto modes, SWWIND(), WIBORD()
;			 - Dec. 02, UPDVIS for CRT version
;			 - Mar. 31, 1987 - Allow YWS# - use numeric register
;			 - Apr. 03 - WININI() clears WWMODE and WWSPEC (Bug fix)
;			 - Apr. 05 - New routine GETCWN() "Get command window name"
;				     Make status line window "+"
;			 - Apr. 11 - PRTCHA handles Bit 8 based on BIT7AL
;			 - Apr. 18 - WWMODE initialized to -1 for new window
;			 - Apr. 27 - STUPFL() sets WWNDUP to -1 for V0 test
;
;
;	LSTFLG output control values
;
;	Mask 80 - Allow chars to exceed window margin
;	Mask 40 - Cause ^L to clear window
;	Mask 20 - Expand CR and LF to <CR> and <LF>
;	Mask 10 - Expand BS to ^H		Depends upon Mask 01
;	Mask 08 - Change ESC to $
;	Mask 04 - Stop for CTRL-S in file
;	Mask 02 - Expand tabs
;	Mask 01 - Expand CTRL chars
;
; PCHAR  - Display character in %C on the console.
; PCHARA - Display character in %A.
;	   The output routine to use is in ROUTAD.
;	   Return: ALL registers are preserved.
;
PCHAR:	MOV	A,C		;Entry for char. in C
PCHARA:	PUSH	H		;Save all registers
	PUSH	D
	PUSH	B
	PUSHA
	CALL	PRTCHA		;DE is saved
	POPA
	POP	B
	POP	D
	POP	H
	RET
;
PSPACE:	MVI	A,SPACE
	JMPR	PCHARA
;
; EXPTAB - Expand tab with %A "Tab-fill" spaces (HL clobbered).
;
				;{WRTLN3}
EXPTAB:	LXI	H,TABFIL	;HL-> special char
	MOV	C,M		;Get char for tab filling
	JMPR	OUTNCH		;Merge below
;
; OUTNSP - Output %A spaces.
				;{VEDIT-CP}
OUTNSP:	MVI	C,SPACE		;Get a space
;	JMPR	OUTNCH
;
; OUTNCH - Output %C %A times.
;
	BPROC	OUTNCH		;{OUTNSP^,EXPTAB}
OUTNCH:	ORA	A		;Check count
	RZ			;Do nothing if count zero
	MOV	B,A		;Put count into B
..2:	PUSH	B
	CALL	WRTCON		;Send to screen, update WINHOR, LOGPOS
	POP	B
	DJNZ	..2		;Loop
	RET
	EPROC	OUTNCH
;
; PRTCHA - Put character on screen and expand tabs and control chars.
;	   Enter: A = character.
;	   Exit:  Update WINHOR, LOGPOS.  DE must be saved for WRTLIN()
;
				;{PCHARA,WRTLIN}
	BPROC	PRTCHA
PRTCHA:	MOV	C,A		;Save char in C
	ORA	A		;Is bit 8 set?
	JRP	..1		;No, process normally
;
;	Handle 8 Bit characters (always sent directly to WRTCON)
;
	LDA	BIT7AL		;;Get EP parameter
	ANI	2		;;Is Bit 8 allowed on output?
	JNZ	WRTCON		;Yes, send unmodified character
;
;	Convert High Bit character to reverse video
;
	MOV	A,C		;;Get char again
	ANI	7FH		;;Strip to 7 bits
	MOV	C,A
	LDA	ATTRIB		;;Get attribute
	PUSHA			;;Save it

	IF	P8086, [
	RVBCUR			;;Flip normal/reverse
	] [
	XRI	1		;;Flip reverse video bit
	]

	STA	ATTRIB		;;Save new value
	CALL	WRTCON		;;Output the character
	POPA			;;AL = original ATTRIB
	STA	ATTRIB		;;Restore real ATTRIB
	RET
;
..1:	CPI	SPACE		;Is char a control char?
	JNC	WRTCON		;No, branch
	CPI	TAB		;Is it a Tab?
	BEQ	..TAB		;Yes, branch
	CPI	CR
	BEQ	..CR
	CPI	LF
	BEQ	..LF
	CPI	ESC
	BEQ	..ESC
	CPI	BS
	BEQ	..BS
	CPI	CTRLS		;CTRL-S?
	BEQ8	..STOP		;Yes, branch
	CPI	CTRLL		;Form feed?
	BEQ8	..FF
;
;	Deal with control characters
;
..2:	ANIB$	LSTFLG,1	;Expand control chars?
	JRZ8	WRTCON		;No, pass control char
..3:	PUSH	B		;Save original char
	MVI	C,'^'		;No, prefix other control chars with '^'
	CALL	WRTCON		;Send char to console
	POP	B		;Get original char. back
	MVI	A,'@'		;Get offset for control chars
	ADD	C		;Create letter for control char
	MOV	C,A		;Put char. in C
	JMPR	WRTCON
;
..TAB:	ANIB$	LSTFLG,2	;Expand Tab chars?
	JRZ	..2		;No, branch
;
	LHLD	LOGPOS		;Get logical position
	CALL	FNDTAB		;A = next tab pos
	JRZ	..2		;If invalid - display tab char
	SUB	L		;Compute positions moved
	JMPR8	EXPTAB		;Expand tab with tab-fill spaces
;
..CR:	ANIB$	LSTFLG,20H	;Expand CR to <CR>?
	JRZ	..CMCR		;No, must be command mode CR
	LXI	H,CRMSG		;HL-> "<CR>"
	JMP	PRTSTR		;Display it
;
;	Deal with Command Mode CR
;
..CMCR:	CALL	BRKCHK		;A CR, so check for user abort
	LDA	CRDELY		;A = delay for command mode CR
	CALL	DELAY		;Perform delay
	CALL	RSTPOS		;Reset LOGPOS = 1
				;WINHOR reset in window handler
	JMPR	WRTCTL		;Send to window handler
;
..LF:	ANIB$	LSTFLG,20H	;Expand LF to <LF>?
	JRZ	WRTCTL		;No, send to output routine
	LXI	H,LFMSG		;HL-> "<LF>"
	JMP	PRTSTR		;Display it
;
..ESC:	ANIB$	LSTFLG,8H	;Change ESC to $?
	JRZ	..2		;No, treat ESC as normal CTRL char
	MVI	C,'$'		;Yes, convert to "$"
	JMPR	WRTCON		;Display "$"
;
..BS:	ANIB$	LSTFLG,10H	;Expand BS to ^H?
	JRNZ	..2		;Yes, treat BS as normal CTRL char
	LHLD	LOGPOS		;HL = current pos
	DCX$	H		;Decrement it
	MOV	A,H
	ORA	L		;Trying to decrement to 0000?
	JRZ	..BS2		;Yes, leave it alone (can this even happen?)
	SHLD	LOGPOS		;Save new pos
..BS2:	JMPR	WRTCTL		;Send BS to console driver
;
;
; CTRL-S is typically used as a STOP code, unless suppressed
;
..STOP:	ANIB$	LSTFLG,4	;Stop on CTRL-S?
	JRZ8	..2		;No, treat as normal CTRL char
	JMP	CTRLSS		;Yes, handle CTRL-S (see VEDIT-C5)
;
;	Check if Form Feed char will clear window
;
..FF:	ANIB$	LSTFLG,40H	;Cause ^L to clear window?
	JZ	..2		;No, treat ^L as normal CTRL char
	JMP	CLRWIN		;Yes, clear window, home cursor
	EPROC	PRTCHA
	.PAGE
;
; WRTCON - Write char. in 'C' to output routine in (ROUTAD).
;	   Enter: Char. in C.
;	   Exit:  Update WINHOR, LOGPOS
;		  BC and HL clobbered, DE is saved.
;
				;{OUTSPC,PRTCHA}
WRTCON:
	IFNOT	P8086, [
	LXI	H,LOGPOS	;Increment LOGPOS
	INR	M
	JNZ	..1		;Jump if position < 255
	INX$	H		;Point to high order LOGPOS
	INR	M		;Increment high order byte
	DCX$	H		;HL-> LOGPOS
..1:	TST$	FAKFLG		;Is this a fake write?
	JZ	WRTCTL		;No, send character (WINHOR updated in WINOUT)
	DCX$	H		;Yes, HL-> WINHOR
	INR	M		;Move window position right
	RET
	] [
;6	INC	LOGPOS		;Increment LOGPOS
;6	TEST	FAKFLG,0FFH	;Is this a fake write?
;6	JZ	WRTCTL		;No, send character (WINHOR updated in WINOUT)
;6	INC	WINHOR		;Yes, increment WINHOR here
;6	RET
	]
;
; WRTCTL - Do not update WINHOR & LOGPOS when outputing CR, LF or BS
;
				;{PRTCR,PRTLF,PRTBS,WRTCON}
WRTCTL:	LHLD	ROUTAD		;HL-> output routine address
	PCHL			;Jump to it; Char is in C
				;LSTOUT - Send char to printer
				;INSCHR - Insert char into edit buffer
				;WINOUT - Send char to current window
				;STAOUT - Send char to status line
	.PAGE
;
; WINOUT - Output character in reg. C to screen window.
;	   Switches to command window when first command char received.
;	   Retrn: BC and HL clobbered, DE is saved.
;
; STAOUT - Entry point when writing the status line
;
	BPROC	WINOUT
WINOUT:	TST$	VISFLG		;Is this visual mode?
	JRNZ	..3		;Yes, branch
	LDA	WWMODE		;;Get window mode
	DCR	A		;;Visual mode?
	JRM	..2		;No, branch
;
;	Switch window to command mode operation
;
	IF	WINDOO, [
	PUSH	B		;Save C
	PUSH	D		;Save DE
	CALL	UPDRTC		;Remove update cursor
	LDA	COMMWI		;Get command mode window #
	CALL	SWWIND		;Switch to command mode window
	LXI	H,WWMODE	;HL-> buffer # or 00 if command mode
	MOV	A,M		;Get buffer #
	MVI	M,00		;Window now used for command mode
	DCR	A		;;Was this window last used for command mode?
	JRM	..1		;;Yes, branch
;
	LDA	WWNAME		;Get command mode window #
	CALL	WSTCAL		;ES:HL-> window structure
	INX$	H		;HL-> WWMODE
	MVI%ES	M,00		;Make sure structure value cleared too
				;Prevents command window update in UPDVIS()
	]
;
;	Scroll visual mode stuff up to make room for command prompt
;
				;(No longer need to STNSNL)
	CLR$	HZSCBG		;Command mode window is not horz. scrolled
	CALL	HZENST
	LD16	H,WWNLIN	;L = Line # for last line in window
	CALL	WINST0		;Move write pos to last line
	CALL	CRLF		;Send CRLF  (*** Calls WINCON Recursively ***)
				;Resetting WWMODE above prevents problems
..1:	POP	D		;Restore DE
	POP	B		;Restore C = original char causing this trouble
;
..2:	CLR$	WWMODE		;Ensure window set for command mode
..3:	MOV	A,C		;Get char
	CPI	SPACE		;Displayable char?
	BGE	WINOU2		;Yes, branch
	CPI	CR
	JRZ	WINCR
	CPI	LF
	JRZ	WINLF
;
				;{WRTCTL}
STAOUT:	MOV	A,C		;Get char
	CPI	BS
	JRZ	WINBS
;
;	Only display char. if WINHOR is within scrolling window
;
WINOU2:	LXI	H,WINHOR	;HL-> window position
	MOV	A,M		;A = WINHOR
	INR	M		;Increment WINHOR (even if no char sent)
	LXI	H,HZSCBG	;HL-> value of first virtual column
	CMP	M		;Within screen window?
	BLT	..4		;No, branch
	INX$	H		;HL-> HZSCEN = past last virtual column
	CMP	M		;Within screen window?
	JC	OUTCHR		;Yes, send to screen
;
..4:	TST$	VISFLG		;Is this visual mode?
	RNZ			;Yes, return, throw character away
	ANIB$	LSTFLG,80H	;Ok to wrap screen?
	JRNZ	..JOUT		;No, send char anyway (user told me to)
;
;	For command mode first wrap to next window line
;
	CPIB$	WWNAME,'?'	;Is this the status line?
	RZ			;Yes, throw character away
	PUSH	B		;Save original char
	CALL	WINCR		;Perform CR on window
	CALL	WINLF		;Perform LF on window
	LXI	H,WINHOR	;HL-> window position
	INR	M		;Account for char about to be sent
	POP	B		;C = output char
..JOUT:	JMP	OUTCHR		;Display the char
	EPROC	WINOUT
	.PAGE
;
;
; WINCR -  Position to first column of line in window
;
WINCR:	LD16	H,WINVER	;L = line # in window
	JMPR	WINST0		;Move to column 00 of current line
;
; WINLF - Position to next line of window, scrolling if necessary
;
WINLF:	LD16	H,WINVER	;HL = old window position
	INR	L		;HL-> next window line
	LDA	WWNLIN		;Get max  lines in window
	CMP	L		;Desired line past end of window?
	JRNC	WINSET		;No, move to next window line
	JMP	WISFWD		;Scroll window forward
;
; WINBS - Move cursor back one position
;
	BPROC	WINBS
WINBS:	LD16	H,WINVER	;L = WINVER, H = WINHOR
	LDA	HZSCBG		;Get first virtual column
	CMP	H		;Is position OK?
	JRNC	..1		;No, move to previous line
	DCR	H		;Backup one position
	JMPR	WINSET		;Move cursor back
;
;	Move cursor to previous window line
;
..1:	DCR	L		;Backup to previous line
	RZ			;Do NOTHING if at begin of window
				;This catches status line too
	LDA	WWLLEN		;A = max column #
	DCR	A		;Columns counted from 0
	MOV	B,A		;Save in B
	LDA	HZSCBG		;Get virtual begin column
	ADD	B		;Compute end of virtual line
	MOV	H,A
	JMPR	WINSET		;Move cursor to end of previous line
	EPROC	WINBS
	.PAGE
;
; ATTBCK / ATTFOR - Swith to background / foreground window attribute
;
				;{WINEOS,WINEOL,WININI,WIBTOP}
ATTOFF:
ATTBCK:	LDA	WWBKAT		;Get window background attribute
	JMPR	ATTSET
;
ATTFOR:	LDA	WWFRAT		;Get window foreground attribute
	JMPR	ATTSET
;
ATTBRD:	LDA	SBRDAT		;Get window border attribute
	JMPR	ATTSET		;Used for window border

ATTBMS:	LDA	SBMSAT		;Get border "WINDOW" message attribute
	JMPR	ATTSET		;Used for window border

ATTSTM:	LDA	SSMSAT		;Get status line message attribute
ATTSET:	STA	ATTRIB		;Save as desired attribute

	IF	CRTVRS, [
	CALL	SV%BDH
	CALL	OUTATT		;Switch to new attribute NOW
	]

	RET
	.PAGE
;
; WINSET - Address the cursor within window.
;	   If outside window, position to leftmost column.
;	   Update WWUSLN to bottom most line containing text
;	   Enter: L = vertical row #.  Top row = 1.
;		  H = horizontal column #.  Left column = 0.
;	   Retrn: Update WINVER,WINHOR,WWUSLN.
;		  BC and HL clobbered, DE saved.
;
;	   Note: Don't set WINVER directly, or WWUSLN may not update.
;
				;{CLRWIN}
WINHOM:	MVI	L,01		;Set to home position
				;{CLRWIN,WISFWD,WINOUT,WINCR}
WINST0:	LDA	HZSCBG		;Get virtual begin column
	MOV	H,A		;Set to leftmost column
;
;	Before addressing the new position, WWUSLN is set to the current
;	line if the current WINHOR != 00.
;
				;{WINEOS,RESCUR,GETCHR,WINLF,WINBS}
				;{YEHCMD,YEVCMD,UPDVIS,WRTLIN}
	BPROC	WINSET
WINSET:	PUSH	H		;Save new position
	LD16	H,WINVER	;L = current line #
	LDA	WWUSLN		;A = bottom used line in window
	CMP	L		;WWUSLN already set to/past current line?
	BGE	..1		;Yes, branch
;
	CALL	WNNHOR		;'Z' if physical cursor in leftmost column?
	JRZ	..1		;Branch if line does not contain text
	MOV	A,L		;Yes, get current line #
	STA	WWUSLN		;Save new WWUSLN
;
..1:	POP	H		;Restore HL
	ST16	WINVER,H	;Save value
;
WINRST:	LDA	WINHOR		;AL = desired column
	CALL	RNGCHK		;Check range; 'C' if out of range
				;HL-> HZSCBG if in range
	JRC	..OUTR		;Branch if out of range
	SUB	M		;Subtract window offset
..2:	LD16	H,WINVER	;L = desired line #
	MOV	H,A		;H = adjusted column #
	LDA	WWBGLN		;Get beginning line # of window
	DCR	A		;Adjust
	ADD	L
	MOV	L,A
	LDA	WWBGCO		;Get beginning column # of window
	ADD	H
	MOV	H,A
	JMP	PHYSET
;
..OUTR:	XRA	A		;Out of range - set to leftmost column
	JMPR	..2		;Merge above
	EPROC	WINSET
;
; WNNHOR - Return: 'Z' if physical cursor in leftmost column.
;		   Save HL.  Clobbers A, B
;
	BPROC	WNNHOR
WNNHOR:	LDA	WWBGCO
	MOV	B,A
	LD8	PHYHOR
	SUB	B		;'Z' if PHYHOR in leftmost window column
	RET
	EPROC	WNNHOR
	.PAGE
;
	DSEG	$
;
;	Initial values for Status line window
;
STATWW:	DB	'?'		;.WWNAME
	DB	00		;.WWMODE+1
	DB	00		;.WWSPEC+2
	DB	00
	DB	00
	DB	1		;.WWBGLN+5
	DB	1		;.WWENLN+6
	DB	00		;.WWBGCO+7
	DS	1		;.WWENCO+8
	DS	1		;.ATTRIB+9
	DS	1		;.WWFRAT+10
	DS	1		;.WWBKAT+11
	DW	0000		;.UPDCUR+12
	DW	0000		;.UPDSVC+14
WUPOFS	=	. - STATWW
	DB	00		;.WWNDUP
	DB	01		;.WWUSLN
	DB	01		;.WINVER
	DB	00		;.WINHOR
	DW	0001		;.LOGPOS
	DB	00		;.HZSCBG
;
WSTSIZ	=	. - STATWW
;
;	Initial values for new windows
;
	IF	WINDOO, [
WSTINI:	DW	0000		;UPDCUR
	DW	0000		;UPDSVC
	DB	01		;WWNDUP (ensure update on initial switch to $)
	DB	0		;WWUSLN
	DB	1		;WINVER
	DB	0		;WINHOR - Home
	DW	1		;LOGPOS
	DB	0		;HZSCBG
WSTIEN:
WSTINM	=	WSTIEN - WSTINI
	]
;
SAVEWW:	DS	WSTSIZ		;Save area when status line used
;
	CSEG	$
	.PAGE
;
; WININI - Initialize parameters for status-line & first windows.
;	   The window structure is saved as a text register.
;
				;{BEGIN,YWICMD}
WININI:	LD16	H,SFORAT	;Load INSTALL attributes
	ST16	WWFRAT,H	;Save into window values
;
;	Setup initial status line window values
;
	LD8	PYLLEN		;Displayed line length
	DCR	A		;Adjust for counting from 0
	STA	STATWW+8	;Store
	LD16	H,SSTAAT	;L = status line attribute
	LDA	SBCKAT		;Get clear screen attibute
	MOV	H,A		;HL = status window attributes
	ST16	STATWW+9,H	;;Set default ATTRIB
	ST16	STATWW+10,H	;;Save WWFRAT and WWBKAT for status line
;
	CALL	WININ0		;Init rest of window variables
	CALL	ATTFOR		;Restore to foreground attribute
	JMP	STACLR		;Clear the status line
;
				;{YWDELE}
WININ0:	CALL	WIZOOM		;Set current window to full screen and clear
;
	IF	WINDOO, [
	MVIB$	WWNAME,'@'	;Reset to original window
	MVIW$	WWMODE,00FFH	;;Reset WWMODE and WWSPEC (bug fix)
	CLR	WWZMFL		;Window isn't really zoomed
				;A == 0 causes new window register
	JMPR	WINRG0		;Create first window structure
	]
;
; WINREG - Create new window structure by appending to window "register".
;	   Enter: None
;	   Retrn: ES:HL-> new structure in expanded window "register".
;		  REGNUM is changed.
;
	IF	WINDOO, [
				;{YWCMD, WININI above}
WINREG:	MVI	A,1		;Get non-zero
				;{WININI}
WINRG0:	STA	REGAPP		;Set to append to existing reg.
	MVIB$	REGNUM,REGWIN	;Window structure treated as text reg.
	LXI	H,WWNAME	;HL-> existing values
	XCHG			;DE-> window values
	LXI	H,WSTSIZ	;Size of window structure
	DAD	D		;HL-> past end of window values
	CALL	BRKCPY		;This is too easy
	CALL	PNTREG		;ES:HL-> past end of new structure
	IFNOT	P8086, [
	LXI	B,-WSTSIZ	;BC = - structure size
	]
;6	MOV	CX,WSTSIZ
;6	NEG	CX
	DAD	B		;ES:HL-> begin of new structure
	RET
;
; WIZOOM - Zoom current window to fill entire screen (except status line).
;
;	   WWZMFL = 81 when auto-zoom, i.e. HELP or [FILE]-Directory
;		  = 01 when user-zoom, i.e. YWZ  or [WINDOW]-Zoom
;
				;{CLRSCR,HELPEX,[WINDOW],YWCMD,WININI}
	BPROC	WIZOOM
WIZOOM:	MVI	A,1		;Value to clear window
				;{8086 EXE,DEZOOM}
WZOOM0:	PUSHA
	CALL	STNSNL		;Ensure visual window gets rewritten
	LD16	H,PYLLEN-1	;H = physical line length
	DCR	H		;Adjust for counting from zero
	MVI	L,00		;Value for WWBGCO
	ST16	WWBGCO,H	;Set WWBGCO and WWENCO to full screen
;
	LD16	H,PYLINE-1	;H = # physical lines
	MVI	L,2		;L = assume status line on top
	CPIB$	STLINE,1	;Use top line for status line?
	BEQ	..1		;Yes, HL set correctly
	DCR	L		;L = 1
	DCR	H		;H = status line on bottom
..1:	ST16	WWBGLN,H	;Set WWBGLN and WWENLN to full screen
;
	CLR$	HILIWW		;Make sure we rewrite WINDOW highlighted
	TSTM	WWZMFL		;Is zoom-flag already set?
	JRNZ	..2		;Yes, may be user-zoom
	MVI	M,81H		;No, set flag that window auto-zoomed
..2:	CALL	ADJWIN		;Set other parameters
	POPA			;Restore flag
	ORA	A		;Is window to be cleared?
	RZ			;No, return now
	JMP	CLRWIN		;Clear window and home cursor
	EPROC	WIZOOM
;
; YWZOOM - Perform "YWZ" or "-YWZ" commands
;
YWZOOM:	CPIB$	RMINUS,'-'	;Is this "-YWZ"?
	JRZ	WIZOOM		;Yes, same as auto-zoom
				;No, perform user-zoom
				;{[WINDOW],YWZOOM^}
USZOOM:	CALL	WIZOOM		;Zoom the window
	MVIB$	WWZMFL,1	;Set flag that user zoomed window
	RET
;
; DEZOOM - Rewrite entire screen.
;
;;DEZOOM:	CALL	STBRFL		;Set to rewrite status line
;;	XRA	A		;Flag for WZOOM0 not to clear
;;	CALL	WZOOM0		;Zoom window without clearing it
;;	LDA	WWNAME		;Switch to itself
;;	JMP	SWWIN0		;This causes WWZMFL -> CALL WIBORD
;
; MAXWIN - Calculate # of windows in "register".
;	   Retrn: # in B and A
;		  'Z' if only one text window (i.e B = 1)
;		  ES:HL-> structure
;		  DE = size of one structure
;
	BPROC	MAXWIN
MAXWIN:	MVI	A,REGWIN	;Register # for window stuff
	CALL	PNTRG0		;ES:DE-> begin of structures
				;BC = size of entire stucture
	PUSH	D		;Save PTR
	MOV	H,B
	MOV	L,C		;HL = entire size
	LXI	D,WSTSIZ	;DE = size of one stucture
	CALL	DVHLDE		;C = # structure
	MOV	B,C		;Return in B
	POP	H		;HL-> structure
	MOV	A,B		;Get # in AL
	CPI	1		;Return 'Z' if just one text window
	RET
	EPROC	MAXWIN
;
; WSTCAL - Calculate HL-> window structure for window # in A.
;	   Retrn: ES:HL-> window.
;		  BC and DE saved.
;		  'C' if window does not exist.
;
	BPROC	WSTCAL
WSTCAL:	CALL	SV%BD		;Save BC and DE
	PUSHA			;Save A
	CALL	MAXWIN		;B = # window structures to check
				;DE = size of structure
				;ES:HL-> first window structure
	POPA			;A = desired window #
..1:	CMP%ES	M		;Found desired window?
	RZ			;Yes, HL-> desired window
	DAD	D		;HL-> next structure
	DJNZ	..1		;Check all stuctures
	STC			;Set 'C'
	RET			;Return 'C' that window not found
	EPROC	WSTCAL
	.PAGE
;
; SWWIND - Switch to a new window if it exists; else stay in current window.
;	   If WWZMFL set, don't copy size parameters, let WIBORD rewrite screen.
;	   Enter: A = desired window #.
;	   Retrn: Window switch made if possible, cursor position reset.
;
				;{YWCMD}
SWWI00:	STA	COMMWI		;Save as new window for command mode
;
				;{HELPEX,VEDCMD,WINOUT,WIBORD,UPDVIS,YWCMD}
	BPROC	SWWIND
SWWIND:	MOV	C,A		;Save in A
	ANIB$	WWZMFL,80H	;Is this auto-zoom?
	MOV	A,C		;Restore
	JRNZ	SWWIN0		;Yes, de-zoom the window
	LXI	H,WWNAME	;HL-> current window #
	CMP	M		;Same?
	JRZ	SWWDON		;Yes, only reset cursor position
;
				;{YWCREA}
SWWIN0:	MOV	C,A		;Save desired window #
	CPI	'?'		;;Switch to status line?
	BEQ	..0		;;Yes, branch
	CALL	WSTCAL		;Does the desired window exist?
	JRC	SWWDON		;No, only reset cursor position
				;This prevents re-write when window zoomed
;
;	Save current parameters in table for old window
;
..0:	PUSH	B		;Save C = desired window
	LDA	WWNAME		;Get window # for current parameters
	CALL	WSTCAL		;ES:HL-> desired window structure
	JRC	..2		;Branch if not found
				;Perhaps trying to save status line (UPDVIS)
	INX$	H		;HL->.WWMODE
	LDA	WWMODE		;Get current value
	MOV%ES	M,A		;Save it, even if window zoomed!
;
;	Only copy size parameters if WWZMFL == 0
;
	TST$	WWZMFL		;Current window zoomed?
	JRNZ	..1		;Yes, branch
	INX$	H		;HL-> .WWSPEC
	XCHG			;DE-> destination
	LXI	H,WWSPEC	;HL-> source of next parameter
	LXI	B,WSTSIZ-2	;BC = size of all but first two
	MVXO			;Copy these parms
	JMPR	..2
;
..1:	LXI	B,8		;Offset to ATTRIB
	DAD	B		;HL-> .ATTRIB
	XCHG			;DE-> destination
	LXI	H,ATTRIB	;HL-> source of next parameter
	LXI	B,14		;BC = remaining parms
	MVXO			;Copy these parms
;
;	Copy current values from new window to parameters
;
..2:	POP	B		;C = desired window #
	MOV	A,C		;A = #
	CPI	'?'		;;Switch to status line?
	JZ	SWSTAT		;;Yes, jump to special routine
				;;Old window stored in structure and SAVEWW[]
	CALL	WSTCAL		;ES:HL-> desired window structure
	JRC	SWWDON		;Use current window if desired doesn't exist
				;(Probably can't happen due to check above)
;
	LXI	D,WWNAME	;DE-> destination of first variable
	LXI	B,WSTSIZ	;BC = count
	MVXI			;Make the copy
	CALL	ADJWIN		;Set variables from window parameters
;
	TSTM	WWZMFL		;Was the screen zoomed?
	MVI	M,00		;Reset the flag in any case
	JNZ	WIBORD		;Yes, write all borders and all visual mode windows
;
SWWDON:	JMP	WINRST		;Reset cursor position and return
	EPROC	SWWIND
	]			;<IF WINDOO>
	.PAGE
;
; SWBACK - Switch back to original window.
;	   Don't bother saving status line window variables (for now anyway).
;
SWBACK:	LXI	H,SAVEWW	;HL-> where window variables are saved
	JMPR	SWSTBK		;Merge to copy variables, JMP ADJWIN
;
; SWSTAT - Switch to status-line (fake) window; WWNAME = '?'.
;	   Save current window in SAVEWW.
;
SWSTAT:	LXI	H,WWNAME	;HL-> source for block move
	LXI	D,SAVEWW	;DE-> save area
	LXI	B,WSTSIZ	;Size of block move
	LDIR			;Block move (intra-segment)
;
	LXI	H,STATWW	;HL-> source for block move
SWSTBK:	LXI	D,WWNAME	;DE-> window variables
	LXI	B,WSTSIZ	;Size of block move
	LDIR			;Block move (intra-segment)
;
	CALL	ADJWIN		;Adjust other window parameters
	JMP	WINRST		;Reset cursor position
;
; ADJWIN - Adjustments for window parameters outside window structure
;
				;{WIZOOM,WINCHK,SWWIND,SWSTAT,YWCMD}
	BPROC	ADJWIN
ADJWIN:	PUSH	D
	LDA	WWBGCO
	MOV	B,A
	LDA	WWENCO
	SUB	B
	STA	WWLLE1
	INR	A
	STA	WWLLEN
;
	LDA	WWBGLN
	MOV	B,A
	LDA	WWENLN
	SUB	B
	STA	WWNLI1
	INR	A
	STA	WWNLIN
;
	LXI	H,WWUSLN	;HL-> # used lines on screen
	CMP	M		;Is WWUSLN > WWNLIN?
	BGE	..1		;No, branch
	MOV	M,A		;Yes, set WWUSLN to max allowed
;
..1:	LXI	H,WINVER	;HL-> vertical window pos
	CMP	M		;Is WINVER > WWNLIN?
	BGE	..2		;No, branch
	MOV	M,A		;Yes, set WINVER to max allowed
;
..2:	CALL	HZENST		;Set HZSCEN from HZSCBG and WWLLEN
	CPIB$	WWNAME,'?'	;Is this the status line?
	JRZ	..END		;Yes, don't bother with rest
;
	LDA	APGSZ		;AL = customized PAGESZ
	CALL	SWWADJ		;Compute adjustment
	ORA	A		;Is value zero?
	JRNZ	..3		;No, branch
	INR	A		;Yes, set to 1
..3:	STA	WWPGSZ		;Set page size for window
;
	LDA	ACRVTTP		;AL = customized CRVTTP
	DCR	A		;Compute # margin lines
	CALL	SWWADJ		;Compute adjustment
	INR	A		;Adjust
	STA	CRVTTP		;Set top cursor line for window
;
	LDA	PYLINE		;AL = # screen lines
	DCR	A		;Adjust for status line
	LXI	H,ACRVTBT	;HL-> customized CRVTBT
	SUB	M		;AL = # margin lines
	CALL	SWWADJ		;Compute adjustment
	LDA	WWNLIN		;AL = # lines in window
	SUB	C		;Subtract margin
	STA	CRVTBT		;Set bottom cursor line for window
;
..END:	POP	D
	RET
	EPROC	ADJWIN
;
; SWWADJ - Compute adjustment for CRVTTP and CRVTBT.
;	   Retrn: Adjusted value in AL and CL.
;
	IF	WINDOO, [
SWWADJ:	LD16	D,WWNLIN
	MVI	D,00		;DE = # lines in window
	MOV	L,A
	MOV	H,D		;HL = full screen value
	CALL	MULTIP		;HL = product
	LD16	D,PYLINE
	MVI	D,00		;DE = # screen lines
	DCR	E		;Adjust for status line
	CALL	DVHLDE		;BC = best value for window
	MOV	A,C
	RET

	] [

SWWADJ:	MOV	A,L		;Just return entry value
	RET
	]
;
; WISCST -
;
WISCST:	MOVB	HZSCBG,VSHZBG
HZENST:	ADDB	HZSCBG,WWLLEN
	STA	HZSCEN
	RET
	.PAGE
;
; WIBORD - Write the border for all windows on screen.
;	   Also updates all visual mode windows.
;	   Enter: Just a valid window structure.
;	   Retrn: Restored edit buffer, and window.
;		  Restores current status of REGNUM, etc.
;
				;{YWCMD,YWDELE,SWWIND}
	BPROC	WIBORD
WIBORD:
;6	PUSH	DS		;Save current segment
;6	CALL	RSTSEG		;Switch to edit segment
	LDA	REGNUM
	PUSHA
	LDA	INPFLG		;Get current input flag
	PUSHA			;Save on stack
	LD16	H,WINVER	;L = WINVER
	PUSH	H		;Save on stack
	LDA	WWNAME		;Get current window name
	PUSHA			;Save on stack
	LDA	EDTNUM		;Get current edit buffer #
	PUSHA			;Save on stack
;
	CALL	MAXWIN		;B = # window structures to check
				;ES:HL-> first window structure
				;'Z' if only one text window
	JZ	UPDDON		;Don't need any borders if only one window
;
;	CRT Version - May want to clear entire screen here
;
	CLR$	HILIWW		;Make sure highlighted WINDOW rewritten
;
..1:
;6	PUSH	ES
	PUSH	H
	MOV%ES	A,M		;AL = window #
	INX$	H		;HL-> WWMODE
	MOV%ES	C,M		;CL = WWMODE
	PUSH	B
;
	CALL	SWWIND		;Switch to the window now
	CALL	CLRWIN		;Clear the window
				;Needed here, in case window has attributes
	CALL	STNSNL		;Make sure Visual window rewritten
	POP	B
	PUSH	B
	MOV	A,C		;AL = WWMODE of current window
	DCR	A		;;Does window contain visual mode?
	JRM	..3		;;No, just write border for window
;
	INR	A		;;AL = desired buffer #
	CALL	CONVRG		;Convert ASCII to binary
	CALL	UPEEB		;Switch to edit buffer
	CALL	UPVISW		;Update visual mode window
;
..3:	POP	B
	POP	H
;6	POP	ES
	CALL	WIBOR3		;Write border for window (regs saved)
	LXI	D,WSTSIZ	;DE = size of structure
	DAD	D		;HL-> next structure
	DJNZ	..1		;Loop for all structures
;
	JMP	UPDDON		;Restore current status
	EPROC	WIBORD
;
; WIBOR3 - Write the border for one window.
;	   ES:HL-> window structure in high memory.
;	   Preserves BC,HL & ES.
;
	BPROC	WIBOR3		;{WIBORD,WISFW6 in t3}
WIBOR3:	PUSH	B		;Save BC
	PUSH	H
;6	PUSH	ES
;
;6	PUSH	ES
	CALL	WIBTOP		;Write "=WINDOW" part of border
				;B = # columns of "=" needed
	PUSH	H		;Save -> .WWBGLN
	CALL	ATTBRD		;Set window border attribute
..1:	LDA	BRCHHR		;Get "="
	CALL	OUTCHA		;Display
	DJNZ	..1
;
;	Setup to write side border.
;
	POP	H		;ES:HL-> .WWBGLN
;6	POP	ES
	CALLES	MVINDE		;E =  .WWBGLN, D = .WWENLN
				;ES:HL-> .WWBGCO
	MOV	A,D
	SUB	E		;A = number of line in window (-1)
	INR	A		;Adjust
	MOV	B,A
	MOV%ES	A,M		;A = beginning column
	DCR	A		;Adjust to border column
	JRM	..9		;Branch if this is only horizontal window
;
	MOV	H,A		;H = border column
	MOV	L,E		;L = first line for window
;
;	Loop for side border, writing "|"
;
..2:	PUSH	H
	CALL	PHYSET		;Position cursor
	LDA	BRCHVT		;Get "|"
	CALL	OUTCHA		;Display
	POP	H
	INR	L		;Set to next line
	DJNZ	..2		;Loop for all window lines
;
..9:	CALL	ATTFOR		;Restore window foreground attribute
;6	POP	ES
	POP	H		;Restore HL
	POP	B
	RET
	EPROC	WIBOR3
	.PAGE
;
; WIBTOP - Write window's border reverse/normal "WINDOW".
;	   Enter: ES:HL-> window structure
;		  WIHLFL != 0 if message to be highlighted
;	   Retrn: HL-> .WWBGNL in window structure
;		  B = # remaining border line columns to fill with "="
;
	IF	P8086, [
	DSEG	$
CRNCHR:	DS	1		;Corner char
	CSEG	$
	] [
CRNCHR	=	BRCHHR
	]
;
				;{WIBOR3,BRDNM3}
	BPROC	WIBTOP
WIBTOP:	MOV%ES	E,M		;E = window #
	LXI	B,5		;Offset
	DAD	B		;HL-> .WWBGLN
	PUSH	H		;Save -> .WWBGLN
	MOV%ES	D,M		;D = .WWBGLN
	DCR	D		;D = line for border
	INX$	H
	INX$	H		;HL-> .WWBGCO
;
;	Determine which border character to use
;
	IF	P8086, [
	MOV%ES	A,M		;A =  .WWBGCO
	PUSH	H		;Save HL-> .WWBGCO
	LXI	H,BRCHHR	;HL-> default char
	ORA	A		;Window begin in first column?
	JRZ	..1		;Yes, there is no side border
	LXI	H,BRUPLF	;HL-> upper left char
	DCR	A		;Left edge of screen?
	JRZ	..1		;Yes, branch
	LXI	H,BRUPRT	;No, use T-corner char
;
..1:	MOV	C,A		;Save begin border column #
	MOV	A,M		;A = corner char to use
	STA	CRNCHR		;Save char
	POP	H		;Restore ES:HL-> .WWBGCO
	] [
	MOV%ES	C,M		;C = .WWBGCO
	DCR	C		;Adjust to border column
	JRP	..1		;Branch if OK
	INR	C		;In case of no side border
..1:
	]
;
	INX$	H		;HL-> .WWENCO
	MOV%ES	A,M		;A = end column for window
	SUB	C		;A = # columns in window (-1)
	SUI	8		;Account for "=WINDOW #"
	MOV	B,A		;Save in B
	PUSH	B		;Save B
	MOV	H,C		;H = column for border
	MOV	L,D		;L = line for border
	CALL	PHYSET		;Position
;
	CALL	ATTBRD		;Window border attribute
	LDA	CRNCHR		;Get corner char
	CALL	OUTCHA		;Display
	TSTM	WIHLFL		;Should window name be highlighted?
	MVI	M,00		;Clear flag
	JRZ	..2		;No, branch
	CALL	ATTBMS		;Yes, enable reverse video
..2:	LXI	H,WWWMSG	;HL-> "WINDOW "
	CALL	PHYSTR		;Send chars directly to OUTCHR()
	MOV	C,E		;Get window #
	CALL	OUTCHR		;Display
	CALL	ATTFOR		;Restore main text attribute
;
	POP	B		;B = # remaining columns
	POP	H		;HL-> WWBGLN
	RET
	EPROC	WIBTOP
;
; - Messages
;
PHYSTR:	MOV	A,M
	ORA	A
	RZ
	PUSH	H
	CALL	OUTCHA
	POP	H
	INX$	H
	JMPR	PHYSTR
	.PAGE
;
; UPEEB - Switch to edit buffer in reg. A.
;	  Saves REGNUM and RMINUS.
;
				;{WIBORD,UPDVIS,UPDDON}
UPEEB:	LXI	H,EDTNUM	;HL-> current edit buffer #
	CMP	M		;Already in desired buffer?
	RZ			;Yes, nothing to do
				;Prevents setting unneeded NWSCFL
	MOV	C,A		;Save in C
	LDA	REGNUM		;Save REGNUM
	PUSHA
	LDA	RMINUS
	PUSHA
	MOV	A,C		;A = desired edit buffer
	STA	REGNUM		;Save in REGNUM for OPENBF()
	MVIB$	RMINUS,'-'	;Make sure no auto-squeeze
	CALL	EECMD2		;Switch to edit buffer in REGNUM
	POPA
	STA	RMINUS
	POPA
	STA	REGNUM		;Restore REGNUM (prevents big blowups)
	RET
;
; UPVISW - Rewrite visual mode window.
;	   Note: It is possible for visual mode to call itself recursively!!
;		 (Via VEDCMD-SWWIND-WIBORD)  Thats why VSTACK has to be saved.
;
UPVISW:	MVIB$	WWUPFL,1	;Set flag that only updating
	LHLD	VSTACK		;Get current visual stack-save  value
	PUSH	H		;Push it on the stack
	CALL	VEDCMD		;Rewrite visual mode window (Clears WWUPFL)
	POP	H		;Restore VSTACK value
	SHLD	VSTACK		;Restore
	RET
	.PAGE
;
; UPDVIS - Update all visual mode windows.
;	   WWNDUP - flag for current edit buffer.
;	   .WWNDUP - flags for other edit buffers.
;	   SCNDUP - flag set if any .WWNDUP is set.
;
	IF	WINDOO, [
				;{GETCHR,EQCMD}
	BPROC	UPDVIS
UPDVIS:	TST$	WWZMFL		;Is current window zoomed?
	RNZ			;Yes, can't update
;

	TSTM	WWNDUP		;Window need update?
	RZ			;No, don't need update (just <CR> probably)
	MVI	M,00		;Clear the flag
;
	CALL	MAXWIN		;B = # window structures to check
				;ES:HL-> first window structure
				;'Z' if only one text window
	RZ			;Yes, nothing to update if only one window
;
;	Save current status
;
;6	PUSH	DS		;Save current segment
;6	CALL	RSTSEG		;Switch to edit segment
	LDA	REGNUM
	PUSHA
	LDA	INPFLG		;Get current input flag
	PUSHA			;Save on stack
	LD16	H,WINVER	;L = WINVER
	PUSH	H		;Save on stack
	LDA	WWNAME		;Get current window name
	PUSHA			;Save on stack
	LDA	EDTNUM		;Get curent edit buffer name
	PUSHA			;Save on stack
;
	CALL	RTOASC		;Convert to ASCII
	MOV	C,A		;Save in C
	LDA	WWNAME		;Get window #
	CPI	'?'		;Is this the status line?
	BNE	..0		;No, branch
	LDA	SAVEWW		;Yes, get real window #
..0:	CMP	C		;Are we in window to be updated?
	JRZ	UPDOTH		;Yes, don't update current window
	MOV	A,C		;A = EDTNUM (ASCII)
	CALL	WSTCAL		;ES:HL-> window structure
	JRC	UPDOTH		;Branch if window not in use
	INX$	H		;ES:HL-> .WWMODE
	MOV%ES	B,M		;B = mode flag

	IF	CRTVRS, [
	LXI	D,WUPOFS-1	;Offset to .WWNDUP; account for INX H above
	DAD	D		;HL-> .WWNDUP
	MOV%ES	A,M		;Get .WWNDUP
	ORA	A		;Is the flag set?
	JRNZ	UPOTH1		;Yes, update below, prevent double write
	]

	MOV	A,B		;A = mode
	CMP	C		;Window contain corresponding edit buffer?
	CZ	UPVISW		;Yes, update visual mode window
;
;	Update (other) windows which contain visual mode and
;	have WWNDUP flag set.
;
	PUBLIC	UPDOTH
UPDOTH:	TSTM	SCNDUP		;Does any window need an update?
	JRZ	UPDDON		;No, don't need update (just <CR> probably)
	MVI	M,00		;Yes, reset flag
;
UPOTH1:	CALL	MAXWIN		;B = # window structures to check
				;ES:HL-> first window structure
..1:	INX$	H		;HL-> WWMODE
	MOV%ES	A,M		;Get mode of window
	DCX$	H		;HL-> WWNAME
	DCR	A		;;Does window contain visual mode?
	JRM	..3		;;No, check next window
	INR	A		;;AL = WWMODE
	CMP%ES	M		;Is window # same as edit buffer #?
	BNE	..3		;No, check next window
				;Possible for window to contain other buffer!!
	MOV	C,A		;C = name of edit buffer in window
	PUSH	H
	LXI	D,WUPOFS	;Offset to WWNDUP flag
	DAD	D		;HL-> WWNDUP
	MOV%ES	A,M		;Get flag
	MVI%ES	M,00		;Clear flag
	POP	H		;Restore HL-> WWNAME
	ORA	A		;Is flag set?
	JRZ	..3		;No, check next window
;
	PUSH	H
	PUSH	B
;6	PUSH	ES
	MOV	A,C		;A = name of edit buffer in window
	CALL	CONVRG		;Convert ASCII to binary
	CALL	UPEEB		;Switch to edit buffer

	IF	CRTVRS, [
	CALL	STNSNL		;Rewrite entire window
	]

	CALL	UPVISW		;Update visual mode window
;6	POP	ES
	POP	B
	POP	H
;
..3:	LXI	D,WSTSIZ	;DE = size of structure
	DAD	D		;HL-> next structure
	DJNZ	..1		;Loop for all structures
;
UPDDON:	POPA			;A = "real" EDTNUM
	CALL	UPEEB		;Restore original edit buffer
	POPA			;A = original window #
	CALL	SWWIND		;Restore original window
	POP	H		;HL = window position
	CALL	WINSET		;Restore cursor pos.
	POPA			;A = original INPFLG
	STA	INPFLG		;Restore
	POPA
	STA	REGNUM
;6	POP	AX
;6	CALL	SWAPSG		;Return to original segment
	RET			;WOW!!
	EPROC	UPDVIS
	]			;<IF WINDOO>
;
; STUPFL - Set .WWNDUP in corresponding window.
;	   Set  SCNDUP flag too.
;
				;{EECMD1}
	PUBLIC	STUPFL,STUPF1
	BPROC	STUPFL
STUPFL:	LDA	EDTNUM		;Get current edit buffer #
				;{RMCMD}
STUPF1:	CALL	RTOASC		;Convert to ASCII
	CALL	WSTCAL		;ES:HL-> window structure
	RC			;Return if window does not exist
;
;	Set .WWNDUP
;
				;{T3-UPDONE}
STUPF2:	LXI	D,WUPOFS	;Offset to WWNDUP flag
	DAD	D		;HL-> WWNDUP
	MVI	A,-1		;Get non-zero (see V0 for CM NWSCFL)
	STA	SCNDUP		;Flag that some window(s) needs update

	IF	CRTVRS, [
	STA	WWNDUP		;Make sure UPDVIS executed
	]

	MOV%ES	M,A		;Set flag
	RET			;Done, return
	EPROC	STUPFL
	.PAGE
;
; YECMD - CRT Emulation command
;
	BPROC	YECMD
YECMD:	CALL	NXCMUC		;A = next command char UC
	LHLD	ITRCNT		;Get user value
	MOV	C,L		;Save in C
	LD16	H,WINVER	;Get current cursor pos
	CPI	'C'		;Clear screen?
	JZ	CLRWIN
	CPI	'L'		;EOL?
	JZ	WINEOL

	IF	WYSE, [
	CPI	'M'		;;Mode?
	JZ	..YEM
	]

	CPI	'S'		;EOS?
	JZ	WINEOS
	CPI	'H'		;Set horizontal cursor?
	BEQ	..YEH
	CPI	'V'		;Set vertical cursor?
	BEQ	..YEV
	CPI	'A'		;Attributes?
	JNZ	C2BR8K		;None - give error
;
;	n1,n2YEA - Change attribute
;
..YEA:	MOV	L,C		;L = user value
	MOV	H,L		;In case only one argument
	CPIB$	NM2FLG,COMMA	;Are there two arguments?
	BNE	..1		;No, branch
	LD8	ITRCN2		;Yes, get 2nd argument
	MOV	H,A		;H = background attribute
..1:	ST16	WWFRAT,H	;Store in white-window fraternity
	MOVB	WWUSLN,WWNLIN	;Make sure window rewritten
	JMP	ATTFOR		;Set new foreground attribute
;
;	nYEH - Set horizontal screen position.
;
..YEH:	MOV	H,C
	JMP	WINSET		;Set cursor position
;
;	nYEV - Set vertical screen position.
;
..YEV:	MOV	L,C
	JMP	WINSET		;Set cursor position
;
;	nYEM - Change Screen-Hardware Mode
;
	IF	WYSE, [
..YEM:	RET
	]
	EPROC	YECMD
	.PAGE
	IF	WINDOO, [
;
; YWCMD - Create a new window in current window/ Switch windows/ Delete/ Zoom
;
;	  Syntax:  YWD		Delete current window
;		   YWDr		Delete window 'r'
;		   YWI		Init all windows
;		   YWTr	n	Create window 'r' of 'n' lines at top
;		   YWBr	n	Create window 'r' of 'n' lines at bottom
;		   YWLr	n	Create window 'r' of 'n' columns at left
;		   YWRr	n	Create window 'r' of 'n' columns at right
;		   YWSr		Switch to window 'r'
;		   YWZ		Zoom current window
;
	DSEG	$
WWNEWN:	DS	1		;Name of window being created/deleted
WINTYP:	DS	1		;Quadrant of window being created - T/B/R/L
	CSEG	$
;
	BPROC	YWCMD
YWCMD:	CALL	NXCMUC		;A = next command char UC
	CPI	'Z'		;Zoom?
	JZ	YWZOOM
	CPI	'I'		;Init?
	JZ	WININI
	CPI	'D'		;Just check for valid 3d letter
	BEQ	..1
	CPI	'S'
	BEQ	..1
	CPI	'T'
	BEQ	..1
	CPI	'B'
	BEQ	..1
	CPI	'R'
	BEQ	..1
	CPI	'L'
	JNZ	C2BR8K		;None, give error
..1:	STA	WINTYP		;Save T/B/L/R as flag
	LDA	WWNAME		;Get current window #
	MOV	D,A		;Save for YWD to delete current window
;
;	Get window #, check for "#".
;
	CALL	GETCWN		;;AL = window name (checks for #w)
	MVI	E,'@'		;Value when invalid window specified
	CPI	SPACE+1		;Is window # valid?
	JRC	..3		;No, use '@'
	CPI	DEL		;Is window # valid?
	JRNC	..3		;No, use '@'
	MOV	E,A		;Yes, set to user value
	MOV	D,E		;Set window # to delete
;
..3:	CPIB$	WINTYP,'D'	;Is this window delete?
	JZ	YWDELE		;Yes, delete the window # in %D
	CPI	'S'		;Is this window switch?
	MOV	A,E		;Assume yes, A = desired window
	JZ	SWWI00		;Yes, switch to the window
;
	CALL	GETDEC		;HL = line/column argument for new window
	MOV	A,L		;Is return 00?
	ORA	A
	JRZ	..ERR		;Yes give error (Needed for YWB$ <CR>)
	MOV	D,L		;D = # rows
	MOV	A,E		;A = name of new window
	CPI	'?'		;;Trying to create status line window?
	BEQ	..ERR		;;Yes, give error
	CALL	WSTCAL		;Does window already exist?
	JRNC	..ERR		;Yes, give error
;
	CALL	YWCREA		;Create the window
	JNC	WIBORD		;Write new borders if window created OK
;
..ERR:	JMP	PRMBRK		;Give error, stack will be reset
	EPROC	YWCMD
;
; YWCREA - Create the window.
;	   Enter: D = # row/columns, E = name of window, WINTYP = location.
;	   Retrn: 'NC' if window created, window structure updated.
;		  'C'  if parameter error, window not created.
;
;	If only one current window adjust it for border line
;
				;{YWCMD,YWDELE}
	BPROC	YWCREA
YWCREA:	PUSH	D		;Save DE
	LDA	WWNAME		;Get # of current window
	CALL	SWWIN0		;Just make sure window no longer zoomed
	CALL	MAXWIN		;B = # windows
				;'Z' if only one text window
	POP	D		;Restore DE
	JRNZ	..2		;Branch if top border already set
	LXI	H,WWBGLN	;Yes, make room for border above window
	INR	M
;
..2:	LDA	WINTYP		;Get type of window to create
	CPI	'T'		;Is this a top/bottom split?
	BEQ	..3
	CPI	'B'
	BEQ	..3		;Yes, don't need to check for side border
;
	MOV	A,D		;Get # desired columns
	CPI	10		;Less than 10?
	JC	..ERR		;Yes, give error
	CALL	WILINC		;Does window extend full screen width?
	JRC	..3		;No, already adjusted for border
	LXI	H,WWBGCO	;Yes, make room for side border
	INR	M
..3:	CALL	ADJWIN		;Adjust window parameters
;
;	Check if desired window is too big
;
	MVI	B,10		;B = minimum # columns in window
	LDA	WINTYP		;Get type of window to create
	CPI	'L'		;Is this a left/right split?
	BEQ	..4
	CPI	'R'
..4:	LDA	WWLLEN		;A = # columns in old window
	BEQ	..5		;Yes, branch
	LDA	WWNLIN		;A = # lines in old window
	MVI	B,2		;B = minimum # lines in window
..5:	SUB	B		;Subtract minimum size for old window
	CMP	D		;Enough room for new window?
	JC	..ERR		;No, error
;
;	Window parameters are good, so create new window structure
;	Append space to existing window "text register"
;
	PUSH	D		;Save DE
	CALL	WINREG		;Append to window "register"
				;ES:HL-> begin of new window structure
;
;	Setup WWNAME and WWMODE of new window
;
	POP	D		;D = # rows/columns in new window, E = new window #
	MOV	A,E		;Get new window #
	STA	WWNEWN		;Save name of new window
	MOV%ES	M,A		;Save as WWNAME of new window
	INX$	H
	MVI%ES	M,-1		;;WWMODE = -1 (empty value)
	INX$	H
;
;	Save creation location and size
;
	LDA	WWNAME		;Get name of current (parent) window
	MOV%ES	M,A		;Save in window
	INX$	H
	LDA	WINTYP		;Get location (AL needed below)
	MOV%ES	M,A		;Save in window
	INX$	H
	MOV%ES	M,D		;Save original size in window
	INX$	H
;
;	Setup values for new top/bottom (horizontal) window
;
	LD16	B,WWBGLN	;C = old WWBGLN, B = WWENLN
	CPI	'L'		;Is this a left/right split?
	BEQ	..LR
	CPI	'R'
	BEQ	..LR		;Yes, branch for vertical window
;
	CPIB$	WINTYP,'B'	;New window at bottom of screen?
	BEQ	..B1		;Yes, branch
;
;	Setup values when new window is at top of screen
;
..T1:	MOV	A,C		;A = old WWBGLN
	MOV%ES	M,A		;Set new WWBGLN same as old
	INX$	H
	ADD	D		;Add number of rows in new window
	DCR	A		;Adjust
	MOV%ES	M,A		;Set new WWENLN
	INX$	H
	ADI	2		;Skip over border and to next line
	STA	WWBGLN		;New WWBGLN for old window
	JMPR	..TB2
;
;	Setup values when new window is at bottom of screen
;
..B1:	MOV	A,B		;A = old WWENLN
	SUB	D		;Account for new window
	INR	A		;Adjust
	MOV%ES	M,A		;Save as WWBGLN
	INX$	H
	SUI	2		;Compute WWENLN for old window
	STA	WWENLN		;Save
	MOV%ES	M,B		;Set new WWENLN
	INX$	H
;
..TB2:	LD16	B,WWBGCO	;C = old WWBGCO, B = WWENCO
	MOV%ES	M,C
	INX$	H
	MOV%ES	M,B
	INX$	H
	JMPR	..COMM		;Branch down
;
;	Setup values for left/right (vertical) windows
;
..LR:	MOV%ES	M,C
	INX$	H
	MOV%ES	M,B
	INX$	H
;
	LD16	B,WWBGCO	;C = old WWBGCO, B = WWENCO
	CPIB$	WINTYP,'R'	;New window at right of screen?
	BEQ	..R1		;Yes, branch
;
;	Setup values when new window is at left of screen
;
..L1:	MOV	A,C		;A = old WWBGCO
	MOV%ES	M,A		;Set new WWBGCO same as old
	INX$	H
	ADD	D		;Add number of columns in new window
	DCR	A		;Adjust for counting from 0
	MOV%ES	M,A		;Set new WWENCO
	INX$	H
	ADI	2		;Skip over border and to next columne
	STA	WWBGCO		;New WWBGCO for old window
				;WWENCO for old window is unchanged
	JMPR	..COMM
;
;	Setup values when new window is at right of screen
;
..R1:	MOV	A,B		;A = old WWENCO
	SUB	D		;Account for new window
	INR	A		;Adjust
	MOV%ES	M,A		;Save as WWBGCO
	INX$	H
	SUI	2		;Compute WWENCO for old window
	STA	WWENCO		;Save
	MOV%ES	M,B		;Set new WWENCO
	INX$	H
;
;	Rest of new window structure gets initialized
;
..COMM:	LDA	WWFRAT		;Get attribute for text chars
	MOV%ES	M,A		;Save in ATTRIB
	INX$	H		;HL-> WWFRAT
	MOV%ES	M,A
	LDA	WWBKAT		;Get attribute for screen-erase character
	INX$	H		;HL-> WWBKAT
	MOV%ES	M,A
	INX$	H		;HL-> UPDCUR
;
	LXI	D,WSTINI	;DE-> values to init rest of structure
	XCHG
	LXI	B,WSTINM	;C = # values
	MVXO			;Init rest
;
;	Switch to the new window and clear it
;
	LDA	WWNAME		;Get current window name
	PUSHA			;Save it
	LDA	WWNEWN		;A = name of new window
	CALL	SWWIN0		;Switch to new window
				;Also saves adjusted values into structure
	CALL	CLRWIN		;Clear window and home cursor
	POPA			;Restore window name
	CALL	SWWIN0		;Back to original window
	XRA	A		;'NC'
	RET
;
..ERR:	CALL	WINCHK		;May need to reset "full" window
	STC
	RET			;Return 'C'
	EPROC	YWCREA
;
; YWDELE - Delete a window, recreate all existing windows.
;	   Enter: %D = Window to delete
;
	BPROC	YWDELE
YWDELE:	MOV	A,D		;A = # of window to delete
	STA	WWNEWN		;Save it
	CPI	'@'		;Is this default window?
	RZ			;Yes can't delete default window
	CALL	MAXWIN		;B = # of current windows
	RZ			;Can't delete if only one window
				;ES:HL-> window stucture
;
	LDA	WWNAME		;Save current window
	PUSHA			;Save it
;
	DAD	D		;Skip over the default window
	DCR	B		;Don't count the default window
;
	PUSH	B		;Save B
	LXI	D,ACTBUF	;Use active line as work area
..1:	PUSH	B		;Save B
	MOV%ES	C,M		;C = name of window
	LDA	WWNEWN		;Get # of window to delete
	CMP	C		;Is this window to be deleted?
	BEQ	..2		;Yes, skip over it
	PUSH	H
	LXI	B,5		;Copy first five bytes
	MVXI			;Make the copy from (HL) to (DE)
	POP	H		;HL-> .WWNAME
..2:	LXI	B,WSTSIZ	;Get element size
	DAD	B		;HL-> next window element
	POP	B		;B = remaining windows
	DJNZ	..1		;Repeat for all windows
;
;	Now recreate all windows after initializing first one
;
	CALL	WININ0		;Initialize first window structure
	POP	B
	DCR	B		;Account for deleted window
	JRZ	..4		;Branch if no windows left
	LXI	H,ACTBUF	;HL-> work area
..3:	PUSH	B		;Save count
	PUSH	H		;Save-> WWNAME
	INX$	H
	INX$	H		;HL-> parent
	MOV	A,M		;Get parent
	CALL	SWWIND		;Switch to parent (if it still exists)
	POP	H		;HL-> WWNAME
	MOV	E,M		;E = name of window
	INX$	H
	INX$	H
	INX$	H		;HL-> location
	MOV	A,M		;A = location T/B/R/L
	INX$	H		;HL-> size
	MOV	D,M		;D = Size
	INX$	H		;HL-> next 5 byte block
	STA	WINTYP		;Save where used by YWWCMD
	PUSH	H
	CALL	YWCREA
	POP	H
	POP	B		;B = remaining count
	DJNZ	..3		;Repeat for all windows
;
..4:	CALL	WIBORD		;Rewrite border
	POPA			;Restore A = original window
	JMP	SWWIND		;Back to original window (if it exists)
	EPROC	YWDELE
	.PAGE
;
; WINCHK - Adjust window parameters if window border(s) are to be removed.
;
				;{YWCMD,YWCLOS}
	BPROC	WINCHK
WINCHK:	CALL	WIEOLC		;Is Window full screen width?
	JRC	..2		;No, leave alone
	LXI	H,WWBGCO	;HL-> first window column
	MOV	A,M
	CPI	1		;Full screen width, except for border?
	JRC	..1		;Branch if no left border set
	RNZ			;No, return - also need top border
	DCR	M		;Yes, drop border column
;
..1:	LXI	H,PYLINE	;HL-> physical lines
	LDA	WWNLIN		;A = # lines in window
	INR	A		;Account for status line
	INR	A		;Account for border line
	CMP	M		;Window cover all screen lines?
	BLT	..2		;No, branch, leave border line
	LXI	H,WWBGLN	;Yes, drop border line
	DCR	M
;
..2:	JMP	ADJWIN		;Adjust window parameters
	EPROC	WINCHK
;
;
; WILINC - Check if window line extends full width of screen (no border)
;	   Retrn: 'NC' if window extends full width
;
WILINC:	LXI	H,PYLLEN	;HL-> physical columns
	LDA	WWLLEN		;A = # lines in window
	CMP	M		;Window as wide as the screen?
	RET			;Yes, return 'NC' and 'Z'
;
; WIEOLC - Check if Physical EOL can be performed in current window
;	   Retrn: 'NC' if EOL can be performed
;
WIEOLC:	LDA	WWENCO		;A = last physical column for window
	INR	A		;Adjust for counting from 0
	LXI	H,PYLLEN	;HL-> last physical screen column
	CMP	M		;Is end of window at end of screen?
	RET			;Yes, return 'NC' that EOL ok
	]			;<IF WINDOW>
;
	IFNOT	WINDOO, [
WILINC:
WIEOLC:	XRA	A		;Set 'NC'
	RET			;Return 'NC' that line full width, EOL OK
	]
;
; GETCWN - Get next command window name.  Checks for "#w".
;	   Return: AL = window name.  CMDGET updated.
;
				;{YWCMD}
GETCWN:	CALL	SV%BDH		;;Save BC, DE and HL
	CALL	GETCRN		;;REGASC = evaluated numeric register value
	LDA	REGASC		;;AL = window #
	RET
