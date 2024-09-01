	.PAGE
	.TITLE	'VEDIT-V3'
;************************************************
;*						*
;*		Screen Routines			*
;*						*
;************************************************
;
; Copyright (C) 1986 by CompuView Products, Inc.
;			1955 Pauline Blvd.
;			Ann Arbor, MI 48103
;
;	Written by: Theodore J. Green
;
;	Last Change: Ted - Oct. 27, 1985 - Combine output routines
;			   Feb. 20 - Windows
;			   Mar. 01 - Delete CALL HZENST()
;			   Apr. 10 - Optimize scroll code
;			   Apr. 20 - Command mode status line
;		     Tom - Apr. 21 - MORE bug fix (JRNC not JRC in WRTSTA)
;		     Ted - May  01 - STAKEY() new routine
;			   June 03 - "FILE" message variations
;			   July 20 - New routine STASET()
;		     Tom - July 30 - WRTSTA call new MORBUF (8086 problems)
;		     Ted - Aug. 21 - WINDOWS (color for IBMPC)
;			   Oct. 10 - CHKBRD changes, added CURLIN variable
;			   Oct. 13 - SWSTAT, SWBACK status line routines
;		     Tom - Oct. 20 - CHKBRD() reset edit segment for 8086 (GETCHR() wrinkle)
;		     Ted - Oct. 23 - WRTLIN() use WINVER and WINSET properly
;			 - Nov. 02 - EOLCHR fix
;			 - Nov. 05 - Handle lone <LF> and <LF><CR>
;			 - Nov. 07 - Changes to use POSTBL
;			 - Nov. 11 - STAKEY() uses CALL CONVUC()
;			 - Nov. 21 - Display "CNS" for Caps/Num/Scroll lock
;		     Tom - Nov. 23 - EOFCHK() handle infinite loop
;			 - Nov. 25 - WRTSTA() put up DISK when disk full
;				   - BRDNM3() for REPEAT()
;		     Ted - Dec. 02 - WTSCRL for CRTVRS
;			 - Dec. 03 - Optimize STACLR,STAPRM
;			 - Dec. 06 - Fix WTSCRL bug
;			 - Dec. 14 - CRT version changes
;
;
;	Usage of NWSCFL:
;
;	80H	Write entire screen from SCRFCB[0].
;	40H	Scroll screen by direction in NSCROL.
;	20H	Write partial screen from FSACLN.
;	10H	Perform Insert or Delete Line according to LINCHG.
;
; WRTSCR - Write a full screen.  NWSCFL specifies full or partial
;	   screen.  Partial screen written from FSACSC.
;	   Retrn: LINCNT set to # lines written, the tables
;		  SCRFCB and SCRCNT are set.
;
WRTSCR:	LXI	H,HORFLG	;HL-> Horz. flag
	LDA	HOROPT		;Get parameter option
	MOV	M,A		;Save in flag
	ORA	A		;Is flag set?
	JRNZ	WRTSC0		;Yes, branch
	TST$	VSHZBG		;Horizontal scrolling on?
	JRZ	WRTSC0		;No, branch
	MVI	M,1		;Yes, set the flag
;
;	Determine operation to be performed.  Scrolls and line inserts
;	are done immediately, rewrites are done later in Background.
;
WRTSC0:	LDA	NWSCFL		;Get the new screen flag
	RLC			;Set to entire new screen?
	JRC	WTFULL		;Yes, branch
	RLC			;Set to scroll screen?
	JRC	WTSCRL		;Yes, scroll screen if available
;
				;Else must be write partial screen
				;NOTE: - Insert/delete line no longer supported
;
;	Write a partial screen beginning with line # in FSACSC.
;	For CRT, perform a fake write on previous line.
;
WTPART:
	IF	CRTVRS, [
	MVI	B,-1		;B = -1.  Do fake write on previous line
	] [
	MVI	B,00		;Don't adjust line #
	]

	CPIB	FSACSC,2	;Attempting to write at or before top line?
	BGE	WRTSC1		;No, setup for partial write
;
WTFULL:	MVI	A,1		;Begin full screen write with line # 1
	MVI	B,00		;Start fake write on same line
;
;	WRTSC1 - Write the screen beginning with line # in reg A.
;
WRTSC1:	CALL	WRTSET		;Save line on which to start update
	ADD	B		;Line on which to begin fake write
	JMP	FAKSCR		;Setup all screen tables
;
;	WRTSET - Setup to update screen in Background.
;
WRTSET:	LXI	H,UPDLIN	;HL-> line at which to begin update
	CMP	M		;Before that line?
	RNC			;No, all set
	MOV	M,A		;Yes, start update earlier
	RET
;
; WTSCRL - Scroll the screen one line and write top or bottom line.
;
	BPROC	WTSCRL
WTSCRL:	TST$	NSCROL		;Is this a reverse scroll?
	JRM	WTSLBK		;Yes, branch
;
;	Scroll the window forward, make POSTBL[0] = POSTBL[1]
;
	MVI	A,2		;;Index into POSTBL[]
	CALL	LINPOS		;;HL = value for 2nd line
	CALL	STLGTP		;;Save in POSTBL[0]

	IF	CRTVRS, [
	CALL	WILINC		;;Is window full screen width?
	JRC	WTFULL		;;No, rewrite window
	TST$	ESCINS		;;Does CRT have insert line?
	JRNZ	..1		;;Yes, branch
	CALL	WFULLC		;;Is window full screen?
	JRC8	WTFULL		;;No, rewrite window
	]

..1:	CALL	WISFWD		;Yes, scroll the window forward
	CALL	FAKSC0		;Write fake screen to update tables
;
	CPIM	UPDLIN,-1	;Is UPDLIN flag set?
	BEQ	..2		;No, branch
	DCR	M		;Yes, adjust for scroll
	JRNZ	..2
	INR	M		;But not below 1
;
..2:	CMPMB	WWNLIN,RRWLIN	;Is RRWLIN count set?
	BLT	..3		;No, branch
	DCR	M		;Adjust for scroll
	JRNZ	..3
	INR	M		;But not below 1
;
;	If a partial screen needs to be written do so in background
;	Typing a [RETURN] requires a partial screen write
;
..3:	ANIB$	NWSCFL,030H	;Also want partial new screen or insert line?
	JRZ	..4		;No, just write new bottom line
	LDA	FSACSC		;Yes, get begin line #
	DCR	A		;Account for the scroll
	JMPR	WRTSET		;Setup to write partial window
;
..4:	LDA	WWNLIN		;;Set to write on bottom line of window
	LXI	H,LINCNT	;;# lines containing text
	CMP	M		;;Has EOF been reached on screen?
	BEQ	WTSLB2		;;No, write a new bottom line
	RET			;Yes, nothing to update on screen
	EPROC	WTSCRL
;
;
; WTSLBK - Scroll backward and write new top line.
;
	BPROC	WTSLBK
WTSLBK:	ANIB$	NWSCFL,10H	;Set for a Delete line too?
	JRNZ	WTFULL		;Yes, give up, rewrite entire window
;
	CALL	WISLBK		;Scroll window back
	IF	CRTVRS, [
	JC	WTFULL		;Partial screens cannot be scrolled back
				;Also in case Reverse scroll not available
	]
	LXI	H,WWUSLN	;HL-> used line count
	INR	M		;One more line used in window
	LDA	WWNLIN		;A = max lines in window
	CMP	M		;Gone too far?
	BGE	..1		;No, branch
	MOV	M,A		;Yes, set to max
;
..1:	CALL	FAKSC0		;Write fake screen to update tables
	MVI	A,1		;Value to write the top line
;
WTSLB2:	CALL	SETWRT		;Setup DE-> text for WRTLIN, set LOGPOS
	JMP	WRTLIN		;Write top/bottom line on the CRT
	EPROC	WTSLBK
	.PAGE
;
; FAKSCR - Write a fake screen, either whole or partial.	[11/7/86]
;	   Will set the FCB, continuation and tab split tables.
;	   A fake screen is written for scrolls and line insert or delete.
;
;	   Enter: A = line number to begin fake screen write on.
;	   Return: A saved.
;
FAKSC0:	MVI	A,1		;Start fake screen from line one
	BPROC	FAKSCR
FAKSCR:	MVIM	EOFSCR,00	;Clear EOF on screen flag (don't change AL)
	CALL	SETWRT		;Setup DE-> text for WRTLIN, set LOGPOS
	DCR	A		;Adjust for INR below
	JRNZ	..5		;;Branch if not top line
;
;	Writing top line - 
;
	PUSHA
	PUSH	D
	DCX$	D		;;DE-> previous char
	LDAX	D		;;Get previous char
	CPI	LF		;;LF?
	LXI	H,1		;;Assume YES, reset LOGPOS = 1
	BEQ	..1		;;Yes, reset LOGPOS = 1
	CPIW	LOGPOS,2	;;No, is LOGPOS value valid?
	BGE	..3		;;Yes, branch
	POP	H		;;No, HL-> top screen character
	PUSH	H		;;Save again
	CALL	BACKLN		;;Write previous line
	LHLD	LOGPOS		;;HL = LOGPOS for top window line
;	
..1:	SHLD	LOGPOS		;;Save for upcoming WRTLIN
	CALL	STLGTP		;;Save in POSTBL[0]
..3:	POP	D
	POPA
;
..5:	INR	A		;Set line # to next line
	STA	LINCNT		;Save as last line written
	MVIM	FAKFLG,80H	;Set for fake line write with table update
	CALL	WRTLIN		;Write another line; LOGPOS set for next line
				;A = window line #
	CALL	ENTFCB		;Save DE (text pointer) in screen-FCB
	JRC	..9		;Return if EOF found
	CMPM	WWNLIN		;Written a full window?
	BLT	..5		;No, continue
..9:	RET
	EPROC	FAKSCR
	.PAGE
;
; STWTPS - Resets LOGPOS = 1 and sets WTACFL from value in A.
;	   Reg A and status is preserved.
;
				;{WTACLN,BACKLN}
STWTPS:	STA	WTACFL		;Save value for WTACFL
	JMP	RSTPOS		;;Reset LOGPOS=1
;
; SETWRT - Sets up to write a line of text.
;	   Enter: A = window line # to write
;	   Retrn: LOGPOS set for WRTLIN; DE-> line of text
;	
				;{UPDSCR,PAGEDW,WTSCRL,FAKSCR}
SETWRT:	PUSH	PSW		;+Save line number
	CALL	LINPOS		;;HL = LINPOS for desired line
	SHLD	LOGPOS		;;Save it
	POP	PSW
	PUSH	PSW
	CALL	FCBADD		;HL-> text buffer for line
	XCHG			;DE = active point in text
	POP	PSW		;+Restore line number
	RET
;
; SCRBG  - Store HL at SCRFCB[0] to indicate begin of screen	[2/20/86]
; ENTFCB - Store DE  (-> first text char of next line)
;	   in FCB table indexed by reg. A.
;
SCRBG:	XCHG			;DE-> text for begin of screen
	XRA	A		;Top is line one (adjust for INR A below)
;
ENTFCB:	PUSH	PSW		;+Save AF
	INR	A		;Adjust for next line
	CALL	IDXFCB		;ES:HL-> entry in FCB
	MOV%ES	M,E		;Move DE to FCB
	INX$	H
	MOV%ES	M,D
	POP	PSW		;+Restore AF
	RET
;
; FCBADD - Use reg. A as the index into the FCB.
;	   Return with HL = address from FCB.
;
				;{TOVIS,NEWACT,ADDACT,CRDOWN,PAGEDW,SETWRT}
FCBADD:	LES	H,SCRFCB	;ES:HL-> table of address
	JMPES	TBLADD		;HL = entry from table -> text for screen line
;
; IDXFCB - Use Reg. A as the index into the SCRFCB.
;	   Return with ES:HL-> correct item in SCRFCB.
;
				;{UPSCTB,ENTFCB}
IDXFCB:	LES	H,SCRFCB	;ES:HL-> table of address
	JMP	IDXTBL		;ES:HL-> entry in table
;
; LINPOS - Return in HL the LOGPOS for begin of line.
;	   Enter: A = desired line #, top line == 1.
;
				;{BACKTB}
CURPOS:	LDA	CURVER		;Get the cursor line #
				;{WRTLIN,CRDOWN,WRTLIN}
LINPOS:	LES	H,POSTBL	;;ES:HL-> table of LOGPOS
	JMPES	TBLADD		;HL = entry from table = LOGPOS for line
;
IDXPOS:	LES	H,POSTBL	;;ES:HL-> table of LOGPOS
	JMP	IDXTBL		;;ES:HL-> entry in table
;
; STLGTP - Store HL at POSTBL[0] to set LOGPOS for top screen line.
;
				;{TOVIS}
CLLGTP:	LXI	H,1		;;Init value
				;{BACKLN}
STLGTP:	PUSH	D		;;Save caller's DE
	XCHG
	MVI	A,1		;;Index for line 1
	CALL	IDXPOS		;;HL-> POSTBL[0]
	CALLES	STDEIN		;;Store new value
	POP	D
	RET
;
; CNTSCR - Return in ES:HL -> continuation flag indexed by A.
;
				;{CRDOWN,SCRLDW,CRNXLN}
CNTVER:	LDA	CURVER		;Get cursor line #
				;{CRZIP,WRTLIN}
CNTSCR:	DCR	A		;Create table index
				;{WRTLIN}
CNTSC1:	LES	H,SCRCNT	;ES:HL-> cont. flag table
	JMP	ADDAHL		;HL-> flag for selected line
	.PAGE
;
; WRTLIN - Writes one window line on the screen.  Depending upon
;	   the WINHOR and LOGPOS entry values, only a partial line may
;	   be written.   Returns when LF, EOF or end of window line
;	   encountered.
;
;	   Enter:   DE-> first char. to display.
;		     A = window line # to write on.
;			 If negative or zero, write Fake line
;			 without updating tables.
;		FAKFLG = 80H : write fake line and update tables.
;			 01H : CURLIN is <= 00.
;		LOGPOS = logical position for next char.
;			 Used for Tab positions and setting LOGHOR.
;			 Sets initial WINHOR.
;
;	   Exit:    DE-> past last character written.
;		     A = CURLIN = window line just written.
;		FAKFLG = 00 (Reset).
;		WINVER = physical line #. (when not a fake write)
;		WINHOR = last write position (when not a fake write)
;		LOGPOS = correct value for next line.
;		LOGHOR = (if active line) logical horizontal cursor position.
;		SCRCNT[] (continuation) and POSTBL[] updated when CURLIN is valid.
;
;	        Flags = 'Z' if LF or EOF, 'C' if EOF.
;
	DSEG	$
CURLIN:	DS	1		;Current line # being written with WRTLIN
				;== -1 or 0 if Fake line
	CSEG	$
;
; WRTLNA - Entry when writing active line
;
				;{WTACLN}
WRTLNA:	CALL	WRTENT		;Set CURLIN and FAKFLG
	ORA	A		;Is FAKFLG set?
	JNZ	WRTLI5		;Yes, branch
;
;	Only write continuation char if writing entire line.
;
	TST$	WTACFL		;Update on entire active line?
	JRZ	WRTLI2		;Yes, branch
;
;	Set cursor position, WINHOR already set from LOGPOS
;
	LD16	H,WINVER	;;H = correct WINHOR
	LDA	CURLIN		;A = line # to write
	MOV	L,A		;Desired line #
	CALL	WINSET		;Address the cursor for writing
	JMPR8	WRTLI5		;Branch to write line
;
; WRTENT - Setup for WRTLIN/WRTLNA.  Set CURLIN = A.
;	   Set FAKFLG if CURLIN <= 0.  
;	   Note: FAKFLG may be set on entry with valid CURLIN.  Pretty tricky.
;
;	   Retrn: A = value of FAKFLG.
;		  'C' if CURLIN <= 0 (FAKFLG is then also set)
;
	BPROC	WRTENT
WRTENT:	DCX$	D		;DE-> last char, adjust for INX D at WRTLI5
	STA	CURLIN		;Save line # to write
;
;	Set WINHOR (in case of fake write)
;
	PUSHA
	CALL	DIVPOS		;;A = WINHOR corresponding to LOGPOS
	STA	WINHOR
	POPA
;
	ORA	A		;Ensure 'NC'
	DCR	A		;Writing a line not on the screen?
	MOVB	LSTFLG,LVLVTX	;Setup correct LSTFLG for visual mode text
	LDAM	FAKFLG		;Get fake write flag, HL-> flag
	RP			;No, return with 'NC', A = FAKFLG
	ORI	1		;Yes, set fake line flag
	MOV	M,A		;Save new flag
	STC			;Set 'C' bit
	RET			;Return with 'C'
	EPROC	WRTENT
;
; WRTLIN -
;
				;{UPDSCR,BACKLN,PAGEDW,WTSCRL,WTSCBK,FAKSCR}
	BPROC	WRTLIN
WRTLIN:	CALL	WRTENT		;Set CURLIN and FAKFLG
	JRC	WRTLI5		;Branch if CURLIN not on screen
	LDA	CURLIN		;Get current line #
	CALL	CNTSCR		;ES:HL-> cont. flag for this line
	LDAX	D		;Get last text char
	SUI	LF		;Was last char. a LF?
	BEQ	..1		;Yes, branch
	MVI	A,1		;No, get flag value for cont
..1:	MOV%ES	M,A		;Save value in flag table
	TST$	FAKFLG		;Is FAKFLG set?
	JRNZ	WRTLI5		;Yes, skip continuation char, initial spaces
;
;	Setup for writing an entire window line.
;
;	Write continuation column character
;
WRTLI2:	LD16	H,CURLIN	;L = desired line
	MVI	H,00		;Start in virtual column 00
	CALL	WINSET		;Address to column 0 of desired line
				;(Note: CALL WINST0 is not the same thing)
;
	LDAX	D		;Get last text char
	CPI	LF		;Was last char. a LF?
	LDA	FILLCH		;Assume yes, no continuation
	BEQ	..3		;Yes, branch
	LDA	CONTCH		;No, get continuation char
..3:	MOV	C,A		;Put char in C
	ORA	A		;Want reverse video?
	JRP	..4		;No, branch
	ANI	7FH		;Yes, strip high bit
	MOV	C,A		;Back into C
	MOVB	ATTRIB,SSTAAT	;Use attribute for status line
..4:	CALL	WINOUT		;Display if within window (don't change LOGPOS)
	CALL	ATTFOR		;Reset to normal screen attribute
;
;	Pad line for tab split on continuation line
;
	LHLD	LOGPOS
	PUSH	H
	CALL	DVHOR1		;;A = number of spaces needed
	DCR	A		;Adjust for being on column 1
	CALL	EXPTAB		;Expand the tab, sets WINHOR
	POP	H
	SHLD	LOGPOS
	JMPR	WRTLI5		;Jump into main loop
;
;	The main loop checks if logical horizontal cursor position has been
;	reached, handles special characters, else writes displayable 
;	characters on screen.
;
WRTLI4:	CALL	PRTCHA		;Display character in A on screen
WRTLI5:	INX$	D		;Point to current character
	TST$	WTACFL		;Writing active line?
	JRZ	..6		;No, branch
;
	LHLD	ACTPNT		;Get visual edit PTR
	CALL	CMHLDE		;Writing that char?
	JNZ	..6		;No, branch
	MOVW	LOGHOR,LOGPOS	;Save logical pos. as logical cursor pos
;
..6:	LDA	HZSCLN		;Get end of virtual screen
	LXI	H,WINHOR	;HL-> line pos. for next char
	CMP	M		;Are we past end of virtual screen?
				;Note: HL must -> WINHOR
	BLT	..LEND		;Yes, branch, DE-> last char
	LDAX	D		;Get current char
	CPI	SPACE		;Is it displayable?
	BGE	WRTLI4		;Yes, display it
;
;	Handle non display characters.
;
	CPI	CR		;Is it a CR?
	BEQ	..CR		;Yes, process it
	CPI	LF		;Is it a LF?
	BEQ	..LF		;Process and return
	CPI	EOF		;Is it an EOF?
	BNE	WRTLI4		;No, display whatever it is
;
..EOF:	CALL	EOFCHK		;Check if true EOF, return here if true
				;Also returns here if cannot auto-buffer
;
;	Handle EOF - set mask 80 hex in continuation flag table
;
	LDA	CURLIN		;Get current line #
	DCR	A		;Writing line not on screen?
	JM	..7		;Yes, don't set EOF bit
	CALL	CNTSC1		;ES:HL-> cont. flag for line
	MOV%ES	A,M		;Get the flag
	ORI	80H		;Set EOF found bit
	MOV%ES	M,A		;Save new flag
	STA	EOFSCR		;Set flag that EOF reached on screen
;
;	Put message [EOF] on screen someday - or status line
;
..7:	XRA	A		;Set Z bit
	STC			;Set C bit
	JMPR	..OUT		;EOF return
;
..CR:	INX$	D		;DE-> next char
	LDAX	D		;Get the char
	DCX$	D		;Restore DE
	CPI	LF		;Is this a CR-LF?
	BEQ	WRTLI5		;;Yes, continue writing line
	MVI	A,CR		;;No, get the CR again
	JMPR	WRTLI4		;Display "<CR>"
;
..LF:	DCX$	D		;;DE-> previous char
	LDAX	D		;;AL = previous char
	INX$	D		;;Restore DE
	CPI	CR		;;Is it CR?
	BEQ	..LF1		;;Yes, branch
	MVI	A,LF		;;No, get LF back
	CALL	PRTCHA		;;Display "<LF>"
	JMPR	..LF2		;;Merge below
;
;	Check if EOL character to be written
;
..LF1:	TST$	FAKFLG		;;Is this a fake write?
	JRNZ	..LF2		;;Yes, don't write EOLCHR
	LDA	EOLCHR		;Get char for end of line
	CPI	SPACE		;Is it invisible space?
	BEQ	..LF2		;Yes, branch
	MOV	C,A		;No, put char in C
	CALL	WINOUT		;Display the char
;
..LF2:	CALL	RSTPOS		;;Reset LOGPOS=1

	IF	CRTVRS, [
	CALL	CHKKYB		;Check for keyboard char
	]

	XRA	A		;Set 'Z' and 'NC'
	JMPR	..OUT		;Merge below	
;
..LEND:	DCX$	D		;DE-> last char written
;
;	Set remaining bits for continuation flag.
;
	LDA	CURLIN		;Get current line #
	MOV	B,A		;Save it in B
	DCR	A		;Writing line not on screen?
	JM	..END2		;Yes, don't set flags
	CALL	CNTSC1		;ES:HL-> cont flag for this line
	LDA	WWNLIN		;Get last line # to be written
	CMP	B		;Written last line?
	MOV%ES	B,M		;Get current flag for line
	MVI	A,2		;Assume not last line
	BNE	..END1		;No, branch
	MVI	A,6		;Yes, cont. line off screen
..END1:	ORA	B		;OR in previous line flag
	MOV	M,A		;Save final line flag
..END2:	ORI	1		;Set to return 'NZ' and 'NC'
;
;	Set POSTBL and Clear the rest of the line.
;	*** NOTE *** 'Z' and 'C' must be correctly set for return.
;
..OUT:	PUSHF			;Save return flags
	PUSH	D		;;Save DE
	TST	CURLIN		;Is current line real or line zero?
	JM	..OUT3		;No, don't set LINTBL or flags
	INR	A		;;Save for next line
	CALL	IDXPOS		;;ES:HL-> POSTBL[A]
	LDED	LOGPOS		;;DE = LOGPOS
	CALLES	STDEIN		;;Save LOGPOS in table
;
;	Don't send an EOL if this is a fake line, or if this is the active
;	line and EOLFLG is not zero (set).  However if a Tab was the last
;	character, a EOL has to be sent.
;
	TST$	FAKFLG		;Is this any type of fake line?
	JRNZ	..OUT3		;Yes, branch around EOL stuff
	TST$	WTACFL		;Writing active line?
	JRZ	..EOL		;No, send EOL to CRT
;
	LDAX	D		;Get last char on screen line
	CPI	TAB		;Is it a Tab?
	BEQ	..EOL		;Yes, must send EOL regardless of EOLFLG
	TST$	EOLFLG		;Should EOL be sent?
	JRNZ	..OUT3		;No, don't send EOL to CRT
;
;	Perform an EOL from last screen position written.
;
..EOL:	LDA	WINHOR		;Get last write position (+1)
	CMPM	HZSCEN		;Is WINHOR off right side?
	BGE	..OUT3		;Yes, don't EOL
;
	CALL	WINEOL		;Clear to the end of line
;
..OUT3:	CLR$	FAKFLG		;Clear the fake line flag
;
	IF	POLLING, [
	CALL	CHKKEY		;Check for keyboard char
	]
	POP	D		;;Restore DE-> char
	INX$	D		;DE-> next char to write
	POPF			;Restore return flags
	LDA	CURLIN		;Return with A = line just written
	RET
	EPROC	WRTLIN
	.PAGE
;
;	S T A T U S   L I N E   R O U T I N E S
;
;	Note:	ROUTAD must first be set to STAOUT() by ADDSTA().
;		When done, must go through STADON to restore ROUTAD, etc.
;
;	Usage of STLNFL:
;
;		00  -	Status line contains scrolled data (garbage)
;		01  -   Contains prompting message
;		02  -   Command mode status line
;		03  -   Visual mode status line
;	
;
; STBRFL - Set the flag STLNFL to write new status line
;	   when CHKBRD() is next called.
;
				;{VMAIN,CHKSPC,HZRTLF}
STWTBR:
STBRFL:	CLR$	STLNFL		;Set flag to write new status line
	RET
;
; CHKBRD - Check if status line or line and column # need to be written.
;	   If STLNFL != (VISFLG+2) entire status line is re-written.
;
	IF	IBMPC, [
	DSEG	$
	PUBLIC	KYSTSV
CLMSG	DB	'C', 00
NLMSG	DB	'N', 00
SLMSG	DB	'S', 00
KYSTSV	DB	00		;;Displayed value of keyboard status
LOW418	=	417H		;Offset to keyboard status byte
	CSEG	$
	]
				;{VLOOP,GETCHR}
	BPROC	CHKBRD
CHKBRD:	CPIW	ROUTAD,STAOUT	;Is output going to status line now?
	RZ			;Yes, don't dare mess with it
;
	LDA	VISFLG		;Get command/visual mode flag
	INR	A
	INR	A		;Adjust to STLNFL values
	LXI	H,STLNFL	;HL-> flag
	CMP	M		;Is status line up to date?
	JZ	BRDNUM		;Yes, check if line and col # ok
	MOV	M,A		;Update flag
				;No, write entire new status line
	IF	P8086, [
;6#1:	PUSH	DS
;6	CALL	RSTSEG
;6	CALL	WRTSTA
;6	POP	AX
;6	JMPL	SWAPSG
	EPROC	CHKBRD
	]
;
; WRTSTA - Write the status line with all messages.
;
;0---------1---------2---------3---------4---------5---------6---
;FULL-TEXT---LINE:12345--COL:123---FILE:ABCDEFGH.XYZ--INSERT-----
;FULL-TEXT---LINE:12345--COL:123---FILE:ABCDEFGH.XYZ--INSERT--R#- (VPLUS)
;
;0---------1---------2---------3---------4---------5---------6---------7---------
;FULL-TEXT---LINE:12345--COL:123---FILE: d:ABCDEFGH.XYZ---------CNS---INSERT--R#- (VPLUS)
;
;
;
	BPROC	WRTSTA
WRTSTA:	XRA	A		;Start in column zero
	CALL	STASTN		;Address to status line, normal attribute
	TST$	VISFLG		;Is this visual mode?
	JRNZ	..1		;Yes, branch
;
;	Messages for Command Mode
;
	LXI	H,CMMMSG	;No, HL-> "COMMAND"
	MVI	A,1		;Message in column 1
	CALL	WRTMSG		;Write message on status line
	CALL	STAFIL		;Put up filename
;
	IF	IBMPC, [
;6	CALL	KYLOCK		;;Display Caps/Num/Scroll Lock
	]
;
	IF	VPLUS, [
	CALL	STABUF		;Put up buffer name
	]
..FILL:	LD8	PYLLEN		;Last column #
	CALL	FILLST		;Fill out status line
	JMP	BRDNM3		;Close status line
;
;	Messages for Visual Mode
;
..1:	TST$	DSKFUL		;;Disk full?
	LXI	H,DISKMS	;;Assume YES, HL-> DISK msg
	JRNZ	..1A		;;Yes, display message
				;;No, check for "FULL"
;
	TST$	FULLFG		;Is the text buffer full?
	JRZ	..2		;No, branch
	LXI	H,FULMSG	;HL-> FULL message
..1A:	XRA	A		;Message in column 00
	CALL	WRTMSG		;Write message on status line
	JMPR	..3		;Branch around scroll value
;
..2:	TST$	VSHZBG		;Horizontal scrolling?
	JRZ	..3		;No, branch
	MOV	C,A		;Put value into BC
	XRA	A		;Get zero
	MOV	B,A
	INR	A		;Put at column 1
	CALL	FILLST		;Position cursor on status line (BC saved)
	LXI	H,100		;Highest digit is 100s
	CALL	PRTDC1		;Display the line #
;
..3:	TSTW$	BLMVEN		;Is the text-block pointer set?
	LXI	H,ENDMSG	;HL-> 1 END message
	JRNZ	..5		;Yes, display message
;
;	Any text in registers?
;
	IF	VPLUS, [
	CALL	REGLEN		;Any text registers?
	LXI	H,TXTMSG	;Set HL-> TEXT message
	JRNZ	..5		;Yes, display TEXT message
;
;	Additional edit buffers?
;
	CALL	MORBUF
	LXI	H,MORMSG	;HL -> MORE (edit buffers) message
	JRC	..6		;No, check next switch
	]
;
	IFNOT	VPLUS, [
	CALL	REGLEN		;Is text register active?
	JRZ	..6		;No, check next switch
	LXI	H,TXTMSG	;Yes, HL-> TEXT message
	]
;
..5:	MVI	A,5		;Get switch pos
	CALL	WRTMSG		;Write message on status line
;
..6:	CALL	STALIN		;Put up "LINE:" and "COL:"
	CALL	STAFIL		;Put up filename
;
	IF	IBMPC, [
	CALL	KYLOCK		;;Display Caps/Num/Scroll Lock
	]
;
	TST$	INSFLG		;Is the insert mode on?
	JRZ	..9		;No, branch
	LD8	PYLLEN		;Get line length
	CPI	63		;Is line length at least 63?
	BGE	..8		;Yes, branch
	INR	A		;No, move INSERT over by 2
	INR	A
..8:	SBI	11		;Compute switch position
	LXI	H,INSMSG	;HL-> INSERT message
	CALL	WRTMSG		;Write message on status line
..9:
	IF	VPLUS, [
	CALL	STABUF
	]
;
	CALL	..FILL		;Fill out rest of status line
	EPROC	WRTSTA
;
; BRDNUM - Write optional line and column numbers on status line.
;	   Restores ROUTAD, etc when done
;
	BPROC	BRDNUM
BRDNUM:	TST$	VISFLG		;Is this visual mode?
	JRZ	BRDNM3		;No, branch if command mode
	LBCD$	LINFIL		;BC= line # in file
	LHLD	DISPLN		;Get last displayed #
	DSUB	B		;Is new # the same?
	BEQ	..1		;Yes, branch
	SBCD$	DISPLN		;No, save current #
	MVI	A,17		;Pos. of line #
	CALL	STASTN		;Position cursor on status line
	CALL	PRTDEC		;Display the line #
;
..1:
	LBCD$	LOGHOR		;BC = logical cursor pos
	LHLD	DISPCL		;Get last displayed #
	DSUB	B		;Is new # the same?
	BEQ	BRDNM3		;Yes, branch
	SBCD$	DISPCL		;No, save current #
	MVI	A,28		;Pos. of line #
	LXI	H,100		;Highest digit is 100s
				;{REPEAT}
BRDNM2:	PUSH	H		;;
	CALL	STASTN		;Position cursor on status line
	POP	H		;;
	CALL	PRTDC1		;Display the line #
BRDNM3:	CALL	STADON		;Restore ROUTAD, LSTFLG and LOGPOS
;
;	Check if reverse video window names need updating
;
	LDA	HILIWW		;Get # of highlighted window
	LXI	H,WWNAME	;Is current window highlighted?
	CMP	M
	RZ			;Yes, nothing to do
	TST$	WWZMFL		;Window zoomed?
	RNZ
	CALL	MAXWIN		;'Z' if there is only one window
	RZ			;Prevent wierd problems
;
	LDA	HILIWW		;Get # of currently highlighted window
	CALL	WSTCAL		;ES:HL-> window structure
				;'C' if window was deleted (or none highlighted)
	CNC	WIBTOP		;Turn reverse video WINDOW off
;
	MVIB$	WIHLFL,1	;Set flag that we want to highligh
	LXI	H,WWNAME	;HL-> current structure
	MOV	A,M
	STA	HILIWW		;Save highlighted window name
;6	PUSH	DS
;6	POP	ES		;Set ES:HL-> structure
	CALL	WIBTOP		;Turn reverse video WINDOW on (Reset WIHLFL)
	JMP	WINRST		;Reset cursor position
	EPROC	BRDNUM
;
; STALIN -
;
				;{WRTSTA}
	BPROC	STALIN
STALIN:	MVIW	DISPLN,00	;Force line # rewrite
	SHLD	DISPCL		;Force col. # rewrite
	LXI	H,LINMSG	;HL-> "LINE:"
	MVI	A,12		;Get mess. position
	CALL	WRTMS0		;Write header, in normal attribute
..1:
	LXI	H,COLMSG	;HL-> "COL:"
	MVI	A,24		;Get mess. position
	JMP	WRTMS0		;Write header
	EPROC	STALIN
;
				;{WRTSTA}
	BPROC	STAFIL
STAFIL:	TST$	OUTFLG		;Is an output file open?
	RZ			;No, branch
;	CPIB$	PYLLEN,63	;Is line length at least 63?
;	RC			;No, can't display file name
	TST$	BOFFLG		;Is begin of file in memory?
	MVI	C,'F'		;Assume Yes
	JRZ	..1		;Yes, branch
	MVI	C,'f'		;No, get LC "f"
..1:	CALL	INFLCH		;Is end of file in memory?
	LXI	H,FILMS1	;Assume Yes, HL-> "FILE:"
	JRZ	..2		;Yes, branch
	LXI	H,FILMS2	;No, HL-> "File:"
..2:	MOV	M,C		;Save "F" or "f" in message
	MVI	A,34		;Get mess. position
	CALL	WRTMS0		;Write header
	LXI	H,RENFCB	;HL-> Output file drive/name
	LDA	ORGDRV		;Get original drive
	CMP	M		;Is this the default drive?
	MOV	A,M		;A = drive
	BEQ	..3		;Yes, don't display drive
	ADI	'A'-1		;Convert to ASCII
	CALL	PCHARA		;Display drive
	MVI	A,':'
	CALL	PCHARA		;Display :
..3:	JMP	PRTFCB		;Display filename on status line
	EPROC	STAFIL
;
; KYLOCK - Display "CNS" for Caps/Num/Scroll Lock
;
	IF	IBMPC, [
;6	public	kylock
;6KYLOCK	PROC
;6	PUSH	ES
;6	MOV	AX,00		;;Low memory segment
;6	MOV	ES,AX		;;ES-> low memory
;6	MOV	AL,EBP LOW418	;;Get keyboard status
;6	AND	AL,70H		;;Only look at LOCK bits
;6	MOV	KYSTSV,AL	;;Save it
;6	POP	ES
;
;6	TEST	KYSTSV,040H	;;Is Caps Lock on?
;6	JZ	#1
;6	MOV	AL,63
;6	MOV	BX,OFS CLMSG	;;BX-> single letter message
;6	CALL	WRTMSG		;;Write message with fill
;6#1:
;6	TEST	KYSTSV,020H	;;Is Num Lock on?
;6	JZ	#2
;6	MOV	AL,64
;6	MOV	BX,OFS NLMSG	;;BX-> single letter message
;6	CALL	WRTMSG		;;Write message with fill
;6#2:
;6	TEST	KYSTSV,010H	;;Is Scroll Lock on?
;6	JZ	#3
;6	MOV	AL,65
;6	MOV	BX,OFS SLMSG	;;BX-> single letter message
;6	CALL	WRTMSG		;;Write message with fill
;6#3:	RET
;6KYLOCK	ENDP
	]
;
; STABUF - Display Buffer Number unless MAINRG
;
	IF	VPLUS, [
STABUF:
	IFNOT	P8086, [
	LDA	EDTNUM		;Get current edit buffer number
	] [
;6	MOV	ES,EDTSEG
;6	MOV	AL,EBP EDTNUM	;Get current edif buffer number
	]

	CPI	MAINRG		;Main text buffer?
	RZ			;Yes, don't display
	CALL	RTOASC		;Convert to ASCII
	LXI	H,RGNMSG+1
	MOV	M,A		;Store into E# message
	DCX$	H		;HL-> to start of E# message
	LD8	PYLLEN
	SUI	3
	JMP	WRTMS0		;Display on status line
	]
	.PAGE
;
; STAKEY - Put up status line prompt and wait for user reply
;	   Enter: HL-> message to prompt
;	   Retrn: C = single key reply; A = key converted to UC
;
				;{VSMENU,VWICRE,VWINUM,VFLNEW,VDELET}
				;{PAUSE,LRNDEL,DEFINE}
	BPROC	STAKEY
STAKEY:	PUSH	H
	PUSH	D
	CALL	STAPRM		;Give prompt on status line
	CALL	GETKEY		;A = C = reply, disable function decoding
	JRC	..CANC		;;Abort if [CANCEL] pressed
	CPI	CTRLC		;CTRL-C user abort?
	BNE	..RET		;;No, branch
;
..CANC:	TST$	VISFLG		;;Is this visual mode?
	JNZ	CANCL1		;;Yes, cancel the prompt
				;;STADON() is called, stack reset
..RET:	CALL	STADON		;Close status line, restore cursor pos
	POP	D
	POP	H
	MOV	A,C		;A = reply char
	JMP	CONVUC		;;Convert %A to UC, C = original char
	EPROC	STAKEY
;
; STAPRM - Write status line prompt starting at column 0
;	   Enter: HL-> message ending in [00]
;
;	   Note: Caller must call STADON() to close status line when done
;
				;{GETRNM,VRPEXE,FNDEXE,VGTSTR}
STAPRM:	XRA	A		;Start message at column 0
	CALL	STAMSG		;Put up prompt message
	CALL	WINEOL		;;Erase rest of status line
	JMP	ATTBCK		;User reply in screen background attribute
;
; STAMSG - Write a message on status line.
;	   Enter: A = column at which to start message, HL-> message.
;
				;{VISMSS,STAPRM}
STAMSG:	CALL	STASET		;Setup status line
	JMPR	WRTMS1		;Branch
;
; WRTMSG - Write a message on the status line.  '00' terminates message.
;	   Enter with HL-> message, A = starting column.
; WRTMS0 : Normal status line attribute.
;
; Assume:  Status line already opened with CALL STASTN (normal attribute)
;
				;{WRTSTA}
WRTMSG:	CALL	FILLST		;Fill to desired column (with normal attribute)
	CALL	ATTSTM		;Set status-line message attribute
	JMPR	WRTMS1		;Merge below
;
WRTMS0:	CALL	FILLST		;Fill to desired column
				;{STAMSG}
WRTMS1:	CALL	PRTSTR		;Display message on status line
	JMP	ATTFOR		;Back to normal status line attribute
;
; VISMSS - Put up "WAIT FOR DISK" message.
;	   Set NWSCFL to rewrite status line.
;
				;{WRTTXT,READTX-8086,READSC,WRTBND,RDPREV}
VISMSS:	CALL	SV%ALL		;Save all regs
	CALL	STNSNL		;Make sure visual window rewritten
	LDA	STLNFL		;;AL = status line flag
	CPI	4		;Already have message?
	RZ			;Yes, do nothing more
	CPI	2		;Is status line flag already set?
	CC	STACLR		;Yes, clear old message first
				;Needed for WAIT FOR DISK during [FIND]
	LXI	H,DSKMSG	;HL-> Message
	MVI	A,12		;Position on status line
	CALL	STAMSG		;Write the message
	MVIB$	STLNFL,4	;Set flag that "WAIT" message is on it
;	JMPR	STADON		;Restore ROUTAD, etc
;
				;{CANCEL and numerous other}
STADON:	CALL	SV%BDH		;Save regs
	CPIB$	WWNAME,'?'	;Is output going to status line now?
	RNZ			;No, status line aleady closed
;	
	CALL	SWBACK		;Switch back to original window
	CALL	RSTOUT		;Restore ROUTAD, LOGPOS and LSTFLG
	RET			;Nice place for breakpoint
;
; STASET - Setup (open) status line.  Set STLNFL = 1.
;	   Enter: A = column at which to start message.  HL saved.
; STASTM - Set message attribute
; STASTN - Set normal status-line attribute
;
				;{YTCMD,STAMSG}
	PUBLIC	STASET
	BPROC	STASET
STASET:	PUSHA			;-8086
	MVIB$	STLNFL,1	;Set flag that new status line needed
	POPA			;-8086
				;{STASET above}
STASTM:	CALL	ADDSTA		;Open status line, set start column
	JMP	ATTSTM		;Set status line message attribute
;
STASTN:	CALL	ADDSTA		;Open status line, set start column
	JMP	ATTFOR		;Set status line normal attribute
	EPROC	STASET
;
; STACLR - Clear status line.
;
				;{VISMSS,WININI}
STACLR:	XRA	A		;;Start clear in column 0
	CALL	ADDSTA		;;Address status line
	CALL	WINEOL		;;Erase status line
	CALL	STBRFL		;Set status line flag
	JMPR	STADON		;;Close status line
	.PAGE
;
; FILLST - Fill status line with status-line-character up to desired column.
;	   Enter: A = desired column.
;	   Retrn: WINHOR set properly, BC and HL saved.
;
				;{WRTSTA,WRTMSG}
	BPROC	FILLST
FILLST:	PUSH	H		;Save HL
	PUSH	B		;Save BC
	LXI	H,PHYHOR	;HL-> pos on status line
	CMP	M		;To the left of current pos?
	BLT	ADDST0		;Yes, just address
	JRZ	..2		;Return if already there
;
	SUB	M		;Compute # columns to fill
	MOV	B,A		;Put (non-zero) count into B
	LDA	BRDRCH		;Get status line char
	MOV	C,A		;Put in C
..1:	CALL	OUTCHR		;Fill with status line character
	DJNZ	..1
..2:	POP	B		;Restore BC and HL
	POP	H
	RET
	EPROC	FILLST
;
; ADDSTA - Address status line, column in reg. A.
;	   Also sets ROUTAD to STAOUT.
;
				;{BRDNUM,WRTBRD,STASTR,WRTMSG}
	BPROC	ADDSTA
ADDSTA:	PUSH	H		;Save HL
	PUSH	B		;Save BC
ADDST0:	PUSHA			;Save AL
;
	CPIB$	WWNAME,'?'	;Already set to status line?
	BEQ	..1		;Yes, branch
	LDA	LVLVCM		;LSTFLG value for line editing, show ESC as '$'
	LXI	H,STAOUT	;HL-> new output routine
	CALL	SETOUT		;No, set new ROUTAD, stack old one
;
	CALL	SWSTAT		;Switch to status line window
..1:	POPA			;A = desired column
	STA	WINHOR		;Save it
	CALL	WINRST		;Position to desired column
	POP	B		;Restore BC
	POP	H		;Restore HL
	RET
	EPROC	ADDSTA
	.PAGE
;
; EOFCHK - Read in more text if true EOF not yet reached.
;;
;;	   Just returns if window update, auto-buffering disabled,
;;	   or unsuccessful auto-buffer last time through.
;;	   WRITOK is reset at ECMD.
;;
;;	   If more text read, ignores return and jumps to rewrite screen.
;;
;	   Note: must save DE and FAKFLG.
;
				;{WRTLIN}
EOFCHK:	TST$	WWUPFL		;Is this a window update?
	RNZ			;Yes, don't auto-buffer
	TST$	ATBFSW		;Is auto-buffering enabled?
	RZ			;No, return
	TSTM	WRITOK		;;Infinite loop?
	RZ			;;Yes, quit
	MVI	M,00		;;Reset flag
;;
	LXI	H,FAKFLG	;HL-> fake line flag
	MOV	C,M		;Save it
	MVI	M,00		;Clear it so "PLEASE WAIT FOR DISK" works
	PUSH	B		;Save FAKFLG
	PUSH	D		;DE is critical to WRTLIN()

	CALL	NEXFIL		;Try to read in more text
	MVIB$	WRITOK,1	;;Indicate successful operation

	POP	D		;DE-> text
	POP	B
	MOV	A,C		;A = original FAKFLG
	STA	FAKFLG		;Restore original value
	RNZ			;Return if true EOF reached
	JMP	VMAIN3		;Rewrite screen again
;
; CHKRVB - Check for/perform auto reverse buffering.
;	   HL = # bytes to auto buffer.
;	   DE = address of routine to push onto OPSTK.
;
				;{CRUP,PAGEUP}
	BPROC	CHKRVB
CHKRVB:	LDA	ATBFSW		;Is reverse buffering enabled?
	CPI	2
	RC			;No, return
	TST$	WRITOK		;;OK to write to disk?
	RZ			;;No, return
	CLR$	WRITOK		;;Upcoming write not yet completed
;;
	PUSH	D		;Save -> visual routine to be performed next
	LDED	MINSEC		;Save user specified auto-buffer amount
	PUSH	D
	SHLD	MINSEC		;Set temporary auto-buffer amount
	CALL	FRMVIS		;Write back active line, set command edit point
;;
	CALL	PRVFIL		;Attempt reverse auto buffering
	MVIB$	WRITOK,1	;;Write operation successful, OK to try again
;;
	POP	H		;Restore user customized auto-buffer amount
	SHLD	MINSEC
	POP	D		;DE-> visual routine to be performed next
	JRNZ	..2		;Branch if at file begin
	CALL	PUSHOP		;DE's routine will be performed next
..2:	LHLD	EDTPTR		;Rewrite screen
	JMP	TOVIS0
	EPROC	CHKRVB
