	.PAGE
	.TITLE	'VEDIT-V1'
;************************************************
;*						*
;*	V I S U A L   E D I T O R		*
;*						*
;************************************************
;
; Copyright (C) 1986 by CompuView Products, Inc.
;			1955 Pauline Blvd.
;			Ann Arbor, MI 48103
;
;	Written by: Theodore J. Green
;
;	Last change: Ted - Feb. 07, 1985 - VLOOP console check
;			 - Aug. 12, 1985 - BCKATV - restore SCRNOK
;			   Aug. 30, 1985 - BCKATV changes for YF command
;		     Tom - Sep. 18, 1985 - Merged with VPLUS (VLOOP, 2 changes)
;		     Ted - Feb. 12 - Windows
;			   Feb. 14 - Extensive UPDSCR changes
;			   Feb. 18 - VLOOP sets stack from VSTACK
;			   Mar. 10 - Change routines which set NWSCFL
;			   Mar. 27 - SETLOG() based on LSTFLG
;			   May  04 - CHKCUR() changes
;			   May  14 - New [FIND]
;			   July 29 - VLOOP - New REPEAT code
;			   Nov. 04 - WTACLN - Fix writing on last window line
;			   Nov. 05 - SETLOG, handle lone <CR>  and <LF> better
;			   Nov. 07 - Use new POSTBL
;			   Nov. 08 - All new SETPNT
;
; VMAIN - Do Visual editing until exited with RET.
;
;	Usage of NWSCFL:
;
;	80H	Write entire screen from SCRBGN.
;	40H	Scroll screen by direction and amount in NSCROL.
;	20H	Write partial screen from FSACLN.
;	10H	Perform Insert or Delete Line according to LINCHG.
;	08H	Get new text (logical) line.
;	04H	Rewrite entire active line.
;	02H	Rewrite partial active line from WAPWPN.
;	01H	Set ACTPNT from Cursor position in CURVER amd CURHOR.
;
				;{TOVIS drops in here!}
VMAIN:	LXI	H,HZSCLN	;HL-> virtual screen length
				;If HZSCLN = 255, change to 254
	MOV	A,M		;Get value
	INR	A		;Is value 255?
	JRNZ	VMAIN3		;No, branch
	DCR	M		;Yes, change to 254
;
				;{EOFCHK,VINTXT}
VMAIN3:	CALL	STNSNL		;Bits for complete screen, new text line
	JMPR	VMAIN5		;Write the first screen
;
; NEWLIN - This is the outer most part of main work loops.
;
	BPROC	NEWLIN
NEWLIN:	CALL	BCKATV		;Write active line back to text buffer
				;{TOVIS,VUNDO}
VMAIN5:	CALL	CHKSPC		;Check for (make) insertion space
NEWSCR:	ANIM	NWSCFL,0F0H	;Check screen flag if screen to be rewritten
	JRZ	..1		;No, branch
;
	CALL	WRTSCR		;Write a new screen
	ANIM	NWSCFL,09H	;Strip all but new text line, set cursor bits
	MOV	M,A		;Save new screen flag
;
..1:				;HL-> NWSCFL
	MVIB$	ACTCNT,-1	;Set flag so WTACLN won't rewrite screen
	MOV	A,M		;Get screen flag
	ANI	08H		;Set for new text line?
	JRZ	VSTLIN		;No, branch
;
	MOV	A,M		;Yes, get flag again
	XRI	08H		;Strip new text line bit
	MOV	M,A		;Save new screen flag
	CALL	NEWACT		;Find -> text for new active line
	CALL	WTACTV		;Move active line into buffer
	EPROC	NEWLIN
;
VSTLIN:
;	ANIM	NWSCFL,01H	;Set new ACTPNT?
;	JRZ	VLOOP		;No, branch
	CALL	SETPNT		;Set edit pointer from cursor pos
				;Also sets CURCNT and FSACSC
	ANIM	NWSCFL,06H	;Strip all but write active line bits
	MOV	M,A		;Save new screen flag
;
; VLOOP - This is inner most work loop.  It is executed for
;	  every key typed.  The character and function handlers
;	  will set NWSCFL if any outer loops need to be executed.
;
	BPROC	VLOOP
VLOOP:	LHLD	VSTACK		;HL = proper stack value
	SPHL			;Set new SP
;
	MVIB$	INPFLG,080H	;;Make sure INPFLG set for visual mode
				;;Can get corrupted by printing
	CALL	MACSTA		;Is a keystroke macro running?
	JRZ	..NORP		;No, reset REPEAT flag
	CPIB$	RPTFLG,0FFH	;;Is entire macro being repeated?
	BEQ	..KY		;;Yes, keep building the REPEAT string
..NORP:	CLR$	RPTFLG		;Reset flag - not building REPEAT string
;
;	Check for a keyboard character to allow [CANCEL] to abort
;	a [REPEAT] even if there is no polling
;
..KY:	CALL	CONST		;Check keyboard
	BEQ	..1		;No, branch
	CALL	KBFCHK		;Yes, but is keyboard buffer full?
	BEQ	..1		;Yes, leave char for later
	CALL	DECKEY		;Get the char; check for [CANCEL]
	CNC	PSHCHR		;Save char in keyboard buffer
;
..1:	ANIM	NWSCFL,08H	;Is screen flag set for new text line?
	JRNZ	NEWLIN		;Yes, branch
	MOV	A,M		;Get flag again
	CPI	10H		;New screen written?
	BGE	NEWSCR		;Yes, branch
	ANI	01H		;New ACTPNT from cursor?
	JRNZ	VSTLIN		;Yes, branch
;
	CALL	WTACLN		;Rewrite active line as needed
				;Set cursor pos. from edit pointer
	LXI	H,VLOOP		;Get address of this routine
	PUSH	H		;Save as return address
	CLR$	EOLFLG		;Set for EOL on active line
	ANIM	NWSCFL,0F9H	;Did WTACLN change # lines or cont. line?
	RNZ			;Yes, decode it. (VLOOP on stack)
	MVI	M,00		;No, then screen is up to date
;
;	Check if [CANCEL] is pending
;
	TST$	CNCLFL		;Is [CANCEL] pending?
	JNZ	CANCEL		;Yes, jump to routine
;
;	Branch to write screen if in middle of REPLACE command
;
	TST$	REPLFL		;Is REPLACE command active?
	BNE	..6		;Yes, branch to write screen
;
;	Perform any operation on operation stack.
;
..3:	CALL	DECOP		;Test, decrement operation stack
	JRC	..5		;Branch if stack empty
	JRNZ	..4		;Branch if count still valid
	CALL	POPOP		;Pop stack if count is zero
	JMPR	..3		;Check stack again
;
..4:	MOV	C,A		;Put char. in C
	XCHG			;Put routine address in HL
	XRA	A		;Need A = 00
	PCHL			;Branch to routine
;
;	Branch around if only updating window
;
..5:
	IF	WINDOO, [
	TST$	WWUPFL		;Is this only a window update?
	JRNZ	..6		;Yes, don't read keyboard
	]
;
	TST$	REPLFL		;Is FIND/REPLACE command active?
	JRNZ	..6		;Yes, don't read keyboard
;
	CALL	GETSTA		;Is a keyboard char waiting?
	JRNZ	..JKEY		;Yes, go process it
;
..6:	CALL	UPDSCR		;Update a screen line
	JRNC	..5		;Keep updating screen until done
;
;	If only a window update, we are done in visual mode
;
	IF	WINDOO, [
	TST$	WWUPFL		;Is this only a window update?
	JNZ	VEND		;Yes, return from Visual Mode (WWUPFL reset later)
	]
;
;	Branch if in the middle of FIND/REPLACE.
;
	TST$	REPLFL		;Is FIND/REPLACE command active?
	JNZ	VFNEXE		;Yes, perform FIND/REPLACE
;
	CALL	CHKCUR		;Make cursor positioned
	RC			;Rewrite screen if screen horz. scrolled 
				;(VLOOP on stack)
;
;	Status line and column #s are not updated until the
;	user pauses typing.
;
	IFNOT	MEMVRS, [
	CPIB$	STLNFL,3	;Is status line set for visual mode?
	BNE	..8		;Branch only if we need a complete new line
				;i.e. due to FIND, or other prompts
				;Will also catch very first status line write
	MVI	D,15		;Init down counter
..7:	PUSH	D		;Save counter
	CALL	GETSTA		;Is a keyboard char waiting?
	POP	D		;Restore counter
	JRNZ	..JKEY		;Yes, go process it
;
	MVI	A,20		;Delay 20 milliseconds
	CALL	DELAY
	DCR	D		;Count down
	JRNZ	..7		;Keep checking status
	]
;
..8:	CALL	CHKBRD		;Perform any status line update
..JKEY:	JMP	PROCKY		;Process a key
	EPROC	VLOOP
	.PAGE
;
;
; UPDSCR - Update any necessary window lines.		[11/7/86]
;
;	UPDLIN <  RRWLIN	:: Begin new update from UPDLIN
;	RRWLIN <= WWNLIN	:: Continue update; UPDLIN = -1
;	RRWLIN >  WWNLIN	:: All done, return 'C'
;
				;{VLOOP}
	BPROC	UPDSCR
UPDSCR:	LXI	H,RRWLIN	;HL-> line # to update
	LDA	UPDLIN		;A = beginning line # for new update
	CMP	M		;Need a new update?
	BLT	..NEW		;Yes, A = line to start at
;
	LDA	WWNLIN		;Get # window lines
	CMP	M		;In middle of screen update?
	MOV	A,M		;A = RRWLIN
	RC			;No, return with 'C' if updating done
	JMPR	..1		;Yes, update another line
;
..NEW:	DCX$	H		;HL-> UPDLIN
	MVI	M,0FFH		;Reset update value
;
;	Check if writing active or non-active line
;
..1:	CMPM	FSACSC		;Need to write active line?
	BLT	..2		;No, update earlier line
	INX$	H		;HL-> LSACSC
	CMP	M		;Write active line?
	BEQ	..5		;Yes, write active line
	BLT	..5		;8080 lacks BLE
;
;	Write non-active line on screen.
;
..2:	CALL	SETWRT		;Setup DE and LOGPOS for WRTLIN
	CALL	WRTLIN		;Write another line
;
..3:	JRNC	..4		;Branch if EOF not reached
	LDA	WWNLIN		;Value that screen updating done
..4:	LXI	H,RRWLIN	;HL-> next line # to update
	INR	A		;Set line # to next line
	MOV	M,A		;Set RRWLIN for next update
;
	LDA	WWNLIN		;A = last line in window
	CMP	M		;Screen updating done?
	RNC			;No, return 'NC', RRWLIN set for next update
;
	LDA	LINCNT		;Updating done, A = last line number written
	CALL	CLRSC2		;Clear rest of the window
	XRA	A		;Set 'NC'
	RET			;Return 'NC'
;
;	Write active line on screen.
;
..5:	CALL	UPDACT		;Rewrite entire active line
	JMPR	..3		;Merge above
	EPROC	UPDSCR
;
; UPDACT - Rewrite entire active line.  Return 'C' if EOF reached.
;
	BPROC	UPDACT
UPDACT:	CALL	WTACL0		;Write the active line
	LDA	LSACSC		;Get last line # written
	CMPM	WWNLIN		;Past end of window?
	BLT	..1		;No, branch
	MOV	A,M		;Yes, set to max
..1:	MOV	B,A		;Save line #
	CALL	ENDCHK		;Does active line end in EOF?
	MOV	A,B		;Restore line #.  'C' if EOF
	RET
	EPROC	UPDACT
	.PAGE
;
; CHKCUR - Check that screen cursor is set correctly for editing
;	   Return: Cursor positioned correctly on screen, unless
;		   horizontal scrolling required. Then NWSCFL set.
;		   'C' if screen needs to be rewritten based on NWSCFL.
;
				;{VLOOP,PROCKY,REPEAT,CHGACT,VRPEXE,HORFIX}
CHKCUR:	CALL	HZRGCK		;Is cursor within horizontal scroll window?
	RC			;No, return 'C' to rewrite screen
;
	LD16	H,CURVER
	CALL	WINSET		;Position cursor
				;Unneeded positioning ignored at PHYSET()
	XRA	A		;Make sure 'NC'
	RET
;
; HZRGCK -
;
	BPROC	HZRGCK
HZRGCK:	LDA	CURHOR		;Get virtual cursor pos
	CALL	RNGCHK		;Is position on screen?
	JRC	..1		;No, branch to scroll screen
;
	ANIB$	NWSCFL,80H	;Is new screen needed due to scroll?
	RZ			;No, return 'NC'
	STC			;Yes, return 'C'
	RET
;
;	Need to force screen to contain cursor pos
;
..1:	CMPM	HZSCEN		;Past end of screen?
	BLT	..2		;No, must be before screen
;
	CALL	HZRTRG		;Yes, window screen right
	JMPR	HZRGCK		;Repeat if necessary
;
..2:	CALL	HZRTLF		;Window screen left
	JMPR	HZRGCK		;Repeat if necessary
	EPROC	HZRGCK
;
				;{HZRGCK,WINSET)
RNGCHK:	CMPM	HZSCEN		;Within physical screen?
	CMC			;Change 'NC' to 'C'
	RC			;Return 'C' if out of range
	DCX$	H		;HL-> HZSCBG
	CMP	M		;Within physical screen?
	RET			;Return 'C' if out of range
	.PAGE
;
; WTACTV - Move 1 line of text into active line buffer.
;	   Enter HL-> Text buffer for active line.
;	   If EOF encountered, do not bump TXACEN, but place in ACTBUF,
;	   otherwise BUFFUP is called with -> past the EOF.
;
	BPROC	WTACTV
WTACTV:	LXI	D,ACTBUF-1	;DE-> active buffer
	LXI	B,ACTLEN	;Get allowable length of active line
..1:	INX$	D		;Bump active line PTR
	MOV	A,M		;Get next text char
	STAX	D		;Move to active buffer
	CPI	EOF		;EOF?
	BEQ	..2		;Yes, don't bump TXACEN
	INX$	H		;Bump main PTR
	CPI	LF		;A LF?
	JRZ	..2		;Yes, branch
	LOOP	..1		;Decrement BC and loop until == 0
;
..2:
	IF	POLLING, [
	CALL	CHKKYB		;Poll keyboard
	]
;
	SHLD	TXACEN		;Save -> past last char . (or to EOF)
	XCHG			;HL-> end of active line
	SHLD	ACTEND		;Save -> last char
	LXI	D,ACTBUF-1	;DE-> adjusted begin of active line
	DSUB	D		;Compute length of active line
	SHLD	HOLSIZ		;Save as hole size
	SHLD	ACTSIZ		;Save as active line length
	RET
	EPROC	WTACTV
	.PAGE
;
; BCKATV - Move Active line back into text buffer.  Update HOLSIZ
;	   so that routine can be called several times in a row.
;
;	   [COPY ...] causes BCKATV to be called.
;	   Checks VISFLG before calling UPSCTB to allow cmd mode formatting. [7/23/85]
;
;	   Return: HL = TXACEN -> next line to to EOF.
;
				;{FRMVIS,VMAIN,VJUMP,CRHOME,PAGEDW,SAVLIN,FRMELN}
	BPROC	BCKATV
BCKATV:	SUB%BC	ACTSIZ,HOLSIZ	;BC = ACTSIZ - HOLSIZ
	PUSH	H		;Save length of active line
	SHLD	HOLSIZ		;Save updated hole size
	LDA	NWSCFL		;Get NWSCFL value (remember YF)
	PUSHA
	BEQ	..3		;Branch if same size
;
	PUSHF			;Save flags
	TST$	VISFLG		;In visual mode?
	CNZ	UPSCTB		;Don't update SCB for YFCMD
	LHLD	TXACEN		;HL = TXACEN -> begin of text to move
	POPF			;Restore flags. (ACTSIZ-HOLSIZ)
	BLT	..2		;Branch if text hole too large
;
; Move text after "hole" up by BC.
;
	CALL	BUFFUP		;Move text up
	JMPR	..3		;Move active line into text buffer
;
; Move text after "hole" down by -BC.
;
..2:	CALL	BUFFDW		;Move text down
;
; Perform actual move.
;
..3:	CLR$	CNTBFL		;Clear flag set in BUFFUP/DW
	POPA			;Restore NWSCFL
	STA	NWSCFL		;Fix Flag cleared in BUFFUP/DW
	LDED$	TXACTV		;DE-> text buffer hole
	POP	B		;BC = length of active line
	LXI	H,ACTBUF	;HL-> active line
	CALL	MOVEBC		;Move active line into text buffer
				;DE-> past last char
	LHLD	TXTRWF		;HL = End PTR
	CALL	MNHLDE		;HL = lesser of TXACEN and ENDPTR
				;Necessary when EOF in active line
	SHLD	TXACEN		;Save -> past last char
	RET
	EPROC	BCKATV
	.PAGE
;
; NEWACT - Returns in HL -> new active line based on the
;	   SCB and line the cursor is on.  Sets TXACTV.
;	   Updates line # count as necessary.
;
				;{VMAIN}
	BPROC	NEWACT
NEWACT:	LDA	CURVER		;Get line cursor is on
	CALL	FCBADD		;HL-> first char of this screen line
				;{TOVIS}
NEWAC2:	DCX$	H		;This line may consist of lone LF
	CALL	PREVLF		;Find end of last logical line
	INX$	H		;HL-> beginning of this logical line
	CALL	MINBGN		;Make sure HL has min. value of TXTBGN
				;Happens if CRLF missing from last written line
	SHLD	TXACTV		;Save pointer to begin of active line
;
;	Update LINFIL.  If CNTBFL is set, must count LFs from TXTBUF.
;	else must just compare to OLDACT, which is then updated.
;
	XCHG			;DE-> begin of active line
	TSTM	CNTBFL		;Need to count LF from TXTBUF?
	MVI	M,00		;Clear the flag
	JRZ	..2		;No, compare to OLDACT
;
	XCHG			;HL-> begin of line
	CALL	CNTFIL		;HL = Count of # LF from Begin of file
	JMPR	..4		;Save line # in file
;
..2:	LHLD	OLDACT		;HL-> text for old LINFIL value
	CALL	CMHLDE		;Before, after or at TXACTV?
	RZ			;Same position, return
	PUSHF			;Save flags of > or <
	LXI	B,-1		;Init LF count
	BGE	..3		;Branch if OLDACT > TXACTV
	XCHG			;Need HL > DE
..3:	CALL	CNTLFB		;Compute # of LFs
;
	POPF			;Was OLDACT > or < TXACTV?
	CNC	NEGBC		;Negate count if line change is neg
	LHLD	LINFIL		;Get old line #
	DAD	B		;Add change
..4:	SHLD	LINFIL		;Save new line #
	MOVW	OLDACT,TXACTV	;Save this active line position
	RET
	EPROC	NEWACT
	.PAGE
;
; SETPNT - Sets ACTPNT in active buffer based on cursor positions.
;	   Also sets CURCNT, FSACSC.	(11/9/86)
;
	BPROC	SETPNT
SETPNT:	CALL	CURPOS		;;HL = LOGPOS for first char on line
	CALL	DVHOR1		;;B = continuation line #
	LDA	CURVER		;Get cursor position
	SUB	B		;Compute first active line #
	STA	FSACSC		;Save first active line #
	MOV	A,B		;Get cont. line # we are on
	STA	CURCNT		;Save cursor continuation #
;
;	Compute logical position corresponding to CURVER and CURHOR.
;	Note: Actually is more difficult to compute using CURPOS().
;
	LD16	D,HZSCLN	;Get displayed line length
	MVI	D,00		;8 bit value
	LD16	H,CURHOR	;Get Horiz. cursor pos
	MVI	H,00		;8 bit value
	ORA	A		;Are we on a cont. line?
	JRZ	..6		;No, HL = logical pos
..5:	DAD	D		;Add to logical position
	DJNZ	..5		;Multiply by B
;
;	Determine value for ACTPNT which reached the logical position
;
..6:	MOV	B,H
	MOV	C,L		;;BC = logical cursor position
	LXI	H,1		;Init. logical horizontal pos
	LXI	D,ACTBUF-1	;DE-> active line buffer -1
;
..7:	INX$	D		;Increment active line pointer
	CALL	CMHLBC		;Has horz. pos. reached cursor pos.?
	BGE	..9		;Yes, branch
	LDAX	D		;No, get next active line char
	CALL	SETLOG		;Compute logical pos
	JRNC	..7		;Continue, unless end of line found
;
..9:	SDED	ACTPNT		;+Save pointer into active line
	RET
	EPROC	SETPNT
	.PAGE
;
; SETLOG - Set logical position.  Expand control characters based on LSTFLG.
;	   Enter: HL = begin value, A = char.
;		  DE-> char if return codes desired.
;	   Retrn: HL = end value;  BC, DE saved.
;		  'C' if EOF, LF or CR-LF.
;
				;{FRMCHR,SETPNT,GETLGP,FORMIN}
	BPROC	SETLOG
SETLOG:	CPI	SPACE		;Is it a control char?
	INX$	H		;;Assume NO
	RNC			;;No, return 'NC' and HL incremented
;
	DCX$	H		;;Yes, restore HL
	CPI	SPACE		
	BGE	..1W		;No, it is one pos. wide
	CPI	TAB		;Is it a Tab?
	BEQ	..TAB		;Yes, branch
	CPI	ESC
	BEQ	..ESC
	CPI	CR
	BEQ	..4W
	CPI	LF
	BEQ	..4W
;
;	Deal with control characters
;
..CTRL:	ANIB$	LSTFLG,1	;Expand control chars?
	JRZ	..1W		;No, they are just one pos. wide
	JMPR	..2W		;Yes, they are two wide
;
..TAB:	ANIB$	LSTFLG,2	;Expand Tab chars?
	JRZ	..CTRL		;No, treat as normal CTRL char
	CALL	FNDTAB		;Yes, find next tab position
	JRZ	..CTRL		;Branch if not valid tab
	MOV	L,A		;Update offset if valid
	ORA	A		;'NC'
	RET			;Return HL = new value
;
..ESC:	ANIB$	LSTFLG,8H	;Change ESC to $?
	JRZ	..CTRL		;No, treat ESC as normal CTRL char
	JMPR	..1W		;Yes, it is one pos. wide
;
..4W:	INX$	H		;<CR> and <LF> are four wide
	INX$	H
..2W:	INX$	H		;Yes, control chars take two positions
..1W:	INX$	H		;Increment horz. pos
;
	CALL	DE%EOL		;;Is (DE) at end-of-line (EOF, LF or CR-LF)?
	STC			;Assume yes
	RZ			;Yes, return 'C'
	ORA	A		;No, return 'NC'
	RET
	EPROC	SETLOG
	.PAGE
;
; GETLGP -  Compute logical position for character at (HL).
;	    Enter: HL-> char.
;	    Retrn: HL = logical position,; BC and DE clobbered.
;
				;{XGET}
	BPROC	GETLGP
GETLGP:	MOV	D,H		;
	MOV	E,L		;DE -> current char
	CALL	PREVLF		;HL -> preceeding LF
	INX$	H		;HL -> start of line
;
;	Compute logical positions from (HL) to (DE)
;
				;{BSCCNT,TOVIS}
GETLP0:	MOV	B,D
	MOV	C,E		;;BC = end PTR
	XCHG			;;DE = begin PTR
	LXI	H,1		;Init position counter
	DCX$	D		;;Account for INX$ D below
;
..1:	INX$	D		;DE-> next char
	CALL	CMBCDE		;Reached end yet?
	RZ			;Yes, return
	LDAX	D		;No, get next char
	CALL	SETLOG		;Compute new logical position
	JRNC	..1		;Keep counting if not end-of-line
	RET
	EPROC	GETLGP
	.PAGE
;
; WTACLN - Rewrite all, some or no part of active line on screen.
;	   Computes horizontal cursor position from ACTPNT during rewrite.
;
;	Enter - CURVER, CURCNT, FSACSC, ACTPNT.
;	Exit  - LSACSC, LINCHG, EDTHOR, CURHOR, LOGHOR, NWSCFL= 00.
;
;	NOTE: UPDACT calls WTACL0 with WTACFL == 0; kludge city.
;
;  1)	Perform a fake write on entire logical line and compare
;	resulting # continuation lines with previous value in ACTCNT.
;	This also sets logical horz. pos. LOGHOR.
;
	BPROC	WTACLN
WTACLN:	MVI	A,1		;Get one for WTACFL = 1
	CALL	STWTPS		;Set WTACLN, reset LOGPOS=1
	LDA	FSACSC		;Get first active line #
	DCR	A		;Account for INR A below
	LXI	D,ACTBUF	;DE-> active line
	MVI	C,-1		;C = continuation line counter
;
..1:	INR	C		;Increment line counter
	INR	A		;Increment line #
	MOV	B,A		;;Save line # in B
	JRM	..2		;Skip check if before top of window
	LDA	WWNLIN		;;A = last window line #
	CMP	B		;;Past last line in window?
	BLT	..3		;;Yes, only count to end of window
;
..2:	PUSH	B		;Save counter
	MVI	A,-1		;Set to write fake line
	CALL	WRTLNA		;Write a fake active line (without table update)
	POP	B		;Restore counter
	MOV	A,B		;A = line # of line just written
	JRNZ	..1		;Repeat until LF or EOF reached
;
;	Check if active line has changed number of screen lines
;	since last screen write.
;
..3:	MOV	A,B		;;A = last active line written
	STA	LSACSC		;Save last active screen line #
;
	LDAM	ACTCNT		;Get previous # of active screen lines
	MOV	M,C		;Save new value
	CPI	-1		;Ignore old value?
	BEQ	..5		;Yes, branch
	SUB	C		;No, has the # of screen lines changed?
	BEQ	..5		;No, ready to update screen
	STA	LINCHG		;Save difference. -1= insert line, 1= delete line
	JM	..4		;Branch if insert line
	CPI	2		;Deleting more than one line?
	CNC	STPSNL		;Yes, write partial screen
..4:	CALL	STCSNL		;Yes, INS/DEL screen lines, new text line
	JMPR	WTACL9		;Branch to check for cursor up or down
;
;	Screen can be updated now.
;
..5:	ANIB	NWSCFL,06	;Does any part of active line need writting?
	JRZ	WTACL9		;No, skip any screen writting
	ANI	04		;Writing entire active line?
	JRZ	WTACL5		;No, setup for partial write
;
;	Setup to write entire active line.
;
				;{UPDSCR}
WTACL0:	LDA	FSACSC		;Begin write from first active line
	MOV	C,A		;Put # of first line to write in C
	LXI	H,1		;Logical position is also 1
	LXI	D,ACTBUF	;DE-> begin of active line
	JMPR	WTACL6		;Finish setup for WRTLIN call
;
;	Setup for writing partial active line.
;
WTACL5:	LDA	WACURV		;Get previous cursor vertical
	MOV	C,A		;Put # of first line to write in C
	LDED$	WAPWPN		;Get starting point for partial write
	LHLD	WALHOR		;Get logical write position
;
;	Before WRTLNA can be called to write an active line, the
;	variables WINHOR, LOGPOS must be setup.
;
WTACL6:	SHLD	LOGPOS		;Setup logical write position
	DCR	C		;Adjust for INR C below
;
;	Loop to  rewrite active line on the screen.
;
WTACL7:	INR	C		;Bump to next line
	LDA	WWNLIN	 	;Get last window line #
	CMP	C		;Is line # past last window line?
	BLT	WTACL9		;Yes, branch
	PUSH	B		;Save line #
	MOV	A,C		;Put line # in A
	CALL	WRTLNA		;Write a physical active line
	POP	B		;Restore line #
	BNE	WTACL7		;Repeat until LF or EOF
;
WTACL9:	CALL	DIVHOR		;A = CURHOR, B = continuation from LOGHOR
	CALL	SETHOR		;Set EDTHOR and CURHOR as needed
	MOV	A,B		;A = new cont. #
	LXI	H,CURCNT	;HL-> line cont. old cursor was on
	CMP	M		;Is cursor on same continuation line?
	MOV	M,A		;Save new value for UPDACT call
	JC	CRUP		;New < old then cursor up
	JNZ	CRDOW2		;New > old then cursor down
				;If CRDOW2 or CRUP, NWSCFL is set and WTACLN
				;is called later to set partial write pointers
;
;	Save current position for later partial write.
;
	XRA	A		;Zero to clear WTACFL flag
	CALL	STWTPS		;Clear WTACFL and LOGPOS
				;Needed in case line ends in EOF
;
	MOVB	WACURV,CURVER	;Save vert. and hor. write position
	MOVW	WAPWPN,ACTPNT	;Save edit pointer as partial write pointer
	MOVW	WALHOR,LOGHOR	;Save logical position too
	RET
	EPROC	WTACLN
	.PAGE
;
; UPSCTB - Update the window FCB by adjusting all lines
;	   following LSACSC by text movement in BCKATV.
;
				;{BCKATV}
UPSCTB:	LDA	LSACSC		;Get last active window line
	MOV	E,A		;Save line #
	INR	A		;Start at following window line
	CALL	IDXFCB		;ES:HL-> FCB for this line
	LDA	WWNLIN		;Get # window lines
	SUB	E		;Compute # FCB entries to change
	RC			;;Prevent high-memory becoming a Big-Mac
	MOV	E,A		;Put in E. (Could be zero)
	JMPES	CHGTBL		;Change 'E' entries at (HL) by BC
;
; DIVHOR - Computes Cursor horizontal position from logical
;	   position. Returns B = continuation line for cursor.
;	   Note: Assume HZSCLN = 80, then enter 79 - exit
;	   0 & 79, enter 80 -> 1 & 1, enter 81 -> 1 & 2.
;
				;{WTACLN}
	BPROC	DIVHOR
DIVHOR:	LHLD	LOGHOR		;Get logical cursor position
	JMPR	DVHOR1
;
				;{WRTLIN}
DIVPOS:	LHLD	LOGPOS		;Get logical write position
				;{TOVIS,CRTAB}
DVHOR1:	PUSH	D		;;Save DE
	LD16	D,HZSCLN	;Get displayed line length
	MVI	D,00
	MOV	B,D		;Init counter
	INX$	D		;Adjust, DE = 256 is OK
..2:	DSUB	D		;Subtract one line worth
	BLT	..3		;Branch if too much
	INX$	H		;Adjust for LINLEN - 1!!!
	INR	B		;Inc. continuation counter
	JMPR	..2		;Continue
;
..3:	DAD	D		;Add line length back
	MOV	A,L		;Get final cursor pos
	POP	D		;;Restore DE
	RET
	EPROC	DIVHOR
	.PAGE
;
;	Routines to set flags in NWSCFL.
;
STACPN:	MVI	A,01H		;Bit to set new ACTPNT
	JMPR	STNWFL		;Merge below
;
STPWAC:	MVI	A,02H		;Bit for partial active line
	JMPR	STNWFL		;Merge below
;
STRWAC:	MVI	A,04H		;Bit to rewrite active line
	JMPR	STNWFL		;Merge below
;
STNWLN:	MVI	A,09H		;Bits for new text line
	JMPR	STNWFL
;
STCSNL:	MVI	A,19H		;Bits to change # screen lines, new text line
	JMPR	STNWFL		;Merge below
;
STPSNL:	MVI	A,29H		;Bits for partial screen, new line
	JMPR	STNWFL		;Merge below
;
STSSNL:	MVI	A,49H		;Bits to scroll screen, new text line
	JMPR	STNWFL
;
STNSKL:	MVI	A,80H		;Bit to rewrite entire screen
	JMPR	STNWFL		;Merge below
;
				;{EECMD1,YFCMD,ICMD,EQCMD,OVERWT,ADJPNT}
				;{READSC,READTX,WRTTXT,VISMSS,#OTH}
				;{VEDCMD,TOVIS1,WIZOOMWIBORD,VMAIN3}
				;{VRPEXE,ERLINE,PAGEUP,PAGEDW}
STNSNL:	MVI	A,089H		;Bits for new text line and screen
STNWFL:	PUSH	H		;Save HL
	LXI	H,NWSCFL	;HL-> multi-bit screen update flag
	ORA	M		;OR old bits with new ones
	MOV	M,A		;Save new flag
	POP	H		;Restore HL
	RET
	.PAGE
;
; BACKLN - Determines text pointer for "previous" line.
;	   Enter: HL-> text at beginning of a screen line.
;	   Return: HL-> text for "previous" screen line
;		   'NC' if pointer already at TXTBGN.
;	   	   B = # lines written to reach entry point.
;		   C is preserved, rest clobbered.
;
				;{TOVIS,CRUPRT,FAKSCR}
	BPROC	BACKLN
BACKLN:	XCHG			;DE-> text buffer for line
	LHLD	TXTBGN		;HL-> first text character
	CALL	CMHLDE		;Is pointer at or before file begin?
	RNC			;Yes, return with TXTBGN
	XCHG			;HL = pointer again
				;(Note: enter here if dealing with ACTBUF)
	DCX$	H		;HL-> char ending last line
	DCX$	H		;Search point for LF
	PUSH	H		;Save end of previous line
	CALL	PREVLF		;HL-> previous LF
	INX$	H		;HL-> begin of next line
	XCHG			;DE-> where to write from in text
	XRA	A		;Get zero for WTACFL = 0
	MOV	B,A		;Init counter B = 0
	CALL	STWTPS		;Reset WTACFL and LOGPOS
;
;	Compute Tab-split value for top of screen from LOGPOS
;
..2:	LHLD	LOGPOS		;;Logical Horz. pos.
	CALL	STLGTP		;;Save as POSTBL[0] for top line
;
	PUSH	D		;Save begin point for this line
	INR	B		;Increment line counter
	PUSH	B
	MVI	A,-1		;Neg. # to write fake line
	CALL	WRTLIN		;Write a fake line
	POP	B
	POP	H		;HL = begin point for this line
	XTHL			;HL = end of desired line
	CALL	CMHLDE		;Reached end of desired line?
	XTHL			;HL = begin point for this line
	BGE	..2		;No, continue
	POP	D		;Clear stack
	RET			;Return with 'C' and 'NZ'
	EPROC	BACKLN
