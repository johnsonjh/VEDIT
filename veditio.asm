	.PAGE
	.TITLE	'VEDIT-IO'
;****************************************
;*					*
;*	Console I/O Routines		*
;*					*
;****************************************
;
; Copyright (C) 1987 by CompuView Products, Inc.
;			1955 Pauline Blvd.
;			Ann Arbor, MI 48103
;
;	Written by: Theodore J. Green
;
;	Last Change: Ted - Oct. 28 - 8 bit/FUNC/CTRL-key handling
;			   Nov. 10 - Check for REPEAT buffer overflow
;			   Nov. 11 - DEFINE() extensive changes; DECDAT[] in DS
;			   Nov. 12 - DECKEY() and CHKKEY() combined
;				     GETCHR sets 'Z' if B = 0; AL = char in CL
;			   Nov. 18 - Fix KEYMAT bug
;			   Nov. 24 - Fix PEJECT bug (when # printed = #/page)
;			   Nov. 30 - All new DECKEY (again!)
;			   Dec. 04 - GETCHR check for CNS Lock
;			   Dec. 09 - DECKEY & GETCHR return "VN" if undefined
;			   Apr. 01, 1987 - Fix bug redefining displayable chars
;			   Apr. 06 - DEFINE() clears CNCLFL (bug fix)
;			   Apr. 09 - TRS-80-II bank select
;
;	Storage area for keyboard decoding
;
	DSEG	$
	PUBLIC	DECDAT, KEYBUF, KBFPUT, KBFGET, KEYPTR, seqadd, RPTSTR, RPTCNT
DECDAT:	DS	16		;Buffer for control sequence (size @ KEYADD)
KEYBUF:	DS	40		;Keyboard buffer; each entry is 2 bytes
;
KBFPUT:	DW	KEYBUF		;Keyboard buffer PUT Ptr
KBFGET:	DW	KEYBUF		;Keyboard buffer GET Ptr
;
;
DEFTMP:	DS	1		;;Flag used in DEFINE() (0=Confirmation needed)
DECPUT:	DSW	1		;-> DECDAT, now in DS:
LRNPUT:	DSW	1
KEYSAV:	DSW	1		;Temp used in KEYMAT
KEYEND:	DSW	1		;-> past final KFF in key table
KEYPTR:	DSW	PTRSIZ		;PTR for keystroke macros (init at MACRST)
;
SEQADD:	DSW	PTRSIZ		;-> control sequence in Key-Table (when found)
;
LEPRMT:	DC	'NEW FUNCTION/CONTROL KEY? - [DEFINE] TO END ' [00]
LESTRG:	DC	'KEY STRING? - [DEFINE] TO END ' [00]
LECONF:	DC	'REDEFINE EXISTING KEY (Y/N)? ' [00]
LEDCNF:	DC	'REDEFINE DISPLAYABLE CHAR (Y/N)? ' [00]
LERRMS:	DC	'INVALID SEQUENCE ' [00]
;
;	Data storage for REPEAT function
;
	IFNOT	P8086, [
RPTSTR	==	DEFFCB		;Use DEFFCB for string storage
RPTSEN	==	RPTSTR+32
	] [
RPTSTR:	DSW	30
RPTSEN:	DS	2		;2 spare
	]

RPTCNT:	DW	0000		;REPEAT down counter
RPTPTR:	DW	RPTSTR		;-> to string for [REPEAT]
;
;	Data storage for PRINT functions
;
PPMRFL:	DS	1		;Flag that left margin needed for printer
PPLCNT:	DB	0		;Current line number on printed page
PPLSTX:	DS	1		;Last line number for text
;
LOW418	=	417H		;Offset to keyboard status byte
	CSEG	$
	.PAGE
	IF	POLLING, [
;
; CHKKEY - Checks if a keyboard character is ready.  If so, it is added
;	   to KEYBUF.  In visual mode, an entire control sequence is decoded.
;
;	   NOTE: Self modifying code at BEGIN, VSTART
;
	BPROC	CHKKYB
				;{OUTCHR,CRTCRL,FNDTAB,DELAY,MOVEBC,MOVEUP}
CHKKYB:	NOP			;Changed to RET to disable fast polling
				;{CRTCKK,MVCHST}
CHKKEY:	NOP			;Changed to RET to disable slow polling
	PUSHXF			;+Save AX and flags
	TST$	VISFLG		;Is this visual mode?
	CNZ	KYPOLL		;Only Poll if visual mode
				;BRKCHK does all command mode polling
	POPXF			;+Restore all registers
	RET	
	EPROC	CHKKYB
;
; KYPOLL - Poll keyboard for character and if available
;	   AND polling buffer has room, buffer the character.
;	   In Visual Mode polls for entire sequence.
;
				;{CHKKEY,GETSTA,VLOOP}
	BPROC	KYPOLL
KYPOLL:	PUSH	H		;Save all registers
	PUSH	D
	PUSH	B
;6	PUSH	ES
	CALL	CONST		;Is a char. ready?
	JRZ	..1		;No, ready to return
	CALL	KBFCHK		;Is the keyboard buffer full?
	JRZ	..1		;Yes, return, leave char for later
	CALL	DECKEY		;Get keyboard char and check for CANCEL
				;BC = char, HL = PTR, 'C' if undefined keystroke
	CNC	PSHCHR		;Save char or PTR in keyboard buffer
..1:
;6	POP	ES
	POP	B
	POP	D
	POP	H
	RET
	EPROC	KYPOLL
	]			;<IF POLLING>
;
;
; KBFCHK - Check if keyboard buffer is full.
;
;	   Assumption: Need to add two chars, KEYBUF is of even length
;	   Return: 'Z' if buffer full, 'NZ' if not full
;
				;{KYPOLL,BRKCHK}
KBFCHK:	LHLD	KBFPUT		;HL = PUT Ptr
	INX$	H		;Bump PTR by 2 bytes
	INX$	H
	CALL	KEYWRP		;Wrap HL if reached end of buffer
;
KBFCMM:	LDED	KBFGET		;Get buffer GET Ptr
	JMP	CMHLDE		;Is the buffer empty/full?
				;'Z' if empty/full, 'NZ' if char
;
; PSHCHR - Save char in BC / or PTR in HL in KEYBUF polling buffer.
;	   Enter: BC and HL set from DECKEY.
;
;	   NOTE: Be sure to CALL KBFCHK first.
;
				;{KYPOLL,BRKCHK}
	BPROC	PSHCHR
PSHCHR:	MOV	A,H		;Is the PTR set?
	ORA	L
	JRZ	..1		;No, branch if BC = simple char
	MOV	B,H		;Yes, move PTR to BC
	MOV	C,L
..1:	LHLD	KBFPUT		;HL = PUT Ptr
	MOV	M,C		;Save char or PTR in KEYBUF[]
	INX$	H
	MOV	M,B
	INX$	H
	CALL	KEYWRP		;Wrap HL if reached end of buffer
	SHLD	KBFPUT		;Save new PUT PTR
	RET
	EPROC	PSHCHR
;
; KEYWRP - Wrap HL if reached end of KEYBUF.
;
KEYWRP:	LXI	D,KEYBUF+40	;DE-> past end of buffer
	CALL	CMHLDE		;Reached end of buffer?
	RNZ			;No, return
	LXI	H,KEYBUF	;Yes, wrap around
	RET
;
;
; POLSTA - Get status/16 bit char/PTR from polling buffer.
;	   Return: 'Z' if empty.
;		   'NZ' if char/PTR ready, char/PTR in BC.  HL = new Get PTR.
;
				;{GETSTA,GETCHR}
POLSTA:	LHLD	KBFPUT		;HL = Put PTR
	CALL	KBFCMM		;Return 'Z' if buffer empty; DE = KBFGET
	RZ			;'Z' if empty
	XCHG			;HL = KBFGET
	MOV	C,M		;Put char into C
	INX$	H
	MOV	B,M		;Put 00/PTR into B
	INX$	H
	CALL	KEYWRP		;Wrap HL if reached end of buffer
	ORI	1		;Return 'NZ'
	RET
	.PAGE
;
; RPTSTA - Get status/char of repeat string.
;	   Retrn: 'Z' if empty.
;		  'NZ' if char ready, BC = char/code, HL-> new RPTPTR value.
;
RPTSTA:	TSTW$	RPTCNT		;Is the repeat count zero?
	RZ			;Yes, return 'Z'
	TST$	RPTFLG		;Still building repeat string?
	JNZ	RET%Z		;Yes, return 'Z'
	LHLD	RPTPTR		;HL-> next key character
	MOV	C,M
	INX$	H
	MOV	B,M		;BC = next key char
	INX$	H
	MVI	A,KFF		;Get terminator
	CMP	C		;Reached terminator?
	RNZ			;No, return BC, and 'NZ'
	LHLD	RPTCNT		;Yes, decrement count
	DCX$	H
	SHLD	RPTCNT		
	MVIW$	RPTPTR,RPTSTR	;Init PTR-> begin of string
	JMPR	RPTSTA		;Try again
;
; RPTADD - Add char/code in BC to string (RPTPTR)
;	   Return: See return codes for GETCHR()
;
	BPROC	RPTADD
				;{GETCHR via return address on stack}
RPTADD:	LDA	RPTFLG		;Get repeat setup flag
	CPI	2		;In middle of [FIND] or [REPLACE]
	JRZ	RPTAD3		;Yes, don't add to repeat string
	ORA	A		;Building the repeat string?
	LXI	H,RPTSTR	;Assume NO, reset string
	JRZ	..1		;No, branch
	LHLD	RPTPTR		;Yes, add to string
	LXI	D,RPTSEN	;DE = upper limit
	CALL	CMHLDE		;Reached limit?
	JRNC	GCHRRT		;Yes, can't add any more
..1:	MOV	M,C
	INX$	H
	MOV	M,B
	INX$	H
	MVI	M,KFF		;Add terminator
;
GCHRRT:	SHLD	RPTPTR
RPTAD3:	MOV	A,B		;Is this a function code?
	ORA	A		;Yes, return 'NZ'
	MOV	A,C		;Return AL = CL
	RET
	EPROC	RPTADD
	.PAGE
;
; MACSTA - Get status/char of keystroke macro.
;	   Retrn: 'Z' if empty.
;		  'NZ' if char ready, BC = char/code, ES:HL-> new KEYPTR value.
;
MACSTA:	LES	H,KEYPTR	;HL-> next key character
	MOV%ES	C,M
	INX$	H
	MOV%ES	B,M		;BC = next key char
	INX$	H
	MVI	A,KFF		;Get terminator
	CMP	C		;Reached terminator?
	RET			;Yes, 'Z' - macro is empty
;
; GETSTA - Checks if keyboard buffer or keyboard has
;	   character ready.  Returns 'NZ' if ready.
;
				;{GETCHR,PURGEK,ATTLST,VLOOP}
GETSTA:
	IF	POLLING, [
	CALL	KYPOLL		;Poll for a char
	]
;
	CALL	RPTSTA		;Does [REPEAT] have char ready?
	RNZ			;Yes return 'NZ'
;
	CALL	MACSTA		;Does keyboard macro have char ready?
	RNZ			;Yes, return 'NZ'
;
	CALL	POLSTA		;Does buffer have char?
	RNZ			;Yes, return 'NZ'
;
	IFNOT	POLLING, [
	CALL	CONST		;No, check keyboard
	]
	RET
;
; RPTRST - Clear Repeat count and string
;
				;{VDONE,VRPCAN,CANCEL}
RPTRST:	CALL	SV%ALL
	CLR$	RPTFLG		;Clear flag
	MVIW$	RPTCNT,0000	;Clear count
	RET
;
; MACRST - Clean out keystroke macro buffer by setting KEYPTR->KFFCHR.
;
				;{BEGIN,YLCMD,GETRNM,VFLQUT}
MACRST:	MVI	A,REGKEY	;Register number for KEYTBL
	CALL	PNTRG0		;ES:DE-> begin of KEYTBL, HL-> past end
	XCHG			;ES:HL-> KEYTBL
	MVI%ES	M,KFF		;Make sure there is a KFF
	STES	KEYPTR,H	;Disable keystroke macros
	RET
;
; POLRST - Reset type-ahead buffer.
;
				;{INIT,YLCMD}
POLRST:	LXI	H,KEYBUF	;Initialize keyboard pointers
	SHLD	KBFGET
	SHLD	KBFPUT
	RET
;
; PURGEK - Purge any pending or buffered keystrokes
;
				;{DEFINE,ERRCON,BREAK,VSMENU}
PURGEK:	CALL	GETSTA		;Is a char waiting?
	RZ			;No, return
	CALL	GETCHR		;Yes, get and ignore it
	JMPR	PURGEK		;Keep purging
	.PAGE
;
; GETKEY - Same as GETCHR, except disable visual mode sequences.
;	   Retrn: Simple char in C and A.
;		  'C' if CNCLFL set.
;
				;{LITCHR,VRPEXE,STAKEY,ATTLST,XKCMD}
GETKEY:	LDA	INPFLG
	PUSHA
	ANI	7FH
	STA	INPFLG
	CALL	GETCHR		;Get next char
	POPA
	STA	INPFLG
	TST$	CNCLFL		;User press [CANCEL]?
	MOV	A,C		;Return simple char in A
	STC			;Assume Yes
	RNZ			;Yes, return 'C'
	ORA	A		;No, retunr 'NC'
	RET
;
;
; GETCHR - Get the next keyboard character/sequence, first checking the REPEAT
;	   Buffer, then the keystroke macro buffer, then the polling buffer
;	   and finally waiting for a character.
;	   Retrn: 'Z' -  simple char in A and C; B = 00.
;		  'NZ' - function code in BC; A = C.
;
				;{GETKEY,KEYDEC,GETSTR,GETRNM,VSHEL2}
	BPROC	GETCHR
GETCHR:	CALL	GETSTA		;Is a character already waiting?
	JRNZ	..RPT		;Yes, go get it
;
	TST$	VISFLG		;Is this visual mode?
	JRNZ	..SKIP		;Yes, branch, don't mess with wierd stuff
;
;	First check if WINHOR has gone out of window
;
	LXI	H,HZSCEN	;HL-> past last virtual column
	LDA	WINHOR		;AL = current virtual column +1 ??
	CMP	M		;Within screen window?
	JRC	..0		;Yes, all is OK
	CALL	WINCR		;No, move cursor to left edge
	CALL	WINLF		;And go to/scroll  next window line
..0:
	IF	WINDOO, [
	CALL	UPDVIS		;Update active visual mode window
	]
;
;	Check if status line needs update
;
	CALL	CHKBRD		;Update status line if needed
;
; 	Memory Mapped cursor is blinked while waiting for character.
;	IBM - Check for Caps/Num/Scroll lock at each cursor blink.
;
..SKIP:
	IF	IBMPC, [
;6	MOV	AX,00		;Low memory segment
;6	MOV	ES,AX		;ES-> low memory
;6	MOV	AL,EBP LOW418	;Get keyboard status
;6	AND	AL,70H		;Only look at LOCK bits
;6	CMP	KYSTSV,AL	;Did lock status change?
;6	BEQ	#1		;No, branch
;6	CALL	STBRFL		;Yes, force rewrite
;6	CALL	CHKBRD		;Rewrite status line (if possible)
	]			;<IF IBMPC>
;
..1:
	IF	MEMVRS, [
	CALL	CURSON		;Turn the cursor on
	CALL	..KDEL		;Keyboard check and delay
	CALL	CUROFF		;Turn cursor off
	CALL	..KDEL		;Keyboard check and delay
	JMPR	..SKIP		;Wait here
;
..KDEL:	LDA	BLNKRT		;Get the blink rate
	MOV	B,A		;Make blink rate the loop count
..2:	PUSH	B		;Save the count
	CALL	GETSTA		;Is char ready?
	POP	B		;Restore count
	JRNZ	..OUT		;Yes, branch
	MVI	A,10		;Get delay value
	CALL	DELAY		;Perform the delay (polls keyboard)
	DJNZ	..2		;Here for 10 * BLNKRT milliseconds
	RET
;
..OUT:	POP	B		;Ignore return to GETCHR
	CALL	CUROF1		;Turn cursor off
	]			;<IF MEMVRS>
;
;	Now we are ready to get the next character, check for [REPEAT] first.
;
..RPT:	CALL	RPTSTA		;Does [REPEAT] have char?
	JNZ	GCHRRT		;Yes, save new RPTPTR, set return codes
;
;	GETCHR() must return through RPTADD()
;
..3:	LXI	H,RPTADD
	PUSH	H

..4:	CALL	MACSTA		;Does keystroke macro have char ready?
	JRZ	..5		;No, try polling buffer
				;Yes, BC = char, ES:HL = new PTR
	STES	KEYPTR,H	;Save new PTR
	RET			;Return BC = char/function code
;
..5:	CALL	POLSTA		;Does buffer have char?
	JRZ	..7		;No, get key from console
				;Yes BC = char/PTR
	SHLD	KBFGET		;Save new Get PTR
	MOV	H,B
	MOV	L,C		;HL-> code-string
	MOV	A,B		;Get char/PTR flag
	ORA	A		;Is this a PTR?
..6:	RZ			;No, C = char, return through RPTADD()
	SHLD	KEYPTR		;Yes, save to setup keystroke macro
				;ES:KEYPTR always -> KEYTBL
	JMPR	..4		;Merge above
;
..7:	CALL	DECKEY		;Get and decode keyboard sequence
	JRNC	..6		;Merge above if valid
	RET			;Invalid, return "VN" (thru RPTADD)
	EPROC	GETCHR
	.PAGE
;
; DECKEY -  Get control sequence from keyboard and decode it.
;	    Also check for [CANCEL] code by setting CNCLFL.
;	    Retrn: Simple char:   'Z',  B = 0, C = char; HL = 0000
;		   Function code: 'NZ', ES:HL-> code-string, BC = first code
;		   Not found:	  'C',  BC = "VN", HL undefined.
;		   DECDAT[] contains entire sequence ending with KFF.
;
				;{KYPOLL,GETCHR,DEFINE,BRKCHK,VLOOP}
	BPROC	DECKEY
DECKEY:	MVIW$	DECPUT,DECDAT	;Init PUT Ptr to decode line buffer
;
;	If first char is not Function/ALT key, we just return it.
;
;6	CALL	CONIN		;AL = next keyboard char, AH = code
;6	CALL	KEYADD		;Store AL in DECDAT[0]
;6	OR	AH,AH		;Is this a Function/ALT key?
;6	JNZ	#2		;Yes, try to decode it
;6	CMP	AL,80H		;Is this an 8-bit char (but not FUNC/ALT)?
;6	BGE	#KEYC		;Yes, treat as displayable char
;6	JMP S	..2		;No, merge
;
..1:	CALL	CONIN		;A = next keyboard char
	CALL	KEYADD		;Add to DECDAT[]
..2:	LXI	H,DECDAT	;HL-> buffer to match
	CALL	KEYMAT		;Match a control sequence?
	LDA	INPFLG		;A = input decode flag
	JRC	..PART		;Branch if partial match
	JRZ	..MTCH		;Yes, ES:HL-> function code in Key-Table
;
;	No match, return char if Command mode.
;
	ORA	A		;Is this Command mode decode?
	JRP	..KEYC		;Yes, return C = keyboard char
;
;	Visual mode - if CTRL/Function key, check if to be entered verbatim
;
	LDA	DECDAT		;Get keyboard char
	CPI	DEL		;Is it DEL key or function key?
	BGE	..NOTM		;Yes, branch
	CPI	SPACE		;Is it a displayable char?
	BGE	..KEYC		;Yes, branch
..NOTM:	ANIB$	BIT7AL,4	;Unused CTRL/Function chars to be inserted?
	JRNZ	..KEYC		;Yes, treat as displayable char
;
;	Since keystring not found, return 'C' and 'Z'.
;
	LXI	B,'N'*256+'V'	;Return BC = "VN"
	STC
	RET			;Return 'C' and 'Z'
;
;	Partial match - check INPFLG for advice
;
..PART:	ORA	A		;Is this Visual mode decode?
	JRM	..1		;Yes, keep decoding it
;
;	It's a displayable char - check reverse switch.
;
..KEYC:	LDA	DECDAT		;No, Command Mode - get the single char
	MOV	C,A		;Save in C
	TST$	SWCCNV		;Is the reverse switch set?
	MOV	A,C		;Get char. back
	CNZ	REVCAS		;Yes, reverse upper and lower case
	MOV	C,A		;C = char
	XRA	A		;Get 00 and 'Z'
	MOV	B,A		;B = 00
	LXI	H,0000		;HL = 00, (used in PSHCHR via BRKCHK)
	RET			;Return: B = 00, C = char; 'Z', 'NC'
;
;	Matched a control sequence - test for [CANCEL]
;
..MTCH:	MOV%ES	C,M		;Get function code
	INX$	H
	MOV%ES	B,M		;BC = function code
	DCX$	H		;Restore HL
	LXI	D,'A'*256+'C'	;DE-> "CA"
	CALL	CMBCDE		;Did user type [CANCEL]?
;6	MOV	AL,DL		;Ensure AL != 00 if 'Z'
	BNE	..CHEK		;No, branch
	STA	CNCLFL		;Yes, set flag to CANCEL
;
;	If Command Mode and a function code, return DECDAT[0] instead.
;
..CHEK:	TST$	INPFLG		;Is visual function decode enabled?
	JRM	IO%NZ		;Yes, function code is OK
	MOV	A,B		;Get function code
	ORA	A		;Is this a function code?
	JRNZ	..KEYC		;Yes, return DECDTA[0] instead
IO%NZ:	ORI	1		;Return 'NZ' and 'NC'
	RET			;HL-> function code, BC = function code
	EPROC	DECKEY
;
; KEYADD -  Add char in AL to DECDAT[].  If overflow DECPUT is not
;	    incremented.  If overflow, adding char causes KEYMAT()
;	    to return no-match.
;
				;{DECKEY}
KEYADD:	LHLD	DECPUT
	MOV	M,A		;Save char
	INX$	H		;Bump PTR
	MVI	M,KFF		;Add terminator
	LXI	D,DECDAT+14	;DE = upper limit (+15 is OK)
	CALL	CMHLDE		;Reached upper limit?
	RNC			;Yes, return, leave DECPUT same
	SHLD	DECPUT		;No, save new PUT PTR
	RET
	.PAGE
;
; KEYMAT - Check for match between [HL] and entries in KEYTBL.
;	   Entry:  HL-> buffer containing key strokes.
;	   Return: 'C'  - partial match, need to check more keystrokes;
;		   'NZ' - does not match any entry;
;		   'Z'  - Match found &
;
;		   DWORD SEQADD -> control sequence
;		   ES:HL -> function code (code-string)
;
				;{DECKEY,DEFINE}
	BPROC	KEYMAT
KEYMAT:	SHLD	KEYSAV		;Save -> to buffer
	MVI	A,REGKEY	;Register number for KEYTBL
	CALL	PNTRG0		;ES:DE-> begin of KEYTBL, HL-> past end
	SHLD	KEYEND		;Save -> past end
	XCHG			;HL-> begin of KEYTBL table
	INX$	H		;Skip over initial KFF
	MOV%ES	A,M		;Get 2nd char in table
	CPI	CR		;Is table valid?
	JRNZ	IO%NZ		;No, return 'NC' and 'NZ'
;
..1:	LDED	KEYEND		;DE-> past end of KEYTBL
	CALL	CMHLDE		;Reached the end?
	JRNC	IO%NZ		;Yes, return 'NC' and 'NZ'
;
	LDED	KEYSAV		;+DE-> input buffer
	LDAX	D		;Get next keyboard char
	MOV	C,A		;Save in C
	CALL	CVCTRL		;Get next char out of ES:(HL)
				;Converts "^Z" to CTRL-Z
	CMP	C		;Match first char of this entry?
	BEQ	..3		;Yes, check rest of entry
;
;	Skip over to next entry
;
..2:	CALL	KEYNXT		;Scan past end of control-sequence
	CALL	KEYNXT		;Scan past end of code-string
	JMPR	..1		;HL-> begin of next control-sequence
;
;	Check remaining characters in key-sequence
;	Ignore UC/LC distinction if KEYUCF is set.
;
..3:	DCX$	H
	STES	SEQADD,H	;Save -> begin of control-sequence
	INX$	H
..4:	INX$	D		;DE-> next keyboard char
	TST$	KEYUCF		;Ignore UC/LC distinction?
	PUSHF
	LDAX	D		;Get next key char
	CNZ	CONVUC		;Yes, convert to UC
	MOV	B,A		;Save in B
	CALL	CVCTRL		;A = next table char
	MOV	C,A		;Save in C
	POPF
	MOV	A,C
	CNZ	CONVUC		;Yes, convert to UC
	CMP	B		;Matched entire keystring?
				;Note this includes KFF at end of each
	BEQ	..5		;Yes, branch
;
	MOV	A,B		;No match, get key char back
	DCX$	H		;Restore BX due to CVCTRL above
	CPI	KFF		;Reached end of keyboard chars?
	BNE	..2		;No, check next table entry
;
	ORI	1		;'NZ'
	STC			;'C' - partial match
	RET
;
..5:	CPI	KFF		;Matched entire keystring?
	JRNZ	..4		;No, keep matching
	RET			;Yes, return 'Z' and 'NC'
	EPROC	KEYMAT
;
; KEYNXT - Scan ES:HL past KFF of current string, i.e. ES:HL-> begin next string
;
KEYNXT:	MOV%ES	A,M		;Get current char
	INX$	H		;++ptr
	CPI	KFF		;Reached end?
	BNE	KEYNXT		;No, keep going
	RET
;
; CVCTRL - Convert control char in format "^Z" to control char.
;	   Enter: ES:(HL) -> char to get 
;	   RETRN: A = character, DE-> next char
;
CVCTRL:	MOV%ES	A,M		;Get next char
	INX$	H		;Bump PTR
	CPI	'^'		;Special control char lead-in?
	RNZ			;No, return
	MOV%ES	A,M		;Yes, get next char
	INX$	H		;Bump PTR
	CALL	CONVUC		;Convert to UC
	CPI	'^'		;A literal "^"?
	RZ			;Yes, return "^"
	SUI	40H		;No, convert to control char
	RET
	.PAGE
;
; KEYDEC - Returns in HL address of decoded routine.  Char in C.
;
				;{PROCKY,REPEAT}
	BPROC	KEYDEC
KEYDEC:	CALL	GETCHR		;Get keyboard char; 'Z' if displayable char
	LXI	H,VCHAR		;Assume displayable char
	RZ			;No, displayable char, C = char
;
;	Look through HLPDEC for address of desired routine.
;	BC = two letter routine code.
;
	LXI	H,HLPDEC-2	;HL-> table of routine address and codes
..2:	INX$	H		;Skip over function code
	INX$	H
	CALL	TSTWIN		;Reached end of table?
	BEQ	..3		;Yes, branch
	INX$	H		;Skip over address
	INX$	H
	CALL	CMINBC		;Reached desired routine code?
	BNE	..2		;No, loop
;
	DCX$	H		;Yes, backup to routine address
	DCX$	H
	JMP	MVINHL		;HL = routine address
;
;	Very strange if routine codes not found - return VNOOP.
;
..3:	LXI	H,VNOOP
	RET
	EPROC	KEYDEC
	.PAGE
;
; DEFINE - Get control-sequence and code-string for new keystroke macros
;
	BPROC	DEFINE
DEFINE:	LXI	H,STADON	;Make sure we close status line when done
	PUSH	H		;Put address on stack
	CLR$	DEFTMP		;;Clear flag - need confirmation
	LHLD	EDTRWF		;Use buffer free-space to build macro
	INX$	H		;Skip over EOF
	SHLD	LRNPUT		;Save PUT Ptr
	MVI	M,KFF		;Set terminator
	CALL	PURGEK		;;Make sure no keys pending
	LXI	H,LEPRMT	;HL-> prompt
	CALL	STAPRM		;Prompt on status line
;
;	Loop to receive control-sequence.
;
..1:	CALL	DECKEY		;Get user input, BC = keycode
	CLR$	CNCLFL		;;Clear CANCEL flag in case set
	LXI	H,'F'*256+'D'	;HL = "DF"
	CALL	CMHLBC		;Did user type [DEFINE]?
	BEQ	..4		;Yes, branch
	LXI	H,'S'*256+'B'	;HL = "BS"
	CALL	CMHLBC		;Did user type [BACKSPACE]?
	JZ	..ERR		;Yes, give error
;
;	Copy control-sequence from DECDAT to macro-build-buffer.
;
	LXI	H,DECDAT
..3:	MOV	A,M
	INX$	H
	CPI	KFF
	BEQ	..1
	CALL	LRNADD
	JMPR	..3
;
;	Check if control-sequence is valid.
;
..4:	LHLD	EDTRWF		;HL-> macro build buffer -1
	INX$	H
	MOV	A,M		;Get first char
	CPI	KFF		;Was anything entered?
	JZ	..ERR		;No, give error
;
	CPI	DEL		;A DEL or high bit char?
	BGE	..5		;Yes, valid
	CPI	SPACE		;A control char?
	BLT	..5		;Yes, valid
;
;	When first char is displayable, it must be only char.
;
	INX$	H		;HL-> second char
	MOV	A,M		;Get second char
	CPI	KFF		;Terminator?
	BNE	..ERR		;No, give error
;
	LXI	H,LEDCNF	;HL-> confirmation prompt
	CALL	STAKEY		;Give prompt; get reply, AL = UC
	CPI	'Y'		;
	RNZ			;Ignore if not "Y"
	STA	DEFTMP		;;Set flag that already confirmed
;
;	If already defined, delete old entry following confirmation
;
..5:	LHLD	EDTRWF		;HL-> macro buffer -1
	INX$	H
	CALL	KEYMAT		;Exactly match existing table entry?
				;Also sets DWORD SEQADD -> table entry to delete
	JRNZ	..55		;No branch
	CALL	LRNDEL		;Yes, delete the key table first
	RC			;Return if user abort
;
..55:	MVI	A,KFF
	CALL	LRNAD0		;Add terminator
;
;	Prompt for key string.
;
	LXI	H,LESTRG	;HL-> prompt
	CALL	STAPRM		;Prompt on status line
;
;	Loop to receive key string.
;
..6:	CALL	GETCHR		;Get user input; 'Z' if displayable char
	JRZ	..7		;Branch if displayable char
	LXI	H,'F'*256+'D'	;HL = "DF"
	CALL	CMHLBC		;Did user type [DEFINE]?
	BEQ	..8		;Yes, loop is done
	LXI	H,'S'*256+'B'	;HL = "BS"
	CALL	CMHLBC		;Did user type [BACKSPACE]?
	BEQ	..ERR		;Yes, give error
;
;	Display functions as \xx\; add to buffer
;
	MVI	A,'\'
	CALL	PCHARA
	MOV	A,C		;Get first char
	CALL	LRNADD		;Add to buffer
	MOV	A,B
	CALL	LRNADD		;Add 00 to buffer, don't echo
	MVI	A,'\'
	CALL	PCHARA
	JMPR	..6		;Loop
;
;	Add displayable chars to buffer
;
..7:	MOV	A,C		;Get first char
	CALL	LRNADD		;Add to buffer
	MOV	A,B
	CALL	LRNAD0		;Add 00 to buffer, don't echo
	JMPR	..6		;Loop
;
;	End-Of-Loop - Add macro buffer to key table.
;
..8:	CLR$	CNCLFL		;;Clear CANCEL flag in case set
	LHLD	LRNPUT		;HL-> terminator
	DCX$	H		;HL-> last string char
	MOV	A,M		;Get char
	CPI	KFF		;A NULL string?
	RZ			;Yes, return now
;
	INX$	H
	INX$	H		;HL-> past terminator
	LDED	EDTRWF		;DE-> begin of macro buffer -1
	INX$	D
;
;	Append to key-table text register
;
	MVIB	REGNUM,REGKEY	;Set register # to key-table
	STA	REGAPP		;Set to append to table
	JMP	BRKCPY		;Append the contents of macro buffer
;
;
;
..ERR:	LXI	H,LERRMS	;HL-> "Invalid sequence"
	JMP	STAKEY		;Prompt on status line
				;Wait for any keystroke
	EPROC	DEFINE
;
; LRNADD - Add char in A to define buffer and display on screen.
;	   Expand CTRL-Z to "^Z" and literal "^" to "^^".
;
	BPROC	LRNADD
LRNADD:	CALL	PCHARA
LRNAD0:	CALL	SV%BDH		;Save BC, DE and HL
	MOV	C,A
	LDED$	MIDBAS		;DE-> end of free-space
	DCX$	D
	DCX$	D		;DE-> upper limit
	LHLD	LRNPUT	
	CALL	CMHLDE		;Reached limit?
	BLT	..OK		;No, branch
	LXI	H,LERRMS	;HL-> error message
	JMP	MSGBRK		;Give error and BREAK
;	
..OK:	MOV	A,C
	CPI	CTRLZ		;Is this special EOF char?
	BNE	..1
	MVI	M,'^'		;Yes, convert to "^Z" notation
	INX$	H
	MVI	A,'Z'
	JMPR	..2
;
..1:	CPI	'^'		;Is this normal "^" char?
	BNE	..2
	MVI	M,'^'		;Yes, convert to "^^" notation
	INX$	H
	MVI	A,'^'
;
..2:	MOV	M,A
	INX$	H
	SHLD	LRNPUT
	MVI	M,KFF		;Set terminator
	RET			;Restore BC, DE and HL
	EPROC	LRNADD
;
; LRNDEL - Delete the key table entry.
;	   Entry: DWORD SEQADD -> begin of key entry.
;	   Retrn: 'C' if user abort, 'NC' if deleted.
;
	BPROC	LRNDEL
LRNDEL:	TST$	DEFTMP		;;Already confirmed?
	JRNZ	..1		;;Yes, branch
	LXI	H,LECONF	;HL-> confirmation prompt
	CALL	STAKEY		;Give prompt, get reply, AL = UC
	CPI	'Y'		;
	STC
	BNE	..RET		;Return 'C' to abort
;
;	Scan forwards to end of table entry.
;
..1:	LES	H,SEQADD	;ES:HL-> begin of control-sequence
	PUSH	H		;Save HL again
	CALL	KEYNXT		;Scan past end of control-sequence
	CALL	KEYNXT		;Scan past end of code-string
	POP	D		;DE-> begin of table entry
	CALL	SBHLDE		;BC = size of table entry
	XCHG			;HL-> table entry to delete
	MVI	A,REGKEY	;Register number for KEYTBL
	CALL	RGSHNK		;Shrink register to remove entry
	ORA	A		;'NC' for successful delete
..RET:	RET
	EPROC	LRNDEL
	.PAGE
;
;	Low level console routines.
;
;
; CONIN - Low level console input routine.  
;	  Strip 8th bit if not allowed on input.
;	  Return:  Char in A and C.
;
				;{VGTKEY}
CONIN:
	IF	P8086, [
	CALL	CONINA		;Get the console char in AL, code in AH
	] [
	IF	MEMVRS, [
	LXI	H,CONINA	;Get the console char in AL
	CALL	SYSOUT		;Back-select screen out/in
	] [
	CALL	CONINA		;Get the console char in AL, code in AH
	]
	]
	MOV	C,A		;Save char in C
	ANIB$	BIT7AL,1	;Is Bit 8 allowed on input?
	MOV	A,C		;Get char back in A
	RNZ			;Yes, return
	ANI	7FH		;No, strip Bit 8
	MOV	C,A
	RET
;
CONST:
	IFNOT	P8086, [
	IF	MEMVRS, [
	LXI	H,CONSTA	;Get the console status
	CALL	SYSOUT		;Back-select screen out/in
	] [
	CALL	CONSTA		;Call the BIOS routine
	]
	ORA	A		;Make sure 'Z' flag set if no char
	RET
	] [
	JMP	CONSTA		;Use the BIOS routine
	]
;
CONOUT:	PUSH	B		;Save regs
	PUSH	D
	PUSH	H
	IF	P8086, [
	CALL	CONOTA		;Console output
	] [
	IF	MEMVRS, [
	LXI	H,CONOTA	;Console output
	CALL	SYSOUT		;Back-select screen out/in
	] [
	CALL	CONOTA		;Console output
	]
	]
	POP	H
	POP	D
	POP	B
	RET
;
;
; SYSOUT - System output for Memory Mapped bank select
;	   Enter: HL = routine address to use
;
	IFNOT	P8086, [
	IF	MEMVRS, [
SYSOUT:	TST$	BANKFL		;;Is screen selected now?
	JZ	CALLHL		;;No, jump to I/O routine
;
	CALL	SCROUT		;;Bank select the screen OUT
	CALL	CALLHL		;;Call the routine
	JMP	SCRIN		;;Bank select the screen IN
	]			;;<IF MEMVRS>
	]			;;<IFNOT 8086>
	.PAGE
;
; LSTOUT - Output handler for Printer.  Performs paged output.
;	   Enter: Char in C
;	   Retrn: BC, DE and HL saved
;
	BPROC	LSTOUT
LSTOUT:	CALL	SV%BDH		;Save BC, DE and HL
	IFNOT	P8086, [
	IF	MEMVRS, [
;;	LXI	H,LST0		;Listing output
	CALL	SCROUT		;Back-select screen out
;;	RET
LST0:
	]
	]
	TST$	PPLCNT		;At very top of page?
	JRNZ	..2		;No, branch
	PUSH	B		;Yes, save char
	LDA	PPXPL		;A = physical lines/page
	STA	PPMRFL		;Set flag that left margin needed
	LXI	H,PPLNCN	;HL-> printed lines/page
	SUB	M		;A = "blank" lines per page
	JRNC	..1		;Branch if valid
	XRA	A		;Else set to zero
..1:	RAR			;Divide by two
	PUSHA
	ADD	M		;Compute last line # for text
	STA	PPLSTX		;Save it
	POPA
	CALL	PRBLNK		;Print %A blank lines
	LXI	H,PPLCNT	;HL-> line counter
	INR	M		;Adjust for real line number/ reset flag
				;Gets set to 00 at PEJECT
	POP	B		;Restore char
;
..2:	TST$	PPMRFL		;Is left margin needed?
	JRZ	..3		;No, branch
	CALL	PRLMAR		;Yes, print left margin
;
..3:	MOV	A,C		;Get char
	CPI	CTRLL		;Is it a FORM-FEED?
	BNE	..4		;No, branch
	TST$	PPFFFL		;<FF> used for new page?
	JRZ	..4		;No, branch
	JMPR	PEJECT		;Yes, do page eject
;
..4:	CALL	LSTCHR		;Print the char
	LDA	PPLSTX		;A = last line # for text
	LXI	H,PPLCNT	;HL-> current line #
	CMP	M		;Past bottom line?
	RNC			;No, return
				;Yes, eject page
	EPROC	LSTOUT
;
; PEJECT - Eject the current page
;
				;{LSTOUT^,PECMD}
	BPROC	PEJECT
PEJECT:
	IFNOT	P8086, [
	IF	MEMVRS, [
	CALL	SCROUT		;Back-select screen out
	]
	]
	TST$	MINUS		;Is this -PE?
	JRNZ	..2		;Yes, only reset line counter
;
	LDA	PPXPL		;A = physical lines/page
	LXI	H,PPLCNT	;HL-> current line #
	SUB	M		;A = remaining line on page
	JRC	..2		;Branch if already on next page
				;Happens when # printed = #/page
	INR	A		;Adjust for first line of next page
	MOV	C,A		;Save
;
	TST$	PPFFFL		;Send FF to printer?
	JRZ	..1		;No, branch
;
	MVI	C,CTRLL		;Yes get FF char
	CALL	LSTOTA		;Send to printer
	JMPR	..2		;Merge below
;
..1:	MOV	A,C		;A = # blank lines needed
	CALL	PRBLNK		;Print blank lines
..2:	CLR$	PPLCNT		;Init line counter/flag
	RET
	EPROC	PEJECT
;
PRBLNK:	ORA	A		;Is count zero?
	RZ			;Yes, return
	PUSHA			;Save count
	MVI	C,CR
	CALL	LSTCHR
	MVI	C,LF
	CALL	LSTCHR
	POPA
	DCR	A		;Decrement count
	JMPR	PRBLNK
;
	BPROC	PRLMAR
PRLMAR:	CLR$	PPMRFL		;Clear margin needed flag
	LDA	PPLFMR		;Get left margin
	ORA	A		;Is there one?
	RZ			;No, return
	MOV	B,A		;Put count in B
..1:	PUSH	B		;Save C = char, B = count
	MVI	C,SPACE		;
	CALL	LSTOTA		;Send to printer
	POP	B		;Restore count/char
	DJNZ	..1		;Loop
	RET			;C = original char
	EPROC	PRLMAR
;
	BPROC	LSTCHR
LSTCHR:	MOV	A,C
	CPI	LF
	BNE	..1
	LXI	H,PPLCNT
	INR	M
	STA	PPMRFL		;Set flag that left margin needed
..1:	JMP	LSTOTA
	EPROC	LSTCHR

	.XLIST
	IFNOT	P8086, [
	IF	LSIO ! LSCUST, [
	.LIST
	]
	.PAGE
;
;	These are the Console I/O entry points which allow direct BIOS,
;	CP/M 2.0 or MP/M type system calls.  If the run-time OPSYS is
;	MP/M, the MP/M system calls are always used.  Otherwise the type
;	of calls is determined by KBINSW, where 0 = BIOS, 1 = CP/M and
;	2 = MP/M.
;
; SETIO - Set up internal copy of Console I/O vectors.
;	  Normally use BIOS, unless MP/M - then use BDOS #6
;
	BPROC	SETIO
SETIO:
	IF	MPM, [
	CALL	MPMCHK		;Is MP/M running?
	LXI	H,MPMTBL	;For MP/M use MP/M I/O routines
	JRNZ	..1		;Yes, branch
	CPIB	KBINSW,2	;MP/M-like I/O selected?
	JRNC	..1		;Yes, branch
;
	DCR	A		;Use CP/M BDOS function #6?
	LXI	H,CPMTBL	;In case so
	JRZ	..1		;Yes, branch
	]
;
	LHLD	BASE + 0001	;No, use direct BIOS calls, HL-> Warm Boot
	INX$	H
	INX$	H
	INX$	H		;HL-> BIOS Console Status
..1:	LXI	D,CONSTA	;DE-> Internal I/O jump vectors
	JMP	MOVE12		;Set I/O vectors (12 bytes) and return
	EPROC	SETIO
;
	IF	MPM,[
MPMTBL:	JMP	MPMST		;This table is copied to the BIOS jump
	JMP	MPMIN		;vectors below for the MP/M version
	JMP	MPMOUT
	JMP	MPMLST
;
MPMST:	MVI	C,CONSIO	;Perform direct console I/O
	MVI	E,0FEH		;FE denotes console status
	JMP	BDOS		;Get input status
;
MPMIN:	MVI	C,CONSIO	;Perform direct console input
	MVI	E,0FFH		;FF denotes console input
	JMP	BDOS		;Get console char. when available
;
MPMOUT:	MOV	E,C		;Put the char. in E
	MVI	C,CONSIO	;Perform direct console I/O
	JMP	BDOS		;Send char. to console through CP/M
;
MPMLST:	MOV	E,C		;Put the char. in E
	MVI	C,5		;Perform BDOS listing
	JMP	BDOS		;Send char. to listing through CP/M
;
CPMSTB	==	MPMIN
CPMOUT	==	MPMOUT
CPMLST	==	MPMLST
;
CPMTBL:	JMP	CPMST		;This table is copied to the BIOS jump
	JMP	CPMIN		;vectors below for CPM system calls
	JMP	CPMOUT
	JMP	CPMLST
;
CPMST:	TST	XXCHAR		;Is an input char waiting?
	RNZ			;Yes, return 'NZ' and A = char
	CALL	CPMSTB		;No, perform status/input check
	ORA	A		;Set flags
	STA	XXCHAR		;Save input char if A != 0
	RET
;
XXCHAR:	DB	0		;The input char from BDOS #6 call
;
CPMIN:	CALL	CPMST
	JRZ	CPMIN
	LXI	H,XXCHAR
	MVI	M,0
	RET
	]
;
; These JUMPS go directly to BIOS, or to CP/M or MP/M routines above.
;
CONSTA:	JMP	.		;Console status
CONINA:	JMP	.		;Get console char
CONOTA:	JMP	.		;Send console char
LSTOTA:	JMP	.		;Send listing char

	.XLIST
	]			;<IFNOT P8086>
	IF	LSIO, [
	.LIST
	]
	.PAGE
;
; ATTLST - Attach list device and give prompt messages
;
	BPROC	ATTLST
ATTLST:
	IF	MPM, [
	CALL	MPMCHK		;Is this MP/M?
	JRZ	..3		;No, branch
;
	MVI	C,161		;Conditional attach list
	CALL	BDOSSV
	ORA	A		;Is the list device available?
	JRZ	..3		;Yes, branch
;
;	List device is not available
;
	LXI	H,MPMLMS	;HL-> "PRINTER BUSY ..."
	XRA	A		;Just display message
	CALL	MSGHND		;Display message
;
..1:	CALL	GETSTA		;Has user typed a key?
	CNZ	GETKEY		;Yes, get it
	CPI	3		;Is it CTRL-C?
	JZ	BREAK		;Yes, give BREAK
;
	LXI	D,60		;No, setup to delay 1 second
	MVI	C,141		;Delay
	CALL	BDOSSV
;
	MVI	C,161		;Conditional attach list
	CALL	BDOSSV
	ORA	A		;Is the list device available?
	JRNZ	..1		;No, stay in this loop
	]			;<IF MPM>
;
..3:	LXI	H,PRTUMS	;HL-> "PRINTING ..."
	XRA	A
	CALL	MSGHND		;Display message
;
;6	IF	MEMVRS AND IBMPC
;6	CALL	HCRSON		;Turn on hardware cursor (we want life)
;6	ENDIF
	RET
	EPROC	ATTLST
