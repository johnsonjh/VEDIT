	.PAGE
	.TITLE	'VEDIT-T3'
;********************************************************
;*							*
;*	CRT Terminal Dependent Routines and Tables	*
;*							*
;********************************************************
;
; Copyright (C) 1987 by CompuView Products, Inc.
;			1955 Pauline Blvd.
;			Ann Arbor, MI 48103
;
;	Written by: Theodore J. Green
;
;	Last Change: Ted - Feb. 19 - Windows
;			   Mar. 06 - VSTART and VEXIT only from BEGIN
;			   Mar. 26 - Conditional assembly for windows
;			   Oct. 10 - Adapt to new WINSET
;			   Oct. 27 - Changes to UPDONE()
;			   Dec. 19 - 8086 Debugging
;			   Apr. 11 - Clean up
;			   Apr. 23 - WINEOL bug fix
;
	DSEG	$
ATTRFL:	DB	00		;Current attribute of CRT terminal
UPDGET:	DS	1		;Flag used at UPDCHR()
;
;	These are the terminal dependent Escape sequence tables.
;	They are set during customization by selecting a CRT in the menu.
;
; ADDLED, ADDMID and ADDEND are for Cursor Addressing.
;
	IFNOT	DEBUG, [	;Distribution is Zenith Z19
;
ADDLED:	DB	2		;Count for Cursor Lead In
	DB	01BH		;First byte of sequence
	DB	059H		;Second byte
	DB	00,00,00,00	;Spare bytes
;
ADDMID:	DB	0		;Count for chars between X & Y
	DB	00		;Note: some require a "," between the X & Y
	DB	00
	DB	00,00,00,00	;Spare bytes
;
ADDEND:	DB	0		;Count for terminate chars
	DB	00
	DB	00
	DB	00,00,00,00	;Spare bytes
;
; ADDOFF - Table for Cursor addressing information.  Specifies whether
;	   the address is sent in binary or Ascii and whether the row
;	   or column byte is sent first, and the offset to add to the
;	   row and column addresses.
;
ADDOFF:	DB	0		;Bit 0 : 0 = ROW first then COLUMN
				;	 1 = COLUMN first then ROW
				;Bit 7 : 0 = Address in Binary
				;	 1 = Address in Ascii
	DB	020H		;First offset
	DB	020H		;Second offset
;
;	Escape sequence table for other functions.
;
ESCCLR:	DB	00		;Count for CLEAR sequence
	DB	00H		;First byte
	DB	00H		;Second byte
	DB	00,00,00,00	;Spare bytes
;
ESCEOS:	DB	2		;Count for EOS sequence
	DB	01BH
	DB	04AH
	DB	00,00,00,00	;Spare bytes
;
ESCEOL:	DB	2		;Count for EOL sequence
	DB	01BH
	DB	04BH
	DB	00,00,00,00	;Spare bytes
;
ESCINS:	DB	2		;Count for INSERT LINE sequence
	DB	01BH
	DB	04CH
	DB	00,00,00,00	;Spare bytes
;
ESCDEL:	DB	2		;Count for DELETE line sequence
	DB	01BH
	DB	04DH
	DB	00,00,00,00	;Spare bytes
;
ESCFSL:	DB	01		;Count for FORWARD SCROLL sequence
	DB	0AH
	DB	00H
	DB	00,00,00,00	;Spare bytes
;
ESCBSL:	DB	2		;Count for BACKWARD SCROLL sequence
	DB	01BH		;The "Insert Line" should work too
	DB	04CH
	DB	00,00,00,00	;Spare bytes
;
ESCVDR:	DB	02		;Count for BEGIN REVERSE VIDEO sequence
	DB	01BH
	DB	'P'
	DB	00,00,00,00	;Spare bytes
;
ESCVDN:	DB	02		;Count for END REVERSE VIDEO sequence
	DB	01BH
	DB	'Q'
	DB	00,00,00,00	;Spare bytes
	] [			;<IFNOT DEBUG>
;
	.IFE	DEBUG-2, [	;DEBUG == 2 is Ann Arbor Ambassador
;
ADDLED:	DB	2		;Count for Cursor Lead In
	DB	01BH		;First byte of sequence
	DB	05BH		;Second byte
	DB	00,00,00,00	;Spare bytes
;
ADDMID:	DB	1		;Count for chars between X & Y
	DB	3BH		;Note: some require a ";" between the X & Y
	DB	00
	DB	00,00,00,00	;Spare bytes
;
ADDEND:	DB	1		;Count for terminate chars
	DB	48H		;
	DB	00
	DB	00,00,00,00	;Spare bytes
;
ADDOFF:	DB	80H		;Bit 0 : 0 = ROW first then COLUMN
	DB	1		;First offset
	DB	1		;Second offset
;
;	Escape sequence table for other functions.
;
ESCCLR:	DB	00		;Count for CLEAR sequence
	DB	00H		;First byte
	DB	00H		;Second byte
	DB	00,00,00,00	;Spare bytes
;
ESCEOS:	DB	4		;Count for EOS sequence
	DB	ESC
	DB	5BH
	DC	'J'
	DB	08,00,00	;Spare bytes
;
ESCEOL:	DB	3		;Count for EOL sequence
	DB	ESC
	DB	5BH
	DC	'K'
	DB	00,00,00	;Spare bytes
;
ESCINS:	DB	3		;Count for INSERT LINE sequence
	DB	ESC
	DB	5BH
	DC	'L'
	DB	00,00,00	;Spare bytes
;
ESCDEL:	DB	3		;Count for DELETE line sequence
	DB	ESC
	DB	5BH
	DC	'M'
	DB	00,00,00	;Spare bytes
;
ESCFSL:	DB	01		;Count for FORWARD SCROLL sequence
	DB	LF
	DB	00H
	DB	00,00,00,00	;Spare bytes
;
ESCBSL:	DB	2		;Count for BACKWARD SCROLL sequence
	DB	ESC		;The "Insert Line" should work too
	DC	'M'
	DB	00,00,00,00	;Spare bytes
;
ESCVDR:	DB	4		;Count for BEGIN REVERSE VIDEO sequence
	DB	ESC
	DB	5BH
	DC	'7m'
	DB	00,00		;Spare bytes
;
ESCVDN:	DB	4		;Count for END REVERSE VIDEO sequence
	DB	ESC
	DB	5BH
	DC	'0m'
	DB	00,00		;Spare bytes
	]			;<IFE DEBUG-2>
	.IFN	DEBUG-2, [	;DEBUG == 1 is IBM PC ANSI
;
ADDLED	DB	2		;Count for Cursor Lead In
	DB	01BH		;First byte of sequence
	DB	05BH		;Second byte
	DB	00,00,00,00	;Spare bytes
;
ADDMID	DB	1		;Count for chars between X & Y
	DB	03BH		;Note: some require a "," between the X & Y
	DB	00
	DB	00,00,00,00	;Spare bytes
;
ADDEND	DB	1		;Count for terminate chars
	DB	048H
	DB	00
	DB	00,00,00,00	;Spare bytes
;
ADDOFF	DB	80H		;Bit 0 : 0 = ROW first then COLUMN
	DB	01		;First offset
	DB	01		;Second offset
;
;	Escape sequence table for other functions.
;
ESCCLR	DB	84H		;Count for CLEAR sequence
	DB	1BH		;First byte
	DB	5BH		;Second byte
	DB	32H		;Third byte
	DB	4AH		;Fourth byte
	DB	00,00		;Spare bytes
;
ESCEOS	DB	0		;Count for EOS sequence
	DB	00
	DB	00
	DB	00,00,00,00	;Spare bytes
;
ESCEOL	DB	3		;Count for EOL sequence
	DB	01BH
	DB	05BH
	DB	04BH
	DB	00,00,00	;Spare bytes
;
ESCINS	DB	0		;Count for INSERT LINE sequence
	DB	00
	DB	00
	DB	00,00,00,00	;Spare bytes
;
ESCDEL	DB	0		;Count for DELETE line sequence
	DB	00
	DB	00
	DB	00,00,00,00	;Spare bytes
;
ESCFSL	DB	01		;Count for FORWARD SCROLL sequence
	DB	0AH
	DB	00H
	DB	00,00,00,00	;Spare bytes
;
ESCBSL	DB	0		;Count for BACKWARD SCROLL sequence
	DB	00		;The "Insert Line" should work too
	DB	00
	DB	00,00,00,00	;Spare bytes
;
ESCVDR	DB	04		;Count for BEGIN REVERSE VIDEO sequence
	DB	01BH
	DB	05BH
	DB	037H
	DB	06DH
	DB	00,00		;Spare bytes
;
ESCVDN	DB	04		;Count for END REVERSE VIDEO sequence
	DB	01BH
	DB	05BH
	DB	030H
	DB	06DH
	DB	00,00		;Spare bytes
	]
	]			;<IF DEBUG>
;
;
;
ESCSTE:	DB	00		;Count for ENABLE STATUS LINE sequence
	DB	00H
	DB	00H
	DB	00,00,00,00	;Spare bytes
;
ESCSTD:	DB	00		;Count for DISABLE STATUS LINE sequence
	DB	00H
	DB	00H
	DB	00,00,00,00	;Spare bytes
;
ESCENT:	DB	00		;Count for ENTER VISUAL MODE sequence
	DB	00H
	DB	00H
	DB	00,00,00,00	;Spare bytes
;
ESCEXT:	DB	00		;Count for EXIT VISUAL MODE sequence
	DB	00H
	DB	00H
	DB	00,00,00,00	;Spare bytes
;
	CSEG	$
	.PAGE
;
; OUTCHR - Output character to physical device.
;	   Entry: C = Char
;		  ATTRIB = Desired screen attribute; see OUTATT()
;
	BPROC	OUTCHA
OUTCHA:	MOV	C,A		;The char MUST be in C
OUTCHR:	CALL	OUTATT		;Set output attributes
	CALL	CRTCKK		;Poll for keyboard char. every 30th
	LXI	H,PHYHOR	;HL-> true CRT pos
	INR	M		;Account for CRT moving cursor to the right
;
OUTSAV:	CALL	CONOUT		;Send char. in reg. C to CRT
				;BC, DE and HL saved by CONOUT() and CHKKYB()
	JMP	CHKKYB		;Poll keyboard unless Fast-Poll disabled
	EPROC	OUTCHA
;
; OUTATT - Set screen attributes.
;
;	   ATTRIB = Desired attributes.
;	   ATTRFL = Current attributes
;
;		Bit 0	Reverse video
;		Bit 1	Underline	(not implememented yet)
;		Bit 2	Bold		("")
;
	BPROC	OUTATT
OUTATT:	LXI	H,ATTRFL	;HL-> current attributes
	LDA	ATTRIB		;A = desired attributes
	XRA	M		;Are they the same?
	RZ			;Yes, nothing to do
;
	PUSH	B		;Save BC
	MOV	C,A		;Save difference bits in C
	ANI	1		;Is the reverse video different?
	JRZ	..2		;No, branch
;
;	Switch R.V. state.
;
	MOV	A,M		;Get current value
	ANI	1		;Is reverse video currently on?
	LXI	H,ESCVDN	;Assume Yes, HL-> escape char. table
	JRNZ	..1		;Yes, turn it off
	LXI	H,ESCVDR	;No, HL-> escape char. table
..1:	CALL	CRTCRL		;Turn reverse video on/off
;
..2:	MOVB	ATTRFL,ATTRIB	;Save new screen attribute
	POP	B
	RET
	EPROC	OUTATT
;
; CRTCKK - Polls and buffers any keyboard character after
;	   every 30 bytes are sent to CRT.
;
CRTCKK:	LXI	H,CCOUNT	;HL-> slow poll counter
	INR	M		;Increment counter
	MOV	A,M		;Get the count
	CPI	30		;Reached critical point?
	RC			;No, return
	MVI	M,00		;Yes, reset count
	JMP	CHKKEY		;Poll and buffer any keyboard char
;
; PHYSET - Address the cursor.
;
;	   Enter:  L = vertical row #.  Top row = 1.
;	   	   H = horizontal column #.  Left column = 0.
;
;	   Return: HL, DE and BC saved
;
PHYST0:	MVI	H,00		;Set to column 00
	BPROC	PHYSET
PHYSET:	PUSH	B		;Save char. in C
	PUSH	D
	PUSH	H
	LD16	D,PHYVER	;Get current position
	CALL	CMHLDE		;Already at desired position?
	BEQ	..RET		;Yes, return now
;
	ST16	PHYVER,H	;Save physical cursor pos
	XCHG			;Save pos. in DE
	DCR	E		;Adjust so that top left is 0,0
	LXI	H,ADDLED	;HL-> cursor lead in table
	CALL	CRTCRL		;Send cursor address lead in
	LXI	H,ADDOFF	;HL-> cursor address info
	MOV	A,M		;Get column or line first switch
	ANI	1		;Is the line # sent first?
	JRZ	..1		;Yes, D & E are set
	MOV	A,D		;No, switch D & E
	MOV	D,E
	MOV	E,A
..1:	INX$	H		;HL-> first byte offset
	MOV	A,E		;Get first addr. coordinate
	ADD	M		;Add the offset
	PUSH	H		;Save HL-> ADDOFF
	CALL	WRTADD		;Send address to CRT, convert if necessary
	LXI	H,ADDMID	;HL-> middle char. table
	CALL	CRTCRL		;Send any chars between first and second byte
	POP	H		;Restore HL
	INX$	H		;HL-> second byte offset
	MOV	A,D		;Get second addr. coordinate
	ADD	M		;Add the offset
	CALL	WRTADD		;Send address to CRT, convert if necessary
;
	LXI	H,ADDEND	;HL-> cursor address end table
	CALL	CRTCRL		;Send any chars to end sequence
..RET:	POP	H
	POP	D
	POP	B		;Restore char in C
	RET
	EPROC	PHYSET
	.PAGE
;
; WRTADD - Send the cursor address in Reg. A to CRT and convert
;	   to Ascii if necessary.  HL and BC clobbered.
;
				;{PHYSET}
	BPROC	WRTADD
WRTADD:	MOV	C,A		;Save char. in C
	LXI	H,ADDOFF	;HL-> cursor address info
	MOV	A,M		;Get Binary or Ascii address switch
	ANI	80H		;Is address to be converted to Ascii?
	JZ	OUTSAV		;No, send address in C to CRT
;
	MOV	A,C		;Yes, get address in A
	LXI	B,03031H	;Get B = '0' is blanking flag, C = '1'
				;If your CRT needs leading zeroes, let B = 2F hex
	CPI	100		;Is pos. > 99?
	JRC	..1		;No, branch around
	SUI	100		;Yes, subtract the 100
	DCR	B		;Clear the blanking flag
	PUSHA			;Save count
	CALL	OUTSAV		;Send the '1'
	POPA			;Restore count
;
..1:	MVI	C,'0'		;Init tens counter to '0'
..2:	INR	C		;Tens = tens + 1
	SUI	10		;Subtract ten
	JRNC	..2		;Branch until underflow
;
	ADI	'0'+10		;Restore from underflow and make Ascii
	DCR	C		;Restore from underflow
	PUSHA			;Save Ones digit
	MOV	A,C		;Get the tens digit
	CMP	B		;Is tens count zero, and to be blanked?
	JRZ	..3		;Yes, don't send zero to CRT
	CALL	OUTSAV		;Send the tens count to CRT
;
..3:	POPA			;Restore Ones digit
	MOV	C,A		;Put digit in C
	JMP	OUTSAV		;Send the digit to the CRT. (Zero not blanked)
	EPROC	WRTADD
	.PAGE
;
; WISFWD - Scroll window forward.
;	   Visual Mode only calls WISFWD() when window is full screen width.
;
;	If INSERT-LINE not available, scroll entire screen with CR-LF.
;	Use INSERT/DELETE to scroll partial-screen windows.  If window is full
;	screen only use INSERT/DELETE if bit 80H of ESCINS is set.  This is
;	needed because some CRTs are very slow to use INSERT/DELETE.
;
;	If CR-LF is sent, sets flag to rewrite status line.
;	Checks and updates any visual mode windows affected by scroll.
;
				;{WTSCRL,WINLF}
	BPROC	WISFWD
WISFWD:	CALL	ATTBCK		;Make sure using erase attribute
	TST$	ESCINS		;Does CRT have insert line?
	JRM	..1		;Yes, use it whenever possible
	JRZ	..0		;No, scroll entire screen
;
	LDA	PYLINE		;AL = # screen lines
	DCR	A		;Account for status line
	CMPM	WWNLIN		;Is window full screen height?
	JRNZ	..1		;No, use INSERT/DELETE line
;
;	Scroll the entire screen by using CR-LF
;
..0:	LD16	H,PYLINE	;L = bottom line #
	CALL	PHYST0		;Address bottom screen line
	CALL	CRTFSL		;No, scroll entire screen forward
	CALL	STBRFL		;Flag to rewrite status line
	JMPR	..2		;Merge below
;
;	Scroll window with DELETE-Line & INSERT-Line
;
..1:	LD16	H,WWBGLN	;L = top line of window
	CALL	PHYST0		;Move physical cursor to top left corner
	CALL	CRTDEL		;Delete the line
	LD16	H,WWENLN	;L = bottom physical line of window
	CALL	PHYST0		;Address bottom window line
	CALL	CRTINS
;
;	Update side border for any windows which were scrolled.
;	Branch around if only one window on screen (may be due to ZOOM).
;
..2:
	IF	WINDOO, [
	TST$	WWZMFL		;Is window zoomed?
	JRNZ	WISFW5		;Yes, no border update needed
	CALL	MAXWIN		;ES:HL-> window structure
				;B = # windows, 'Z' if only one
	JRZ	WISFW5		;No border update needed if only one window
;
WISFW2:	PUSH	B		;Save count
;6	PUSH	ES
	PUSH	H		;Save -> structure
	LXI	D,5		;Offset
	DAD	D		;HL-> WWBGLN
	LD16	D,WWBGLN	;E = first line, D = last line that scrolled
	TST$	ESCINS		;Did entire screen scroll?
	JRNZ	..3		;No, branch, DE set
;
	LDA	PYLINE		;A = # physical lines
	MOV	D,A		;D = last line that scrolled
	MVI	E,1		;E = first line
;
..3:	INR	D		;Adjust
	MOV%ES	A,M		;A = .WWBGLN
	DCR	A		;Adjust to border line
	CMP	E		;Did border scroll?
	BLT	..4		;No, branch
	CMP	D		;Did border scroll?
	BLT	WISFW6		;Yes, have to rewrite entire border
;
..4:	MOV%ES	A,M		;Get .WWBGLN again
	CMP	D		;Is .WWBGLN below scrolled lines?
	BGE	WISFW4		;Yes, this window unaffected
	INX$	H		;HL-> .WWENLN
	MOV%ES	A,M		;Get .WWENLN
	CMP	E		;Entire window above scrolled lines?
	BLT	WISFW4		;Yes, this window unaffected
;
	INX$	H		;HL-> .WWBGCO
	MOV%ES	H,M		;H = beginning column
	DCR	H		;Adjust to border column
	JRM	WISFW4		;Branch if this is only horizontal window
;
;	Update border for this window
;
	MOV	L,D		;L = column for border
	DCR	L		;Account for INR D above
	CALL	PHYSET
	MVI	C,'|'
	CALL	OUTCHR		;Display char
WISFW3:	POP	H		;HL-> begin structure
;6	POP	ES		;ES:HL
;6	PUSH	ES
	PUSH	H		;Save again
	CALL	STUPF2		;Set flag to update visual mode window
;
WISFW4:	POP	H		;HL-> begin of structure
;6	POP	ES
	POP	B		;B = count
	LXI	D,WSTSIZ
	DAD	D		;HL-> next structure
	DJNZ	WISFW2		;Loop for all structures
	]			;<IF WINDOO>
;
WISFW5:	CALL	ATTFOR		;Reset to text attribute
	JMP	WINRST		;Reset cursor pos. in window
	EPROC	WISFWD
;
	IF	WINDOO, [
;
;	If a border scrolled, the border first must be erased
;	then a complete new one written
;
	BPROC	WISFW6
WISFW6:	MOV%ES	A,M		;A = .WWBGLN
	DCR	A
	DCR	A		;Line that border scrolled to
	CMP	E		;Did border get erased with CRTDEL()?
	BLT	..4		;Yes, don't need to erase it
;
	MOV	D,A		;D = .WWBGLN - 2
	INX$	H
	INX$	H		;HL-> .WWBGCO
	MOV%ES	C,M		;C = .WWBGCO
	INX$	H		;HL-> .WWENCO
	MOV%ES	A,M		;Get end column for window
	SUB	C		;A = # columns in window (-1)
	INR	A		;Adjust
	MOV	B,A		;Save in B
	MOV	L,D		;L = border line to be erased
	MOV	H,C		;H = first window column
	DCR	H		;H = border column
	JRP	..0		;Branch if border
	INR	H		;Adjust when no border
..0:	PUSH	B		;Save B
	CALL	PHYSET		;Position
	MOV	A,C		;Get .WWBGCO
	MVI	C,'|'		;Side border
	ORA	A		;Is there a side border?
	JRNZ	..1		;Yes, branch
	MVI	C,SPACE		;No, blank with SPACE
..1:	CALL	OUTCHR		;Yes, write border char
;
	POP	B		;B = remaining length of window
..2:	MVI	C,20H		;Clear old border
	CALL	OUTCHR
	DJNZ	..2
;
..4:	POP	H		;HL-> window structure
;6	POP	ES
;6	PUSH	ES
	PUSH	H		;Save it again
	CALL	WIBOR3		;Rewrite entire border
	JMPR	WISFW3
	EPROC	WISFW6
	]			;<IF WINDOO>
	.PAGE
;
; WISLBK - Scroll window backward.
;	   Will only scroll backward if this window is full width.
;	   Otherwide might screw up command mode window.
;	   Retrn: 'C' if window cannot be scrolled backward.
;		  Cursor position changed.
;		  WWUSLN updated.
;
WISLBK:	CALL	ATTBCK		;Make sure using erase attribute
	TST$	ESCINS		;Does CRT's have reverse scroll?
	STC
	RZ			;No, return 'C', can't do it
	CALL	WILINC		;Is window full width?
	RC			;No, can't do it
	LD16	H,WWENLN	;L = bottom physical line of window
	CALL	PHYST0		;Address bottom window line
	CALL	CRTDEL		;Delete the bottom line
	CALL	WINHOM		;Home in window
	CALL	CRTINS		;Insert a blank line
	LXI	H,WWUSLN	;HL-> # used window lines
	INR	M		;Increment
	LDA	WWNLIN		;A = # lines in window
	CMP	M		;Is WWUSLN now past max?
	RNC			;No, return 'NC'
	DCR	M		;Yes, adjust back
	XRA	A		;Return 'NC'
	RET
	.PAGE
	IF	WINDOO, [
;
; UPDONE - Show cursor position with reverse video character
;
				;{VEND}
UPDONE:	LDA	LVLVTX
	LXI	H,UPDSAV
	CALL	SETOUT
;
	CALL	CHKCUR		;Update cursor pos
	MOVW	UPDCUR,WINVER	;Save cursor pos
	LDAM	ATTRIB		;Get current attribute
	XRI	1		;Switch reverse video
	MOV	M,A		;Save new ATTRIB
	LHLD	EDTPTR		;HL = edit PTR
	CALL	UPDCHR		;A = displayed char
	MOV	C,A		;Put in C
	CALL	OUTCHR		;Display reverse video char on screen
	CALL	ATTFOR		;Back to foreground video
	CALL	RSTOUT		;Restore ROUTAD and LSTFLG
	RET			;Nice place for breakpoint
;
	BPROC	UPDCHR
UPDCHR:	CLR$	UPDGET		;Clear flag
	MOV	A,M		;Get char at (EDTPTR)
	CALL	CMPCLE		;Is it CR, LF or EOF?
	BNE	..1		;No, branch
	MVI	A,SPACE		;Yes, display a space
..1:	MOV	C,A
	CALL	PRTCHA		;Char will be rerouted via UPDSAV()
	LD8	UPDSVC		;Get char
	RET
	EPROC	UPDCHR
;
UPDSAV:	TSTM	UPDGET		;Already get first char?
	MVI	M,1		;Set flag TRUE
	RNZ			;Yes, just return
	MOV	A,C		;No, get char
	ST8	UPDSVC		;Save it
	RET
;
				;{VEDCMD,WINOUT}
UPDRTC: TSTW	UPDCUR		;Is old cursor on screen?
	RZ			;No, return
	CALL	WINSET		;Yes, position on screen
	LD8	UPDSVC		;A = character there
	MOV	C,A		;Put char into C
	CALL	OUTCHR		;Restore the character to screen
;
				;{CLRWIN}
UPDRST:	MVIW$	UPDCUR,0000	;Clear position/flag
	RET
	]			;<IF WINDOO>
	.PAGE
;
;	Clear Screen/window Routines.
;
; CLRSCR - Clear entire window.  First try to use EOS sequence.
;
				;{PRTFF,YWCLOS,WININI}
CLRSCR:
CLRWIN:	CALL	UPDRST		;Flag that update cursor gone
	MOVB	WWUSLN,WWNLIN	;Ensure all lines are cleared
	CALL	WINHOM		;Home cursor
	TST$	ESCEOS		;Is EOS function available?
	JRNZ	WINEOS		;Yes, use it
;
;	Else can use screen-clear if window is full screen.
;
	CALL	WFULLC		;Is window full screen?
	JRC	WINEOS		;No, let WINEOS() figure it out
;
	CALL	ATTBCK		;Yes, be sure using erase attribute
	LXI	H,ESCCLR	;HL-> escape char. table
	MOV	A,M		;Get count for Clear sequence
	ORA	A		;Is this function available?
	JRZ	WINEOS		;No, use window EOS
;
	CALL	STBRFL		;Flag to rewrite status line
	XRA	A		;New value for WWUSLN
	JMPR	WIEOS1		;Perform clear
;
; CLRSC2 - Clear rest of window from begin of next line # in Reg. A.
;
				;{UPDSCR}
CLRSC2:	LXI	H,WWUSLN	;HL-> # screen lines not clear
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
; WINEOS - Clear to end of Window from cursor position.
;
WINEOS:	CALL	ATTBCK		;Be sure using erase attribute
	CALL	SAVCUR		;Save cursor position
;
	CALL	WIEOSC		;Can physical EOS be performed?
	JRC	WIEOS2		;No, perform using multiple EOL
;
	LXI	H,ESCEOS	;HL-> escape char. table
	MOV	A,M		;Get count for EOS sequence
	ORA	A		;Does CRT have EOS sequence?
	JRZ	WIEOS2		;No, simulate EOS with multiple EOLs
	LXI	H,ESCEOS	;HL-> escape char. table
	LDA	WINVER		;Yes, get line number in window
;
				;{CLRWIN}
WIEOS1:	STA	WWUSLN		;Update # used lines
	CALL	CRTCRL		;Perform the screen clear
	JMP	ATTFOR		;Restore text attribute
;
;	Simulate an EOS using multiple EOLs.
;
WIEOS2:	LDA	WWUSLN		;A = last line that needs clearing
	LXI	H,WINVER	;HL-> current line
	SUB	M		;Compute # lines to clear
	RC			;Do nothing if WWUSLN < WINVER
	INR	A		;Adjust
	MOV	B,A		;Save count in B
	MOV	A,M		;Get WINVER
	STA	WWUSLN		;Update WWUSLN for EOS
;
	LD16	H,WINVER		;HL = desired write pos
WIEOS3:	PUSH	B		;Save count in B
	CALL	WINSET		;Position the cursor
	CALL	WINEOL		;Erase the line
				;Note - first EOL may be partial line
	LD16	H,WINVER		;HL = previous pos
	INR	L		;Move to next line
	MVI	H,00		;Now start from left margin
	POP	B		;B = remaining count
	DJNZ	WIEOS3		;Loop
;
RESCUR:	LHLD	SAVWIN		;Restore to position saved in SAVCUR()
	JMP	WINSET
;
SAVCUR:	LD16	H,WINVER
	SHLD	SAVWIN
	RET
;
; WINEOL - Clear to end of Window Line from cursor position.
;
	BPROC	WINEOL
WINEOL:	LXI	H,ATTFOR	;;Routine to restore text attribute
	PUSH	H		;;Make sure text attribute restored
	CALL	ATTBCK		;;Be sure using erase attribute
	CALL	WIEOLC		;Can physical EOL be performed?
	JRC	..1		;No, branch
	LXI	H,ESCEOL	;HL-> escape char. table
	MOV	A,M		;Get count for EOL sequence
	ORA	A		;Does CRT have EOL sequence?
	JNZ	CRTCRL		;Yes, perform the line clear
;
;	Simulate EOL by sending out spaces to end of window line
;
..1:	LDA	WWENCO		;Get last physical column for window
	LXI	H,PHYHOR	;HL-> current physical column
	SUB	M		;Compute # pos. to clear
	JC	WINRST		;DON'T if CRTHOR past end of window
	INR	A		;Adjust
	MOV	B,A		;Put count in B
	MVI	C,20H		;Clear with spaces
..2:	JZ	PHYRST		;When done, reset cursor pos
	CALL	OUTSAV		;Send a space to the CRT
	DCR	B		;Decrement count
	JMPR	..2		;Continue
	EPROC	WINEOL
;
PHYRST:	LD16	D,PHYVER	;Get desired cursor pos
	LXI	H,-1		;Make sure cursor gets positioned
	SHLD	PHYVER		;PASM translation problems with MVIW$
	XCHG			;HL = desired pos
	JMP	PHYSET		;Reset the cursor
;
; WIEOSC - Check if Physical EOS can be performed in current window.
;	   Retrn: 'NC' if EOS can be performed
;
WIEOSC:	CALL	WILINC		;Does window extend full screen width?
	RC			;No, return 'C'
	LDA	WWENLN		;Get last physical line for window
	LXI	H,PYLINE	;HL-> last physical line for screen
	CMP	M		;Is bottom of window at bottom of screen
	RET			;Yes, return 'NC' that EOL ok
;
; WFULLC - Check if window is full screen (except for status line).
;	   Retrn: 'NC' if window is full screen.
;
WFULLC:	CALL	WILINC		;Does window extend full screen width?
	RC			;No, return 'C'
	LDA	WWNLIN		;A = # lines in window
	INR	A		;Adjust for status line
	LXI	H,PYLINE	;HL-> # screen lines
	CMP	M		;Window full screen?
	STC
	RNZ			;No, return 'C'
	ORA	A
	RET			;Yes, return 'NC'
	.PAGE
;
; CRTCRL - Send escape sequence to CRT and perform any necessary delay.
;	   Enter: HL-> escape sequence table, consisting of escape
;	   sequence count byte, followed by the bytes making up the
;	   sequence, followed by any delay in milliseconds.
;
	BPROC	CRTCRL
CRTCRL:	MOV	A,M		;Get the char. count
	ANI	7FH		;Strip EMULATE bit
	ORA	A		;Test for initial zero
	RZ			;Yes, return now
	CALL	CHKKYB		;Poll keyboard
	PUSH	B		;Save BC
	MOV	B,A		;Put the count in B
..1:	INX$	H		;HL-> escape char
	MOV	C,M		;Get the next char
	CALL	OUTSAV		;Send to CRT, HL is saved
	DJNZ	..1		;Continue sending bytes
;
	POP	B		;Restore BC
	INX$	H		;HL-> delay byte
	MOV	A,M		;Get the delay in milliseconds
	ORA	A		;Is there a delay?
	RZ			;No, return now
	JMP	DELAY		;Yes, do a delay
	EPROC	CRTCRL
;
;
; CRTINS, CRTDEL - Insert and delete a line on the CRT.
;	Note: Some CRTs need to be in a special mode for Insert and Delete.
;
CRTINS:	LXI	H,ESCINS	;HL-> escape char. table
	JMP	CRTCRL		;Insert a line
;
CRTDEL:	LXI	H,ESCDEL	;HL-> escape char. table
	JMP	CRTCRL		;Delete a line
;
; CRTFSL, CRTBSL - Perform forward and backward scroll on CRT.
;
CRTFSL:	LXI	H,ESCFSL	;HL-> escape char. table
	JMP	CRTCRL		;Scroll forward
;
CRTBSL:	LXI	H,ESCBSL	;HL-> escape char. table
	JMP	CRTCRL		;Scroll backward
;
; CRTSTE, CRTSTD - Enable and disable the 25th status line.
;
CRTSTE:	LXI	H,ESCSTE	;HL-> escape char. table
	JMP	CRTCRL		;Enable status line
;
CRTSTD:	LXI	H,ESCSTD	;HL-> escape char. table
	JMP	CRTCRL		;Disable status line
;
; CRTENT, CRTEXT - Setup and Reset the CRT for Visual mode editing.
;
				;{VSHEL2,VSTART}
CRTENT:	LXI	H,ESCENT	;HL-> escape char. table
	JMP	CRTCRL		;Setup CRT on Visual mode entry
;
CRTEXT:	LXI	H,ESCEXT	;HL-> escape char. table
	JMP	CRTCRL		;Reset CRT on visual mode exit
;
	.PAGE
;
; VSTART - Initialize terminal for VEDIT use.
;
	BPROC	VSTART
VSTART:	LDA	ESCCLR		;Get count for Clear sequence
	ORA	A		;Emulating a CRT?
	JP	..1		;No, branch
	MVIM%CS	CHKKYB,RETINS	;Yes, turn off fast polling
;
..1:	CALL	CRTENT		;Send init sequences to CRT
	CPIB$	PYLINE,25	;Are there at least 25 lines?
	BGE	..2		;Yes, branch
	CLR	ESCSTE		;No, don't use any status line
	STA	ESCSTD		;Clear status line disable count too
..2:	JMP	CRTSTE		;Enable 25th status line if there is one
	EPROC	VSTART
;
; VEXIT - Get ready to exit editor.  Screen may be cleared.
;
	BPROC	VEXIT
VEXIT:	CALL	CRTEXT		;Reset CRT with escape sequences
	CALL	CRTSTD		;Disable status line if there is one
	CALL	ATTBCK		;Make sure exit in normal video
;
	LD16	H,PYLINE	;L = last screen line
	JMP	PHYST0		;Position cursor for clean exit
	EPROC	VEXIT
;
; KDISPY - Return A = 0 for CRT display.
;
	PUBLIC	KDISPY
				;{WDGET}
KDISPY:	XRA	A		;0 for CRT's
	RET
;
;	IF	P8086, [
	PUBLIC	HCRSOF
HCRSOF:	RET			;Fewer modules need re-assembly
;	]
