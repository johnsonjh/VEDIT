	.PAGE
	.TITLE	'VEDIT-W1'
;****************************************
;*					*
;*	Word Processing Functions	*
;*					*
;****************************************
;
; Copyright (C) 1986 by CompuView Products, Inc.
;			1955 Pauline Blvd.
;			Ann Arbor, MI 48103
;
;	Written by: Theodore J. Green
;
;	Last Change: Ted - Feb. 08, 1985 - Allow double spaces
;		     Ted - July 29, 1985 - JUSTY
;			   Aug. 05, 1985 - Changes for YF
;			   Oct. 03, 1985 - JUSTY
;			   Nov. 06, 1986 - Change use of SETLOG/SETLGA
;			   Nov. 16 - Fix EOF check bug at WRDSPN
;
;	Variables used to Format/Justify Paragraphs.
;
	DSEG	$
	PUBLIC	FRMGET, FRMEND, FRMSAV, FRMLBG, FRMPOS, FRMIND
	PUBLIC	GRPCNT, JUSDIR
FRMGET:	DSW	1		;Current Get PTR
FRMEND:	DSW	1		;Pointer to end of Para. (FRMGET above)
FRMSAV:	DSW	1		;-> to next para. (FRMEND above)
FRMLBG:	DSW	1		;Get pointer for line begin
FRMPOS:	DSW	1		;Logical pos. on formatted line
FRMIND:	DSW	1		;Additional indent for first line
GRPCNT:	DSW	1
JUSDIR:	DS	1
	CSEG	$
;
; PRWORD - Return: HL -> first char. of current or previous word.
;		'NZ' normally, or 'Z' if CR, LF or EOF found.
;		'C' if CR, LF or EOF found before word.
;
;	Span over non-word characters.
;
				;{CRPVWD,DLPVWD}
	BPROC	PRWORD
PRWORD:	CALL	FRCSET		;Make sure cursor positioned correctly
				;Needed in case begin of line encountered
	LHLD	ACTPNT		;Get edit PTR
;
;	First scan backwards over all non-word characters
;
				;{WRAPWD}
PRWOR2:	DCX$	H		;HL-> previous char
	CALL	VALCHR		;'Z' if valid char., 'C' if CR/LF/EOF
	RC			;'C' - word not reached yet
	BNE	PRWOR2		;No, span until word found
;
;	Now look for begin of current word.
;
..4:	DCX$	H		;Bump PTR
	CALL	WRDSPN		;Is it valid char. in words?
				;Could check here for [ { or (
				;and return 'NZ' with current HL
	BEQ	..4		;Yes, keep looking
	INX$	H		;No, point to begin of word
	ORA	H		;Make sure 'NZ' and 'NC' set
	RET			;HL-> delimiter to use
	EPROC	PRWORD
;
; NXWORD - Return: HL -> first char. of next word.
;
				;{CRNXWD}
	BPROC	NXWORD
NXWORD:	LHLD	ACTPNT		;Get edit PTR
				;{DLNXWD}
NXWOR1:	DCX$	H		;Account for INX H
;
;	Span over word characters.
;
..2:	INX$	H		;Bump PTR
	CALL	WRDSPN		;Is it valid char. in words?
				;Could check here for [ { or (
				;and branch down with current HL
	BEQ	..2		;Yes, span until other char found
	DCX$	H		;Adjust to look at non-word char
;
;	Now look for begin of next word.
;
NXWOR3:	INX$	H		;Bump PTR
				;{CRNXWD}
NXWOR4:	CALL	VALCHR		;'Z' if valid char., 'C' if CR or LF
	RC			;'C' - word not reached yet
	BNE	NXWOR3		;No, keep looking
NXWOR5:	ORA	H		;Yes, make sure 'NZ' and 'NC' set
	RET			;HL-> begin of word
	EPROC	NXWORD
	.PAGE
;
; VALCHR - Get char. at (HL), check if valid char in words.
;	   Return: 'C' if CR/LF/EOF found, 'Z' if valid.
;
				;{PRWORD,NXWORD}
VALCHR:	MOV	A,M		;Get the char
	CALL	CMPCLE		;Is it line delimiter?
	STC			;Set 'C' - word not reached yet
	RZ			;Yes, return 'Z' and 'C'
;
; WRDSPN - Check char. at (HL) and return 'Z' if char. is allowed in words.
;	   Allow "," surrounded by digits as in "10,000"
;
				;{PRWORD,NXWORD,VALCHR above}
WRDSPN:	MOV	A,M		;Get the char
	CALL	LETCHK		;Is it a letter?
	RZ			;Yes, return 'Z'
	CPI	EOF		;;EOF?
	JZ	RET%NZ		;;Yes, return 'NZ'
				;;Test not needed if ALWTBL-EOT != EOF
	PUSH	D		;Save DE
	LXI	D,ALWTBL	;DE-> table of chars not allowed in words
	CALL	LOOKCH		;Is char in table?
	POP	D		;Restore DE
	JNZ	RET%Z		;No, return 'Z' - allowed char
	MVI	A,','		;Get comma
	CMP	M		;Is delimiter a comma?
	JRNZ	NXWOR5		;No, return 'NZ' and 'NC'
;
;	Check if previous and next chars are digits
;
	DCX$	H		;HL-> previous char
	MOV	A,M		;Get char
	INX$	H		;Restore
	CALL	DIGCHK		;Is it a digit?
	JRNZ	NXWOR5		;No, return 'NZ' and 'NC'
	INX$	H		;HL-> next char
	MOV	A,M		;Get char
	DCX$	H		;Restore
	CALL	DIGCHK		;Is it a digit?
	JRNZ	NXWOR5		;No, return 'NZ' and 'NC'
	RET			;Yes, return'Z'
	.PAGE
;
; WRAPWD - Find word wrap position scanning backwards. (2/08/85)
;	Enter:	HL-> current char.
;	Exit :	'C' if word too long or current and previous chars are
;		space.  This prevents next line from beginning with space.
;		'NC' if wrap point set at ACTPNT and BC = adjust + 1.
;		DE-> current char.  HL-> begin of word.
;
				;{ADDACT,FORMAT}
	BPROC	WRAPWD
WRAPWD:	MOV	D,H
	MOV	E,L		;Init DE
	DCX$	H		;HL-> previous char
	MOV	A,M		;Get previous char
	INX$	H		;HL-> current
	CALL	CMPBLK		;Is previous char Space or Tab?
	JRNZ	..1		;No, branch and use [PREV WORD] routine
	MOV	A,M		;Get current char
	CALL	CMPBLK		;Is this char also Space or Tab?
	STC			;Set 'C' flag
	RZ			;Yes, return 'C'
	JMPR	..2		;No, wrap at current char
;
..1:	PUSH	H		;Save original PTR
	CALL	PRWOR2		;HL-> first char of word
	XCHG			;DE-> ""
	CALL	BLKSPN		;HL-> first non white-space on line
	CALL	CMHLDE		;Is this word too long?
	POP	H		;HL = original PTR
	CMC			;Change 'NC' to 'C'
	RC			;Return 'C' if word too long
;
..2:	CALL	SBHLDE		;BC = adjust. Set 'NC'
	INX$	B		;BC = adjust + 1
	XCHG			;HL-> where to insert CR-LF
	RET
	EPROC	WRAPWD
	.PAGE
;
; CRPVWD - Move cursor to first letter of current or previous word.
;
CRPVWD:	CALL	PRWORD		;Find begin of current or previous word
	JRC	CRPVW1		;Branch if begin of line found before word
				;{CRNXWD}
J1CRPV:	SHLD	ACTPNT		;Save new edit pointer
	RET
;
CRPVW1:	LDED$	TXTBAS		;DE-> begin of text buffer
	DCX$	D		;Adjust
	DCX$	D
	LHLD	TXACTV		;HL-> begin of current line
	CALL	CMHLDE		;At first word of text buffer?
	RC			;Yes, DO NOTHING ***
	LXI	D,CRPVWD	;Set to reenter routine
	CALL	PUSHOP		;Add to operation stack
				;{DLPVWD}
CRPVW2:	MOVB	CURHOR,WWLLE1	;Move cursor to end of previous line
	CALL	STACPN		;Set edit point from cursor pos
	JMP	CRUP		;Move the cursor up too
;
; CRNXWD - Move cursor to first letter of next word.
;
CRNXWD:	CALL	NXWORD		;Find end of current or next word
CRNXW1:	JRC	CRNXW2		;Branch if end of line found before word
	JMPR	J1CRPV		;Save new edit pointer
;
CRNXW2:	CPI	EOF		;Found EOF?
	RZ			;Yes, return now!
	LXI	D,CRNXW4	;Set to reenter routine
	CALL	PUSHOP		;Add to operation stack
				;{DLNXWD}
CRNXW3:	MVI	A,1		;Set for column 1
	MOV	B,A		;Move down one line
	JMP	CRZIP4		;Branch to STACPN and move cursor down
;
;	Enter here after cursor is moved to next line.
;
CRNXW4:	LHLD	ACTPNT		;Get edit PTR
	CALL	NXWOR4		;Find next word
	JMPR	CRNXW1		;Merge above
	.PAGE
;
; DLPVWD - Delete Previous word.
;
DLPVWD:	CALL	PRWORD		;Find begin of current or previous word
	JRC	CRPVW2		;Branch if begin of line found before word
	XCHG			;DE-> first char of word
	CALL	STRWAC		;Set to rewrite active line
	LHLD	ACTPNT		;Get edit PTR
				;{DLNXWD}
DLPVW1:	CALL	SBHLDE		;BC = # chars to delete
	RZ			;Return if none (????)
	XCHG			;HL-> begin of previous word
	SHLD	ACTPNT		;Save new edit PTR
				;(No change for DLNXWD)
	JMP	DELAC1		;Delete the chars
;
; DLNXWD - Delete Next word.
;
;	First span over any initial non-word characters.
;
	BPROC	DLNXWD
DLNXWD:	LHLD	ACTPNT		;Get edit PTR
	DCX$	H		;Account for INX H
..1:	INX$	H		;Bump PTR
	CALL	VALCHR		;Is it valid char. in words?
	JRC	..3		;;Move to next line if end of line encountered
	BNE	..1		;No, keep looking
;
;	Next find begin of next word or line delimiter.
;
	CALL	NXWOR1		;HL-> begin of next word or CR/LF/EOF
	XCHG			;Save in DE
	LHLD	ACTPNT		;Get edit PTR
	DCX$	H		;HL-> previous char
	CALL	WRDSPN		;Were we at begin of word?
	INX	H		;+HL = ACTPNT
	XCHG			;HL = new position
	BNE	DLPVW1		;Yes, delete in-between chars including spaces

..2:	DCX$	H		;No, leave any following spaces
	CALL	WRDSPN		;Found end of last word?
	BNE	..2		;No, keep looking
	INX$	H		;Yes, adjust
	JMPR	DLPVW1
;
..3:	MOV	A,M		;;Get char back
	CPI	EOF		;;At end-of-file?
	RZ			;;Yes, do nothing
	JMPR	CRNXW3		;;No, move to next line
	EPROC	DLNXWD
	.PAGE
;
; PVPARA - Move cursor to previous paragraph.
;
PVPARA:	CALL	FRMVIS		;Save active line, set edit PTR
	CALL	SCANB1		;Find previous non-delimiter
	CALL	BGPARA		;HL-> begin of this para
JTOVIS:	JMP	TOVIS0		;Set new edit point, write new screen
;
; NXPARA - Move cursor to next paragraph.
;
NXPARA:	CALL	FRMVIS		;Save active line, set edit PTR
	CALL	ENPARA		;HL-> end (+1)  of this para
	JMPR	JTOVIS		;Set new edit point, write new screen
;
; BGPARA - Return: HL -> first char of current paragraph.
;
;
				;{PVPARA,FORMAT}
BGPARA:	CALL	PREVLF		;HL-> previous LF
	CALL	MINBGN		;Don't allow HL < TXTFLR
	RC			;Return if scanned beyond begin of text
	CALL	PARCHK		;Is this end of paragraph?
	RZ			;Yes, HL-> begin next para
	DCX$	H		;No, HL-> back to LF
	DCX$	H		;Setup HL for PREVLF
	JMPR	BGPARA		;Keep going back
;
; ENPARA - Return: HL-> first char of next paragraph.
;
				;{NXPARA,FORMAT}
ENPARA:	CALL	NEXTLF		;HL-> next LF
	RC			;Return HL-> EOF if EOF reached
;
	CALL	PARCHK		;Is this end of paragraph?
	JRNZ	ENPARA		;No, check next line
	RET			;Yes, HL-> begin of next para
;
; PARCHK - Check if following line is blank or text processor command.
;	   Return: 'NZ' if normal text encountered, HL not moved
;		   'Z' & 'C' if print format command, HL not moved
;		   'Z' % 'NC' if blank line, HL-> begin next non-blank line.
;
				;{BGPARA,ENPARA}
	BPROC	PARCHK
PARCHK:	INX$	H		;HL-> begin of line
				;{FORMAT}
PARCH0:	MOV	A,M		;Get char
	NOP			;CPI 080H, RNC - Return if typesetting char
	NOP
	NOP
	CALL	BLKLIN		;Is this line blank?
	BEQ	..2		;Yes, found blank line
;
	MOV	A,M		;Get char
	LXI	D,PARTB1	;DE-> word processor characters
	CALL	LOOKCH		;Is char in table?
	STC			;Assume Yes, return 'C'
	RET			;Yes, HL-> format command line
				;No, return 'NZ' to indicate normal char
;
;	Skip over all blank lines.
;
..1:	INX$	H		;Bump PTR
..2:	MOV	A,M		;Get char
	CPI	CR
	JRZ	..1		;Skip over CR and LF
	CPI	LF
	BEQ	..1		;Yes, keep looking
	CPI	EOF		;Find EOF?
	RZ			;Yes, return 'Z' & 'NC'
;
	CALL	BLKLIN		;Is this line blank?
	BEQ	..2		;Yes, continue looking
	XRA	A		;Set 'Z' and 'NC'
	RET
	EPROC	PARCHK
;
; BLKLIN - Check if HL-> blank line. 
;	   Return: 'Z' if blank line, HL -> end of line
;		   'NZ' if not blank line, HL not changed.
;
				;{PARCHK}
BLKLIN:	MOV	D,H		;Save -> line begin
	MOV	E,L
	CALL	BLKSP0		;A = first non-space, non-tab. HL-> char
	CALL	CMPCLE		;CR, LF, EOF?
	RZ			;Return 'Z'
;
	MOV	H,D		;HL-> line begin
	MOV	L,E
	RET
;
; SCANBK - Scan backward over spaces, tab, CR and LF.
;
SCANBK:	MOV	A,M		;Get next char
	CALL	CMPSTL		;Space, tab, CR or LF?
	RNZ			;HL-> other char
SCANB1:	DCX$	H		;Backup pointer
	JMPR	SCANBK		;Continue
;
CMPSTL:	CALL	CMPBLK		;Is char. space or Tab?
	RZ			;Yes, return 'Z'
CMPCL:	CPI	CR		;CR?
	RZ
	CPI	LF		;LF?
	RET
	.PAGE
;
; VFPARA - Visually reformat paragraph to within margins set. (7/25/85)
;
VFPARA:	CMPMB	INDPOS,WRAPCL	;Is Indent pos >= wrap column?
	RNC			;Yes, ignore command
;
	CALL	FRMVIS		;Save active line, set edit PTR
	CALL	FORMAT		;Format the paragraph, HL-> begin of next para
	JMP	TOVIS2		;Store HL as new edit pointer, redraw screen
;
; FORMAT - Reformat paragraph between [INDPOS,WRPCOL].  (7/25/85)
;	    Return HL-> start of next paragraph.
;
				;{VFPARA,YFCMD}
	BPROC	FORMAT
FORMAT:	CALL	BGPARA		;HL-> start of paragraph
..1:	MOV	A,M		;Get char
	CPI	EOF		;Reached EOF?
	RZ			;Yes, return (or infinite loop)
;
	CALL	PARCH0		;Is this a blank or format command line?
	JRNZ	..3		;No, branch HL-> usable beginning
	JRNC	..1		;Branch if a blank line, HL-> past blank lines
				;Stay in loop until real text line found
;
	CALL	NEXTLF		;Skip over text processor command line
	RC			;Return if EOF found
	INX$	H		;Bump HL to begin of next line
	JMPR	..1		;Check again
;
..2:	RET			;Room for breakpoint
;
..3:	PUSH	H		;Save -> begin of true paragraph
	CALL	ENPARA		;HL-> begin of next para
	SHLD	FRMSAV		;Save ending cursor pos
	CALL	SCANB1		;Ignore delimitors. (Watch out for EOF!!)
	INX$	H		;Adjust
	SHLD	FRMEND		;Save -> past end of para
	XCHG			;DE-> end of para
	POP	H		;HL-> begin of para
	CALL	CMHLDE		;Is Begin PTR >= End PTR ?
	BGE	..2		;Yes, abort
	SHLD	FRMGET		;Save HL = Get PTR
;
;	Compute how much first line is indented compared to second,
;	or how much 2nd line is offset from first
;
	CALL	FORMIN		;HL = indent for line #1
	PUSH	H		;Save indent #1
	XCHG			;HL-> first non space
	CALL	NEXTLF		;HL-> end of line
	INX$	H		;HL-> next char
	LDED	FRMEND		;+DE-> end of para
	CALL	CMHLDE		;Reached end already?
	POP	D		;DE = indent #1
	BLT	..4		;No, branch
	LXI	H,00		;Yes, Indent = Offset = 00
	JMPR	..5		;Yes, branch
;
..4:	PUSH	D		;Save indent #1
	CALL	FORMIN		;HL = indent for line #2
	POP	D		;DE = indent #1
	XCHG			
	DSUB	D		;HL = additional indent for first line
	MOV	A,L		;Put indent in A
	MVI	H,00		;Assume offset is 00
	BGE	..5		;Branch if there is indent
;
;	If A < 0 then this is the 2nd line offset
;
	NEG			;Get positive value
	MOV	H,A		;Save as offset
	MVI	L,00		;Indent is zero
..5:	SHLD	FRMIND		;Save L = first line indent
				;Save H = second line offset
;
;	L O O P  for formatting each line
;	HL-> begin of paragraph, DE-> end of paragraph including any CR-LF.
;
..L1:	MVIW$	ACTEND,ACTBUF	;Begin inserting text in ACTBUF
	MVIW$	FRMPOS,1	;Init logical pos
	MVIB$	TEMPFL,1	;Set flag to ignore spaces
				;Prevents new lines from beginning with space
	MOVW	FRMLBG,FRMGET	;Save Get PTR for line beginning
;
;	Insert any Indent.
;
	LDA	INDPOS		;Get indent position
	LHLD	FRMIND		;L = additional indent/offset
	ADD	L		;Add it in
;
;	Now make the 2nd line offset the additional indent for further lines
;
	MOV	L,H		;Get offset value
	SHLD	FRMIND		;Save new value
	CALL	INDDET		;Get # Tabs and Spaces for Indent
	PUSH	B		;Save count
	INR	B		;Account for initial DCR
..L2:	DCR	B		;Get remaining # Tabs
	BEQ	..L3		;Branch if zero
	MVI	C,TAB		;Get tab char
	CALL	FRMCHR		;Enter into work line
	JMPR	..L2		;Continue
;
..L3:	POP	B		;C = # spaces
	MOV	B,C		;Put count in B
	INR	B		;Account for initial DCR
..L4:	DCR	B		;Get remaining # Tabs
	BEQ	..L6		;Yes, process line
	MVI	C,SPACE		;Get space char
	CALL	FRMCH0		;Enter into work line (force space)
	JMPR	..L4		;Continue
;
;	When CR or LF encountered ignore the CR or LF, but make sure that a
;	space precedes the CR-LF.  Then ignore all following spaces and tabs.
;
..L5:	MVIB$	TEMPFL,1	;Set flag to ignore spaces
	MVI	C,SPACE		;Get Space
	MOV	M,C		;Change CR or LF to Space
	DCX$	H		;HL-> previous char
	MOV	A,M		;Get previous char
	CMP	C		;Was previous char a space?
	BNE	..L8		;No, add space to work line
				;Yes, then ignore CR or LF
				;Note how changing CR to Space will ignore LF
;
;	L O O P  for and add each character until wrap column reached.
;
..L6:	CALL	FORMDN		;Check for end of paragraph
				;Does not return here if Yes!
	MOV	A,C		;Get the char
	CALL	CMPCLE		;Is char CR or LF (or EOF)?
	BEQ	..L5		;Yes, process
	CALL	CMPBLK		;Is char Space or Tab?
	BNE	..L7		;No, add char
;
	TST$	TEMPFL		;Ignoring space and tab?
	JRNZ	..L6		;Yes, ignore it
;
..L7:	CLR$	TEMPFL		;Clear Ignore-Space flag
..L8:	CALL	FRMCHR		;Add char. in C to work line
	JRNC	..L6		;Continue until wrap point reached
;
	LHLD	ACTEND		;HL = edit PTR
	DCX$	H		;HL-> last char
	CALL	WRAPWD		;HL-> first char to wrap to next line
	JRC	..L6		;Branch if word to long
;
	SHLD	ACTEND		;ACTEND set for CR-LF, BC = adjust + 1
	LHLD	FRMGET		;Get PTR
	DSUB	B		;Subtract for wrapped word
	SHLD	FRMGET		;Save Get PTR for next line
	LHLD	FRMPOS
	DSUB	B
	SHLD	FRMPOS
	CPIB$	JUSTSW,1	;Is line to be justified?
	CZ	JUSTY		;Yes, justify it
				;Will set new ACTEND
	CALL	FRMELN		;Process end of line
	JMP	..L1		;Continue
	EPROC	FORMAT
;
; FORMDN  - Check if Get Pointer has reached end of Paragraph.
;	If not, return new char in C, save new Get PTR.
;	If end reached, write out last work line and write new screen.
;	
	BPROC	FORMDN
FORMDN:	CMPW	FRMGET,FRMEND	;Reached end of para?
	BGE	..1		;Yes, branch
	INX$	H		;Bump PTR
	SHLD	FRMGET		;Save new PTR
	DCX$	H		;HL-> current char
	MOV	C,M		;Get current char
	RET			;Return with char. in C
;
..1:	MVIB$	CNTBFL,1	;Set to recount LFs for line count
	CALL	FRMEL1		;Send work line to text buffer
	POP	H		;Ignore return address
	LHLD	FRMSAV		;Get PTR past end of Para
	RET			;RETURN from FORMAT - HL-> next para
	EPROC	FORMDN
	.PAGE
;
; FRMCHR - Enter char. in C into work line being formatted.
;	   Update ACTEND and FRMPOS.  
;	   Return: 'C' when wrap column reached.
;
				;{FORMPA}
	BPROC	FRMCHR
FRMCHR:	LHLD	ACTEND		;Get work line PTR
	MOV	A,C		;Get char
	CPI	SPACE		;Is it a space?
	BNE	FRMCH0		;No, add char
	TST$	JUSTSW		;Are we justifying?
	JRZ	FRMCH0		;No, don't strip spaces
;
;	Strip extra spaces, leaving two after ".", "?", "!".
;
	DCX$	H		;HL-> prev char
	MOV	A,M	
	CPI	SPACE		;Is prev char a space too?
	BNE	FRMCH0		;No, leave alone
	DCX$	H		;2nd prev. char
	MOV	A,M		;Get char
	CALL	PQECHK		;Is it sentence ending?
	RNZ			;No, ignore this space
				;{Don't drop SPACEs}
				;{FORMPA}
FRMCH0:	LHLD	ACTEND		;Get work line PTR
	MOV	M,C		;Save char
	LXI	D,ACTBUF+ACTLEN-2	;Get max ACTEND
	CALL	CMHLDE		;Reached max?
	BGE	..2		;Yes, don't bump
	INX$	H		;Bump PTR
..2:	SHLD	ACTEND		;Save new PTR
	LHLD	FRMPOS		;Get logical line pos
	MOV	A,C		;;SETLOG needs char in A
	CALL	SETLOG		;HL = new logical pos. for char
				;;Note: cannot use return code, because DE not set
	SHLD	FRMPOS		;Save new logical pos
	XCHG			;DE = new pos
	LD16	H,WRAPCL	;Get column at which to word-wrap
	MVI	H,00		;Is 8 bit value
	INR	L		;Allow char on last column if followed by CR
	JMP	CMHLDE		;Has wrap point been reached? 'C' if so
	EPROC	FRMCHR
;
; FORMIN - Compute indent for line <- HL. 
;	   Return: HL = indent, DE-> begin of text beyond indent
;
	BPROC	FORMIN
FORMIN:	XCHG			;DE-> begin first line
	LXI	H,0001		;Init logical pos
..1:	LDAX	D		;Get next char
	CALL	CMPBLK		;Is is space or tab?
	RNZ			;No, HL = indent
	CALL	SETLOG		;Yes, set log. pos
	INX$	D		;Bump PTR
	JMPR	..1		;Continue
	EPROC	FORMIN
;
; FRMELN - End a formatted line by appending CR-LF, setting ACTSIZ and HOLSIZ,
;	calling BCKATV. Enter HL = Get PTR for next line.
;
FRMELN:	LHLD	ACTEND		;Get work line PTR
	MVI	M,CR		;Add CR-LF to work line
	INX$	H
	MVI	M,LF
	INX$	H
	SHLD	ACTEND		;Save new ACTEND
;
				;{Entry to just save work line}
FRMEL1:	LHLD	FRMGET		;HL-> past last char processed
	SHLD	TXACEN		;Save for BCKATV
	XCHG			;DE = FRMGET
	MOVW	TXACTV,FRMLBG	;Get line begin PTR for BCKATV
	XCHG			;HL = Get PTR
	DSUB	D		;Compute size of text processed
	SHLD	HOLSIZ		;Save as "Hole" size
	PUSH	H		;Save HOLSIZ
;
	LHLD	ACTEND		;Get work line pointer
	LXI	D,ACTBUF	;DE-> begin of work line
	DSUB	D		;Compute size of work line
	SHLD	ACTSIZ		;Save length for BCKATV
	POP	D		;D = HOLSIZ
	CALL	SBHLDE		;BC = amount main text moves up
	LXI	H,FRMGET	;HL-> FRMGET, FRMEND and FRMSAV
	MVI	E,3		;Two PTRs
	CALL	CHGTBL		;Adjust PTRs
	JMP	BCKATV		;Write work line into main text
;
; JUSTY - Justify text in active line.
;
	BPROC	JUSTY
JUSTY:	LDED$	FRMPOS		;DE = logical pos
	LHLD	ACTEND		;HL-> past last char
..0:	DCX$	H		;HL-> one more char. back
	MOV	A,M		;Get last char
	CALL	CMPBLK		;Is this char Space or Tab?
	BNE	..1		;No, branch (other chars in ALWTBL)
	DCX$	D		;Yes, decrement logical pos (FRMPOS)
				;Needed to handle spaces after "."
	JMPR	..0		;Loop
;
..1:	LD16	H,WRAPCL	;L = word-wrap column
	MVI	H,00		;8 bit value
	CALL	SBHLDE		;BC = # unused columns
	JRC	..9		;Branch if BC = negative
	PUSH	B		;Save
;
;	Count # of "space groups" on line
;
	CALL	BLKSPN		;HL-> begin of line
	LXI	B,-1		;Init counter
..2:	INX$	B		;Bump count
	CALL	SPCGRP		;Scan for spaces
	JRNC	..2		;Loop until end of line reached
;
	SBCD$	GRPCNT		;Save the group count
	POP	H		;HL = total # spaces needed
	JCXZ	..9		;Can't justify if count zero
;
;	Compute number of spaces needed for justification
;
	MOV	E,C		;Move # space groups to DE
	MOV	D,B
	CALL	DVHLDE		;Divide, BC = # spaces to add uniformly
				;HL = remaining spaces to add
	MOV	B,C		;Put 8 bit count in B
	MVI	C,SPACE		;We will add spaces
	PUSH	H
	PUSH	B
;
;	Add the spaces at each space-group
;
	MOV	A,B		;Get count
	ORA	A		;Is it zero?
	JRZ	..5		;Yes, branch
;
	CALL	BLKSPN		;HL-> begin of line
..3:	CALL	SPCGRP		;HL-> where to insert
	JRC	..5		;Branch when end reached
	POP	B
	PUSH	B		;B = count, C = space
..4:	CALL	JUSINS		;Insert the space
	DJNZ	..4
	JMPR	..3		;Keep going with next space-group
;
;	Add the remaining spaces -
;		Start at alternate ends and add one space
;
..5:	CALL	BLKSPN		;HL-> begin of line
	POP	B		;Clear stack
	POP	B		;BC = # spaces to add
	LDA	JUSDIR		;Get justify direction flag
	CMA			;Complement the flag
	STA	JUSDIR		;Save it
	ORA	A		;Test flag
	JRZ	..7		;Branch if direction flag is "right"
;
;	Skip over the first space groups
;
	LHLD	GRPCNT		;HL = number of groups on line
	DSUB	B		;HL = # groups to skip over
	MOV	B,L		;B = skip count
	PUSH	B
	CALL	BLKSPN		;HL-> begin of line
	POP	B
..6:	CALL	SPCGRP		;
	DJNZ	..6
	LXI	B,-1		;Set BC to add spaces to end of line
;
;	Add one space at each space-group
;
..7:	MOV	B,C		;Put 8 bit count into B
..8:	MOV	A,B		;Is count zero?
	ORA	A
	JRZ	..9		;Yes, all done
	CALL	SPCGRP
	JRC	..9		;Branch if end of line reached
	MVI	C,SPACE
	CALL	JUSINS		;Add the space
	DCR	B
	JMPR	..8
;
..9:	RET
	EPROC	JUSTY
;
;
; SPCGRP - Scans to and over a group of spaces.
;	   Entry: HL-> where to start, DE-> end pointer
;	   Retrn: 'C' if end of line reached, else HL-> past last space
;	   Regs:  BC unused, DE unchanged, HL return PTR
;
	BPROC	SPCGRP
SPCGRP:	LDED	ACTEND		;+DE-> past last char on line
	MOV	A,M		;Get next char
	INX$	H		;Bump PTR
	CPI	SPACE		;Is this a space?
	BEQ	..1		;Yes, branch
	CALL	CMHLDE		;Reached end?
	CMC			;Complement 'C'
	RC			;Yes return 'C'
	JMPR	SPCGRP		;No, keep looking
;
..1:	CALL	CMHLDE		;Reached end?
	CMC
	RC			;Yes, return 'C'
	MOV	A,M		;Get char after space
	CPI	SPACE		;Is this a space?
	JNZ	RET%Z		;No, return 'NC'
	INX$	H		;Bump PTR
	JMPR	..1		;Scan over spaces
	EPROC	SPCGRP
;
; JUSINS - Add character in %C to active line at (HL)
;
JUSINS:	PUSH	B		;Save B = count, C  = char to insert
	PUSH	H		;HL-> where to insert
	XCHG			;DE-> first char to move up
	LXI	B,1		;Move up by one char
	LHLD	ACTEND		;HL-> end of active line
	CALL	MVTXUP		;Move text up
	SHLD	ACTEND		;Save end PTR
	POP	H		;HL-> insertion point
	POP	B		;C = char
	MOV	M,C		;Insert the char
	INX$	H		;Bump PTR
	RET
;
PQECHK:	CPI	'.'
	RZ
	CPI	'?'
	RZ
	CPI	'!'
	RET
