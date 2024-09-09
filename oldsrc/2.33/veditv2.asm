	.TITLE	'VEDIT-V2'
	.PAGE
;************************************************
;*						*
;*		Visual Operations		*
;*						*
;************************************************
;
; Copyright (C) 1986 by CompuView Products, Inc.
;			1955 Pauline Blvd.
;			Ann Arbor, MI 48103
;
;	Written by: Theodore J. Green
;
;	Last Change: Ted - Oct. 05, 1985 - 2.02 changes
;			 - Mar. 09 - Windows, VPRINT()
;			 - Mar. 10 - DELETE() minor
;			 - Mar. 12 - Changes due to NWSCFL routines
;			 - May. 13 - Menu functions
;			 - May  20 - New [FIND] and [REPLACE]
;			 - July 29 - HZRGHT, REPEAT, VSMENU changes
;			 - Oct. 11 - FNDEXE saves old pos. in FINDPT
;			 - Oct. 12 - REPEAT, VCHAR, [FIND]/[REPACE] changes
;			 - Oct. 16 - CANCEL() empty type ahead buffer
;			 - Oct. 24 - Fix [USER] bug, fix [REPLACE] bugs
;			 - Nov. 03 - Fix VDELET bug
;			 - Nov. 05 - Change ACTEOF to ACTEOL and DE%EOL, Misc
;			 - Nov. 07 - Change PAGEDW & CRDOW5; use of HZSCIC
;			 - Nov. 11 - VFLDMS message, misc
;			 - Nov. 12 - Check CNCLFL at VGTSTR() and VSMENU()
;;		     Tom - Nov. 25 - REPEAT() nums in col 0, 4 cols wide
;
; PROCKY - Decode keyboard character and jump to routine.
;
PROCKY:	CALL	CHKCUR		;Make sure cursor in position
	RC			;Return if screen needs rewriting
				;(VLOOP on stack)
	CALL	KEYDEC		;Get and decode in HL next keyboard char
	XRA	A		;Clear accumulator
	PCHL			;Jump to routine
;
;	Note:   The return address on the stack is to VLOOP.
;		Must dispatch with A = 00.
;
;
; VNOOP - Ignore unassigned control keys.
;
VNOOP:	RET
	.PAGE
;
; REPEAT - Repeat next function or character.  Default count is 4.
;	   Each additional REPEAT multiplies count by 4.
;	   Or user may type in decimal count. 			[7/21/86]
;

REPFLG:	DS	1		;Local flag for REPEAT()
REPFL2:	DS	1

	BPROC	REPEAT
REPEAT:	TST$	RPTFLG		;;Is a REPEAT running?
	RNZ			;;Yes, ignore this one - they don't nest
	CALL	MACSTA		;;Currently in keystroke macro?
	MOV	A,C		;;A = KFF if no macro
	STA	REPFL2		;;Save for later
;
	LXI	H,4		;Initial count is 4
	CLR$	REPFLG		;Clear decimal mode flag
..1:	LXI	D,257		;Get 257
	CALL	CMHLDE		;Count > 256?
	BLT	..2		;No, branch if count OK
	LXI	H,0000		;Yes, clear back to zero
..2:	PUSH	H		;Save count
;
;	Display current value on status line.
;
	XRA	A		;Display in column 0
	MOV	B,H		;Put count in BC
	MOV	C,L
	LXI	H,1000		;;4 cols
	CALL	BRDNM2		;Display count on status line
;
	CALL	KEYDEC		;Get and decode next key
	XCHG			;Put address in DE
	LXI	H,REPEAT
	CALL	CMHLDE		;Is it another REPEAT?
	MOV	A,C		;Get char
	POP	H		;Restore count
	JRNZ	..3		;No, branch
;
	DAD	H		;Mult. by 2
	DAD	H		;Mult. by 4
	JMPR	..1
;
..3:	CALL	DIGCHK		;Is it numeric digit?
	JRNZ	..5		;No, branch to put on stack
;
	TST$	REPFLG		;Encountered a digit yet?
	MOV	A,C		;Get digit back
	JRNZ	..4		;Yes, branch
	STA	REPFLG		;Set flag
	LXI	H,00		;Init counter
;
..4:	CALL	MULT10		;Mult. HL by 10 and add digit in 'A'
	JMPR	..1		;Continue
;
;	Save repeat count and dispatch to edit-function for first execution
;
..5:	SHLD	RPTCNT		;Save repeat count (will be pre-decremented)
	CALL	STBRFL		;Set to rewrite status line
	MOV	A,H
	ORA	L		;Is count zero?
	RZ			;Yes, do nothing
	MOVB	RPTFLG,REPFL2	;;Set repeat flag
				;;== KFF allows repeating entire keystroke macro
	XCHG			;HL = address of routine
	XRA	A		;Dispatch needs AL == 0
	PCHL			;Goto routine
				;This is needed to make menu-functions work
	EPROC	REPEAT
	.PAGE
;************************************************
;*						*
;*	Handle Text Characters			*
;*						*
;************************************************
;
; LITCHR - Insert next character literally. [5/10/86]
;
LITCHR:	CALL	GETKEY		;Get next input char without function decode
				;Char in A and C
	CPI	EOF		;Don't allow EOF to be inserted
	RZ
	CPI	LF		;;Is this a LF?
	BNE	VCHAR		;;No, treat normally
	CALL	STPSNL		;;Yes, write a partial screen, get new line
;	JMPR	VCHAR		;Insert LF into text
;
; VCHAR - Character in reg. C is new char or inserted char.
;
				;{LITCHR above, via PROCKY}
	BPROC	VCHAR
VCHAR:	CPIB$	UCNVSW,1	;Is the upper case convert switch set?
	BLT	CHGACT		;No, change or insert char
	BEQ	..4		;Branch if unconditional convert
;
;	Only convert from lower to upper case if before the
;	the ";" char on active line.
;
	PUSH	B		;Save char
	SUI%BC	ACTPNT,ACTBUF	;BC = # chars to search
	JRZ	..2		;None, reverse case
;
	XCHG			;HL-> begin search at ACTBUF
	LDA	CMNTCH		;Get char. to look for
	IF	POLLING, [
	CALL	CHKKYB		;Poll keyboard
	]
	CCIR			;Search for the char
	IF	POLLING, [
	CALL	CHKKYB		;Poll keyboard
	]
	BEQ	..3		;Found, don't convert
				;Not found, reverse the case
;
;	UCNVSW = 2 - Convert to upper case
;	UCNVSW = 3 - Reverse upper/lower case
;
..2:	POP	B		;Get char. back
	CPIB$	UCNVSW,2	;Convert to upper case?
	BEQ	..4		;Yes, branch
	MOV	A,C		;Get the char
	CALL	REVCAS		;Convert to opposite case, put char in C
	PUSH	B
;
..3:	POP	B		;Change or insert char. in C
	JMPR	CHGACT		;Insert the char
;
..4:	MOV	A,C		;Get the char
	CALL	CONVUC		;Convert lower to upper case
	MOV	C,A		;Put char in C
;	JMPR	CHGACT		;Insert the char
	EPROC	VCHAR
;
; CHGACT - Change character at edit pointer to value in C.
;
				;{VCHAR above}
CHGACT:	TST$	INSFLG		;In Insert mode?
	JRNZ	ADDACT		;Yes, add don't change
	CALL	ACTEOL		;Are we at end-of-line (EOF, LF or CR-LF)?
				;DE = ACTPNT, AL = char at (ACTPNT)
	JRZ	ADDACT		;;Yes, add, don't change
;
;	Test for overwriting TAB
;
	CPI	TAB		;Is old char a tab?
	BNE	CHGAC1		;No, branch
	LHLD	LOGHOR		;HL = current logical cursor position
	MOV	D,H		;Save in DE
	MOV	E,L
	CALL	FNDTAB		;Find next Tab position
	JZ	CHGAC1		;Overwrite if invalid pos
	MOV	L,A		;HL = next tab pos
	MVI	H,00
	INX	D		;Bump current pos
	CALL	CMHLDE		;Are we in last position of Tab?
	BNE	ADDACT		;No, then insert the char
				;Yes, then overwrite TAB
;
				;{CHGCAS}
CHGAC1:	LHLD	ACTPNT		;HL-> char. to be changed (label for VPLUS)
	MOV	A,M		;Get old char. again
	MOV	M,C		;Change to new char
	CPI	' '		;Was old char. some other control char?
	BLT	CHGAC2		;Yes, set to partial rewrite
;
	MVI	A,' '-1		;Largest control char
	CMP	C		;Is new char a control char?
	BGE	CHGAC2		;Yes, set for partial rewrite
;
	PUSH	B
	CALL	HORFIX		;Check and correct horizontal position
				;Is this needed???
	CALL	CHKCUR		;Make sure cursor positioned on screen
	POP	B
;
	CALL	OUTCHR		;Display on screen too
	JMPR	JCRRGH
;
CHGAC2:	CALL	STPWAC		;Set for partial active line rewrite
JCRRGH:	JMP	CRRGHT		;Move cursor right
;
; ADDACT - Add character in C to active line, perform word wrap if needed.
;	   Move cursor right.
;
				;{ADDCHR}
ADDACT:	CPIB$	HOROPT,2	;Might we need to fill with spaces?
	BNE	ADDAC1		;No, branch.  Without this test a forced
				;horizontal scroll will abort this char
	PUSH	B
	CALL	HORFIX		;Check if chars need to be inserted
	POP	B
ADDAC1:	CPIW	ACTSIZ,ACTLEN-2	;Is active line size within 2 of max?
	RNC			;Yes, don't add
ADDAC2:	TST$	FULLFG		;Is text buffer full?
	RNZ			;Yes, don't add
;
	MVIB$	EOLFLG,1	;No need for an EOL
	CALL	STPWAC		;Set for partial active line rewrite
	INCW$	ACTSIZ		;Increment size of active line
	PUSH	B		;Save char. to insert
	LDED$	ACTPNT		;DE-> first char. to move up
	PUSH	D		;Save ACTPNT
	LXI	B,1		;Move up by 1 char
	LHLD	ACTEND		;HL-> end of active line
	CALL	MVTXUP		;Move text up
	SHLD	ACTEND		;Save end PTR
	POP	H		;HL = ACTPNT
	POP	B		;Restore char to insert
	MOV	M,C		;Insert char

	IFNOT	FULL, [
	JMPR	JCRRGH		;Move cursor right
	]
	IF	FULL, [
;
;	Check for Word-Wrap.
;
	TST	WRAPCL		;Get column at which to word wrap
	JRZ	JCRRGH		;Branch if Word-wrap is off
;
	MOV	L,A		;Put into L
	MVI	H,00		;HL = WRAPCL
	LDED	LOGHOR		;+Get logical cursor pos
	CALL	CMHLDE		;Reached wrap column?
	BGE	JCRRGH		;No, branch
	MVIW$	LOGHOR,00	;Prevent infinite loop with INDENT(*)
	MOV	A,C		;Get char
	CALL	CMPCLE		;Is char CR, LF (or EOF)?
	JRZ	JCRRGH		;Yes, just move cursor right
				;In case user types CR at critical column
;
;	Perform the word wrap by inserting a CRLF before this word.
;
	LHLD	ACTPNT		;HL = edit PTR
	CALL	WRAPWD		;DE-> current char, HL-> begin of word
				;BC = DE-HL + 1
	JRC	JCRRGH		;Branch if word too long
;
	SHLD	ACTPNT		;ACTPNT-> where to insert CR-LF
	MVI	B,00		;Count is 8 bits
	LXI	D,CRRGHT	;Perform cursor right
	CALL	PUSHO1		;Put on operation stack
	CALL	FRMVS1		;HL-> main text buffer
	XCHG			;Save in DE
	LDA	CURVER		;Get cursor line #
	CALL	FCBADD		;HL-> begin of line
	XCHG			;DE-> begin of line, HL-> begin of word
	CALL	CMHLDE		;Is word being wrapped on same line?
	JMPR	VCHCR1		;Add CR-LF and move cursor
	]
;
; VCHCR - Insert CR-LF into active line.
;	  Also add tabs and spaces for any indent.
;
VCHCR:	TST$	FULLFG		;Is text buffer full?
	RNZ			;Yes, don't insert(*)
	CPIW	ACTSIZ,ACTLEN-1	;Is there room for CR-LF?
	RNC			;No, don't add
;
;	The CRT version erases the rest of the line and writes a partial
;	screen from the next line.
;
	IF	CRTVRS, [
	LDA	CURVER		;Get cursor line number
	INR	A		;Start screen write at next line
	STA	FSACSC		;Save as screen write beginning line #
	CALL	CHKCUR		;Pos. cursor in case of REPEAT
	CALL	WINEOL		;Clear rest of line now
	]

	XRA	A		;Set 'NC'
				;{ADDACT}
VCHCR1:	PUSHF			;Save flag to determine cursor movement
	CALL	ADCRLF		;Add CR-LF to active line
	CALL	ADDIND		;Add any indent to line, set new CUR & EDTHOR
	CALL	STPSNL		;Set write partial screen, get new text line
	POPF			;Move cursor to new line?
	RC			;No, return
	JMP	CRDOW2		;Yes, move cursor to next line
	.PAGE
;
; VTAB - Handle tab character by inserting a tab character or
;	 spaces to the next tab position.
;
VTAB:	MVI	C,TAB		;Make sure C contains the Tab
	TST$	EXPTSW		;Are tabs expanded with spaces?
	JZ	VCHAR		;No, insert the tab character
;
	LHLD	LOGHOR		;Yes, get logical line position
	MOV	B,L		;Save begin position
	CALL	FNDTAB		;Find next tab position
	JZ	VCHAR		;Insert a tab if past last tab pos
	SUB	B		;Compute movement
;
				;{HORFIX,ADDIND}
ADDSP:	MOV	B,A		;Save movement in B
	MVI	C,SPACE		;Expand with spaces
;
; ADDCHR - Add 'B' chars in 'C' to the active line.
;
				;{VTAB,ADDIND}
	BPROC	ADDCHR
ADDCHR:	INR	B		;Adjust for initial DCR
..1:	DCR	B		;Decrement count
	RZ			;Return if zero
	PUSH	B		;Save char. and count
	CALL	ADDACT		;Add space to active line
	POP	B		;Restore
	JMPR	..1		;Continue
	EPROC	ADDCHR
;
; ADCRLF - Add CR-LF to active line.
;
ADCRLF:	MVI	C,CR		;Get CR
	CALL	ADDAC2		;Add to active line
	MVI	C,LF		;Get LF
	JMP	ADDAC2		;Add to active line
	.PAGE
;************************************************
;*						*
;*	Indent/Undent Routines			*
;*						*
;************************************************
;
; ADDIND - Perform an Indent by inserting the most Tabs and fewest
;	   Spaces to the current Indent Position.
;	   Begin by inserting tabs as far as possible.
;	   Return: Sets new CURHOR.  Caller must set for new active line.
;
				;{VCHCR,INDENT,UNDENT}
	BPROC	ADDIND
ADDIND:	TST$	AUTIND		;In auto-indent mode?
	JRZ	ADDIN1		;No, branch
;
;	For auto-indent compute indent for previous line
;
	CPIW	ACTPNT,ACTBUF	;At begin of active line?
	XCHG			;HL-> ACTBUF
	BNE	..2		;No, use indent on current line
	CMPW	TXACTV,TXTFLR	;At begin of edit buffer?
	MVI	A,00		;Assume yes
	BEQ	..3		;Yes, use indent of zero
;
	CALL	PREVLF		;No, HL-> previous line
	INX$	H		;Skip over LF
..2:	CALL	FORMIN		;HL = indent for previous line
	MOV	A,L		;A = indent amount
	DCR	A		;Adjust
..3:	STA	INDPOS		;Save as indent value
;
				;{INDENT}
ADDIN1:	LDA	INDPOS		;Get indent position
	INR	A		;Adjust for reserved column
	STA	EDTHOR		;Move cursor to indent in case <CR> typed
	STA	CURHOR
	DCR	A		;Restore
	CALL	INDDET		;Get B = # tabs, C = # spaces
;
;	Perform Indent by inserting %B Tabs and %C Spaces
;
	PUSH	B		;Save # spaces
	MVI	C,TAB		;Add Tab char
	CALL	ADDCHR		;Add #B Tabs to active line
	POP	B		;C = # spaces
	MOV	A,C		;Put count in A
	JMPR	ADDSP		;Add 'A' spaces to the line
	EPROC	ADDIND
;
; INDDET - Compute # of Tabs and Spaces needed for Indent.
;	   Enter: A = Indent.
;	   Retrn: B = # Tabs, C & A = # Spaces.
;
				;{ADDIND,FORMPA}
	BPROC	INDDET
INDDET:	MOV	C,A		;Save A
	MVI	B,00		;Clear Tab counter
	TST$	EXPTSW		;Are tabs expanded with spaces
	MOV	A,C		;A = INDPOS
	RNZ			;Yes, return with indent in C
	INR	A		;Adjust for way Tab table is setup
	LXI	H,TABTBL	;HL-> Tab table
..2:	CMP	M		;Is indent past this tab position?
	BLT	..3		;No, rest is spaces
	INR	B		;Yes, increment Tab count
	INX$	H		;HL-> next tab position
	JMPR	..2		;Check next tab pos
;
..3:	DCX$	H		;HL-> previous tab pos
	SUB	M		;Compute # spaces still needed
	MOV	C,A		;Return with with # spaces in A and C
	RET
	EPROC	INDDET
;
; UNDENT - Decrease the indent position by the indent increment.
;
	BPROC	UNDENT
UNDENT:	SUBB	INDPOS,INDINC	;Subtract increment from position
	BGE	..1		;Branch if position good
	XRA	A		;Else change to zero
..1:	STA	INDPOS		;Save new indent pos
	JMPR	INDEN1		;Perform an indent to the new position
	EPROC	UNDENT
;
; INDENT - Increment the indent position by the indent increment.
;
INDENT:	ADDB	INDINC,INDPOS	;Add increment to position
	MOV	C,A		;Save tentative value in C
	LDA	WWLLE1		;Get displayed line length
	DCR	A		;Allow one less
	CMP	C		;Beyond one line?
	RC			;Yes, don't indent further
	MOV	M,C		;No, set new indent pos
;
;	If cursor is at begin of line, also perform an indent.
;
INDEN1:	CALL	BLKSPN		;HL-> first non-space-tab on line
	LDED	ACTPNT		;+Get the cursor position
	CALL	CMHLDE		;Is cursor within spaces and tabs?
	RC			;No, cannot perform an indent
	MVIW$	ACTPNT,ACTBUF	;Move edit point to active line begin
	CALL	DELAC1		;Delete the chars up to non-space-tab
	CALL	ADDIN1		;Add the new indent
	CLR$	EOLFLG		;Need to EOL in case of UNDENT
	JMP	STRWAC		;Set to rewrite the active line
	.PAGE
;************************************************
;*						*
;*	BLOCK Function				*
;*						*
;************************************************
;
; VBLOCK - Copy/move text to register, insert register, delete or print
;
;
VBLDEC:	DB	'C'		;Copy
	DW	VCPTXT
	DB	'M'		;Move
	DW	VMVTXT
	DB	'I'		;Insert
	DW	VINTXT
	DB	'D'		;Delete text
	DW	VDELET
	DB	'S'		;Swap
	DW	VBLSWP
	DB	KFF		;End of table

	DSEG	$
VBLPMT:	DC	'[C]opy  [M]ove  [I]nsert  [D]elete  [S]wap ' [00]
VDELMT:	DC	'OK to delete       Lines [Y]es / [N]o ' [00]
	CSEG	$

VBLOCK:	LXI	H,VBLPMT	;HL-> Option prompt
	LXI	D,VBLDEC	;DE-> decode address table
	JMP	VSMENU		;Dispatch to desired routine
;
; VCPTXT - Set the visual text block move end pointers,
;	   and make a text copy when second pointer.
;	   Enter: Assume A = 0
;
VMVTXT:	INR	A		;!=00
VCPTXT:	STA	MVCPFL		;Save flag, 0 = Copy, 1 = Move
	CALL	GTBLCK		;DE = current text PTR, HL = BLMVEN
				;Note: does not return here if BLMVEN not set
;
;	Make a copy of the text block. Error if not enough
;	room remains for the copy. Sets the appropriate
;	visual switch and clears the BLMVEN pointer.
;
	IF	FULL, [
	CALL	GETRNM		;Get register number
	]
	CALL	MAKCPY		;Make a copy of text
				;DE-> block begin, HL-> end
				;MAKCPY returns text length in BC
	JRC	MVTXFL		;Branch if no space error
	TST$	MVCPFL		;Is this a MOVE - need to delete too?
	JRZ	STVTSW		;Branch if no delete
;
	PUSH	D		;Save -> begin of block copied
				;HL-> begin of text to move down
	CALL	NEGBC		;Need -BC
	CALL	BUFFDW		;Move text buffer down
				;{VDELET}
VCPTX1:	CALL	STVTSW		;Clear BLMVEN end pointer
	POP	H		;HL-> original text block begin
	POP	D		;Ignore return address
	JMP	TOVIS1		;Set screen from edit pointer
;
MVTXFL:	MVIB$	FULLFG,1	;Turn FULL message on
;
				;{CANCEL}
STVTSW:	LXI	D,0000		;Get zero to clear end pointer
STVTS1:	SDED$	BLMVEN		;Save new end pointer
	JMP	STBRFL		;Setup to write new status line
;
; GTBLCK - Get begin/end PTRs for desired block of text.
;
GTBLCK:	CALL	FRMVIS		;HL-> text for cursor
	XCHG			;DE-> "
	LHLD	BLMVEN		;Get one pointer
	MOV	A,H		;Is it set?
	ORA	L
	RNZ			;Yes, HL = BLMVEN, DE = current text PTR
	POP	PSW		;No, don't return to caller
	JMPR	STVTS1		;Set BLMVEN from current text PTR
;
; VINTXT - Insert text from the text register at the current
;	   cursor position.
;
	BPROC	VINTXT
VINTXT:	CALL	GETRNM		;Get text register number
	CALL	FRMVIS		;HL-> where to insert in text
	CALL	GETTXT		;Copy the text in
	JRC	MVTXFL		;Give FULL if no room
	TST$	PTAFSW		;Move edit pointer past insert?
	JZ	VMAIN3		;No, branch to rewrite screen ;
;
	XCHG			;HL-> past insert
	JMP	TOVIS1		;Set new visual edit pointer
	EPROC	VINTXT
;
; VDELET -
;
	BPROC	VDELET
VDELET:	CALL	GTBLCK		;DE = current text PTR, HL = BLMVEN
				;Note: does not return here if BLMVEN not set
;
	SHLD	DELPTR		;DELPTR and EDTPTR are now set
	CALL	MXHLDE		;Set HL >= DE
	LXI	B,0000		;Init count
	CALL	CNTLFB		;BC = count LFs from (DE) to (HL)
	DCX$	B		;Adjust for full lines
	CALL	ITOA		;HL-> ASCII number string
	LXI	D,VDELMT+13	;DE-> destination inside message
	CALL	STRZCP		;Copy ASCII in without [00]
..1:	LXI	H,VDELMT	;HL-> confirm message
	CALL	STAKEY		;Give prompt, get reply, AL = UC
	CPI	'N'		;"NO" ?
	RZ			;Return- do nothing
	CPI	'Y'		;"YES" ?
	BNE	..1		;Get valid response
	CALL	PACKTX		;Delete the text
	LHLD	EDTPTR		;;HL-> text following the delete
	PUSH	H		;;Needed by VCPTX1
	JMPR	VCPTX1		;Clear BLMVEN and rewrite screen
	EPROC	VDELET
;
;
VBLSWP:	CALL	FRMVIS		;HL-> text for cursor
	XCHG			;DE-> "
	LHLD	BLMVEN		;Get "marker" position
	MOV	A,L
	ORA	H		;Is marker set?
	RZ			;No, do nothing
	SDED	BLMVEN		;Save cursor position
	JMP	TOVIS0		;Jump to marker
	.PAGE
;************************************************
;*						*
;*	GOTO Function				*
;*						*
;************************************************
;
VGODEC:	DB	'B'		;Begin of File
	DW	VGOBEG
	DB	'H'		;Home
	DW	VGOHOM
	DB	'M'		;Mark
	DW	GOMARK
	DB	'J'		;Jump
	DW	GOJUMP
	DB	'L'		;Goto Line
	DW	GOLINE
	DB	'Z'		;Zend
	DW	VGOZEN
	DB	'E'		;End
	DW	VGOEND
	DB	KFF		;End of table

	DSEG	$
VGOPMT:	DC	'[B]egin  [H]ome  [M]ark  [J]ump  [L]ine  [Z]end  [E]nd ' [00]
VGOLPM:	DC	'Enter Line Number: ' [00]
VMHOME:	DC	':B' [00]
VMBEGN:	DC	'_B' [00]
VMZEND:	DC	':Z' [00]
VMEOF:	DC	'_Z' [00]
VMLINE:	DC	':_L' [00]
	CSEG	$
;
; VGOTO - Goto a marker, home, zend, or line number
;
;
VGOTO:	LXI	H,VGOPMT	;HL-> Option prompt
	LXI	D,VGODEC	;DE-> decode address table
	JMP	VSMENU		;Dispatch to desired routine
;
;
VGOBEG:	LXI	H,VMBEGN	;HL-> "_B"
	JMPR	JVSMAC
;
VGOHOM:	LXI	H,VMHOME	;HL-> ":B"
	JMPR	JVSMAC
;
VGOZEN:	LXI	H,VMZEND	;HL-> ":Z"
	JMPR	JVSMAC
;
VGOEND:	LXI	H,VMEOF		;HL-> "_Z"
JVSMAC:	JMP	VISMAC		;
;
; GOMARK - Save current line position
;
GOMARK:	CALL	GETMNM		;Get marker number
	CALL	FRMVIS		;HL-> current edit position
				;BCKATV above helps prevent drift
	XCHG			;Put in DE
	CALL	MRKSET		;HL-> marker storage
	JMP	STDEIN		;Save DE in (HL)
;
MRKSET:	LXI	H,PNTTBL	;Calc save location address
	LDA	REGNUM		;Get marker #
	JMP	ADAAHL		;HL-> storage for marker
;
; GOJUMP - Jump to saved mark position
;
GOJUMP:	CALL	GETMNM		;Prompt, get marker number
	CALL	BCKATV		;Save active line
	CALL	MRKSET		;HL-> marker storage
	CALL	MVINHL		;Get marker position
	MOV	A,H
	ORA	L		;;Is marker set?
	RZ			;;No, do nothing
	LDED	TXTFLR		;Minimum marker position is TXTFLR
	CALL	MXHLDE		;HL = max of HL and DE
	JMP	TOVIS0		;Jump cursor to marker
;
; GOLINE -
;
	BPROC	GOLINE
GOLINE:	LXI	H,VGOLPM	;HL-> "Enter Line Number:"
	CALL	VSGSTR		;Prompt and get reply string
				;HL-> string
	CALL	ATOI		;BC = desired line #
	LHLD	LINFIL		;HL = current line #
	DSUB	B		;HL = (-) difference for forward
	MOV	B,H
	MOV	C,L		;BC = |difference| for backward
	LXI	D,VMCSAV	;DE-> "-" preceding DECSAV
	JRNC	..BACK		;Branch if back towards begin of file
;
..FORW:	CALL	NEGBC		;BC = absolute value
	INX$	D		;DE-> DECSAV
;
..BACK:	CALL	ITOA		;Create ASCII
	PUSH	H		;HL-> ASCIZ
	CALL	STRLEN		;BC = length
	POP	B		;Restore
	DAD	B		;HL-> [00]
	PUSH	D		;Save -> begin of command string
	LXI	D,VMLINE	;DE-> ":_L [00]"
	XCHG			;DE-> destination
	CALL	STRCPY		;Copy it in
	POP	H		;HL-> begin of command string
	JMPR	JVSMAC		;Execute "xx:_L" command
	EPROC	GOLINE

	.PAGE
;************************************************
;*						*
;*	PRINT Function				*
;*						*
;************************************************
;
VPRDEC:	DB	'A'		;All
	DW	VPRALL
	DB	'B'		;Block
	DW	VPRBLK
	DB	'E'		;Eject
	DW	PEJECT
	DB	'L'		;Lines
	DW	VPRLIN
	DB	'M'		;Margin
	DW	VPRMAR
	DB	KFF

	DSEG	$
VPRPMT:	DC	'[A]ll  [B]lock  [E]ject  [L]ines  [M]argin ' [00]
VMALL:	DC	'-#_PR #_PR PE' [00]
VMMAR:	DC	'Enter Printer Margin: ' [00]
	CSEG	$
;
; VPRINT - Print All, Block or Formatted; Eject; set parameters
;
;
VPRINT:	LXI	H,VPRPMT	;HL-> Option prompt
	LXI	D,VPRDEC	;DE-> decode address table
	JMP	VSMENU		;Dispatch to desired routine
;
VPRALL:	LXI	H,VMALL
	JMP	VISMAC
;
VPRLIN:	LXI	H,VWPLIN	;HL-> prompt
	LXI	D,PPLNCN	;DE-> parameter to set
;	JMP	VSTPAR		;Prompt and set parameter
;
; VSTPAR -
;
VSTPAR:	PUSH	D		;Save -> parameter
	CALL	VSGSTR		;Prompt and get user reply string
				;HL-> string
	CALL	ATOI		;C = number
	POP	H		;HL-> parameter
	MOV	M,C		;Set parameter
	RET
;
VPRMAR:	LXI	H,VMMAR		;HL-> prompt
	LXI	D,PPLFMR	;DE-> parameter
	JMPR	VSTPAR		;Prompt and set parameter
;
; VPRBLK - Print defined block of text on printer.	[3/09/86]
;
VPRBLK:	CALL	GTBLCK		;DE = current text PTR, HL = BLMVEN
				;Note: does not return here if BLMVEN not set
	CALL	LSTBLK		;Send block to printer
				;Restores ROUTAD to OUTCHR when done
	JMP	STVTSW		;Clear BLMVEN and rewrite status line
;
	.PAGE
;************************************************
;*						*
;*	FILE Functions				*
;*						*
;************************************************
;
VFLDEC:	DB	'E'		;Exit
	DW	VFLEXT
	DB	'Q'		;Quit
	DW	VFLQUT
	DB	'S'		;Save
	DW	VFLSAV
	DB	'D'		;Directory
	DW	VFLDIR
	DB	'N'		;New
	DW	VFLNEW
	DB	KFF

	DSEG	$
VFLPMT:	DC	'[E]xit  [Q]uit  [N]ew  [S]ave  [D]irectory ' [00]
VQTPMT:	DC	'Abandoning File --  [S]tay in VEDIT  [E]xit VEDIT  [N]o - Resume Editing ' [00]
VNWPMT:	DC	'OK to Save Current File?  [Y]es  [N]o-Abandon File ' [00]
VMFNAM:	DC	'Enter Filename: ' [00]
VMEXIT:	DC	'+EX' [00]
VQTEMS:	DC	'EZY' [00]
VQTSMS:	DC	'YEC+EQY' [00]
VMSAVE:	DC	'EA' [00]
VMFINI:	DC	'EY' [00]
VFLDMS:	DC	'@YT/' [CR] '/-YWZED' [00]
	CSEG	$
;
; VSFILE - Basic file functions
;
;
VSFILE:	LXI	H,VFLPMT	;HL-> Option prompt
	LXI	D,VFLDEC	;DE-> decode address table
	JMP	VSMENU		;Dispatch to desired routine
;
VFLEXT:	LXI	H,VMEXIT	;HL-> "EX"
	JMPR	JVISMC
;
VQTDEC:	DB	'S'
	DW	VQTSTY
	DB	'E'
	DW	VQTEXT
	DB	'N'
	DW	VLOOP
	DB	KFF

VFLQUT:	CALL	MACRST		;Disable keystroke macros
	LXI	H,VQTPMT	;HL-> sub-menu prompt
	LXI	D,VQTDEC	;DE-> decode table
	JMP	VSMENU		;Dispatch to desired routine
;
VQTEXT:	CALL	MAXWIN		;;Only one window?
	LXI	H,VQTSMS	;HL-> "YEC+EQY"
	JRNZ	JVISMC		;;No, erase the window too
	LXI	H,VQTSMS+3	;HL-> "+EQY"
	JMPR	JVISMC		;;Yes, don't erase
;
VQTSTY:	LXI	H,VQTEMS	;HL-> "EZY"
	JMPR	JVISMC
;
VFLSAV:	LXI	H,VMSAVE	;HL-> "EA"
JVISMC:	JMP	VISMAC
;
; VFLNEW -
;
	BPROC	VFLNEW
VFLNEW:	TST$	OUTFLG		;Is an output file open?
	LXI	B,'B'*256+'E'	;BC = "EB"
	LXI	D,2020H		;Get two spaces
	JRZ	..2		;No, branch
..1:	LXI	H,VNWPMT	;HL-> confirm prompt
	CALL	STAKEY		;Give prompt, get reply
	LXI	B,'B'*256+'E'	;BC = "EB"
	LXI	D,'Z'*256+'E'
	CPI	'N'		;"NO" ?
	BEQ	..2		;Empty edit buffer
	LXI	D,'Y'*256+'E'
	CPI	'Y'		;"YES" ?
	BNE	..1		;Get valid response

..2:	LXI	H,STRBUF	;;HL-> GP handy buffer
	CALL	STDEIN		;;Save EZ, EY or "  " in buffer; HL++
	MVI	M,SPACE		;;Add space (needed after "EZ")
	INX$	H
	MOV	M,C		;;Save BC next
	INX$	H
	MOV	M,B
	INX$	H		;;HL-> next position
	XCHG			;;DE-> where to get filename
;
VFLNW3:	PUSH	D		;Save -> input buffer
	LXI	H,VMFNAM	;HL-> prompt
	MOV	B,D
	MOV	C,E		;;BC-> buffer to use
	CALL	VGTSTR		;Get reply, HL-> [00] at end
	POP	D		;DE-> input buffer
	JRZ	VFLNW3		;Over if CTRL-X
	MVI	M,CR		;Terminate with CR
	INX$	H
	MVI	M,00		;Now add zero
	LXI	H,STRBUF	;HL-> entire command string
	JMPR	JVISMC
	EPROC	VFLNEW
;
VFLDIR:	LXI	H,VFLDMS	;HL-> "@YT/" <CR> "/-YWZED"
	LXI	D,STRBUF	;DE-> work buffer
	CALL	STRZCP		;Copy string without [00]
				;;DE-> where to get filename
	JMPR	VFLNW3		;Merge above to get filename

	.PAGE
;************************************************
;*						*
;*	WINDOW Function				*
;*						*
;************************************************
;
VWIDEC:	DB	'C'		;Create new window?
	DW	VWICRE
	DB	'D'		;Delete current window?
	DW	VWIDEL
	DB	'S'		;Switch windows?
	DW	VWISWC
	DB	'Z'		;Zoom window?
	DW	USZOOM		;No, give prompt over
	DB	KFF		;End of table
;
; VWINDO - Goto / create / delete a window
;
	IF	WINDOO, [
	DSEG	$
VWIPMT:	DC	'[C]reate  [D]elete  [S]witch  [Z]oom ' [00]
VWPCRE:	DC	'[T]op  [B]ottom  [L]eft  [R]ight ' [00]
VWPLIN:	DC	'Enter # Lines: ' [00]
VWPCOL:	DC	'Enter # Columns: ' [00]
VWPWIN:	DC	'Enter Window Name: ' [00]
VWPSWC:	DC	'Enter Edit Buffer Name: ' [00]
VWMDEL:	DC	'YWD EE.' [00]
VWMCRE:	DC	'YW        ' [00]
VWMSWC:	DC	'EE ' [00]
	CSEG	$

VWINDO:	LXI	H,VWIPMT	;HL-> Option prompt
	LXI	D,VWIDEC	;DE-> decode address table
	JMP	VSMENU		;Dispatch to desired routine
;
; VWICRE -
;
	BPROC	VWICRE
VWICRE:	LXI	H,VWPCRE	;HL-> T/B/R/L prompt
	CALL	STAKEY		;Give prompt, A = UC reply
	ST8	VWMCRE+2	;Save as part of YW command
;
	PUSHA
	LXI	H,VWPWIN	;HL-> window # prompt
	CALL	STAKEY		;Give prompt, AL = UC reply
	ST8	VWMCRE+3	;Save as part of YW command
	POPA
;
	LXI	H,VWPLIN	;HL-> "Enter # Lines:"
	CPI	'T'		;Top?
	BEQ	..2
	CPI	'B'		;Bottom?
	BEQ	..2
	LXI	H,VWPCOL	;HL-> "Enter # Columns:"
	CPI	'L'		;Left?
	BEQ	..2
	CPI	'R'		;Right?
	BEQ	..2
	BNE	VWICRE		;No, give prompt over
;
..2:	CALL	VSGSTR		;Prompt, get string into STRBUF[]
				;HL-> digit string
	LXI	D,VWMCRE+4	;DE-> command being created
	CALL	STRCPY		;Copy the number to command
	LXI	H,VWMCRE	;HL-> command
	JMPR	WVSMAC		;Execute in command mode
	EPROC	VWICRE
;
VWIDEL:	LXI	H,VWPWIN	;HL-> window # prompt
	CALL	STAKEY		;Give prompt, AL = UC reply
	STA	VWMDEL+3	;;Save as part of YWD command
	LXI	H,VWMDEL	;HL-> "-YW"
	JMPR	WVSMAC		;Yes, execute in command mode
;
VWISWC:	LXI	H,VWPSWC	;HL-> edit buffer prompt
	CALL	STAKEY		;Give prompt, AL = UC reply
	STA	VWMSWC+2	;Make part of EE command
	LXI	H,VWMSWC	;HL-> command
WVSMAC:	JMP	VISMAC		;Execute in command mode
	]
	.PAGE
;************************************************
;*						*
;*	USER Menu Functions			*
;*						*
;************************************************
;
VUSDEC:	DB	'W'		;Word wrap
	DW	VUSWWP
	DB	'J'		;Justify
	DW	VUSJUS
	DB	'I'		;Indent
	DW	VUSIND
	DB	'A'		;Auto-Indent
	DW	VUSAUT
	DB	KFF		;End of table

	DSEG	$
VUSPMT:	DC	'[W]ord Wrap  [J]ustify  [I]ndent  [A]uto-Indent ' [00]
VUSWWM:	DC	'Enter Word Wrap Column (0 = Off): ' [00]
VUSJUM:	DC	'Justify -- (0 = Off) (1 = On) (2 = Unjustify): ' [00]
VUSAUM:	DC	'Auto-Indent -- (0 = Off) (1 = On): ' [00]
VUSINM:	DC	'Enter Indent Increment: ' [00]
	CSEG	$
;
; VSUSER - Change ES and EP parameters
;
VSUSER:	LXI	H,VUSPMT	;HL-> Option prompt
	LXI	D,VUSDEC	;DE-> decode address table
	JMP	VSMENU		;Dispatch to desired routine
;
VUSWWP:	LXI	H,VUSWWM	;HL-> prompt
	LXI	D,WRAPCL	;DE-> parameter
	JMP	VSTPAR		;Prompt and set parameter
;
VUSAUT:	LXI	H,VUSAUM	;HL-> prompt
	LXI	D,AUTIND	;DE-> parameter
	JMP	VSTPAR		;Prompt and set parameter
;
VUSJUS:	LXI	H,VUSJUM	;HL-> prompt
	LXI	D,JUSTSW	;DE-> parameter
	JMP	VSTPAR		;Prompt and set parameter
;
VUSIND:	LXI	H,VUSINM	;HL-> prompt
	LXI	D,INDINC	;DE-> parameter
	JMP	VSTPAR		;Prompt and set parameter

	.PAGE
;************************************************
;*						*
;*	MISC Menu Functions			*
;*						*
;************************************************
;
VMSDEC:	DB	'M'
	DW	VMSMAT
	DB	'U'
	DW	CHGCAS
	DB	'I'
	DW	SETINS
	DB	'O'
	DW	RESINS
	DB	KFF

	DSEG	$
VMSPMT:	DC	'[M]atch Parentheses  [U]C/LC  [I]nsert  [O]verstrike ' [00]
VMSMYM:	DC	'YM' [00]
	CSEG	$
;
; VMISC - Misc. -
;
VMISC:	LXI	H,VMSPMT	;HL-> Option prompt
	LXI	D,VMSDEC	;DE-> decode address table
	JMP	VSMENU		;Dispatch to desired routine
;
VMSMAT:	LXI	H,VMSMYM	;HL-> "YM"
	JMP	VISMAC
;
; CHGCAS - Change case of letter at active point
;
CHGCAS:	LHLD	ACTPNT		;HL-> active char
	MOV	A,M		;Get char
	CALL	LETCHK		;Is it a letter?
	JNZ	CRRGHT		;No, jump move cursor to next char
	XRI	20H		;Yes, flip U/L case
	MOV	C,A		;Save new char in C
	JMP	CHGAC1		;Overwrite old char in active line
;
; Routines to change Insert mode switch.
;
SWCINS:	TST	INSFLG		;Is insert mode flag set?
	JRNZ	RESINS		;Yes, turn insert mode off
;
SETINS:	INR	A		;1 = ON value
	JMPR	SETIN1
;
RESINS:	XRA	A		;0 = OFF value
SETIN1:	CMPM	INSFLG		;Is old insert mode value same as new?
	MOV	M,A		;Save new value
	JNZ	STBRFL		;No, setup to write new status line
	RET			;Yes, just return
	.PAGE
;
; VSMENU - Display visual menu and dispatch to desired routine.
;	   Skip menu if any char is pending.
;	   Enter: HL-> message, DE-> decode table.
;
VSTEMP:	DSW	1		;Temp -> message
VSTMP1:	DSW	1

	BPROC	VSMENU
VSMENU:	SHLD	VSTEMP		;Save-> message
	SDED	VSTMP1
	CALL	GETSTA		;Is a character already pending?
				;Most likely a keystroke macro or REPEAT
	JRZ	..2		;No, branch
;
	CALL	GETCHR		;Get the character, 'NZ' if function key
	JRZ	..3		;;Branch if char OK
..ERR:	CALL	PURGEK		;Yes, error, stop macro/repeat
;
..2:	LHLD	VSTEMP		;HL-> message
	CALL	STAKEY		;Give prompt, A = reply
				;Jumps to VLOOP if CTRL-C or [CANCEL] pressed
..3:	ANI	5FH		;Change LC to UC
	LHLD	VSTMP1		;HL-> decode table
	CALL	CHOICE		;HL = address of desired routine
	JRC	..ERR		;Prompt over if incorrect response
;
	XRA	A		;Put 00 into A
	PCHL
	EPROC	VSMENU
	.PAGE
;************************************************
;*						*
;*	Search / Replace Functions		*
;*						*
;************************************************
;
; VFIND - Search for string with prompt on status line
;
;	Usage of FNDFLG:
;		0 = No FIND or REPLACE setup (Was Canceled)
;		1 = FIND is all setup
;		2 = REPLACE is all setup
;		3 = REPLACE without prompting in progress
;
;	Usage of REPLFL:
;		0 = Normal
;		1 = VLOOP jumps to VFNEXE after screen update
;		2 = VEDCMD jumps to VFNEXE immediately
;
	DSEG	$
VFNDCM:	DC	'_B  :F' [ESC] [ESC] [00]
	CSEG	$
;
	BPROC	VFIND
VFIND:	MVIB$	RPTFLG,2	;Special value - not building REPEAT string
	TSTM	FNDFLG		;Is FIND all set up?
	MVI	M,1		;Will be setup now
	JRNZ	..1		;Yes, go execute it
	CALL	VGTFND		;Prompt and get FIND string
..1:	JMP	FNDEXE		;Search for the string
	EPROC	VFIND
				;Returns to VFNEXE after rewrite
;
; VREPLC - Replace one string with another
;
	BPROC	VREPLC
VREPLC:	MVIB$	RPTFLG,2	;Special value - not building REPEAT string
	CPIM	FNDFLG,2	;Is REPLACE all set up?
	BEQ	..2		;Yes, find next occurrence
	MVI	M,2		;Will be setup now
;
	CALL	VGTFND		;Get the FIND string
	JRC	..2		;Branch to reuse last replace
;
..1:	LXI	H,RPLMSG	;HL-> "REPLACE WITH? "
	LXI	B,REPLST	;BC-> replace string buffer
	CALL	VGTSTR		;Prompt for and get REPLACE string
	JRZ	..1		;Prompt over if CTRL-X
	SHLD	RPSTEN		;Save -> end of replace string
;
..2:
;	JMP	FNDEXE		;Search for the string
				;Returns to VFNEXE after rewrite
	EPROC	VREPLC
	.PAGE
;
; FNDEXE - Search for the string.  'NC' if string found.  Else
;	   give error message and echo string on status line
;
				;{VFIND, VREPLC above!}
	BPROC	FNDEXE
FNDEXE:	CALL	FRMVIS		;HL = current edit position
	SHLD	FINDPT		;Save in case of search error
				;(Note: FINDPT only used when REPLFL set)
;
	TST$	VGLBFL		;Global search? (GLOBSW can force global)
	MVI	B,SPACE		;Set B = SPACE
	MVI	C,'_'		;Assume yes, get global symbol
	JRNZ	..1		;Yes, branch
	MOV	C,B		;No, get space
..1:	LXI	H,VFNDCM+3	;HL-> byte preceeding :F command
	MOV	M,C		;Set '_'/SPACE, retain value in %C for later

	DCX$	H		;HL-> backward search modifier
	MOV	M,B		;Blank it out
	TST$	BCKFLG		;Is this backward search?
	JRZ	..2		;No, branch
	MVI	M,'-'		;Yes, set modifier

..2:	TST$	BEGFLG		;Need to move to beginning?
	JRZ	..3		;No, branch
	DCX$	H		;Yes, HL-> 'B' command
	DCX$	H		;HL-> global modifier byte for 'B' command
	MOV	M,C		;Set '_'/SPACE saved from above

..3:	CLR	BEGFLG		;Only go to beginning once
	INR	A		;Get a 1
	STA	REPLFL		;Set Find/Replace flag
	JMP	VISMAC		;Execute HL's string in command mode
	EPROC	FNDEXE
;
; VFNEXE - Following the screen rewrite, execution continues here.
;	   Enter: SRFAIL !=0 if search failed
;		  EDTPTR-> past end of string, SAVEPT-> begin of string
;
				;{VEDCMD,VLOOP}
	BPROC	VFNEXE
VFNEXE:	CLR	REPLFL		;Clear Find/Replace flag
	TST$	SRCERR		;Did the search fail?
	JRZ	..OK		;No, branch
;
;	Don't give error message if "Rest" replace
;
	CPIB$	FNDFLG,3	;Is this global (Rest) replace?
	CALL	VRPCAN		;Clear FNDFLG and OPSTCK (Save flags)
	BNE	..ERR		;;No, give error
	CALL	RSTOUT		;;Yes, restore Com-Mode LSTFLG
	JMP	VEDCM1		;;Rewrite final screen
;
;	Put error message on status line
;
..ERR:	LXI	H,CNFMSG	;HL-> "CAN NOT FIND"
	LXI	D,TARGST	;HL-> string
	LXI	B,SRCEOS	;Search string ends on SRCEOS char
	MVI	A,2CH		;Wait for keypress, 2 msgs, terminators in BC
	JMP	MSGHND		;Display message on status line
;
..OK:	LDA	FNDFLG		;Get find mode
	CPI	3		;Replace "Rest"?
	JZ	VRPEX4		;Yes, branch
	CPI	2		;Replace with prompt?
	JZ	VRPEXE		;Yes, branch
	RET			;No, all is done
	EPROC	VFNEXE
	.PAGE
;
; VRPEXE - Following the screen rewrite, get the REPLACE option.
;
				;{VFNEXE}
VRPEXE:	MVIB$	RPTFLG,2	;Disable REPEAT process
	LXI	H,RPLPMT	;HL-> Option prompt
	CALL	STAPRM		;Give the prompt
	CALL	STADON		;Close status line
	CALL	CHKCUR		;Position cursor after string
	CALL	GETKEY		;Get single key-stroke
	JRC	VRPCAN		;;Cancel if [CANCEL] pressed
	CPI	CTRLC		;User abort?
	BEQ	VRPCAN		;Yes, cancel operation
	CPI	' '
	RZ			;<SP> is same as NO
	CPI	CR
	BEQ	VRPEX4		;<CR> is same as YES
	ANI	5FH		;Change LC to UC
	CPI	'Y'
	BEQ	VRPEX4		;Branch if YES
	CPI	'N'
	RZ			;Return if NO, REPEAT may skip to next
	CPI	'R'
	BEQ	VRPEX3		;Branch if "Rest" or Global
	CPI	'C'		;Cancel?
	BNE	VRPEXE		;No, prompt again
;
				;{FNDEXE,CANCEL}
VRPCAN:	PUSHF			;Save flags for FNDEXE
	CLR	FNDFLG		;Disable auto-FIND
	STA	OPSTCK		;Disable REPEAT
	CALL	RPTRST		;Disable REPEAT
	POPF			;Restore flags
	RET
;
VRPEX3:	MVIB$	FNDFLG,3	;Set flag that now in global/no-prompt mode
	CLR$	VISFLG		;Prevent FRMVIS() problems on exit
;
;	Perform the Replace by adjusting main text up or down
;
VRPEX4:	LHLD	EDTPTR		;HL-> past search string
	LDED	SAVEPT		;DE-> begin of string
	CALL	MXHLDE		;Make sure HL > DE (in case reverse search)
	PUSH	D		;Save -> begin
	PUSH	H		;Save -> text past string
	CALL	SBHLDE		;BC = length of string to replace
	LHLD	RPSTEN		;HL-> SRCEOS in replace string
	LXI	D,REPLST	;DE-> replace string
	DSUB	D		;HL = length of replace string
	MOV	D,H
	MOV	E,L		;Save length of replace string
	DSUB	B		;HL = difference (+) -> buffer moves up
	MOV	B,H		;Put difference in BC
	MOV	C,L
	POP	H		;HL-> begin of text to move up/down
	PUSH	D		;Save LEN|REPLST|
	BLT	VRPEX5		;Branch if new < old
;
;	New > old, so move text buffer up
;
	CALL	BUFFUP		;Move text up
	JC	BREAK		;Give error if no space
	JMPR	VRPEX6
;
;	New < = old so move text buffer down
;
VRPEX5:	CALL	BUFFDW		;Move text down
VRPEX6:	POP	B		;BC = length of replace string
;
	POP	D		;DE-> space for new string
	LXI	H,REPLST	;HL-> replace string
	CALL	MOVEBC		;Copy in new string
	CALL	STNSNL		;Make sure screen rewritten
;
	CPIB$	FNDFLG,3	;Is this global (Rest) replace?
	LHLD	EDTPTR		;Assume NO
	JC	TOVIS2		;No, rewrite screen using EDTPTR
;
;	Housekeeping for [REPLACE]-"Rest"
;
	CLR$	FNDFLG		;Clear flag in case of BREAK
	CALL	BRKCHK		;Check for CTRL-C BREAK
				;(By 2nd time INPFLG = 2DH)
	MVIB	REPLFL,2	;Set "Rest" Replace flag
	INR	A		;AL = 3
	STA	FNDFLG		;Reset FNDFLG=3
	LXI	H,VFNDCM+2	;HL-> "xx:F$$" command
	JMP	VISMAC		;Execute in command mode
	.PAGE
;
; VGTFND - Prompt for and have user enter FIND string [5/13/86]
;	   Retrn: 'C' to reuse previous string
;		  VGLBFL,RUSFLG,BCKFLG and BEGFLG set/reset
;
	BPROC	VGTFND
VGTFND:	MVIB	TERMCH,ESC	;Use default terminating char
	IF	VPLUS, [
	STA	SAVTRM		;For |Pr
	]
	MVIW	VGLBFL,0000	;Clear VGLBFL and RUSFLG
	ST16	BCKFLG,H	;Clear BCKFLG and BEGFLG
;
..1:	LXI	H,FNDMSG	;HL-> "FIND? "
	LXI	B,TARGST	;Use search string buffer
	CALL	VGTSTR		;Prompt on status line and get user input
	JRC	..OPT		;Get options if empty string
	JRZ	VGTFND		;Prompt over if CTRL-X
	RET
;
;	Get search Options (Begin,Global,Reuse)
;
..OPT:	LXI	H,OPTMSG	;HL-> "OPTIONS?
	CALL	VSGSTR		;Prompt, HL-> reply string [00]
..3:	MOV	A,M
	INX$	H
	ORA	A		;End of string?
	BEQ	..9
	ANI	5FH		;Force upper case
	CPI	'B'		;Go to file beginning?
	JRNZ	..4		;No, branch
	STA	BEGFLG		;Yes, set Begin flag
	JMPR	..3
;
..4:	CPI	'G'		;Global?
	JRNZ	..5
	MVIB$	VGLBFL,'_'
	JMPR	..3
;
..5:	CPI	'R'		;Reverse search?
	JRNZ	..6
	STA	BCKFLG
	JMPR	..3
;
..6:	CPI	'A'		;Again (re-use)?
	JRNZ	..3		;Else ignore it
	STA	RUSFLG
	JMPR	..3
;
..9:	TST$	RUSFLG		;Is re-use flag set?
	STC			;Assume yes
	RNZ			;Yes, return 'C'
	JMPR	..1		;No, back to FIND prompt
	EPROC	VGTFND
	.PAGE
;
; VGTSTR - Display message in (HL) on status line.
;	   Get user string into (BC).  End in SRCEOS
;	   Return: 'Z' and 'NC' if CTRL-X typed to prompt over.
;		   'C' and 'NZ' if RETURN immediately typed.	(5/13/86)
;
				;{VFLNEW,VREPLC,VGTFND,VSGSTR}
	BPROC	VGTSTR
VGTSTR:	PUSH	B		;Save -> input buffer
	CALL	STAPRM		;Give prompt on status line
				;This also set LSTFLG to LVLVCM
	POP	H		;HL-> string storage
	PUSH	H
	LXI	B,TARGLN-5	;Allow 73 characters
	DAD	B
	POP	D		;DE-> storage, HL-> max
;
	CALL	GETST0		;Get user input, without adding CR-LF
	PUSHF
	CALL	STADON		;Close status line (save BDH)
	MVIB$	INPFLG,80H	;Enable function key decoding
	POPF			;Return code from GETST0
	JRC	CANCEL		;CANCEL if CTRL-C or [CANCEL] pressed
				;DE-> first char, HL-> past last
	RZ			;Return 'Z' if CTRL-X typed
;
	CALL	CMHLDE		;Empty search string?
	BNE	..1		;No, branch
	ORI	1		;Yes, set 'NZ
	STC			;Also return 'C'
	RET
;
..1:	MVI	M,SRCEOS	;Terminate with ESC (or EOS)
	JMP	RET%NZ		;Return 'NZ', HL-> terminator
	EPROC	VGTSTR
;
; VSGSTR - Prompt and get reply string into STRBUF.
;	   Enter: HL-> prompt.
;	   Retrn: HL-> reply string.
;
				;{GOLINE,VWICRE}
VSGSTR:	LXI	B,STRBUF	;Use GP string buffer
				;;Note: 8080 size could overflow!  8086 OK
	PUSH	H
	CALL	VGTSTR		;Get user reply
	MVI	M,00		;End reply in [00]
	POP	H		;HL-> prompt
	JRZ	VSGSTR		;Prompt over if CTRL-X
	XCHG			;HL-> begin of reply string
	RET
	.PAGE
;
; CANCEL - Cancel FIND/REPLACE, first text point, status line prompt
;
				;{VLOOP,VGTSTR}
CANCEL:	CALL	STVTSW		;Clear MOVE/COPY first PTR
	CALL	VRPCAN		;Clear FNDFLG and OPSTCK
	CALL	POLRST		;;Empty type-ahead buffer
				;{GETRNM,STAKEY}
CANCL1:	CALL	STADON		;Close status line if open
	CLR	CNCLFL		;Clear cancel flag
	STA	OPSTCK		;Disable pending operation
	CALL	RPTRST		;Disable REPEAT
	JMP	VLOOP		;Continue
	.PAGE
;************************************************
;*						*
;*		Delete Functions		*
;*						*
;************************************************
;
; BKSPAC - Perform Backspace by Cursor Left and Delete.
;
	BPROC	BKSPAC
BKSPAC:	CALL	HORFIX
	CPIW	ACTPNT,ACTBUF	;At start of active line?
	BNE	..1		;No, ok skip
	CMPW	TXTBGN,TXACTV	;Yes, at start of text buffer?
	RZ			;Yes, don't do anything (Bug fix)
..1:	CALL	CRLEFT		;Move the cursor left
	LXI	D,DELACT	;Address of delete routine
	JMP	PUSHOP		;Add to operation stack
	EPROC	BKSPAC
;
; DELACT - Delete a character from active line if valid.
;
				;{[DELETE]}
DELACT:	CALL	HORFIX		;Make sure cursor set properly
	CALL	ACTEOL		;;Does ACTPNT -> EOF?
				;DE = ACTPNT
	RC			;Yes, don't delete EOF
	LXI	B,1		;Assume deleting single char
	JRNZ	DELAC1		;Delete 1 char if not LF or CR-LF (single CR OK)
	CALL	STPSNL		;Set write partial screen, get new text line
	LDAX	D		;;Get char to delete
	CPI	LF		;;Is it a LF?
	BEQ	DELAC1		;;Yes, delete just it
	INX$	B		;;No, delete the CR-LF
;
;	Delete 'BC' characters from ACTPNT in active line.
;
				;{INDENT,EREOL,DLPVWD}
DELAC1:	LHLD	ACTPNT		;HL-> first char. to delete
	DAD	B		;HL-> first char. to move down
	LXI	D,ACTBUF+ACTLEN	;DE-> terminating CRLF
	CALL	CMHLDE		;Trying to delete term. CRLF?
	RNC			;Yes, don't!
	XCHG			;Move begin PTR to DE
	LHLD	ACTEND		;HL = end PTR
	CALL	MVTXDW		;Move text down
	SHLD	ACTEND		;Save new end PTR
	LXI	H,ACTSIZ	;HL -> length of active line
	CALL	SUBMBC		;New length = ACTSIZ - BC
	CALL	FRCSET		;Force cursor positioning
	JMP	STPWAC		;Partial rewrite active line
	.PAGE
;
; EREOL - Erase to end of line but not CR-LF unless that is
;	  all there is.
;
	BPROC	EREOL
EREOL:	CALL	ACTEOL		;Does ACTPNT -> EOF? (DE = ACTPNT)
	RC			;Yes, don't erase EOF
	JRNZ	EREOL1		;Branch if not at LF or CR-LF
	LXI	H,ACTBUF	;HL-> begin active line
	CALL	CMHLDE		;At line beginning?
	RNZ			;No, don't erase
	JMPR	DELACT		;Yes, erase CR-LF
;
;	Delete the characters from (DE) up to a CRLF or EOF.
;
				;{EREOL,ERLINE}
EREOL1:	CALL	ENDCHK		;Does active line end in CRLF?
	BNE	..2		;No, HL-> last char
	DCX$	H		;Yes, leave CR alone too
..2:	CALL	SBHLDE		;BC = # chars to delete
	JMPR	DELAC1		;Delete the chars
	EPROC	EREOL
;
; ERLINE - Erase entire line, unless it ends in EOF.
;
ERLINE:	CALL	STPSNL		;Set for partial screen write, new text line
	CALL	ENDCHK		;Does line end in EOF?
	MVIW	ACTPNT,ACTBUF	;Assume yes, move edit pointer to line begin
	XCHG			;;DE = new ACTPNT
	JRC	EREOL1		;Yes, erase all but EOF
	MVIW$	ACTSIZ,00	;Set active line size to zero
	LES	H,SCRFCB	;HL-> address table
	MVDE%ES			;DE-> text for screen begin
	LHLD	TXACTV		;HL-> text for active line
	CALL	CMHLDE		;Is active point before or at screen begin?
	RNC			;No, return to write partial screen
	CALL	SCRBG		;Yes, set first value of SCRFCB
	JMP	STNSNL		;Write new screen, set new line
;
; VUNDO - Rewrite original active line.
;
VUNDO:	CALL	STPSNL		;Bits for partial screen, new line
	LXI	D,FRCSET	;Setup to re-position cursor
	CALL	PUSHOP		;Add to operation stack
	JMP	VMAIN5		;Rewrite partial screen
	.PAGE
;************************************************
;*						*
;*	Horizontal Scrolling Routines		*
;*						*
;************************************************
;
;
; HZLEFT - Move the screen window left and move cursor left
;
HZLEFT:	CALL	HZRTLF		;Move screen left
	JMPR	HZRGH1		;Merge below
;
; HZRGHT - Move the screen window right and move cursor right
;
HZRGHT:	CALL	HZRTRG		;Move screen right
HZRGH1:	LDA	CURHOR		;Get virtual cursor pos
	CALL	RNGCHK		;Is CURHOR within scroll window?
	RNC			;Yes, return
;
	ADD	D		;No, adjust for scroll
	STA	CURHOR		;Ensure value within scroll window
	RET
;
				;{HZRGCK,HZLEFT}
HZRTLF:	CALL	HZICMX		;;A = allowable increment
	MOV	C,A		;;Save in C
	LDA	VSHZBG		;;Get current value
	SUB	C		;;Compute new value
	BGE	HZRTL1		;Branch if position good
	XRA	A		;Else change to zero
HZRTL1:	MOV	C,A		;Put new value in C
	LXI	H,VSHZBG	;HL-> window begin column
	SUB	M		;Compute difference
	MOV	D,A		;Save in D
	MOV	M,C		;Set new value
	CALL	STBRFL		;Force new status line
	CALL	STNSKL		;Force screen rewrite
	JMP	WISCST		;Set HZSCBG and HZSCEN
				;Return D = difference
;
				;{HZRGCK,HZRGHT}
HZRTRG:	CALL	HZICMX		;;A = allowable increment
	LXI	H,VSHZBG	;;HL-> current value
	ADD	M		;;Compute tentative value
	MOV	C,A		;Save tentative value
	SUBB	HZSCLN,WWLLE1	;A = max allowed screen begin column
	CMP	C		;Is max-allowed << tentative?
	BLT	HZRTL1		;Yes, use max allowed value
	MOV	A,C		;No, use tentative value
	JMPR	HZRTL1		;Merge above
;
HZICMX:	LDA	WWLLEN
	SUI	5
	MOV	C,A
	LDA	HZSCIC		;;A = preferred value
	CMP	C		;;Is preferred value < (WWLLEN-5)?
	LXI	H,VSHZBG	;;HL-> current scroll value
	RC			;;Yes, retunr HZSCIC
	MOV	A,C		;;No, return max
	RET
	.PAGE
;************************************************
;*						*
;*		Cursor Movements		*
;*						*
;************************************************
;
				;{MOVACT,BACKTB}  (7/23/85 for VPLUS)
CRUP:	LXI	H,CRVTTP	;HL-> top line for cursor
	LDA	CURVER		;Get current cursor pos
	DCR	A		;Try new cursor pos
	CMP	M		;Is cursor pos. OK?
	BGE	CRDOW7		;Yes, set new position
	CALL	CRUPRT		;No, try scrolling
;
	JRZ	CRUP4		;Check for auto buffering if at top of screen
;
	JRNC	CRDOW8		;Adjust if cursor moved up
CRUP1:	CALL	STNWLN		;Set for new line
CRUP2:	MVI	A,-1		;Set to scroll backward
				;{CRDOW9}
CRUP3:	STA	NSCROL		;Set scroll direction
	MVI	A,40H		;Bit to scroll screen
	JMP	STNWFL		;Set NWSCFL

CRUP4:	LXI	D,CRUP		;Come back to this routine after auto buffering
	LXI	H,8		;Do 1K (8 sectors) worth of auto buffering
	JMP	CHKRVB		;Go check for/perform auto reverse buffering
;
				;{MOVACT}
CRDOWN:	MVI	B,1		;Move down one line
				;{CRZIP}
CRDOW1:	CALL	CNTVER		;HL-> cont. flag for cursor line
	MOV%ES	A,M		;Get the flag
	ORA	A		;EOF reached on this line?
	JRM	JSTACP		;Yes, set new ACTPNT
	JMPR	CRDOW3
				;{WTACLN,VCHCR,CRTAB}
CRDOW2:	MVI	B,1		;Move down one line
CRDOW3:	LXI	H,CURVER	;HL-> current cursor position
	MOV	A,M		;Get current pos
	ADD	B		;Add movement
	MOV	B,A		;Save desired line
	LDA	CRVTBT		;Get bottom allowed line for cursor
	MOV	M,A		;Assume cursor goes on bottom allowed line
	SUB	B		;Desired line # past allowed line # ?
	JRNC	CRDOW6		;No, branch
;
;	Setup to scroll screen forward
;
CRDOW4:	CALL	STNWLN		;Set for new text line
CRDOW9:	MVI	A,1		;Set to scroll forward
				;Temporarily allow single line scroll only
	CALL	CRUP3		;Set to scroll screen
	MVI	A,1		;Scrolling one line
				;{PAGEDW}
CRDOW5:	INR	A		;Adjust for next line
	PUSHA
	CALL	LINPOS		;;HL = LOGPOS for new line
	CALL	STLGTP		;;Save in POSTBL[0] for first line
	POPA
	CALL	FCBADD		;HL = screen address for new line
	JMP	SCRBG		;Save in SCRFCB[0] for first line
;
;	Check if moved to new text line.
;
CRDOW6:	MOV	A,B		;No, get desired line
				;{CRUP,SCRNTG}
CRDOW7:	STA	CURVER		;Save as new cursor pos
				;{CRUP}
CRDOW8:	LD16	H,FSACSC	;L = first, H = last active screen #
	CMP	L		;Cursor on new text line?
	JC	STNWLN		;Yes, set flag, return
	DCR	A		;Adjust for next compare
	CMP	H		;Cursor on new text line?
	JNC	STNWLN		;Yes, set flag, return
JSTACP:	JMP	STACPN		;No, just set new ACTPNT
;
SCRLUP:	CALL	CRUPRT		;Find new screen beginning
	RZ			;Return if at begin of file
	JNC	STNWLN		;Branch if cursor moved up, no scroll
	LDA	CURVER		;Get cursor pos
	LXI	H,CRVTBT	;Compare to last allowed line
	CMP	M		;Is it too low on screen?
	JRNC8	CRUP1		;Yes, then leave on same screen line
;
	INR	A		;This moves cursor down
	STA	CURVER
	JMPR8	CRUP2		;Merge above
;
SCRLDW:	CALL	CNTVER		;ES:HL-> cont. flag for cursor line
	MOV%ES	A,M		;Get the flag
	ORA	A		;EOF reached on this line?
	RM			;Yes, return
	LXI	H,CURVER	;HL-> cursor pos
	LDA	CRVTTP		;Compare to top allowed pos
	CMP	M		;Is the cursor too high?
	JRNC	CRDOW4		;Yes, scroll up, leave cursor
;
	DCR	M		;Move cursor up
	JMPR	CRDOW9		;Set to scroll screen
;
; CRUPRT - Attempts to scroll up one line, or move cursor up	[3/5/86]
;	   if at Text Buffer begin.
;	   Return: 'C' if scrolled and new SCRFCB[0] set.
;		   'NC' & 'Z' if cursor on first text line.
;		   'NC' & 'NZ' if cursor moved up one line.
;
CRUPRT:	LES	H,SCRFCB	;HL-> table
	MVHL%ES			;HL-> text for begin of screen
	CALL	BACKLN		;Move back one screen line
	PUSHF			;Save flags
	CALL	SCRBG		;Save new screen begin
	POPF
	RC			;Return if new SCRFCB[0] (NZ)
	LDA	CURVER		;Else get line # for cursor
	DCR	A		;Move cursor up
	RZ			;Return if invalid position
	STA	CURVER		;Save cursor line
	RET			;Return with 'NZ'
;
; SCRNTG -
;
	BPROC	SCRNTG
SCRNTG:	CMPMB	CURVER,CRVTBT	;Is cursor on bottom allowed line?
	BNE	..2		;No, move to bottom line
..1:	LDA	CRVTTP		;Yes, get top allowed line
	JMPR8	CRDOW7		;Move to top line
;
..2:	LXI	H,LINCNT	;HL-> # lines on screen
	CMP	M		;Check against last line on screen
	JRZ	..1		;If on last line go to top line
	LDA	CRVTBT		;Tentatively to last allowed line
	CMP	M		;Trying to go past last?
	BLT8	CRDOW7		;No, put cursor on bottom line
	MOV	A,M		;Yes, put cursor on last line
	JMPR8	CRDOW7
	EPROC	SCRNTG
;
LINETG:	CALL	ZIPCHK		;At end of line or end of window
	BNE	CRZIP		;No, then perform ZIP
				;Else perform BACKTB
;
BACKTB:	CALL	CURPOS		;;HL =  LOGPOS for current line
	LDED	LOGHOR		;;DE = logical cursor pos
	CALL	CMHLDE		;;Is cursor horiz. at first possible column?
	MVIB$	CURHOR,1	;Move to column one in any case
	JZ	CRUP		;Yes, move up to previous line
BACKT1:	JMP	STACPN		;No, move to begin of current line
;
CRTAB:	CALL	FRCSET		;Prevent cursor from going past EOL
	LHLD	LOGHOR		;Get logical cursor position
	CALL	FNDTAB		;Find next Tab position
	MOV	L,A		;Set new position
	RZ			;Return if invalid position
	CALL	DVHOR1		;A = CURHOR and B = continuation
	CALL	SETHOR		;Set EDTHOR and CURHOR
	LDA	CURCNT		;Get continuation of old cursor
	CMP	B		;Same as new?
	JC	CRDOW2		;No, cursor down
	JMPR	BACKT1		;Set edit pointer from cursor position
;
	BPROC	CRZIP
CRZIP:	CALL	FRCST0		;Force cursor pos. adjustment
				;But not if off left side of screen
	CALL	ZIPCHK		;Are we at end of line or end of window?
	MVI	B,1		;Assume Yes - move to next line
	JRZ	..1		;Yes, move to next line
	DCR	B		;No, find end of current line
;
..1:	LDA	CURVER		;Get line # cursor is on
	ADD	B		;Add offset to start looking
	DCR	A		;;Adjust
	CMPM	WWNLIN		;Looking past end of window?
	BGE	..5		;Yes, get new screen
	INR	A		;;Restore
	CALL	CNTSCR		;ES:HL-> flag for this line
..2:	MOV%ES	A,M		;Get the flag
	ANI	06H		;Mask "next continuation" bits
	CPI	04H		;Is continuation off screen?
	BGE	..5		;Yes, branch to rewrite screen
	ANI	02H		;Is next line a continuation?
	JRZ	..3		;No, found line to ZIP to
	INR	B		;Yes, increment counter
	INX$	H		;ES:HL-> next line flag
	JMPR	..2		;Continue
;
;	ZIP is allowed to scroll the screen at most one line.
;
..5:	MVI	B,1		;Move down one in case of continuation
..3:	LDA	HZSCEN		;Get displayed line length
	DCR	A		;Last window column
				;{CRNXLN,MOVACT}
CRZIP4:	STA	CURHOR		;Move cursor to end of line
	JMP	CRDOW1		;Move the cursor down by # lines in B
	EPROC	CRZIP
;
; ZIPCHK - Return 'Z' if at end of line or end of window
;
ZIPCHK:	CALL	ACTEOL		;Are we at end-of-line (EOF, LF or CR-LF)?
	RZ			;Yes, return 'Z'
;
	LXI	H,CURHOR	;HL-> virtual pos
	LDA	HZSCEN		;Get screen end pos
	DCR	A		;A = last screen pos
	CMP	M		;Cursor at last pos?
	RET
;
; CRNXLN -
;
	BPROC	CRNXLN
CRNXLN:	CALL	CNTVER		;HL-> flag for this line
	MVI	B,01		;Init counter
..1:	MOV%ES	A,M		;Get the flag
	ANI	06H		;Mask "next continuation" bits
	CPI	04H		;Is continuation off screen?
	BGE	..2		;Yes, branch to rewrite scren
	ANI	02H		;Is next line a continuation?
	MVI	A,1		;Assume no, set to column 1
	JRZ	CRZIP4		;No, found line to move to
	INR	B		;Yes, increment counter
	INX$	H		;HL-> next line flag
	JMPR	..1		;Continue
;
..2:	CALL	BCKATV		;Save the active line
				;HL-> begin of next line or to EOF
	JMP	TOVIS1		;Set screen from text pointer
	EPROC	CRNXLN
	.PAGE
;
; PAGEDW - Move down by customized size of page.
;	   Reg. B used as NEWVER = minimum # screen lines needed.
;
	BPROC	PAGEDW
PAGEDW:	CALL	BCKATV		;Update edit buffer
	TST$	EOFSCR		;EOF reached on window?
	LDA	WWNLIN		;Get number of lines in window
	LXI	D,CRDOWN	;Use cursor down routine
	JNZ	PSHOPC		;Yes, set OP stack to cursor down
;
	CALL	STNWLN		;Definitely set new edit point
	LXI	H,CURVER	;HL-> line # cursor is on
	MOV	B,M		;B = line # cursor is on
	LDA	CRVTTP		;Get top line for cursor
	MOV	D,A		;Save top line #
	SUB	M		;How far from top
	MVI	C,00		;Assume cursor pos. OK
	BLT	..4		;Branch if cursor OK
;
	MOV	C,A		;Save difference
	LDA	WWPGSZ		;A = # lines to page down
	CMP	C		;Is difference greater than a page?
	BGE	..1		;No, branch
;
	ADD	M		;A = line # to move to
	MOV	M,A		;Save as new CURVER
	RET			;Done here
;
..1:	MOV	B,D		;Put cursor on top line
..4:	MOV	M,B		;Save new cursor pos
	LDA	WWPGSZ		;# lines to move down
	SUB	C		;Subtract movement to reach CRVTTP
	RZ			;Return if no screen movement needed
	PUSHA			;Save preferred # lines to move down
	INR	A		;Adjust, point to next line
	CALL	SETWRT		;;Set DE-> tentative screen begin, set LOGPOS
;
;	B = desired CURVER after Page-Down.  If this many lines do not
;	exist in buffer, then reduce the number of lines we are going down.
;
				;B is predecremented for EOF line
..2:	DCR	B		;Decrement count
	JRZ	..3		;Branch if enough lines remain
	PUSH	B		;Save count
	MVI	A,-1		;Neg. # to write fake line
	CALL	WRTLIN		;Write a fake line
	POP	B		;Restore count
	JRNC	..2		;Branch if EOF not reached
;
..3:	POPA			;A = prefered # to move down
	SUB	B		;Subtact # unavailable lines
	RZ			;Ignore if no lines to move
	PUSHA			;Save line movement #
	CALL	STNSNL		;Set for new screen and new text line
	POPA			;Restore line movement #
	JMP	CRDOW5		;Set new screen begin
	EPROC	PAGEDW
;
; PAGEUP - Move up by customized size of page.
;
	BPROC	PAGEUP
PAGEUP:	LES	H,SCRFCB
	MVHL%ES			;Get current screen begin
	PUSH	H		;Save it
	LDA	WWPGSZ		;Get # lines to move up
	MOV	B,A		;Move up most of a screen
	MOV	C,A		;
..1:	PUSH	B		;Save counter
	CALL	CRUPRT		;Move up one line
	POP	B		;Restore counter
;
	JRC	..2		;Branch if scrolling occurred
	JRNZ	..2		;Branch if cursor moved up one line
	MOV	A,B		;Cursor is at text begin
	CMP	C		;Did we start at text begin?
	JRNZ	..2		;No, branch
	LXI	H,16		;Yes, do 2K (16 sectors) worth of buffering
	LXI	D,PAGEUP	;Come back to this routine after buffering
	POPA			;Adjust stack
	JMP	CHKRVB		;Go check for/perform reverse disk buffering
;
..2:	DJNZ	..1		;Repeat while more lines to go
;
;
	LES	H,SCRFCB
	MVHL%ES			;Get final screen begin
	POP	D		;Get old screen begin
	CALL	CMHLDE		;Same as old?
	JZ	CRDOW8		;Yes, don't rewrite screen
	JMP	STNSNL		;No, write new screen, set new line
	EPROC	PAGEUP
	.PAGE
;
;
;
CRLEFT:	LXI	B,-1		;Pointer change is -1
	JMPR	MOVACT		;Move edit pointer
;
CRRGHT:	LXI	B,1		;Pointer change is 1
;
; MOVACT - Move ACTPNT by value in BC if valid.
;
				;{CRRGHT above, CRLEFT)
	BPROC	MOVACT
MOVACT:	CALL	FRCSET		;Force cursor pos. adjustment
	LHLD	ACTPNT		;HL -> active line
	DAD	B		;Tentatively add change
	LXI	D,ACTBUF	;DE-> begin of active line
	CALL	CMHLDE		;Change valid?
	BGE	..1		;Yes, branch
	CMPW	TXTBGN,TXACTV	;Are we at begin of buffer?
	RZ			;Yes, don't move
	MOVB	CURHOR,HZSCLN	;No, move cursor to end of screen line
	JMP	CRUP		;Move to previous line
;
..1:	XCHG			;DE = tentative pos
	CALL	ENDCHK		;Does line end in CR-LF?
	PUSH	PSW		;Also save if end in EOF
	BNE	..2		;No, HL-> last allowed pos
	DCX$	H		;Yes, CR is last allowed pos
..2:	CALL	CMHLDE		;Change valid?
	BGE	..3		;Yes, branch
	POP	PSW		;Is last char an EOF?
	RC			;Yes, don't move
	MVI	A,1		;Move to column one
	MOV	B,A		;Move down one line
	JMP	CRZIP4		;Set CURHOR and perform CRDOWN
;
..3:	SDED$	ACTPNT		;Save new edit pointer
	POP	PSW		;Clear stack
	RET
	EPROC	MOVACT
	.PAGE
;************************************************
;*						*
;*	Visual Operation Support Routines	*
;*						*
;************************************************
;
; ENDCHK - Checks what character the active line ends in.
;	   Return: 'Z'and 'NC' if HL-> LF and previous char is CR.
;		  'NZ'and 'C' if HL-> EOF, else 'NZ' and 'NC'.
;
				;{UPDACT,EREOL,ERLINE,MOVACT}
	BPROC	ENDCHK
ENDCHK:	CPI%M	ACTEND,EOF	;Does active line end in EOF?
	BEQ	..2		;Yes, branch to return 'C' and 'NZ'
	CPI	LF		;Is last char LF?
	BNE	..1		;No, return 'NZ' and 'NC'
	DCX$	H		;HL-> previous char
	MOV	A,M		;Get char
	INX$	H		;Restore HL
	CPI	CR		;Is previous char. a CR?
..1:	RNC			;Return with 'NC'
	CMC			;Else change 'C' to 'NC'
	RET			;Return with 'NC' and 'Z' or 'NZ'
;
..2:	SBI	EOF+1		;Set 'NZ' and 'C'
	RET
	EPROC	ENDCHK
;
; BLKSPN - Returns in BC # of leading spaces and tabs on active line.
;	   HL-> first non space or Tab on line. A = char.
;
				;{INDENT,WRAPWD,JUSTY}
	BPROC	BLKSPN
BLKSPN:	LXI	H,ACTBUF	;HL-> begin of active line
				;{BLKLIN}
BLKSP0:	LXI	B,-1		;Init the counter
..2:	INX$	B		;Increment counter
	MOV	A,M		;Get next character
	INX$	H		;Bump active line PTR
	CALL	CMPBLK		;Is char Space or Tab?
	BEQ	..2		;Yes, continue to next char
	DCX$	H		;No, return HL-> char
	RET			;No ,return with non-blank char. in A
	EPROC	BLKSPN
;
; ACTEOL - Check if ACTPNT is at end of line (EOF, LF or CR-LF).
; DE%EOL - Check for (DE).
;	   Retrn: DE = ACTPNT; AL = char at (ACTPNT); HL, BC saved.
;		  'Z' if at end of line.
;		  'Z' and 'C' if at EOF;  else 'NZ and 'NC'.
;
				;{DELACT,EREOL,SETHOR}
	BPROC	ACTEOL
ACTEOL:	LDED	ACTPNT		;;DE = ACTPNT
				;{SETLOG}
DE%EOL:	LDAX	D		;;Get the char
	CPI	EOF		;;At EOF?
	STC
	RZ			;Yes, return 'C' and 'Z'
	CPI	LF		;;At LF?
	RZ			;;Yes, return 'NC' and 'NZ'
	CPI	CR		;;At CR?
	BNE	..2		;;No, return 'NZ' and 'NC'
	INX$	D		;;Yes, DE-> next char
	LDAX	D		;;Get char
	DCX$	D		;;Restore DE
	CPI	LF		;;Is next char an LF?
	LDAX	D		;;AL = char at (ACTPNT)
..2:	STC			;;Want 'NC' without changing 'Z'
	CMC			;;VOILA!
	RET
	EPROC	ACTEOL
;
; FRCSET - Set FRCFLG so that cursor is subsequently set from EDTHOR
;
				;{CRZIP}
FRCST0:	MVI	A,081H		;Set bit for special ZIP
	JMPR	FRCST2		;Merge below
				;{CRTAB,MOVACT,DELACT,PRWORD}
FRCSET:	MVI	A,1		;Get non-zero
FRCST2:	STA	FRCFLG		;Set flag
	RET
;
; SETHOR - Set EDTHOR and possibly CURHOR. Enter: %A = value for EDTHOR.
;
				;{WTACLN,CRTAB}
	BPROC	SETHOR
SETHOR:	PUSH	B
	STA	EDTHOR		;Set edit horizontal pos
	MOV	B,A		;Save
	LXI	H,FRCFLG	;HL-> force cursor pos. flag
	MOV	A,M
	ORA	A		;Need to force cursor?
	MVI	M,00		;Clear flag
	JRM	..5		;Branch to handle ZIP
	JRNZ	..1		;Yes, set CURHOR
;
	CALL	ACTEOL		;Are we at end-of-line (EOF, LF or CR-LF)?
	JRNZ	..1		;No, set CURHOR same as EDTHOR
;
	LDA	HORFLG		;Get hor. position option
	ORA	A		;Is it char oriented?
	JRNZ	..4		;No, return
;
..1:	MOV	A,B		;Yes, also set CURHOR
..3:	STA	CURHOR
..4:	POP	B
	RET
;
..5:	LDA	VSHZBG		;Get screen begin column
	CMP	B		;Trying to move off left side?
	BLT	..1		;No, set new CURHOR
	JMPR	..3		;Yes, move to first column
	EPROC	SETHOR
;
; HORFIX - Move Cursor to correct EDTHOR position (10/5/85)
;
				;{CHGACT,ADDACT,BKSPAC,DELACT}
	BPROC	HORFIX
HORFIX:	CALL	HORCHK		;Is CURHOR = EDTHOR?
	RZ			;Yes, nothing to do (allow patch)
	LDA	HOROPT		;Get user option
	CPI	2		;Need to fill?
	JRZ	..2		;Yes, branch
;
..1:	MOVB	CURHOR,EDTHOR	;Set screen pos. from edit pos
	JMP	CHKCUR		;Make sure cursor positioned
				;May set NWSCFL if horiz. scrolling needed
;
..2:	SUBB	CURHOR,EDTHOR	;A = difference
	JRC	..1
	PUSHA
	MOVB	CURHOR,EDTHOR	;Be sure CURHOR = EDTHOR to prevent infinite loop
	POPA
	JMP	ADDSP		;Add '%A' spaces to active line
	EPROC	HORFIX
;
;
;
HORCHK:	CMPMB	CURHOR,EDTHOR
	RET

