	.PAGE
	.TITLE	'VEDITG2'
;************************************************
;*						*
;*	General Purpose Routines		*
;*						*
;************************************************
;
;	NOTE:   These routines are completely independent of VEDIT variables.
;		They are register only routines.  They reference no EXTERNALs.
;
; Copyright (C) 1986 by CompuView Products, Inc.
;			1955 Pauline Blvd.
;			Ann Arbor, MI 48103
;
;	Written by: Theodore J. Green
;
;	Last Change: Ted - Jan. 30 - MULTIP, SV%ALL, string routines added
;			 - Feb. 20 - Lots of misc.
;			 - Mar. 07 - ADINDE - add (HL) to DE
;			 - Apr. 27 - CHOICE - new routine
;		     Tom - May. 05 - LOOKCH return 'NZ' for EOT
;		     Ted - May  19 - REVCAS - return char in A and C
;			 - Oct. 13 - 8086 Optimizations
;			 - Nov. 12 - Comment out CRLFPR()
;
; SV%ALL - Save all registers, including flags
; SV%BDH - Save BC, DE and HL (return value to invoker's CALLer in AF only)
; SV%BD	 - Save BC and DE (return value in AF and/or HL)
;
;	   These routines MUST be called before anything is PUSHed on stack.
;
;	   A CALL to these routines actually terminates the invoking routine.
;	   These routines Save registers, CALL the rest of their invoking
;	   routine, then return to their invoker's CALLer.
;
; NOTE   - Cannot pass parameter in HL ***************************
;
SV%ALL:	XTHL
	PUSH	D
	PUSH	B
	PUSH	PSW		;Save flags too
	CALL	CALLHL		;Call the rest of the CALLing routine
	POP	PSW
	POP	B
	POP	D
	POP	H
	RET			;Return one level back
;
SV%BDH:	XTHL
	PUSH	D
	PUSH	B
	CALL	CALLHL		;Call the rest of the CALLing routine
	POP	B
	POP	D
	POP	H
	RET			;Return one level back
;
SV%BD:	XTHL
	PUSH	D
	PUSH	B
	CALL	CALLHL		;Call the rest of the CALLing routine
	POP	B
	POP	D
	XTHL			;Put HL on stack
	POP	H		;HL = return value for CALLing routine
	RET			;Return one level back
;
; CALLDE - CALL (DE) or JMP (DE).
;
CALLDE:
JUMPDE:	PUSH	D
	RET
;
; CALLHL - CALL (HL).
;
CALLHL:	PCHL
;
; CONVUC - Convert lower case letter in A to upper case.
;
				;{VCHAR,FCBCHR}
CONVUC:	CPI	'a'		;Is char at least a 'a'?
	RC			;No, not lower case
	CPI	'z'+1		;Is char greater than 'z'?
	RNC			;Yes, not lower case
	ANI	5FH		;Else convert lower to upper case
	RET
;
; REVCAS - Reverse upper/lower case for letters.
;
				;{VGTKEY,VCHAR}
	BPROC	REVCAS
REVCAS:	CALL	LETCHK		;Is the char a letter?
	JRNZ	..1		;No, don't change it
	XRI	020H		;Reverse upper and lower case
..1:	MOV	C,A		;Also return char in C
	RET
	EPROC	REVCAS
;
; CMPCLE - Compare A for CR, LF or EOF.
;
SLINCH:	LDAX	D		;Get text char in search
	CPI	0CH		;FF?
	RZ
	JMPR	CMPLE
;
CMPCLE:	CPI	CR		;CR?
	RZ			;Yes, return
;
CMPLE:	CPI	LF		;LF?
	RZ
	CPI	EOF		;EOF?
	RET			;Return with flags
;
; CMPCE - Return 'Z' if A is one of {CR,ESC}.
;
CMPCE:	CPI	CR
	RZ
	CPI	ESC
	RET
;
; LETCHK - Return with 'Z' if char in A is a letter.
;
	BPROC	SLETCH
SLETCH:	LDAX	D		;Get char in search
LETCHK:	CPI	'A'		;Is at least an 'A'?
	RC			;No, return 'NZ'
	CPI	'z'+1		;Is it greater than 'z'?
	BGE	RET%NZ		;Yes, return 'NZ'
	CPI	'a'		;Is it lower case?
	BGE	RET%Z		;Yes, return 'Z'
	CPI	'Z'+1		;Is it upper case?
	BLT	RET%Z		;Yes, return 'Z'
;
RET%NZ:	ORA	A
	RNZ			;Return 'NZ' & 'NC'
	CPI	1		;A = 00, return 'NZ' & 'C'
	RET
	EPROC	SLETCH
;
; DIGCHK - Check for Decimal Digit.  Return 'Z' if digit.
;
SDIGCH:	LDAX	D		;Get char in search
DIGCHK:	CPI	'0'
	RC			;Return 'NZ' if < '0'
	CPI	'9'+1
	BGE	RET%NZ		;Return 'NZ' if > '9'
RET%Z:	CMP	A		;Set 'Z'
	RET
;
; BLKCHK - Return 'Z' if char. in A is Space or Tab.
;
SBLKCH:	LDAX	D		;Get char in search
CMPBLK:
BLKCHK:	CPI	SPACE
	RZ
	CPI	TAB
	RET
;
; SKIPWT - Skip over whitespace starting at (HL)
;
SKIPWT:	MOV	A,M		;Get next char
	CALL	BLKCHK		;It it space or tab?
	RNZ			;No, HL-> non-blank
	INX$	H		;Bump
	JMPR	SKIPWT		;Loop

;
; CRLFPR - Check if HL-> CR-LF or LF-CR.
;	   Return: 'Z' & HL+ if true, else 'NZ'.
;
				;{SMATCH}
;;	BPROC	CRLFPR
;;CRLFPR:	MOV	A,M		;Get first char
;;	CPI	CR		;Is it CR?
;;	BEQ	..1		;Yes, look for LF
;;	CPI	LF		;No, is it LF?
;;	RNZ			;Return 'NZ' if neither
;;	INX$	H		;Yes, HL-> 2nd char
;;	MOV	A,M		;Get 2nd char
;;	CPI	CR		;Is it CR?
;;	JMPR	..2		;Merge below
;;
;;..1:	INX$	H		;HL-> 2nd char
;;	MOV	A,M		;Get 2nd char
;;	CPI	LF		;Is it LF?
;;..2:	RZ			;Yes, return HL-> 2nd char
;;	DCX$	H		;No match, restore HL
;;	RET
;;	EPROC	CRLFPR

;
; LOOKCH - Lookup char. in A in Table <- DE.  Table ends in EOT.   [5/4/86]
;	   Return: 'Z' if found, else 'NZ'.
;
				;{WRDSPN,PARCHK,CHKREL}
	BPROC	LOOKCH
LOOKCH:	PUSH	B		;Save BC
	MOV	C,A		;Save char. in C
	MVI	B,EOT		;End of table char
	CALL	..1		;Perform lookup
	MOV	A,C		;Restore char to A
	POP	B		;Restore BC
	RET
;
..1:	CMP	B		;EOT?
	BEQ	RET%NZ		;Yes, return failure
	LDAX	D		;Get table char
	CMP	C		;Compare with char
	RZ			;Match - return 'Z'
	INX$	D		;Bump Ptr
	JMPR	..1		;Continue looking
	EPROC	LOOKCH
	.PAGE
;
; LOOKTB - Lookup DE in table (HL), when found return following 16 bit word.
;	   Table ends in 0000.
;	   Enter: DE = 16 bits to look for. HL -> table to use
;	   Retrn: HL = table entry following DE entry
;		  'C' if not found
;
				;{DOTCMD,HELP}
	BPROC	LOOKTB
LOOKTB:	MOV	B,H
	MOV	C,L		;Save in BC
	CALL	MVINHL		;HL = next table entry
	MOV	A,H
	ORA	L		;End of table reached?
	STC			;Assume yes, set 'C'
	RZ			;Yes, return 'C'
	CALL	CMHLDE		;Found the command?
	MOV	H,B
	MOV	L,C		;HL-> table entry again
	JRZ	..2
	INX$	H		;No, bump to next table entry
	INX$	H
	INX$	H
	INX$	H
	JMPR	LOOKTB		;Continue
;
..2:	INX$	H
	INX$	H		;HL-> routine address
	JMP	MVINHL		;HL = address
	EPROC	LOOKTB
;
; CHOICE - Lookup %A in table CS:(HL).  When found return following address.
;	   Table ends in KFF.
;	   Enter: A = 8 bits "choice", CS:HL-> table to use
;	   Retrn: HL = table entry when found, C clobbered
;		  'C' if not found.
;
	BPROC	CHOICE
CHOICE:	MOV	C,A		;C = char to look for
;6	CALLC	#1
;6	RET
..1:	MOV	A,M		;Get next table entry
	CPI	KFF		;Reached end of table?
	STC			;Assume Yes
	RZ			;Yes, return 'C'
	INX$	H		;HL-> address of table
	CMP	C		;Reached desired table entry?
	JZ	MVINHL		;Yes, return HL = address in table
	INX$	H		;No, skip over address
	INX$	H
	JMPR	..1		;Continue
	EPROC	CHOICE
	.PAGE
;
; CMHLDE - Compare HL with DE for Z, NZ, C OR NC.
;	   If  HL > DE == 'NC' and 'NZ'.
;	   If  HL < DE == 'C' and 'NZ'.
;
CMHLDE:
	IFNOT	P8086, [
	MOV	A,H
	CMP	D
	RNZ
	MOV	A,L
	CMP	E
	RET
	] [
;6	CMP	BX,DX
;6	RET
	]
;
; CMBCDE - Compare BC with DE for Z, NZ, C OR NC.
;
CMBCDE:
	IFNOT	P8086, [
	MOV	A,B
	CMP	D
	RNZ
	MOV	A,C
	CMP	E
	RET
	] [
;6	CMP	CX,DX
;6	RET
	]
;
; CMHLBC - Compare HL with BC for Z, NZ, C OR NC.
;
CMHLBC:
	IFNOT	P8086, [
	MOV	A,H
	CMP	B
	RNZ
	MOV	A,L
	CMP	C
	RET
	] [
;6	CMP	BX,CX
;6	RET
	]
;
; CMINDE - Compare (HL) with DE.
;
CMINDE:
	IFNOT	P8086, [
	PUSH	H
	CALL	MVINHL		;HL = table entry
	CALL	CMHLDE		;Compare to DE
	POP	H
	RET
	] [
;6	CMP	[BX],DX
;6	RET
	]
;
; CMINBC - Compare (HL) with BC.
;
CMINBC:
	IFNOT	P8086, [
	PUSH	H
	CALL	MVINHL		;HL = table entry
	CALL	CMHLBC		;Compare to BC
	POP	H
	RET
	] [
;6	CMP	[BX],CX
;6	RET
	]
;
; MXHLDE - Set HL to greater of HL or DE. 'NC' if no change
;
MXHLDE:
	IFNOT	P8086, [
	CALL	CMHLDE		;'NC' when HL > DE
	] [
;6	CMP	BX,DX
	]
	RNC			;No, HL is OK
	XCHG
	RET
;
; MNHLDE - Set HL to lesser of HL or DE.  'C' if no change.
;
MNHLDE:
	IFNOT	P8086, [
	CALL	CMHLDE		;'C' when HL < DE
	] [
;6	CMP	BX,DX
	]
	RC
	XCHG
	RET
;
; POSDIF - Set HL > DE, BC = HL - DE.
;
POSDIF:	CALL	MXHLDE		;Make sure HL > DE
;	JMP	SBHLDE		;BC = (HL - DE)
;
; SBHLDE - Compute BC = (HL - DE).
;
				;{POSDIF above}
SBHLDE:
	IFNOT	P8086, [
	PUSH	H		;Save HL
	DSUB	D
	MOV	B,H		;Move difference to BC
	MOV	C,L
	POP	H		;Restore HL
	RET
	] [
;6	MOV	CX,BX
;6	SUB	CX,DX
;6	RET
	]
;
; SBDEBC - Compute BC = (DE - BC).
;
SBDEBC:
	IFNOT	P8086, [
	MOV	A,E
	SUB	C
	MOV	C,A
	MOV	A,D
	SBB	B
	MOV	B,A
	RET
	] [
;6	SUB	CX,DX
;6	NEG	CX
;6	RET
	]
;
; SBHLBC - Compute BC = (HL - BC)
;
SBHLBC:
	IFNOT	P8086, [
	MOV	A,L
	SUB	C
	MOV	C,A
	MOV	A,H
	SBB	B
	MOV	B,A
	RET
	] [
;6	SUB	CX,BX
;6	NEG	CX
;6	RET
	]
;
; NEGBC - Negate the word in BC.
;
NEGBC:
	IFNOT	P8086, [
	XRA	A		;Get zero
	SUB	C		;Compute 0 - C
	MOV	C,A		;Save
	MVI	A,0		;Get zero, save 'C' bit
	SBB	B		;Compute 0 - BC
	MOV	B,A		;Result in BC
	RET
	] [
;6	NEG	CX
;6	RET
	]
;
; NEGHL - Negate the word in HL. (3/25/85)
;
NEGHL:
	IFNOT	P8086, [
	XRA	A		;See NEGBC above
	SUB	L
	MOV	L,A
	MVI	A,0
	SBB	H
	MOV	H,A
	RET
	] [
;6	NEG	BX
;6	RET
	]
	.PAGE
;
; IDXTBL - Index into address table (HL) with A as the index
;	   Retrn: HL-> table entry
;
				;{IDXFCB,TBLADD}
IDXTBL:	DCR	A		;Create item index
;	JMP	ADAAHL
;
; ADAAHL - Add 2*A to HL, return 'C' on overflow.
;
				;{IDXTBL above)
ADAAHL:	ADD	A
	JMPR	ADAHL0
;
; ADDAHL - Add A to HL, return 'C' on overflow.
;
ADDAHL:	ORA	A		;Clear 'C'
				;{ADDAHL^,ADAAHL}
ADAHL0:	ADC	L
	MOV	L,A
	RNC			;Done, 'NC'
	MVI	A,0
	ADC	H		;MSB updated
	MOV	H,A
	RET			;'C' or 'NC'
;
; AD3AHL - Add 3*A to HL, return 'C' if overflow.
;
	IF	VPLUS, [
AD3AHL:	PUSH	B		;Save BC
	MOV	C,A		;Save A
	CALL	ADAAHL		;HL = HL + 2A
	MOV	A,C		;Restore A
	POP	B		;Restore BC
	JRNC	ADDAHL		;Branch for 3rd add if no overflow yet
	CALL	ADDAHL
	STC			;Otherwise ensure overflow flag set
	RET
	]
;
; TBLADD - Use reg. A as the index into address table (HL)
;	   Enter: HL-> table, A = index;  A == 1 :: first item.
;	   Retrn: HL = address in table.
;
				;{DISPAT,FCBADD}
TBLADD:	CALL	IDXTBL		;HL-> address in table
;
; MVINHL - Move word at (HL) to HL.
;
MVINHL:
	IFNOT	P8086, [
	MOV	A,M
	INX$	H
	MOV	H,M
	MOV	L,A		;Return value in HL
	RET
	] [
;6	MOV	BX,[BX]
;6	RET
	]
	.PAGE
;
; ADDMBC - (HL) = (HL) + BC.  Return: 'Z' if (HL) zero.
;
	IFNOT	P8086, [
DCINHL:	LXI	B,1		;Decrement at (HL)
SUBMBC:	CALL	NEGBC
ADDMBC:	MOV	A,C
	ADD	M
	MOV	M,A
	INX$	H
	MOV	A,B
	ADC	M
	MOV	M,A
	DCX$	H
	ORA	M		;Is 16 bit result zero?
	RET
	] [
;6DCINHL:	MOV	CX,1
;6SUBMBC:	NEG	CX
;6ADDMBC:	ADD	[BX],CX
;6	TEST	WPT [BX],0FFFFH
;6	RET
	]
;
; ADINDE - DE = (HL) + DE.  HL Unchanged
;
ADINDE:
	IFNOT	P8086, [
	PUSH	H		;Save ->
	CALL	MVINHL		;HL = length
	DAD	D		;Compute sum
	XCHG			;Save in DE
	POP	H		;Restore HL
	RET
	] [
;6	ADD	DX,[BX]
;6	RET
	]
;
; DVDEHL - Divide DE by HL. Quotient returned in BC,
;	   remainder in HL.
;
	BPROC	DVDEHL
DVDEHL:	XCHG
DVHLDE:	PUSH	H		;Divide HL by DE
	MOV	L,H
	MVI	H,00H
	CALL	..SUB1
	MOV	B,C		;Save partial quot. in B
	MOV	A,L
	POP	H
	MOV	H,A
..SUB1:	MVI	C,-1
..2:	INR	C
	DSUB	D		;Subtract
	JRNC	..2
	DAD	D		;Add last back
	RET
	EPROC	DVDEHL
;
; MUL128 - Multiple value in BC by 128.
;
	BPROC	MUL128
MUL128:	PUSH	D		;Save DE
	MVI	D,7		;Make sector count into byte count
..1:	ORA	A
	MOV	A,C
	RAL
	MOV	C,A
	MOV	A,B
	RAL
	MOV	B,A
	DCR	D
	JRNZ	..1
	POP	D
	RET
	EPROC	MUL128
;
;	Mult. HL by 10 and add digit in 'A'.
;
MULT10:	PUSH	D		;Save DE
	MOV	D,H
	MOV	E,L
	DAD	H
	DAD	H
	DAD	D
	DAD	H
	SUI	'0'
	POP	D
	JMP	ADDAHL
;
; MULTIP - Multiply DE by HL and return result in HL.
;
	BPROC	MULTIP
MULTIP:	PUSH	B		;Save BC
	MOV	B,H
	MOV	C,L
	LXI	H,0		;Clear result
	MVI	A,16		;16 bit multiply
..1:	DAD	H		;Shift result left
	XCHG			;DE = result, HL = multiplier
	DAD	H		;Shift left
	XCHG			;HL = result again
	JRNC	..2		;Branch if multiplier bit = 0
	DAD	B		;Else add in
..2:	DCR	A		;Decrement count
	JRNZ	..1		;Loop
	POP	B		;Restore BC
	RET			;HL = result
	EPROC	MULTIP
;
; MVINDE - Move (HL)++ into DE.
;
MVINDE:
	IFNOT	P8086, [
	MOV	E,M
	INX$	H
	MOV	D,M
	INX$	H
	RET
	] [
;6	MOV	DX,[BX]
;6	INC	BX
;6	INC	BX
;6	RET
	]
;
; STDEIN - Store DE into (HL)++.
;
STDEIN:
	IFNOT	P8086, [
	MOV	M,E
	INX$	H
	MOV	M,D
	INX$	H
	RET
	] [
;6	MOV	[BX],DX
;6	INC	BX
;6	INC	BX
;6	RET
	]
;
; ZER%HL - Store 0000 into (HL)++
;
ZER%HL:
	IFNOT	P8086, [
	MVI	M,00		;Set (HL) to 0000
	INX$	H
	MVI	M,00
	INX$	H
	RET
	] [
;6	MOV	WPT [BX],0000
;6	INC	BX
;6	INC	BX
;6	RET
	]
;
; TSTWIN - Test value at (HL).  Return 'Z' if (HL) == 00.
;
TSTWIN:
	IFNOT	P8086, [
	MOV	A,M		;Get low byte
	INX$	H
	ORA	M		;OR in high byte
	DCX$	H
	ORA	A		;This simplifies 8086 translation
	RET
	] [
;6	TEST	WPT [BX], 0FFFFH
;6	RET
	]
	.PAGE
;
;	String routines, similar to those in 'C'
;
;
; STRLEN - Compute length of string (HL), looking for delimiter of 00.
;	   Enter: HL-> string
;	   Retrn: HL = length; DE saved
;
	BPROC	STRLEN
STRLEN:	PUSH	D		;Save DE
	MVI	B,00		;Look for 00 terminator
	LXI	D,0000		;Init counter
..1:	MOV	A,M		;Get next char
	CMP	B		;Delimiter?
	JRZ	..2		;Yes, branch
	INX$	D		;Increment counter
	INX$	H		;Bump PTR
	JMPR	..1		;Loop
;
..2:	XCHG			;HL = length
	POP	D		;Restore DE
	RET
	EPROC	STRLEN
;
; STRCPY - Copy string (HL) to (DE), including delimiter of 00.
;
STRCPY:	MOV	A,M
	INX$	H
	STAX	D
	INX$	D
	ORA	A		;Copied the [00]?
	JRNZ	STRCPY		;No, continue
	RET
;
; STRZCP - Copy string (HL) to (DE), but NOT delimiter of 00.
;
STRZCP:	MOV	A,M
	INX$	H
	ORA	A		;Reached [00]?
	RZ			;Yes, done
	STAX	D
	INX$	D
	JMPR	STRZCP		;Continue
	.PAGE
;
;	Basic MOVE routines.
;
; MOVE -  Move (HL) to (DE) for %C bytes
;
MOVE12:	MVI	C,12		;Usually to move a file name
MOVE:	MVI	B,00		;Upper byte is zero
;
	IFNOT	POLLING, [
MOVEBC:
	]

RTLDIR:	MOV	A,B		;Is the count zero to begin with?
	ORA	C
	RZ			;Yes return

	IF	P8080, [
	MOV	A,M
	STAX	D
	INX	D
	INX	H
	DCX	B
	JMP	RTLDIR
	] [
	LDIR			;Move with Z-80 power
	RET
	]
;
	IFNOT	POLLING, [
MOVEUP:
	]

RTLDDR:	MOV	A,B		;Is the count zero to begin with?
	ORA	C
	RZ			;Yes, return

	IF	P8080, [
	MOV	A,M
	STAX	D
	DCX	D
	DCX	H
	DCX	B
	JMP	RTLDDR
	] [
	LDDR			;Move the text
	RET
	]
;
; MVUPBF - Move RAM from DE upto HL UP to BC's new floor.
; MVTXUX - Move RAM from DE upto HL upwards by BC.
; MVTXUP - Move RAM from DE thru HL upwards by BC.
;
;	Return:
;		BC = |displacement|.
;		HL -> topmost raised byte (TXTRWF).
;
				;{RLCMD2}
MVUPBF:	CALL	SBDEBC		;BC = (DE - BC) = -displacement
	CALL	NEGBC		;BC = displacement

				;{MAKEBF,ADDREG,RZREGS,RZCMBF,FREE}
MVTXUX:	DCX$	H		;HL -> highest byte to be moved
				;{BUFFUP,ADDACT}
MVTXUP:	MOV	A,B		;Is BC = 00?
	ORA	C
	RZ			;Yes, return
	PUSH	B		;Save BC
	PUSH	H		;Save end address
	DAD	B		;Compute end after move
	XTHL			;HL-> where to move from
	CALL	SBHLDE		;BC = length of text to move - 1
	INX$	B		;Adjust for true length
	POP	D		;DE -> move destination
	PUSH	D		;Save as final endpointer
	CALL	MOVEUP		;Move the text up
	POP	H		;HL = new end-pointer
	POP	B		;Restore BC
	RET
;
; MVTX2B - Move RAM from DE upto HL down to BC, DE > BC.
; MVTXDX - Move RAM from DE upto HL downwards by BC.
; MVTXDW - Move RAM from DE thru HL downwards by BC.
;
;	Return:
;		BC = |displacement|.
;		DE -> past topmost address moved from.
;		HL -> topmost of lowered bytes (TXTRWF).
;
				;{MALLOC}
MVTX2B:	CALL	SBDEBC		;BC = (DE - BC) = displacement amount
				;{LWREGS,LWCMBF}
MVTXDX:	DCX$	H		;HL -> topmost byte to be moved
				;{OPENDW,DELREG} (VPLUS)
				;{DELACT,BUFFDW}
MVTXDW:	MOV	A,B		;Is BC = 00?
	ORA	C
	RZ			;Yes, return
	PUSH	B		;Save BC
	PUSH	D		;Save begin address
	XCHG			;DE = end address
	DSUB	B		;HL = destination address
	XTHL			;HL = begin address
	XCHG			;HL = end address
	CALL	SBHLDE		;BC = size of text to move
	INX$	B		;Adjust for true length
	POP	H		;HL-> move destination
	XCHG			;HL-> move source
	CALL	MOVEBC		;Move the text down
	XCHG			;HL = new text end pointer
	DCX$	H		;HL-> last char moved
	POP	B		;Restore original BC
	RET
;
; FILL - Fill 'BC' locations with 'A' from (HL). (11/15/84)
;	 Return: HL = HL + BC, DE saved.
;
	BPROC	FILLZ
FILLZ:	XRA	A		;Fill with zero
FILL:	PUSH	D		;Save DE
	MOV	E,A		;Save char in E
	JCXZ	..1		;Just return if BC == 0
;
	MOV	M,E		;Fill first location
	MOV	E,L		;Move HL to DE
	MOV	D,H
	INX$	D		;DE is Move destination
	DCX$	B		;Account for one filled pos
	CALL	MOVEBC		;Fill is overlapped move
	XCHG			;HL-> past last fill
..1:	POP	D
	RET
	EPROC	FILLZ
	.PAGE
;
;	8080 Routines
;
	IF	P8080, [
;
; DSBCB - Subtract BC from HL.
;
DSBCB:	MOV	A,L
	SUB	C
	MOV	L,A
	MOV	A,H
	SBB	B
	MOV	H,A
	RNZ
	ORA	L
	RET
;
; DSBCD - Subtract DE from HL.
;
DSBCD:	MOV	A,L
	SUB	E
	MOV	L,A
	MOV	A,H
	SBB	D
	MOV	H,A
	RNZ
	ORA	L
	RET
;
; CCIRRT - Returns in HL -> next char same as in reg. A, or
;	   when BC is decremented to zero.
;
CCIR0:	POP	PSW		;Restore char
CCIRRT:	CMP	M		;Is char found?
	INX	H		;Bump pointer
	DCX	B		;Decrement count
	RZ			;Yes, return with 'Z'
	PUSH	PSW		;Save compare char
	MOV	A,B		;Is count zero?
	ORA	C
	JNZ	CCIR0		;No, continue
	POP	PSW		;Fix stack
	ORI	1		;Set for 'NZ'
	RET			;Return 'NZ'
	]			;<IF P8080>

