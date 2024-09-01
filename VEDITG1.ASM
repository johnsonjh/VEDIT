	.PAGE
	.TITLE	'VEDIT-G1'
;****************************************
;*					*
;*	Common Routines			*
;*					*
;****************************************
;
; Copyright (C) 1987 by CompuView Products, Inc.
;			1955 Pauline Blvd.
;			Ann Arbor, MI 48103
;
;	Written by: Theodore J. Green
;
;	Last Change: Tom - Nov. 21 - 8086 CS:SLIDER bug
;		     Ted - Nov. 26 - Fix ADJMRK to clear BLMVEN and Markers
;		     Tom - Mar. 05, 1987 - CHKPNT() & CHK@HL() for 8086 to fix SCRFCB[] adjust bug
;
; PREVLF - Returns in HL pointer to previous LF.
;
				;{NEXFIL,FNDLIN}
PREVLF:	MVI	A,LF		;Search for LF
	IF	POLLING, [
	CALL	CHKKYB		;Check for keyboard char
	]

	IF	P8080, [
PRVLF1:	CMP	M		;LF found?
	DCX	H		;Bump pointer
	JNZ	PRVLF1		;No, continue looking
	] [
	PUSH	B		;Save BC
	LXI	B,-1		;Set for unlimited search
	CCDR			;Search backward for LF
	POP	B		;Restore BC
	]
	INX$	H		;HL-> LF

	IF	POLLING, [
	CALL	CHKKYB		;Check for keyboard char
	]
	RET
;
; NEXTLF - Returns in HL pointer to next LF.  If EOF
;	   found before LF, returns 'C' and HL -> EOF.
;
				;{SAVBGN,CRZIP}
	BPROC	NEXTLF
NEXTLF:	IF	POLLING, [
	CALL	CHKKYB		;Check for keyboard char
	]
	PUSH	D		;Save DE
	LXI	D,01A0AH	;Get D = EOF, E = LF
	DCX$	H		;Adjust for next INX H
..1:	INX$	H		;Bump pointer
	MOV	A,M		;Get next character
	CMP	D		;EOF found?
	STC			;Assume Yes, set 'C'
	JRZ	..2		;Yes, return with 'C'
	CMP	E		;LF found?
	JRNZ	..1		;No, continue looking
				;Yes, return with 'NC'
..2:	POP	D		;Restore DE
	IF	POLLING, [
	CALL	CHKKYB		;Check for keyboard char
	]
	RET
	EPROC	NEXTLF
;
; ITOA - Convert number in BC to decimal ASCII.
;
;	 Enter: BC = number.
;	 Retrn:	HL-> ASCIZ string.
;
;
	DSEG	$
VMCSAV:	DB	'-'		;Room for "-xxx_L" command
DECSAV:	DS	7		;Storage for ASCII string
	DS	3		;Room for "_L"
	CSEG	$

	BPROC	ITOA
ITOA:	CALL	SV%BD		;Save registers
	CLR$	DECBLZ		;Flag to blank leading zeros
	MVIW$	DECDIV,10000	;Set first divisor
	PUSH	B		;Save BC
	LXI	H,DECSAV	;HL-> where to save string
	XTHL			;HL = number, *str on stack
;
..1:	LDED	DECDIV		;+Get divisor
	MOV	A,D		;Is it zero?
	ORA	E
	JRZ	..4		;Yes, return
	CALL	DVHLDE		;Get BC = quotient, HL = remainder
	MOV	A,C		;Get quotient
	PUSH	H		;Save remainder as new number
	PUSHA			;Save digit value
	LHLD	DECDIV		;Get the divisor
	LXI	D,10		;Ready to divide by 10
	CALL	DVHLDE		;Get divisor for next digit
	SBCD$	DECDIV		;Save new divisor
	LXI	H,DECBLZ	;HL-> blank zero flag
	MOV	A,B		;Is new divisor zero?
	ORA	C
	BNE	..2		;No, branch
	MVI	M,-1		;Yes, don't blank last digit
..2:	POPA			;Get digit back
	CMP	M		;Are they both zero?
	MVI	C,' '		;Assume Yes, get space
	JRZ	..3		;Yes, print a space
	ADI	'0'		;Convert digit to ASCII
	MOV	C,A		;Needs to be be in C
	MOV	M,A		;No, no more blanking
..3:	POP	H		;HL = new number
	XTHL			;HL-> string
	MOV	M,C		;Save char
	INX$	H		;Bump PTR
	MVI	M,00		;Add [00]
	XTHL			;HL = number
	JMPR	..1		;Continue
;
..4:	POP	H		;Fix stack
	LXI	H,DECSAV	;Return HL-> string
	RET
	EPROC	ITOA
	.PAGE
;
;
; FNDTAB - Find next Tab position from HL and return in A.
;	   Returns 'Z' if HL beyond last Tab position.
;
	BPROC	FNDTAB
FNDTAB:	TST$	HELPFF		;;In middle on on-line help?
	JRZ	..0		;;No, branch
	MOV	A,L		;;Yes, enable tabs at every 8
	DCR	A
	ANI	0F8H
	ADI	9
	RET
;
..0:	INX$	H		;Change 255 to 256
	MOV	A,H		;Get high order
	DCX$	H		;Restore
	ORA	A		;Is position >= 255?
	JNZ	RET%Z		;Yes, return with 'Z'
	PUSH	H		;No, save HL
	MOV	A,L		;Get position
	LXI	H,TABTBL-1	;HL-> Tab pos. table
..1:	INX$	H		;Point to next tab pos
	CMP	M		;Beyond or at that Tab pos?
	JRNC	..1		;Yes get next
	MOV	A,M		;No, get tab pos
	POP	H		;Restore HL
	IF	POLLING, [
	CALL	CHKKYB		;Check for keyboard char
	]
	CPI	0FFH		;Tab table fence?
	RET			;Return with 'Z' if fence found
	EPROC	FNDTAB
;
; DELAY - Perform a delay of the # of milliseconds in reg. A.
;
				;{CRTCRL,VLOOP5}
	IFNOT	P8086, [
	BPROC	DELAY
DELAY:	CALL	SV%BDH		;Save regs
	MOV	L,A		;Put delay value in HL
	MVI	H,00		;8 bit value
	DAD	H		;Need double for 500 uS count
	MOV	B,H		;Put count in BC
	MOV	C,L
..1:	MOV	A,B		;Is count zero (Pre-test)
	ORA	C
	RZ			;Yes, return
	PUSH	B		;Save count
	LXI	H,DLYVAL	;HL-> delay constant
	MOV	B,M		;Load constant into B. (38 for 2mhz)
..2:	NOP			;Delay 26 T states
	NOP
	NOP
	DCR	B
	JNZ	..2		;Loop with CHKKYB is > 500uS
	IF	POLLING, [
	CALL	CHKKYB		;Check fast polling
	]
	POP	B		;Restore count
	DCX$	B		;Decrement
	JMPR	..1		;Continue
	EPROC	DELAY
	] [
;6DELAY	PROC
;6	XOR	AH,AH
;6	SHL	AX,1
..1:	RZ
	PUSH	B
	MOV	B,DLYVAL	;Delay constant
..2:	DCR	B		;3T
	JRNZ	..2		;19T
				;Loop with CHKKYB is > 500 uS
	IF	POLLING, [
	CALL	CHKKYB		;Check fast polling
	]

;6	DEC	AX		;Delay done?
	POP	B
	JMPR	..1		;No, continue
;6DELAY	ENDP
	]
	.PAGE
;
; CPTSET Move (Dword) Command Ptrs CMDGET,CMDPUT into bytes @DE.
;
				;{PROCMD}
CPTSET:	LXI	H,CMDGET
	MVI	C,4*PTRSIZ
	JMP	MOVE
;
	IF	POLLING, [
;
;	Move routines which also  poll and buffer keyboard.
;
; MOVEBC - Move %BC locations from (HL) to (DE).
;
	BPROC	MOVEBC
MOVEBC:	CALL	CHKKYB		;Check for keyboard char
..1:	PUSH	B		;Save the count
	CALL	STMVBC		;Set count for this move
	CALL	RTLDIR		;Perform the move
	POP	B		;Restore count before last move
	CALL	MVCHST		;Check console and update count
	RC			;Return if all moving done
	JMPR	..1		;Else continue the move
	EPROC	MOVEBC
;
; MOVEUP - Move overlapping text up BC bytes from (HL) to (DE).
;
	BPROC	MOVEUP
MOVEUP:	CALL	CHKKYB		;Check for keyboard char
..1:	PUSH	B		;Save the count
	CALL	STMVBC		;Set count for this move
	CALL	RTLDDR		;Perform the move
	POP	B		;Restore count before last move
	CALL	MVCHST		;Check console and update count
	RC			;Return if all moving done
	JMPR	..1		;Continue the move
	EPROC	MOVEUP
;
STMVBC:	MVI	A,4		;Get high order count per loop
	CMP	B		;Is count > than one loop?
	RNC			;No, use remaining count
	MOV	B,A		;Yes, use loop count
	RET
;
MVCHST:	CALL	CHKKEY		;Check for and save any keyboard char
	MOV	A,B		;Get high order count
	SUI	4		;Subtract the last move
	RC			;Return 'C'if all moving done
	MVI	C,00		;Lower order is now zero
	MOV	B,A		;Else update the count
	RET			;Return 'NC'
	]			;;<IF POLLING>
	.PAGE
;
; BUFFUP - Move text buffer from HL up by BC.		[2/20/86]
;	   Define CS:SLIDER for ADJ%HL.
;	   Update all pointers EXCEPT for TXTFLR.
;	   Return: 'C' if no space.  HL and BC are saved.
;
SLIDER:	DSW	1		;+In CS:  Defined by BUFFUP/DW, used by ADJ%HL
;
				;{RDPREV,ICMD,INSBLK,GETTXT,BCKATV,VRPEXE}
BUFFUP:	MOV	A,B
	ORA	C		;Is BC = 00
	RZ			;Yes, nothing to do
	PUSH	H		;Save begin PTR
	PUSH	B		;Save count
	CALL	NEEDBC		;Is there room for BC bytes?
	POP	B		;Restore count
	POP	H		;Restore PTR
	RC			;No room, return 'C'
;
	PUSH	H		;Save PTR
	XCHG			;DE-> begin of block to move
	LHLD	TXTRWF		;HL-> end of text to move up
	CALL	MVTXUP		;Move text from DE to HL up by BC
	SHLD	TXTRWF		;Save new END Ptr
	POP	H		;Restore HL
;
;	Get set to adjust pointers
;
	PUSH	H		;Save HL again
	PUSH	B		;Save offset
	SBCD	SLIDER		;Save offset as slide value
	XCHG			;PTRs below DE don't move
	MOV	B,D		;Move to BC too
	MOV	C,E		;No PTRs will be "reset", just moved up
	CALL	ADJPN1		;Adjust all pointers EXCEPT for TXTFLR
				;Can't insert at TXTFLR if TXTFLR then moved up
	POP	B		;Restore regs
	POP	H
	XRA	A		;Set 'NC'
	RET
;
; BUFFDW - Move text buffer from HL down by -BC.
;	   Define CS:SLIDER for ADJ%HL.
;	   Return: HL and BC are saved.
;
				;{RDPREV,WRTFIX,PACKTX,BCKATV,VCPTXT,VRPEXE}
	BPROC	BUFFDW
BUFFDW:	PUSH	B		;Save HL and BC
	PUSH	H
	JCXZ	..1		;If BC == 00 just check pointers against TXTFLR
				;"nW" < 128 bytes or disk full (MSDOS)
	XCHG			;DE-> begin of text block
	CALL	NEGBC		;Make BC positive
	LHLD	TXTRWF		;HL-> end of text to move
	CALL	MVTXDW		;Move text from DE to HL down by BC
	SHLD	TXTRWF		;Save new END Ptr
	POP	H		;HL-> first byte to move down
;
;	Adjust all pointers whose text moved or was deleted
;
	PUSH	H		;Save again
	CALL	NEGBC		;Make BC negative again
	SBCD	SLIDER		;Save negative offset
	DAD	B		;HL-> where first byte moved to
	XCHG			;Save in DE
	POP	B		;BC-> first byte to move down
	PUSH	B		;Save again
				;DE-> begin of text which was deleted (overwritten)
				;BC-> past last char which was deleted
	CALL	ADJPNT		;Adjust the pointers
..1:	CALL	CHKPNT
	POP	H		;Restore HL and BC
	POP	B
	RET
	EPROC	BUFFDW
	.PAGE
;
;	P O I N T E R   H A N D L I N G   R O U T I N E S
;
;
; ADJPNT - Any pointer DE <= PTR < BC needs to be reset.  [11/04/85]
;	   Some pointers are reset to greater of DE or new TXTFLR.
;	   Any pointer >= BC needs to slide up/down by CS:SLIDER.
;	   Return: BC, DE saved, HL clobbered
;
				;{BUFFDW}
ADJPNT:	LXI	H,TXTFLR	;Does TXTFLR get forced to DE?
	CALL	ADJ%HL
	LES	H,SCRFCB	;Adjust screen pointer in high memory
	CALLES	ADJ%HL		;Needed for auto-buffering

				;{BUFFUP}
ADJPN1:	LXI	H,EDTPTR	;Adjust edit pointer
	CALL	ADJ%HL
	LXI	H,SRFAIL	;Adjust search pointer
	CALL	ADJ%HL
	CALL	ADJ%HL		;Adjust FINDPT for [FIND]
	CALL	ADJ%HL		;Adjust SAVEPT for -n_T and [REPLACE]
	LXI	H,TXTCEL	;Adjust window pointer
	CALL	ADJ%HL
;
;	If OLDACT is in moving text need to set CNTBFL.
;
	CALL	STNSNL		;Ensure that visual mode window rewritten
	LHLD	OLDACT		;Get PTR from which LINCNT is set
	CALL	CMHLDE		;Is it in non-moving text?
	BLT	ADJMRK		;Yes, branch
	MVIB$	CNTBFL,1	;No, set flag to count all LFs
;
; ADJMRK - Adjust "Text Markers" and BLMVEN.  [11/26/86]
;
				;{ADJPNT^ now only}
	BPROC	ADJMRK
ADJMRK:	LXI	H,PNTTBL	;Point to table
	MVI	A,PNTMAX+1	;Number of entries plus BLMVEN
;
..1:	PUSHA
	CALL	ADJ%HL		;Does text marker get forced to DE?
	JRNC	..2		;;No, branch
	DCX$	H
	DCX$	H		;;Yes, backup to PTR
	CALL	ZER%HL		;;Reset PTR to zero (HL++)
..2:	POPA			;Done with whole table?
	DCR	A
	JRNZ	..1		;Nope, fixup next pointer
	RET			;BC and DE saved, HL clobbered
	EPROC	ADJMRK
;
; ADJ%HL - If (HL) < DE, leave (HL) alone and return 'NC',  else
;	   If DE <= (HL) < BC, (HL) is set to DE and return 'C',  else
;	   If (HL) >= BC, (HL) is adjusted by CS:SLIDER, return 'NC'.
;	   Retrn: HL-> next pointer, i.e. HL++
;
	BPROC	ADJ%HL
ADJ%HL:	CALL	CMINDE		;Is (HL) < DE?
	JRC	..NC		;Yes, leave (HL) alone, PTR not in changed text
;
	CALL	CMINBC		;Is (HL) < BC?
	JC	STDEIN		;Yes, store DE in (HL++)

	PUSH	B		;Save BC
	LBCD	SLIDER		;+BC = negative offset value
	CALL	ADDMBC		;Let (HL) = (HL) + CS:SLIDER
	POP	B
..NC:	INX$	H
	INX$	H		;Bump
	XRA	A		;Set 'NC'
	RET
	EPROC	ADJ%HL
;
; CHKPNT - Check pointers to ensure they are not less than TXTFLR.
;	   If they are set them to TXTFLR.
;	   Example: A "100W" can set EDTPTR to BGNPTR, which is most likely
;		less than TXTFLR.
;	   Return:  All regs clobbered
;
;				;{BUFFDW}
CHKPNT:	LES	H,SCRFCB	;Check screen pointer
;6	MOV	DX,TXTFLR	;;
	CALLES	CHK%HL
	LXI	H,EDTPTR	;Check edit pointer
	CALL	CHK%HL
	LXI	H,SRFAIL	;Check search pointer
	CALL	CHK%HL
	LXI	H,FINDPT	;Check pointer for [FIND]
	CALL	CHK%HL
	LXI	H,SAVEPT	;Check saved edit PTR for -n_T
	CALL	CHK%HL
	LXI	H,TXTTOP	;Check window pointer
;	JMP	CHK%HL
;
; CHK%HL - If (HL) < TXTFLR, set (HL) = TXTFLR (8080).
; CHK%HL - If (BX) < DX, set (BX) = DX	(8086).
;
				;{CHKPNT above}
CHK%HL:	PUSHF			;Save flags
	IFNOT	P8086, [
	PUSH	D		;Save DE
	LDED	TXTFLR		;DE-> begin of text window
	]
	CALL	CMINDE		;Is pointer below window?
	CC	STDEIN		;Yes, set pointer to DE
	IFNOT	P8086, [
	POP	D		;Restore
	]
	POPF
	RET
;
; CHGTBL - Changes 'E' entries in table <- (HL) by BC.
;
				;{UPSCTB,FRMELN}
CHGTBL: MOV	A,E		;Get remaining count
	ORA	A		;Is it zero?
	RZ			;Yes, done
	MOV	A,M		;Get low order table value
	ADD	C		;Add low order change
	MOV	M,A		;Update low order
	INX$	H		;HL-> high order of entry
	MOV	A,M		;Get high order table value
	ADC	B		;Add high order change
	MOV	M,A		;Update high order
	INX$	H		;Bump to next entry
	DCR	E		;Decrement count
	JMPR	CHGTBL		;Continue

	.PAGE
;************************************************
;*						*
;*	Operation Stack Routines		*
;*						*
;************************************************
;
	IF	VPLUS, [
PSHITR:	LXI	H,ITRSTK	;{JMCMD,CHKLVL,JNCMD}
	JMPR	PUSHSK
	]
				;{PAGEDW}
PSHOPC:	MOV	C,A		;Get count from A
	MVI	B,00
	JMPR	PUSHO1
;
PUSHOP:	MOV	A,C		;Put char back in A
	LXI	B,1		;Default count is one
PUSHO1:	LXI	H,OPSTCK	;HL-> operation stack
;
; PUSHSK - Pushes the regs DE, BC and A onto stack <- by HL.
; PUSHSK - Pushes the regs ES:DX, ES:CX and AL onto stack <- by BX
;	   Return 'C' if overflow.
;
				;{JMCMD} (VPLUS)
	BPROC	PUSHSK
PUSHSK:	PUSHA
	PUSH	B
	MOV	C,M		;Get current stack length
	MOV	A,C		;Length in A too
	INX$	H		;HL-> max stack length
	CMP	M		;Overflow?
	BLT	..1		;No, branch
;
	POP	B		;Restore stack
	POPA
	STC			;Set 'C'
	RET
;
..1:	ADI	STAKSZ		;Computer length for next round
	DCX$	H		;HL-> length
	MOV	M,A		;Update length
	INX$	H
	INX$	H		;HL-> first entry
	MVI	B,00		;BC = stack offset
	DAD	B		;Add offset
	POP	B		;Restore regs
	POPA
;
;6ST9@HL:
ST5%HL:	MOV	M,E		;Save DE
	INX$	H
	MOV	M,D
	INX$	H
;6	MOV	[BX],ES
;6	INC	BX
;6	INC	BX
	MOV	M,C		;Save BC
	INX$	H
	MOV	M,B
	INX$	H
;6	MOV	[BX],ES
;6	INC	BX
;6	INC	BX
	MOV	M,A		;Save A
	INX$	H
	CMP	A		;Return with 'NC'
	RET
	EPROC	PUSHSK
	.PAGE
;
; POPITR - Pops the regs ES:DX, ES:CX(=BX) and AL from FILO stack ITRSTK.
; POPITR - Pops the regs DE, BC (=HL) and A from FILO stack ITRSTK.
; POPREG -						    REGSTK.
; POPOP  -						    OPSTK.
;
POPOP:	LXI	H,OPSTCK	;HL-> operation stack
	IFNOT	P8086, [
	JMPR	POPSTK
	]
;6	PUSH	ES
;6	CALL	POPSTK
;6	POP	ES
;6	RET

	IF	VPLUS, [
				;{ENDITL,JMCMD,JNCMD,ENDITR}
POPITR:	LXI	H,ITRSTK	;HL -> LOOP/IF stack
	JMPR	POPSTK
	]
				;{POPCMD}
POPREG:	LXI	H,REGSTK	;HL -> Register Execution stack
				;{POPITR,POPREG,POPOP^}
POPSTK:	MOV	A,M		;Get current length
	ORA	A		;Is stack empty?
	STC			;Set 'C'
	RZ			;Yes, return 'C'
	SUI	STAKSZ		;No, new stack length
	MOV	M,A
	INX$	H
	INX$	H		;HL-> first entry
	CALL	ADDAHL		;HL-> current entry
;
; LD9@HL - Moves 9 bytes at [BX] to ES:DX, ES:CX and AL.  BX=CX.
; LD5%HL - Moves 5 bytes at (HL) to DE, BC and A. HL = BC.
;
LD5%HL:	CALL	MVINDE		;Load first word into DE.  (HL)++
;6	MOV	ES,[BX]
;6	ADD	BX,2
	MOV	C,M		;Set BC
	INX$	H
	MOV	B,M
	INX$	H
;6	ADD	BX,2
	MOV	A,M		;Set A
	MOV	H,B		;Set HL = BC
	MOV	L,C
	RET
;
; DECSTK - Get argument from FILO stack, decrement count.
;	   Return: 'C' if stack empty. 'Z' and 'NC' if count = 00.
;
DECOP:	LXI	H,OPSTCK	;HL-> operation stack
;6	PUSH	ES
;6	CALL	DECSTK
;6	POP	ES
;6	RET
				;{JECMD} (VPLUS)
DECSTK:	MOV	A,M		;Get stack offset
	ORA	A		;Is stack empty?
	STC			;Set 'C'
	RZ			;Yes, return 'C'
	SUI	STAKSZ		;Offset to last entry
	INX$	H
	INX$	H		;HL-> first entry
	CALL	ADDAHL		;HL-> current entry
	CALL	MVINDE		;Put address in DE.  (HL)++
;6	MOV	ES,[BX]
;6	ADD	BX,2
	MOV	A,M		;Is count now zero?
	INX$	H
	ORA	M
	RZ			;Yes, return 'Z' and 'NC' if zero
	DCX$	H		;Restore HL
	CALL	DCINHL		;Decrement word at (HL)
;
	INX$	H
	INX$	H		;HL-> last argument
;6	ADD	BX,2
	ORA	H		;Set 'NZ'
	MOV	A,M		;Get argument
	RET			;DE = address. A = argument. BC = -1
	IFNOT	P8086, [
	.PAGE
;
; MALLOC - Allocate memory, move command buffer & text registers as necessary.
;	   Memory is allocated between command buffer and text registers.
;
;	   Entry: MIDBAS -> begin of high-memory stuff
;		  CMDMAX -> last byte of command buffer = CMDRWF (VPLUS)
;		  TXTRWF -> last byte used by main edit buffer
;		  BC = # desired bytes
;	MALLHL()  HL-> desired address (end PTR) of space
;			== 00 if space after command buffer is OK
;	   Retrn: HL-> allocated memory
;		  DE-> past allocated memory
;		  BC = bytes allocated
;		  'C' could not allocate as much as desired
;
	BPROC	MALLOC
				;{RAMDWN,TBLINI}
MALLOC:	LXI	H,0000		;Allocate space above command buffer
				;{RGOPEN,GETSTR}
MALLHL:
MALLSG:				;{OPENDW, 8086 entry point}
	PUSH	B		;Save BC
	PUSH	H		;Save HL
	LHLD	MIDBAS		;HL-> bottom of high-memory stuff
	DSUB	B		;HL-> tentative new bottom
	JRNC	..0		;Branch unless underflow
	LXI	H,0		;Correct underflow
..0:	LDED	TXTRWF		;DE-> last byte of edit buffer
	INX$	D		;First available byte
	CALL	MXHLDE		;HL = max(HL and DE)
	XTHL			;(SP)-> move destination
				;HL-> desired space
	MOV	A,H		;Was malloc address specified?
	ORA	L
	JRNZ	..1		;Yes, branch
	LHLD	CMDMAX		;No, use end of command buffer
	INX$	H		;HL-> first byte past command buffer
..1:	LDED	MIDBAS		;DE-> move source
	POP	B		;BC-> move destination
	CALL	MVTX2B		;Move text from (DE) upto (HL) down to BC
				;BC = distance moved, HL-> last byte moved down
				;DE-> high memory which did NOT move
	INX$	H		;HL-> allocated space
	PUSH	H
	PUSH	D		;Save DE-> past allocated space
	PUSH	B		;Save BC = amount allocated
	CALL	NEGBC
	CALL	FIXALL		;Fix all necessary pointers based on DE
	POP	B		;BC = bytes allocated
	POP	D		;DE-> past allocated space
	POP	H		;HL-> allocated space
;
	XTHL			;HL = originally desired size
	DSUB	B		;Did we get desired number?
	POP	H		;HL-> allocated space
	RZ			;Yes, return 'NC'
	STC
	RET			;No, return 'C'
	EPROC	MALLOC
;
; FREE - Free memory space by moving any lower high-memory stuff up
;	 Enter: HL-> memory to free
;		BC = # bytes to free
;
FREEDE:	XCHG
FREE:	PUSH	H		;Save HL
	LDED	MIDBAS
	CALL	MVTXUX		;Move text from (DE) upto (HL) up by BC
				;BC = # bytes freed
	POP	D		;DE-> free memory
	INX$	D		;Want PTR =< freed memory adjusted upward
;	JMP	FIXALL		;Fix all necessary pointers
	]			;<IFNOT P8086>
	.PAGE
;
;
; FIXALL - Fix all pointer that need fixing after text register/command
;	   buffers are moved.
;
				;{MALLOC,FREE^}
FIXALL:	CALL	FXRSTK		;Fix register execution stack
	CALL	FIXREG		;Fix register pointers
	CALL	FXCMPT		;Fix command pointers

	IF	VPLUS, [
	LXI	H,MIDBAS	;Fix base pointer for upper buffers/registers
	CALL	FXRNGE
	]

	RET
;
; FXCMPT - Moves the command buffer pointers by amount in BC.
;
	IFNOT	VPLUS, [
FXCMPT:	LXI	H,CMDBAS	;Update CMDBAS (also called MIDBAS)
	CALL	FIXHL
	CALL	FIXHL		;Update CMDGET
	CALL	FIXHL		;Update CMDPUT
	CALL	FIXHL		;Update CMDMAX
	]			;<IFNOT VPLUS>
;
; FXCMBF - Fix command buffer pointers in range (MIDBAS):DE-1 by BC.
;
	IF	VPLUS, [
FXCMPT:
	BPROC	FXCMBF
FXCMBF:	LXI	H,CMDRWF	;HL-> 1st of two pointers
	CALL	FIXPUT		;CMDRWF
	CALL	FXRNGE		;CMDBAS

	LXI	H,CMDGET	;HL-> 1st of 3 more pointers
	CALL	FXRNGE		;CMDGET
	CALL	FIXPUT		;CMDPUT
	CALL	FXRNGE		;RIPTR
	]			;<IF VPLUS>
;
;	Change the command buffer pointers in the iteration stack.
;
	LXI	H,ITRSTK	;Point to stack
	MOV	A,M		;Get length
	INX$	H		;Point to first stack entry
	INX$	H
..1:	SUI	STAKSZ		;# bytes/entry
	RC			;Return if done (or initially zero)
	PUSHA			;Save counter
	CALL	FIXHL		;Fix stacked "CMDGET"
	REPT	STAKSZ-2*PTRSIZ,(
	INX$	H
	)			;Advance to next entry
	POPA			;Restore counter
	JMPR	..1		;Fix next entry
	EPROC	FXCMBF
;
; Fixup text register address table
;
; BC = Distance stuff moved (ammount to change pointers by)
; DE = Highest address moved (anything at/below this gets changed)
;
	IFNOT	VPLUS, [

	BPROC	FIXREG
FIXREG:	LXI	H,KEYPTR
	CALL	FIXHL
	LXI	H,REGTBL	;Point to base of table
	MVI	A,(REGTEN-REGTBL)/4  ;A = total number of text registers
..1:	PUSHA			;Save count
	CALL	FIXHL		;Fix entry (HL)++.
	INX$	H		;Ignore length
	INX$	H
	POPA			;Restore count
	DCR	A		;Fixed all entries?
	JRNZ	..1		;Nope, do next one
	RET
	EPROC	FIXREG
	]			;<IFNOT VPLUS>
;
; FXRSTK - Fixup text register execution stack.
;
; BC = Distance stuff moved (amount to change pointers by).
; DE = Highest address moved (anything below this gets changed for VEDIT).
;
; MIDBAS -> lowest address to be changed for VPLUS.
;
	BPROC	FXRSTK
FXRSTK:	LXI	H,REGSTK	;Point to stack
	MOV	A,M		;Get length
	INX$	H		;Point to first stack entry
	INX$	H
..1:	SUI	STAKSZ		;# bytes/entry
	RC			;Return if done (or initially zero)
	PUSHA			;Save counter

	IF	VPLUS, [
	CALL	FIXPUT		;Fix "CMDPUT"
	CALL	FXRNGE		;Fix "CMDGET"
	]

	IFNOT	VPLUS, [
	CALL	FIXHL		;PUT
	CALL	FIXHL		;GET
	]

	INX$	H		;Skip over REGEXN
	POPA			;Restore counter
	JMPR	..1		;Fix next entry
	EPROC	FXRSTK
;
;
; FIXHL - Fix pointers into high-memory stuff.
;	  (HL) < DE, (HL) = (HL) + BC; if (HL)==00, don't change
;	  Enter: HL-> pointer to check and fix
;		 DE-> top of memory which changed
;		 BC = amount to add to pointers (may be negative number)
;	  Retrn: HL-> next pointer.
;
	IFNOT	VPLUS, [
				;{FXCMPT,FIXREG,FXRSTK}
	BPROC	FIXHL
FIXHL:	PUSH	H		;Save ->PTR
	CALL	MVINHL		;HL = PTR being fixed
	MOV	A,H
	ORA	L		;Is PTR == 00
	JRZ	..1		;Yes, don't change
	CALL	CMHLDE		;Is pointer into memory that changed?
	POP	H
	CC	ADDMBC		;Yes, adjust the pointer
	PUSH	H		;Setup stack
..1:	POP	H		;HL-> PTR
	INX$	H		;Point to next entry
	INX$	H
	RET
	EPROC	FIXHL
	]			;<IFNOT VPLUS>

	IF	VPLUS, [
;
; FIXREG - Fix register base pointers in BUFTBL.
;	   Adjust pointers in range (MIDBAS):DE-1 by BC.
;
	BPROC	FIXREG
FIXREG:	LXI	H,KEYPTR	;Fix keystroke macro PTr
	CALL	FXRNGE
	LXI	H,BUFTBL	;Edit buffers & text registers are located here
..1:	MOV	A,M		;Get register type
	INX$	H		;No, advance HL -> base pointer
	ORA	A		;Done?
	RM			;Yes,0FFh fence reached
	JRZ	..2		;Branch if nonexistent register
	CALL	FXRNGE		;Adjust it if necessary, HL++
	JMPR	..1
..2:	REPT	2*PTRSIZ,(
	INX$	H
	)			;Update pointer to next entry
	JMPR	..1
	EPROC	FIXREG
;
; FIXPUT - Fix PUT & Roof pointers.
;
FIXPUT:	INX$	D
	CALL	FXRNGE
	DCX$	D
	RET
;
; FXRNGE - Fix (Dword) pointers in range (MIDBAS):DE-1 by BC, HL++.
;	   Don't change if (HL)==00.
;
FIXHL:				;{FXCMPT,FIXBUF,FIXREG,FXISTK,FXRSTK}
	BPROC	FXRNGE
FXRNGE:
;6	MOV	BP,DS
;6	CMP	2[BX],BP
;6	JZ	#0
;6	ADD	BX,2*PTRSIZ
;6	RET
..0:	PUSH	H		;Save ->PTR
	CALL	MVINHL		;HL = PTR being fixed
	MOV	A,H
	ORA	L		;Is PTR == 00
	JRZ	..1		;Yes, don't change
	CALL	CMHLDE		;Is pointer into memory that changed?
	JRNC	..1		;No, branch
	POP	H		;Maybe, retrieve ptr
	PUSH	D		;Save ceiling
	LDED	MIDBAS		;DE = floor
	CALL	CMINDE		;Is floor LE pointer?
	CNC	ADDMBC		;If so, add BC to the pointer
	POP	D		;Retrieve ceiling
	PUSH	H		;Setup stack
..1:	POP	H		;Retrieve pointer
	REPT	2*PTRSIZ,(
	INX$	H
	)			;Advance HL to next pointer
	RET
	EPROC	FXRNGE
	]			;<IF VPLUS>
