	.PAGE
	.TITLE	'VEDIT-F2'
;************************************************
;*						*
;*	Common File Handling Routines		*
;*						*
;************************************************
;
; Copyright (C) 1987 by CompuView Products, Inc.
;			1955 Pauline Blvd.
;			Ann Arbor, MI 48103
;
;	Written by: Theodore J. Green
;
;	Last Change: TnT - Oct. 25, 1986 - 2.03 changes
;		     Ted - Mar. 03, 1987 - Changes for DEMO version
;
; NEXFIL - Reads in file to nearly fill buffer or until EOF.
;	   If buffer already full, first writes out 1 "page" of text.
;	   TXTCEL is set to TXTRWF when EOF is found.
;	   Return: 'NZ' if TXTCEL already includes complete input file.
;
				;{SETFIL,CLOSER,NCMD,EOFCHK}
	PUBLIC	NEXFIL
	PUBLIC	NEXFRD,NEXFR2,NEXFR3,NEXFI5,NEXFR5,AUTORD
	BPROC	NEXFIL
NEXFIL:	CALL	INFLCH		;Is the entire file in edit buffer?
	JZ	RET%NZ		;Yes, return 'NZ'
	CALL	RESTOP		;No, restore real character at TXTCEL
	CALL	INRDCH		;Entire input file in memory?
	JRNZ	..DISK		;No, branch to read more
	CALL	STNSNL		;Yes, need screen rewrite even if no Disk I/O
	LHLD	TXTRWF		;HL = TXTRWF -> EOF of file
	JMP	SETTOP		;Set TXTCEL = TXTRWF, return 'Z'
;
;	Write out some of file if no room for MINSEC sectors.
;
..DISK:	CALL	MINCHK		;Is MINSEC space free?
				;BC = MINSEC
	JRNC	AUTORD		;Yes, branch to append
;
;	Write MINSEC sectors unless within 1K of top of text
;
	CALL	MUL128		;BC = # bytes
	LHLD	TXTBAS		;HL-> begin of text
	DAD	B		;HL-> end of text
	LXI	B,1024		;1K (moved for overflow-bug fix)
	JRC	..3		;Just branch to 1K-write if overflow
	XCHG			;DE-> tentative end of text to write
	LHLD	TXTCEL		;HL-> top of text
	DSUB	B		;HL-> top of allowable write
	CALL	CMHLDE		;Attempting to write last 1K?
	XCHG			;HL-> tentative end of text to write
	BGE	..4		;No, branch

..3:	LHLD	TXTBAS		;HL-> begin of text
	DAD	B		;Just write out 1K and 1 line
;
;	Keep text window beginning on line boundary
;
..4:	CALL	NEXTLF		;HL-> next LF (or to EOF at TXTRWF)
	INX$	H		;HL-> begin of full line
	LDED	TXTCEL		;+DE-> top of text window
	CALL	MNHLDE		;HL cannot be greater than TXTCEL
				;If < 1K of text, it will all be written
	CALL	WRTTXT		;Write out text from TXTBAS to (HL)

	IFNOT	DEMO, [
	JRNC	AUTORD		;Branch if no error
	]

	JMP	BREAK		;;Give BREAK to abort, reset STACK, SETTOP()
	EPROC	NEXFIL
;
;	Read sectors until ISPARE bytes free or EOF. (2/16/85)
;
				;{OPNIOX,0Acmd}
	BPROC	NEXFRD
NEXFRD:
AUTORD:	CALL	INFLCH		;Is entire file in edit buffer?
	JRZ	NEXFR3		;Yes, don't need (want) screen rewrite
	CALL	STNSNL		;No, need screen rewrite even if no Disk I/O
	CALL	INRDCH		;Entire file in memory?
	JRZ	NEXFR3		;Yes, set TXTCEL = TXTRWF
	CALL	CMPEND		;BC = # sectors to read, leaving ISPARE free
	MOV	A,B		;Enough memory?
	ORA	C
	JRNZ	NEXFR2		;Yes, read the sectors in
	CALL	MNFREE		;BREAK out if < 1K of space free
	LXI	B,8		;Set to read 1K of file
NEXFR2:	LHLD	TXTRWF		;HL = DMA for sector read
	CALL	READTX		;Read the sectors, update TXTRWF
				;{Called from above}
NEXFR3:	CALL	INRDCH		;Has end of input file been reached?
	LHLD	TXTRWF		;HL-> last text char
				;{WRTBND}
NEXFI5:	JRZ	NEXFR5		;Yes, appended the rest of file
	LHLD	TXTRWF		;Get TXTRWF (for WRTBND)
	CALL	EOLLIN		;;HL-> past last line read = TXTRWF if no LF's
;;	CALL	PREVLF		;No, search for last LF
;;	INX$	H		;HL-> past LF

;
;	Make sure EDTPTR is less than TXTCEL.
;
NEXFR5:	XCHG			;DE = new TXTCEL
	LXI	H,EDTPTR	;HL-> pointer to check
	LXI	B,0FFFFH	;Make sure (HL) < BC
	CALL	ADJ%HL		;Change EDTPTR if necessary
	XCHG			;HL = new TXTCEL
	JMP	SETTOP		;Set PTR, return 'Z' bit
	EPROC	NEXFRD
;;
;; EOLLIN - Move HL back past end of last entire line read in or -> TXTRWF
;;	   if no LF's encountered.
;;
				;;{NEXFRD,ACMD}
	public	eollin
	BPROC	EOLLIN
EOLLIN:	CALL	PREVLF		;;HL-> last LF read in or fence at TXTFLR
	INX$	H
	PUSH	D
	LDED	EDTPTR		;;Backed upto/ past edit point?
	CALL	CMHLDE
	POP	D
	JRZ	..1		;;Backed upto edit point, branch
	RNC			;;No, HL-> past end of last entire line read
..1:	LHLD	TXTRWF		;;LF's not found, set HL-> past last char read
	RET
	EPROC	EOLLIN
	.PAGE
;
; PRVFIL - Reads in backwards from OUTPUT file.  If insufficient space,
;	   first writes out to .REV file.
;	   Return: 'NZ' if begin of input file in memory.
;
				;{CHKGLB,CHKRVB}
	PUBLIC	PRVFIL
	BPROC	PRVFIL
PRVFIL:	TST$	BOFFLG		;At begin of Output file?
	JZ	RET%NZ		;Yes, return 'NZ'
;
	CALL	MINCHK		;Is MINSEC space free?
	JRNC	..2		;Yes, branch
;
	CALL	MUL128		;Convert sector count to byte count
	LHLD	TXTRWF		;HL-> top of text
	DSUB	B		;Subtract MINSEC bytes
;
;	Keep text window end on line boundary
;
	CALL	PREVLF		;First previous LF
	INX$	H		;HL-> begin of full line to write out
	CALL	MINBGN		;Make sure HL is within text window
	CALL	WRTBND		;Write end of text to .REV file
	TST$	WTERFL		;Is the disk full or other write error?
	JNZ	BREAK		;Yes, give BREAK
;
..2:	LHLD	ITRCNT		;Save ITRCNT used by "_L"
	PUSH	H
;
;	First check if ISPARE bytes are free
;
	CALL	CMPEND		;BC = # sectors to read, leaving ISPARE free
	MOV	A,B		;Enough memory?
	ORA	C
	LXI	H,0000		;Assume yes, simulate "-0A" command
	JRNZ	..3		;Yes, read the sectors in
	CALL	MNFREE		;No, make sure at least 1K of free space
				;Does not return is < 1K free
	LXI	H,10000		;Use "-nA" to read something in
..3:	SHLD	ITRCNT		;Set appropiate iteration count
	CALL	RDPREV		;Read from OUTPUT file
	POP	H
	SHLD	ITRCNT		;Restore ITRCNT
	XRA	A
	RET			;Return 'Z'
	EPROC	PRVFIL
	.PAGE
;
; WRTTXT - Write text from TXTBAS to (HL) to disk.	[10/14/86]
;	   Return: See WRTFIX.
;
	PUBLIC	WRTTXT
	PUBLIC	WRTFIX
				;{CLOSER,NEXFIL,WCMD,ENCMD,MAKSPC}
	BPROC	WRTTXT
WRTTXT:	LDED	TXTBAS		;+DE-> where to write from
	CALL	CMHLDE		;Anything to write?
	RZ			;No, don't bother with rest (screws up BOFFLG)
	PUSH	H		;Save desired new TXTFLR
	CALL	CMPWRT		;BC = # sectors to write out

	IF	DEMO, [
	IFNOT	P8086, [
	CLR	WTERFL
	ORA	B		;;Writing more than 1K?
	JRNZ	..DMOM		;;Yes, branch
	MOV	A,C
	CPI	9
	BLT	..OK		;;No, branch
..DMOM:	CALL	DEMOMS		;;Yes, give demo message
	LXI	B,9		;;
	DCX$	B		;;Only write 1K
	MOV	A,C
	STA	WTERFL		;;Set flag
	] [
;6	MOV	WTERFL,00	;;Reset flag
;6	CMP	CX,9		;;Writing more than 1K?
;6	BLT	#OK		;;No, branch
;6	CALL	DEMOMS		;;Yes, give demo message
;6	MOV	CX,9
;6	DEC	CX		;;Only write 1K
;6	MOV	WTERFL,CL	;;Set flag
	]
	]

..OK:	XCHG			;HL-> text to write
	CALL	MUL128		;BC = # bytes to write
	CALL	DELBAK		;Delete any .BAK file to make more disk space
				;Give BREAK if no output file
	MVIB$	BOFFLG,1	;Clear Begin of Output file flag
	CALL	VISMSS		;Put up WAIT message, set NWSCFL = 089H
				;Save all regs
	LXI	D,OUTFCB	;DE-> FCB, HL-> text to write
	CALL	WRITE		;Write out the bytes, 'C' if write error
				;BC = bytes that were written
	POP	H		;HL = desired new TXTFLR
	SHLD	TXTFLR		;Save tentative new TXTFLR
	JRNC	WRTFIX		;Branch if no write error
;
;	Write error - set TXTFLR past first LF in new text buffer
;
	LHLD	TXTBAS		;HL-> text buffer
	DAD	B		;HL-> beginning of new text buffer
	CALL	BOFLIN		;Set TXTFLR to first line beginning
;
; WRTFIX - Move text down by count in BC and fix all necessary pointers.
;	   Updates CNTOUT to # of LFs written out.
;	   Return: 'C' if write error.
;		   BC = # bytes written.
;
;   This routine should only be accessed via WRTTXT or TXTFLR may be clobbered.
;
				;{CLOSER,WRTTXT above}
WRTFIX:	LHLD	TXTBAS		;HL-> text buffer
	DAD	B		;HL-> beginning of text to move
;
;	Before text is compressed, must count # LFs which were written out.
;
	PUSH	H		;Save -> new beginning = end of text written
	PUSH	B		;Save BC
	CALL	CNTFIL		;HL = new # of LFs written out (+1)
	DCX$	H		;Adjust
	SHLD	CNTOUT		;Save new count
;
;	Move the text down to compress it.
;
	POP	B		;Restore char count in BC
	POP	H		;HL-> begin of block to move down
	CALL	NEGBC		;BUFFDW needs -BC
	CALL	BUFFDW		;Move the text down, update pointers
;
	CALL	NEGBC		;Get positive BC again
	JMP	WTERCK		;Merge into F1
	EPROC	WRTTXT		;Return 'C' if WTERFL set (write error)
;
;
; CMPEND - Compute in BC number of sectors from TXTRWF to MIDBAS - 1
;	   leaving spare space (ISPARE) for insertion. 
;
; CMPSEC - Same - except leave BC spare bytes free. (VPLUS)
;
;	   Return: DE = TXTRWF, HL changed, BC = # sectors.
;		   'C'  if not ISPARE/BC sectors available.
;
	PUBLIC	CMPEND
	PUBLIC	CMPSEC
	PUBLIC	CMPWRT,CMPWR1,DIV128
				;{NEXFIL,RDPREV,MINCHK}
	BPROC	CMPEND
CMPEND:	LBCD$	ISPARE		;BC = amount spare space
				;{RDPREV}
CMPSEC:	LDED$	TXTRWF		;Compute to end of text
	LHLD	MIDBAS		;HL -> past main free space
	DCX$	H		;HL-> end of memory
	DSUB	B		;Compute where to fill to
	JRC	CMPWR1		;Return BC=0 if underflow

				;{NEXFIL,RDPREV,WRTTXT,WRTBND}
CMPWRT:	DSUB	D		;HL = max. size of text
CMPWR1:	LXI	B,00		;If no space, return BC=00
	RC			;Return if no space
DIV128:	PUSH	D		;Save pointer
	LXI	D,128		;DE = size of sector
	CALL	DVHLDE		;Compute BC = HL/DE
	POP	D		;Restore original DE
	RET
	EPROC	CMPEND
;
	IFNOT	VPLUS, [
;
; NEEDBC - BC = # free bytes needed
; NEEDTX - Check if -DE bytes are free between TXTRWF and MIDBAS
;	   Retrn: 'NC' if space available; 'C' if space NOT available
;
				;{BUFFUP}
NEEDBC:	LXI	H,0
	DSUB	B		;HL = -bytes needed
	XCHG			;DE = -bytes needed
;
				;{ACMD}
NEEDTX:	LHLD	MIDBAS		;HL-> begin of command buffer
	DAD	D		;HL-> lowest needed byte
	CMC			;If underflow return 'C'
	RC			;Return 'C' if no room
;
	LDED	TXTRWF
	INX$	D		;DE-> first free byte
	JMP	CMHLDE		;Return 'C' if no space
	]
;
; MINCHK - Return: 'NC' if free space > MINSEC * 128, else 'C'.
;		   BC = MINSEC
;
				;{NEXFIL,PRVFIL}
	PUBLIC	MINCHK
	BPROC	MINCHK
MINCHK:	CALL	CMPEND		;BC = # sectors there is space for
	MOV	H,B		;Put # in HL
	MOV	L,C
	LBCD	MINSEC		;BC = minimum # of sectors to append
	DSUB	B		;Is there room for # sectors?
	RET			;Return 'NC' if space, 'C' if no space
	EPROC	MINCHK
;
; MNFREE - Check that at least 1K of free space available
;	   Give error and BREAK if less than 1K free
;
	PUBLIC	MNFREE
	BPROC	MNFREE
MNFREE:	CALL	FREESP		;BC = # bytes free
	LXI	H,1024		;HL = 1K
	DSUB	B		;Are enough bytes free?
	RC			;Yes, return
	LXI	H,NOMEMO	;No, HL-> error
	JMP	MSGBRK		;Give error and break
	EPROC	MNFREE
;
; FREESP - Return in BC # bytes of free space. 'C' if no space free.
;
	IFNOT	VPLUS, [
				;{UCMD,ENCMD (VPLUS's clearly belongs to R1)}
FREESP:	LDED$	TXTRWF		;DE-> end of edit buffer
	INX$	D		;DE-> first free byte
	LHLD	CMDBAS		;HL-> first byte of "high memory" stuff
	CALL	SBHLDE		;BC = # unused bytes
	RNC			;Return # bytes in BC
	LXI	B,0000		;Else set to zero
	RET
	]
;
; MINBGN - Make sure HL has minimum value of TXTFLR.
;
				;{PRVFIL,NEWACT,BGPARA}
	PUBLIC	MINBGN
	BPROC	MINBGN
MINBGN:	LDED	TXTFLR		;DE-> bottom of text
	JMP	MXHLDE		;Set HL to greater of HL or DE
	EPROC	MINBGN
	.PAGE
;
; JAMUP - Move text from (HL) up by # sectors in BC
;
				;{RDPREV}
	PUBLIC	JAMUP
	BPROC	JAMUP
JAMUP:	CALL	SETSEC		;Save BC+1 as sector count
	CALL	MUL128		;BC = # bytes we have space for
	LXI	H,TXTFLR	;BUFFUP does not update TXTFLR
	CALL	ADDMBC		;Slide TXTFLR up
;
	LHLD	TXTBAS		;HL-> begin of text to move up
	MVIB$	JAMFL,0FFH	;For BREAK()
	CALL	BUFFUP		;Move the text up; fix text markers
				;Save HL and BC
	DAD	B		;HL-> first text byte (relocated TXTBAS)
	SHLD	DELPTR		;Save -> bottom of text
	RET
	EPROC	JAMUP
;
; JAMDWN - Move text from DELPTR to TXTRWF down to TXTBAS.
;
				;{RDPREV}
	PUBLIC	JAMDWN
	BPROC	JAMDWN
JAMDWN:	SUB%BC	DELPTR,TXTBAS	;BC = # bytes to move down
	CALL	NEGBC		;BC = neg. number of bytes to move down
				;HL = DELPTR -> text to move down
	CALL	BUFFDW		;Move the text down
	CLR$	JAMFL		;Set by JAMUP(), checked by BREAK()
	JMP	RSTDMA		;And reset dma address
	EPROC	JAMDWN
	.PAGE
;
; CHKSPC - Return if insertion space remains in text buffer.	[2/20/86]
;	   Else call MAKSPC to try and make space.
;
				;{NEWLIN,[vundo]}
	PUBLIC	CHKSPC
	BPROC	CHKSPC
CHKSPC:	LXI	D,-ACTLEN	;Allow for full line and spare
	CALL	NEEDTX		;Is there insertion space?
	BLT	..3		;No, branch

..1:	XRA	A		;Yes, get a zero
..2:	CMPM	FULLFG		;Is old flag same as new?
	MOV	M,A		;Save new value
	JNZ	STWTBR		;No, write new border, return
	RET			;Yes, just return
;
..3:	CALL	MAKSPC		;Try to make space
	JRNC	..1		;Branch if space was made
	MVI	A,1		;Get full flag set value
	JMPR	..2		;Update flag and display
	EPROC	CHKSPC
;
; MAKSPC - Try to make text buffer space by writing out first    [1/10/86]
;	   1K of text if active point is not in it.
;
;	   Returns: 'NC' if text written, 'C' if file cannot be written.
;
				;{CHKSPC}
	PUBLIC	MAKSPC
	BPROC	MAKSPC
MAKSPC:	TST$	OUTFLG		;Is an output file open?
	STC			;
	RZ			;No, return 'C'
	TST$	ATBFSW		;Is auto buffering enabled?
	STC			;
	RZ			;
	TST$	DSKFUL		;Disk known to be full?
	STC			;
	RNZ			;Yes, return 'C'
;
	LHLD	TXTBAS		;HL-> Begin of any text
	LXI	B,1024
	DAD	B		;HL-> top of first 1K
	CALL	NEXTLF		;Keep text window on line boundary
	INX$	H		;HL-> begin of full line
				;OK if HL-> past EOF
	LDED	TXACTV		;+DE-> begin of active line
	CALL	CMHLDE		;Is active point in first 1K?
	CMC			;Change 'NC' to 'C' ('Z' -> 'C')
	RC			;Yes, return, too bad
;
	JMP	WRTTXT		;Write the text from (HL) out
	EPROC	MAKSPC		;Return 'C' if write error occurred
	.PAGE
;
;; BOFLIN - Set TXTFLR past first LF in text.  If no LF found set to TXTBAS.
;
				;{WRTTXT}
	PUBLIC	BOFLIN 
	BPROC	BOFLIN
BOFLIN:	MOV	D,H		;Save starting point
	MOV	E,L
	CALL	NEXTLF		;HL-> before first complete line
	INX	H		;+(save F) HL-> first complete line
	JRNC	..2		;Branch if normal LF found before EOF
	XCHG			;EOF found, HL-> begin of only partial line
..2:	SHLD	TXTFLR		;Save new TXTFLR
	RET
	EPROC	BOFLIN
;
; CNTFIL - Count # LFs (+1) from Begin of file to (HL).
;	   Return: HL = # of LF, BC = # LF in memory, DE clobbered.
;
				;{YGET,GLOBHLx3}
	PUBLIC	CNTFL0,CNTFIL
	BPROC	CNTFL0
CNTFL0:	LHLD	EDTPTR
				;{RESTRT,WRTFIX}
CNTFIL:	CALL	CNTBGN		;BC = # LFs in memory
	LHLD	CNTOUT		;HL = # LFs written to disk
	DAD	B		;HL = total # LFs
	RET			
	EPROC	CNTFL0
				;{NEWACT}
	PUBLIC	CNTBGN,CNTLFB
	BPROC	CNTBGN
CNTBGN:	LXI	B,0000		;Init LF count
	LDED	TXTBAS		;+DE-> begin of text
;
				;{NEWACT,VDELET}
CNTLFB:	CALL	PREVLF		;Find previous LF
	DCX$	H		;Setup for next search
	INX$	B		;Count LF
	CALL	CMHLDE		;Found LF before TXTBUF?
	BGE	CNTLFB		;No, keep looking
	RET
	EPROC	CNTBGN
;
; SETSEC - Save sector count in BC in SECCNT.  Adjust by +1 because
;	   count is pre-decremented at DECSEC.
;
				;{READSC,JAMUP}
	PUBLIC	SETSEC
	BPROC	SETSEC
SETSEC:	INX$	B
	SBCD	SECCNT		;Save sector count
	DCX$	B
	RET
	EPROC	SETSEC
;
; DECSEC - Decrement sector count and test for zero.
;	   Return: 'Z' if remaining count zero.
;
				;{READSC,PRVSEC}
	PUBLIC	DECSEC
	BPROC	DECSEC
DECSEC:	LBCD$	SECCNT		;Get sector count
	DCX$	B		;Decrement
	SBCD$	SECCNT		;Save new count
	MOV	A,B		;Is count zero?
	ORA	C
	RET			;Return 'Z' if BC = 00
	EPROC	DECSEC
	.PAGE
;
; WTCLCH - Give error if an output file is already open.
;
	PUBLIC	WTCLCH
	BPROC	WTCLCH
WTCLCH:	TST$	OUTFLG		;Is an output file open?
	LXI	H,OTOMSG	;HL-> error message
	JRNZ	WTOPC1		;Yes, give error
	RET			;No, just return
	EPROC	WTCLCH
;
; WTOPCH - Give error if no output file is open.
;
	PUBLIC	WTOPCH,WTOPC1
	BPROC	WTOPCH
WTOPCH:	TST$	OUTFLG		;Is the output file open?
	RNZ			;Yes, just return
	LXI	H,NOOMSG	;No, HL-> error message
WTOPC1:	JMP	MSGBRK		;Give error
	EPROC	WTOPCH
;
; RVOPCH - Give error if .REV file is open
;
	PUBLIC	RVOPCH
	BPROC	RVOPCH
RVOPCH:	TST$	REVFLG		;Is .REV file open?
	RZ			;No, just return
	LXI	H,REVMSG	;Yes, HL-> error message
	JMPR	WTOPC1		;Give error
	EPROC	RVOPCH
;
; INFLCH - Return 'Z' if end of input file is in edit buffer.
;
	PUBLIC	INFLCH, INRDCH
	BPROC	INFLCH
INFLCH:	CMPW	TXTCEL,TXTRWF	;Appended last part from memory?
	RNZ			;No, return 'NZ
				;Yes, check file open flags
;
; INRDCH - Return 'Z' if neither input file nor .REV file is open.
;
INRDCH:	TST$	INFLG		;Is input file open?
	RNZ			;Yes, return 'NZ'
	TST$	REVFLG		;Is .REV file open?
	RET
	EPROC	INFLCH
;
;
;
	IF	DEMO, [
DEMOMS:	LXI	H,DEMMSG-123	;;Offset to DEMO message
	LXI	D,123
	DAD	D		;;Make it hard to disassemble
	JMP	PRTMSG
	]

	