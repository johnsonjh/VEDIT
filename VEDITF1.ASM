	.PAGE
	.TITLE	'VEDIT-F1'
;************************************************
;*						*
;*	OPSYS Dependent File Handling Routines	*
;*						*
;************************************************
;
; Copyright (C) 1987 by CompuView Products, Inc.
;			1955 Pauline
;			Ann Arbor, MI 48103
;
;	Written by: Theodore J. Green
;
;	Last Change: Tom - Dec. 15, 1986 - 2.03a changes
;		     Ted - Jan. 22, 1987 - BDSUSR returns MPM error code in HL
;		     Tom - Feb. 05 - CHKATR() for R/O checking
;		     Ted - Mar. 31 - DIR() sets FCBPAD to enable padding
;				     RESTRT() - allow -EA
;
;***************************************************************************
;
;
; SETIOX - Setup edit I/O FCB's.
;
;	  EXIT: INFCB/INUSR, RENFCB/RENUSR set.
;		NM2FLG set nonzero if both I/O files specified.
;
;	  NOTE: The 'X' both differentiates this routine from the console
;		I/O setup routine and is parallel with OPNIOX where the
;		'X' stands for 'eXecute auto-read'.
;
				;{PRCPRM-,EBCMD}
	BPROC	SETIOX
SETIOX:	CALL	SETINP		;Setup input FCB
	CALL	CMPCE		;Explicit output file (CR or ESC => no) ?
	BEQ	..2		;No, skip
;
;	Check for VPLUS fname<sp><cr>.
;
	CPI	SPACE		;Space?
	BNE	..1		;No, handle 2d parameter
	CALL	FCBCHX		;Skip any more spaces, check for terminator
	PUSHF			;Save result of file-terminator check
	CALL	BAKGET		;Backup GET pointer
	POPF			;File terminator?
	BEQ	..2		;Yes, branch;  only 1 file parameter
;
;	Set output filename = 2d parameter.
;
..1:	CALL	SETREN		;Yes, setup RENFCB from command line
	MVI	A,1		;Specify two file parameters
	JMPR	..3
;
;	Set output filename = input filename.
;
..2:	LXI	H,INUSR		;Setup RENFCB/USER from INFCB/USER
	LXI	D,RENUSR	;DE-> destination = RENFCB/USER
	MVI	C,17
	CALL	MOVE		;Copy 17 bytes
	XRA	A		;Specify only 1 file parameter
;
;	Set NM2FLG.
;
..3:	STA	NM2FLG		;Nonzero -> 2 parameters
	RET
	EPROC	SETIOX
;
; SETINP - Setup INFCB with source filename.
;	   Breaks out if name too long.
;
;	   Returns A = terminator.
;
				;{EBCMD,ERCMD}
SETINP:	CALL	RELSRC		;Close any open input file
	LXI	H,INFCB		;HL-> FCB/USER # to change
	JMPR	JSTFCB		;Setup FCB from command line
;
; SETREN - Setup RENFCB with object filename.
;	   Breaks out file name too long.
;
				;{EBCMD, EWCMD}
SETREN:	LXI	H,RENFCB	;HL-> FCB/USER # to change
	JMPR	JSTFCB		;Setup FCB from command line
;
; SETAUX - Setup AUXFCB with auxiliary filename.
;	   Breaks out if name too long.
;
				;{RLCMD,RSCMD,EDCMD,EGCMD}
SETAUX:	LXI	H,AUXFCB	;HL-> auxiliary FCB/USER #
JSTFCB:	JMP	SETFCB		;Setup SETFCB/USER # from command line
;
; SETSPX - Setup AUXFCB & AUXUSR with appropriate special VEDIT file.
;
;	   HL-> name of special file:  [INILEN]'VEDIT.INI'[CR]  (OPNEXE)
;				       [HLPLEN]'VxHELP.HLP'[CR] (HCMD)
;
				;{OPNSPX}
SETSPX:	CALL	SETCMD		;Point GET/PUT at filename
	JMP	SETAUX		;Setup FCB/USER #
;
; FRMREN - Setup FCB at (DE) with output filename.
;
				;{CREOUT,FRMBAK}
FRMREN:	LXI	H,RENFCB	;HL-> full output file name
	MVI	C,9		;Length of drive/filename
	JMPR	JMOVE		;Move to (DE)
;
; FRMBAK - Setup FCB at (DE) with filename.BAK
;
				;{CLOSE,DELBAK}
FRMBAK:	CALL	FRMREN		;Setup drive & FILENAME
	LXI	H,BAKSTR	;HL-> "BAK"
	MVI	C,3
JMOVE:	JMP	MOVE		;Add .BAK extension to FCB
	.PAGE
;
; OPNIOX - Opens INFCB for reading.  Sets OUTFCB/OUTUSR from RENFCB/RENUSR
;	   & opens for writing.  Auto reads the input file.
;
				;{EBCMD,RESTRT,BEGIN-}
	BPROC	OPNIOX
OPNIOX:	CPIB$	INFCB+1,' '	;Is a file specified?
	RZ			;No, return
;
	CALL	FILINI		;Init file flags and variables
	CALL	OPNSRC		;Able to open source file?
	JRNC	..3		;Yes, branch
;
	TST$	NM2FLG		;Were 2 files specified?
	LXI	H,CREMSG	;No, HL-> "NEW FILE"

	IF	VPLUS, [
	JNZ	FNFBRK		;Yes, break out with FNF message
	]

	IFNOT	VPLUS, [
	JRZ	..1		;No, branch to NEW FILE message
	LXI	H,FNFMSG	;HL-> "FILE NOT FOUND"
..1:
	]

	MVI	A,0C0H		;CRLF() fore/aft of msg
	CALL	MSGHND		;Print message
;
..3:	CALL	CREOUT		;Create .$$$ output file
	CALL	RESTOP		;Restore real character at TXTCEL
				;In case something already in buffer
	JMP	AUTORD		;Auto read the input file
	EPROC	OPNIOX
;
; OPNAUX - Open the file from auxiliary FCB/USER #.
;	   Return: 'C' if error.
;
				;{OPNSPX,RLCMD,RSCMD,EGCMD}
OPNAUX:	CALL	CLRAUX		;Zero out extent & NR, DE-> AUXFCB/USER #
	CALL	OPNFIL		;Open the file.  Success? (NZ)
				;Does not return if MP/M error
				;Sets AUXFLG if file opened OK
	IFNOT	VPLUS, [
	LXI	H,FNFMSG	;HL-> "FILE NOT FOUND" (VPLUS uses FNFBRK)
	]

	RC			;Return 'C' if file not found

				;{CREAUX}
OPNAU1:	CALL	RSTDMA		;Reset DMA address to 080H
	ORA	A		;'NC'
	RET
;
; OPNSPX - Open special file in AUXFCB, checking several places.
;	   HL-> file name to open:  length,name.ext,cr.
;	   Return: 'C' if file not found on either allowed drive.
;
;	   If INIDV0 <> 0, check current drive/user #.
;	   if INIDV0 =  2, check current drive/user 0.
;	   If INIDVA <> 0, check drive INIDVA/user 0.
;
;	   VPLUS - Does not check one directory more than once.
;
	IFNOT	VPLUS, [
				;{AUTEXE,HELPEX}
	BPROC	OPNSPX
OPNSPX:	CALL	SETSPX		;Setup AUXFCB/USER # from HL's string
	TST$	INIDV0		;Look on current drive/user #?
	JRZ	..1		;No, go check customized drive
;
;	Check on current drive, current user #.
;
	CALL	OPNAUX		;File opened?
	RNC			;Return 'NC' if so
;
;	? Look on default drive, user 0 ?
;
	ANIB$	INIDV0,2	;Configured to check user 0?
	JRZ	..1		;No, go try customized drive
	CALL	..SUB2		;Yes, set user #0 and try to open
	RNC			;Return 'NC' if file opened
;
;	Finally check user specified drive, if any, user 0.
;
..1:	TST	INIDVA		;Look on alternate drive?
	STC			;Return 'C' if drive not to be checked
	RZ			;No, return
	STA	AUXFCB		;Setup AUXFCB with drive # in A
..SUB2:	CLR$	AUXUSR		;and user # = 0
	JMP	OPNAUX		;Try to open the file, return 'C' if not found
	]
;
	IF	VPLUS, [
				;{OPNEXE,HELPEX}
	BPROC	OPNSPX
OPNSPX:	CALL	SETSPX		;Setup AUXFCB/USER # from HL's string
	CLR	ROOTDF		;Clear flag that default drive/user 0 checked
	STA	ROOTSP		;Clear flag that special drive/user 0 checked
	TST$	INIDV0		;Look on default drive?
	JRZ	OPNSP5		;No, go check customized drive
;
				;{PARMI,+RL}
OPNSP1:	CLR	ROOTDF		;Clear flag that default drive/user 0 checked
	STA	ROOTSP		;Clear flag that special drive/user 0 checked
;
;	Check on current drive, current user #.
;
	CALL	OPNAUX		;File opened?
	RNC			;Return 'NC' if so
;
;	? Look on default drive, user 0 ?
;
	CALL	CKROOT		;Check whether either root directory checked
	CPIB$	INIDV0,2	;Configured to check user 0?
	BNE	OPNSP5		;No, go try customized drive
	TST$	ROOTDF		;Has it already been checked?
	BNE	OPNSP5		;Yes, go try customized drive
;
;	Do check current drive, user 0.
;
	LDA	DEFDRV		;Get default drive #
	CALL	TRYSPX		;File opened?
	RNC			;Return 'NC' if so
;
;	Finally check user specified drive, if any, user 0.
;
OPNSP5:	TST$	ROOTSP		;Has it already been checked?
	STC			;In case so
	RNZ			;Yes, return 'C'
	TST	INIDVA		;Look on alternate drive?
	STC			;In case not
	RZ			;No, return 'C'
;	JMPR	TRYSPX		;A = special drive #, return 'NC' for success
;
; TRYSPX - Try to open file in AUXFCB on drive # in A, user 0.
;	    Return 'NC' if successful.
;
				;{OPNSPX}
TRYSPX:	STA	AUXFCB		;Setup AUXFCB with drive # in A
	CLR$	AUXUSR		;and user # = 0
	CALL	CKROOT		;Set appropriate flags ROOTDF and/or ROOTSP
	JMP	OPNAUX		;Try to open the file, return flags
	EPROC	OPNSPX
;
; CKROOT - Set ROOTDF and/or ROOTSP non-zero if AUXFCB specifies
;	    appropriate default-drive root or configured-special-drive root.
;
;	    Done for parallelism with 8086-F5 and to somewhat assist OPNSPX.
;
				;{OPNSPX,TRYSPX}
	BPROC	CKROOT
CKROOT:	TST$	AUXUSR		;User 0?
	RNZ			;No, return
	CMPMB	AUXFCB,DEFDRV	;Yes, default drive?
	BNE	..1		;No, branch
	STA	ROOTDF		;Yes, DEFAULT ROOT
..1:	CMPM	INIDVA		;Special drive?
	RNZ			;No, return
	STA	ROOTSP		;Yes, SPECIAL ROOT
	RET
	EPROC	CKROOT
	]
	.PAGE
;
; OPNSRC - Open the Source file.  Return 'C' if file not found.
;
				;{OPNIOX,ERCMD}
OPNSRC:	CALL	CLRSRC		;Set Extent & NR to 0, DE -> INFCB/USER #
;	JMP	OPNFIL		;Open file from FCB/USER #, 'NZ' if open OK
				;Does not return if MP/M error
				;Sets INFLG if file opened OK
;
; OPNFIL - Open file with DE-> FCB/USER #.
;	   Return: 'C' and A = 00 if file not found.
;		   Does not return if MP/M error.
;
				;{OPNSRC,OPNAUX,CREOUT}
OPNFIL:
	IF	VPLUS, [
	SDED	CURFCB		;So FNFBRK can list full filename
	]
;
	CALL	FCBVAL		;Check if filename valid
				;Does not return if error
	MVI	C,OPENF
	CALL	OPNCRE		;Open file, set FCB open flag
				;'C' and A == 00 if error
				;For MPM, HL is set to error code
	IFNOT	MPM, [
	RET
	]
;
; BDSCHK - For MP/M give error if physical or extended error
;
	IF	MPM,[
				;{OPNFIL^,CREFIL}
	BPROC	BDSCHK
BDSCHK:	RNC			;Return if no error
	CALL	MPMCHK		;Is this MP/M?
	JRZ	..ERR		;No, set 'C' & return
;
	MOV	A,H		;Get error code
	ORA	A		;Is it physical or extended error?
	JRNZ	..NERR		;No, file just wasn't found
..ERR:	STC			;
	RET

..NERR:	LXI	H,NOPMSG	;HL-> "FILE NOT OPENED"
	JMPR	CREBRK		;Give error and BREAK
	EPROC	BDSCHK
	]
	.PAGE
;
; CLRSRC - Clear extent & NR in INFCB.  Return DE-> INFCB/INUSR.
;
CLRSRC:	LXI	H,INFCB		;{OPNSRC}
	JMPR	CLRFCB
CLRAUX:	LXI	H,AUXFCB	;{OPNAUX}
	JMPR	CLRFCB
CLRREN:	LXI	H,RENFCB	;{CREOUT}
	JMPR	CLRFCB
CLROUT:	LXI	H,OUTFCB	;{CREOUT}
	JMPR	CLRFCB
CLRREV:	LXI	H,REVFCB	;{WRTBND}
;	JMPR	CLRFCB
;
; CLRFCB - Clear extent & NR in FCB <- HL.  Return DE -> FCB/USER #.
;
				;{CLRSRC,CLRAUX,CLRREN,CLRREV,CLROUT^}
CLRFCB:	XCHG			;DE-> FCB/USER #
	XRA	A		;A = 0
	LXI	H,12		;HL = extent counter offset
	DAD	D		;DE-> extent counter
	MOV	M,A		;Clear extent counter
	LXI	H,32		;HL = NR offset
	DAD	D		;HL-> NR
	MOV	M,A		;Clear NR
	RET			;DE-> FCB/USER #
	.PAGE
;
; CREOUT - Create the output file, name is in RENFCB/RENUSR.
;
				;{OPNIOX,EWCMD}
	BPROC	CREOUT
CREOUT:	LXI	D,RENFCB
	CALL	CHKATR		;BREAK out if R/O or MP/M err
;
;	Setup .$$$ FCB/USER #.
;
	CALL	STBRFL		;Set flag to rewrite status line "FILE" message
	MOVB	OUTUSR,RENUSR	;Associate user number with FCB
	LXI	D,OUTFCB	;DE-> output FCB/USER #
	CALL	FRMREN		;Setup output FCB
;
;	Setup .$R$ file
;
	LXI	D,REVFCB	;DE -> reverse FCB

	IF	VPLUS, [
	CALL	FILTAG		;Add edit buffer # tag to FCB
	]

	CALL	FRMREN		;Setup reverse FCB, DE-> filename extension
	MOVW	REVUSR,DEFUSR	;Copy DEFUSR and DEFDRV to REV FCB
;
;	Create .$$$ file
;
	CALL	CLROUT		;Clear extent & NR, DE-> OUTFCB/OUTUSR

	IF	VPLUS, [
	CALL	FILTAG		;Add edit buffer number to FCB extension
	]

	CALL	CREFIL		;Create file from OUTFCB/OUTUSR
				;'C' and A == 00 if error
;
	STA	DELFLG		;Flag that .BAK be deleted
	RNC			;Return if no error

				;{CREAUX}
CREERR:	LXI	H,MAKMSG	;HL-> error message
CREBRK:	JMP	MSGBRK		;Yes, give error
	EPROC	CREOUT
;
; CHKATR - Check if visible, R/W, user file exists.
;
;	   BREAK out if R/O or MP/M error.
;	   Return 'C' if no such file.
;
;	   Input:  DX-> FCB.
;
	BPROC	CHKATR
CHKATR:	PUSH	D
	XCHG			;BX-> FCB
	CALL	CLRFCB		;Clear extent & NR, DE-> FCB
	CALL	OPNFIL		;Open file, 'C' if file not found
				;Does not return if MP/M error
				;Set RENFLG if file opened OK
	POP	D
	JRC	..RET		;Branch if file not found
;
	LXI	H,9
	DAD	D		;HL-> attribute byte
	MOV	A,M
	ANI	80H		;Is R/O attribute set?
	LXI	H,MPMFMS	;HL-> error message
	JRNZ	CREBRK		;Yes, BREAK out
;
..RET:	RET
	EPROC	CHKATR
;
; CREAUX - Create file in AUXFCB/AUXUSR for writing.
;
				;{RSCMD}
CREAUX:	CALL	CLRAUX		;DE-> FCB/USER #
	CALL	CREFIL		;Create the file
	JC	CREERR		;Give error
	JMP	OPNAU1		;Merge with OPNAUX
;
; CREFIL - Create a file. DE-> FCB/USER #.  Delete any old file first.
;	   Set Open Flag for FCB if file created OK.
;	   Return: 'C' if error.
;
				;{CREOUT,CREAUX,WRTBND}
CREFIL:	CALL	FCBVAL		;;BREAK out if invalid filename
	CALL	DELBDS		;Delete any old file (save DE)

	IF	MPM, [
	CALL	BDSCHK		;Give error and BREAK if MPM error
	]

	MVI	C,CREATE	;Create output file

				;{OPNFIL,CREFIL^}
OPNCRE:	CALL	BDSUSR		;Set user #, call BDOS, save registers
				;'C' and A == 00 if error
				;If MPM error, HL = error code
;6	PUSHF			;;Save flags
	DCX$	D		;DE-> user #
	DCX$	D		;DE-> open flag
	STAX	D		;Set open flag, or == 0 if open error
	INX$	D
	INX$	D		;Restore DE-> FCB
;6	POPF			;;Restore flags
	RET			;'C' if error
	.PAGE
;
; SETFCB - Set FCB drive, name, extension & user-# from command.
;	   Expand '*'.  HL-> FCB/USER # on input.
;
;	   Return: A = terminator (CR, ESC, SP, TAB, '[', '=').
;		  'Z' if terminator is (CR, ESC, SP, TAB).
;
;	   BREAK out with error message if filename too long.  [8086]
;
				;{SETINP,SETAUX,SETREN}
	BPROC	SETFCB
SETFCB:	LDA	DEFDRV		;Use default drive initially
	MOV	M,A		;Set initial drive field
	PUSH	H		;Save -> FCB/USER #
	INX$	H		;HL-> name field
	PUSH	H		;Save -> name
	MVI	A,' '		;Get a space
	LXI	B,11		;Name is 11 long
	CALL	FILL		;Init the FCB
	LXI	B,4		;Length of FCB to zero
	CALL	FILLZ		;Zero out FCB

	CALL	FCBCHX		;Get next non-blank character. (HL saved)

	IF	VPLUS, [
	LES	H,CMDGET	;Can't do earlier, GET may be undefined!
	DCX	H		;HL-> start of filename
	STES	FNMGET,H	;Save for FNF error
	]

	POP	H		;Restore -> name
	JRZ	SETFEN		;Return if ESC, CR, TAB, or '['
;
	LXI	B,7		;7 chars left after this one
	CPI	'*'		;Is it wildcard '*'?
	JRZ	..4		;Yes, branch
;
	MOV	M,A		;Save assumed name byte
	CALL	FCBCHR		;Get next char
	JRZ	SETFEN		;Branch if ESC, CR, SP, TAB, or '['
	CPI	':'		;Was previous char drive?
	BNE	..3		;No, branch
;
;	Handle the drive designator
;
	MOV	A,M		;Get last char back
	MVI	M,' '		;Replace it with a space
	SUI	'A'-1		;Create disk number, A=1, B=2, etc
	DCX$	H		;HL-> drive
	MOV	M,A		;Set the drive
	MVI	C,8		;8 name bytes available
;
;	Handle the Filename
;
..2:	CALL	FCBCHR		;Get next command char
	JRZ	SETFEN		;Return if ESC, CR, SP, TAB, or '['
..3:	INX$	H		;HL-> next name char
	CPI	'.'		;Has '.' been found?
	JRZ	..6		;Yes, branch to scan extension
;
	DCR	C		;Decrement name length
	JM	NAMERR		;Error if too long
	CPI	'*'		;Is it wildcard?
	JRNZ	..5		;No, branch
;
..4:	INR	C
	CALL	FILLWC		;Fill field up with '?'
	DCX$	H
	JMPR	..2		;Next char better be '.'
;
..5:	MOV	M,A		;Save name char
	JMPR	..2		;Continue
;
;	Handle the Extent
;
..6:	DAD	B		;HL-> file extension
	MVI	C,4		;Extension length is 3 (adjust for DCR C)
..7:	CALL	FCBCHR		;Get next char
	JRZ	SETFEN		;Branch if ESC, CR, SP, TAB, or '['
	DCR	C		;Decrement name length
	JRZ	NAMERR		;Error if too long
	CPI	'*'		;Is it wildcard?
	JRNZ	..8		;No, branch
	CALL	FILLWC		;Fill field up with '?'
	JMPR	..7		;Next char should be delimiter
;
..8:	MOV	M,A		;Save char
	INX$	H		;Bump PTR
	JMPR	..7		;Continue
	EPROC	SETFCB
;
; SETFEN - Set User # variable.
;
	BPROC	SETFEN
SETFEN:	CPI	'='		;Is a user number specified?
	LD16	H,DEFUSR	;L= default user #
	JRNZ	..1		;No, use default user #
	CALL	GETDEC		;Yes, get specified user number in L
..1:	MOV	A,L		;Put in A
	POP	H		;HL-> FCB
	DCX$	H		;HL-> USR
	MOV	M,A		;Store user #
;
	LDA	CMDCHR		;Get last command char
	JMPR	CKEOFN		;Set flags
	EPROC	SETFEN
;
; CKEOFN - Check for proper filename terminator: CR, ESC, SP, TAB.
;
				;{SETRX,FCBCHR,SETFEN}
CKEOFN:	CALL	BLKCHK		;Check if delimiter was SP or TAB
	RZ			;Yes, return 'Z'
	JMP	CMPCE		;'Z' if CR or ESC
;
NAMERR:	LXI	H,BDFMSG	;HL-> error message
	JMP	MSGBRK		;Give error and abort
	.PAGE
;
; FCBCHX - Get next non-blank command character.
;	   Returns 'Z' if CR, ESC, TAB or '['.
;
				;{SETFCB,CKSPRM}
FCBCHX:	CALL	FCBCHR		;Get next UC command char
	RNZ			;Return 'NZ' if not terminator
	CPI	SPACE
	BEQ	FCBCHX		;Skip over spaces
	CMP	A		;Set 'Z'
	RET
;
; FCBCHR - Get next command char and convert lower to upper case.
;	   Return 'Z' if char is ESC, CR, SP, TAB, '[' or "=".  HL is preserved.
;
				;{SETFCB,FCBCHX}
FCBCHR:	CALL	NXTCHR		;Get next command/register char

				;{SETRX}
FCBCH1:	CALL	CONVUC		;Convert lower to upper case
	CPI	'['		;'[' ?
	RZ
	CPI	'='		;'=' ?
	RZ			;Yes, return with 'Z'
	JMPR	CKEOFN		;Terminator (ESC, CR, SP, or TAB) ?
;
; FILLFC - Fill FCB at (HL)+  for count C with char in A.
;
FILLWC:	MVI	A,'?'		;Get wildcard char
FILLFC:	MOV	M,A
	INX$	H
	DCR	C
	JRNZ	FILLFC
	RET

	IF	VPLUS, [
;
; FILTAG - Add edit buffer number to File extension for FCB at (DE).
;
	BPROC	FILTAG
FILTAG:	LDA	EDTNUM		;Get edit buffer number
	CALL	RTOASC		;Convert to ASCII
	CPI	'@'		;Is it main edit buffer?
	BNE	..1		;No, branch
	MVI	A,'$'		;Yes, change to old '$'
..1:	LXI	H,9		;Offset to first char of extent
	DAD	D		;HL-> extent in FCB
	MOV	M,A		;Save buffer number in FCB
	RET
	EPROC	FILTAG
	]
;
; FCBVAL - Checks if DE-> FCB contains valid characters.  Gives error if
;	   invalid char (* ? or control) encountered.
;		Note: Does not return if error
;
	BPROC	FCBVAL
FCBVAL:	PUSH	D		;Save DE-> FCB
	MVI	B,11		;Total length of filename
..1:	INX$	D		;Bump PTR (first skip over drive designator)
	LDAX	D		;Get next char
	CPI	' '		;Is it a control char?
	BLT	NAMERR		;Yes, error. (Stack will be fixed)
	CPI	'*'		;Is it special '*'
	BEQ	NAMERR		;Yes, error
	CPI	'?'		;Is it special '?'
	BEQ	NAMERR		;Yes, error
	DJNZ	..1		;Check all 11 chars
	POP	D		;Restore DE-> FCB
	RET
	EPROC	FCBVAL
;
; DIR - Display directory.
;	VPLUS:  If COLFLG, just return result in .rv.
;
;	AUXFCB/AUXUSR contains (ambiguous) filename. (8/20/86)
;
				;{EDCMD,EKCMD}
	BPROC	DIR
DIR:	MVIB$	TEMPFL,0FFH	;Set FNF flag
	CLR$	FCBPAD		;;Assume no filename padding
	CALL	RSTDMA		;Set DMA to 0080H
	LXI	D,AUXFCB	;DE-> FCB/AUXUSR
	CALL	SETUS0		;Change user #, if necessary
;
;	Display DIRECTORY drive & user #.
;
	IF	VPLUS, [
	CPIB$	RMINUS,'-'	;Show drive & user #?
	BEQ	..1		;No, skip
	CPIB$	COLFLG,':'
	BEQ	..1
	LXI	H,DIRMSG
	CALL	PRTSTR
	LXI	H,AUXFCB
	CALL	PRTDNM		;Display drive & user #
	CALL	CRLF
	MVIB$	FCBPAD,1	;Set flag to pad filename/extension
	]
;
;	Default to 4 fnames/line.
;
	TST$	NUMFLG		;User specified # columns?
	BNE	..1		;Yes, branch
	MVIB$	ITRCNT,4	;No, display 4 files/line
;
..1:	MVI	C,SRCHF		;Search first
..2:	LD8	ITRCNT		;Get # fnames/line
	MOV	B,A		;
;
;	Search & Display Loop.
;
..3:	LXI	D,AUXFCB	;Search for the file
	CALL	BDOSSV
;
;	If just checking...
;
	IF	VPLUS, [
	MOV	E,A		;Save result
	CPIB$	COLFLG,':'	;Just checking?
	BNE	..5		;No, continue
	LXI	H,0		;Yes, presume file not found
	INR	E		;File found?
	JRZ	..4		;No, branch, HL = FALSE
	INR	L		;Yes, set HL = TRUE
..4:	SHLD	RETVAL		;Set return result
	RET
;
;	else...
;
..5:	MOV	A,E		;Retrieve result
	]
;
	CPI	0FFH		;All done? (Preserve A)
	JZ	..9		;Files found, clear FNF flag
	PUSH	B		;Save count
	LXI	H,DEFDMA	;Point to dma buffer
	ANI	03H		;Get fcb index
	RRC			;*32 for buffer index
	RRC			;
	RRC			;
	MOV	E,A		;Put into DE
	MVI	D,0		;
	DAD	D		;Point to fcb
	CALL	PRTFC1		;Print the file name (padding set by FCBPAD)
	CALL	PSPACE		;
	CALL	PSPACE		;
	CLR$	TEMPFL		;Files found, clear FNF flag
	POP	B		;Restore count
	MVI	C,SRCHN		;Search next
	DJNZ	..3		;Do next file name if on same line
;
	CALL	CRLF		;Next line
	MVI	C,SRCHN		;
	JMPR	..2		;
;
..9:	TST$	TEMPFL		;Any files found?
	JZ	CRLF		;Yes, give CRLF
	LXI	H,FNFMS2	;No, give FNF message
	JMP	PRTMSG
	EPROC	DIR
	.PAGE
;
; RESTRT - Restart the editor by writing the entire output
;	   file out, and performing an EB on the output file.
;
				;{EACMD, Visual restart}
	BPROC	RESTRT
RESTRT:	LHLD	TXTFLR		;HL-> begin of window
	CALL	CNTFIL		;HL = # LFs written out (+1)
	DCX$	H		;Adjust
	SHLD	EASAV1		;Save away
	MOVW	EASAV2,EDTPTR	;Save current edit pos
;
;	Copy text marker positions into POSTBL
;
	IF	FULL, [
	LXI	B,PNTMAX*2	;BC = # bytes to move
	LXI	H,PNTTBL	;HL-> source
	LES	D,POSTBL	;ES:DE-> destination
	MVXO			;Copy out to destination segment
	]
;
;	Close all files.
;	Reset INFCB/INUSR from RENFCB/RENUSR.
;
	CALL	CLOSER		;Close all files
				;;BREAK out if write/close error
	LXI	H,RENUSR	;HL-> RENUSR/RENFCB
	LXI	D,INUSR		;DE-> INUSR/INFCB
	MVI	C,13		;user#, drive#, filename
	CALL	MOVE		;Move output name to input name
	CALL	INITXB		;Initialize file & edit buffer variables
				;;**** - Shouldn't really be necessary *****
;
;	Open input & output files.
;
	CLR$	NM2FLG		;Only 1 filename
	CALL	OPNIOX		;Open and auto-read the files
;
;	Restore edit position in the file unless -EA command.
;
	CPIB$	RMINUS,'-'	;;Is this -EA?
	PUSHF			;;Save flags
	BEQ	..1		;;Yes, leave at beginning of file
	MVIB$	GLBFLG,'_'	;Setup for "_L" command
	MOVW	ITRCNT,EASAV1	;Get old CNTOUT
	CALL	LCMD		;Perform "_L" command
	MVIW$	ITRCNT,0	;Setup for "0W" command
	CALL	WCMD		;Write all the text out
	CALL	ACMD		;Perform a "0A" - auto-read
	MOVW	EDTPTR,EASAV2	;Restore edit position
;
;	Restore text marker positions from POSTBL
;
..1:	POPF			;;Is this -EA?
	BNE	..2		;;No, restore markers
	TSTW	EASAV1		;;Were we at beginning of file?
	JRNZ	..3		;;No, can't restore markers
..2:
	IF	FULL, [
	LXI	B,PNTMAX*2	;BC = # bytes to move
	LES	H,POSTBL	;ES:HL-> source
	LXI	D,PNTTBL	;DS:DE-> destination
	MVXI			;Copy in from source segment
	]
;
..3:	JMP	TOCMD		;Back to command mode (check for "V")
	EPROC	RESTRT
	.PAGE
;
; READTX - Read the number of sectors specified in BC into the text
;	   buffer.  Update TXTRWF.  Set EOF flag if EOF encountered.
;	   HL in -> DMA, HL out = [TXTRWF].
;
;	   If REVFLG is set, reads from .REV file.
;
				;{NEXFIL,ACMD}
	BPROC	READTX
READTX:	LXI	D,INFCB		;Normally read from input FCB/USER #
	TST$	REVFLG		;Is the reverse file to be used?
	JRZ	..1		;No, skip
	LXI	D,REVFCB	;Yes, DE-> REVFCB/RENUSR
..1:	CALL	READSC		;Read the sectors
	SHLD	TXTRWF		;Set TXTRWF
	RET
	EPROC	READTX
;
; READSC - Read number of sectors specified in BC or until EOF.
;	   DE = FCB/USER # address,  HL = DMA address.
;
;	   Return: BC = # sectors read, DE = # sectors to have been read,
;		   HL -> EOF past last byte read.
;
				;{NEXFIL,ACMD}
	BPROC	READSC
READSC:	PUSH	B		;Save initial # sectors to be read
	CALL	SETSEC		;Save BC as sector count
	CALL	VISMSS		;Put up wait message, set NWSCFL = 089H
				;Save all regs
;
..1:	CALL	DECSEC		;Is sector count exhausted?
	JRZ	..END		;Yes, branch
	XCHG			;DE-> DMA
	CALL	DMABDS		;Set DMA address
	XCHG			;DE-> FCB, HL-> DMA
	PUSH	H		;Save DMA address
	CALL	DISKR		;Read one sector, DE-> FCB/USER #
	POP	H		;HL-> start of record just read
	PUSH	D		;Save -> FCB
	CALL	CHKEOF		;Was EOF reached?
	POP	D		;DE-> FCB/USER #
	JRNC	..1		;No, continue reading, HL -> past record
;
	PUSH	H
	CALL	CLSSRC		;Yes, close current FCB/USER #'s file
	POP	H
;
..END:	MVI	M,EOF		;Make sure there is an EOF
	CALL	RSTDMA		;Safety measure, HL saved
	POP	D		;DE = # records to have been read
	LBCD	SECCNT		;BC = # records not read
	JMP	SBDEBC		;BC = # records read
				;HL -> EOF past last byte read
	EPROC	READSC
;
				;{OPNAUX,CREAUX,WRITSC,WRTBND,READSC,RDPREV,EDCMD}
RSTDMA:
;6	MOV	DX,DS		;;{CP/M-86}
;6	MOV	CL,51		;;Set DMA base
;6	CALL	BDOSSV
	LXI	D,DEFDMA	;DE-> default DMA address
				;{READSC,DISKW,PRVSEC}
DMABDS:	MVI	C,SETDMA	;Change DMA address to prevent problems
;
; BDOSSV - Invoke operating system kernel, saving registers.
;
BDOSSV:	CALL	SV%BDH
	JMP	BDOS

	IFNOT	P8086, [
;
; BDOS - Invoke operating system
;
BDOS:
	IF	MEMVRS, [
	CALL	SCROUT
	]
	JMP	BASE+5
	]
	.PAGE
;
; WRITE -  Write number of bytes specified in BC.
;
;	   Enter:  BC = # bytes, DE-> FCB/USER #, HL-> DMA address.
;	   Return: BC = number of bytes actually written.
;		   AUXPUT-> past bytes in DEFDMA which needs Purging
;		  'C' if write error.
;
;	NOTE:  Currently assumes that the output file is open.
;
				;{WRTTXT,RSCMD}
	BPROC	WRITE
WRITE:	PUSH	H		;Save first write address
	SBCD$	SECCNT		;Save BC as # bytes to write
	CLR$	WTERFL		;Clear the write error flag
	PUSH	H		;-
	MVIW$	AUXPUT,DEFDMA	;Empty purge buffer in case of write error
	POP	H		;-
;
	IF	DEMO, [
	LXI	H,DEMMSG	;HL-> DEMO message
	CALL	PRTMSG
	]
;
..1:	PUSH	H		;Save DMA address for next write
	LHLD	SECCNT		;HL = # bytes left to write
	LXI	B,-128		;Account for one sector worth
	DAD	B		;HL = # bytes after this write
	JRNC	..2		;Branch if less than one sector left
	SHLD	SECCNT		;Save
	POP	H		;HL-> bytes to be written
	PUSH	H
	PUSH	D
	CALL	DISKW		;Set DMA, write a sector, check for error
	POP	D
	POP	H
	JRC	..DONE		;Get out if write error
	LXI	B,128		;Size of sector
	DAD	B		;HL = new DMA address
	JMPR	..1		;Continue
;
..2:	POP	H		;HL-> text to write
	LBCD$	SECCNT		;BC = # bytes left
				;If BC == 00 will set AUXPUT = DEFDMA
	LXI	D,DEFDMA	;Copy them to default DMA buffer
				;HL-> bytes to be written
	CALL	RTLDIR		;Move the bytes
	SDED	AUXPUT		;Save -> end of bytes in DEFDMA[80]
				;HL-> past last bytes "written"
..DONE:	CALL	RSTDMA		;Reset DMA back to 80H (Save HL)
	POP	D		;DE = write begin address
	CALL	SBHLDE		;BC = number bytes written
				;{WRTFIX}
WTERCK:	TST$	WTERFL		;Test flag, return 'C' if error
	RZ			;Return 'NC' if no error
	STC
	RET			;Return 'C' if error
	EPROC	WRITE
;	.PAGE
;
; CLOSER - For EX command, write text buffer out, close and rename files.
;;	   BREAK out if write/close error.
;
				;{EXCMD,EYCMD}
CLOSER:	CALL	WTOPCH		;Make sure output file open
CLOSE1:	CALL	NEXFIL		;Move text until EOF is found
	JRZ	CLOSE1		;Repeat until end of input file in memory
;
	LHLD	TXTRWF		;HL-> end of text buffer
	CALL	WRTTXT		;Write out all full sectors of text
				;This includes CALL to WRTFIX
	JC	BREAK		;*BREAK* if out of disk space
;
;	Write any remaining bytes between TXTBAS and TXTFLR.
;;	BREAK out if write/close error.
;
				;{EFCMD}
CLOSE2:
	IF	VPLUS, [
	MVIB$	DELFLG,1	;Force .BAK deletion.  Kludge needed in case
				;same file edited in multiple buffers
	]
;
	CALL	STBRFL		;Set flag to rewrite status line "FILE" message
	CALL	DELBAK		;Delete any .BAK file, check for file open
;
	SUB%BC	TXTFLR,TXTBAS	;BC = # remaining bytes to be written
	PUSH	B
	XCHG			;HL-> text to write
	LXI	D,OUTFCB	;DE-> FCB to close
	CALL	WRITE		;Write the last bytes out
				;They will go into DEFDMA
;
	LXI	D,OUTFCB	;DE-> FCB to close
	CALL	CLOSE		;Write remaining text in DEFDMA, close FCB
				;;*BREAK* if write/close error
;
	POP	B		;BC = # bytes moved
	CALL	WRTFIX		;Move text down, fix pointers
	CALL	CNTINI		;Clear CNTOUT = # LFs written out
;
	IF	MPM, [
	CALL	CLSREN		;Close holding FCB/USER #
	]			;EQCMD also has this MPM condition
;
;	Rename any existing output file to .BAK
;
	IFNOT	DEMO, [
	LXI	D,RENFCB+16	;DE-> rename "TO" file
	CALL	FRMBAK		;Setup Rename "TO" file with .BAK
	LXI	D,RENFCB	;DE -> rename FCB/USER #
	CALL	RENBDS		;Rename file to FILE.BAK
;
; Now use OUTFCB as rename FCB to rename file.$$$ to file.ext.
;
	LXI	H,RENFCB	;HL-> full output file name
	LXI	D,OUTFCB+16
	MVI	C,16		;16 bytes to copy
	CALL	MOVE		;Move original name to 2d half of OUTFCB
;
	LXI	D,OUTFCB	;DE -> Rename FCB/USER #
RENBDS:	MVI	C,RENAME
	JMP	BDSUSR		;Rename .$$$ to original file, return
				;Return 'C' and A == 00 if error
	]			;<IFNOT DEMO>
;
	IF	DEMO, [
	LXI	D,OUTFCB	;DEMO - just delete the .$$$ file
	JMP	DELBDS
	]
;
;
; CLOSE - Close file by flushing remaining text in DEFDMA and closing FCB.
;
;	  Enter: DE-> FCB, AUXPUT-> past last byte in DEFDMA.
;;	  BREAK out if write error (disk full) or close error.
;
;	  Note:  Can only be called immediately after WRITE()
;
				;{CLOSE2,RSCMD}
	BPROC	CLOSE
CLOSE:	PUSH	D		;Save DE-> FCB
	LHLD	AUXPUT		;HL-> past last text char
	LXI	D,DEFDMA	;DE-> buffer
	CALL	CMHLDE		;Is purge buffer empty?
	JRZ	..2		;Yes, branch
;
	TST$	WTERFL		;Did last write result in an error?
	JRNZ	..2		;Yes, then just close file
;
	MVI	M,EOF		;Insert 1 EOF (OK to clobber loc 100H)
	INX$	H		;HL-> pad area past EOF
	LXI	D,DEFDMA+128	;DE-> past end of buffer
	XCHG
	CALL	SBHLDE		;BC = bytes left to pad in buffer
	JRC	..1		;Be safe, be cool
	XCHG			;HL-> pad area past EOF
	LDA	EOFPAD		;Fill with EOFPAD char
	CALL	FILL
..1:	LXI	H,DEFDMA	;HL-> sector to write
	POP	D		;DE-> FCB
	PUSH	D
	CALL	DISKW		;Write the last bytes out
				;Set WTERFL if write error
..2:	MVIW$	AUXPUT,DEFDMA	;Clear PTR/flag
	POP	D		;DE-> FCB to close
	CALL	IFCLOS		;;Close the FCB, return 'C' if close error
	JRC	..ERR		;;
	TST$	WTERFL		;;Write error?
	RZ			;;No, return
;;
..ERR:	CLR$	WTERFL		;;Don't need to know about write error anymore
	JMP	BREAK		;;BREAK out
	EPROC	CLOSE
	.PAGE
;
; CLSAUX - Close AUXFCB if running MP/M.
;
				;{RLCMD,EGCMD,BREAK}
CLSAUX:	LXI	D,AUXFCB	;DE-> FCB/USER # to close
	JMPR	CLSMPM		;Close the file only if this is MP/M
;
; CLSREN - Close RENFCB if running MP/M.
;
	IF	MPM, [
				;{CLOSER,EZCMD}
CLSREN:	LXI	D,RENFCB	;DE-> FCB/USER # to close
	JMPR	CLSMPM		;Close file if this is MP/M
	]

				;{SETINP,EZCMD}
RELSRC:	MOVW	TXTRWF,TXTCEL	;Delete any unread input buffer
	MVIB$	TOPCHR,EOF	;Clear TOPCHR
;
; CLSSRC - Close INFCB if running MP/M.
;	   Return: DE-> INFCB/INUSR.
;
				;{RELSRC^,READSC}
CLSSRC:	LXI	D,INFCB		;DE-> input FCB/USER #
;	JMPR	CLSMPM		;Close file if this is MP/M
;
; CLSMPM - Explicitly close files for MP/M.
;	   Input FCBs don't need closing otherwise; wastes disk time.
;
;	   Enter: DE-> FCB/USER #.
;
				;{CLSSRC^,CLSREN,CLSAUX}
CLSMPM:	CALL	CLSFLG		;Test/Clear the FCB open flag
	RZ			;Return 'NC' if the FCB wasn't open
;
	IFNOT	MPM, [
	RET
	]
;
	IF	MPM, [
	CALL	MPMCHK		;Is this MP/M?
	RZ			;No, don't bother with closing
;
	CALL	CLSBDS		;Yes, close file if it is open
				;Return 'C' if close error
	RNC			;Ok close, all done
	JMP	MSGBRK		;Close error.  HL-> message
	]
;
; CLSFLG - Clear file-open-flag associated with DE's FCB.  Return 'Z' & 'NC'
;	   if file not open.  (Caller's caller uses C/NC).
;
				;{CLSMPM,IFCLOS}
CLSFLG:	MOV	H,D
	MOV	L,E		;HL-> open flag
	DCX$	H		;HL-> user #
	DCX$	H		;HL-> open flag
	MOV	A,M		;Get open flag, == 0 if not open
	MVI	M,0		;Clear file-open flag
	ORA	A		;Is FCB open?
	RET			;Return 'Z' and 'NC' if FCB not open
	.PAGE
;
; CLSOUT - Close output file.  Return 'Z' if not open or close error.
;
				;{DELOUT}
CLSOUT:	CALL	STBRFL		;Set flag to rewrite status line "FILE" message
	LXI	D,OUTFCB	;DE-> Output.$$$ FCB/USER #
	JMPR	IFCLOS
;
; CLSAUW - Close auxiliary file after writing.
;
				;{RSCMD}
CLSAUW:	LXI	D,AUXFCB	;DE-> FCB/USER #
	JMPR	IFCLOS		;Close it
;
; CLSREV - Close reverse file.  Return DX-> REVFCB/RENUSR.
;	   Return: 'Z' if not open or close error.
;
				;{DELREV}
CLSREV:	LXI	D,REVFCB	;DE-> .$R$ FCB/USER #
;	JMPR	IFCLOS		;Close the file if open, saving DE
;
; IFCLOS - Closes FCB if it is open.
;	   Enter: DE-> FCB.
;	   Retrn: 'Z' if file not open or close error.
;		  'C' if close error.
;
				;{CLSOUT,CLSREV,CLSAUW^}
IFCLOS:	CALL	CLSFLG		;Clear the FCB open flag at -2[DE]
	RZ			;Return 'Z' and 'NC' if file not open
;	JMP	CLSBDS
;
; CLSBDS - Close file.  DX-> FCB/USER #.  Saves DX. Trash BX.
;	   Return: 'C' and 'Z' if close error.
;
				;{IFCLOS^,CLSMPM,FCBKUP}
CLSBDS:	MVI	C,CLOSEF
	CALL	BDSUSR		;Set user #, close file, save regs
				;'C' and A == 00 if error
	RNC			;Return if no error
	LXI	H,CLSMSG	;Assume Yes, HL -> error message
	JMP	ERRCON		;Give error, without break
				;Return 'C' and A == 0
	.PAGE
;
; DELREV - Close and delete .REV file
;
				;{EQCMD,DSKREV}
DELREV:	CALL	CLSREV		;Close .REV file, DX-> FCB/USER #
	JRNZ	DELBDS		;Delete the file if it was open
	RET
;
; DELOUT - Close and delete .$$$ file
;
				;{EQCMD}
DELOUT:	CALL	CLSOUT		;Close .$$$ file, DX-> FCB/USER #
	JRNZ	DELBDS		;Delete the file if it was open
	RET
;
; DELBAK - Delete old .BAK file to make more room on disk.
;
				;{WRTTXT,CLOSER}
DELBAK:	CALL	SV%BDH		;Save regs
	CALL	WTOPCH		;Make sure output file open
	TSTM	DELFLG		;Is .BAK to be deleted?
	RZ			;No, return now
;
	IF	DEMO, [
	RET			;DEMO - don't delete .BAK file
	]
;
	MVI	M,00		;Yes, clear the flag and delete
	LXI	D,AUXFCB	;Use AUXFCB/AUXUSR
	CALL	FRMBAK		;Setup FILENAME.BAK
	MOVB	AUXUSR,RENUSR	;Associate user # with AUXFCB
;
				;{DELBAK^,EKCMD}
DELAUX:	LXI	D,AUXFCB	;DE-> AUXFCB/AUXUSR
;
				;{CREFIL}
DELBDS:	MVI	C,DELETE
	JMP	BDSUSR		;Set user #, delete file, save DX, trash BX
				;Return: 'C' and A == 00 if file not found
				;	 If MPM error: 'C' and BH != 00.
	.PAGE
;
; DISKR - Read one sector from disk.  DE-> FCB/USER #.  [05/8/86]
;	  Considers INFCB to be the continuation of REVFCB.  Changes DE
;	  from REVFCB to INFCB when .REV is found to be empty.
;
;	  Enter: DE-> FCB to use
;	  Return: A = read status from BDOS: (CP/M)
;		  0 = Read successful, 1 = EOF reached, 255 = error
;	  Returns codes are checked at CHKEOF.
;
				;{READSC}
	BPROC	DISKR
DISKR:	LXI	H,REVFCB	;Reading from REVFCB?
	CALL	CMHLDE
	BNE	DISKR1		;No, skip
;
	CALL	DSKREV		;Yes, read from .REV file
	RNC			;Return if EOF not reached
;
	TST$	INFLG		;EOF reached, is Input file open?
	JRNZ	..0		;Yes, read from Input file
	MVI	A,1		;No, return code for EOF is checked in CHKEOF
	RET

..0:	LXI	D,INFCB		;DE -> input FCB/USER #

				;{DSKREV,RLCMD,EGCMD}
DISKR1:	MVI	C,READF		;Get Read command
	PUSH	H
	CALL	BDSUSR		;Read a sector, save BC and DE
	POP	H
	DCR	A		;Undo INR A in BDSUSR
	RET
	EPROC	DISKR
;
; DISKW - Set DMA from HL, write a sector to disk using DE-> FCB/USER #. [1/10/86]
;	  Return: 'C' if write failure.
;
				;{WRITE,WRTBND,CLOSE2}
DISKW:
	IF	DEMO, [
	XRA	A		;DEMO - return 'NC'
	RET
	]
;
	XCHG			;DE = DMA address, HL-> FCB
	CALL	DMABDS		;Set DMA address from DE  (Save HL)
	XCHG			;DE -> output FCB

	MVI	C,WRITEF	;Get Write command
	PUSH	H
	CALL	BDSUSR		;Write the sector
	POP	H
	DCR	A		;Undo INR A in BDSUSR
	STA	WTERFL		;;Set/reset the write error flag
	RZ			;Return 'NC' and 'Z' if no error
;
	STA	DSKFUL		;;Set disk full flag for MAKSPC/WRTSTA
	LXI	H,WMSG		;;HL -> 'NO DISK SPACE'
;
; ERRCON - Display HL's message.  Return 'C' & 'Z' & A == 0.
;
				;{CLSBDS,DISKW^}
ERRCON:	PUSH	B		;;
	PUSH	D		;;
	PUSH	H		;;
	CALL	RSTCON		;Make sure error goes to console
	CALL	PURGEK		;;Purge any pending keystrokes
	POP	H		;;
	MVI	A,0D0H		;CRLF() fore/aft (cm), temporary pause (vm)
	CALL	MSGHND		;Display message
	POP	D		;;
	POP	B		;;
;;
	XRA	A		;;Return 'Z' & A == 0
	STC			;Also return 'C'
	RET
;
;
; AXREAD - Read a sector using AUXFCB into DEFDMA.
;	   Return: See CHKEOF.
;
;				;{AUXCHR,RLCMD,EGCMD}
AXREAD:	LXI	D,AUXFCB	;DE-> FCB
	CALL	DISKR1		;Read one sector
	LXI	H,DEFDMA	;HL-> sector start
;	CALL	CHKEOF		;Reached EOF?
;
; CHKEOF - Check if EOF (or error) encountered in last sector read.
;	   BREAK out if read error.
;
;	   EOF is reached if CP/M returns EOF code or if EOF
;	   character is encountered in sector.
;
;	   Enter:  A = CP/M return code, HL-> begin of sector
;		   0 = Read successful, 1 = EOF reached, >3 = error
;	   Return: DE-> begin of sector, HL-> end of record (+1)
;		   'C' if EOF reached (HL-> EOF)
;		   'C' and 'Z' if EOF and NULL record.
;
				;{READSC,AXREAD^}
	BPROC	CHKEOF
CHKEOF:	MOV	D,H		;Set DE to begin of record
	MOV	E,L
	CPI	4		;Error? (=255 for CP/M.  0-3 handled by MSDOS)
	BLT	..1		;No, check for EOF
;
	LXI	H,RMSG		;Yes, HL -> error msg
	JMP	MSGBRK		;Fatal error, back to command mode


..1:	CPI	1		;EOF return code?
	MVI	A,EOF		;Search for EOF
	BEQ	..3		;Yes, branch to set EOF char
				;Yes, return 'C' and 'Z'
;
;	Scan sector just read for EOF character
;
	LXI	B,128		;BC = size of sector
	CCIR			;Let HL-> EOF or end of sector
	JRZ	..2		;Branch if EOF character found
;
	ORA	A		;No EOF - return 'NC'
	RET			;DE-> begin of sector, HL-> past end
;
..2:	DCX$	H		;HL-> EOF char
..3:	MOV	M,A		;Make sure there is an EOF
	CALL	CMHLDE		;Set 'Z' if NULL record
	STC			;Set 'C'
	RET
	EPROC	CHKEOF
	.PAGE
;
; BDSUSR - Set user # from [DE-1], call BDOS, save BC and DE.
;	   Return: OK    - 'NC' A = return code +1, HL = return value.
;		   Error - 'C'  A = 00, HL = return value.
;				If MPM error: H != 00.
;
;	   Note: Return codes are invalid for Read and Write.
;
				;{OPNFIL,CREFIL,CLSBDS,RENBDS,DELBDS,FCBKUP}
BDSUSR:	CALL	SETUS0
	PUSH	B		;;Save BC and DE only
	PUSH	D
	CALL	BDOS		;CP/M returns A=255 if error
	POP	D
	POP	B
	INR	A		;A = 0 if error
	STC
	RZ			;Return 'C' and 'Z' and A = 00 if error
	ORA	A
	RET			;Return 'NC' and 'NZ' if no error
;
; SETUS0 - Set user # from [DE-1] saving registers.  (DE-> FCB)
;
				;{DIR,BDSUSR}
SETUS0:	DCX$	D		;DE-> user #
	LDAX	D		;Get it
	INX$	D		;Restore DE
;	JMP	SETUSR
;
; SETUSR - Set User #/CURUSR to A if different from current value.
;	   Registers saved.  Gets called a lot, so be QUICK.
;
				;{SETUS0^,EUCMD}
	BPROC	SETUSR
SETUSR:	PUSH	H
	LXI	H,CURUSR	;HL-> current value
	CMP	M		;Are they same?
	JRZ	..1		;Yes, nothing to do
	PUSH	D
	PUSH	B
	MOV	M,A		;Save new user #
	MOV	E,A		;Put in E
	MVI	C,CPMUSR	;SET/GET User Code function
	CALL	BDOS
	POP	B
	POP	D
..1:	POP	H
	RET
	EPROC	SETUSR

	IFNOT	P8086, [
;
; MPMCHK - Return 'NZ' if running MP/M or CCP/M.  Save ALL regs.
;
				;{SETPOL, F1 routines}
MPMCHK:	PUSH	B
	MOV	B,A
	ANIB$	CPMVER+1,5	;Look at MP/M and CCPM/86 bits
	MOV	A,B
	POP	B
	RET
	]
	.PAGE

	IF	VPLUS, [
;
;	EU - Set/Show Drive/User-#
;
	BPROC	EUCMD
EUCMD:	CALL	FCBCHX		;Get next non-blank Upper-Cased command char
	CALL	LETCHK		;Is it a drive designator?
	BNE	..1		;No, skip
	SUI	'A'-1		;Conver to binary, A=1, B=2, etc
	CALL	SETDRV		;Login the specified drive, set DEFDRV
	CALL	FCBCHR		;Get next Upper-Cased command char, 'Z'=>terminator
;
..1:	CALL	DIGCHK		;Digit?
	BNE	..2		;No, skip
	CALL	BAKGET		;Backup
	CALL	GETDEC		;Get user #
	MOV	A,L		;A = user #
	STA	DEFUSR		;New default user #
	CALL	SETUSR		;Set new user #/CURUSR
;
..2:	CPIB$	CMDCHR,':'	;':' delimiter?
	CZ	NXCMCH		;Yes, bypass it
	CPIB$	RMINUS,'-'	;Echo the change?
	RZ			;No, return
	JMP	SHWDDU		;Yes, display default drive/user #
	EPROC	EUCMD
;
; SHWDDU - Display default drive/user-#.
;
				;{EUCMD}
SHWDDU:	LDA	DEFUSR		;Get user #
	MOV	B,A		;Save in B
	LDA	DEFDRV		;1=A, 2=B, etc
	CALL	SHOWDU		;
	JMP	IFCRLF		;
;
; SHOWDU - Display A = drive, B = user #.
;
				;{SHWDDU}
SHOWDU:	CALL	SHOWDR		;Display drive # as ASCII letter
	MOV	A,B		;Get user #
;	JMP	SHWUSN
;
; SHWUSN - Display User # in A.
;
				;{SHOWDU^,PRTDNM}
SHWUSN:	CPI	10		;Only 1 digit?
	JRC	SHWUS2		;Yes, skip
	PUSHA			;No, save user #
	MVI	A,'1'
	CALL	PCHARA		;Display tens digit
	POPA			;Get user # back
	SUI	10		;Get units digit
SHWUS2:	CALL	RTOASC		;Convert to ASCII
	JMP	PCHARA		;Display on console
;
; SHOWDR - Display drive # in A (0,1,2...) as ASCII letter (A,B,C...).
;
				;{SHOWDU,PRTDNM}
SHOWDR:	ADI	'A'-1		;Convert to AASCII A, B, etc
	JMP	PCHARA		;Display on console
;
; PRTDNM - Print du: (drive/user #) from FCB/USER # <- by HL.	  (9/1/85)
;	   Does not display user # 0.
;
				;{PRTFNM,DIR}
PRTDNM:	MOV	A,M		;A = drive #
	CALL	SHOWDR		;Display as ASCII letter
	DCX$	H		;HL-> user #
	MOV	A,M		;A = user #
	ORA	A		;User 0?
	CNZ	SHWUSN		;Show user # if not 0
	MVI	A,':'
	JMP	PCHARA		;Display ':'
;
;
; SETDRV - Login A as the current/default drive.  Drive A=1, B=2, etc.
;
				;{EUCMD}
SETDRV:	STA	DEFDRV		;Save drive # according to this convention
	DCR	A		;But CP/M wants A=0, B=1,
	MOV	E,A
	MVI	C,SETDSK	;Code to login specified drive
	JMP	BDOS		;Log it in
	]			;<IF VPLUS>
	.PAGE
;************************************************
;*						*
;*	Reverse Disk Buffering Routines		*
;*						*
;************************************************
;
;	Last Change: Ted - Sept. 24, 1985
;
; DSKREV - Read backwards from .REV file.   [5/08/86]
;
;	   Return 'C' if EOF reached (nothing read)
;          else 'NC' and A = read return code.
;
				;{DISKR only}
	BPROC	DSKREV
DSKREV:	CALL	RVBKUP		;Backup reverse FCB, return DE -> REVFCB
	JRNC	..1		;Branch unless EOF (really BOF) reached
;
	CALL	DELREV		;EOF reached, so close and delete .REV file
	STC			;Return 'C' for test in DISKR
	RET
;
..1:	CALL	DISKR1		;Read sector, return A = true BDOS code
	PUSHA			;Save return code in A
	CALL	RVBKUP		;Decrement the auto-increment.  DE-> REVFCB
	POPA			;Restore return code
	ORA	A		;'NC' for success
	RET			;A = 0 unless wierdness happened
	EPROC	DSKREV
;
; WRTBND - Write out text from (HL) to end of text buffer to .REV file
; 	   and "delete" from the text buffer.	(02/20/86)
;
				;{WCMD,PRVFIL,ENCMD}
	BPROC	WRTBND
WRTBND:	PUSH	H		;Save -> text to write out
	XCHG			;DE-> text to write out
	CALL	VISMSS		;Need screen rewrite even if no Disk I/O
	CALL	RESTOP		;Restore real character at TXTCEL
	LHLD	TXTRWF
	CALL	CMPWRT		;BC = number of sectors to write
	MOV	A,B
	ORA	C		;No full sectors to write?
	JRZ	..4		;NONE - set new TXTCEL, check EDTPTR
;
;	Open .REV file.
;
	CALL	SETSEC		;Save BC as sector count
	TST$	REVFLG		;Is the .REV file already open?
	JRNZ	..1		;Yes, branch around open code
;
	CALL	CLRREV		;Clear counters in REVFCB, DE-> REVFCB
	MOVB	REVUSR,DEFUSR	;Associate default user number with FCB
;
	CALL	CREFIL		;Create the file
				;Sets REVFLG if file created OK
	JC	CREERR		;Error - give error message
;
;	Get ready to write to .REV file
;
..1:	CLR	WTERFL		;Clear write error flag
	LHLD	TXTRWF		;Starting dma address + 128
;
;	HL = DMA + 128
;	(SECCNT) = sector count (+1)
;
..2:	CALL	DECSEC		;Is sector count done?
	JRZ	..3		;Yes, done
	LXI	D,-128		;Point to dma address
	DAD	D		;HL = DMA
	PUSH	H		;Save dma address
	LXI	D,REVFCB	;DE-> FCB/USER #
	CALL	DISKW		;Set DMA, write sector, check for error
	POP	H		;Restore dma address
	JRNC	..2		;Branch if no error
;
	LXI	B,128		;BC = sector size
	DAD	B		;Adjust HL because no write occurred
;
..3:	LDED	TXTRWF		;+DE = old TXTRWF
				;HL = end of write = new TXTRWF
	CALL	PCKTX1		;Pack the text down, update pointers
	CALL	RSTDMA		;Reset DMA to 80H
;
	TST$	WTERFL		;Did a write error occur?
..4:	POP	H		;HL-> desired new end of text
	JMP	NEXFI5
;	JMP	AUTEN1		;Set TXTCEL from HL if no error.
				;Else use PREVLF from TXTRWF
	EPROC	WRTBND
	.PAGE
;
; RDPREV - Read in previous number of lines specified by ACMD.	[11/13/85]
;	   Sectors are read back in reverse order, beginning with
;	   the bottom of the text buffer and going down.
;
				;{PRVFIL,ACMD}
	BPROC	RDPREV
RDPREV:	CALL	WTOPCH		;Make sure output file is open
	CALL	VISMSS		;Put up wait message, set NWSCFL = 089H
				;Save all regs
	CALL	GETITR		;Is command "-0A" or CALL from PRVFIL?
	JRZ	..1		;Yes, branch
;
	INX$	B		;Adjust - first LF found begins current line
	SBCD$	ITRCNT
	LXI	B,1		;Leave one byte free
	CALL	CMPSEC		;BC = # sectors there is room for
	JMPR	..2
;
..1:	CALL	CMPEND		;BC = # sectors there is room for
;
;	BC = # sectors there is room for.  OK if BC = 00.
;	Move text to top of memory.  Move is a MULTIPLE of 128.
;
..2:	CALL	JAMUP		;Move text to top of memory
				;DELPTR -> bottom of text
;
;	Scan backwards from TXTFLR to DELPTR, looking for required
;	# of LFs.  If DELPTR reached, read in another sector.
;	"-0A" will read as much as there is space for.
;
..3:	SUB%BC	TXTFLR,DELPTR	;BC = # bytes to scan
..4:	JCXZ	..6		;Branch if count is zero
;
	DCX$	H		;HL-> next char to scan
	MOV	A,M		;Get character
	CPI	LF		;Is it a line feed?
	BNE	..5		;No branch
;
	INX$	H		;Yes, HL-> past LF
	PUSH	B
	CALL	DECITR		;Read in enough?
	POP	B
	JRZ	..7		;Yes, branch
	DCX$	H		;No, restore HL
;
..5:	DCX$	B		;Decrement count
	JMPR	..4		;Keep looking until count done
;
;	Read in another sector
;
..6:	SHLD	TXTFLR		;Save as new starting PTR
	CALL	PRVSEC		;Read previous sector
				;Update DELPTR and CNTOUT
	JRNC	..3		;Keep scanning unless BOF or SECCNT reached
	LHLD	DELPTR		;If BOF found, set TXTFLR to first text byte
	CZ	BOFLIN		;If memory full, set TXTFLR past first LF
;
;	Here if -nA count reached, (not -0A), set TXTFLR.
;
..7:	SHLD	TXTFLR		;Set TXTFLR - will move down with BUFFDW
;
;	Have read in all that we can or need to read in.
;	DELPTR -> first byte of text to be moved to TXTBAS.
;
	JMP	JAMDWN		;Move text back down into position
	EPROC	RDPREV
;
; PRVSEC - Read the previous sector in at 128 bytes before DELPTR
;	   Update DELPTR and CNTOUT if sector read successful.
;	   Return: 'C' & 'NZ' if BOF reached - HL = DELPTR -> begin of text
;	           'C' & 'Z'  if SECCNT exhausted.
;
	BPROC	PRVSEC
PRVSEC:	TST$	BOFFLG		;Has Begin of output file been reached?
	JRZ	..BOF		;Yes, return 'C' and 'NZ'
	CALL	DECSEC		;Is sector count exhausted?
	STC			;Assume exhausted
	RZ			;Return 'C' & 'Z' if sector count exhausted
;
	LHLD	DELPTR		;HL-> begin of text
	LXI	D,-128		;First sector is 128 below (DELPTR)
	DAD	D		;HL = DMA
	PUSH	H		;Save DMA
	XCHG			;Put dma address into DE
	CALL	DMABDS		;Set dma address
	LXI	D,OUTFCB	;Back up fcb pointer
	CALL	FCBKUP
	POP	H		;HL = DMA
	JRC	..BOF		;Branch if begin of output file reached
;
	CALL	DISKR1		;Able to read in a sector?
	JRNZ	..BOF		;Nope, assume BOF
;
;	Read was successful, HL = DMA just used.
;
	SHLD	DELPTR		;Save new begin of text
	CALL	FCBKUP		;Backup FCB to sector just read
;
;	Count total LFs and update CNTOUT.
;
	LHLD	DELPTR		;HL-> begin of sector just read
	MVI	A,LF		;Scan for LF
	LXI	D,00		;Init counter
	MVI	B,128		;Length of sector
..1:	CMP	M		;Is char LF?
	JRNZ	..2		;No, branch
	INX$	D		;Yes, count it
..2:	INX$	H		;Bump pointer
	DJNZ	..1		;Check entire sector
;
	LHLD	CNTOUT		;HL-> count of LFs written
	DSUB	D		;Subtract # LFs read back
	SHLD	CNTOUT		;Save new count
	XRA	A		;Return 'NC'
	RET
;
;	Here if Begin of File found
;
..BOF:	CALL	CNTINI		;Clear CNTOUT count of # LFs written out
	CLR	BOFFLG		;Set flag that begin of Output file
	CPI	1		;Return with 'C' and 'NZ'
	RET
	EPROC	PRVSEC
	.PAGE
;
; FCBKUP - Backup the FCB @DE to the previous record.  Allows reading files
;	   backwards.
;	   Return: 'C' if at beginning of file. DE saved.
;
RVBKUP:	LXI	D,REVFCB	;DE-> reverse FCB
				;{RDPREV}
	BPROC	FCBKUP
FCBKUP:	MOV	L,E		;Put FCB address into HL
	MOV	H,D
	LXI	B,32
	DAD	B		;HL-> NR field
	MOV	A,M		;Get the record number
	SUI	1		;Back it up (Note: DCR A does not set 'C')
	PUSHF			;Save flags
	ANI	7FH		;Mod 127
	MOV	M,A		;Put new record number into FCB
	POPF			;If no carry, extent did not change
	RNC
;
;	Need to backup extent also.  Before backing up extent, close it.
;
	LXI	B,-32+12	;Point to the extent
	DAD	B
	MOV	A,M		;Get it
	SUI	1		;Decrement it
	JRC	..BOF		;Branch if carry - BOF was reached
;
;	Close the extent. (First extent is not closed - BOF check above)
;	Really only needed for OUTFCB.
;
	PUSHA			;Save new extent
	PUSH	H		;Save -> extent byte
	CALL	CLSBDS		;Close the extent. DE-> FCB/USER #
	POP	H		;Restore
	POPA
	MOV	M,A		;Save new extent
;
;	Open the new extent.
;
	MVI	C,OPENF
	JMP	BDSUSR		;DE-> FCB/USER #
				;'C' and A == 00 if error
				;Nearly impossible to get error
;
..BOF:	LXI	B,32-12
	DAD	B		;HL-> NR
	MVI	M,0		;Set NR to 00 since it was set to 7F above
	STC			;Flag BOF
	RET
	EPROC	FCBKUP
