	.PAGE
	.TITLE	'VPLUS - B1'
;************************************************
;*						*
;*	  Initialization Section		*
;*						*
;************************************************
;
; Copyright (C) 1987 by CompuView Products, Inc.
;			1955 Pauline Blvd.
;			Ann Arbor, MI 48103
;
;	Written by: Theodore J. Green
;
;	Last Change: Ted - Apr. 08 - New memory management
;			   Apr. 10 - Order for overlaying stack
;			   Apr. 27 - Create edit buffer code inline @ BEGIN
;			   July 18 - Rearranged modules
;		     Tom - Aug. 19 - INIT0 call COPYI to INSTALL tables SW,PRM,TAB
;		     Ted - Sep. 18 - ATVSSW renamed BGCMSW in VISOPT().
;			   Oct. 13 - INIT0() set INPFLG
;		     Tom - Oct. 27 - LODEXE() restore edtseg
;		     Ted - Nov. 07 - Change LINTBNL to POSTBL
;		     Tom - Dec. 08 - PRCPRM(), OPNEXE() for -i,-x,-s together
;		     Ted - Dec. 19 - INIT0() decrements PYLLEN for CRTVRS
;			 - Mar. 31, 1987 - Fix YMCMD bug for "|" character
;
; INIT0 - First priority initialization.
;
				;{BEGIN}
	BPROC	INIT0
INIT0:	TST$	BGCMSW		;Begin in command mode?
	MVI	A,2CH		;For command mode
	JRNZ	..1		;Yes, branch
	MVI	A,80H		;No, begin in visual mode
	STA	INPFLG
;
..1:	CALL	POLRST		;;Init type-ahead buffer NOW
;6	MOV	CURSEG,DS	;For BREAK recovery
;
	IF	MEMVRS,	[
	CLR$	BANKFL		;Initialize bank select flag
	]
	IF	CRTVRS, [
	LXI	H,PYLLEN	;;HL-> # physical line length
	DCR	M		;;Prevent wrapping at end of lines
	]

	CALL	COPYI		;INSTALL tables ESTBL, EPTBL, TABTBL
	LDA	WILDCH		;;Get INSTALLED wildcard char.
	STA	GRP0PT		;;Save "|" in YM command strings
	STA	GRPGPT
	LXI	H,INIVLS	;HL-> table of some initial values
	JMP	SETVLS		;Store into runtime variables
	EPROC	INIT0
;
; PRCPRM - Process invocation parameter line.	[12/08/86]
;
;	   Parameters:
;
;	   -i:  Substitute specified file for VEDIT.INI.  Open and
;		process, whether or not INIFLG set.
;		Using an unknown filename effectively turns INIFLG off.
;
;	   -s:  Small model (8086) flag.  Already processed.  Skip over.
;
;	   -x:  Load and execute specified file after executing any
;		-i/.INI file and after opening and loading any edit file.
;
PRMDEC:	DB	'I'
	DW	PARMI
	DB	'S'
	DW	PARMS
	DB	'X'
	DW	PARMX
	DB	KFF
				;{BEGIN}
	BPROC	PRCPRM
PRCPRM:	CALL	SETPRM		;;Make parameter line into VEDIT command line
..1:	CALL	FCBCHX		;Get 1st non-blank char
	JRZ	..DONE		;Quit when terminator reached
	CPI	'-'		;;Parameter lead-in?
	JRNZ	..DONE		;;No, quit
;;
	CALL	FCBCHR		;;Yes, get parameter
	LXI	H,PRMDEC	;;HL-> parameter table
	CALL	CHOICE		;;HL-> processing routine
	JC	PRMBRK		;;BREAK out if unrecognized parameter
	CALL	CALLHL		;;Process the parameter
	JMPR	..1		;;Go check for more
;
;	No further options.
;
..DONE:	JMP	BAKGET		;Put look-ahead char back into buffer
	EPROC	PRCPRM
;
;	Process -i init file.
;
	BPROC	PARMI
PARMI:	CALL	SETAUX		;Yes, setup AUXFCB with the command file name
	CALL	BAKGET		;Backup GET to the filename terminator
	CALL	OPNSP1		;;Try to open the file, 'C' if unable
	MVI	A,2		;;Successful open?
	JRNC	..1		;;Yes, branch
	INR	A		;;No, AL = file not found code
..1:	STA	INIFLG		;;Insert .INI-file found/not found code
	RET
	EPROC	PARMI
;
;	Skip over -s small model flag which has already been processed.
;
PARMS:	RET
;
;	Process -x execution file.
;
PARMX:	CALL	SETRX		;Put RXZxfile<TERMINATOR> into CMDBUF
	JMP	BAKGET
;
; SETRX - Transform -x xfile into 'RXZ xfile' in command buffer.  [10/07/85]
;	  Entry: ACTBUF contains invocation line
;		 CMDGET-> ACTBUF following "-X", CMDPUT-> past end of invocation
;
;	  NOTE: This routine screams for a block move from ACTBUF to (CMDPUT)
;
				;{OPNEXE}
	BPROC	SETRX
SETRX:	CALL	FCBCHX		;Get next nonblank char.  Terminator?
	RZ			;Yes, quit
;
	LHLD	CMDPUT
	PUSH	H
	LHLD	CMDGET
	PUSH	H
	PUSHA			;Save 1st filename char
	CALL	POPCMD		;Retrieve previous command buffer
	MVI	A,'R'
	CALL	INSCMD		;Insert 'R' into command buffer
	MVI	A,'X'
	CALL	INSCMD		;Insert 'X'
	MVI	A,'Z'
	CALL	INSCMD		;Insert 'Z'
	POPA			;RXZ now in command buffer, get 1st filename char
	CALL	INSCMD
	POP	H		;HL-> next filename char
;
..2:	MOV	A,M		;Get next filename char
	INX$	H
	CALL	CKEOFN		;Reached filespec terminator?
	JRZ	..3		;Yes, branch
	CALL	INSCMD		;No, insert char into command buffer
	JMPR	..2		;Continue
;
..3:	CALL	INSCMD		;Put terminator into command buffer
	XCHG			;DE-> rest of invocation command line
	POP	H		;HL-> past end of invocation cmd line
	JMP	SETCM2		;Push command buffer onto stack.  Reset
				;command ptrs to remaining VEDIT parameters
	EPROC	SETRX
;
; OPNEXE - Attempt to open .INI file.  Return 'C' if unable.	[12/8/86]
;	   Return result from previous attempt to open -i's file, if any;
;	   else open VEDIT.INI if INIFLG=1.
;
				;;{BEGIN}
	BPROC	OPNEXE
OPNEXE:	CPIB	INIFLG,2	;;-i's file already opened?
	RZ			;;Yes, return 'NC'
	CMC			;;-i's file not found?
	RC			;;Yes, return 'C'
	ORA	A		;;Use VEDIT.INI?
	STC			;;In case not
	RZ			;No, return 'C'
	LXI	H,VDTMSG	;HL-> "VEDIT.INI"
	JMP	OPNSPX		;Open VEDIT.INI, 'C' if unable
	EPROC	OPNEXE
;
; VISOPT - Process visual mode option.
;
				;{BEGIN}
VISOPT:	TST$	BGCMSW		;Begin in command mode?
	RNZ			;Yes, return
	MVI	A,'V'		;No, insert (append) visual-mode command
;	JMP	INSCMD		;into command buffer
;
; INSCMD - Insert (append) A into command buffer at CMDPUT.  Advance CMDPUT.
;
;	   (Special purpose for init routines.  Needs overflow conditions
;	   before being used generally.)
;
;	   HL, DE, BC preserved.
;
				;{VISOPT^,SETRX}
INSCMD:	PUSH	H
	LES	H,CMDPUT	;HL-> end of command buffer
	MOV%ES	M,A		;Insert command char
	INX$	H		;Bump PTR
	SHLD	CMDPUT		;Save PTR
	POP	H
	RET
;
; SETPRM - Save input parameter line in ACTBUF.  Set GET/PUT -> to it. (10/5/85)
;
				;{BEGIN}
SETPRM:	LXI	H,DEFDMA	;HL-> VEDIT parameter line
	MOV	C,M		;Get # chars in the line
	INX$	H		;HL-> start of the string
	LXI	D,ACTBUF	;DE-> destination
	PUSH	D		;Save
	CALL	MOVE		;Move parameter string into ACTBUF
	XCHG			;HL-> past last char moved
	MVI	M,CR		;Ensure filename terminator
	INX$	H		;HL = new PUT ptr
	POP	D		;DE = new GET ptr
	JMP	SETCM2		;Set new GET/PUT ptrs
;
; SIGNON - Display Signon Message.
;
;	There is an adverse side-effect that is interlinked with
;	MOVE? that is dependent on when this routine is initialized!
;
SIGNON:	LXI	H,VERSMS	;HL-> VEDIT version
	CALL	PCRMSG		;Print it
	LXI	H,INIMS1	;HL-> init message
	JMP	PCRMSG		;Print it
;
; DOSINI - Disk Operating System Dependent Initialization.
;
				;{BEGIN}
DOSINI:	CALL	GETENV		;Determine environment

;6	IF	CPM86
	CALL	SETIO		;Setup I/O vectors based on CP/M or MP/M
;6	CALL	HSCPI		;;Initialization for HSCPM/MMCPI
;6	ENDIF

	IF	POLLING, [
	CALL	SETPOL		;Turn off fast/slow polling (DOS/SETBYT)
	]
;
	JMP	SAVUSR
;
	IF	POLLING, [
;
; SETPOL - Turn off fast and/or slow polling depending on DOS & SETBYT
;
				;{DOSINI}
	BPROC	SETPOL
SETPOL:	CALL	MPMCHK		;Is it MP/M or CCP/M?
	LDA	SETBYT		;Get setup byte
	JRZ	..1		;No, CP/M;  branch
	RAR			;Yes, move MPM bits right two
	RAR
;
..1:	RAR			;Put into right most position
	RAR
;
	RAR			;Disable all console polling?
	JRNC	..2		;No, branch to allow polling
	MVIM%CS	CHKKEY,RETINS	;Yes, disable slow polling routine via RET code
	RAL			;Get bit back to also disable fast polling
;
..2:	RAR			;Disable fast polling?
	RNC			;No, return
	MVIM%CS	CHKKYB,RETINS	;Yes, disable fast polling routine
	RET
	EPROC	SETPOL
	]			;IF POLLING
;
;
; MSCINI - Miscellaneous initialization:  BUFTBL & Qvals.
;
				;{BEGIN}
MSCINI:	LXI	H,BUFTBL	;HL-> edit buffer/text register descriptor tables
	IFNOT	P8086, [
	LXI	B,REGTEN-BUFTBL
	] [
;6	MOV	CX,OFS REGTEN
;6	SUB	CX,BX		;CX = size of table
	]
	CALL	FILLZ		;Initialize BUFTBL
				;HL-> past end of table
	MVI	M,0FFH		;Set table fence marker
;
	LXI	H,VALREG	;Zero out numerical registers
	LXI	B,3*QRGMAX	;3-byte signed integers
	JMP	FILLZ
;
; TBLINI - Allocate space for POSTBL, SCRCNT and SCRFCB.
;	Note that these pointers DO NOT ADJUST during FREE()
;
				;{BEGIN}
TBLINI:	LD16	B,PYLINE
	MVI	B,00		;BC = number of physical lines
	CALL	MALLOC		;Allocate space
	STDS	SCRCNT,H	;Set SCRCNT
	CALL	MALLOC
	CALL	MALLOC		;Need PYLINE words, tricky huh
	STDS	POSTBL,H	;Set POSTBL
	CALL	MALLOC
	CALL	MALLOC		;Need PYLINE words
	STDS	SCRFCB,H	;Set SCRFCB
	SHLD	MAXMEM		;Pointers above MAXMEM do not adjust
;6	CALL	CB@INI		;Establish cmd buffer in table segment
;
;	Make KEYTBL into text register
;
	MVIB$	REGNUM,REGKEY	;The decode table is treated as a register
;6	MOV	ES,DSEG0	;
	LD%ES	D,KEYTEN	;ES:DE-> past end of table
	LXI	H,KEYTBL	;HL-> begin of table
	CALL	MAKCPY		;Create the text register
	IFNOT	WORDP, [
	JMP	MACRST		;Set pointers to KEYTBL, reset KEYPTR
	] [
	CALL	MACRST
;
;	Make print headers into text registers
;
	MVIB$	REGNUM,REGHED	;Set register # for header
	LXI	H,PXHEAD	;HL-> begin of table
	LXI	D,PXSUBH	;DE-> past end of table
	CALL	MAKCPY		;Create the text register
	MVIB$	REGNUM,REGSHD	;Set register # for sub-header
	LXI	H,PXSUBH	;HL-> begin of table
	LXI	D,PXFOOT	;DE-> past end of table
	CALL	MAKCPY		;Create the text register
	MVIB$	REGNUM,REGFOT	;Set register # for footer
	LXI	H,PXFOOT	;HL-> begin of table
	LXI	D,PXHEND	;DE-> past end of table
	JMP	MAKCPY		;Create the text register
	]

	.PAGE
				;{100H}
	BPROC	BEGIN
BEGIN:	XTSTAK			;Setup temporary stack pointer
	CALL	INIT0		;Things that MUST be initialized immediately
	CALL	DOSINI		;Perform DOS dependent ini

;6	IF	MEMVRS
;6	CALL	MMINIT
;6	ENDIF
	CALL	VSTART		;Setup hardware (sets PYLINE)
				;CRT - enable status line, keypad mode
	CALL	MSCINI		;Init BUFTBL & Qvals
	CALL	MALLC0		;Initial memory setup
	CALL	TBLINI		;Allocate space for POSTBL, SCRCNT and SCRFCB.
				;Make KEYTBL & print headers into text registers
;
;	Setup main edit buffer.  Cannot be done until KEYTBL and
;	print headers are moved to high memory.
;	This requires that EDTNUM = 0FFH, REGNUM = '@',
;	TXTRWF = (WSABEG) -1, REGPTR-> BUFTBL for entry '@'.
;
	MVI	A,MAINRG	;Value for '@' buffer
	CALL	GETREG		;Make sure REGPTR set correctly for MAKEBF()
	CALL	MAKBF0		;Create '@' edit buffer
;
;	Initialize screen driver and put up sign-on message.
;
	CALL	RSTCON		;Set to internal console output
	CALL	WININI		;Init window parameters, clear screen
;
STACK:				;This is the top of operating stack
	XOSTAK			;Set operation stack NOW
;
	CALL	SIGNON		;Display sign-on message
	CALL	PRCPRM		;;Process invocation parameter line
	CALL	OPNEXE		;Open -i/.INI file
	PUSHF			;Save results of open attempt
	JRC	..2		;Skip if no .INI file
	CALL	LODEXE		;Load .INI or -i file into RZ
	MVIB$	INXFLG,0FFH	;Ensure RZ executed even if bad edit fname
..2:
	IF	P8086, [
	CALL	SETABT		;Set ENBRFL for CP/M-86 & set BREAK trap for MSDOS
	]

	CALL	EBCMD0		;Open I/O files for editing
	CALL	POPCMD		;Reset GET/PUT to CMDBUF
	CALL	VISOPT		;Process visual-mode-entry option

	POPF			;Has .INI or -i file been opened?
	CNC	MCMD1		;Yes, set GET/PUT -> RZ
	CLR$	INXFLG		;RZ will now be executed, clear flag

	JMP	TOCMD		;Begin editing
	EPROC	BEGIN

	.PAGE
;
;	Init file variables edit buffer pointers and text markers.
;
				;{EZCMD,MAKEBF,RESTRT}
	BPROC	INITXB
INITXB:	LHLD	EDTBAS		;-> main edit buffer (header)
	LXI	D,EHDLEN	;Header length for edit buffer with files
	DAD	D		;-> main edit buffer proper
	SHLD	TXTBAS		;-> text not yet buffered to disk
	SHLD	TXTFLR		;-> start of text window (floor)
	SHLD	TXTCEL		;
	SHLD	TXTRWF		;-> roof of edit buffer (-> EOF)
	SHLD	EDTPTR		;-> command edit position
;
	MVI	M,EOF		;Init buffer to empty
	DCX$	H		;HL-> before buffer
	MVI	M,LF		;Make sure we have a LF
;
;	Init Text markers to 0000 => HOME
;
	LXI	H,PNTTBL	;Point to table
	MVI	B,PNTMAX	;Number of entries
..2:	CALL	ZER%HL		;Value of 0000 will set to HOME
	DJNZ	..2		;Loop for all markers
;	JMPR	FILINI
	EPROC	INITXB

;
; FILINI - Initialize all file FCB flags and other file variables.
;
				;{INITXB^,OPNIOX}
FILINI:	LXI	H,FILVLS	;HL-> table of initial file values
	CALL	SETVLS		;Init the variables
;
				;{FILINI^,CLOSER,PRVSEC}
CNTINI:	MVIW$	CNTOUT,0000	;Clear counter for # lines written out
	RET

;
; SETVLS - Set byte and word values from the tables <- by HL.
;
;	   The byte table comes first followed by the word table.
;	   A table entry consists of a memory address followed by the
;	   byte or word datum.
;
;	   A table is terminated when the purpotive memory address = 0.
;
				;{INIT0,FILINI}
	BPROC	SETVLS
SETVLS:	MVI	B,1		;Flag for byte values (word values = 0)
..1:	CALL	MVINDE		;DE = next address to be initialized, HL++
	MOV	A,D
	ORA	E		;End of table?
	BEQ	..2		;Yes, skip
;
	MOV	A,M		;No, get byte datum or LSB of word datum
	INX$	H		;Advance table pointer
	STAX	D		;Set datum
	DCR	B		;Setting byte values?
	BEQ	SETVLS		;Yes, repeat
;
	INR	B		;No, word values, reset B = 0
	INX$	D		;DE-> MSB to be set
	MOV	A,M		;Get MSB
	INX$	H		;Advance table pointer
	STAX	D		;Set MSB
	JMPR	..1		;Repeat setting word values
;
..2:	DCR	B		;Were we setting byte values?
	BEQ	..1		;Yes, now set word values, B = 0
	RET			;No, all done, return
	EPROC	SETVLS
;
; LODEXE - Automatically load AUXFCB's file into Register Z. (10/5/85)
;
				;{BEGIN,BR8K}
LODEXE:	MVI	A,'Z'
	CALL	SETRN		;Select RZ
	IFNOT	P8086, [
	JMP	RLCMD0		;Read the file into RZ
	] [
	CALL	RLCMD0		;Read the file into RZ
	JMP	RSTSEG
	]
