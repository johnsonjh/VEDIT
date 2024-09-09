	.IDENT	VEDSET
;****************************************
;*					*
;*		V E D S E T		*
;*					*
;****************************************
;
;
;	Last Change: Ted - Aug. 30, 1985
;
; Copyright (C) 1979, 1985 by CompuView Products, Inc.
;
;	CompuView Products, Inc.
;	1955 Pauline Blvd.
;	Ann Arbor, MI 48103
;
; TDL ASSEMBLER DIRECTIVES.
;
	.PABS
	.PHEX
	.LADDR
	.I8080
	.OPSYN	.LOC,ORG
	.OPSYN	.IFN,IF
	.OPSYN	.IFE,IFNOT
	.OPSYN	.BLKB,DS
	.OPSYN	.BLKW,DSW
	.OPSYN	.ASCII,DC
	.OPSYN	.ASCIS,DCS
	.OPSYN	.BYTE,DB
	.OPSYN	.WORD,DW
	.SALL				;Don't expand the macros
	.DEFINE	PUSHA=[PUSH	PSW]
	.DEFINE	POPA=[POP	PSW]
	.DEFINE	PUSHF=[PUSH	PSW]
	.DEFINE	POPF=[POP	PSW]
;
EXIT	=	BASE
BDOS	=	BASE + 5
DEFFCB	=	BASE + 005CH
DEFDMA	=	BASE + 0080H
LF	=	0AH
CR	=	0DH
SPACE	=	020H
TBLLEN	=	115		;CRT table length
CHKOFF	=	8		;Offset from start of vedit to checksum word
KEYCOL	=	35		;Column to tab over to for keyboard entry
;
;
; BDOS CALLS
;
WLIST	=	5
CONSIO	=	6
PRINT	=	9
READLN	=	10
GETVER	=	12
OPEN	=	15
CLOSE	=	16
DELETE	=	19
READ	=	20
WRITE	=	21
CREATE	=	22
RENAME	=	23
SETDMA	=	26
	.PAGE
;
VPLUS	= \ 'VEDIT (0) or VEDIT PLUS (1) ? '
VERTYP	= \ 'MEMORY MAPPED (1), CRT (2) OR PIICEON-TDL (3) ? '
DECRAIN	= \ 'DEC RAINBOW (0=NO) (1=YES) ? '
SPCVER	= \ 'NORMAL AT 0100H (1), RELOCATED AT 04200H (2) OTHER (3) ? '
	.IFE	SPCVER -3, [
BASE	= \ 'CPM BASE ADDRESS IN HEX (H) ? '
	]
TAB138	= \ 'DECODE TABLES (0=OLD SINGLE TABLE) (1= NEW DOUBLE TABLES) '
LSMAIN	= \ 'LISTING OF PROGRAM CODE ? (0=NO) (1=YES) '
LSKEY	= \ 'LISTING OF KEYBOARD DECODE CODE ? (0=NO) (1=YES) '
LSMSG	= \ 'LISTING OF MESSAGES ? (0=NO) (1=YES) '
;
;	Setup simple variables for Version type.
;
MEMVRS	=	0
CRTVRS	=	0
PICVRS	=	0
P8086	=	0
;
	.IFE	VERTYP -1, [
MEMVRS	=	1		;Make MEMVRS True for IF condition.
	]
	.IFE	VERTYP -2, [
CRTVRS	=	1
	]
	.IFE	VERTYP -3, [
PICVRS	=	1
	]
;
	.IFE	SPCVER -1, [
BASE	=	0000H
	]
	.IFE	SPCVER -2, [
BASE	=	04200H
	]
	IF	MEMVRS, [
SYNCHB	=	118		;Synchronization byte.
	]
	IF	CRTVRS, [
SYNCHB	=	140		;Synchronization byte.
	]
	IF	PICVRS, [
SYNCHB	=	170		;Synchronization byte.
	]
;
;	Format of VEDIT for Setup.
;
; 1)	The third byte of VEDIT contains the address of the
;	table ADDTBL, which begins with two words:
;
;	SYNCRO			Synchronization word.
;	MAXLIN			Max number of screen lines.
;
;	These are followed by the addresses of all other
;	tables modified by VEDSET, namely:
;
;	ADDLED			Begin of CRT escape sequences.
;	ESCTAB			Escape sequence table.
;	CNTBL1			Index table for Bit 7 control keys.
;	CNTBL2			Index table for control keys.
;	CNTADD			Table of control routine addresses.
;	TABPOS			The Tab table.
;	CHRTBL			Table of various characters.
;	STATCH			Table of status line characters.
;	SWTBL			The switch table.
;	PRMTBL			The parameter table.
;	ISPARE			Two words dependent upon memory size.
;	NLINE			The first of the screen parameters.
;
; 2)	CNTBL1 is 100H bytes long for characters with the top
;	bit set.  This is followed by ESCTAB which specifies
;	whether escape sequences end in CR.
;	This is followed by CNTBL2 which is 40H bytes long for normal
;	control characters.  This is followed by the routine address
;	used for the RUBOUT key.
;
; 3)	CNTADD is a table of addresses of all the visual
;	function routines.  These addresses are put in 
;	the appropriate place in the CNTBL1 and CNTBL2 tables.
;
;
	.PAGE
	ORG	BASE + 0100H
	JMP	BEGIN		;Jump around version message.
	JMP	BEGIN2		;Restart at control table initialization.
PRLOFF:	DW	0000		;Offset is 0100H for .PRL file.
	DW	MPMTBL		;Address of MPM routines.
;
SETBYT:	DB	00000001B	;Set for auto determination of CPM or MPM.
;
;	Signon Message
;
VERMSG:	
;
	IFNOT	VPLUS, [
	IF	MEMVRS, [
	DC	'     VEDSET  -  Version 1.18 '
	]
	IF	CRTVRS, [
	DC	'   VDSETCRT  -  Version 1.40 '
	]
	IF	PICVRS, [
	DC	'   VDSETPIC  -  Version 1.80 '
	]
	]
;
	IF	VPLUS, [
	IF	MEMVRS, [
	DC	'     VEDSET  -  Version 2.02 '
	]
	IF	CRTVRS, [
	DC	'   VDSETCRT  -  Version 2.32 '
	]
	IF	PICVRS, [
	DC	'   VDSETPIC  -  Version 2.32 '
	]
	]
;
	DCS	[CR] [LF]
;
;
CONBUF:	DS	16		;Console polling input buffer
;
BEGIN:	LXI	SP,STACK	;Set stack pointer.
	MVI	A,1		;Set output to the console
	STA	OUTFLG		;not the printer
	XRA	A		;Clear GET and PUT pointers
	STA	GET
	STA	PUT
;
;	Determine if running MPM or CPM and whether fast, slow or
;	no interrupt simulation.
;
	LXI	H,SETBYT	;HL-> the version setup byte.
	MOV	C,M		;Get the version setup byte.
	MOV	A,C		;Get byte into A too.
	ANI	02		;Look for MPM or CPM bit.
	MOV	H,A		;Save in H in case not auto.
	MVI	L,30H		;Assume MPM II if MPM
	MOV	A,C		;Get SETBYT again.
	ANI	01		;Set for auto determination of CPM and MPM?
	JZ	BEGIN1		;No, branch.
	MVI	C,GETVER	;Yes, get CPM version.
	CALL	BDOS
BEGIN1:				;H <> 0 - MPM, H=0 - CPM
				;L=30 - MPM II
	IFNOT	P8086, [
	MOV	A,L
	CPI	30H		;Is it MPM II?
	JC	BEGIN7		;No, branch
	MVI	A,0FDH		;MPM II Console Input value
	STA	MPMIN+3		;Save as arg for BDOS #6
	]
BEGIN7:	MOV	A,H		;A=1 - MPM, A=0 - CPM.
	CALL	SETIO		;Setup I/O vectors based on CPM or MPM.
;
	LXI	B,VERMSG	;BC-> version message.
	CALL	PRTMSG		;Print it.
	CALL	SETUP		;Set up File Control Blocks.
	CALL	SRCOPN		;Open the source file.
	CALL	CRENEW		;Create the new .$$$ file.
	CALL	EDREAD		;Read in the editor.
;
	LHLD	PRLOFF		;Get PRL file offset.
	LXI	D,TXTBUF+CHKOFF	;DE-> normal place for checksum.
	DAD	D		;HL-> checksum including PRL offset.
;
	MOV	E,M		;Get old checksum.
	INX	H
	MOV	D,M		;DE = old checksum.
	PUSH	D		;Save it.
	MVI	M,00		;Clear checksum.
	DCX	H
	MVI	M,00
;
	CALL	CHKSUM		;DE=editor checksum
	POP	H		;HL = existing checksum.
	DAD	D		;Add 2's complement.
	MOV	A,H		;It should be zero
	ORA	L
	JZ	BEGIN2		;Check sum ok ...
	LXI	B,CHKERR	;Bad, output warning
	CALL	PRTMSG
	LXI	H,DEFDMA	;Wait for a return
	MVI	M,10		;max input length
	XCHG
	MVI	C,READLN
	CALL	BDOS
;
BEGIN2:	CALL	RELOCT		;Compute relocated table addresses.
	IF	CRTVRS, [
	CALL	SETCRT		;Let user select CRT from menu.
	]
;
;	Select from main menu.
;
BEGIN3:	XRA	A		;Flag control-C is abort
	STA	CTRLCF		;Save the flag
	INR	A		;Get a 1
	STA	OUTFLG		;Set console output

	LXI	B,INIMSG	;BC-> keyboard layout option message.
	CALL	PRDCCH		;Print mess., get user's value.
	JZ	BEGIN3		;Repeat if Rubout.
	MOV	A,L		;Get user's value.
	CPI	1		;All new layout?
	JZ	BEGIN4		;Branch to init decode tables.
	CPI	2		;Add to layout?
	JZ	BEGIN5		;Yes ...
	CPI	3		;Change special characters?
	JNZ	NOTSCH		;Nope ...
	CALL	SETCHT
	JMP	BEGIN3

NOTSCH:	CPI	4		;Set ES, EP?
	JNZ	NOTESP
	CALL	SETSWT		;DO ES
	CALL	SETPRM		;DO EP
	JMP	BEGIN3

NOTESP:	CPI	5		;Screen parameters?
	JNZ	NOTSCR		;Nope ...
	CALL	SETSCR
	JMP	BEGIN3

NOTSCR:	CPI	6		;Set memory,  tabs, status line
	JNZ	NOTMEM
	CALL	SETMEM
	CALL	SETTAB
	IF	CRTVRS,[
	CALL	SETDEL		;Set Processor Speed for CRTs.
	]
	CALL	SETINS		;Set INSERT flag.
	CALL	SETINI		;Set VEDIT.INI and VHELP search
	CALL	SETSTT
	JMP	BEGIN3

NOTMEM:	CPI	7
	JNZ	NOTMSG
	CALL	SETSIG		;Set signon message.
	JMP	BEGIN3

NOTMSG:	CPI	8		;Dump the keyboard layout?
	JZ	DMPKEY
	CPI	9		;All done?
	JZ	BEGIN8		;Yes ...
	CALL	PRTERR		;Else give bad number error.
	JMP	BEGIN3		;Repeat prompt.
;
BEGIN4:	CALL	KEYINI		;Initialize the keyboard decode tables.
BEGIN5:	CALL	SETKEY		;Set a new keyboard layout.
	JMP	BEGIN3		;Get next option
;
; All done, write out VEDIT and exit
;
BEGIN8:	LHLD	PRLOFF		;Get PRL file offset.
	LXI	D,TXTBUF+CHKOFF	;DE-> normal place for checksum.
	DAD	D		;HL-> checksum including PRL offset.
	PUSH	H		;Save checksum address.
	MVI	M,0		;Get old checksum.
	INX	H
	MVI	M,0
;
	CALL	CHKSUM		;Compute checksum.
	XCHG			;HL = checksum.
	MOV	A,L		;Must 2's complement it first
	CMA
	MOV	L,A
	MOV	A,H
	CMA
	MOV	H,A
	INX	H		;HL = complement.
	XCHG			;DE = complement.
;
	POP	H		;HL-> ckecksum area.
	MOV	M,E		;Save new checksum.
	INX	H
	MOV	M,D
;
	CALL	EDWRIT		;Write out the editor.
	CALL	CLOSER		;Close the files.
	JMP	EXIT		;Return to CP/M.
;
; SETIO - Set up internal copy of Console I/O vectors.
;	  If A = 0, use BIOS, else use MPM routines.
;
SETIO:	ORA	A		;Running with MPM?
	LXI	H,MPMTBL	;For MPM use MPM I/O routines.
	JNZ	SETCP1		;Yes, merge in below.
;	
	LHLD	BASE + 0001	;HL-> BIOS.
	INX	H
	INX	H
	INX	H		;HL-> BIOS Console Status.
SETCP1:	LXI	D,CONST		;DE-> Internal I/O jump vectors.
	MVI	C,9		;Count for 3 vectors.
	JMP	MOVE		;Set I/O vectors and return.
;
;
;
EXITRT:	LXI	D,OUTFCB	;DE-> Output FCB
	MVI	C,DELETE	;Delete VEDIT.$$$ file
	CALL	BDOS
	JMP	EXIT
	.PAGE
;
; SETUP - Set up FCBs.
;
SETUP:	LDA	DEFFCB+1	;Was .SET file specified?
	CPI	' '		;Check if blank.
	LXI	D,NONAME	;Assume No.
	JZ	ERROR		;No, give error.
	LXI	H,DEFFCB	;HL-> input file name.
	LXI	D,INFCB		;DE-> input FCB.
	MVI	C,9		;Get size of drive and name.
	CALL	MOVE		;Move name to Input FCB.
	LXI	H,DEFFCB	;HL-> input file name.
;
;	Use any file extension or let it default to ".SET".
;
	LDA	DEFFCB+9	;Get input file extension.
	CPI	' '		;Was it specified?
	JZ	SETUP1		;No, use defa}lt ".SET".
	LXI	H,DEFFCB+9	;HL-> input file extension.
	LXI	D,INFCB+9	;DE-> input FCB.
	MVI	C,3		;Get size of extension.
	CALL	MOVE		;Move .ext to Input FCB.
;
SETUP1:	LDA	DEFFCB+17	;Look at second file name.
	CPI	' '		;Was output name specified?
	JZ	SETUP2		;No, branch.
	LXI	H,DEFFCB+16	;Yes, HL-> output file name.
SETUP2:	LXI	D,OUTFCB	;DE-> Output FCB.
	MVI	C,9		;Get size of drive and name.
	JMP	MOVE		;Move name to Output FCB, return.
;
; Create the output file
;
CRENEW:	LXI	D,OUTFCB	;DE-> output FCB.
	MVI	C,DELETE	;Delete any old file by name.
	CALL	BDOS
	LXI	D,OUTFCB	;DE-> output FCB again.
	MVI	C,CREATE	;Create new file.
	CALL	BDOS
	CPI	255		;Create error?
	RNZ			;No, return.
	LXI	D,MAKERR	;DE-> error message.
	JMP	ERROR		;Print error, return to CP/M.
;
; SRCOPN - Open the source file.
;
SRCOPN:	XRA	A		;Get a zero.
	STA	INFCB+32	;Set NR to 0.
	STA	INFCB+12	;Set extent to 0.
	LXI	D,INFCB		;DE-> Input FCB.
	MVI	C,OPEN
	CALL	BDOS		;Open the source file.
	CPI	255		;Open error?
	RNZ			;No, return.
;
; ERROR - Input file not found.
;
	LXI	D,NOFILE	;DE-> error message.
	JMP	ERROR
;
; EDREAD - Read in the object of the editor.
;
EDREAD:	LXI	D,TXTBUF	;DE = initial DMA address.
	LXI	B,00		;Clear sector counter.
;
; READSC - Read Input file until EOF. DE = DMA address.
;	   Return: BC = length of file in sectors.
;
READSC:	PUSH	B		;Save sector count.
	PUSH	D		;Save DMA address.
	MVI	C,SETDMA	;Set DMA address.
	CALL	BDOS
	CALL	DISKR		;Read one sector.
	POP	H		;Get back old DMA address.
	DCR	A		;EOF found?
	JZ	RDEOF		;Yes, branch.
	LXI	B,128		;No, get sector size.
	DAD	B		;Compute new DMA.
	XCHG			;DE = new DMA address.
	POP	B		;Get sector count back.
	INX	B		;Account for sector read.
	JMP	READSC		;Continue until EOF found.
;
RDEOF:	POP	H		;Get count off stack.
	SHLD	FILSIZ		;Save it.
;
	LXI	D,DEFDMA	;DE-> default DMA address.
	MVI	C,SETDMA	;Change DMA address to prevent problems.
	JMP	BDOS		;Set DMA and return.
	.PAGE
;
; EDWRIT - Write the modified editor out to disk.
;
EDWRIT:	LXI	D,TXTBUF	;Initial DMA address.
	LHLD	FILSIZ		;Get size of file.
	MOV	B,H		;Put size in BC.
	MOV	C,L
;
; WRTFIL - Write number of sectors specified in BC.
;	   DE = DMA address.
;
WRTFIL:	PUSH	B		;Save remaining sector count.
	PUSH	D		;Save DMA address.
	MVI	C,SETDMA
	CALL	BDOS		;Set DMA address.
	CALL	DISKW		;Write a sector.
	POP	H		;HL = DMA address.
	LXI	B,128		;Size of sector.
	DAD	B		;Compute new DMA address.
	XCHG			;DE = new DMA address.
	POP	B		;Get back remaining sector count.
	DCX	B		;Decrement sector count.
	MOV	A,B		;Count exhausted?
	ORA	C
	JNZ	WRTFIL		;No, continue.
	STC			;Set C bit.
	RET			;Return with 'C'.
;
; CLOSER - Close and Rename files.
;
CLOSER:	LXI	D,OUTFCB	;DE-> Output.$$$ FCB.
	MVI	C,CLOSE
	CALL	BDOS		;Close the Output file.
	CPI	255		;Close error?
	LXI	D,CLSERR	;DE -> error message.
	JZ	ERROR		;Yes, abort program.
;
; Now use OUTFCB as rename FCB.
;
	LXI	H,OUTFCB	;HL -> original file FCB.
	LXI	D,OUTFC2
	MVI	C,9		;Length of drive and name.
	CALL	MOVE		;Move original name to Rename FCB.
	LXI	H,COMMSG	;HL-> "COM".
	MVI	C,3		;Length of new file type.
	CALL	MOVE		;Finish making rename FCB.
;
; Delete any existing VEDIT.COM file.
;
	LXI	D,OUTFC2	;DE-> VEDIT.COM name.
	MVI	C,DELETE
	CALL	BDOS		;Delete any existing file.
;
	LXI	D,OUTFCB	;DE -> Rename FCB.
	MVI	C,RENAME
	JMP	BDOS		;Rename .$$$ to .COM.
;
; DISKR - Read one sector from disk.
;
DISKR:	LXI	D,INFCB		;DE -> input FCB
	MVI	C,READ
	CALL	BDOS		;Read a sector.
	CPI	2		;Read error?
	RNZ			;No, return.
RDERR:	LXI	D,RMSG		;Yes, DE -> error message.
	JMP	ERROR		;Branch to print error, return to CP/M.
;
; DISKW - Write one sector to the disk.
;
DISKW:	LXI	D,OUTFCB	;DE -> output FCB.
	MVI	C,WRITE
	CALL	BDOS		;Write a sector.
	ORA	A		;Any error?
	RZ			;No, return.
	LXI	D,WMSG		;DE -> error message.
				;Print error, return to CP/M.
;
; Error handlers.
;
ERROR:	MVI	C,PRINT
	CALL	BDOS		;Print message.
	JMP	EXITRT		;Back to CP/M.
	.PAGE
;
; RELOCT - Get each address out of the VEDIT.SET address table,
;	   compute where the desired table is during this setup,
;	   and save this relocated table address.
;
RELOCT:	LHLD	PRLOFF		;Get any .PRL file offset.
	MOV	B,H		;Save in BC.
	MOV	C,L
	LXI	H,TXTBUF+6	;HL-> normal pointer to address table.
	DAD	B		;Unless a PRL offset needs to be added.
	CALL	MVINHL		;HL-> begin of address table in VEDIT.
	XCHG			;Save in DE.
	LXI	H,TXTBUF-(EXIT+100H) ;Get setup offset to table.
	DAD	B		;Add in additional offset for .PRL.
	MOV	B,H		;Save in BC for computations below.
	MOV	C,L
	DAD	D		;Add in offsets needed for setup and .PRL.
	XCHG			;DE-> synchronization word.
;
;	Check that the synchronization bytes agree.
;
	CALL	MVTBHL		;HL = synchronization word. (DE++)
	PUSH	D
	LXI	D,SYNCHB	;Get the synch byte for VEDSET.
	CALL	CMHLDE		;Running correct VEDSET and VEDIT?
	POP	D		;Restore DE
	JNZ	BADSYN		;No, abort.
;
	CALL	MVTBHL		;HL = max screen lines in VEDIT.
	SHLD	MAXLIN		;Save max screen lines.
;
;	Now get the relocated address of all the tables.
;
	CALL	MVTBHL		;HL = address of control table
	DAD	B		;Add setup offset.
	SHLD	CNTADD		;Save relocated control table address.
;
	IF	CRTVRS, [
	CALL	MVTBHL		;HL = address of CRT escape sequence table.
	DAD	B		;Add setup offset.
	SHLD	ADDLED		;Save relocated address of table.
	CALL	MVTBHL		;HL = address of ESC - CR table # 1
	DAD	B		;Add setup offset.
	SHLD	ESCTB1		;Save relocated address of table.
	]
;
	CALL	MVTBHL		;HL = address of ESCDC1.
	DAD	B		;Add setup offset.
	SHLD	ESCDC1		;Save relocated address of table.
	CALL	MVTBHL		;HL = address of CTLDEC.
	DAD	B		;Add setup offset.
	SHLD	CTLDEC		;Save relocated address of table.
;
	CALL	MVTBHL		;HL = tab table address.
	DAD	B		;Add setup offset.
	SHLD	TABPOS		;Save relocated tab table address.
	CALL	MVTBHL		;HL = address of character table.
	DAD	B		;Add setup offset.
	SHLD	CHRTBL		;Save relocated character table address.
	CALL	MVTBHL		;HL = address of status table.
	DAD	B		;Add setup offset.
	SHLD	STATCH		;Save relocated status char. table address.
	CALL	MVTBHL		;HL = switch table address.
	DAD	B		;Add setup offset.
	SHLD	SWTBL		;Save relocated switch table address.
	CALL	MVTBHL		;HL = switch table address.
	DAD	B		;Add setup offset.
	SHLD	PRMTBL		;Save relocated parameter table address.
	CALL	MVTBHL		;HL = address of memory param.
	DAD	B		;Add setup offset.
	SHLD	ISPARE		;Save relocated memory param. address.
	CALL	MVTBHL		;HL = screen param. address.
	DAD	B		;Add setup offset.
	SHLD	NLINE		;Save relocated screen parameter address.
;
	IF	MEMVRS, [
	CALL	MVTBHL		;HL = screen init code address.
	DAD	B		;Add setup offset.
	SHLD	SCRINI		;Save relocated screen init code address.
	]

	CALL	MVTBHL		;HL = user message address
	DAD	B		;Add setup offset.
	SHLD	USRMSG		;Save relocated user message address
;
	IF	TAB138, [
	IF	CRTVRS, [
	CALL	MVTBHL		;HL = ESC - CR table # 2 address
	DAD	B		;Add setup offset.
	SHLD	ESCTB2		;Save relocated address of table
	]
	CALL	MVTBHL		;HL = ESC decode table # 2 address
	DAD	B		;Add setup offset.
	SHLD	ESCDC2		;Save relocated address of table
	]			;IF TAB138
	RET			;Return to main.
;
BADSYN:	LXI	D,BDSYMS	;DE-> Bad Synchronization message.
	JMP	ERROR		;Print message, abort.
	.PAGE
;
; SETCRT - Let user select CRT from menu.  Move corresponding
;	   CRT table to VEDIT.SET.
;
	IF	CRTVRS, [
SETCR0:	CALL	PRTERR		;Print error.
;
SETCRT:	LXI	D,TBLFCB	;Open CRT table file
	MVI	C,OPEN
	CALL	BDOS
	INR	A		;Is it there?
	JZ	SETCR2		;Nope - Error

SETCR1:	LXI	B,CRTMSG	;BC-> CRT menu.
	CALL	PRTMSG		;Print the menu, part one.
	CALL	CONIN		;Wait for user's key.
	LXI	B,CRTMS1	;BC-> CRT menu, part two.
	CALL	PRTMSG		;Print it.
	CALL	DECCHK		;Get the reply.
	JZ	SETCR1		;Repeat if Rubout.
	MOV	A,L		;Get the users value.
	ORA	A		;Is it zero?
	JZ	SETCR0		;Yes, print error
	CPI	NUMCRT+1	;Is value valid?
	JNC	SETCR0		;No, print error.
;
; Read in sector %A - 1 from crt table file
;
	DCR	A		;Set record number
	STA	TBLFNR
	LXI	D,TBLBUF	;Where to read it into
	MVI	C,SETDMA
	CALL	BDOS
	LXI	D,TBLFCB	;Read it
	MVI	C,READ
	CALL	BDOS
	ORA	A		;Was record there?
	JNZ	SETCR2		;Nope - Error

	LHLD	ADDLED		;HL-> VEDIT's CRT table.
	XCHG			;DE = destination of CRT table.
	LXI	H,TBLBUF	;HL-> CRT table
	LXI	B,TBLLEN	;Get size of single CRT table.
	JMP	MOVEBC		;Move from master table to VEDIT table.
;
; Here if CRT.TBL missing, or bad format
;
SETCR2:	LXI	D,TBLMSG	;Output error message
	JMP	ERROR
	]
;
; KEYINI - Perform any required initialization of keyboard
;	   decode tables.
;
KEYINI:
	IF	CRTVRS, [
	LHLD	ESCTB1		;HL-> ESC - CR entry table
	LXI	D,00		;Fill with zero
;
	IF	TAB138, [
	LXI	B,256		;Table is 256 bytes words long
	]
	IFNOT	TAB138, [
	LXI	B,128		;Table is 128 bytes words long
	]
;
	CALL	FILLTB		;Clear the table
	]
;
	LHLD	CNTADD		;HL-> address of NOOP routine.
	CALL	MVINDE		;DE = address of NOOP routine.
	LHLD	ESCDC1		;HL-> first table.
;
	IF	TAB138,[
	LXI	B,0200H		;Get size of two large tables
	]
	IFNOT	TAB138,[
	LXI	B,0100H		;Get size of large table.
	]
;
	CALL	FILLTB		;Fill the large table with routine address.
;
	LHLD	CNTADD		;HL-> address of NOOP routine.
	CALL	MVINDE		;DE = address of NOOP routine.
	LHLD	CTLDEC		;HL-> second table.
	LXI	B,0042H		;Get size of second table (RUBOUT too).
	CALL	FILLTB		;Fill the small table with NOOP address.
;
;	Enter routine address for CR key into table.
;
KEYIN2:	LHLD	CNTADD		;HL-> control addresses.
	INX	H		;Skip over NOOP address.
	INX	H
	XCHG			;DE-> address of CR routine.
	LHLD	CTLDEC		;Get address of <CTRL> decode table
	LXI	B,01AH		;Get offset to CR entry.
	DAD	B		;HL-> CR entry.
	JMP	MVDETB		;Put address into table, RETURN with 'NC'.
;
;	Fill the (HL) table with address of routine in DE.
;
FILLTB:	MOV	A,B		;Is count zero?
	ORA	C
	RZ			;Yes, return.
	MOV	M,E		;Put in low byte.
	INX	H		;Point to high byte.
	MOV	M,D		;Put in high byte.
	INX	H		;Point to next word.
	DCX	B		;Decrement count.
	DCX	B
	JMP	FILLTB		;Continue
;
	.XLIST
	IF	LSKEY, [
	.LIST
	]
	.INSERT SETKEY.ASM
;
	.XLIST
	IF	LSMAIN, [
	.LIST
	]
	.PAGE
;
;	Setup the Special Characters
;
SETCHT:	LHLD	CHRTBL		;HL = address of character table.
	INX	H		;Skip over Escape Char # 1 already set.
	INX	H		;Skip over Escape Char # 2 already set.
	INX	H		;Skip over Escape NOOP # 1 already set.
	INX	H		;Skip over Escape NOOP # 2 already set.
	XCHG			;DE-> continuation char. in table.
	LXI	B,CHTMSG	;BC-> first message.
;
SETCH1:	CALL	PRTMSG		;Print the next message.
	RZ			;Return if all done
	CALL	HEXCHK		;Get next number.
	JZ	SETCHT		;Start over if Rubout.
	CALL	MVLPAR		;Save in table.
	JMP	SETCH1		;Continue.
	.PAGE
;
;	Setup the ES defaults.
;
SETSWT:	LHLD	SWTBL		;HL = address of switch table.
	XCHG			;DE-> first switch in table.
	LXI	B,SWTMSG	;BC-> first message.
;
SETSW1:	CALL	PRTMSG		;Print the next message.
	RZ			;Return if done
	CALL	DECCHK		;Get next number.
	JZ	SETSWT		;Start over if Rubout.
	CALL	MVLPAR		;Save in table.
	JMP	SETSW1		;Continue.
;
;	Setup the EP defaults.
;
SETPRM:	LHLD	PRMTBL		;HL = address of parameter table.
	XCHG			;DE-> first parameter in table.
	
;
	IFNOT	MEMVRS, [
	INX	D		;Skip over cursor type.
	INX	D		;Skip over cursor speed.
	]
	LXI	B,PRMMSG	;BC-> first message.
;
SETPR1:	CALL	PRTMSG		;Print the next message.
	RZ			;Return if done
	CALL	DECCHK		;Get next number.
	JZ	SETPRM		;Start over if Rubout.
	CALL	MVLPAR		;Save in table.
	JMP	SETPR1		;Continue.
	.PAGE
;
;	Setup the Screen Parameters.
;
SETSCR:	LHLD	NLINE		;HL-> first screen parameter.
	XCHG			;DE-> first screen parameter.
	JMP	SETSC2		;Skip around error print.
;
SETSC1:	CALL	PRTERR		;Print error message.
SETSC2:	LXI	B,LINMSG	;BC-> # lines prompt.
	CALL	PRDCCH		;Print mess. get user's value.
	JZ	SETSC2		;Back if Rubout.
	PUSH	D		;Save DE.
	LXI	D,6		;At least 6 lines.
	PUSH	H		;Save user's value.
	LHLD	MAXLIN		;Get line size of VEDIT.
	MOV	B,H		;Put in BC.
	MOV	C,L
	POP	H		;Restore user's value.
	CALL	RNGCHK		;Check for valid range.
	POP	D		;DE-> parameter table.
	JC	SETSC1		;Branch if bad value.
	SHLD	NLINES		;Save for own use later.
	PUSH	H		;Save on stack too.
	CALL	MVLPAR		;Save in table.
	DCX	H		;Compute one less.
	CALL	MVLPAR		;Into table.
	DCX	H		;Compute 2 less.
	CALL	MVLPAR
	POP	H		;Get NLINES back.
	MOV	A,L		;Get # lines.
	ORA	A		;Clear carry bit.
	RAR			;Divide by two.
	CALL	MVAPAR		;Put into table.
	SHLD	NLIND2		;Save for own use.
	JMP	SETPG2		;Branch.
;
;	Get number of lines to move for UP and DOWN.
;
SETPG1:	CALL	PRTERR		;Print error message.
SETPG2:	LXI	B,SPGMSG	;BC-> # lines prompt.
	CALL	PRDCCH		;Print mess. get user's value.
	JZ	SETPG2		;Back if Rubout.
	PUSH	D		;Save DE.
	LXI	D,3		;At least 3 lines.
	PUSH	H		;Save user's value.
	LHLD	NLINES		;Get total screen lines.
	DCX	H		;Adjust for border line.
	MOV	B,H		;Put in BC.
	MOV	C,L
	POP	H		;Restore user's value.
	CALL	RNGCHK		;Check for valid range.
	POP	D		;DE-> parameter table.
	JC	SETPG1		;Branch if bad value.
	CALL	MVLPAR		;Save in table.
	JMP	SETCU2		;Branch.
;
;	Get number line range for cursor.
;
SETCU1:	CALL	PRTERR		;Print error.
SETCU2:	LXI	B,SCTPMS	;BC-> cursor top prompt.
	CALL	PRDCCH		;Print message, get user's value.
	JZ	SETCU2		;Back if Rubout.
	PUSH	D		;Save DE.
	LXI	D,1		;Top line is 1.
	PUSH	H		;Save user's value.
	LHLD	NLINES		;Get total screen lines.
	DCX	H		;Account for bottom border.
	MOV	B,H		;Put in BC.
	MOV	C,L
	POP	H		;Restore user's value.
	CALL	RNGCHK		;Check for valid range.
	POP	D		;DE-> parameter table.
	JC	SETCU1		;Branch if bad value.
	CALL	MVLPAR		;Save in table.
	SHLD	CRVTTP		;Save for own use too.
	JMP	SETCU4		;Branch.
;
SETCU3:	CALL	PRTERR		;Print error message.
SETCU4:	LXI	B,SCBTMS	;BC-> cursor bottom prompt.
	CALL	PRDCCH		;Print mess. get user's value.
	JZ	SETCU2		;Back if Rubout.
	PUSH	D		;Save DE.
	PUSH	H		;Save user's value.
	LHLD	CRVTTP		;Get cursor top position.
	XCHG			;DE = minimum bottom pos.
	LHLD	NLINES		;Get total screen lines.
	DCX	H		;Account for border line.
	MOV	B,H		;Put in BC.
	MOV	C,L
	POP	H		;Restore user's value.
	CALL	RNGCHK		;Check for valid range.
	POP	D		;DE-> parameter table.
	JC	SETCU3		;Branch if bad value.
	CALL	MVLPAR		;Save in table.
	JMP	SETSC4		;Continue.
;
;	Get screen and displayed line length values.
;
SETSC3:	CALL	PRTERR		;Print error.
SETSC4:
	IFNOT	MEMVRS,[
	LXI	H,255		;Fake it
	SHLD	SLNLEN
	JMP	SETSC6		;continue
	]
;
	IF	MEMVRS,[
	LXI	B,LENMSG	;BC-> line length prompt.
	CALL	PRDCCH		;Print mess. get user's value.
	JZ	SETSC4		;Back if Rubout.
	PUSH	D
	LXI	D,20		;At least 20 columns.
	LXI	B,255		;At most 255 columns.
	CALL	RNGCHK		;Value within range?
	POP	D		;DE-> parameter table.
	JC	SETSC3		;No, give error.
	SHLD	SLNLEN		;Save for own use.
	CALL	MVHLPR		;Put into table.
	DCX	H		;Compute 1 less.
	CALL	MVHLPR		;Into table.
	JMP	SETSC6		;Continue.
]
;
SETSC5:	CALL	PRTERR		;Print error.
SETSC6:	LXI	B,DISMSG	;BC-> display length prompt.
	CALL	PRDCCH		;Print mess. get user's value.
	JZ	SETSC6		;Back if Rubout.
	IF	CRTVRS, [
	DCX	H		;Don't use right margin on CRT.
	]
	PUSH	D		;Save parameter pointer.
	LXI	D,20		;At least 20 long.
	LDA	SLNLEN		;Get screen length.
	MOV	C,A		;At most the screen length.
	MOV	B,D		;Set high order B to 0.
	CALL	RNGCHK
	POP	D
	JC	SETSC5
	SHLD	LINLEN		;Save display length.
	CALL	MVHLPR		;Put into table.
	DCX	H		;Compute 1 less.
	CALL	MVHLPR
;
	IF	MEMVRS, [
;
;	Get screen address.
;
SETSC7:	LXI	B,SCRMSG	;BC-> screen address prompt.
	CALL	PRHXCH		;Print mess, get user's value.
	JZ	SETSC7		;Back if Rubout.
	SHLD	SCRADD		;Save it for later.
;
;	Do all kinds of computations for screen parameters.
;
	LHLD	NLINES		;Get NLINES.
	DCX	H		;Get NLINES-1.
	MOV	B,H		;Move it to BC.
	MOV	C,L
	LHLD	SLNLEN		;Get screen line length.
	CALL	MULTBC		;Compute (NLINES-1)*SLNLEN)
	MOV	B,H		;Move it to BC.
	MOV	C,L
	LHLD	SCRADD		;Get screen begin.
	DAD	B		;VRAM+((NLINES-1)*SLNLEN)
	CALL	MVHLPR		;Save border address in table.
;
;	Setup the table of screen line addresses.
;
	LHLD	NLINES		;Get number of screen lines.
	MOV	B,L		;Save in B.
	LHLD	SCRADD		;Get screen address.
	CALL	MVHLPR		;First table entry.
;
SETSC8:	PUSH	D		;Save table pointer.
	XCHG			;DE = screen address.
	LHLD	SLNLEN		;Get line length again.
	DAD	D		;Address of next line.
	POP	D		;DE-> paramater table.
	CALL	MVHLPR		;Save in table.
	DCR	B		;Done all screen lines?
	JNZ	SETSC8		;No, continue.
;
;	Setup the screen init code.
;
SETSIN:	LHLD	SCRINI		;HL-> init code area.
	LXI	B,20		;Area is 20 bytes long.
	MVI	A,00		;Fill with NOP instruction.
	CALL	FILL		;Fill the area with NOPs.
	JMP	SETSI1		;Branch around error.
;
SETSI0:	CALL	PRTERR		;Print error.
;
SETSI1:	LXI	B,IN1MSG	;BC-> message.
	CALL	PRDCCH		;Print mess. get user's value.
	JZ	SETSI1		;Back if RUBOUT.
	LXI	D,0000		;At least 00. (No negative #)
	LXI	B,5		;At most 5.
	CALL	RNGCHK		;Check for valid range.
	JC	SETSI0		;Reprompt if bad.
	MOV	C,L		;Put number in C.
	MOV	A,C		;Get the count.
	ORA	A		;Is it zero?
	RZ			;Yes, return to main for confirmation.
;
	LHLD	SCRINI		;HL-> init code area again.
	XCHG			;DE-> init code area.
	PUSH	B		;Save # of pairs.
	LXI	B,IN2MSG	;BC-> message.
	CALL	PRTMSG		;Print it.
	POP	B		;BC = # pairs to output.
;
SETSI2:	PUSH	B		;Save count.
	JMP	SETSI4		;Continue.
;
SETSI3:	POP	D		;Restore pointer in case of RUBOUT.
;
SETSI4:	PUSH	D		;Save current pointer.
	LXI	B,IN3MSG	;BC-> data prompt message.
	CALL	PRHXCH		;Print mess, get data byte.
	JZ	SETSI3		;Back if Rubout.
	MVI	A,03EH		;Get MVI A, instruction.
	CALL	MVAPAR		;Place instruction.
	CALL	MVLPAR		;Place data byte.
	LXI	B,IN4MSG	;BC-> port prompt message.
	CALL	PRHXCH		;Print mess, get port number.
	JZ	SETSI3		;Back if Rubout.
	MVI	A,0D3H		;Get OUT instruction.
	CALL	MVAPAR		;Place instruction.
	CALL	MVLPAR		;Place port number.
	POP	H		;Ignore old pointer.
	POP	B		;Restore count.
	DCR	C		;Decrement count.
	JNZ	SETSI2		;Continue until all pairs done.
	]
	RET
	.PAGE
;
;	Setup the two memory parameters.
;
SETME1:	CALL	PRTERR		;Print error.
SETMEM:	LHLD	ISPARE		;HL-> first memory param.
	XCHG			;DE-> first memory param.
	LXI	B,MM1MSG	;BC-> first message.
	CALL	PRDCCH		;Print mess. get user's value.
	JZ	SETMEM		;Start over if Rubout.
	PUSH	D		;Save pointer.
	LXI	D,1024		;At least 1K bytes.
	LXI	B,8000H		;At most 32K.
	CALL	RNGCHK		;Check for valid range.
	POP	D		;Restore pointer.
	JC	SETME1		;Branch if bad.
	CALL	MVHLPR		;Save value in param.
	JMP	SETME3		;Continue.
;
SETME2:	CALL	PRTERR		;Print error.
SETME3:	LXI	B,MM2MSG	;BC-> second message.
	CALL	PRDCCH		;Print mess. get user's value.
	JZ	SETME3		;Start over if Rubout.
	PUSH	D		;Save pointer.
	LXI	D,1		;At least 1K bytes.
	LXI	B,32		;At most 32K.
	CALL	RNGCHK		;Check for valid range.
	POP	D		;Restore pointer.
	JC	SETME2		;Branch if bad.
	LXI	B,8		;8 sectors per 1K.
	CALL	MULTBC		;Compute total sectors.
	CALL	MVHLPR		;Save value in param.
	RET
	.PAGE
;
;	Setup the intitial Tab table.
;
SETTB1:	CALL	PRTERR		;Print error.
;
SETTAB:	LXI	B,TBQMSG	;BC-> tab question message.
	CALL	PRTMSG		;Print it.
	LXI	H,DEFDMA	;Wait for a return
	MVI	M,10		;max input length
	XCHG			;
	MVI	C,READLN	;
	CALL	BDOS		;
	LDA	DEFDMA+2	;Get the reply.
	ANI	5FH		;Convert lower to upper case.
	CPI	'Y'		;Use default positions?
	RZ			; Yes, all done
	CPI	'N'		;User positions?
	JNZ	SETTAB		;Repeat if not Y or N.
;
SETTB2:	LHLD	TABPOS		;HL = Tab table address.
	XCHG			;DE-> tab table in VEDIT.
	LXI	B,TABMSG	;BC-> tab message prompt.
	CALL	PRTMSG		;Print it.
	MVI	B,33		;Up to 33 tab positions.
;
SETTB3:	CALL	DECCHK		;Get next number.
	JZ	SETTB2		;Start over if Rubout.
	CPI	CR		;Reached end of line?
	JZ	SETTB4		;Yes, branch.
	MOV	A,L		;No, get the number.
	CPI	255		;Is it too large?
	JZ	SETTB1		;Yes, give error.
	STAX	D		;No, save in table.
	INX	D		;Bump table pointer.
	DCR	B		;Decrement count.
	JNZ	SETTB3		;Continue if not exhausted.
;
SETTB4:	MVI	A,0FFH		;Get table fence.
	STAX	D		;Save fence in table.
	RET
	.PAGE
;
;	Setup the values to produce a 1 millisecond delay.
;
	IF	CRTVRS, [
SETDEL:	LHLD	ISPARE		;HL = address of misc. parameters.
	LXI	B,4		;Offset to DLYVAL.
	DAD	B
	XCHG			;DE-> DLYVAL in table.
	LXI	B,DELMSG	;BC-> delay value message.
	CALL	PRTMSG		;Print the message.
	CALL	DECCHK		;Get users value.
	JZ	SETDEL		;Repeat message if Rubout.
	JMP	MVLPAR		;Save in table.
	]
;
;	Get flag for starting in INSERT mode.
;
SETINS:	LHLD	ISPARE		;HL = address of misc. parameters
	LXI	B,5		;Offset to INSFLG
	DAD	B
	XCHG			;DE-> INSFLG in table
	LXI	B,INSMSG	;BC -> prompt
	CALL	PRTMSG		;Print the message
	CALL	DECCHK		;Get users value
	JZ	SETINS		;Repeat if Rubout
	JMP	MVLPAR		;Save in table
;
;	Setup INIT and HELP drives
;
SETINI:	LHLD	ISPARE
	LXI	B,6
	DAD	B
	XCHG
	LXI	B,INIMS1
	CALL	PRTMSG
	CALL	DECCHK		;Get users value
	JZ	SETINI		;Repeat if Rubout
	CALL	MVLPAR		;Save in table
	LXI	B,INIMS2	;BC -> prompt
	CALL	PRTMSG		;Print the message
	CALL	DECCHK		;Get users value
	JZ	SETINS		;Repeat if Rubout
	CALL	MVLPAR		;Save in table
	LXI	B,INIMS3	;BC -> prompt
	CALL	PRTMSG		;Print the message
	CALL	DECCHK		;Get users value
	JZ	SETINS		;Repeat if Rubout
	JMP	MVLPAR		;Save in table
	.PAGE
;
;	Setup the status line characters.
;
SETSTT:	LXI	B,STAMSG	;BC-> status line message.
	CALL	PRTMSG		;P	LXI	B,HGHMSG	;Yes, output thing fisrt
	CALL	PRTMS0
	POPA			;Restore the character

OUTKE6:	ANI	07FH		;Mask off top bit
	CPI	27		;Is it ESC-ESC?
	JZ	OUTK2A		;Yes, branch to output second ESC.
	CPI	' '		;Is it a printable character?
	JNC	OUTKE1		;Yeond table.
	LHLD	STATCH		;HL-> first char.
SETST1:	MOV	A,M		;Get next char,
	ANI	07FH		;Strip the high bit.
	JZ	SETST2		;If its 00H it stays
	XRA	C		;Maybe set the top bit
SETST2:	MOV	M,A		;Back in table.
	INX	H		;Bump pointer.
	CALL	CMHLDE		;Reach		;Followed by a space
	CALL	CONOUR
	POP	PSW		;Restore the registers
	POP	B
	POP	D
	POP	H
	RET
;
; Tab over to position KEYCOL, current position is in CURCOL
;
TABOVR:	PUSH	H
	PUSH	D
	PUSH	B
	PUSH	PSW
	MVI	A,KEYCOL	;Column we want
	LXI	H,CURALL	BDOS
	LXI	B,CRLF
	CALL	PRTMSG
	LHLD	USRMSG		;Get address to copy message to
	XCHG			;Put into DE
	LXI	H,DEFDMA+1	;Get length of string
	MOV	C,M
	INX	H
	CALL	MOVE		;Move it
	MVI	A,' '+128	;Get terminaing SPace.
	STAX	D		;Last message characterW		;Restore registers
	POP	B
	POP	D
	POP	H
	RET
	.PAGE
;
; PRTMSG - Print message at BC, return Z if no message.
;	   Return BC-> next message.
;
PRTERR:	LXI	B,NUMERR	;BC-> error message.
PRTMSG:	PUSH	B		;Put mess. addr on stack.
	MVI	C,CR		;Fills keyboard every 500 uS
	POPA			;;Restore char in A
;
				;{DMPKEY}
OUTKEY:	PUSH	H		;Save all the registers
	PUSH	D
	PUSH	B
	PUSH	PSW
	CPI	13		;[NONE] ?
	JNZ	OUTKE2
	LXI	B,NONEMS
	CALL	PRTMS0
	JMP	OUTKE3

OUTKE2:	JMP	OUTKE4
;
OUTK2A:	LXI	.
	RET
	.PAGE
;
; OUTKEY - Output %A in form of "CTRL-X", etc (1/11/85)
; OUTKYP - First delay for 35 milliseconds, polling keyboard
;
OUTKYP:	PUSHA			;;Save char. in A
	MVI	A,35		;;Setup for 35 mS delay
	CALL	DELAY		;;Perform delay
				;;This poB,ESCKMS
	CALL	PRTMS0
	JMP	OUTKE3


OUTKE4:	ORA	A		;Is high bit set?
	JP	OUTKE6		;Nope ...
	PUSHA			;Save the character
s, just output it
	PUSHA
	LXI	B,CTRLMS
	CALL	PRTMS0
	POPA
	ADI	'@'

OUTKE1:	CPI	7FH		;Rubout?
	JNZ	OUTKE5		;Nope ...
	LXI	B,RUBMS
	CALL	PRTMS0
	JMP	OUTKE3

OUTKE5:	MOV	C,A		;Put the character into C
	CALL	CONOUR		;Output it
OUTKE3:	MVI	C,' 'rint the message.
	CALL	HEXCHK		;Get the answer.
	JZ	SETSTT		;Over again if Rubout.
	MOV	A,L		;Get the answer.
	ORA	A		;Is answer 1=YES?
	MVI	C,128		;YES
	JNZ	SETST0
	MVI	C,0		;NO
;
SETST0:	LHLD	SWTBL		;HL-> beyond end of table.
	XCHG			;DE-> beyCOL	;Where we are
	SUB	M		;A=number of spaces to output
	JC	TABOV2		;None ...
	MOV	B,A		;Put count into B
TABOV1:	MVI	C,' '
	CALL	CONOUR		;Space over
	DCR	B		;Decrement counter
	JNZ	TABOV1		;Loop until correct number of spaces output
TABOV2:	POP	PSed end of table?
	JC	SETST1		;Continue till done.
	RET
	.PAGE
;
;	Set Signon Message
;
SETSIG:	LXI	B,SONMSG
	CALL	PRTMSG
	LXI	H,DEFDMA	;Input line to DEFDMA
	MVI	M,64		;Max length
	XCHG			;Put buffer address into DE
	MVI	C,READLN	;Read line
	Crst send a CR-LF.
	CALL	CONOUR
	MVI	C,LF
	CALL	CONOUR
	POP	B
;
PRTMS0:	PUSH	B
	XTHL			;HL-> message.
PRTMS1:	MOV	A,M		;Get next char.
	ANI	7FH		;Strip off top bit.
	JZ	PRTMS2		;Branch if zero.
	MOV	C,A		;Put char. in C.
	CALL	CONOUR		;Print it.
	CPI	10		;New line?
	LDA	CURCOL		;Increment current position
	JNZ	PRTMS3		;Nope, normal increment
	MVI	A,255		;Yes, set it to zero
PRTMS3:	INR	A		;
	STA	CURCOL		;
	MOV	A,M		;End of message?
	INX	H		;Point to next character
	ANI	80H
	JZ	PRTMS1		;No output next character

PRTMS2:	XTHL			;Restore HL.
	POP	B		;Restore updated pointer.
	RET
;
; PRTKEY - Print CR-LF and message, check for <CTRL-S>
;
PRTKEY:	PUSH	B		;Put mess. addr on stack.
	MVI	C,CR		;First send a CR-LF.
	CALL	CONOUR
	MVI	C,LF
	CALL	CONOUR
;
	CALL	CONSTR		;Is a character waiting?
	JZ	PRTKY1		;No, branch
	CALL	CONINR		;Yes, get the character
	CPI	013H		;Is it <CTRL-S>
	JNZ	PRTKY1		;No, branch, ignore it
	CALL	CONINR		;Yes, wait for ignore next char
;
PRTKY1:	POP	B
	JMP	PRTMS0		;Merge above
	.PAGE
;
; Number conversion routines.
;
PRDCCH:	CALL	PRTMSG		;Print the message.
DECCHK:	CALL	GETDEC		;Convert the decimal number.
	JMP	CHKRUB		;Check if line to be deleted.
;
PRHXCH:	CALL	PRTMSG		;Print the message.
HEXCHK:	CALL	GETHEX		;Convert the hex number.
	JMP	CHKRUB		;Check if line to be deleted.
;
;
;
GETDEC:	PUSH	D		;Save the registers.
	PUSH	B
GETDC0:	LXI	H,00		;Preset result.
	CALL	GETCON		;Get keyboard character.
	CPI	' '		;Is it a space?
	JZ	GETDC0		;Yes,skip over it.
GETDC1:	CPI	'0'		;Is char less than '0'?
	JC	GETDC2		;Yes, branch.
	CPI	':'		;Is it numeric?
	JNC	GETDC2		;No, branch.
	ANI	0FH		;Mask for decimal digit.
	LXI	B,10		;Get 10.
	CALL	MULTBC		;Multipy by 10.
	MOV	E,A		;Get new digit in DE.
	MVI	D,00
	DAD	D		;Add in new digit.
	CALL	GETCON		;Get next digit.
	JMP	GETDC1		;Continue.
;
GETDC2:	POP	B		;Restore the registers.
	POP	D
	RET			;Return for now.
;
GETHEX:	PUSH	D		;Save registers.
	PUSH	B
GETHX0:	LXI	H,00		;Preset result.
	CALL	GETCON		;Get keyboard character.
	CPI	' '		;Is it space?
	JZ	GETHX0		;Yes, skip over it.
GETHX1:	CPI	'0'		;Is it less than '0'?
	JC	GETDC2		;Yes, branch.
	CPI	':'		;Is char 0-9?
	JC	GETHX3		;Yes, branch.
	ANI	5FH		;Covert lower to upper case.
	CPI	'A'		;Is it less than 'A'?
	JC	GETDC2		;Yes, branch.
	CPI	'G'		;Is it greater than 'H'?
	JNC	GETDC2		;Yes, branch.
;
	SUI	'A'-'9'-1	;Subtract gap between 9 and A.
GETHX3:	ANI	0FH		;Mask for the nibble.
	DAD	H		;Multiply HL by 16.
	DAD	H
	DAD	H
	DAD	H
	ORA	L		;Add in 4 bits.
	MOV	L,A		;HL = new number.
	CALL	GETCON		;Next next char.
	JMP	GETHX1		;Continue.
;
; CHKRUB - Check if RUBOUT or [CTRL-U] was typed.
;	   Return 'Z' if one was typed.
;
CHKRUB:	CPI	7FH		;Was char. RUBOUT?
	RZ			;Yes, return 'Z'.
	CPI	5FH		;Was char. converted RUBOUT?
	RZ			;Yes, return 'Z'.
	CPI	15H		;Was char. [CTRL-U]?
	RET			;Return with flags set.
	.PAGE
;
; Utility Subroutines.
;
;
; MVDETB - Move word at (DE) to (HL).
;
MVDETB:	LDAX	D		;Get low order byte.
	MOV	M,A		;Store low order byte.
	INX	D		;DE-> high order byte.
	INX	H		;HL-> high order store.
	LDAX	D		;Get high order byte.
	MOV	M,A		;Store high order byte.
	DCX	D		;Restore DE.
	RET
;
; MVHLPR - Move HL to (DE).
;
MVHLPR:	PUSHA			;Save A
	MOV	A,L		;Get low order byte.
	STAX	D		;Save in table.
	INX	D		;Bump table pointer.
	MOV	A,H		;Get high order byte.
	STAX	D		;Save in table.
	INX	D		;Bump table pointer.
	POPA			;Restore A
	RET
;
; MVTBHL - Move (DE) to HL.
;
MVTBHL:	LDAX	D		;Get low order byte.
	MOV	L,A		;Put it in L.
	INX	D		;Bump pointer.
	LDAX	D		;Get high order byte.
	MOV	H,A		;Put it in H.
	INX	D		;Bump pointer.
	RET
;
; MVLPAR - Move L to (DE).
;
MVLPAR:	MOV	A,L		;Get the value.
MVAPAR:	STAX	D		;Save it in table
	INX	D		;Bump pointer.
	RET
;
; MVINHL - Move word at (HL) to HL
;
MVINHL:	MOV	A,M		;Save low order byte.
	INX	H		;Point ot high byte.
	MOV	H,M		;Get high byte.
	MOV	L,A		;Get low order byte.
	RET
;
; MVINDE - Move word at (HL) to DE.
;
MVINDE:	MOV	E,M		;Get low order byte.
	INX	H		;Point ot high byte.
	MOV	D,M		;Get high byte.
	RET
;
; CMHLDE - Compare HL with DE for Z, NZ, C or NC.
;
CMHLDE:	MOV	A,H
	CMP	D
	RNZ
	MOV	A,L
	CMP	E
	RET
;
; SETZER - Changes A to zero without changing flags.
;
SETZER:	MVI	A,00		;Clear the accumulator.
	RET			;No flags changed.
;
; RNGCHK: Checks that DE <= HL <= BC.  Else returns 'C'.
;
RNGCHK:	CALL	CMHLDE		;Is HL below lower limit?
	RC			;Yes, return with 'C'.
	CALL	CMBCHL		;Is HL above upper limit?
	RET			;Return with 'C' if YES.
;
; CMBCHL - Compare BC with HL.
;
CMBCHL:	MOV	A,B
	CMP	H
	RNZ
	MOV	A,C
	CMP	L
	RET
;
; ADDAHL - Add A to HL.
;
ADDAHL:	ADD	L		;Add A and L.
	MOV	L,A		;Result in L.
	RNC			;Return if no carry.
	INR	H		;Else add carry to H.
	RET
;
; SUBAHL - Subtract A from HL.
;
SUBAHL:	CMA			;Get negative A.
	ADI	1		;Make 2's complement. (INR A won't work!!!)
	RZ			;Return if A = 00.
	ADD	L		;Really subtract.
	MOV	L,A		;Save result in L.
	RC			;Return if no borrow.
	DCR	H		;Else subtract borrow from H.
	RET
;
; MULTBC - Multiply HL by BC.
;
MULTBC:	PUSH	D		;Save registera.
	PUSH	B
	PUSHA
	XCHG			;DE = number.
	LXI	H,00		;Clear product.
MLTBC1:	MOV	A,B		;Is count zero?
	ORA	C
	JZ	MLTBC2		;Yes, done.
	DAD	D		;Add in again.
	DCX	B		;Decrement count.
	JMP	MLTBC1		;Continue.
;
MLTBC2:	POPA			;Restore registers.
	POP	B
	POP	D	
	RET
;
; MOVE - Move %C bytes from (HL) to (DE).
;
MOVE:	MVI	B,00		;Upper byte is zero.
;
; MOVEBC - Move %BC bytes form (HL) to (DE).
;
MOVEBC:	MOV	A,B		;Is the count zero?
	ORA	C
	RZ			;Yes, return.
	MOV	A,M		;No, get the next byte.
	STAX	D		;Move it to destination.
	INX	D		;Increment pointers.
	INX	H
	DCX	B		;Decrement count.
	JMP	MOVEBC		;Continue.
;
; FILL - Fill %BC locations with %A from (HL).
;
FILL:	PUSH	D		;Save DE
	MOV	M,A		;Fill first location
	MOV	E,L		;Move HL to DE
	MOV	D,H
 	INX	D		;DE-> next location to fill.
	DCX	B		;One position already filled.
	CALL	MOVEBC		;FILL is overlapped Move.
	POP	D		;Restore DE.
	RET
;
; DELAY - Perform a delay of the # of milliseconds in reg. A.
;
				;{OUTKYP,SETKEY}
DELAY:	PUSH	H
	PUSH	D
	PUSH	B
	MOV	L,A		;;Put delay value in HL
	MVI	H,00		;;8 bit value
	DAD	H		;;Need double for 500uS count
DELAY1:	MVI	B,52		;;Magic value for 500uS at 2mhz
DELAY2:	NOP
	DCR	B
	JNZ	DELAY2		;;500uS loop
;
	CALL	CONSTR		;Poll for keyboard character
	DCX	H		;Decrement count
	MOV	A,H
	ORA	L
	JNZ	DELAY1		;Loop until time expired
;
	POP	B		;Restore registers
	POP	D
	POP	H
	RET
	.PAGE
;************************************************
;*						*
;*		Check sum routine		*
;*						*
;************************************************
;
CHKSUM:	LXI	D,0		;Clear check sum
	LHLD	FILSIZ		;Length / 128
	MOV	B,H
	MOV	C,L
	LXI	H,TXTBUF	;Point to start of editor
CHKS1:	PUSH	B		;Save length / 128
	MVI	B,64
CHKS2:	MOV	A,M		;Add low 8 bits to DE
	ADD	E
	MOV	E,A
	JNC	CHKS3
	INR	D
CHKS3:	INX	H		;Point to next byte in editor
	MOV	A,M		;Add on high 8 bits
	ADD	D		;Forget about any carry
	MOV	D,A
	INX	H
	DCR	B		;Loop for 128 bytes
	JNZ	CHKS2
	POP	B		;Get length / 128
	DCX	B		;Done?
	MOV	A,B
	ORA	C
	JNZ	CHKS1		;Nope, checksum next 128 bytes
	RET
	.PAGE
;************************************************
;*						*
;*		Console Routines.		*
;*						*
;************************************************
;
; Console routines to save all registers
;
CONINR:	PUSH	H		;Save registers.
	PUSH	D
	PUSH	B
CONINW:	CALL	CONST1		;Get character from CPM if there is one
	LDA	GET		;Get buffer GET pointer
	MOV	E,A		;Put into %DE
	MVI	D,0
	LDA	PUT		;Get buffer PUT pointer
	CMP	E		;Same as GET?
	JZ	CONINW		;Yes, wait for a character
	LXI	H,CONBUF	;Nope, pointer to the charactdr
	DAD	D
	INR	E		;Increment buffer get pointer
	MOV	A,E		;Save new buffer get pointer
	ANI	0FH
	STA	GET
	MOV	A,M		;Get the character
	CPI	3		;Control-C?
	JNZ	CONRET		;Nope, all done here
	LDA	CTRLCF		;Yes, abort, or return the ^C?
	ORA	A		;
	JZ	EXITRT		;Abort ...
	MVI	A,3		;Return a control-C
CONRET:	POP	B		;Restore registers
	POP	D
	POP	H
	RET			;Return with character in A.
;
CONOUR:	PUSH	H		;Save all registers.
	PUSH	D
	PUSH	B
	PUSHA
	PUSH	B
	CALL	CONST1		;Poll keyboard
	POP	B
	MOV	E,C		;Put character into E for BDOS
	MVI	C,2		;CONOUT function
	LDA	OUTFLG		;Console, or printer?
	ORA	A		;#0 = console
	JNZ	CONOU1		;Console ...
	MVI	C,5		;Printer ...
CONOU1:	CALL	BDOS		;Output the character
	CALL	CONST1		;Poll the keyboard
	POPA
	JMP	CONRET		;Merge above.
;
; CONSTR - Combination POLL routine and status routine
;	   Polls keyboard and puts char into buffer
;	   Return: A=00 if buffer empty, A=FF if buffer contains char
;
				;{PRTKEY,DELAY}
CONSTR:	PUSH	H		;Save registers.
	PUSH	D
	PUSH	B
	CALL	CONST1		;Get the console status.
	JMP	CONRET		;Merge above to restore regs and return.
;
;	Actual routine
;
				;{CONINW,CONOUR,CONSTR}
CONST1:	CALL	CONST		;Check if keyboard character
	ORA	A
	LDA	PUT		;Get put pointer
	MOV	E,A
	JZ	CONST2		;No character ...
	MVI	D,0
	LXI	H,CONBUF	;Index into the buffer
	DAD	D
	INR	E		;Increment put pointer
	MOV	A,E
	ANI	0FH
	STA	PUT		;Save new put pointer
	PUSH	H		;Save buffer pointer
	CALL	CONIN		;Get the character
	POP	H		;Restore buffer pointer
	MOV	M,A		;Put character into buffer

CONST2:	LDA	GET		;Get get pointer
	SUB	E		;Return 00H if buffer is empty
	RZ
	MVI	A,0FFH		;Else 0FFH
	RET
;
GETCON:	CALL	CONINR		;Get console character
	MOV	C,A		;Put in C for console output
	CPI	7FH		;Is it rubout?
	JNZ	GETCN1		;No, branch
	MVI	C,'#'		;Yes, change to "#"
GETCN1:	CPI	15H		;Is it [CTRL-U]?
	JNZ	CONOUR		;No, send char to console
	MVI	C,'#'		;Yes, change to "#"
	JMP	CONOUR		;Echo it
	.PAGE
;
;	These are the alternate Console I/O entry points when direct
;	I/O calls in MP/M are used.
;
MPMTBL:	JMP	MPMST		;This table is copied to the I/O Jump
	JMP	MPMIN		;vectors below for the MPM version
	JMP	MPMOUT
;
MPMST:	MVI	C,CONSIO	;Perform direct console I/O
	MVI	E,0FEH		;FE denotes console status
	JMP	BDOS		;Get input status and/or character
	DS	6		;Patch space for STA TEMPCH, RET
;
MPMIN:	MVI	C,CONSIO	;Perform direct console I/O
	MVI	E,0FFH		;FF denotes console input
	JMP	BDOS		;Get console char. if available
	DS	12		;Patch space
;
MPMOUT:	MOV	E,C		;Put the character in E
	MVI	C,CONSIO	;Perform direct console I/O
	JMP	BDOS		;Send char. to console through CPM
	DS	4		;Patch space
;
TEMPCH:	DB	00		;Character store if routines changed to CPM 2.2
;
;	Internal copy of BIOS I/O Jump vectors.
;
CONST:	JMP	00		;Console status
CONIN:	JMP	00		;Get console character
CONOUT:	JMP	00		;Send console character
	.PAGE
	.INSERT	DMPKEY		;Keyboard dump routine
	.PAGE
;
	.XLIST
	IF	LSMSG, [
	.LIST
	]
;
;	Various FCBs for CP/M communication.
;
INFCB:	DB	00		;Input FCB
INFCB1:	DC	"VEDIT   SET"
	DB	0,0,0,0
	DS	16
	DB	00
;
OUTFCB:	DB	00		;Output FCB
OUTFC1:	DC	"VEDIT   $$$"
	DB	0,0,0,0
OUTFC2:	DS	16
	DB	00
;
	IF	CRTVRS,[
TBLFCB:	DB	00		;Crt table fcb
	DC	"CRT     TBL"
TBLFEX:	DB	00
	DB	00,00,00
	DS	16
TBLFNR:	DB	0

TBLBUF:	DS	128		;Buffer to read crt table into
	]
;
	.XLIST
	IF	LSMSG, [
	.LIST
	]
	.INSERT	VEDSTMSG
;
	IF	CRTVRS, [
	.INSERT	VDCRTTAB
	]
;
	.PAGE
;
GET:	DS	1		;Get pointer
PUT:	DS	1		;Put pointer
CTRLCF:	DS	1		;00 = Ctrl-C abort, =FF ctrlc will not abort
OUTFLG:	DS	1		;00 = printer, 01 = console output
CURCOL:	DS	1		;Current column
ESCNUM:	DS	1		;Escape table # to use
FNCADD:	DS	2		;Address of edit function being customized
FILSIZ:	DS	2		;Size of editor
MAXLIN:	DS	2		;# screen lines VEDIT was assembled for
	IF	CRTVRS, [
ADDLED:	DS	2		;Relocated address of CRT escape sequences
ESCTB1:	DS	2		;Relocated address of Escape CR table # 1
ESCTB2:	DS	2		;Relocated address of Escape CR table # 2
	]
ESCDC1:	DS	2		;Relocated address of ESC decode table # 1
ESCDC2:	DS	2		;Relocated address of ESC decode table # 2
CTLDEC:	DS	2		;Relocated address of Control Char decode table
CNTADD:	DS	2		;Relocated address of control addresses
TABPOS:	DS	2		;Relocated address of tab table
CHRTBL:	DS	2		;Relocated address of character table
STATCH:	DS	2		;Relocated address of status line chars
ISPARE:	DS	2		;Relocated address of memory parameters
SWTBL:	DS	2		;Relocated address of switch table
PRMTBL:	DS	2		;Relocated address of parameter table
NLINE:	DS	2		;Relocated address of screen parameters
SCRINI:	DS	2		;Relocated address of screen init code
USRMSG:	DS	2		;Relocated address of user message
;
ESCCH1:	DB	00		;Escape character # 1
ESCCH2:	DB	00		;Escape character # 2
ESCNP1:	DB	00		;Escape sequence Noop #1 char
ESCNP2:	DB	00		;Escape sequence Noop #2 char
ESCFLG:	DS	1		;Escape mode flag
;
CNINCH:	DS	1		;Temp. storage for console char. (MP/M)
TMPCHR:	DS	1		;Temporary storage for keyboard char
ULCFLG:	DS	1		;00 = Upper/lower case escapes seq's equivalent
NLINES:	DS	2		;Number of screen lines
NLIND2:	DS	2		;NLINES / 2
CRVTTP:	DS	2		;Top line for cursor
SLNLEN:	DS	2		;Length of screen line
LINLEN:	DS	2		;Length of displayed line
SCRADD:	DS	2		;Address of screen memory
;
	DC	'Copyright (C) 1979,1985 by Theodore Green.'
	DC	'01/11/85'
;
	DS	40H		;Extra stack space.
;
	ORG	. & 0FF00H + 100H
;
STACK:
TXTBUF:				;Where buffer begins.
	.END

