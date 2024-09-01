	.PAGE
	.TITLE	'VEDITA1'
;**************************************************************************
;
; Debug features:
;
;	INIDV0 = 2, normally 1		[8/8/85]
;	INIDVA = 1, normally 1
;
; *****  Temporary kludges:  DEBUG = 2  ********
;
;	NLINES = 30 			For Ann Arbor Ambassador terminal
;
;**************************************************************************
;
	.OPSYN	.IFN,IF
	.OPSYN	.IFE,IFNOT
;
;	Conditional Assembly assignments
;
;	Set up simple variables for Version type.
;
MEMVRS	=	0
CRTVRS	=	0
PICVRS	=	0
TDLVRS	=	0
P8080	=	0
;
P8086	=	0
IBMPC	=	0
TIPC	=	0
MMIBM	=	0
MSDOS	=	0
WYSE	=	0
VWRD	=	0
;
CCPM86	=	0
MPM	=	1
;
TRUE	=	1
FALSE	=	0
;
;	Is this VEDIT or VEDIT PLUS ?
;
VPLUS	=	\ 'VEDIT (0) or VEDIT PLUS (1) ?'
;
;	Ask if this is a standard version
;
COMMON=\ '
Full version, Z-80, CRT           (1)
Full version, Z-80, Memory mapped (2)
Full version, 8080, CRT           (3)	note:Versions 1-8 have I/O polling
Full version, 8080, Memory mapped (4)	     set on and org. @ 0000H. CRT
Mini version, Z-80, CRT           (5)	     versions are 24X80 while MEM
Mini version, Z-80, Memory mapped (6)	     mapp`d versions are 16X64.
Mini version, 8080, CRT           (7)
Mini version, 8080, Memory mapped (8)	     CRT emulation is always ON.
Full version, Z-80, Model II, P&T (9)
Full version, Z-80, Model II, Gen (10)
Full version, Z-80, Piiceon @ 90H (11)
Other version made to custom specs.(12)
       enter a version number (1 to 12) '
;
;	Set defaults if standard version
;
	.IFN	COMMON - 12, [		;;Default when not "Custom Specs"
BASEVR	=	1			;Default base at 0000H
POLLING =	1			;Enable polling
	 ]
;
	.IFE	COMMON - 1, [		;Full version, Z-80 , CRT
FULL	=	1
CRTVRS	=	1
NLINES	=	24
LINLEN	=	80
	]
	.IFE	COMMON - 2, [		;Full version, Z-80 , Memory mapped
FULL	=	1
MEMVRS	=	1
MEMVER	=	1
NLINES	=	16
LINLEN	=	64
	]
	.IFE	COMMON - 3, [		;Full version, 8080 , CRT
FULL	=	1
CRTVRS	=	1
NLINES	=	24
LINLEN	=	80
P8080	=	1
	]
	.IFE	COMMON - 4, [		;Full version, 8080 , Memory mapped
FULL	=	1
MEMVRS	=	1
MEMVER	=	1
NLINES	=	16
LINLEN	=	64
P8080	=	1
	]
	.IFE	COMMON - 5, [		;Mini version, Z-80 , CRT
FULL	=	0
CRTVRS	=	1
NLINES	=	24
LINLEN	=	80
	]
	.IFE	COMMON - 6, [		;Mini version, Z-80 , Memory mapped
FULL	=	0
MEMVRS	=	1
MEMVER	=	1
NLINES	=	16
LINLEN	=	64
	]
	.IFE	COMMON - 7, [		;Mini version, 8080 , CRT
FULL	=	0
CRTVRS	=	1
NLINES	=	24
LINLEN	=	80
P8080	=	1
	]
	.IFE	COMMON - 8, [		;Mini version, 8080 , Memory mapped
FULL	=	0
MEMVRS	=	1
MEMVER	=	1
NLINES	=	16
LINLEN	=	64
P8080	=	1
	]
	.IFE	COMMON - 9, [		;Full, Z-80, MM, Model II, P&T
FULL	=	1
MPM	=	0
POLLING  =	0			;Turn polling off
MEMVRS	=	1
MEMVER	=	2
TRSVER	=	2
NLINES	=	24
LINLEN	=	80
	]
	.IFE	COMMON - 10, [		;Full, Z-80, MM, Model II, General
FULL	=	1
MPM	=	0
POLLING  =	0			;Turn polling off
MEMVRS	=	1
MEMVER	=	2
TRSVER	=	1
NLINES	=	24
LINLEN	=	80
	]
	.IFE	COMMON - 11, [		;Full version, Z-80 , Piiceon
FULL	=	1
MPM	=	0
POLLING  =	0			;Turn polling off
PICVRS	=	1
PICBAS	=	90H
NLINES	=	24
LINLEN	=	80
	]
;
;
	.IFE	COMMON - 12, [	;Other version made to custom specs
;
P8080	= \ 'Z80 (0) OR 8080 (1) ? '
FULL	= \ 'MINI (0) OR FULL (1) ? '
VERTYP	= \ 'MEMORY MAPPED (1), CRT (2), PIICEON (3), TDL (4) ? '
;
	.IFE	VERTYP -1, [
MEMVRS	=	1		;Make MEMVRS True for IF condition
	]
	.IFE	VERTYP -2, [
CRTVRS	=	1
	]
	.IFE	VERTYP -3, [
PICVRS	=	1
	]
	.IFE	VERTYP -4, [
TDLVRS	=	1
	]
;
BASEVR	= \ 'BASE ADDRESS : 0000 (1), 4200H (2), OTHER (3) ? '
	.IFE	BASEVR -3, [
BASE	= \ 'BASE ADDRESS IN HEX (H) ? '
	]
;
	IF	MEMVRS, [
MEMVER	= \ 'REGULAR (1), TRS80-II (2) ? '
	] [
MEMVER	=	0		;Value if not memory mapped
	]
;
	.IFE	MEMVER-2, [
TRSVER	= \ 'GENERAL (1), P&T (2) ? '
	]
;
	IF	PICVRS, [
PICBAS	= \'I/O BASE OF PIICEON ? '
	]
;
	IF	MEMVRS ! PICVRS ! TDLVRS, [
POLLING	=\ 'INCLUDE KEYBOARD POLLING ROUTINES? (0=NO) (1=YES) ? '
	] [
POLLING	=	1		;Include polling for CRT version
	]
;
NLINES	=\ 'ASSEMBLED NUMBER OF SCREEN LINES (16 OR 24) ? '
LINLEN	=\ 'ASSEMBLED LENGTH OF SCREEN LINES (64 OR 80) ? '
	]			;<.IFE COMMON - 12>
;
;	************* End of custom section ***********
;
;
	IFNOT	MEMVRS, [
MEMVER  =	0		;If not a memory mapped version
	]
	.IFE	MEMVER-2, [
VRAM	=	0F800H		;Memory for TRS-80 Model II
	] [
VRAM	=	0DC00H		;Else set for Ted's VDM board
	]
;
;
	.IFE	BASEVR -1, [
BASE	=	0000H		;CP/M base address
	]
	.IFE	BASEVR -2, [
BASE	=	04200H
	]
;
;
;
WORDP	=\ 'INCLUDE PRINT FORMATTER? (0=NO) (1=YES)'
WINDOO	=\ 'INCLUDE WINDOWS? (0=NO) (1=YES)'
DEMO	=\ 'DEMO VERSION? (0=NO) (1=YES)'
DEVLOP	=\ 'DEVELOPMENT VERSION?  (0=NO) (1=DEVELOPMENT) (2=ALPHA) (3=BETA)'
;
	.IFE	DEVLOP -1, [
DEBUG	=\ 'DEBUG VERSION? (0=NORMAL) (1=DEBUG) (2=A**2 AMBASSADOR)'
	.IFE	DEBUG - 2, [
NLINES	=	30		;JAN86 AMBASSADOR INSTALLing KLUDGE
	]
	] [
DEBUG	=	0
	]
;
; Listing section
;
LSANY	=\ 'PRODUCE LISTING?  (0=NO) (1=YES) (2=CUSTOMER PATCH ONLY)'
;
	.IFE	LSANY-1, [
LSMAIN	=\'LISTING OF MAIN? (0=NO) (1=YES) '
LSHARD	=\'LISTING OF HARDWARE DEPENDENT ROUTINES? (0=NO) (1=YES) '
LSIO	=\'LISTING OF CONSOLE I/O ROUTINES? (0=NO) (1=YES) '
LSINIT	=\'LISTING OF INTIALIZATION? (0=NO) (1=YES) '
LSFIL1	=\'LISTING OF OPSYS FILE HANDLING? (0=NO) (1=YES) '
LSFIL2	=\'LISTING OF COMMON FILE HANDLING? (0=NO) (1=YES) '
LSC1	=\'LISTING OF COMMAND PROCESSING? (0=NO) (1=YES) '
LSC2	=\'LISTING OF COMMAND FUNCTIONS? (0=NO) (1=YES) '
LSC3	=\'LISTING OF COMMAND SUPPORT? (0=NO) (1=YES) '
LSE1	=\'LISTING OF VPLUS MATH ROUTINES? (0=NO) (1=YES) '
LSSR	=\'LISTING OF SEARCH ROUTINES? (0=NO) (1=YES) '
LSCP	=\'LISTING OF PRINT FORMATTER? (0=NO) (1=YES) '
LSTREG	=\'LISTING OF TEXT REGISTER SUPPORT (0=NO) (1=YES) '
LSV0	=\'LISTING OF VISUAL- COMMAND INTERFACE? (0=NO) (1=YES) '
LSV1	=\'LISTING OF VISUAL MAIN? (0=NO) (1=YES) '
LSV2	=\'LISTING OF VISUAL FUNCTIONS? (0=NO) (1=YES) '
LSW1	=\'LISTING OF WORD PROCESSING FUNCTIONS? (0=NO) (1=YES) '
LSV3	=\'LISTING OF VISUAL DISPLAY? (0=NO) (1=YES) '
LSV4	=\'LISTING OF WINDOW DRIVERS? (0=NO) (1=YES) '
LSMISC	=\'LISTING OF AUXILIARY? (0=NO) (1=YES) '
LSBUF	=\'LISTING OF STORAGE AREA? (0=NO) (1=YES) '
;
	.IFE	LSBUF -1, [
LSFIX	=\'LISTING OF FIXED STORAGE (0=NO) (1=YES) '
LSCUST	=\'LISTING OF INSTALL STORAGE (0=NO) (1=YES) '
	] [
LSFIX	=	0
LSCUST	=	0
	]
	]			;<.IFE	LSANY-1>
;
	.XLIST
	.IFN	LSANY-1, [
LSMAIN	=	0
LSHARD	=	0
LSIO	=	0
LSINIT	=	0
LSFIL1	=	0
LSFIL2	=	0
LSC1	=	0
LSC2	=	0
LSC3	=	0
LSE1	=	0
LSSR	=	0
LSCP	=	0
LSTREG	=	0
LSV0	=	0
LSV1	=	0
LSV2	=	0
LSW1	=	0
LSV3	=	0
LSV4	=	0
LSMISC	=	0
LSBUF	=	0
LSFIX	=	0
LSCUST	=	0
	]

	.IFE	LSANY-2, [
LSCUST	=	1
	]
;
	IF	LSMAIN, [
	.LIST
	]
;
	IF	P8080, [
	.I8080
	]
;
;	CPU dependent equates
;
EXIT	=	BASE
DEFFCB	=	BASE + 005CH
DEFDMA	=	BASE + 0080H
