	.IDENT 	VEDIT
	.SBTTL	'Copyright (c) 1986 by CompuView Products, Inc.'
;****************************************
;*					*
;*	V E D I T    P L U S		*
;*					*
;****************************************
;
; Copyright (C) 1986 by CompuView Products, Inc.
;			1955 Pauline Blvd.
;			Ann Arbor, MI 48103
;
;	Written By: Theodore Green and Tom Burt
;
;	Last Change: Tom - Jan. 08 - 2.02a changes
;		         - Mar. 16 - 2.32b bug fix (GETTXT in R1)
;			 - Mar. 24 - 2.33 data storage a la VEDIT
;			 - Mar. 28 - RGBASE variable deleted
;
;		     Ted - Oct. 29, 1985 - Merge output routines
;			   Nov. 14, 1985 - New keyboard decode, LEARN
;			   Jan. 23 - New MM installation
;			   Jan. 30 - New print routines
;			   Feb. 06 - Memory management
;			   Mar. 16 - Windows
;		     Tom - Mar. 18 - C1,C2,C3,C5,CP,B1,R1,V0,MAIN merged VPLUS
;		     Ted - Mar. 25 - Extensive C1 and GETSTR changes
;			 - Mar. 26 - Numerous F1 changes and optimizations
;			 - Mar. 26 - Conditional assembly for windows and VPRINT
;		         - Apr. 02 - Windows and memory management
;			 - Apr. 03 - Merge VED-MAIN with VEDPLUS
;		     TnT - Apr. 10 - New T-Reg routines, etc
;		     Ted - Apr. 20 - [BLOCK] and [WINDOW] functions
;		     Tom - Apr. 21 - End of BUFTBL now REGTEN, marked by 0FFH
;			 - May  08 - TCMD, EOCMD, PRCGLB (new), GLBCMD removed
;			 - May  09 - Global K cmd (D cmd affected)
;				     PRCGLB renamed GLOBHL w/e.p. GLOB0,GLOBX
;				     PAKTX0 & PKTX00 added to PACKTX()
;			 - May  11 - BOL & EOL patterns |< & |>
;			 - May  12 - Group delimiter pattern |G
;				   - YM cmd finds matching group delimiter
;				   - GRPTBL below
;			 - May  13 - JAMFL set by JAMUP(), cleared by JAMDW(), checked by BREAK()
;			 - May  15 - PARMFL set by commands, checked by NXCMCH to prevent macro
;				     popping when last command has a null parameter string
;				     e.g. ED<EOF>
;
;		     Ted - May  13 - [FILE], [USER], [MISC]
;			 - May  18 - New [FIND]
;			   June 01 - MM version
;
;		     Tom - June 02 - Structural changes for xlating to PASM86
;				   - SRCERR implemented for search error (.ES evaluator for VPLUS)
;			   June 13 - Bug fixes, PROCMD,SDFSET
;
;		     Ted - July 21 - REPEAT, Attributes, Status line, windowing
;			   Aug. 15 - Decode table check, misc
;
;		     Tom - Aug. 19 - COPYI to move SW,PRM,TAB tables into buffer header
;				   - EQAY implemented
;			 - Aug. 20 - DIR just set RETVAL if :ED
;				   - .WE -> .EW, .WH & .WV for window coordinates
;				   - RACMD (new) for auto-execution
;			   Sept 04 - GETTXT bug:  return DX=EDTPTR when no register
;		     Ted - Oct. 13 - Update visual mode window routines
;		     Tom - Oct. 15 - Added PNTTRG(), IFCLRG() & RGOPEN() & EECMD() use PNTTRG()
;				   - UPEEB() restore RMINUS for TRACE()
;			 - Oct. 16 - RAMRGP ptr added for RAMUP0() & RAMDWN()
;		     Ted - Oct. 16 - KEYTBN+, CBINI(), EQCMD()
;			 - Oct. 25 - Rewrote V4 during a short TV commercial
;			 - Oct. 26 - HELPEX(), update cursor changed
;			 - Nov. 09 - New SETPNT, use of POSTBL
;			 - Nov. 13 - Extensive changes IO, fix |< and |>
;			 - Nov. 30 - New DECKEY, etc.
;			 - Dec. 08 - Debug T3 routines
;		     Tom - Dec. 08 - -S invoke option; TRACE bug
;
;	.DEFINE	DATE	= [12/21/86]

	.INSERT	VEDITA1		;Conditional assembly & CPU dependent assignments
	.INSERT	VCONST.ASM
	.INSERT	P86MACRO	;Macros to assist in translating to 8086

	IFNOT	P8080, [

	IFNOT	P8086, [
	.INSERT	Z80MACRO
	]

	IF	P8086, [
	.INSERT	MAC1
	.INSERT	MAC2
	]
	]

	IF	P8080, [
	.INSERT	808MACRO
	]
;
	.XLIST
	IF	LSMAIN ! LSCUST, [
	.LIST
	]
	.PAGE
	SETORG
	JMP	BEGIN		;Jump around addresses
	JMP	BREAK		;Restart editor
;
	DW	ADDTBL		;Address of pointer table for INSTALL
	DW	0000		;Multi-bit storage for INSTALL
	IF	VPLUS, [
WSABEG:	DW	TXTBUF		;Variable pointer to start of working storage
	] [
TXTBAS:	DW	TXTBUF		;Variable pointer to begin of text
	]
	DW	0000		;Reserved for future
	IFNOT	P8086, [
DSEGBG:	DW	0000		;For INSTALL compatibility w/8086 MMGT
	] [
DSEGBG:	DW	CEND
	]
EICMD0:	JMP	EICMD		;Jump for setting breakpoints
;
	IF	MEMVRS, [
SETBYT:	DB	00101001B	;29H - auto determination of CPM or MPM
				;MPM : regular polling, CPM : regular polling
	]

	IF	CRTVRS, [
SETBYT:	DB	00100001B	;21H - auto determination of CPM or MPM
				;MPM : regular polling, CPM : fast polling
				;20H = CRT version for CDOS
				;3DH = No polling, auto determination
	]

	IF	PICVRS ! TDLVRS, [
SETBYT:	DB	00101001B	;29H - auto determination of CPM or MPM
	]

EOFPAD:	DB	1AH		;EOF padding char
;
PATCH:	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
;
	DSEG	$		;For 8086 Translatability
	IF	P8086, [
	IF	CPM86, [
	DB	LF		;Fence for DEFBUF
	]
	]
;
;	Version Message for Bootup.
;
	IF	VPLUS, [
VERSMS:	DC	'VEDIT PLUS '
TRVCMD	=	VERSMS		;Trace's 'V' option needs a pointer to a 'V'
	]
	IFNOT	VPLUS, [
VERSMS:	DC	'VEDIT '
	]

	IF	DEMO, [
	DC	'DEMO '
	]

	.IFE	DEVLOP-1, [
	DC	'Development '
	]

	.IFE	DEVLOP-2, [
	DC	'Alpha Test '
	]

	.IFE	DEVLOP-3, [
	DC	'Beta Test '
	]

	DC	'Ver. '

	IF	VPLUS, [
	IF	MEMVRS, [
VRSNUM	=	203
	]

	IFNOT	MEMVRS, [
VRSNUM	=	233
	]
	]
	IFNOT	VPLUS, [
	IF	MEMVRS, [
VRSNUM	=	119
	]

	IFNOT	MEMVRS, [
VRSNUM	=	141
	]
	]

	DB	'0' + VRSNUM/100
	DB	'.'
	DB	'0' + (VRSNUM-(VRSNUM/100)*100)/10
	DB	'0' + (VRSNUM-(VRSNUM/10)*10)
	DC	'  '

	DC	'12/21/86' [00]
;
;	Console output control values
;
;	Mask 80 - Allow line wrap outside of window
;	Mask 40 - Cause ^L to clear window
;	Mask 20 - Expand CR and LF to ^M and ^J
;	Mask 10 - Expand BS to ^H		Depends upon Mask 01
;	Mask 08 - Change ESC to $
;	Mask 04 - Stop for CTRL-S in file
;	Mask 02 - Expand Tabs
;	Mask 01 - Expand CTRL chars
;
LVLCON:	DB	0BH		;Command mode commands
LVLTYP:	DB	13H		;Command mode text type
LVLLST:	DB	02H		;Printing output
LVLRT:	DB	17H		;RT command
LVLRD:	DB	00H		;RD command
LVLVCM:	DB	2BH		;Visual mode commands
LVLVTX:	DB	33H		;Visual mode text
LVLHLP:	DB	57H		;Help text
;
;	Tables of special characters used by LOOKCH.
;
	IF FULL, [
ALWTBL:	DB	' ',09H,',',';',CR,LF,EOF	;Chars not allowed in words
	DB	EOT,EOT,EOT,EOT,EOT,EOT,EOT
;
PARTB1:	DB	'.','@','!',EOT,EOT		;Begin word processor command
	]
;
	IF	VPLUS, [
SEPTBL:	DB	SPACE,TAB,';',',',':',027H,'"',CR,LF,EOF,EOT,EOT,EOT,EOT
GRPTBL:	DC	'[][()({}{<><'
	DB	EOT,EOT,EOT			;Room for another set
	DB	EOT				;GRPTBL terminator
	]
;
CMDPMT:	DC	'COMMAND: ' [00]
;
	.XLIST
	IF	P8086, [
	IF	LSMAIN ! LSCUST, [
	.LIST
	]
	.INSERT	VEDITD1			;Data Storage Area
	]

	.XLIST
	IF	LSMAIN, [
	.LIST
	]

	CSEG	$			;For 8086 translatability

	.XLIST
	IF	LSHARD, [
	.LIST
	]

	IF	MEMVRS, [
	.INSERT	VEDITM2
	.INSERT VEDITM3			;Memory Mapped
	.PRNTX	/VEDITM3/
	]
;
	IF	CRTVRS, [
	.INSERT	VEDITT3			;CRT Terminal
	.PRNTX	/VEDITT3/
	]
;
	IF	PICVRS ! TDLVRS, [
	.INSERT	VEDITP3			;PIICEON and TDL -
	.PRNTX	/VEDITP3/
	]
;
	.XLIST
	IF	LSIO, [
	.LIST
	]
	.INSERT	VEDITIO
	.PRNTX	/VEDITIO/
;
	.XLIST
	IF	LSINIT, [
	.LIST
	]
	.PAGE
;
;	NOTE: All of VEDITO1 and much of VPLUSB1 are used
;	      as the main stack space, once VEDIT PLUS is running
;
	IF	VPLUS, [
BSTACK:				;Bottom of stack used for temp storage
				;Top of stack set in VEDIT-B1
				;VPLUS stack must be 230 bytes large
PICLIN	=	. + 40		;PIICEON uses stack for line scroll buffer
				;8080/Z80 put STRBUF[] here too
	] [
OVERLY:				;VEDIT overlays VEDITO1 with variables
	]
;
	.INSERT VEDITO1		;OS dependent routines
;
	IF	VPLUS, [
	.INSERT	VPLUSB1
	.PRNTX	/VPLUSB1/
	]
	IFNOT	VPLUS, [
	.INSERT	VEDITB1
	.PRNTX	/VEDITB1/
	]
;
	.XLIST
	IF	LSFIL1, [
	.LIST
	]
	.INSERT	VEDITF1		;File Handling routines
	.PRNTX	/VEDITF1/
;
	.XLIST
	IF	LSFIL2, [
	.LIST
	]
	.INSERT	VEDITF2
	.PRNTX	/VEDITF2/
;
	.XLIST
	IF	LSC1, [
	.LIST
	]
;
	IF	VPLUS, [
	.INSERT	VPLUSC0
	]
	IFNOT	VPLUS, [
	.INSERT	VEDITC0
	]

	.INSERT	VEDITC1		;Main and Dispatcher
	.PRNTX	/VEDITC1/
;
	IF	VPLUS, [
	.XLIST
	IF	LSE1, [
	.LIST
	]
	.INSERT VPLUSE1
	.PRNTX	/VPLUSE1/
	]
;
	.XLIST
	IF	LSC2, [
	.LIST
	]
	.INSERT	VEDITC2		;Edit commands
	.INSERT	CMD.TBL
	.PRNTX	/VEDITC2/
;
	.XLIST
	IF	LSC3, [
	.LIST
	]
	.INSERT	VEDITC3		;Edit command support routines
	.INSERT VEDITC4		;Console output routines
	.PRNTX	/VEDITC4/
;
	.XLIST
	IF	LSSR, [
	.LIST
	]
;
	IF	VPLUS, [
	.INSERT	VPLUSSR		;Search routines
	.PRNTX	/VPLUSSR/
	]
	IFNOT	VPLUS, [
	.INSERT	VEDIT-SR	;Search routines
	.PRNTX	/VEDIT-SR/
	]
;
	IF	WORDP, [
	.XLIST
	IF	LSCP, [
	.LIST
	]
	.INSERT	VEDIT-CP
	.PRNTX	/VEDIT-CP/
	]
;
	.XLIST
	IF	LSTREG, [
	.LIST
	]
;
	IF	VPLUS, [
	.INSERT	VPLUSR1		;Text register routines
	.INSERT	VPLUSR2
	.PRNTX	/VPLUS-R1/
	]
	IFNOT	VPLUS, [
	.INSERT	VEDIT-R1	;Text register routines
	.PRNTX	/VEDIT-R1/
	]
;
	.XLIST
	IF	LSV0, [
	.LIST
	]
	.INSERT	VEDITV0		;Command to visual interface
	.PRNTX	/VEDITV0/
;
	.XLIST
	IF	LSV1, [
	.LIST
	]
	.INSERT	VEDITV1		;V-main and active line routines
	.PRNTX	/VEDITV1/
;
	.XLIST
	IF	LSV2, [
	.LIST
	]
	.INSERT	VEDITV2		;Visual Functions
	.INSERT	HLPDEC.TBL
	.PRNTX	/VEDITV2/
;
	IF	FULL, [
	.XLIST
	IF	LSW1, [
	.LIST
	]
	.INSERT VEDITW1		;Word Processing Functions
	.PRNTX	/VEDITW1/
	]
;
	.XLIST
	IF	LSV3, [
	.LIST
	]
	.INSERT VEDITV3		;WRTSCR, WRTLIN
	.PRNTX	/VEDITV3/
;
	.XLIST
	IF	LSV4, [
	.LIST
	]
	.INSERT VEDITV4		;Window output routines
	.PRNTX	/VEDITV4/
;
	.XLIST
	IF	LSMISC, [
	.LIST
	]
	.INSERT	VEDITG1
	.INSERT	VEDITG2		;Misc. routines
	.PRNTX	/VEDITG2/
;
	.XLIST
	IF	LSMAIN ! LSCUST, [
	.LIST
	]

	IFNOT	P8086, [
CEND:
	.XLIST
	IF	LSFIX, [	;List fixed storage
	.LIST
	]

	.INSERT	VEDITD1		;Data Storage Area

	] [

	ORG	(($ - CODBAS - 1) AND 0FFF0H) + 15
CEND:	DB	0FFH
DSGOFF	=	($ - CODBAS)/16
	]
;
	.END

