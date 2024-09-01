	.TITLE	'O1'
	.PAGE
;************************************************
;*						*
;*	CP/M-80  Dependent Initialization	*
;*						*
;************************************************
;
; Copyright (C) 1986 by CompuView Products, Inc.
;			1955 Pauline Blvd.
;			Ann Arbor, MI 48103
;
;	Written by: Theodore J. Green
;
;	Last Change: Tom - Apr. 17, 1985
;		     Ted - Mar. 19, 1986 - Move more routines in.
;			 - Apr. 02 - Delete ORGUSR and ORGDRV
;			 - June 06 - GETTOP() - use BASE+6
;			 - Oct. 10 - Set ORGDRV for status line check
;
	IFNOT	P8086, [
;
;	NOTE -	This entire module is overlayed with storage.
;		These routines can only be called from BEGIN.
;
;
; SEGINI - Define upper, middle, & lower buffer pointer values.
;
				;{BEGIN,MAKEBF(8086)}
MALLC0:				;(Expanded entry for 8086)
SEGINI:	LHLD	BASE+6		;Get BDOS entry point
	MVI	L,0		;HL-> past end of useable memory
	DCX$	H		;
	SHLD	CMDRWF		;Roof of command buffer
	LXI	B,-CMDSIZ-CMDSPR ;Reserve CMDBUF+CMDSPR bytes for command buffer
	DAD	B		;
	SHLD	CMDBAS		;
	SHLD	MIDBAS		;Base for extra edit buffers
	SHLD	CMDGET		;GET PTR
	SHLD	CMDPUT		;PUT PTR
;
	IF	VPLUS, [
	LHLD	WSABEG
	DCX$	H		;HL-> just ahead of free space
	SHLD	TXTRWF
	]
	RET
;
; GETENV - Determine environment:  MPM or CPM.
;
				;{DOSINI}
	BPROC	GETENV
GETENV:	LDA	SETBYT		;Get version setup byte
	MOV	C,A		;Save byte into C too
	ANI	02		;Look for MPM or CPM bit
	MOV	H,A		;If not auto, H=00 for CPM
	MVI	L,20H		;Assume not MPM II if not auto
	MOV	A,C		;Get byte again
	ANI	01		;Perform auto determination of CPM and MPM?
	JRZ	..1		;No, branch
	MVI	C,GETVER	;Yes, get CPM version
	CALL	BDOS
..1:	SHLD	CPMVER		;H <> 0 - MPM, H=0 - CPM
				;L=30 - MPM II, L=00 for CPM 1.4
;
	IF	MPM, [
	MOV	A,H		;Get MPM flag
	ORA	A		;Is it MP/M?
	RZ			;No, return
;
	MVI	C,45		;Yes, set BDOS error mode
	MVI	E,254		;Set to return and display mode
	CALL	BDOS
;
	LDA	CPMVER		;Get version byte
	CPI	30H		;Is it MP/M II?
	RNZ			;No, return
	MVIB$	MPMIN+3,0FDH	;Yes, set MP/M-II input for BDOS #6 call
	]			;<IF MPM>
	RET
	EPROC	GETENV
	.PAGE
	]			;<NOT P8086>
;
; SAVUSR - Save Drive/User # at time of invocation.
;
				;{DOSINI}
SAVUSR:	CALL	GETDRV		;Get & save currently logged in drive
	STA	DEFDRV		;The current default drive
	STA	ORGDRV		;Save original drive for status line check
	CALL	GETUSR		;A = user #
	STA	DEFUSR
	STA	CURUSR
	RET
;
; GETDRV - Return DEFDRV and A = currently logged in drive, A=1, B=2, etc.
;
				;{SAVUSR}
GETDRV:	MVI	C,INTDSK	;Code to get logged-in drive
	CALL	BDOS		;A=0, B=1, etc
	INR	A		;A=1, B=2, etc
	RET
;
; GETUSR - Return A = current user #.
;
				;{SAVUSR}
GETUSR:	MVI	C,CPMUSR	;SET/GET User Code function
	MVI	E,0FFH		;GET
	JMP	BDOS		;Get user # (0 for CP/M 1.4 & lower)
