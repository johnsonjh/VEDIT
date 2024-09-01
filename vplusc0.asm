	.TITLE	'C0'
	.PAGE
;************************************************
;*						*
;*	  Command Mode ENTER/BREAK/EXIT		*
;*						*
;************************************************
;
; Copyright (C) 1987 by CompuView Products, Inc.
;			1955 Pauline Blvd.
;			Ann Arbor, MI 48103
;
;	Written by: Theodore J. Green
;
;	Last Change: Tom - Oct. 05, 1985 - 2.32 changes
;			 - Oct. 27, 1986 - SBREAK(), BREAKC(), BRKC1(), BREAK
;		     Ted - Nov. 28 - CBREAK() to handle MSDOS CTRL-C trapping
;		     Tom - Dec. 10 - PUSH/POP DE in BREAK()
;		     Ted - Mar. 21, 1987 - BRK8() calls MACRST()
;			 - Apr. 05 - Macro errors clear EXCBUF
;			 - Apr. 08 - BREAK() - new entry point MSBR8K
;			 - Apr. 18 - New use of WWMODE
;
;
; TOCMD - Enter command mode.
;
;	  May be branched to anywhere from within command mode to reset
;	  stack.  Presumes GET/PUT are correct.
;
				;{BEGIN, EXTCHK, BR8K, CLSERR, RESTRT, VMAC}
TOCMD:	LXI	SP,STACK	;Make sure stack set correctly
;	JMP	MAIN		;Do editing
;
;	MAIN - Begin editing.
;
MAIN:	CALL	PROCMD		;Process commands forever
	JMPR	MAIN
;
; FRMCMD - Exit to operating system.
;
				;{EXTCHK}
EXITV:				;{VEDIT LABEL}
FRMCMD:	CALL	VEXIT		;Reset hardware, position cursor bottom line
	JMP	EXIT

	.PAGE
;
; FNFBRK - Display "FILE NOT FOUND:  d/u#:filename" then BREAK.
;
				;{HCMD,EGCMD,EKCMD,ERCMD,RLCMD}
	BPROC	FNFBRK
FNFBRK:	LXI	H,FNFMS2	;HL-> FNF msg
	LDA	WWMODE		;;Get window mode
	DCR	A		;;Visual mode?
	JP	MSGBRK		;Yes, BREAK
	LXI	H,FNFMSG	;No, HL-> FNF:  msg
	MVI	A,80H
	CALL	MSGHND
	LHLD	CURFCB		;HL-> FCB of last open attempt
	PUSH	H		;
	CALL	PRTDNM		;Display drive & user #
	POP	H		;
	CALL	PRTFCB		;Display filename
..1:	LXI	H,NULMSG
	MVI	A,40H		;Display CR,LF
	JMPR8	BRK0		;and abort
	EPROC	FNFBRK
;
; MACBRK - Cause break after printing DE's message, macro #, MACRO ERROR
;
				;{JPCMD,NXCMCH,POPCMD}
MACBRK:	CALL	INSEXN		;Insert REGEXN into 2d error message
	XCHG			;HL->1st msg, DE-> 2d
	MVI	A,0DAH		;Code for MSGHND()
	JMPR	BRK0

				;{CHKEXE}
MACBR1:	CALL	INSEXN		;Insert REGEXN into message
	JMPR	MSGBRK
;
				;{MACBRK,MACBR1}
INSEXN:	LDA	REGEXN		;Get macro number/letter
	CALL	RTOASC		;Convert to ASCII
	STA	MCRMSG+17	;Store into MACRO ERROR message
	CLR$	EXCBUF		;;Disable auto-execution
	LXI	H,MCRMSG	;HL-> MACRO ERROR message
	RET
;
BUFBRK:	LXI	H,ILGOP		;Register insertions illegal for 
	JMPR	MSGBRK		;edit-registers
;
PRMBRK:	LXI	H,PRMMSG	;HL-> BAD PARAMETER
	JMPR	MSGBRK		;Give error
;
QRNBRK:	LXI	H,ILLQRN
	JMPR	MSGBRK
;
;
; VALBRK - Display 'UNKNOWN INTERNAL VALUE "x" ' then BREAK.
;
				;{TSTVAL via DISPAT}
VALBRK:	LDA	EVLCHR		;Get current valuator char
	LXI	D,ILVMSG	;DE -> message
	LXI	H,24		;Offset to byte to be packed
;
; PAKBRK - Pack A into DE's message @ OFFSET HL, display and BREAK.
;
				;{VALBRK^,JCMD}
	BPROC	PAKBRK
PAKBRK:	PUSH	D		;Save msg ptr
	DAD	D		;HL-> byte to be packed with %A
	CPI	SPACE		;Control char?
	BGE	..2		;No, branch
	MVI	M,'^'		;Yes, make visible
	INX$	H
	ADI	'A'-1

..2:	MOV	M,A		;Insert char into msg
	INX$	H		;Bump pointer
	MVI	M,'"'		;Insert end quote and msg termination bit
	INX$	H
	MVI	M,0
	POP	H		;HL-> msg start
	MVI	A,80H
	CALL	MSGHN7
	LXI	H,BRKMSG
	MVI	A,0D1H		;2 msgs, separated, CRLF following last msg
	JMPR	BRK0
	EPROC	PAKBRK
;
;
;  B R E A K - Aborts any execution and returns to command/visual mode.
;	       This routine changes as often as governments in Italy.
;	       Stack is reset, so don't worry about stack contents.
;
	IF	MSDOS, [
	PUBLIC	SBREAK
SBREAK:	MVI	C,19H		;Vis a vis Technical warning for INT 24H
	CALL	BDOS		;get current disk, a function > 12
	JMPR	BRKC1
;
;	Entry from DOS CTRL-C trap which puts ^C on screen.
;
CBREAK:	CALL	STNSNL		;;Make sure screen rewritten
	CALL	STBRFL		;;Make sure status line rewritten
	MVIB$	WWZMFL,81H	;;Set auto-zoom flag (forces rewrite)
	]
	
	BPROC	BREAKC
BREAKC:	TST$	LSOPFL		;Printing?
	CNZ	CRLF		;Some printers won't flush buffer w/o CRLF
				;(BRKCHK occurs before the CR)
;
BRKC1:	LDA	WWMODE		;;Get window mode
	DCR	A		;;Visual mode?
	JRM	BREAK		;;No, give BREAK msg
	LXI	H,NULMSG	;Yes, don't bother flashing msg on status line
	XRA	A
	JMPR	BRK0
	EPROC	BREAKC
;
	BPROC	BREAK
BREAK:	LXI	H,BRKMSG	;HL-> BREAK mesage
MSGBRK:	MVI	A,0D0H
BRK0:	LXI	SP,STACK	;Stop stack overflowing on #<ctrl-c>'s
	PUSHA			;Save MSGHND parameter
	PUSH	H		;Save -> message
	PUSH	D		;;Save -> possible 2d msg
	CALL	STADON		;Make sure status line turned off
	CALL	ATTFOR		;;Reset to text attribute
	CALL	RSTCON		;Turn off listing, display to console
	CALL	PURGEK		;Purge any pending key strokes
;
	TST$	RAMFL		;Text register out of position?
	CNZ	RAMUP
;6	CALL	RSTSEG
	TST$	JAMFL		;Edit buffer out of position?
	CNZ	JAMDWN
;
	CLR	RIFLG		;No longer RI command
	STA	TRACEF		;Turn trace mode off
	STA	GETDCF		;GETDEC no longer running
	STA	CNCLFL		;Clear CANCEL flag
;
	IF	MEMVRS, [
	CALL	HCRSOF		;;Turn off any hardware cursor
	]
	IF	MSDOS, [
	LDA	WWNAME		;;Get current window #
	CALL	SWWIND		;;Let auto-zoom flag rewrite screen
	]
;
	CALL	CHKBOT		;Restore char before [TXTFLR] if "-F"
	IF	MPM,[
	CALL	CLSAUX		;Close AUXFCB
	]
;
;	Clear Search string if waiting for terminator.
;
	TSTM	SRCHFL		;Waiting for terminator?
	MVI	M,0		;Clear flag
	JRZ	..1		;No, branch
	MVIB$	TARGST,SRCEOS	;Clear search string
;
..1:	POP	D		;DE-> possible 2d msg
	POP	H		;HL-> primary msg
	POPA			;A = message control byte for MSGHND
;
				;{CMBR8K,ERRHND}
MSBR8K:	CALL	MSGHND		;;Display message with desired processing
;
; BR8K - Finish BREAKing out to command main.
;
				;{BREAK^,VESC}
BR8K:
;6	CALL	RSTSEG		;Restore data segment to last EE selection
	CALL	RESTOP		;Restore top of window if not already
	CALL	SETTOP		;Make sure text window set
	CLR$	ITRSTK		;Make sure no iterations executing
	CALL	MACRST		;;Kill any keystroke macro
				;;Prevents unexpected chars in text - [FIND]
	IF	FULL, [
	TST$	INXFLG		;In the midst of booting up w/unexecuted init file?
	JRZ	..81		;No, skip
	CLR$	INXFLG		;Yes, clear flag
	MOVW	CMDGET,CMDPUT	;!
	CALL	LODEXE		;Load init file into RZ
	CALL	MCMD1		;Set GET/PUT -> RZ
	JMP	TOCMD		;Go process it
..81:
	]
;
;	Handle visual mode break.
;
	LDA	WWMODE		;;Get window mode
	DCR	A		;;Visual mode?
	JRM	..9		;No, branch
	TSTM	VISFLG		;True visual mode?
	MVI	M,0		;Clear in any case
	JRZ	..82		;No, visual macro executing.  Go pop it
	LES	H,CMDGET	;Half-way to pseudo command mode?
	MOV%ES	A,M
	ANI	5FH		;Handle lower case and 8-th bit V's
	CPI	'V'		;VISBAK's V?
	JRZ	..BEND		;Yes, ok to re-enter visual mode
	CALL	BAKGET		;No, backup to previous command letter
	JMPR	..85
;
;	Temporary command mode break.
;
..82:	CALL	POPCMD		;Get back to 'V' command
	JRC	..9		;Can't, branch
	LES	H,CMDGET	;ES:HL-> command
..85:	MOV%ES	A,M		;Get it
	ANI	5FH		;Strip top bit
	CPI	'V'		;Visual mode cmd?
	BNE	..82		;No, keep looking
	JMPR	..BEND		;Yes, go reenter visual mode
;
..9:	CALL	NEWCMD		;Get next command, init pointers
..BEND:	JMP	TOCMD		;Reset SP and restart command mode
	EPROC	BREAK
