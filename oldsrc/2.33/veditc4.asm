	.TITLE	'VEDIT-C4'
	.PAGE
;************************************************
;*						*
;*	Console Input / Output Routines		*
;*						*
;************************************************
;
; Copyright (C) 1986 by CompuView Products, Inc.
;			1955 Pauline Blvd.
;			Ann Arbor, MI 48103
;
;	Written by: Theodore J. Green
;
;	Last Change: Ted - Oct. 29, 1985 - Combine output routines
;			 - Jan. 29 - PRTDEC uses ITOA()
;			 - Feb. 04 - ++ direction for command buffer
;			   Feb. 18 - CLR WWMODE at CRLF()
;			   Mar. 08 - Use LSTOUT stack
;
;		     Tom - Mar. 08 - PRTNUM, BC displayed in free format
;				   - PRTNMA:  display A in free format
;				   - PRTDEC, PRTDC1 save BDH
;		     Ted - July 18 - OUTPMT change
;			   Aug. 22 - PREPLY fix
;		     Tom - Sep. 22 - MSGHND+
;			   Sep. 25 - MSGHND()
;		     Ted - Oct. 17 - MSGHND calls GETCHR; fix attributes
;		     Tom - Oct. 19 - PRTCHL(), PRTNUM(), PRTNMA()
;		     Ted - Nov. 13 - LSTSET strips high-bit of INPFLG
;			 - Dec. 03 - ENDLST detach only if ROUTAD != LSTOUT
;
; MSGHND - Display HL/DE's message, terminated by B/C as determined by
;	   @A and (WWMODE).  May return char in A.		[9/30/86]
;
;	   A's codes in hex:
;		80: WWMODE = 0:  precede message with CRLF
;		40: WWMODE = 0:  follow message with CRLF
;		20: WWMODE all:  Message, get/return keyboard char in A
;		10: WWMODE <>0:  Message, wait for keystroke
;		08: 2 messages:  HL-> 1st, DE-> 2d
;		04: HL's message terminated by B, DE's by C else by 0.
;		    Note:  when WWMODE is nonzero, HL's must be zero terminated
;		02: WWMODE <>0:  2d message w/same attribute as 1st
;		01: WWMODE <>0:  Special processing.  Message is being
;				 inserted before the previous message
;
;		Note:  leading CR,LF's are stripped in visual mode
;
	DSEG	$
	PUBLIC	MSGCOL
MSGCOL:	DS	1
MSGPRM:	DS	1
MSGHOR:	DS	1
	CSEG	$
				;{BREAK,ERRHND,PREPLY,OPNIOX...}
	BPROC	MSGHND
MSGHND:	STA	MSGPRM		;Save message parameter-byte
	CLR$	MSGCOL
	JMPR	MSGHN0

				;{JPCMD}
MSGHN7:	STA	MSGPRM
	MVIB$	MSGCOL,9

MSGHN0:	TST$	WWMODE		;Visual mode?
	JRZ	..CMD		;No, branch
;
;	Display on status line.
;
	PUSH	D
;6	PUSH	CX
	LDA	MSGCOL
	CALL	STASET		;Opens status line, address to column in %A
				;Setup for message attribute
	MVI	B,0
	CALL	PSKIPL		;Display msg, skipping leading <cr>,<lf>.
;6	POP	CX
	POP	D
	ANIB$	MSGPRM,0AH	;Another message?
	JRZ	..S1		;No, branch
	ANI	2		;Same attribute?
	CZ	ATTBCK		;No, background status line attribute
	XCHG			;HL-> 2d msg
	MOV	B,C		;B = possible terminator
	ANIB$	MSGPRM,04H	;Terminator supplied?
	JRNZ	..S0		;Yes, use it
	MVI	B,0		;No, use 0
..S0:	CALL	PSKIPL		;Display 2d message

..S1:	ANIB$	MSGPRM,1	;Special processing?
	JRNZ	..S2		;Yes, branch
	MOVB	MSGHOR,PHYHOR	;Save for special processing section
;
	IF	CRTVRS, [
	CALL	OUTATT		;Make sure CRT set to attribute
	]
;
	CALL	WINEOL		;;Erase rest of status line
	JMPR	..S3
;
;	Special processing:  insert *BREAK* ahead of previous msg.
;
..S2:	CALL	PSPACE		;Space over to start of previous msg
	CALL	PSPACE
	LDA	MSGHOR		;Move cursor past previous msg
	CALL	ADDSTA

..S3:	ANIB$	MSGPRM,30H	;Get key  or pause?
	JRZ	..SEND		;No, branch
;
;	Wait for key press.
;
	CALL	GETCHR		;Get single key/sequence
	MOV	A,C		;Make sure char is in A

..SEND:	PUSHA			;Save any input char
	CALL	STADON		;Close status line
	POPA
	RET
;
;	Display in command mode.
;
..CMD:	ANIB$	MSGPRM,80H	;Precede with CRLF()?
	CNZ	CRLF
	ANIB$	MSGPRM,04H	;Terminator supplied in B?
	JRNZ	..C1		;Yes, branch
	MVI	B,0		;No, use 0
..C1:	CALL	PRTST1

	ANIB$	MSGPRM,08H	;2d message?
	JRZ	..GCKY		;No, branch
	XCHG			;HL-> 2d msg
	MOV	B,C
	ANIB$	MSGPRM,04H	;Terminator supplied?
	JRNZ	..C2		;Yes, branch
	MVI	B,0		;No, use 0
..C2:	CALL	PRTST1

..GCKY:	ANIB$	MSGPRM,20H	;GETKEY()?
	CNZ	GETKEY		;Yes, get keyboard char
	PUSHA
	ANIB$	MSGPRM,40H	;Send CRLF?
	CNZ	CRLF
	POPA
	RET
	EPROC	MSGHND
;
; PSKIPL - Print HL's string which is terminated by char in B after
;	   skipping over any leading <cr>'s and <lf>'s.
;
				;{MSGHND}
	BPROC	PSKIPL
PSKIPL:	MOV	A,M		;Get leading char
	CPI	CR		;CR?
	BEQ	..CHKB
	CPI	LF		;No, LF?
	JNZ	PRTST1		;No, print the string
;
..CHKB:	CMP	B		;Be safe
	RZ			;Return if end of string reached
	INX$	H		;Skip leading CR or LF
	JMPR	PSKIPL		;Continue checking
	EPROC	PSKIPL
;
; TYPLST - Type out the text between (HL) and (DE).
;
				;{TCMD,PRCMD}
TYPLST:	CALL	POSDIF		;Set HL > DE, BC = HL-DE
	XCHG			;HL-> type begin
;6	MOV	AX,DS
;6	MOV	ES,AX
;
; PRTLEN - Print string at ES:(HL)+ for %BC characters.
;	   Use Reg. E as mask for stripping 8th bit on chars.
;
				;{RTCMD,TYPLST^}
	BPROC	PRTLEN
PRTLEN:	MVI	E,0FFH		;Don't strip 8th bit
	JMPR	PRTLE0		;Merge
				;{PRTFCB}
OUTFLD:	MVI	E,07FH		;Mask = 7FH to strip directory attr. bit
;6	MOV	AX,DS
;6	MOV	ES,AX
PRTLE0:	MOV	A,C		;Is count done?
	ORA	B
	RZ			;Yes, return
	MOV%ES	A,M		;Get next char
	ANA	E		;Strip bit 7 (Directory attr.)
;6	PUSH	ES
	CALL	PCHARA		;Output to the console
;6	POP	ES
	INX$	H		;Bump PTR
	DCX$	B		;Decrement count
	JMPR	PRTLE0		;Loop until done
	EPROC	PRTLEN
;
; PRTFCB - Print a file name <- HL.
;
	IFNOT	MSDOS, [
				;{EWCMD,STAFIL,DIR,FNFBRK,PRTFNM}
PRTFCB:	INX$	H		;Advance past drive name
	LXI	B,8		;Output file name
	CALL	OUTFLD		;
	MVI	A,'.'		;Output a dot
	CALL	PCHARA		;
	LXI	B,3		;Output extension
	JMPR	OUTFLD		;
	]
;
; PRTCHL - Print the 17-bit value in CHL in decimal.
;
	IF	VPLUS, [
				;{XTCMD1 et al, CALC}
	BPROC	PRTCHL
PRTCHL:	CALL	CHKCHL		;Set sign flags
	MVI	A,SPACE		;Presume positive number
	JP	..1		;Branch if positive #
	CALL	CHGSGN		;Convert negative to positive
	MVI	A,'-'		;Get minus sign
..1:	MOV	B,H
	MOV	C,L		;BC = 16-bit value
	JMPR	PRTNUM		;Print the number
	EPROC	PRTCHL
;
; PRTNUM - Print the number in BC in decimal, left justified if
;	   RMINUS='-', else right justified.
;	   %A=SPACE specifies positive number, '-' specifies negative.
;	   (Both cases actively assumed in code below)
;	   Registers BDH saved.
;
				;{PRTNMA,PRTCHL^}
	BPROC	PRTNUM
PRTNUM:	CALL	SV%BDH
	PUSHA			;Save sign
	CALL	ITOA		;HL-> ASCII-converted number
	POP	D		;D=numeric sign for Z80
	IF	P8086, [
	MOV	D,E		;Now for 8086 as well
	]
	MVI	A,SPACE		;
	CMP	M		;Blank 1st char?
	BNE	PRTSTR		;No, display number (any '-' is lost)
	PUSH	H		;Yes, save right-justified start of number
..2:	INX$	H		;Skip over blank
	CMP	M		;Next char blank?
	BEQ	..2		;Yes, keep skipping
	CMP	D		;Positive number?
	BEQ	..3		;Yes, branch
	DCX$	H		;No, backup to final blank
	MOV	M,D		;Insert minus sign at start of string
..3:	POP	D		;DE-> start of right-justified string
	CPIB$	RMINUS,'-'	;Left justified?
	BEQ	PRTSTR		;Yes, display number
	XCHG			;HL-> right-justified string
	JMPR	PRTSTR		;Display it
	EPROC	PRTNUM
;
; PRTNMA - Display decimal number in A on the console in free format.
;	   Registers BDH saved.
;
PRTNMA:	PUSH	B
	MOV	C,A
	MVI	B,0		;BC = number to display
	MVIB$	RMINUS,'-'
	MVI	A,SPACE		;Specifie positive integer
	CALL	PRTNUM		;Display it
	POP	B
	RET
	]			;<IF VPLUS>
;
; PRTDEC - Print the number in BC in decimal, right justified.
;	   Registers BDH saved.
;
				;{UCMD,RUCMD,BRDNUM,PRTCHL+^}
PRTDEC:	CALL	SV%BDH
	CALL	ITOA		;Convert number to ASCII <- (HL)
	JMPR	PRTSTR		;Send to output routine
;
				;{BRDNUM}
PRTDC1:	CALL	SV%BDH
	CALL	ITOA		;Convert number to ASCII
	INX$	H		;Skip over two blanks
	INX$	H
;	JMPR	PRTSTR		;Send to status line
;
; PRTSTR - Print string at DS:(HL) until 00 (char. in B) is found.
;
				;{WRTMSG,PRTMSG,PRTNUM,PRTDEC,PRTBNM,PROMPT, cmds, BS,CR,LF}
PRTSTR:	MVI	B,00		;End of message char

				;{MSGHND}
PRTST1:
;6	MOV	AX,DS
;6	MOV	ES,AX

				;{XOSCMD msdos}
PRESTR:	MOV%ES	A,M		;Get next message char
	CMP	B		;Is it end?
	RZ			;Yes, return
;6	PUSH	ES
	CALL	PCHARA		;Print it
;6	POP	ES
	INX$	H		;Bump PTR
	JMPR	PRESTR		;Continue
;
; PREPLY - Print message at (HL), get user reply, check for 'Y'.
;	   Return: 'Z' if 'Y'.
;
PREPLY:	MVI	A,70H		;Get keyboard char, follow with CRLF
	CALL	MSGHND		;Display message, get char
	ANI	05FH		;Change lower to upper case
	CPI	'Y'		;Is reply affirmative?
	RET
;
;
; PRTFNM - Print full output filename including drive & user #.
;
	IF	VPLUS, [
	PUBLIC	PRTFN1
				;{EXCMD,EECMD}
PRTFNM:	TST$	OUTFLG		;Output file open?
	RZ			;No, return
	LXI	H,RENFCB	;
				;{ERCMD/EWCMD}
PRTFN1:	PUSH	H
	CALL	PRTDNM		;Display drive & user #
	POP	H
	JMP	PRTFCB		;Display filename
;
; PRTBNM - Convert buffer number in A to ASCII & display 'BUFFER a' on console.
;
				;{EECMD,UCMD}
PRTBNM:	PUSHA			;Save buffer name/number
	LXI	H,BUFRMS	;HL-> 'BUFFER '
	CALL	PRTSTR		;
	POPA			;Retrieve buffer name/number
;
; PRTBN0 - Convert buffer number in A to ASCII & display on console.
;
				;{PRTBNM^,EKONAM,RUCMD,SHWUSR}
PRTBN0:	CALL	RTOASC		;Convert to ASCII
	JMP	PCHARA		;Display on console
;
; OUTPMT - Send prompt string from command line to console, setting
;	   LOGPOS = 1 for line-edited input to follow.
;
				;{RQCMD,XQCMD,XKCMD}
OUTPMT:	CALL	YTCMD0		;Issue the prompt string
	JMPR	RSTPOS		;Set LOGPOS = 1 so <bs> will behave properly
	]			;<IF VPLUS>
;
; PCRMSG - Print a CRLF, the message (HL) and another CRLF.
; PRTMSG - Print a message and then CRLF.
; CRLF - Print a CR and a LF.
;
PCRMSG:	CALL	CRLF		;Print initial CRLF
PRTMSG:	CALL	PRTSTR		;Print the message
;	JMP	CRLF
;
; CRLF - Send CR and LF to console.
;
				;{PCRMSG^ and you name it}
CRLF:	PUSHA			;All regs will be saved
	MVI	A,CR		;Get a CR
	CALL	PCHARA		;Send to console
	MVI	A,LF		;Get a LF
	CALL	PCHARA		;Send to console
	POPA			;Restore
	RET
;
; IFCRLF - Output CR,LF unless colon parameter has been specified.
;
				;{ERCMD,EWCMD,XTCMD,YDCMD,CALC+}
IFCRLF:	TST$	COLFLG		;Suppress CRLF?
	RNZ			;Yes, return
	JMPR	CRLF		;Else CRLF
	.PAGE
;
;	LSTBLK - Print block of text from (DE) to (HL)
;
				;{PRCMD,VPRINT}
LSTBLK:	CALL	LSTSET		;Setup ROUTAD and LSTFLF for printing
	CALL	TYPLST		;Print the text out
	JMPR	ENDLST		;Reset ROUTAD, LSTFLG and LOGPOS
;
;	LSTSET - Set ROUTAD to printer and attach LIST device
;
				;{RPCMD,YPCMD,LSTBLK}
LSTSET:	CALL	SV%ALL		;Save all registers
	TSTM	LSOPFL		;Is LIST already attached?
	MVI	M,1		;Set flag
	CZ	ATTLST		;No, attach list device, give prompt
	LDA	LVLLST		;New LSTFLG value
	LXI	H,LSTOUT	;New ROUTAD value
	CALL	SETOUT		;Set new output routine values
				;This also saves old ones
	LDAM	INPFLG		;;Get input decode flag
	ANI	07FH		;;Allow CTRL-C to abort printing
	MOV	M,A		;;Set new INPFLG
RSTPOS:	MVIW$	LOGPOS,1	;Ensure that Tabs expand properly
				;Note that previous LOGPOS is saved on stack
	RET
;
; RSTCON - Turn off any listing and restore output routine to console.
;	   First setup the "saved" values, then restore from "saved" values
;
				;{ERRCON,BREAK,NEWCMD,ERRHND}
RSTCON:	CLR$	LSTSTK		;Empty the listing stack
				;This will force default values at RSTOUT()
;	JMP	ENDLST		;Turn off any listing, set LSTFLG and ROUTAD
;
; ENDLST - Reset to previous LSTFLG and ROUTAD, detach printer
;	   Regs: Save HL, DE and BC.
;
				;{RPCMD,YPCMD,RSTCON^,LSTBLK}
	BPROC	ENDLST
ENDLST:	CALL	SV%BDH		;;Save regs
	CALL	RSTOUT		;Restore saved ROUTAD, LSTFLG and LOGPOS
	CPIW	ROUTAD,LSTOUT	;;Still routed to printer?
	BEQ	..1		;;Yes, don't detach!
;
	IF	MEMVRS,[
	CALL	HCRSOF		;Turn hardware cursor off
				;Turned on at ATTLST()
	]
;
	TSTM	LSOPFL		;Is list flag set?
	MVI	M,00		;Clear flag
	JRZ	..1		;No, branch
	TST$	WWMODE		;In visual mode?
	CZ	CRLF		;No, give CRLF to end printing message

	IF	MPM, [
	CALL	MPMCHK		;Is this MP/M?
	JRZ	..1		;No, branch
	MVI	C,159		;Detach list device
	CALL	BDOSSV		;Allow other processes to use list
	]

..1:	RET
	EPROC	ENDLST
	.PAGE
;
;	SETLST - Set new LSTFLG from value in A.
;	SETOUT - Set new LSTFLG from value in A and new ROUTAD from HL.
;		 Current values and LOGPOS are saved on stack.
;
				;{CMDSTR,PRCMD,RTCMD,TCMD,YDCMD,HELPEX}
SETLST:	LHLD	ROUTAD		;Get current value
				;{TRACE,YICMD,LSTSET,VEDCMD,ADDSTA}
SETOUT:	PUSH	H		;Save regs
	PUSH	B
	PUSHA
	LXI	H,LSTSTK	;HL-> listing stack
	LBCD	ROUTAD		;Save current ROUTAD
	LDED	LOGPOS		;Save current LOGPOS
	LDA	LSTFLG		;Save current LSTFLG
	CALL	PSHCHK		;Push values on stack
				;Does not return if overflow error
	POPA
	POP	B
	POP	H
SETOU1:	SHLD	ROUTAD		;Set new ROUTAD
	STA	LSTFLG		;Set new LSTFLG
	RET
;
; RSTOUT - Restore ROUTAD and LSTFLG to saved values.
;	   If restored ROUTAD is different from current, LOGPOS is also restored.
;	   Regs: HL, DE and BC saved
;
				;{TRACE,CMDSTR,RTCMD,TCMD,YDCMD,YICMD,
				; HELPEX,ENDLST,STADON}
	BPROC	RSTOUT
RSTOUT:	CALL	SV%BDH		;Save regs
	LXI	H,LSTSTK	;HL-> listing stack
	CALL	POPSTK		;Pop the stack to restore previous values
	JRC	..1		;Use command mode values if stack empty
	PUSH	D		;Save LOGPOS
	MOV	B,A		;Save LSTFLG
	LDED	ROUTAD		;+Get current ROUTAD
	CALL	CMHLDE		;Same?
	MOV	A,B		;A = LSTFLG
	POP	D		;DE = LOGPOS
	JRZ	SETOU1		;Yes, don't restore LOGPOS
	SDED	LOGPOS		;+Restore previous LOGPOS
	JMPR	SETOU1		;Restore ROUTAD and LSTFLG
;
..1:	LXI	H,WINOUT	;Use console output to window
	LDA	LVLCON		;Value for command mode commands
	JMPR	SETOU1		;Set default values for ROUTAD and LSTFLG
	EPROC	RSTOUT
	.PAGE
;
; BRKCHK - Check for console BREAK and CTRL-S pause.
;	   Performs all Polling in Command Mode.
;
				;{PROCMD+,DISPAT-,SCMD-,CHKGLB,DECITR,PRTCR}
BRKCHK:	CALL	SV%BDH		;Save regs
;
	CALL	CONST		;Char ready?
	RZ			;No, return
	CALL	KBFCHK		;Yes, but does keyboard buffer have room?
	RZ			;No, return
	CALL	DECKEY		;Yes, get the single char
	RC			;;Ignore key if undefined function
;
	MOV	A,C
	CPI	CTRLS		;Is it CTRL-S?
	JRNZ	BRKCH1		;No, branch
				;{PCHAR}
CTRLSS:	CALL	DECKEY		;Yes, get next keyboard char
	MOV	A,C
	CPI	CTRLC		;Is it CTRL-C?
	BEQ	JBREAK		;Yes, break out
	RET			;No, ignore this char
;
BRKCH1:	CPI	CTRLC		;Is it CTRL-C console break?
	BEQ	JBREAK		;Yes, break out
	TST$	CNCLFL		;Or [CANCEL]?
	JZ	PSHCHR		;No, save char in buffer
;
JBREAK:	JMP	BREAKC		;Perform BREAK

