	.PAGE
	.TITLE	'VEDIT-V0'
;************************************************
;*						*
;*	Command / Visual Interface		*
;*						*
;************************************************
;
; Copyright (C) 1986 by CompuView Products, Inc.
;			1955 Pauline Blvd.
;			Ann Arbor, MI 48103
;
;	Written by: Theodore J. Green
;
;	Last Change: Tom - Jan. 10 - 2.32a - TOVIS (clear DSKFUL)
;			 - Mar. 01 - FRMVIS (for .PREL assm'y)
;		     Ted - Mar. 09 - Windows, UPDWIN()
;			   Apr. 28 - VSHELP changes for windows
;			   May  04 - VMACCK changes
;			   May  15 - Changes for new [REPLACE]
;			   Aug. 15 - Check that decode table is valid
;			   Sep. 18 - VISBAK(), VMACCK() use ES:
;			   Oct. 24 - Change stack control at VEND, VMAC, etc.
;			   Oct. 29 - Skip FRMVIS() if WWUPFL is set
;			   Nov. 05 - TOVIS2 - Prevent CURVER = 0 problems
;			   Nov. 06 - Change VMACC1 to PAUSE, also calls MACRST()
;			   Nov. 09 - Handle continuation in single line window
;		     Tom - Nov. 25 - DSKFUL clearing moved to ECMD
;		     Ted - Dec. 04 - VSHELP() changes
;
; VEDCMD - Sets the screen beginning, active line and
;	   cursor positions from the command edit pointer.
;
	BPROC	VEDCMD
VEDCMD:	SSPD	VSTACK		;Save current stack value for VLOOP/VDONE
				;8080 - OK to destroy HL
	IF	MEMVRS, [
	CALL	HCRSOF		;Out damn spot
	]
;
;	Make sure that the decode table is a real one
;
	MVI	A,REGKEY	;Register number for KEYTBL
	CALL	PNTRG0		;ES:DE-> begin of KEYTBL
	INX$	D		;;Skip KFF
	LXI	H,KFF*256+CR	;;Load bytes 2 and 3 in table
	XCHG			;HL-> KEYTBL
	IFNOT	P8086, [
	CALL	CMINDE		;Is the table valid?
	] [
;6	CMP	EWP [BX],DX	;Is the table valid?
	]
	LXI	H,NOTVMS	;HL-> error message
	JNZ	MSGBRK		;No give an error and BREAK
;
	CPIB$	REPLFL,2	;In middle of Replace "Rest"?
	JZ	VFNEXE		;Yes, branch to handler
				;Jumps back to VEDCM1 after last replace
VEDCM1:
;
;	Switch to visual mode window
;
	IF	WINDOO, [
	TST$	WWUPFL		;Is this only a window update?
	JRNZ	..1		;Yes, branch
;
	MVIB$	COMMWI,'$'	;No, goto "$" window on exit
	CALL	VMACCK		;Pause if end of VMAC execution
;
..1:
	IF	VPLUS, [
	CALL	EDTNAM		;;A = edit buffer name
	] [
	MVI	A,'@'		;Use window @ if open
	]

	PUSHA			;Save edit buffer name
	CALL	SWWIND		;Switch to visual mode window
	TST$	WWMODE		;Is this window in visual mode?
	CZ	STNSNL		;No, ensure window rewritten
	POPA
	STA	WWMODE		;Save ASCII buffer # in window structure
	]			;<IF WINDOO>
;
	CALL	WISCST		;Set HZSCBG from VSHZBG
;
	IF	WINDOO, [
	CALL	UPDRTC		;Remove any update cursor
	]
;
	CLR	CNCLFL		;;CNCLFL may have been set during VSHELP
	INR	A		;A = 1
	STA	VISFLG		;Set visual mode flag
;
	IFNOT	WINDOO, [
	STA	WWMODE		;Window used for visual mode
	]
;
	LDA	LVLVTX		;New value for LSTFLG
	LXI	H,WINOUT	;Use window handler
	CALL	SETOUT		;Set new values, save old ones
	MVIB$	INPFLG,80H	;Enable visual function decode
	LHLD	EDTPTR		;HL = command edit pointer
;
				;{CRHOME,CRZEND,PVPARA,NXPARA,VFIND,VJUMP}
TOVIS0:	ANIB$	NWSCFL,80H	;Is visual mode display up to date?
	JRNZ	TOVIS2		;No, branch
;
;	Check if edit point is still on screen
;
	LD16$	B,CRVTTP	;C = cursor top line, B = bottom
	INR	B		;First line not allowed
	INR	B		;Adjust
	PUSH	H		;Save edit point
	LDED$	TXTFLR		;DE -> begin of buffer
	LES	H,SCRFCB	;HL -> table
	MVHL%ES			;HL-> text for first line
	CALL	CMHLDE		;Is screen at begin of buffer?
	POP	D		;DE = edit point
	JRNZ	TV1		;No, branch
	MVI	C,1		;Yes, allow cursor on line 1
;
TV1:	MOV	A,C		;Get top cursor line
	CALL	FCBADD		;HL-> begin of line
	XCHG			;HL = edit point
	CALL	CMHLDE		;Edit before first screen line?
	BLT	TOVIS2		;Yes, write new screen
TV2:	INR	C		;Look at next line
	MOV	A,C		;Put in A
	CMP	B		;Past last line?
	BGE	TOVIS2		;Yes, write new screen
	XCHG			;DE = edit point
	CALL	FCBADD		;HL-> begin of line # %A
	XCHG			;HL = edit PTR
	CALL	CMHLDE		;Is edit PTR after this line?
	BGE	TV2		;Yes, keep looking
	MOV	A,C		;No, found correct line
	DCR	A
	STA	CURVER		;Set cursor
	JMPR	TOVIS3		;Compute horizontal
;
;	Entry point when new screen needed with cursor in middle
;
				;{VREPLC,FORMPA}
TOVIS2:	LDA	WWNLIN		;Get # lines in window
	ADI	1		;;Prevent CURVER = 0 problems, clear 'C'
	RAR			;Preferred cursor pos. is middle of screen
	STA	CURVER		;
;
;	Entry when new screen needed with cursor preferred at old position
;
				;{VINTXT,VCPTXT,CRNXLN}
TOVIS1:	CALL	STNSNL		;Set to force new window write
TOVIS3:	PUSH	H		;Save edit pointer
	CALL	NEWAC2		;TXACTV & HL-> first char of current line
				;NEWAC2 checks HL against TXTBGN, sets TXACTV
;
;	Determine logical horizontal position.
;
	POP	D		;DE = edit point
	CALL	GETLP0		;HL = horizontal logical position of EDTPTR
;
;	Determine cursor horizontal position and
;	continuation line number for cursor.
;
TOVIS5:	CALL	DVHOR1		;A = CURHOR, B = continuation
	STA	CURHOR		;Set screen Hor. pos
	CALL	STNWLN		;Setup for next text line in case display OK
	CALL	HZRGCK		;Horz. scroll screen to contain cursor pos
				;Check NWSCFL mask 80H if screen needs rewrite
	JNC	VMAIN5		;No, goto VMAIN loop
				;Else, setup for new screen
;
;	Setup for new screen - Need SCRFCB[0] and POSTBL[0].
;
	CALL	CLLGTP		;;Clear LOGPOS for first first line
				;;Needed in case BACKLN not called
	LHLD	TXACTV		;HL-> active line begin
	LDA	CURVER		;Get preferred cursor pos
	MOV	C,A		;Save in C
	DCR	A		;# lines back to screen begin
	SUB	B		;# lines before active line
	MOV	B,A		;Save in B
	BEQ	TOVIS8		;Branch if at top of screen
	BGE	TOVIS6		;Find correct begin of screen
;
;	Active line begins before top of screen.
;	Compute screen-begin from EDTPNT.
;
	LHLD	EDTPTR		;;HL = edit PTR
	INX$	H
	INX$	H		;;Adjust for 2x DCX H in BACKLN
				;;Causes BACKLN return -> begin CURRENT line
	CALL	BACKLN		;;HL-> begin current line, set POSTBL[0]
	JMPR	TOVIS8		;;Store as SCRFCB[0]
;
TOVIS6:	PUSH	B		;Save line count
	CALL	BACKLN		;Move back one screen line
	POP	B		;Restore count
	JRNC	TOVIS7		;Branch if at begin of file
	DJNZ	TOVIS6		;Try to move back all lines
;
TOVIS7:	MOV	A,C		;Get preferred cursor pos
	SUB	B		;Subtract any lines which don't exist
	STA	CURVER		;Save as vertical cursor pos
TOVIS8:	CALL	SCRBG		;Store HL at SCRFCB[0]
	JMP	VMAIN		;Begin visual editing
	EPROC	VEDCMD
	.PAGE
;
; FRMVIS - Determines the command edit pointer from the
;	   Visual edit pointer. [10/29/86]
;
				;{VEND, numerous in V2}
FRMVIS:	TST$	WWUPFL		;Is this only an update?
	RNZ			;Yes, return
				;(Prevents "C" command problems at LF)
	CALL	BCKATV		;Write the active line back
FRMVS1:	LHLD	ACTPNT		;HL = visual edit point
	LXI	D,ACTBUF	;DE = begin of active line
	DSUB	D		;HL = visual edit offset
	LDED	TXACTV		;+DE = begin of active line
	DAD	D		;HL = command edit point
	SHLD	EDTPTR		;Save as new edit pointer
	RET
;
; VESC - Break out to command mode.
;
				;{[VIS ESCAPE]}
VESC:	LSPD	VSTACK		;Reset stack to entry condition
	POP	H		;Ignore normal return address
	LXI	H,BR8K		;HL-> end of command BREAK processing
	PUSH	H		;Make sure we return to BREAK processing
	CLR$	REGSTK		;MSGHND()/BR8K() complication
;
; VDONE - Exit visual mode, continue processing command buffer. (3/86)
;
				;{[VIS EXIT],UPDONE,VESC^}
VDONE:	CALL	RPTRST		;Prevent [REPEAT] [VEXIT] problems
	CALL	MACSTA		;;Is a keystroke macro running?
	JRZ	VEND		;;No, branch
;
;	Build keystroke macro into command execution string
;	Should not end in "V" (or stack won't get popped)
;
	CALL	FRMVIS		;;Set EDTPTR
	CALL	VISBAK		;;Backup to "V" command
	LXI	H,ACTBUF	;;Use active line for work area
..9:	PUSH	H
	CALL	MACSTA
	JRZ	..3		;;Loop done when end of keystroke-macro
	SHLD	KEYPTR
	LXI	H,'T'*256+'R'	;;HL = "RT"
	CALL	CMHLBC		;;Is this a RETURN?
	POP	H		;;HL-> work buffer
	BNE	..2		;;No, branch
	MVI	M,CR		;;Yes, convert to CR-LF
	INX$	H
	MVI	C,LF
..2:	MOV	M,C		;;Add character
	INX$	H		;;Bump PTR
	JMPR	..9		;;Get next char
;
..3:	POP	H		;;HL-> past last char
	MVI	M,00		;;Terminate with [00]
	LXI	H,ACTBUF	;;HL-> begin of string
	CALL	SETCMD		;;Set command stack to execute
	JMPR	VEND1		;;Merge below
;
; VEND - Reset command/visual-mode interface to command mode values.
;	 Continue processing at command mode routine <- by HL.
;
				;{VDONE,VMAC}
	BPROC	VEND
VEND:	CPIB$	REPLFL,2	;Is this [REPLACE]-"Rest"?
	BEQ	VEND2		;Yes, branch around some stuff
	CALL	FRMVIS		;Set command edit pointer
VEND1:	CALL	UPDONE		;Put up update cursor
	CALL	ENDLST		;Restore previous LSTFLG and ROUTAD
	CALL	RSTPOS		;;Reset LOGPOS = 1 to prevent "<<"
VEND2:	CLR	VISFLG		;BCKATV needs VISFLG, called by FRMVIS
	STA	WWUPFL		;Clear update-only flag
	LSPD	VSTACK		;Reset stack to entry condition
	RET			;WWMODE may trigger switch to command window
	EPROC	VEND
	.PAGE
;
; VMAC - Perform command macro from visual mode.
;
				;{[MACRO]}
VMAC:	CALL	VISBAK		;Backup CMDGET -> "V"
	CALL	GETRNM		;Prompt for register # to execute
	CALL	MCMD1		;Save GET/PUT, set GET/PUT from register 'r'
	JMPR	VEND		;Enter command mode to process the macro
;
; VISMAC - Execute command string [00] <- (HL) in command mode.
;
VISMAC:	PUSH	H		;Save -> command string
	CALL	VISBAK		;Backup CMDGET -> "V"
	POP	H		;HL-> string [00]
	CALL	SETCMD		;Set command stack to execute
	JMPR	VEND		;Execute command string
;
				;{VMAC,VISMAC}
VISBAK:	CALL	BAKGET		;Back up GET to point to the V command
	MOV%ES	A,M		;Get 'v' or 'V'
	STA	VSAVE		;Save it to be restored by DISPAT
	MVI%ES	M,VMCODE	;Visual-macro-end code
	RET
;
; VMACCK - Give pause prompt if needed after end of VMAC processing. (8/1/85)
;
				;{VEDCMD}
VMACCK:	LES	H,CMDGET	;HL-> past visual mode entry command
	DCX$	H		;HL-> vme command
	MOV%ES	A,M		;Get it ('V',VMCODE)
	CPI	VMCODE		;VMAC processing just finished?
	RNZ			;No, return
	LDA	VSAVE
	MOV%ES	M,A		;Yes, restore original visual entry command
	TST$	WWMODE		;Was window used for command mode output?
	RNZ			;No, screen OK
;
				;{HELPEX,VMACCK^}
PAUSE:	CALL	MACRST		;;Kill any keystroke macro
	LXI	H,WAITMS	;HL-> "Press any key to continue"
	CALL	STAKEY		;Prompt on status line and wait for reply
	CLR$	CNCLFL		;;Clear flag if [CANCEL] pressed
	JMP	MACRST		;;Kill any keystroke macro
	.PAGE
;
; VSHELP - Visual mode help.  Need to exit/reenter in case multiple
;	   windows get rewritten.
;
	BPROC	VSHELP
VSHELP:	CALL	BAKGET		;Backup to "V" command
	CALL	FRMVIS		;Finish up visual mode
	CALL	ENDLST		;Restore LSTFLG,ROUTAD to previous
;
	LXI	H,VVHELP	;HL-> "VVHELP.HLP"
	CALL	HELPEX		;Display help screen(s)
	JMP	TOCMD		;Let's go back to command mode, briefly
;
; VSHEL2 - Get and decode edit functions.
;	   Return: 'C' if unable to decode keypress.
;
				;{HCMD}
VSHEL2:	CALL	GETCHR		;Get encoded keystroke(s); 'Z' if simple char
	JRZ	..ERR		;Give error if not edit-function
	ST16	STRBUF,B	;Use STRBUF[] for search pattern buffer
	XRA	A		;A = 0, 'NC'
	ST8	STRBUF+2	;Set EOS
	RET			;'NC'
;
..ERR:	LXI	H,VHLPMS	;HL-> help error message
	MVI	A,10H		;Temporary pause
	CALL	MSGHND		;Display message
	STC			;'C'
	RET
	EPROC	VSHELP

