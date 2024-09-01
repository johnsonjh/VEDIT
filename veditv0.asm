	.PAGE
	.TITLE	'VEDIT-V0'
;************************************************
;*						*
;*	Command / Visual Interface		*
;*						*
;************************************************
;
; Copyright (C) 1987 by CompuView Products, Inc.
;			1955 Pauline Blvd.
;			Ann Arbor, MI 48103
;
;	Written by: Theodore J. Green
;
;	Last Change: Tom - Jan. 10, 1986 - 2.32a - TOVIS (clear DSKFUL)
;		     TnT - Dec. 04, 1986 - 2.03 changes
;		     Ted - Feb. xx, 1987 - DEMO changes for 8086
;			   Mar. 24 - Set NWSCFL if WWNDUP is set
;			   Apr. 08 - Call VMACCK() depending upon WWZMFL
;			   Apr. 18 - Fixes for new use of WWMODE
;			   Apr. 27 - Update screen if WWNDUP == -1
;
;
; VEDCMD - Sets the screen beginning, active line and
;	   cursor positions from the command edit pointer.
;
	BPROC	VEDCMD
VEDCMD:	SSPD	VSTACK		;Save current stack value for VLOOP/VDONE
				;8080 - OK to destroy HL
	IF	P8086, [
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

	IFNOT	DEMO, [
	LXI	H,NOTVMS	;HL-> error message
	JNZ	MSGBRK		;No give an error and BREAK

	] [
;
;	DEMO version - first time KEYTBL[1] will have 00 in it.
;
;6	JZ	#0		;;Yes, branch
;6	MOV	DI,345		;;Offset against hackers
;6	MOV	DX,OFS KFF*256	;;No, get INIT0() value
;6	MOV	AX, EWP [BX]	;;AX = table values
;6	PUSH	BX		;;Save -> KEYTBL
;6	MOV	BX,OFS NOTVMS-345  ;HL-> error message
;6	ADD	BX,DI		;;Hard to disassemble
;6	CMP	AX,DX		;Is this normal 1st time demo value?
;6	JLNZ	MSGBRK		;No give an error and BREAK (reset stack)
;
;	Trap for DEMO version
;
;6	MOV	BX,OFS NEXFIL	;;Begin checking at WRITE()
;6	MOV	CX,OFS WRTFIX	;;End checking at WRTFIX()
;6	SUB	CX,BX		;;CX = # bytes to check
;6	MOV	DX,0000		;;Init counter to 0000
;6	MOV	AH,00
;6#11:	MOV	AL, CS: [BX]
;6	ADD	DX,AX		;;Add it in
;6	INC	BX
;6	LOOP	#11
;6	POP	BX		;;BX-> KEYTBL
;
;6	CMP	DX,DEMCHK	;;Is it correct value?
;6	BNE	#12		;;No, disable CLOSER() too
;6	MOV	EBP [BX],CR	;;Yes, fix KEYTBL
;6	JMP S	VEDCMD		;;Try again
;
;	A hacker has tinkered - be subtle
;
;6#12:	MOV	BX,OFS CLOSER	;;Modify CLOSER
;6	ADD	BX,15		;;Offset to JLC BREAK
;6	MOV	AX,09090H	;;Get double NOP instructions
;6	MOV	CS:[BX],AX	;;Force BREAK
;
;6	MOV	BX,(OFS BREAK)-345  ;;Offset to BREAK()
;6	ADD	BX,DI
;6	JMP	BX
	]			;<IF DEMO>
;
..0:	CPIB$	REPLFL,2	;In middle of Replace "Rest"?
	JZ	VFNEXE		;Yes, branch to handler
				;Jumps back to VEDCM1 after last replace
VEDCM1:
;
;	Switch to visual mode window
;
	IF	WINDOO, [
	TST$	WWZMFL		;;In auto-zoom mode ([FILE]-D)?
	JRP	..1		;;No, check for VMACCK() later
	TST$	WWUPFL		;Is this only a window update?
	JRNZ	..1		;Yes, branch
	CALL	VMACCK		;Pause if end of VMAC execution
..1:
	IF	VPLUS, [
	CALL	EDTNAM		;;A = edit buffer name
	] [
	MVI	A,'@'		;Use window @ if open
	]

	PUSHA			;Save edit buffer name
	CALL	SWWIND		;Switch to visual mode window
;
	TST$	WWUPFL		;Is this only a window update?
	JRNZ	..5		;Yes, branch
	MVIB$	COMMWI,'$'	;No, goto "$" window on exit
	CALL	VMACCK		;Pause if end of VMAC execution
;
..5:	LDA	WWMODE		;;Get window mode
	DCR	A		;;Visual mode?
	CM	STNSNL		;No, ensure window rewritten
	TST$	WWNDUP		;;Does window need update?
	CNZ	STNSNL		;;Yes, ensure window rewritten
				;;CRT without INSERT-line needs this
				;;because screen-scroll sets WWNDUP
	POPA
	STA	WWMODE		;Save edit buffer # in window structure
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
	BPROC	VDONE
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
	EPROC	VDONE
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
	RNZ			;No, screen OK (Returns if new window too!)
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
