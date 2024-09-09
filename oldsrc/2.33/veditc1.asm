	.PAGE
	.TITLE	'VEDIT-C1'
;****************************************
;*					*
;*	C O M M A N D   E D I T O R	*
;*					*
;****************************************
;
; Copyright (C) 1986 by CompuView Products, Inc.
;			1955 Pauline Blvd.
;			Ann Arbor, MI 48103
;
;	Written by: Theodore J. Green
;
;	Last Change: Ted - Nov. 09, 1985 - Look at char from GETCHR in AL.
;			   Jan. 27 - All new GETNUM, GETDEC and CMDNUM
;			   Feb. 04 - Command buffer now in ++ direction
;			   Mar. 11 - UPDWIN() before COMMAND: prompt
;			   Mar. 13 - DOBS() and misc. changes
;
;		     Tom - Mar. 14 - CMDBR2 to handle 2-letter cmds
;				     CMDERR renamed CMBR8K (BR8Ks out)
;			   Mar. 14 - VEDIT/VPLUS merged
;
;		     Ted - Mar. 22 - DOBS, PROMPT, NEWCMD, GETSTR changed
;			   Apr. 03 - Memory management for GETSTR
;			   Apr. 10 - Debugging
;
;		     Tom - Apr. 21 - Moved PSHCHK to end of PSHCMD in C3
;				   - PRCSPX lda cmdchr
;			 - Apr. 23 - GETNXP new routine
;			 - May  05 - PROCMD fix -command bug
;			 - May  15 - NXCMCH check PARMFL for null parameter strings
;				     at end of macro  (e.g. ED<EOF>)
;				   - PROCMD default bug for xa1:  set CHL = 1
;			 - June 13 - PROCMD default bug for -xa1: set CHL =-1
;		     Ted - July 18 - Remove CNCLFL check in GETSTR()
;		     Tom - Aug. 20 - NEWCMD, EXCBUF=0 => no auto-execution
;			   Sep. 18 - BAKGET() return ES as well as BX
;		     Ted - Oct. 12 - Fix GETSTR bug - call RSTOUT for CTRL-C
;		     Tom - Oct. 13 - C2BR8K() - "xX" now prints as "xx"
;		     Ted - Nov. 13 - GETSTR() tests for CNCLFL
;			 - Dec. 04 - NXCMUC - UC version of NXCMCH
;		     Tom - Dec. 07 - NEWCMD() fix '??' segment bug
;
MOVE3:	MVI	C,3
	JMP	MOVE
;
; PROCMD - Process next Edit command and get next command string when needed.
;
;	First check for iteration number.
;
	BPROC	PROCMD
PROCMD:	CALL	BRKCHK		;Abort on user's CTRL-C
				;This works for #[] "infinite" loop

	IF	VPLUS, [
	LXI	D,TRVGET	;For 'V' option in TRACE
	CALL	CPTSET		;Move CMDGET,CMDPUT into TRVGET,TRVPUT
	]

	MOVW	DELPTR,EDTPTR	;Initial Delete PTR is edit PTR

	CLR	PARMFL		;Clear parameter-expected-after-command flag
	STA	SAVPFL		;And it's backup value

	IF	VPLUS, [
	STA	COLFLG		; Clear suppress (whatever) flag
	STA	GLBFLG		; Clear global flag
	MOVB	ATTFLG,ATTSW	; Initialize explicit delimiters flag from ES 8
	]			;<IF VPLUS>

	IFNOT	VPLUS, [
	MOVW	COLFLG,COLSW	;Set COLFLG and ATTFLG from ES
	MOVB	GLBFLG,GLOBSW	;Set GLBFLG from ES
	]			;<IFNOT VPLUS>

	CALL	NXTCHR		;Make sure command line is not empty
	CALL	BAKGET		;Backup GET PTR
;
;	Get & set command parameter(s) and flags.
;
	CALL	GETDEC		;Get HL = |expression e|, set flags
	LXI	D,MAXINT	;DE = 65535
	CALL	CMHLDE		;|e| = 65535?
	MVI	A,0		;In case not
	BNE	..0		;No, branch
	MVI	A,'#'		;Yes
..0:	STA	PNDFLG		;Set/reset flag

	TST$	XNMFLG		;Was an explicit expression encountered?
	JRNZ	..1		;Yes, branch
	LXI	H,1		;No, use 1 as the default value
	CPIB	XRMINU,'-'	;Real minus sign?

	IF	VPLUS, [
	MOV	C,H		;CHL=1
	PUSH	H
	CZ	CHGSGN		;If so, get CHL = -1
	LXI	D,CHL
	CALL	STCHL		;Change CHL from 0 to +/- 1
	POP	H		;HL=1
	] [
	BNE	..1		;No, branch
	STA	XMINUS		;Yes, ensure MINUS gets set properly
	]

..1:	SHLD	ITRCNT		;Save iteration count

	LXI	H,XMINUS	;HL-> 3 flag-bytes set by EXPR
	LXI	D,MINUS		;DE-> 3 command flags MINUS, RMINUS, NUMFLG
	CALL	MOVE3

;
;	Check for 2d command parameter.
;
	IF	VPLUS, [
..1A:	CALL	NXSCAN		;Get non-blank delimiter
	CPI	COMMA		;Parameter terminator = ',' ?
	STA	NM2FLG		;Set 2d parameter flag
	BNE	..1B		;No, branch
	CALL	GETDEC		;Yes, get 2d parameter
	SHLD	ITRCN2		;Save 2d parameter for K,P,T commands
	CALL	NXSCAN		;Get non-blank elimiter
;
;	Check for command-loop-breakout char.
;	(ERRCMD not cleared as it would be by eJL).
;
..1B:	CPI	SEMCOL		;Loop breakout symbol ";" ?
	JRNZ	..2		;No, branch
	TST$	ERRFLG		;Is the ERROR flag set?
	RZ			;No, just ignore ";"
	JMP	ENDITR		;Yes, break out of current iteration loop
	]			;<IF VPLUS>
;
;	Check for modifiers.
;
	IF	VPLUS, [
..2:	CALL	CHKMOD		;Command modifier?
	JRNZ	..3		;No, branch
	STAX	D		;Yes, set proper modifier flag
	CALL	NXCMCH		;A = next char from command string
	JMPR	..2		;Check for more modifiers
;
..3:	TST$	GLOBSW		;Has user selected global environment?
	JRZ	..5		;No, branch
	MVIB$	GLBFLG,'_'	;Yes, set flag
	]

	IFNOT	VPLUS, [
..2:	CALL	NXCMCH		;Get next char
	CPI	':'		;":" modifier?
	BNE	..4		;No, branch
	STA	COLFLG		;Yes, set ":" flag
..3:	CALL	NXCMCH		;Get next char
	JMPR	..2		;Continue checking
;
..4:	CPI	'@'		;"@" modifier?
	BNE	..5		;No, branch
	STA	ATTFLG		;Yes, set "@" flag
	JMPR	..3		;Get next char
;
..5:	CPI	'_'		;"_" modifier?
	BNE	..6		;No, branch
	STA	GLBFLG		;Yes, set "_" flag
	JMPR	..3		;Get next char
	]			;<IFNOT VPLUS>
;
;	Check for !LABEL!
;
	IF	VPLUS, [
..5:	CPIB	CMDCHR,'!'	;Label designator?
	BNE	..6		;No, branch
	LES	H,CMDGET	;HL -> next command-line character
	MVI	B,'!'		;Set search character
	CALL	SCNSTR		;Advance GET past first of {!,CR,ESC}
	SHLD	CMDGET		;Set new CMDGET, presuming no error
	RET			;Yes, return, thus skipping over the label
	]			;<IF VPLUS>
;
;	Check for start of DO clause:  "[".
;	(VPLUS's REPEAT-UNTIL & IF-THEN-ELSE clause)
;
..6:	LXI	D,ITRSTK	;DE-> iteration stack
	LXI	H,ITRMCL	;HL-> opening bracket "["
	CMP	M		;Start of clause?
	BNE	PROCM8		;No, branch
;
	IFNOT	VPLUS, [
	CALL	GETITR		;Get iteration count
	RZ			;Return if zero now
	DCX$	B		;Adjust for when end bracket found
	]			;<IFNOT VPLUS>

	TST$	NUMFLG		;Explicit iteration count?
	JRNZ	..7		;Yes, use it

	IF	VPLUS, [
	LXI	H,MAXINT	;No, set count as large as possible
	SHLD	ITRCNT
..7:	TST$	TRACEF
	CNZ	TRACE
;
	CALL	GETITR		;BC = iteration count, zero flag set/reset
	JRZ	ELSEIF		;IF-THEN condition false, skip to ELSE clause
;
	DCX%BC			;Counts 1st time thru REPEAT, checks for IF
	LXI	D,ITRSTK
	LES	H,CMDGET	;Get command forward pointer
	JRNZ	PUSHCL		;ITRCNT > 1 => REPEAT-UNTIL clause
	LXI	H,0		;Set GET ptr = 0 => THEN-clause flag
	]			;<IF VPLUS>

	IFNOT	VPLUS, [
	LXI	B,MAXINT	;Yes, change to "#"
..7:	LHLD	CMDGET		;Get command forward pointer
	]
;
;	Push clause onto ITRSTK.
;
PUSHCL:	XCHG			;HL-> iteration stack, DE = CMDGET

	IF	VPLUS, [
	LDA	REGEXN		;In case of JM (macro breakout) command
	]

	JMP	PSHCHK		;Push A,BC,DE onto ITRSTK, BREAK if overflow
;
;	Check for end of clause:  "]".
;
PROCM8:	INX$	H		;HL-> end bracket "]"
	CMP	M		;End of bracketed clause ?
	BNE	DISPAT		;No, just dispatch
;
	XCHG			;Yes, HL-> iteration stack
	CALL	DECSTK		;Test for empty stack, decrement count if not
	JRC	DISPAT		;Ignore end bracket if stack is empty

	IF	VPLUS, [
	JRZ	ENDITL		;When count -> zero, pop stack, check for
	]			; end of THEN clause

	IFNOT	VPLUS, [
	LXI	H,ITRSTK	;HL-> iteration stack
	JZ	POPSTK		;Pop stack if iteration count = 00
	]

	SDED$	CMDGET		;Else set GET ptr to start of REPEAT clause
	RET
;
;	Pop REPEAT-UNTIL / IF-THEN-ELSE from iteration stack.
;
	IF	VPLUS, [
ENDITL:	CALL	POPITR		;Pop clause from iteration stack
	RC			;Return if not on stack. (?)
	MOV	A,D		;GET PTR = 00 => THEN clause
	ORA	E		;IF-THEN clause?
	RNZ			;No, return
	CALL	NXCMCH		;Yes, check for ELSE clause
	LXI	H,ITRMCL	;HL -> opening bracket
	CMP	M		;ELSE clause ?
	JZ	SCNGIT		;Yes, skip past it
JBAKGT:	JMP	BAKGET		;No, put look-ahead char back into scan line
;
;	Check for ELSE clause for false condition.
;
ELSEIF:	CALL	SCNGIT		;Skip the THEN clause
	LXI	H,ITRMCL	;HL -> opening bracket
	CALL	NXCMCH		;A = next command char
	CMP	M		;ELSE clause?
	BNE	JBAKGT		;No, put look-ahead char back into scan line
;
;	Process ELSE clause.
;
ELSIF1:	LXI	B,0		;Set loop count for one time through
	LXI	D,ITRSTK	;DE -> iteration stack
	LXI	H,1		;GET = 1 => ELSE clause for JLCMD & JNCMD
	JMPR	PUSHCL		;Push ELSE clause onto iteration stack
;
CHKMOD:	LXI	D,COLFLG	;De -> colon-modifier flag
	CPI	':'		;":" modifier?  (return error, don't abort)
	RZ
	INX$	D		;DE -> ATTFLG
	CPI	'@'		;"@" modifier?  (explicit string delimiters)
	RZ
	INX$	D		;DE -> GLBFLG
	CPI	'_'		;"_" modifier?  (global search)
	RET
	]			;<IF VPLUS>
	EPROC	PROCMD
;
; DISPAT - Command Dispatcher.  Find command letter in TABLE & jmp to routine.
;
;	   VPLUS:  display number followed by <cr> or <esc> on console.
;		   Handle trace mode.
;

BR8KPT:	DSW	PTRSIZ		;-> start of current command
TWOLET:	DS	1		;Flag to CMBR8K for 2-letter command

	BPROC	DISPAT
DISPAT:
	IFNOT	VPLUS, [
	CALL	BAKGET		;Backup so that NXTCHR gets current char
	CALL	NXTCHR		;Get current char & set CMDCHR
	]			;<IFNOT VPLUS>

	IF	VPLUS, [
	CALL	GTSCAN		;Get current command, skipping blanks
	]

	MOVW	BR8KPT,CMDGET	;Set pointer for CMBR8K processing
;6	MOVW	BR8KPT+2,CMDGET+2
	MVIM%CS	TWOLET,0	;1-letter command at this point

	CPI	VMCODE		;Visual macro running?
	BNE	..1		;No, branch
	MVI	A,'V'		;Yes, get visual entry command
..1:

	IF	VPLUS, [
	CALL	LETCHK		;Is it a letter?
	JNZ	PRCSPX		;No, process special character
	PUSHA			;
	STA	WWNDUP		;Set flag - visual window update needed (TJG)
	CLR$	ERRFLG		;Clear command-failed flag
	TST$	TRACEF		;Trace flag set?
	CNZ	TRACE		;If so, display <CR><LF> CHL CMDCHR
	POPA			;
	]			;<IF VPLUS>

	LXI	H,DSPTBL	;HL-> command address table
	LXI	D,CCBR8K	;Conditional command error BR8K out routine

;
; DISPT2 - Branch to routine described by HL,A with A a letter, HL-> table.
;	   DE-> error BREAK out routine for case when A is not a letter.

				;{ECMD,TSTVOP+}
DISPT2:	CALL	LETCHK		;Is it a letter?
	JNZ	JUMPDE		;No, jump to where DE-> & give error
	ANI	5FH		;Convert to upper case
	SUI	'A'-1		;Compute table entry
	CALLC	TBLADD		;Get routine address
	XRA	A		;Put zero in A
	PCHL			;And dispatch to it
	EPROC	DISPAT

;
; PRCSPX - Process special char.
;
;	   parm<CR> or parm<ESC>:  display parm
;	   ?:  flip state of trace flag
;	   Ignore control chars
;	   Otherwise give error and BR8K out.
;
	IF	VPLUS, [
	BPROC	PRCSPX
PRCSPX:	CALL	CMPCE		;CR or ESC ?
	JRNZ	..1		;No, branch
	CLR$	ERRFLG		;Clear error flag
	TST$	NUMFLG		;Expression entered?
	RZ			;No, ignore CR or ESC
	JMPR	CALC
;
..1:	CPI	'?'		;Flip trace-flag?
	JZ	SETTRF		;Yes, turn on trace mode
	JMPR	CCBR8K		;Conditional command error BR8K out routine
	EPROC	PRCSPX
;
;	CALCULATOR - Display 17-bit signed CHL on console.
;
				;{PRCSPX,QCMD}
CALC:	CALL	GITCHL		;CHL = (CHL)
	JMP	XTCMD0		;Merge with XTCMD
	]			;<IF VPLUS>
	.PAGE
;
; CCBR8K - Conditional Command-BR8K-out if CMDCHR is a visible char ( > 20H).
;	   If so, insert it into error msg, display msg & BR8K out.
;
				;{DISPAT}
	BPROC	CCBR8K
CCBR8K:	LDA	CMDCHR		;Get current command char
	CPI	SPACE+1		;Control char or space?
	JRNC	CMBR8K		;No, BR8K out
	RET			;Yes, just return

C2BR8K:	MVIB$	TWOLET,1	;2-letter command
CMBR8K:	LES	D,BR8KPT	;DE-> past 1st char of invalid command
	DCX$	D
	LDX%ES	D
	INX$	D
	LXI	H,ILGMSG+17	;HL-> spot in message to insert bad char(s)
	MOV	M,A
	INX$	H
	TST$	TWOLET		;Two letter command?
	JRZ	..1		;No, branch

	LDX%ES	D
	CPI	SPACE		;Ctrl char?
	JRC	..1		;Yes, ignore
	MOV	M,A		;Insert char into msg
	INX$	H		;Bump pointer

..1:	MVI	M,'"'		;Insert end quote and msg termination bit
	INX$	H
	MVI	M,00
	LXI	H,ILGMSG	;HL-> Invalid Command msg
	MVI	A,050H		;CRLF() after msg (cm), temporary pause (vm)
	CALL	MSGHND		;Display message
	JMP	BR8K		;Finish BR8King out to command main
	EPROC	CCBR8K
	.PAGE
;
; TRACE - Display current command & rest of line, then wait for user response.
;			[7/5/85]
;
;	<CR>  Single step:  perform current command, remain in trace mode.
;	<SP>  Hop step:  same as <cr> except for macro execution command 'M'.
;	      In that case, disable tracing until the macro finishes.
;	<ESC> Exit trace mode, continue performing command string.
;	<ctrl-c> Exit trace mode, abort rest of command string.
;	<?>   Displays the next command line.  May be pressed iteratively
;	      to show more of the buffer.
;	<v>   Go to visual mode.  Continues when <visual exit> is pressed.
;
	IF	VPLUS, [
	BPROC	TRACE
TRACE:	CPIB$	CMDCHR,20H	;Command char = control char?
	RC			;Yes, don't allow
;
	LXI	H,WINOUT	;Make sure output goes to console
	LDA	LVLCON		;Probably best choice for LSTFLG
	CALL	SETOUT		;Stack up a new ROUTAD and LSTFLG
;
				;{TRCVER}
TRACE0:	CLR$	TRVFLG		;Clear trace-into-visual-mode flag
;6	MOV	AX,CMDPUT+2
;6	CALL	SWAPSG
	CALL	..SUB1		;Start tracing
;6	CALL	RSTSEG
	TST$	TRVFLG		;Trace-visual option?
	JNZ	TRACEV		;Yes, process
	JMP	RSTOUT		;Restore correct output handler
;
;	Begin tracing.
;
..SUB1:	CALL	CRLF		;Send <CR><LF> to console
;
	LBCD	ITRCNT		;Get parameter value
	CALL	PRTDEC		;Display it
	MVI	A,'-'		;Check for minus sign
	LXI	H,RMINUS	;This check will get the -0 parameter
	CMP	M		;   Explicit minus sign?
	BEQ	..2		;   Yes, branch
	TST$	MINUS		;True negative number?
	JRZ	..21		;No, branch
	MVI	A,'-'		;Yes, get minus again
..2:	CALL	PCHARA		;Display minus sign
;
..21:	CPIB	NM2FLG,COMMA	;Second parameter?
	BNE	..3		;No, skip
	CALL	PCHARA		;Display the comma
	LBCD	ITRCN2		;BC = value of 2d parameter
	CALL	PRTDEC		;Display it
;
..3:	MVI	C,SPACE		;Separate rest of line from evaluated parm
	CALL	PCHAR
	CALL	PRTFLG		;Display command prefixes if any
	LDED	CMDGET		;DE -> past command char
	JMPR	..6		;Display rest of command line
;
..4:	CALL	GETKEY
	CPI	3
	JZ	BREAK		;Break back to command mode on <ctrl-c>
	CPI	CR
	RZ			;Continue on <CR>
	CPI	SPACE
	BEQ	HOPSTP		;Hop step (execute w/o tracing) macro commands
	CPI	ESC
	BEQ8	CLRTRF		;Turn trace mode off on <esc>
	CPI	'?'
	BEQ	..5		;
	ANI	5FH		;Make upper case
	CPI	'V'		;Enter visual mode?
	BNE	..4		;Ignore anything else
	MVIB$	TRVFLG,0FFH	;Yes, set flag so TRACEV will be called
	RET			;
;
;	Print out rest of command line
;
..5:	LDED$	OLDGET		;DE -> end of previously displayed line
..6:	LHLD	CMDPUT
	DCX$	H
	CALL	CMHLDE		;Already at end ?
	BLT	..4		;Yes, then do nothing
	INX$	H		;HL -> past end of the command string
	MOV	A,M
	PUSHA
	MVI	M,EOF
	PUSH	H
	MOV	H,D		;Copy GET into HL
	MOV	L,E
	INX$	H		;Advance HL past possible <lf>
	CALL	NEXTLF		;HL -> EOL marker.  Is it <lf>?
	SHLD	OLDGET		;Save -> EOL as GET for next '?'
	BLT	..7		;No, skip
	DCX$	H		;Yes, back up to <cr>
..7:	DCX$	D		;DE-> <cr> or CMDCHR ahead of GET
	CALL	SBHLDE		;BC = # chars to display
;6	MOV	ES,CMDGET+2
	CNZ	RTCMD3		;Display rest of string, if any
	POP	H
	POPA
	MOV	M,A
	JMPR	..4
	EPROC	TRACE
;
PRTFLG:	CPIB	GLBFLG,'_'
	CZ	PCHARA
	CPIB	ATTFLG,'@'
	CZ	PCHARA
	CPIB	COLFLG,':'
	JZ	PCHARA
	RET
;
; HOPSTP - Hop step over command macros (execute but don't trace).
;
HOPSTP:	LDA	CMDCHR		;Get current command char
	ANI	5FH		;Capitalize it
	CPI	'M'		;Macro command?
	RNZ			;No, single step this command
	STA	TRDENB		;Disable tracing
	CLR$	TRACEF		;Turn trace mode off
	MOVB	TRSAVE,REGSTK	;Save for deciding when to re-enable tracing
	RET			;Continue executing command string
;
; SETTRF - Unless disabled, set trace flag.
;	    Returns '?'.
;
				;{DISPAT,CHKEKO}
SETTRF:	TST	TRDENB		;Tracing disabled?
	MVI	A,'?'
	RNZ			;Yes, return
	STA	TRACEF		;No, set trace flag
	RET
;
; CLRTRF  - Reset trace flag, enable setting of trace mode.
;
				;{TRACE,NEWCMD}
CLRTRF:	CLR	TRACEF		;Reset trace mode flag
	STA	TRDENB		;Reset trace-disenable flag
	RET
;
; TRACEV - Process Trace-into-visual-mode option.
;
TRACEV:				;{TRACE}
;
;	See if current command buffer is same as at start of command in PROCMD.
;
	LHLD	CMDPUT		;Get PUT
	LDED	TRVPUT		;
	CALL	CMHLDE		;Same buffer?
	BNE	TRCVER		;No, error
;
;	Reset GET to start of current command.
;
	MOVW	CMDGET,TRVGET	;TRACE will reshow current command next time in
;
;	Enter visual mode via simulated 'V' command.
;
	LXI	H,TRVCMD+1	;HL-> past 'V'
	MOV	D,H
	MOV	E,L		;DE = HL => empty command buffer
	CALL	SETCM2		;Push GET/PUT onto stack, set anew from DE,HL
	CALL	VEDCMD		;Visual mode can backup to the 'V' when exiting
	JMP	TOCMD		;Reparse current command
				;Note: a cleaner PROCMD with separate DISPAT
				;could be called to reevaluate the command
				;and we could loop back up to TRACE()
				;preserving the current stack
;
TRCVER:	LXI	H,TRVERM	;Unable to honor request message
	CALL	PRTMSG		;
	JMP	TRACE0		;Re-enter TRACE
	]			;<IF VPLUS>
	.PAGE
;
; NXTCHR - Get next char from current buffer, swap buffers on |Rr.
;
	IF	VPLUS,  [
				;{FCBCHR,STSEAR}
NXTCHR:	CALL	SV%BDH
	CMPMB	PEEKCH,WILDCH	;Have we just looked ahead and found WILDCH?
	MVIB	PEEKCH,0
	JZ	NXCMC1		;Yes, let's not get caught by ||R
;
	CALL	NXCMC1		;No, get char from current buffer
	LXI	H,WILDCH	;HL-> user defined wild character
	CMP	M		;Wild char?
	RNZ			;No, return with current char in A
;
	CALL	NXCMC1		;Yes, look ahead at next char
	STA	PEEKCH		;Save for next reentry in case it's WILDCH
	ANI	5FH		;Make upper case
	CPI	'R'		;|R?
	JNZ	PRCMC1		;No, backup GET ptr, return WILDCH
;
	CALL	GETCRN		;Yes, get register #
	CALL	PNTREG		;Point to the register
	BEQ	NXCMC1		;Quit if empty
	CALL	MCMD2		;Save command ptrs, set new cmd pointers
	MVIB$	REGEXN,0FFH	;Not executing this buffer!
	MOVB	SAVPFL,PARMFL	;
	CLR	PARMFL		;Need to have this register popped when done
	MOVB	SAVTRM,TERMCH	;Save current string terminator
	MVIB	TERMCH,EOF	;Avoid problem of terminator occurring in |Rr
	JMPR	NXCMC1
	]			;<IF VPLUS>
;
; NXCMCH - Get next character from command string.  [03/05/85]
;	   Return:  Character in A  and CMDCHR.
;	   Saves BDH.
;
				;{PROCMD,CNVDEC,ECMD,FCBCHR,STSEAR,EXPR+}
	IFNOT	VPLUS, [
NXTCHR:
	]

	BPROC	NXCMCH
NXCMCH:	CALL	SV%BDH		;Save all regs
;
				;{NXTCHR+}
NXCMC1:	CMPW	CMDGET,CMDPUT	;Is command exhausted?
	BLT	..3		;Nope ...
;
	TSTM	PARMFL		;Command expecting a string parameter?
	MVI	M,0
	MVI	A,ESC		;Get string terminator
	JRNZ	NXCMC4		;Yes, return terminator
;
	IF	VPLUS, [
	MOVW	OLDGET,CMDGET	;Save in case of "??" command query
;6	MOVW	OLDGET+2,CMDGET+2 ;;Save Segment
	MOVB	OLDEXN,REGEXN	;Also save command buffer number
;
	INR	A		;Running console command buffer?
	BEQ	..1		;Yes, open macros are alright
;
	CALL	POPUNC		;No, check whether any unclosed clauses..
	MOV	A,E		;A = # clauses from REGEXN still on stack
	ORA	A
	LXI	D,ILCSYN	;Invalid clause syntax
	JNZ	MACBRK
	]			;<IF VPLUS>

..1:	CALL	POPCMD		;Previous command macro? (BREAK if open quote)
	JRNC	NXCMC1		;Get next char from it if so

	CALL	NEWCMD		;Get new command

..3:	LES	H,CMDGET	;HL-> next command char

				;{PRCMCH+}
NXCMC5:	MOV%ES	A,M		;Get command char
	INX$	H		;Advance command GET pointer
	SHLD	CMDGET		;Save new pointer
NXCMC4:	STA	CMDCHR		;Save current char
	RET
	EPROC	NXCMCH
;
NXCMUC:	CALL	NXCMCH
	JMP	CONVUC		;;Convert to UC
;
; GTSCAN - Return A = current scan line char (CMDCHR) unless it is a blank.
;	   If (CMDCHR) is a blank, advance to the 1st non-blank character.
;
;	   Requires valid scan line, obtained by calling NXSCAN or NXCMCH.
;
	IF	VPLUS, [
				;{DISPAT}
GTSCAN:	CPIB	CMDCHR,SPACE
	RNZ
;
; NXSCAN - Return A with the next scan line char that is not a blank or tab.
;
				;{GTSCAN^,PROCMDx2}
NXSCAN:	CALL	NXCMCH
	CALL	BLKCHK
	BEQ	NXSCAN
	RET
	]			;<IF VPLUS>
;
; BAKGET - Return ES:HL & CMDGET pointing to just examined command char.
;
;	   Note:  Only valid after call to NXCMCH.
;
BAKGET:	LES	H,CMDGET	;Get the GET Ptr
	DCX$	H		;Move it back to previous char
	SHLD	CMDGET		;Save new Ptr
	RET
;
; PRCMCH - Return previous command char in A and CMDCHR.  CMDGET backed up.
;	   Save BDH.
;
;	   Note:  Not truly defined since GET must be decremented twice.
;
				;{}
;PRCMCH:	CALL	SV%BDH
				;{NXTCHR:  regs not saved)
PRCMC1:	LES	H,CMDGET	;HL-> next cmd chr
	DCX$	H		;HL-> current cmd chr
	DCX$	H		;HL-> previous cmd chr
	JMPR	NXCMC5		;Get it & update CMDGET
;
; GETNXP - Get HL = |next parameter expression|, default = 0.  [4/23/86]
;
;	   Presumes CMDGET -> separator other than ESC, CR, EOS.
;	   Returns failure immediately if entrant separator is ESC, CR or EOS.
;
;	   Exit:  HL = |expression|, (00) if none.
;		  CMDGET -> next delimiter = (CMDCHR).
;		  'C' if no expression.
;
				;{cmds:  EG/EL, ES/EP, ET}
	BPROC	GETNXP
GETNXP:	CPI%ES	CMDGET,EOS	;End of text register/buffer?
	BEQ	..ERR
	CALL	CMPCE		;CR or ESC?
	BEQ	..ERR		;Yes, branch
	INX$	H		;No, advance past separator
	SHLD	CMDGET
	JMPR	GETDEC

..ERR:	LXI	H,0
	STC
	EPROC	GETNXP
	RET
;
; GETDEC - Get HL = decimal number in command line, default = 0.  [3/21/86]
;  VPLUS - Get HL = |expression e|, (CHL)=e.
;
;	   Enter: CMDGET-> command line
;
;	   Exit:  HL = |e|, (00) if none
;		  (CMDGET) -> delimiting char
;
;		  'C' if no expression
;
;	    Return (CMDCHR) with delimiting char, even when blank.
;	    BC & DE saved.
;
				;{PROCMD,SETFCB @ SETFEN,GETNXP}
GETDEC:				;{cmds EP/ES, EU, YW}
	PUSH	B		;DE saved by GETNUM, not used in this routine
	LES	H,CMDGET	;HL-> command line

	IF	VPLUS, [
	MVIB$	GETDCF,0FFH	;Set GETDEC running flag
	]

	CALL	GETNUM		;BC = |(CHL)| = expression, DE saved

	IF	VPLUS, [
	CLR$	GETDCF		;Clear GETDEC running flag
	]

	SHLD	CMDGET		;Save updated PTR
	MOV%ES	A,M
	STA	CMDCHR		;CMDCHR = delimiting char
	MOV	H,B		;Return number in HL
	MOV	L,C
	POP	B		;Restore BC

	TST$	XNMFLG		;Get explicit expression flag
	RNZ			;Yes, return 'NC'
	STC			;No, return 'C'
	RET
;
; GETNUM - Convert decimal number string at (HL) to 16 bit number.
;	   Enter: HL-> string, ends with any non-digit.
;	   Return:  BC = 16 bit unsigned number. (00) if none found.
;		    HL-> delimiting char.
;		    DE saved
;		    XRMINU = 1st non blank char, can be tested for '+' or '-'
;		    MINUS if number preceded with '-'
;		    XNMFLG if number found
;
	IFNOT	VPLUS, [
				;{GETDEC,DOTCMD}
	BPROC	ATOI
ATOI:				;Similar to 'C' function
GETNUM:	PUSH	D		;Save DE
	LXI	B,0		;Return 0000 if no number found
	SBCD	XMINUS		;Clear XMINUS & XNMFLG
;
..1:	MOV	A,M		;Get first/next char
	CPI	' '		;Is it a space?
	JRNZ	..2		;No, branch
	INX$	H		;Bump to next char
	JMPR	..1		;Loop
;
;	Check for '+' or '=' sign.
;
..2:	CPI	'+'		;Is this a plus sign?
	STA	XRMINU		;Set 'real' plus or minus flag
	BEQ	..3		;Yes, scan over '+'
	CPI	'-'		;Or is it a minus sign?
	BNE	..4		;No, don't swallow or set flag
	STA	XMINUS		;Yes, set minus flag
..3:	INX$	H		;Swallow + or - sign
	MOV	A,M		;Get next char
;
;	Check for '#'.
;
..4:	CPI	'#'		;Is it "#" = 32767?
	JRNZ	..5		;No, check for number
;
	INX$	H		;Scan over "#" char
	LXI	B,MAXINT	;Get max pos number
	JMPR	..6		;Branch around
;
..5:	CALL	DIGCHK		;Is character numeric?
	JRNZ	..8		;No, no number found
	CALL	EVAL		;Yes, convert decimal number
..6:	MVIB$	XNMFLG,1	;Set flag that number found
;
..8:	POP	D		;Restore DE
	RET			;BC = number
	EPROC	ATOI
	]			;<IFNOT VPLUS>

;
; EVAL - Convert Ascii Decimal number to 2 byte binary.
;	 Enter: HL-> string
;	 Exit:  HL-> delimiter, BC = number
;
	IFNOT	VPLUS, [
				;{GETNUM}
	BPROC	EVAL
EVAL:	PUSH	H		;Save *string on stack
	LXI	H,0		;Init counter to 0000
..1:	XTHL			;HL-> string
	MOV	A,M		;Get digit
	CALL	DIGCHK		;Is char numeric?
	JRNZ	..2		;Return if not numeric
	INX$	H		;ptr++
	XTHL			;HL = result
	CALL	MULT10		;Multiply HL by 10 and add digit in 'A'
	JMPR	..1		;And go convert it
;
..2:	POP	B		;BC = number
	RET
	EPROC	EVAL
	]			;<IFNOT VPLUS>
	.PAGE
;************************************************
;*						*
;*	     Command Buffer Routines		*
;*						*
;************************************************
;
; NEWCMD - Gets next command line either from console or
;	 - from user specified "locked-in" macro.  [7/23/85]
;
;	   Update command buffer pointer PUT.
;	   Updates BOT & GET as well unless SRCHFL or ITRSTK are set.
;
;	   Echos the executed part of previous command when 1st chars = "??".
;
;	   Returns when {<cr> | <esc><esc>} encountered.
;
	IF	VPLUS, [
				;{BR8K,NXCMCH,ICMD,RICMD}
	BPROC	NEWCMD
NEWCMD:	CALL	RSTCON		;Reset Output for command lines
;
;	Check for user specified "locked-in" execution buffer?
;
	TST	EXCBUF		;Auto-execution buffer?
	JRZ	..1		;No, skip (won't execute '0' or '@')
;
;	Process commands from user's "locked-in" macro.
;
	STA	REGNUM		;Set REGNUM for MCMD
	CLR	REGSTK		;Empty the execution stack
	STA	ITRSTK		;and the iteration stack
	CALL	RJCMD		;Lock into the specified macro
	RNZ			;Return and execute if the macro exists

..1:
;6	PUSH	DS
;6	CALL	SWPCSG		;;Set to cmd/register segment
;
;	Otherwise, interact with the user via the console.
;
	MOVW	OLDGET,CMDGET	;Save end ptr for possible "??" command query
;6	MOVW	OLDGET+2,CMDGET+2
	MOVB	OLDEXN,REGEXN	;Save command buffer # for "??" query
	CALL	CLRTRF		;Turn off trace mode
;
	IF	FULL, [
	CLR	REGSTK		;Clear register execution stack offset
	DCR	A		;Get a 255
	STA	REGEXN		;Flag not executing a register
	]
;
;	Reset command pointers PUT and GET unless open string or iteration.
;
	TST$	SRCHFL		;Open string?
	JRM	..2		;Yes, branch
;
	TST$	ITRSTK		;Open iteration?
	JRNZ	..2		;Yes, branch

	LHLD	CMDBAS		;Reset PUT & GET to CMDBAS
	SHLD	CMDGET		;Reset GET to start of the buffer
	SHLD	CMDPUT		;Reset PUT <= GET
;6	MOV	CMDGET+2,DS
;6	MOV	CMDPUT+2,DS
;
;	Issue prompt & get new command line.
;
..2:	CALL	PROMPT		;Send prompt character, initialize LINPOS
	CALL	CHKEKO		;Get command line, check for "??"
	BNE	..3		;;No, branch
;6	CALL	SWPCSG		;;Restore CMD segment if clobbered by '??'
	JMPR	..2		;;Reissue prompt if ??, <ctrl-x> or <ctrl-u>
;
;	Free up space if sufficient amount left over.
;
..3:	LHLD	CMDBAS		;Compute optimal ceiling
	LXI	D,CMDSIZ+CMDSPR
	DAD	D		;HL = optimal ceiling
	LDED	CMDPUT		;DE = current ceiling
	CALL	MNHLDE		;DE = greater(current, optimal)
	LHLD	CMDRWF		;HL -> end of unused bytes
	CALL	SBHLDE		;BC = # superfluous bytes
	CNC	FREEDE		;Free them up, if any
;
	IFNOT	P8086, [
	RET
	] [
	POPA
	JMP	SWAPSG
	]
	EPROC	NEWCMD
	]			;<IF VPLUS>
	.PAGE
;
; NEWCMD - Gets next command line.  Update Command PUT pointer CMDPUT.
;
	IFNOT	VPLUS, [
				;{MAIN0,NXCMCH,ICMD,RICMD}
	BPROC	NEWCMD
NEWCMD:	CALL	RSTCON		;Reset Output for command lines
	TST$	SRCHFL		;Still waiting for terminator?
				;If 80H need to save buffer for "S" command
	JRM	..1		;"S" command running - don't empty buffer
;
;	Don't empty buffer if iterations executing
;
	TST$	ITRSTK
	JRNZ	..1
;
	MOVW	CMDGET,CMDBAS	;Set GET PTR to begin of command buffer
..1:	LHLD	CMDGET		;Value to reset CMDPUT
	SHLD	CMDPUT		;Set PUT PTR (not for * command)
;
	IF	FULL, [
	CLR	REGSTK		;Clear register execution stack offset
	DCR	A		;Get a 255.
	STA	REGEXN		;Flag not executing a register
	]
;
;	Give COMMAND: prompt and get input string into command buffer.
;
..2:	CALL	PROMPT		;Give the COMMAND: prompt
	CALL	CMDSTR		;Get inp string into CMDBUF
	JC	BREAK		;BREAK out on <ctrl-c>
	JRZ	..2		;Start over for <ctrl-x> & <ctrl-u>
	SHLD	CMDPUT		;Update command buffer's PUT pointer
;
;	Free up space if sufficient amount left over.
;
	LHLD	CMDBAS		;Compute optimal ceiling
	LXI	D,CMDSIZ+CMDSPR
	DAD	D		;HL = optimal ceiling
	LDED	CMDPUT		;DE = current ceiling
	CALL	MNHLDE		;DE = greater(current, optimal)
	LHLD	CMDRWF		;HL -> end of unused bytes
	CALL	SBHLDE		;BC = # superfluous bytes
	CNC	FREEDE		;Free them up, if any
	RET
	EPROC	NEWCMD
	]			;<IFNOT VPLUS>
	.PAGE
;
; PROMPT - Send appropriate message to console.		[3/23/86]
;
;	Normal prompt:  COMMAND: [00]  User may customize:  "*[00]" e.g.)
;	Help prompt:    Precedes normal prompt for 1st 3 prompts.
;	Unterminated string prompt:  Prompts with "-".
;
				;{NEWCMD}
	BPROC	PROMPT
PROMPT:	CALL	SV%BDH		;Save BC, DE and HL
;
;	Un-terminated string prompt?
;
	TST$	SRCHFL		;Still waiting for terminator?
	LXI	H,CNTPMT	;HL-> '-'
	JRNZ	..4		;Yes, give "-" prompt
				;Must already be in Command mode
;
;	Precede prompt by <<CR ?
;
	CPIW	LOGPOS,1	;At left edge of screen?
	BEQ	..1		;Yes, branch
	LXI	H,BKKPRM	;HL-> "<<"
	CALL	PRTMSG		;Issue <<CRLF
;
;	Precede prompt with short help message?
;
..1:	LXI	H,HLPCNT	;HL-> help down count
	MOV	A,M		;Get count
	ORA	A		;Is count zero?
	JRZ	..2		;Branch if count zero
	DCR	M		;Decrement count
	LXI	H,HLPPMT	;HL-> help prompt
	CALL	PCRMSG		;Give prompt
;
;
..2:
;;	TST$	WWMODE		;Is this visual mode window?
;;	JRZ	..3		;No, branch
;;	MVI	C,CR		;Yes, get harmless character
;;	CALL	PCHAR		;This will switch to command window
;
..3:
;
;	Display normal prompt.
;	Switch to command mode window if not already.
;
	LXI	H,CMDPMT	;HL -> command prompt
..4:	JMP	PRTSTR		;Send to console
	EPROC	PROMPT

	.PAGE
;
; CHKEKO - Get command line, check for "Echo-previous-command" '??'.
;
;	    Echos the previous command upto last command entered on "??"
;	    Breaks out if <ctrl-c> is entered.
;
;	    Returns:  Z for <ctrl-x>, <ctrl-u>, "??"
;		      Otherwise 'NZ', new command line & CMDPUT.
;
	IF	VPLUS, [
				;{NEWCMD}
NCMSAV	=	5
	DSEG	$
CMDSAV:	DS	NCMSAV
	CSEG	$

	BPROC	CHKEKO
CHKEKO:	LHLD	CMDBAS		;HL-> start of command buffer
	LXI	D,CMDSAV	;DE-> save buffer
	MVI	C,NCMSAV	;# bytes to save
	CALL	MOVE		;Save start of buffer in case of '??'
;
	CALL	CMDSTR		;Get input string into command buffer
	JC	BREAK		;BREAK out on <ctrl-c>
	RZ			;Return on <ctrl-x> or <ctrl-u>
;
;	Check for '??'.
;
	PUSH	H		;Save PUT ptr from CMDSTR
	LHLD	CMDBAS		;HL-> start of cmd buf
	LXI	D,'??'
	CALL	CMINDE		;Just '??' in cmd buf?
	POP	H		;Retrieve PUT ptr
	BEQ	..ECHO		;Yes, branch
	SHLD	CMDPUT		;No, set PUT ptr
	RET			;'NZ'
;
;	Echo buffer name and buffer contents upto GET of previous command.
;
;
..ECHO:	MOVB	REGNUM,OLDEXN	;Set REGNUM with previous command buffer #

	LXI	H,CMDSAV	;HL-> saved start of command buffer
	LDED	CMDBAS		;DE-> start of command buffer
	MVI	C,NCMSAV	;# bytes saved
	CALL	MOVE		;Save start of buffer in case of '??'

	CALL	EKONAM		;Display buffer name
	LDA	REGNUM		;Retrieve buffer number
	INR	A		;Command buffer?
	LES	D,CMDBAS	;ES:DE -> command buffer
	BEQ	..ECH1		;Yes, use DE

	CALL	PNTREG		;No, macro register but does it exist?
	JZ	..END		;No, quit after sending CRLF

..ECH1:	LHLD	OLDGET		;HL -> past last command or its terminator
	CALL	SBHLDE		;BC = # chars to echo, maybe + 1
	CP	RTCMD3		;Echo command string to console

..END:	CALL	CRLF		;Send CRLF and return
	XRA	A		;'Z' for NEWCMD
	RET
	EPROC	CHKEKO
;
; EKONAM - Print name of buffer in REGNUM on console:
;		<sp>(buffer designator)<cr><lf>
;
				;{EKOCMD}
	BPROC	EKONAM
EKONAM:	CALL	PSPACE
	MVI	A,'('
	CALL	PCHARA
	CALL	..1		;Display buffer name
	MVI	A,')'
	CALL	PCHARA		;Terminate message with ")"
	JMP	CRLF		;Send <cr><lf>
;
..1:	LDA	REGNUM		;A = register number
	CPI	MAINRG		;Main edit buffer?
	BEQ	..2		;Yes, let it pass
	CPI	USRGMX		;Valid user register # ?
	RNC			;No, return
;
..2:	MVI	A,'R'
	CALL	PCHARA		;Display 'R'
	LDA	REGNUM
	JMP	PRTBN0		;Convert to ASCII & display on console
	EPROC	EKONAM
	]			;<IF VPLUS>
	.PAGE
;
; GETSTR - Input string into area delimited by DE & HL, starting at DE.
;	   RETURN & <ESC><ESC> will always end string, but so may "??".
;
;	   Allows line-editing: Character erasing: <bs>, <rubout>
;				String erasing: <ctrl-x> <ctrl-u>
;				Next-char-literal: <ctrl-q>
;				Break-out:      <ctrl-c>
;
;	   Hierarchy for checking return flags:
;		'C' => <ctrl-c> encountered.
;	    	'Z' => <ctrl-x> or <ctrl-u> (also 'NC').
;		'NC','NZ' => success.
;
;	Usage of INPFLG:
;
;	Mask 01 - Expand RETURN to <CR><LF> and end line
;	Mask 02 - Allow <ESC><ESC> to end line
;	Mask 04 - Expand <CTRL-N> to <CR><LF>; don't end line
;	Mask 08 - Enable line editing and CTRL-Q as next-char-literal
;	Mask 10 - Allow "??" to end line - VPLUS only
;	Mask 20 - Break out if <CTRL-C> or [CANCEL] pressed
;	Mask 40 - Attempt to get more space if necessary
;	Mask 80 - GETCHR():  visual mode
;
				;{NEWCMD}
CMDSTR:	LHLD	CMDMAX		;Upper limit of command buffer
	LDED	CMDPUT		;Begin of input buffer
	MVIB$	INPFLG,7FH
	CALL	GETSET		;Save entry regs as PTRs
	JMPR	GETSLP

				;{GETSTR,CMDSTR}
GETSET:	CLR	PRVFLG		;Clear previous-char flag
	SDED	STRFLR		;+Set floor for string space
	SDED	STRPUT		;+Set initial PUT pointer
	DCX$	H		;Leave room for <cr><lf> & <esc><esc>
	DCX$	H
	SHLD	STRCEL		;Set ceiling for string space
;
;	Want to expand CR and LF as <CR> and <LF>
;
	LDA	LSTFLG		;Get current LSTFLG
	ORI	20H		;Bit to expand for <CTRL-N>
	CALL	SETLST		;Set new LSTFLG
	JMP	RSTPOS		;Reset LOGPOS = 1
;
				;{RQCMD,XQCMD}
	BPROC	GETSTR
GETSTR:	MVI	A,2DH		;Normal value to add CR-LF to buffer
	JMPR	GETST1		;Merge
;
				;{VGTSTR}
GETST0:	MVI	A,2CH		;Value not to add CR-LF to buffer
GETST1:	STA	INPFLG		;Save input conversion flag
	CALL	GETSET		;Initialize parameters
;
	IF	VPLUS, [
	CALL	GETCHR		;Temporary (?) kludge to handle input redirection
	MOV	A,C		;
	CPI	LF		;Initial LF from command file?
	BNE	GETST2		;No, use it;  else get next char
	]
;
GETSLP:	CALL	GETCHR		;Get console char
GETST2:	LXI	H,'T'*256+'R'	;HL = "RT"
	CALL	CMHLBC		;Is this a \RT\?
	JRNZ	..1		;No, branch
	MVI	C,CR		;Yes, set C = <CR>
				;This is needed for command mode to work
				;with keystroke macros
..1:	CPIM	PRVFLG,CTRLQ	;Is this char to be taken literally?
	JZ	..CLIT		;Yes, branch

	LDA	AUXESC		;Get alternate ESC char
	CMP	C		;Have it?
	BNE	..2		;No, branch
	MVI	C,ESC		;Yes, convert to ESC
;
..2:	MOV	A,C		;Get char back
	CPI	ESC
	JZ	..ESC

	IF	VPLUS, [
	CPI	'?'
	JZ	..ESC
	]

	MOV	M,A		;Else save current char in PRVFLG
;
	LDED$	STRFLR		;DE -> floor of string buffer
	LHLD	STRPUT		;HL -> next free string byte
	CPI	CR
	JZ	..CR
	CPI	CTRLQ
	JZ	..ECHO		;Just echo, PRVFLG already set
	CPI	CTRLN
	BEQ	..CRLF
	CPI	07FH		; <RUBOUT>  ?
	JZ	..BS
	CPI	BS
	JZ	..BS
	CPI	CTRLU
	JZ	..DEL		;Yes, delete line
	CPI	CTRLX		; <CTRL-X> ?
	JZ	..DEL		;Yes, treat same as CTRL-U
	CPI	CTRLC		; <CTRL-C> ?
	JZ	..ABRT
	TST$	CNCLFL		;;Is the cancel flag set?
	JNZ	..ABRT		;;Yes, treat as CTRL-C
;
;	Check whether there is room for another character or two.
;
..3:	LDED	STRCEL		;+DE -> string space ceiling
	CALL	CMHLDE		;Room? Compare: PUT - Ceiling
	BLT	..RM		;Yes, branch
;
;	No room, try to get more if enabled.
;
	ANIB$	INPFLG,40H	;OK to try?
	JRZ	..NR		;No, branch

	PUSH	B		;Yes, save overflow char in C
	LXI	B,CMDSIZ	;Try to make CMDBUF bytes available
	LHLD	STRPUT		;At end of current string
	CALL	MALLHL
	MOV	A,B
	ORA	C		;Get any?
	JRZ	..NO		;No, branch
	SHLD	STRPUT		;Yes, set new ceiling
	LHLD	STRFLR
	DSUB	B
	SHLD	STRFLR
	POP	B		;Retrieve overflow char
	MOV	A,C		;A = overflow char
;
;	Room, so enter into input buffer.
;
..RM:	CALL	FORCML		;Save char in buffer, echo to console
..JJ:	JMP	GETSLP		;Get next char
;
;	No room, send BEL char to terminal.
;
..NO:	POP	B		;Adjust stack
..NR:	CALL	WRTBEL		;Output <BEL> to console
	JMPR	..JJ		;Get next char
;
;	Handle special characters
;
..CRLF:	ANIB$	INPFLG,04	;Add <CR><LF> to buffer?
	JZ	..3		;No, just add <CTRL-N>
	CALL	..ADCR		;Yes add the CR and LF
	JMPR	..JJ		;Continue
;
..ADCR:	MVI	C,CR		;Be sure we have CR
	CALL	FORCML		;Yes, buffer and echo CR
	MVI	C,LF		;Append LF to CR
	JMPR8	FORCML		;Buffer and echo LF
;
..ESC:	CMP	M		;Was previous char. an ESC or "?"
	MOV	M,C		;Save current char in PRVFLG
	BNE	..3		;No, add first ESC or "?" to buffer

	IF	VPLUS, [
	CPI	'?'		;Is it special '?' for possible echo-command?
	BNE	..NQM		;No, branch
;
	ANIB$	INPFLG,10	;End line on "??"?
	JRZ	..3		;No, banch
;
	CALL	FORCML		;Buffer and echo second "?"
	DCX$	H		;Backup to first "?"
	DCX$	H
	LDED	CMDBAS
	CALL	CMHLDE		;At begin of input buffer?
	JRNZ	..JJ		;No, nothing special
	JMPR	..CEND		;Restore LSTFLG, send CRLF and return
	]
;
..NQM:	ANIB$	INPFLG,02	;End line on <ESC><ESC>?
	JZ	..3		;No, just add to buffer
	CALL	FORCML		;Yes, buffer and echo second ESC
;
..CEND:	CALL	RSTOUT		;Restore normal LSTFLG
	CALL	CRLF		;Move to next line
	JMPR	..END		;Return
;
..ECHO:	CALL	PCHARA		;Echo to console
	JMPR	..JJ		;Get next char
;
..CLIT:	MVI	M,00		;Clear previous char flag
	MVI	A,3BH		;Expand everything, don't stop for CTRL-S
	CALL	SETLST		;Set new LSTFLG
	CALL	FORCML		;Enter char into buffer, echo
	CALL	RSTOUT		;Reset LSTFLG
	JMPR	..JJ
;
..DEL:	MVI	A,'#'		;Get delete line char
	CALL	PCHARA		;Display it
	CALL	RSTOUT		;Restore normal LSTFLG
	CALL	CRLF		;Move to next line
	XRA	A		;Set 'Z', 'NC'
	RET			;Return
;
..BS:	CALL	DOBS		;Perform the BS
	SHLD	STRPUT
	JMPR8	..JJ		;Get next command char
;
..ABRT:	ANIB$	INPFLG,20H	;Abort on <CTRL-C>?
	JZ	..3		;No, just add to input buffer
	CALL	RSTOUT		;Yes, first restore LSTFLG and ROUTAD
	STC
	RET			;Yes, return 'C'
;
..CR:	CALL	RSTOUT		;Restore previous LSTFLG
				;Don't want to show CR-LF
	ANIB$	INPFLG,01	;Add <CR><LF> to buffer?
	CNZ	..ADCR		;Yes, add CRLF to buffer
;
..END:	ORI	1		;'NC','NZ'
	RET
	EPROC	GETSTR
;
; WRTBEL - Output <BEL> to console.
;
				;{GETSTR}
WRTBEL:	MVI	C,CTRLG		;<ctrl-g> = <bel>
	JMP	CONOUT		;Output it
;
; FORCML - Enter char in %C into input buffer at (STRPUT++).
;	   If input buffer is Search buffer, check for UC conversion.
;
	BPROC	FORCML
FORCML:	LHLD	STRPUT
	LXI	D,TARGST	;DE-> begin of search buffer
	CALL	CMHLDE		;Is this search string?
	BLT	..1		;No, branch
	LXI	D,TARGST+TARGLN ;DE-> past end of buffer
	CALL	CMHLDE		;Is this search string?
	BGE	..1		;No, branch

	CALL	CHKUCC		;Yes, convert char, enter at (HL)
	MOV	C,A		;Put converted char into C
	JMPR	..2
;
..1:	MOV	M,C		;Append character to end of string
	INX$	H		;Bump pointer
..2:	SHLD	STRPUT		;Save new pointer
	JMP	PCHAR		;Print char and return
	EPROC	FORCML
	.PAGE
;
; DOBS - Perform the backspace on desired command line.
;	 Entry: DE-> begin of command line, HL = command line PUT Ptr
;	 Exit:	HL backed up, DE saved.
;
				;{GETSTR,VGTBS}
	BPROC	DOBS
DOBS:	CALL	BSCCNT		;B = # positions to back up
	RZ			;Do nothing if line is empty
	PUSH	H		;Save backed up PUT Ptr
;
..1:	PUSH	B		;Save count
	LXI	H,BSSTRG	;HL-> [BS] [SPACE] [BS]
	CALL	PRTSTR		;Send string to console
	POP	B		;B = count
	DJNZ	..1		;Repeat
;
	POP	H
	RET
	EPROC	DOBS
;
; BSCCNT - Compute # of screen position to backup to perform BackSpace
;	   Enter: DE-> begin of command line, HL = PUT Ptr
;	   Retrn: 'Z' if line is empty, else HL is backed up
;		  B = # positions to backup; DE is saved
;
				;{DOBS}
BSCCNT:	CALL	CMHLDE		;Is the command line empty?
	RZ			;Return 'Z' is line is empty
;
	DCX$	H		;Backup PUT Ptr
	PUSH	H		;Save PTR
	PUSH	D		;Save DE
	XCHG			;HL-> begin of line, DE-> last char after BS
	CALL	GETLP0		;Compute HL = logical pos for last char
;
	XCHG			;DE = position after BS
	LHLD	LOGPOS		;HL = current line pos
	CALL	SBHLDE		;BC = amount to move back
	MOV	B,C		;Put value in B
	POP	D		;Restore DE
	POP	H		;Restore backed up PUT Ptr
				;Wierd if 'Z' here, but would be OK
	RET

