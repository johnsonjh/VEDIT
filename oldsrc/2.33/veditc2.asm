	.PAGE
	.TITLE	'VEDITC2'
;************************************************
;*						*
;*		Edit Commands			*
;*						*
;************************************************
;
; Copyright (C) 1986 by CompuView Products, Inc.
;			1955 Pauline Blvd.
;			Ann Arbor, MI 48103
;
;	Written by: Theodore J. Green
;
;	Last Change: Ted - Nov. 12, 1985 - YK and YL
;			   Nov. 13, 1985 - AUXCHR
;			   Jan. 27, 1986 - Changes for new GETDEC
;
;		     Tom - Feb. 07 - VEDIT/VPLUS merged via assm'y sw
;			   Feb. 18 - 1.40a ED,EK: FNF handled more smoothly
;
;		     Ted - Feb. 04 - Changes for ++ command buffer
;			   Feb. 28 - YACMD
;
;		     Tom - Mar. 07 - EBCMD:  SETIOX moved to F1
;				     Displays I/O names if just <cr> or <esc>
;			   Mar. 08 - ER, EW likewise
;
;		     Ted - Mar. 23 - Use VPLUS HCMD and ICMD
;			   Apr. 17 - EG cmd fixed
;			   Apr. 27 - HCMD updated
;			   May  08 - Use AXREAD() in AUXCHR,RLCMD and EGCMD
;				   - Fix RD, RT and RP commands
;			   May  10 - PCMD
;
;		     Tom - May  08 - TCMD, EOCMD
;			 - May  09 - Global K cmd (D cmd affected)
;				     GLBCMD renamed GLOBHL, altered for K cmd
;				     GLOB0 & GLOBX entry points added to GLOBHL
;			 - May  12 - YMCMD finds matching group delimiter
;			 - May  15 - PARMFL set by cmds expecting a parameter string to follow
;				     Fixes early macro popping bug when last command had a null parameter
;				     (numbers are properly handled by EXPR for VPLUS)
;
;				     Includes H,YT,EQ,EU,EX+,EZ
;				     RL,RS,RX,ED,EG,EL,EK
;				     via CKSPRM for EB,ER,EW
;				     via YTCMD for RQ,XK,XQ
;				     via ESCMD1 for ES,EP,PP
;				   - YKCMD & YLCMD moved in from IO, optimized
;				     VEDIT PLUS bug fixed (AUXREG needs setting)
;			 - May  16 - FCMD set SAVEPT -> other end of search string
;			 - May  22 - XK(ctrl-c) BREAKs, +XK(ctrl-c) doesn't
;			 - Aug. 19 - EQCMD,EXCMD,EXTCHK to handle EQAY
;				   - ESTBL, EPTBL, TABTBL  (SWTBL,PRMTBL,TABPOS only for INSTALL)
;			 - Aug. 20 - EKCMD clear COLFLG (:ED but not :EK)
;				   - RACMD (new) for auto-execution
;			   Sep. 11 - ESCMD1() turn off HW cursor when done
;			   Sep. 16 - EXTCHK:  +eq, +ex exit VEDIT if only empty '@' buffer left
;			   Sep. 17 - +EP, +ES: Change Installed values as well as for current edit buffer
;
;		     Ted - Sep. 18 - EECMD2 calls STUPFL() to update window later
;		     Tom - Sep. 19 - +ET & EFY with warning prompt.
;			   Oct. 10 - ENCMD() get > amount specified.
;			   Oct. 15 - EECMD() use PNTTRG() not PNTREG()
;		     Ted - Oct. 16 - EQCMD() force window clear, reset .WWMODE
;			   Oct. 24 - Moved Help routine to C3
;		     Tom - Nov. 01 - YKCMD1 bug fix
;		     Ted - Nov. 12 - Fix "-F" for |< and |>
;		     Tom - Nov. 23 - RSCMD(), EXCMD() write/close error handling
;			 - Nov. 25 - DSKFUL,WRITOK cleared at ECMD()
;		     Ted - Dec. 04 - RPCMD calls ENDLST
;
;	A - APPEND
;
ACMD:	CPIB$	RMINUS,'-'	;Is it negative?
	JZ	RDPREV		;Yes, read in text backwards
;
	CALL	STNSNL		;;Make sure screen updated
	CALL	RESTOP		;No, restore character, HL = TXTCEL
	CALL	GETITR		;Get the iteration count:  auto-read (0A)?
	JRNZ	APPEN4		;No, skip
;
;	For 0A, perform an auto-read.  (3/25/85)
;
;	CLR$	AMTFLG		;Yes, specify full auto-read
;	CALL	GETAUT		;Set size-dependent # bytes to reserve
	JMP	AUTORD		;Do it
;
;	Read more of disk file if there is room.  (3/18/85,2/18/86)
;
APPEN3:	CALL	INRDCH		;End of input file reached?
	JRZ	SETTOP		;Yes, set pointer to end of file
	PUSH	H		;No, save current TXTRWF
	LXI	D,-128		;Need room for one sector
	CALL	NEEDTX		;Is there room?		[2/18/86]
	POP	H		;HL = current TXTRWF
	JRC	APPEN5		;No, set pointers
;
	PUSH	H		;Save again
	LXI	B,1		;Read one sector
	CALL	READTX		;Read it
	POP	H		;HL-> begin of sector just read
;
;	Count lines until EOT or specified count reached.
;
APPEN4:	MOV	A,M		;Get next char
	CPI	EOF		;At end of text?
	BEQ	APPEN3		;Yes, read more
;
	INX$	H		;No, bump pointer
	CPI	LF		;LF?
	BNE	APPEN4		;No, keep looking
;
	CALL	DECITR		;Yes, decrement iteration count
	JRNZ	APPEN4		;If not zero, keep going
	JMPR	SETTOP		;Otherwise set TXTCEL, TOPCHR
;
;	Fix fragmentary line when FULL condition encountered.
;
APPEN5:	CALL	EOLLIN
;	JMP	SETTOP
;
;	Number of Line Feeds has been found, so update pointers.
;
				;{NEXFIL,BR8K,ACMD^}
SETTOP:	SHLD	TXTCEL		;Save top of window pointer
	MOV	A,M		;Get the character there
	STA	TOPCHR		;Save it
	MVI	M,EOF		;Change to text delimiter
	XRA	A		;Return with 'Z'
	RET

RESTOP:	LHLD	TXTCEL		;HL-> end of text window
	LDA	TOPCHR		;Get real char
	MOV	M,A		;Change delimiter to real char
	RET			;Return. (Close files)
;
;	B - BEGINNING
;
	BPROC	BCMD
BCMD:	MVIB$	MINUS,'-'	;Set reverse flag
..1:	CALL	CHKGLB		;Previous text to be read in?
	JRZ	..1		;Yes, go get it
	LHLD	TXTFLR		;HL-> begin buffer/file
	JMPR	SETEPT		;Set new value for EDTPTR
	EPROC	BCMD
;
;	C - CHANGE edit pointer
;
CCMD:	CALL	CHGPTR		;Compute new position
;	JMP	SETEPT		;Set new edit pointer
;
; SETEPT - Store HL into EDTPTR.
;
				;{CCMD^, EICMD, EJCMD}
SETEPT:	SHLD	EDTPTR		;Save as edit PTR
	RET
;
;	D - DELETE
;
DCMD:
	IF	VPLUS, [
	CPIB$	NM2FLG,COMMA	;2-parameters?
	JZ	KCMD		;Yes, use already implemented KCMD
	]

	CALL	CHGPTR		;HL-> beyond deleted chars
	JMP	PAKTX0		;Do it
;
;	E - EXTENDED commands.	[1/28/86]
;
ECMD:	CLR	DSKFUL		;;For EOFCHK()/CHKRVB() in V3
	INR	A
	STA	WRITOK
	CALL	NXCMCH		;Get second letter of command
	LXI	H,EXTTBL	;HL-> extended command table
	LXI	D,C2BR8K	;Error processing routine & BR8K out
	JMP	DISPT2		;Dispatch to command
;
;	N - NEXT  Find next occurrence of string, searching file.
;
NCMD:	MVIB$	GLBFLG,'_'	;Set global flag
;	JMP	FCMD		;Jump to "F" command
;
;	F - FIND
;
	BPROC	FCMD
FCMD:
	IFNOT	VPLUS, [
	STA	MINUS		;Clear reverse flag for CHKGLB
	]

	CALL	STSEAR		;Setup for forward search

	IF	VPLUS, [
				;{YMCMD}
FCMD0:	TST$	MINUS		;Is move negative?
	JRNZ	FBACK		;Yes, branch
	]

..1:	CALL	SEARTX		;Perform search in text buffer
	JRC	..2		;Branch if string not found
	SHLD	EDTPTR		;Save pointer past string
	SDED	SAVEPT		;Save -> to start of string
	CALL	DECITR		;Decrement remaining iteration
	RZ			;Return if iteration now zero
	JMPR	..1		;Continue
;
..2:	CALL	CHKGLB		;Global search & more text from disk?
	JRZ	..1		;Yes, continue searching
	JMP	NOTSUC		;No, EOF reached, give error
;
;	Backwards searching
;
	IF	VPLUS, [
FBACK:	CALL	SETBOT		;Put fence EOF ahead of [TXTFLR]
..BK1:	LHLD	EDTPTR		;HL-> current edit position
	INX$	H		;;Account for DCX H
..BK2:	DCX$	H		;;HL-> next char to begin search
	LDED	TXTFLR		;HL-> start of visible text
	CALL	CMHLDE		;Before begin of buffer?
	XCHG			;;DE-> text for search
	JRC	..BK3		;Yes, error unless auto-buffering
;
;	Attempt to match
;
	PUSH	D		;Save position
	CALL	MACHT0		;Match current position?
	POP	H		;HL-> current position
				;DE-> past end of matched string
	JRNC	..BK2		;No, try again
;
;	For success, entire string must precede edit point.
;	Note: |< and |> may equal old EDTPTR.
;
	LBCD	EDTPTR		;BC = fence
	CALL	CMBCDE		;End of string occur before edit point?
	JRC	..BK2		;No, try again
;
;	Success
;
	XCHG			;DE-> current position, HL-> past end of match
	SDED	EDTPTR		;Yes, set new edit position
	SHLD	SAVEPT		;Save ptr past end of string
	CALL	SEADN2		;Set # chars matched
	CALL	DECITR		;Look for more occurrences?
	JRZ	CHKBOT		;No, restore BOTCHR and return
	JMPR	..BK1		;Yes, continue
;
;	Failed in this buffer.  Check more?
;
..BK3:	CALL	SEAERR		;Set SRFAIL, SRCERR
	CALL	CHKGLB		;More text from disk & global search?
	LHLD	SRFAIL		;Retrieve adjusted search position
	JRZ	..BK2		;Yes, continue searching
	CALL	CHKBOT		;No, restore text buffer
	JMP	NOTSUC		;Start of buffer/file reached, give error
	EPROC	FCMD
;
; SETBOT - Set fence ahead of [TXTFLR], saving that byte in BOTCHR.
;
				;{FBACK,CHKGLB}
SETBOT:	LHLD	TXTFLR		;HL-> 1st char in text buffer
	DCX$	H
	MOV	A,M		;Save preceeding char
	STA	BOTCHR
	MVI	M,EOF		;Set fence char
	RET
;
; CHKBOT - Check whether the char preceeding [TXTFLR] needs to be restored.
;	   Restore it if so.  Returns 'Z' for BOTCHR restored.
;
				;{FBACK,CHKGLB,BREAK}
CHKBOT:	LHLD	TXTFLR
	DCX$	H
	MOV	A,M
	CPI	EOF
	RNZ
	LDA	BOTCHR
	MOV	M,A
	RET
	]			;IF VPLUS
;
;	Gr - Get register (obs)
;
GCMD:	MVI	A,'G'
	JMP	RCMD0		;Now Gr is RGr
;
;	H - Menu driven on-line help.
;
HCMD:	LXI	H,VHELP		;HL-> "VHELP.HLP" or "VPHELP.HLP"
	JMP	HELPEX		;Jump to help

;
;	I - INSERT
;
	BPROC	ICMD
ICMD:	CALL	SETTRM		;Set terminating char

	IF	VPLUS, [
	CALL	CHKITR		;Overwrite? (Argument LE 0)
	BEQ	IOVCMD		;Yes, branch
	]

				;{SCMD - sets DELPTR beyond EDTPNT}
ICMD1:	CALL	INSLEN		;BC = # bytes to be inserted, #I
	LHLD	DELPTR		;HL -> next valid text byte
	LDED	EDTPTR		;+DE -> insertion point = ->I
	PUSH	B		;Save #I
	PUSH	D		;Save ->I
	DSUB	D		;HL = # deleted text bytes
	DSUB	B		;HL = -# bytes needed for insertion = -n
	JRNC	..2		;Branch if no extra bytes needed
	XCHG			;DE = -n
	PUSH	D		;Save -n
	CALL	NEEDTX		;Room?
	POP	B		;BC = -n
	JC	BREAK		;No, break out
;
	CALL	NEGBC		;Yes, BC = n
	LHLD	DELPTR		;HL -> floor of text to be moved up
	PUSH	H		;Save
	DAD	B		;Add movement
	SHLD	DELPTR		;Update DELPTR.  This is needed in case
				;of multi-line insert string
	POP	H		;Restore
	CALL	BUFFUP		;Move the text up, adjust pointers
;
;	Move the insert string to the text buffer.  If terminator is
;	reached command is over.  Else must get another command line.
;
..2:	POP	D		;DE -> Insertion point
	POP	B		;BC = # bytes to be inserted
;
	CALL	OVERWT		;Copy in new text from command buffer
	JC	ENDTRM		;Return if terminator reached
;
;	LF in command string reached, so get another command line
;
	CALL	NEWCMD		;Get another command line
	JMPR	ICMD1		;Insert the rest
	EPROC	ICMD
;
;	-I => Overlay text
;
	IF	VPLUS, [
IOVCMD:	CALL	INSLEN		;BC = # bytes to be inserted, #I
	LHLD	EDTPTR		;Overlay at EDTPTR
	PUSH	H		;Save
	DAD	B		;HL-> past end of overlay
	XCHG			;DE-> past end of overlay
	LHLD	TXTCEL		;HL-> past top of text window
	CALL	CMHLDE		;Would overlay go too far?
	POP	D		;DE = EDTPTR
	JC	BREAK		;Yes, BREAK OUT!
	CALL	OVERWT		;No, copy in new text from command buffer
	JC	ENDTRM		;Return if terminator reached
;
;	LF in command string reached, so get another command line
;
	CALL	NEWCMD		;Get another command line
	JMPR	IOVCMD		;Overlay the rest
	]			;<IF VPLUS>

	.PAGE
	IF	VPLUS, [
;
;	J - Jump commands.
;
JCMD:	CALL	NXCMUC		;;Get second command char; convert to UC
	CPI	'P'		;Jump to specified label?
	JZ	JPCMD
;
;	Check whether to proceed further (Condition = true).
;
	MOV	C,A		;Save command char in C
	PUSH	B
	CALL	GETITR		;Condition = true?
	POP	B		;C = command char
	RZ			;No, quit
	MOV	A,C		;Yes, restore command char
;
	CPI	'L'		;Exit current LOOP clause?
	JZ	ENDITR
	CPI	'M'		;Exit current register macro?
	JZ	JMCMD
	CPI	'N'		;Start next LOOP iteration?
	JZ	JNCMD
	CPI	'O'		;Exit to command prompt?
	JZ	JOCMD
	JMP	C2BR8K		;No, give error and BR8K out
;
;	cJM - Un/conditional jump out of current register macro.
;
				;{RCCMD}
JMCMD:	CALL	POPUNC		;Discard open clauses from current macro
	MOV	M,D		;Set ITRSTK offset to new top
	CALL	POPCMD		;Discard pointers to current register macro
	RNC			;Ok if there was a macro to pop
;6	MOV	ES,CMDSEG
;6	DB	26H		;ES: override
	MOVW	CMDGET,CMDBAS	;Otherwise, fake-pop the empty command buffer
	SHLD	CMDPUT
;6	MOV	CMDGET+2,ES
;6	MOV	CMDPUT+2,ES
	MVIB$	REGEXN,0FFH
	RET
;
;
;	cJN - Un/conditional jump to start of next LOOP iteration.
;
JNCMD:	CALL	POPITR		;Get current clause from stack
	RC			;Ignore if stack is empty
	STA	REGNUM		;Save parent macro name
	MOV	A,D		;THEN clause?
	ORA	E		;'Z' if so
	BEQ	JNCMD		;Yes, keep popping clauses from the stack
	DCX$	D		;No.  Check for ELSE
	MOV	A,D		;GET = 1 => ELSE clause
	ORA	E		;ELSE?
	BEQ	JNCMD		;Yes, keep popping clauses
	INX$	D		;No, restore GET value
	SDED	CMDGET		;+Set GET to start of the REPEAT loop
	MOV	A,B		;REPEAT count bottomed out yet?
	ORA	C		;'Z' if so
	JZ	SCNGIT		;Yes, move to end of the REPEAT loop
	DCX$	B		;No, decrement count
	LDA	REGNUM		;Retrieve parent macro name saved above
	JMP	PSHITR		;Push REPEAT clause back onto stack
;
;	eJO - Un/conditional jump out to command-prompt level.
;
JOCMD:	LXI	H,NULMSG	;User can supply any needed message
	JMP	MSGBRK
;
;	eJPstring - Un/conditional jump to label !string!.
;
	BPROC	JPCMD
JPCMD:	MVIB$	PARMFL,0FFH	;Inform NXCMCH() a string parameter is expected
	CALL	STSEAR		;Setup search for label (less the !
	LXI	D,ILDMSG	;Error message
	LXI	H,36		;Offset to byte to be packed
	CPIB	TERMCH,'!'	;Illegal delimiter?
	JZ	PAKBRK		;Yes, print message and break out
	CALL	GETITR		;No.  Is condition true?
	RZ			;No, continue processing
	CALL	GETSLM		;Yes, branch.  Get ES:DE-> text to be searched
	PUSH	D		;Save for checking clause-level
..1:	INX$	D		;Skip label delimiting !
	LHLD	NFOUND		;Get value for .N
	PUSH	H		;Save
	CALL	SEARTG		;Perform the search
	XTHL			;HL = old .N; -> on stack
	SHLD	NFOUND		;Restore value for .N
	POP	H		;HL = return from SEATX1
	JRC	..3		;Branch if label not found
	MVI	A,'!'		;Really wanted to look for dlabeld
	CMP	M		;Is succeeding char the !
	BNE	..1		;No, advance scan point & try again
	XCHG			;HL -> start of possible label
	DCX$	H		;HL -> ahead of the label
	CMP	M		;Is preceeding char the !
	XCHG			;DE-> before 1st label char, HL-> final !
	INX	D		;DE -> 1st label char
	BNE	..1		;No, not a label, advance scan pont & retry
	INX$	H		;Yes, found the label, set HL -> past !
	SHLD	CMDGET		;Set new position in command string
;
;	Adjust clause stack    (Branching into a clause is illegal!)
;
	XCHG			;HL -> last byte that needs to be scanned
	POP	D		;DE -> start of command string
	CALL	SBHLDE		;BC = # bytes to check in computing level
	CALL	CWNTLV		;HL = level of clause containing the label
	CALL	POPUNC		;Discard higher level open clauses
				;HL -> ITRSTK, B = offset to new stack top
	LXI	D,BRINMS	;Illegal to branch into a clause structure
	JC	MACBRK		;Possible non in/ex-clusive reason
	MOV	M,B		;Top level clauses now popped off ITRSTK
	IFNOT	P8086, [
	RET
	] [
	JMPR	..END
	]
;
..3:	POP	PSW		;Adjust stack
	CPIB$	COLFLG,':'	;Non-fatal error for missing label?

	IFNOT	P8086, [
	JZ	SETERR		;Yes, set error flag and return
	] [
	JRZ	..4
	]

	LXI	H,DEFDMA	;Build non-highlighted !label! string
	APENDC	'!'
	APENDS	TARGST,EOF
	APENDC	'!'
	MOV	M,C		;Set EOF marker at end of string

	LXI	H,LBMSNG	;HL-> "Label missing: "
	LXI	D,DEFDMA	;DE-> !label!
	MVI	A,0CCH		;CRLF() fore/aft of msg (cm)
				;2 msgs, terminators in B/C
	CALL	MSGHN7		;Output msg, @ col 7 if vm
	LXI	H,BRKMSG
	MVI	A,0D1H
	JMP	BRK0		;Break out
;6;
;6#4:	CALL	SETERR
;6#END:	JMPL	RSTSEG
	EPROC	JPCMD
;
SAPEND:	MOV	A,M
	CMP	C
	RZ
	STAX	D
	INX	H
	INX	D
	JMPR	SAPEND
	]
	.PAGE
;
;	K - KILL
;
KCMD:
	IF	VPLUS, [
	LXI	H,PKTX00	;HL-> executive routine
	JMP	GLOB0		;Process globally if required
	]

	IFNOT	VPLUS, [
	CALL	FNDLIN		;HL-> beyond deleted chars
	JMP	PAKTX0		;Move remaining text down
	]
;
;	L - Move edit pointer number of lines.  [7/24/85]
;
				;{RESTRT}
	BPROC	LCMD
LCMD:	CALL	FNDLIN		;Compute new PTR
	SHLD	EDTPTR		;Set the new edit PTR
	JRC	..1		;Branch when attemp to go past buffer end
;
;	At end of buffer?
;
	MOV	A,M		;Get char at edit point
	CPI	EOF		;End of buffer marker?
	RNZ			;No, return
	JMP	CHKGLB		;Yes, try to read more text so cursor is not only
				;past previous line but at head of current line
;
;	Attempted to pass buffer end.
;
..1:	CALL	CHKGLB		;Able to read more text in?
	JRZ	LCMD		;Yes, continue processing
;
;	Unable to fulfill request?
;
	CALL	GETITR		;BC = # occurrences left to search for
	RZ			;Return if none
	CPIB$	PNDFLG,'#'	;Move past 'ALL'?
	RZ			;Yes, OK
;
;	Unable to fulfill request.
;
	MVI	E,0		;E=0 => from LCMD
	JMP	ERRHND		;Process the error
	EPROC	LCMD
;
;	M - MACRO execute.	(2/14/85) [8086]
;
MCMD:	CALL	GETCRN		;Get text register number
;
;	Returns 'Z', 'NZ' for NEWCMD.
				;{AUTEXE,BEGIN,BR8K+(init),VMAC,NEWCMD-}
MCMD1:				;{RJCMD+,RXCMD+,SDFSET+(|p)}
	IF	VPLUS, [
	CMPMB	EDTNUM,REGNUM	;Trying to execute current edit buffer?
	JZ	BUFBRK		;Yes, don't allow!
	]
	CALL	PNTREG		;Does register exist? Registers BDH set if so
	RZ			;No, quit.  'Z' for NEWCMD
;
MCMD2:				;{NXTCHR+}
	CALL	PSHCMD		;Save current REGEXN,CMDPUT,CMDGET on REGSTK
				;Does not return if error
	XCHG			;SVCMPT() wants them reversed
	LDA	REGNUM		;Value for new REGEXN

	IFNOT	VPLUS, [
	JMP	SVCMPT		;Set new CMDGET, CMDPUT and REGEXN
	]

	IF	VPLUS, [
	CALL	SVCMPT		;Set new CMDGET, CMDPUT and REGEXN
	ORI	1		;'NZ' for NEWCMD
	RET
	]			;IF VPLUS
;
;	P - Print Commands.
;
PCMD:	CALL	NXCMUC		;;Get second command char; convert to UC
	CPI	'E'		;Eject?
	JZ	PEJECT
	CPI	'P'		;Parameter?
	BEQ	PPCMD
	CPI	'R'		;Print?
	BEQ	PRCMD
	JMP	C2BR8K		;Else give error

;	[5/13/86]
;
PRCMD:	LDA	LVLLST		;Print control flag
	CALL	SETLST		;Set new value, save old ones
	LXI	H,LSTBLK	;Routine to print the block
	CALL	GLOBX		;Do it, globally if requested
	JMP	RSTOUT		;Restore prevous LSTFLG & LSTOUT
;
PPCMD:	LXI	H,PRNTBL
	LXI	D,PRNTBL	;DE-> parameter table
	LXI	B,PRNMNM	;C = max. parameter number
	JMP	ESCMD1		;Merge in below to set new parameter
;
;	Qr - Display numerical register r on console.
;	     Only comes here when two or more registers are being
;	     displayed at once:   Q1:Q2,...,Qn
;
	IF	VPLUS, [
QCMD:	CALL	CALC		;Display previous numerical register
	JMP	BAKGET		;Back up the command GET pointer
	]			;IF VPLUS
	.PAGE
;
;	REGISTER commands.
;
RCMD:	CALL	NXCMUC		;;Get second character; convert to UC
	CPI	'*'		;Comment?
	JZ	RCOMNT
	CPI	'U'		;Display register lengths?
	JZ	RUCMD		;Yup
;
				;{GCMD}
RCMD0:	PUSHA			;Save
	CALL	GETCRN		;A = REGNUM = text register number

	IF	VPLUS, [
	STA	AUXREG		;Save also in AUXREG
	]

	POPA			;Restore second character
;
	IF	P8086, [
	CALL	RCMD1
	JMP	RSTSEG
RCMD1:
	]
;
	LXI	H,RDEC		;HL-> "R" command decode table
	CALL	CHOICE		;HL = address, 'C' if not found
	JC	C2BR8K		;No, give error and BR8K out
	PCHL			;Jump to routine
;
; RDEC - Decode table for "R" command
;
RDEC:	DB	'C'		;Copy to register?
	DW	RCCMD
	DB	'D'		;Dump?
	DW	RDCMD
	DB	'E'		;Empty out register?
	DW	RECMD
	DB	'G'		;Get text ?
	DW	RGCMD
	DB	'I'		;Insert directly?
	DW	RICMD
	DB	'L'		;Load ?
	DW	RLCMD
	DB	'P'		;Print?
	DW	RPCMD
	DB	'S'		;Save ?
	DW	RSCMD
	DB	'T'		;Type?
	DW	RTCMD
	DB	'Z'		;Zero out/zap register?
	DW	RZCMD
;
	IF	VPLUS, [
	DB	'A'		;Auto-execute register?
	DW	RACMD
	DB	'J'		;Jump to register macro?
	DW	RJCMD
	DB	'M'		;Register to edit buffer direct compare?
	DW	RMCMD
	DB	'Q'		;Queried input into register?
	DW	RQCMD
	DB	'X'		;Load & Execute?
	DW	RXCMD
	]
;
	DB	KFF		;End of table
;
; R* - comment line.
;
RCOMNT:	CALL	NXCMCH		;Get next string char
	CPI	LF		;Reached end of line?
	BNE	RCOMNT		;No, keep looking
	RET			;Yes return
;
;	RA - Auto-execute Register 'r'.
;
;	'+' required to enable auto-execution.
;	"0", no "+", or non alpha-numeric 'r' disables auto-execution.
;	Kludge until doco changes (RA always set RETVAL):  '-' forces RETVAL.
;
;
	IF	VPLUS, [
	BPROC	RACMD
RACMD:	CPIB	RMINUS,'-'	;Want RETVAL set?
	JRNZ	..1		;No, branch
	TST	EXCBUF		;Auto-execution?
	CNZ	RTOASC		;Yes, convert binary to 'r'
	MOV	L,A
	MVI	H,0
	SHLD	RETVAL
	JMPR	..2		;Continue, "-" forces as well as "+"
;
..1:	CPI	'+'		;Really want auto-execution?
	JRNZ	..3		;No, branch to disable

..2:	TST	REGNUM		;Valid register to auto-execute?
	JRZ	..3		;No, '0' disables auto-execution
	CPI	USRGMX
	JRC	..4		;Yes, branch
..3:	XRA	A		;Disable auto-execution
..4:	STA	EXCBUF		;Set auto-execution register
	RET
	EPROC	RACMD
	]
;
;	RC # - Copy text into text register.
;
	BPROC	RCCMD
RCCMD:	CALL	CHKEXE		;BREAK out if executing

	IF	VPLUS, [
	CALL	SKPLIN		;HL-> one text block end
	] [
	CALL	FNDLIN		;HL-> one text block end
	]

	LDED	EDTPTR		;+DE-> other text block end
	JMP	BRKCPY		;Make text copy in register, BREAK if no room
	EPROC	RCCMD
;
;	RG # - Get text from the text register.
;
	BPROC	RGCMD
RGCMD:	LHLD	EDTPTR		;HL-> where to insert copy
	CALL	GETTXT		;Copy text from text register
	JC	BREAK		;Give error if no space
	TST$	PTAFSW		;Move EDTPTR past insert?
	RZ			;No, return with old EDTPTR
	XCHG			;Yes, HL-> past the insert
	JMP	SETEPT		;Set new edit pointer
	EPROC	RGCMD
;
;	RI # - Insert following string into text register
;
	BPROC	RICMD
RICMD:	CALL	CHKEXE		;Check if being executed
				;Doesn't return if executing
;
	IFNOT	VPLUS, [
	CPI%M	CMDGET,ESC	;Is next char ESC?
	RZ			;Yes, nothing to do
	]
;
	CALL	SETTRM		;No, set terminator
..1:	CALL	INSLEN		;BC = insert length
	PUSHF			;'Z' if TERMCH, 'NZ' if LF reached
	MVIB$	RIFLG,1		;Flag - performing RI command
	LES	H,CMDGET	;HL = begin PTR
;
	IF	VPLUS, [
	STES	RIPTR,H		;For use by MAKCPY
	PUSH	H		;Save begin PTR
	DAD	B		;HL -> string terminator
	POP	D		;DE -> first char in the string
	INX$	H		;HL-> past terminator
	SHLD	CMDGET		;Update GET PTR now for next iteration
	DCX$	H		;HL -> terminator again
	CALL	BRKCPY		;Copy into register.  BREAK if no room
	]
;
	IFNOT	VPLUS, [
	MOV	D,H
	MOV	E,L		;DE-> begin of text
	DAD	B		;HL-> past string
	PUSH	H		;Save ->
	CALL	BRKCPY		;Make text copy in register
				;*BREAK* if insufficient memory
	POP	H		;HL-> past string
	INX$	H		;HL-> past terminator
	SHLD	CMDGET		;Set new GET PTR
	]
;
	POPF			;Was TERMCH reached above?
	JZ	ENDTRM		;Yes, CLR SRCHFL
;
	CALL	NEWCMD		;No, get next command line
	MVIB$	REGAPP,'+'	;Set for appending
	JMPR	..1		;Go for more
	EPROC	RICMD
;
; RJ # Jump to Register Macro.
;
	IF	VPLUS, [
RJCMD:	CALL	JMCMD
	JMP	MCMD1
	]
;
; RL# FILENAME - Load text register from file.
;
				;{RXCMD,YLCMD}
	BPROC	RLCMD
RLCMD:	CALL	CHKEXE		;BREAK out if being executed
	MVIB$	PARMFL,0FFH	;Inform NXCMCH() a string parameter is expected
	CALL	SETAUX		;Get file name and setup FCB
;
	IF	VPLUS, [
	MOVB	REGNUM,AUXREG	;Reset register # (SETAUX(|Rr) may have changed it)
;
;	Open the file.
;
	CPIB$	RPLUS,'+'	;Extended directory search?
	LXI	H,OPNSP1	;Presume yes
	BEQ	..1		;Yes, branch
	LXI	H,OPNAUX	;No, just check specified directoy
..1:	CALL	CALLHL		;Try to open the file
	JC	FNFBRK		;BREAK with error message if unable
;
;	Load the file.
;
				;{RLCMD^, LODEXE}
RLCMD0:	CALL	RAMDWN		;Open up REGNUM as wide as possible
;
;	Read next sector.
;
..2:	CALL	AXREAD		;Read next sector into DEFDMA
				;'Z' if NULL record read
				;DE-> begin of sector, HL-> past end
	JRZ	..DONE		;Exit when NULL record read
	CALL	SBHLDE		;BC = # bytes to be appended
;
;	Append minimum of BC and available bytes to edit buffer.
;
	LHLD	RAMTOP		;HL-> UL
	LDED	RAMBOT		;DE-> next free byte
	DSUB	D		;HL = # free bytes available
	PUSH	D		;
	MOV	D,B
	MOV	E,C		;DE = # bytes to be appended
	CALL	MNHLDE		;HL = minimum of HL, DE.  'C' => no change
	POP	B		;BC -> next free byte
	PUSHF			;Save carry flag
	LXI	D,DEFDMA	;DE -> start of text just read in
	DAD	D		;HL -> past last text byte to be moved
	CALL	MVUPBF		;Append the text, HL -> last appended byte
	INX$	H		;HL -> past last appended byte
	SHLD	RAMBOT		;
	POPF			;Retrieve carry flag
	JRNC	..2		;Read next sector if all bytes appended
	JMP	BREAK		;Out of memory.  Pointers OK.  BREAK out
;
;	Finishing up: close up buffers, adjust pointers, (close file).
;
..DONE:	CALL	RAMUP		;Close memory back up
	JMP	CLSAUX		;Close file for CP/M-86
	]			;<IF VPLUS>
;
;
	IFNOT	VPLUS, [
	CALL	OPNAUX		;Open file from FCB
	JC	MSGBRK		;No file, give error and abort
;
;	Load the file.  If REGAPP set, file will be appended
;
				;{AUTEXE}
RLCMD0:	CALL	AXREAD		;Read next sector into DEFDMA
				;DE-> begin of sector, HL-> past end
	PUSHF			;Save EOF flag
;
	CALL	BRKCPY		;Append to text register (could be NULL)
				;*BREAK* if insufficent memory
	MVIB$	REGAPP,'+'	;Append additional sectors to register
;
	POPF			;'C' if EOF reached
	JRNC	RLCMD0		;Continue until EOF
	JMP	CLSAUX
	]			;<IFNOT VPLUS>
	EPROC	RLCMD
;
;	RMt - Compare text register t against edit buffer e,
;		handling upper/lower case.
;
;		Return: EDTPTR set -> past last char matched in e.
;			TRGPTR set -> past last char matched in t.
;			QN = # characters matched.
;			QV = 0 if t is wholly contained in e,
;			     1 if t is < e,
;			     2 if t is > e.
;
	IF	VPLUS, [
	BPROC	RMCMD
RMCMD:	MOVW	SAVEPT,EDTPTR	;So QN can be properly set when done
	CALL	PNTREG		;Does register exist? BDH defined if so
	MVI	C,EOF		;C = end-of-string marker = EOF
	JRNZ	..1		;Skip if length(t) != 0
;
;	Empty text register, quit right now.
;
	LDED	EDTPTR		;+DE -> start point in e
	LDAX	D		;A comes from edit buffer e
	CMP	C		;At end of e also?
	MVI	B,0		;Zero => strings are equal (both null)
	JZ	..9		;Yes, set values and return
	INR	B		;No, t<e, set B = 1
	CALL	SETERR		;Set ERRFLG error flag
	JMP	..9		;Go set values and return
;
..1:	PUSHF			;Save PNTREG() result
	LDA	REGNUM		;For STUPF1()
	CC	STUPF1		;Ensure 2d window gets redrawn for edit buf
	POPF			;Retrieve PNTREG() result
	CNC	RASSOC		;Setup buffer associated ptrs for text reg
	CALL	SETRGT		;Set EOF to mark register ceiling
	LHLD	TRGPTR		;HL -> start point in t
	LDED	EDTPTR		;+DE -> start point in e
;
	TST$	SRCNSW		;Ignore upper/lower case distinction?
	BNE	..4		;Yes, branch
;
;	Compare buffers, characters must be identical.
;
..3:	LDAX	D		;A = char from edit-buffer
	CMP	C		;At end of buffer?
	BEQ	..5		;Yes, terminate comparison
	CMP%ES	M		;No.   Is char(t) = char(e)?
	BNE	..6		;No, terminate comparison
	INX$	H		;Yes, bump pointers to next chars
	INX$	D
	JMPR	..3		;And continue comparison
;
;	Compare buffers, ignoring upper/lower case distinction.
;
..4:	LDAX	D		;A = char from edit-buffer
	CMP	C		;End of edit-buffer?
	BEQ	..5		;Yes, terminate comparison
	MOV%ES	A,M		;A = text-register char, char(t)
	CALL	CONVUC		;Convert to upper case
	MOV	B,A		;B = Char(t)
	LDAX	D		;A = char(e)
	CALL	CONVUC		;A = Char(e)
	CMP	B		;Char(t) = Char(e)?
	BNE	..6		;No, terminate comparison
	INX$	H		;Yes, bump pointers to next chars
	INX$	D
	JMPR	..4		;And continue comparison
;
;	End of edit-buffer e.
;
..5:	SHLD	TRGPTR		;Restore t's edit pointer
	SDED$	EDTPTR		;Restore e's edit pointer
	CALL	SAVERG		;Put t's pointers back into buffer header
	CALL	RESRGT		;Restore ceiling char
	CALL	CHKGLB		;More text for e ?
	JZ	RMCMD		;Yes, more text, continue with match
;
;	End of register t ?
;
	MVI	B,0		;Zero => strings are equivalent
	LDED$	TRGCEL		;DE -> end of t's text window
	LHLD	TRGPTR		;HL -> where we stopped searching
	CALL	CMHLDE		;Same?
	LDED	EDTPTR		;+Restore -> past e's last examined char
	BEQ	..8		;Yes, terminate processing
	MVI	B,2		;No, t>e
	JMPR	..8		;Terminate processing
;
;	Strings not equivalent, text still in edit-buffer, check for EOTr.
;
..6:	PUSHF			;Save flags
	MOV%ES	A,M		;A = unmatched char from t
	CMP	C		;At end of text register?
	BEQ	..7		;Yes, t is contained within e
	POPF			;No, in middle of both t & e, is t < e ?
	MVI	B,1		;B = 1 => yes
	JP	..8		;Yes, terminate processing
	INR	B		;No, B = 2 => t > e
	JMPR	..8		;Terminate processing
..7:	POPF			;Adjust stack
	MVI	B,0		;B = 0 => t in e
;
;	Set values and return.
;
..8:	SHLD	TRGPTR		;Set t's edit pointer
	CALL	SAVERG		;Put back into buffer header
	CALL	RESRGT		;Restore ceiling char in register
;
..9:	MOV	L,B		;L = return code
	MVI	H,0		;HL = return code
	SHLD	RETVAL		;Set QV for user access
	LHLD	SAVEPT		;HL -> edit-buffer starting point
	XCHG
	SHLD	EDTPTR		;Set terminating edit-pointer
	JMP	SEADN2		;Set QN = # bytes matched & return
	EPROC	RMCMD
;
; SETRGT - Save char at register ceiling in TRGCHR, then mark ceiling w/EOF.
;
SETRGT:	LHLD	TRGCEL
;6	MOV	ES,TRGSEG
	MOV%ES	A,M
	STA	TRGCHR
	MVI%ES	M,EOF
	RET
;
; RESRGT - Restore TRGCHR to register ceiling.
;
RESRGT:	PUSH	H
	LHLD	TRGCEL
;6	MOV	ES,TRGSEG
	LDA	TRGCHR
	MOV%ES	M,A
	POP	H
	RET
	]			;<IF VPLUS>
;
; RQCMD - Display string on console & get console input into register.
;
	IF	VPLUS, [
	BPROC	RQCMD
RQCMD:	CALL	OUTPMT		;Display prompt string
	CALL	CHKEXE		;Break out if register being executed
	CALL	RAMDWN		;Open/create register as wide as possible

..1:	LHLD	RAMTOP		;HL-> UL
	LDED	RAMBOT		;DE-> 1st free byte
	CALL	GETSTR		;Input string from console, add CR-LF
	JC	BREAK		;Recover from <ctrl-c>
	JRZ	..1		;Redo for <ctrl-x> & <ctrl-u>

	LDED	STRPUT		;+Valid string, DE -> past last byte input
	CPIB$	COLFLG,':'	;Strip terminator bytes?
	BNE	..2		;No, skip
	DCX$	D		;Yes, back up past CR,LF
	DCX$	D
..2:	CALL	RAMUP0		;Close extra space in T-Reg and return
	JMP	STADON		;Close status line in case of +RQ
	EPROC	RQCMD
	]			;<IF VPLUS>
;
; RS# FILENAME - Save text register in file.
;
				;{YKCMD}
	BPROC	RSCMD
RSCMD:	MVIB$	PARMFL,0FFH	;Inform NXCMCH() a string parameter is expected
	CALL	SETAUX		;Get file name and setup FCB
;
	IF	DEMO, [
	LXI	H,DEMMSG	;HL-> DEMO message
	JMP	PRTMSG
	]
;
;	Determine whether previous version exists.
;
	CALL	OPNAUX		;Try to open the file
	PUSHF			;Save result
	CALL	CLSAUX		;Close it again
	POPF			;Did file exist?
	JRC	..1		;No, create it
;
;	Determine whether user is to be queried.
;
	CPIB$	RMINUS,'-'	;Skip query?
	BEQ	..1		;Yes, branch
;
;	Query user whether previous version to be overwritten.
;
	LXI	H,RSMSG		;HL-> message
	CALL	PREPLY		;Give prompt, get reply
	RNZ			;Return if not 'Y'
;
..1:	CALL	CREAUX		;Create output file
				;*BREAK* if file not created OK
;
	IF	VPLUS, [
	LDA	AUXREG		;Restore register number in case destroyed
	STA	REGNUM		;by SETAUX because of "|Rr"
	CALL	PNTREG		;Does register exist? BDH defined if so
	JZ	CLSAUW		;No, close the file
	XCHG			;HL -> start of text
	]			;<IF VPLUS>

	IFNOT	VPLUS, [
	CALL	PNTREG		;DE-> Text register, BC = Length
	XCHG			;HL-> low address of text register
	]			;<IFNOT VPLUS>
;
;6	push	bx
;6	push	cx
;6	call	swptsg
;6	pop	cx
;6	pop	bx
;
	LXI	D,AUXFCB	;DE-> FCB
	CALL	WRITE		;Write the entire text register out
				;;WTERFL set if write error
	LXI	D,AUXFCB	;DE-> FCB
	JMP	CLOSE		;Flush remnant & close the file
				;;BREAK out if write/close error
	EPROC	RSCMD
;
;	RD # - Dump text register.
;	RP # - Print text register.
;	RT # - Type text register.
;
RDCMD:	CPIB$	RPLUS,'+'
	LDA	LVLRD		;Don't expand anything
	BNE	RTCMD0
	ORI	80H		;Don't wrap screen when window overflow
	JMPR	RTCMD0		;Merge below

RPCMD:	CALL	LSTSET		;Setup to print, expand only tabs
	CALL	RTCMD2		;;Print the text
	JMP	ENDLST		;;Detach printer (unless YP pending)

RTCMD:	LDA	LVLRT		;Stop on CTRL-S
RTCMD0:	CALL	SETLST		;Set new LSTFLG
	CALL	RTCMD2
	JMP	RSTOUT		;Pop OUTPUT stack
;
				;{RTCMD}
RTCMD2:	CALL	PNTREG		;DE -> text register, BC = length
				;{CHKEKO+,TRACE+}
RTCMD3:	XCHG			;HL-> begin of text reg
	JMP	PRTLEN		;Send to output device
;
;	RU - Display memory usage for existing text registers
;
	IF	VPLUS, [
	BPROC	RUCMD
RUCMD:	CALL	UCMD		;Display general buffer usage
;
;	Now print user text registers.
;
..1:	MVI	D,10		;D = # rows/column
	MVI	E,-1		;E = row number = REGNUM for col #1
;
;	Start new row of 4 text registers.
;
..2:	CALL	CRLF		;Send <cr><lf>
	INR	E		;E = row number = REGNUM for col #1
	MOV	A,E
	CMP	D		;Maximum row # ?
	JZ	CRLF		;Yes, send <cr><lf> & quit
	MVI	B,4		;No, set B = # columns (downcounter)
;
;	Print next register in row.
;
..3:	PUSH	B
	PUSH	D
	CALL	..4		;Print "length: name"
	POP	D
	POP	B
	DCR	B		;Decrement column downcounter.  Done?
	BEQ	..2		;Yes, start new row
	CALL	PSPACE		;Separate by sending 2 spaces
	CALL	PSPACE		;
	LDA	REGNUM		;Update register number for next column
	ADD	D		;A = current register # + column length
	CPI	USRGMX		;Normal register # (0-9, A-Z) ?
	JRC	..3		;Yes, print it
	BNE	..2		;No, and more than 1 greater
	MVI	A,MAINRG	;Use 1st available slot for main edit buffer
	JMPR	..3		;Display size of main edit buffer
;
;	Print 'length: r' for current REGNUM.
;
..4:	CALL	..5		;Output "length: "
	LDA	REGNUM		;A = register number
	JMP	PRTBN0		;Convert to ASCII & output to console
;
;	Print "length: " on console.
;
..5:	CALL	GETRLN		;Set REGNUM, return BC = register length
	PUSHF			;Save buffer/register flag
	CALL	PRTDEC		;Output length
	MVI	A,':'		;Output a colon
	CALL	PCHARA		;
	POPF			;Edit buffer ('C') ?
	JNC	PSPACE		;No, output a space
	MVI	A,'*'		;Yes, output an asterisk
	JMP	PCHARA
	EPROC	RUCMD
	]			;<IF VPLUS>
;
;	RU - Display memory usage for all text registers
;
	IFNOT	VPLUS, [
	BPROC	RUCMD
RUCMD:	MVI	B,REGMAX	;Number of registers
	LXI	H,REGNUM	;HL-> register number
	MVI	M,00		;Start at 0
..1:	PUSH	B		;Save counter
	PUSH	H		;Save ->
	MOV	A,M		;Output register number
	ADI	'0'
	CALL	PCHARA
	MVI	A,':'
	CALL	PCHARA
	CALL	PSPACE		;Output a space
	CALL	PNTREG		;BC = register length
	CALL	UCMD2		;Output length and CRLF
	POP	H		;HL-> REGNUM
	INR	M		;Increment register number
	POP	B		;Restore counter
	DJNZ	..1		;Loop until all register lengths output
	RET
	EPROC	RUCMD
	]			;<IFNOT VPLUS>
;
;	RX # filespec - Load filespec into register # and execute.
;	Will look for filespec on [A0] if so selected.
;
	IF	VPLUS, [
RXCMD:	MVIB$	RPLUS,'+'	;Specify that special drive is to be checked
	CALL	RLCMD		;Load file into register
	JMP	MCMD1		;Setup command pointers to execute it
	]			;IF VPLUS
;
;	RZ # - Zero out/zap register #.
;
	BPROC	RZCMD
RECMD:
RZCMD:
	IF	VPLUS, [
	CPIB$	RPLUS,'+'	;Clear out currently executing register?
	BNE	..1		;No, skip
	CMPMB	REGEXN,REGNUM	;Yes, is the it the current command buffer?
	CZ	JMCMD		;Yes, first exit this buffer
..1:
	]			;<IF VPLUS>

	CALL	CHKEXE		;BREAK out if executing
	JMP	CLRREG		;Clear it out
	EPROC	RZCMD
	.PAGE
;
;	S - SUBSTITUTE  (modified 3/11/84)
;
	BPROC	SCMD
SCMD:	CALL	STSEAR		;Setup for the search
;
	IFNOT	VPLUS, [
	CLR$	MINUS		;Clear flag for CHKGLB()
	]
;
..1:	CALL	SEARTX		;Search for the string in the text buffer
	JRC	..2		;Branch if not found
	SHLD	DELPTR		;Save pointer past string end as DELPTR
	SDED	EDTPTR		;+Save pointer to string begin
	CALL	SBHLDE		;BC = size of delete string
	XCHG			;HL-> begin of delete string
	MVI	A,0FFH		;Get delete char
	STA	SRCHFL		;Set SRCHFL to indicate "S" command
				;Prevents command line compression in NEWCMD
	STA	COMPFL		;Set flag for later 0FFH compression
	CALL	FILL		;Fill deleted region with delete char
	LHLD	CMDGET		;HL-> begin of replace string
	PUSH	H		;Save it
	CALL	ICMD1		;Now insert replace string
	CALL	DECITR		;Decrement remaining iteration
	POP	H		;Restore command PTR
	JZ	SCCMPS		;Compress out 0FFH chars if count done
				;RET made from here when no error
	SHLD	CMDGET		;Restore to enable reuse of replace string
	JMPR	..1		;Repeat substitution
;
;	When string not found, check for global search - perform disk I/O
;
..2:	CALL	SCCMPS		;Compress any 0FFH chars out
	CALL	CHKGLB		;More text on disk and global search?
	JRZ	..1		;Yes, continue with search
;
;	Set error, set CMDGET -> end of replace string
;
	LDA	TERMCH		;Get terminator
	MOV	B,A
	CALL	SCNGET		;Move CMDGET until terminator found
	JMP	NOTSUC		;Report error if necessary
	EPROC	SCMD
;
;	T - TYPE Text Lines.
;
	IF	VPLUS, [
TCMD:	LDA	LVLTYP		;Get value to expand BS and ESC
	CALL	SETLST		;Set new value, save old ones
	LXI	H,TYPLST	;Routine to display the block
	CALL	GLOBX		;Do it, globally if requested
	JMP	RSTOUT		;Restore previous LSTFLG & LSTOUT
	.PAGE
;
; GLOBHL - Invoke HL's routine, handling GLBFLG.  EDTPTR restored after
;	   global operation if Register A is nonzero.
;

CSAVE:	DSW	1
NSAVE:	DSW	1
				;{KCMD}
GLOB0:	XRA	A		;Do not attempt to restore edit pointer
	JMPR	GLOBHL
				;{TCMD,EOCMD}
GLOBX:	MVI	A,0FFH		;Ensure edit pointer restored

	BPROC	GLOBHL
GLOBHL:	STA	TEMPFL		;Save EDTPTR-restoration flag
	ST16%CS	..func,H	;Store address of processing routine
	CPIB$	NM2FLG,COMMA	;2 parameters?
	JRZ	..LOC		;Yes, process locally
	CPIB$	GLBFLG,'_'	;Global command?
	BNE	..LOC		;No, branch & process locally
	TST$	TEMPFL		;Need to restore edit pointer?
	JRZ	..GEXC		;No, go do it
;
;	Process globally.
;
..GLOB:	LHLD	EDTPTR		;.p
	PUSH	H		;XSp		Get current edit pos
	CALL	PREVLF
	INX$	H		;0L		(without changing pos)
	POP	D		;(Qp)
	XCHG
	DSUB	D		;Qp-.p
	SHLD	CSAVE		;XSc		Save offset from line begin
	CALL	CNTFL0		;.y
	SHLD	NSAVE		;XSn		Save current line number

	TST$	MINUS		;Is move negative?
	JRNZ	..REV		;Yes, branch
;
;	Global forward (n_T, e.g.)
;
..FORW:	CALL	..GEXC		;n_T (e.g.)	Do it n times if poss
	CALL	CNTFL0		;.y
	LDED	NSAVE		;Qn
	DSUB	D
	SHLD	ITRCNT		;n <- (.y-Qn)	Recompute 'n'
	CALL	BAKDIR		;		Set minus direction
	CALL	LCMD		;-(.y-Qn)_:L	Backup 'n' lines
;
;	Restore original edit position within line.
;
				;{..FORW^,..REV}
..QCC:	CALL	FORDIR		;Ensure forwards direction
	MOVW	ITRCNT,CSAVE
	JMP	CCMD		;QcC
;
;	Global backward (-n_T, e.g.)
;
..REV:	MVIB$	COLFLG,':'
	CALL	LCMD		;-n_:L		Backup n lines if poss
	CALL	CNTFL0		;.y
	XCHG
	LHLD	NSAVE		;Qn
	DSUB	D
	SHLD	ITRCNT		;n <- (Qn-.y)	Recompute 'n'
	CALL	FORDIR		;		Set positive direction
	CALL	..GEXC		;n_T (e.g.)	Do it forwards
	CALL	..QCC		;QcC		Restore edit pointer
	MVIW$	ITRCNT,0	;0T  (e.g.)	Process remnant, if any
;
;	Process locally.
;
..LOC:	CALL	SKPLIN		;HL-> one end
	LDED	EDTPTR		;+DE-> other end
	IFNOT	P8086, [
	DB	0C3H		;JMP
	]
;6	JMP	CWP #FUNC
..FUNC:	DSW	1

;
;	Global executive routine.
;
..GEXC:	CALL	FNDLIN		;HL-> one end
	PUSHF			;Save FNDLIN ret cod
	LDED	EDTPTR		;DE-> other end
	SHLD	EDTPTR		;For next time through (T & EO. PKTX00 handles Kcmd)
	IFNOT	P8086, [
	CALL	..FUNC-1	;Do it
	]
;6	CALL	CWP #FUNC
	POPF			;All lines accounted for?
	RNC			;Yes, quit
	CALL	CHKGLB		;No, any more text?
	BEQ	..GEXC		;Yes, continue
;
FORDIR:	CLR	MINUS
	STA	RMINUS
	RET
BAKDIR:	MVIB	RMINUS,'-'
	STA	MINUS
	RET
	EPROC	GLOBHL
	]			;<IF VPLUS>
	.PAGE
;
;	T - TYPE Text lines.
;
	IFNOT	VPLUS, [
	BPROC	TCMD
TCMD:	LDA	LVLTYP		;Get value to expand BS and ESC
	CALL	SETLST		;Set new value, save old ones

	MOVW	SRFAIL,EDTPTR	;Save edit pointer in variable which
				;adjusts automatically with disk buffering
;
..1:	CALL	FNDLIN		;HL-> where to type to
	LDED	EDTPTR		;+DE-> where to type from
	SHLD	EDTPTR		;Save -> for type after disk buffering
	PUSHF			;Save flag from FNDLIN
	CALL	TYPLST		;Type text
	POPF
	JRNC	..2		;Branch if end of requested TYPE reached
	CALL	CHKGLB		;Try to read more of file in
	JRZ	..1		;Branch if disk buffering occurred
;
..2:	MOVW	EDTPTR,SRFAIL	;Restore edit pointer as best as possible
	JMP	RSTOUT		;Restore previous LSTFLG and LSTOUT
	EPROC	TCMD
	]			;<IFNOT VPLUS>
;
;	U - USED Memory values.
;
	BPROC	UCMD
UCMD:
	IF	VPLUS, [
	CPIB$	EDTNUM,MAINRG	;Main edit buffer?
	BEQ	..1		;Yes, don't display buffer name
	CALL	PRTBNM		;Display [buffer name] for all others
	LXI	H,SPSMSG	;HL-> ':  '
	CALL	PRTSTR
..1:
	]

	CALL	FREESP		;BC = # free bytes
	CALL	PRTDEC		;Print the number
	MVI	A,'/'		;Get character
	CALL	PCHARA		;Print it
	SUB%BC	TXTCEL,TXTFLR	;BC = edit buffer length
	CALL	PRTDEC		;Print the number
	MVI	A,'/'		;Get character
	CALL	PCHARA		;Print it
	CALL	REGLEN		;BC = text register size

				;{RUCMD}
UCMD2:	CALL	PRTDEC		;Print the number
	JMP	CRLF		;Give last CRLF
	EPROC	UCMD

;
;	V - Visual Mode.
;
VCMD:	CPIB$	RMINUS,'-'	;Just update current window?
	JNZ	VEDCMD		;No, go enter visual mode
	CALL	STNSNL		;Flag to rewrite window
	JMP	UPVISW		;Update current window
;
;	W - WRITE
;
	BPROC	WCMD
WCMD:	CALL	WTOPCH		;Error if output file not open
	CALL	GETITR		;Is the command "0W"?
	LHLD	EDTPTR		;Assume yes, get current line
	JRZ	..1		;Yes, "0" puts us at begin of current line
;
	CPIB$	RMINUS,'-'	;Backwards write?
	LHLD	TXTCEL		;Assume yes, write from top of memory
	JRZ	..1		;Yes write out back end of text
	LHLD	TXTFLR		;No write out front end of text
..1:	CALL	FNDLN2		;HL-> text to write out
	CPIB$	RMINUS,'-'	;Backwards write?
	JZ	WRTBND		;Yes write out back end of text
	JMP	WRTTXT		;No, write the text out
	EPROC	WCMD
;
;	X - Value register commands
;
	IF	FULL, [
XCMD:	CALL	NXCMUC		;;Get second command char; convert to UC
	PUSHA			;Save

	IF	VPLUS, [
	CALL	GETQRN		;Get text register number
	]
	IFNOT	VPLUS, [
	CALL	GETCRN		;Get text register number
	]

	POPA			;Restore second char
;
	CPI	'A'		;Add to value?
	JRZ	XACMD

	IFNOT	VPLUS, [
	CPI	'I'		;Insert numeric value?
	JRZ	XICMD
	]

	IF	VPLUS, [
	CPI	'K'		;Get prompted keystroke?
	JRZ	XKCMD
	CPI	'Q'		;Get prompted numeric expression?
	JRZ	XQCMD
	]

	CPI	'S'		;Set value?
	JRZ	XSCMD
	CPI	'T'		;Display value?
	JRZ	XTCMD
	JMP	C2BR8K		;No, give error and BR8K out

	IF	VPLUS, [
;
;	nXAr - Add 'n' to register 'r'
;
XACMD:	CALL	VALCHL		;CHL= signed register value v
	LXI	D,GITCHL	;DE -> routine to get signed (CHL) into CHL
	CALL	NXTBDE		;BDE = iteration count n, CHL = v
	CALL	ADD24		;CHL = result = v + n
	JMP	STQVAL		;Store result into the register and return
;
; XKr string - Get prompted keystroke into numeric register r.  [5/22/86]
;
	BPROC	XKCMD
XKCMD:	CPIB$	RPLUS,'+'	;Prompt on status line? (kludgy cursor fix)
	BEQ	..1		;Yes, branch
	CALL	YTCMD		;No, keep proper LOGPOS
	JMPR	..2
..1:	CALL	OUTPMT		;Issue prompt string, (cursor fix)
..2:	CALL	GETKEY		;A & C = char entered at keyboard
	CPI	CTRLC		;Break out?
	BNE	..OK		;No, branch
	TST$	COLFLG		;Allow <ctrl-c>?
	JZ	BREAK		;No, BREAK out
..OK:	MOV	L,C
	MVI	H,0
	MOV	C,H		;CHL = keyboard char in 24 bits
	JMPR	XQCMD9		;Merge below, store and close status line
	EPROC	XKCMD
;
; XQr string - Get prompted numeric expression from keyboard into register r.
;		NFOUND is set = # characters in the expression by EVLSTR.
;				[4/11/86]
;
	BPROC	XQCMD
XQCMD:	CALL	OUTPMT		;Issue prompt string
	LXI	D,DEFDMA	;Use default file buffer space for input
..1:	LXI	H,DEFDMA+128	;HL -> past upper limit
	PUSH	D		;Save -> start of string
	CALL	GETSTR		;Get keyboard input, add CR-LF
	POP	D		;DE -> start of expression
	JC	BREAK		;Allow user to break out with <ctrl-c>
	JRZ	..1		;Do over for <ctrl-x> & <ctrl-u>
	XCHG			;HL-> start of string
	CALL	EVLSTR		;CHL = value, NFOUND set
;
XQCMD9:	CALL	STQVAL		;Store CHL into numeric register
	JMP	STADON		;Close status line in case of +XQ
	EPROC	XQCMD
;
;	nXSr - Set value of register 'r' to 'n'
;
XSCMD:	CALL	GITCHL		;CHL = new value
	JMP	STQVAL		;Store new value into the register
;
;	XTr - Type value of register 'r'
;
XTCMD:	CALL	VALCHL		;CHL = register value
				;{CALC}
XTCMD0:	CALL	PRTCHL		;Type the value
	JMP	IFCRLF		;Send CRLF unless colon suppressed
	]			;<IF VPLUS>

	IFNOT	VPLUS, [
;
;	nXAr - Add 'n' to register 'r'
;
	BPROC	XACMD
XACMD:	CALL	VALIDX
	CALL	CHKITR		;BC = ITRCNT, 'Z' if RMINUS or BC == 0
				;Is this a subtract?
	JRZ	..1		;Yes, branch
	DAD	B		;HL = new value
	JMPR	..2
;
..1:	DSUB	B
..2:	XCHG			;HL-> reg, DE = new value
	JMP	STDEIN		;Store DE at (HL)
	EPROC	XACMD
;
XICMD:	CALL	VALIDX
	XCHG
	MOV	C,M
	JMP	EICMD2
;
;	nXSr - Set value of register 'r' to 'n'
;
XSCMD:	CALL	VALIDX		;DE-> register
	CALL	GETITR		;BC = new value
	XCHG			;HL-> register
	MOV	M,C
	INX$	H
	MOV	M,B
	RET
;
;	XTr - Type value of register 'r'
;
XTCMD:	CALL	VALIDX		;HL = value
	MOV	B,H
	MOV	C,L
	CALL	PRTDEC		;Type the value
	JMP	IFCRLF		;Issue CRLF unles ':' suppressed
;
;
; VALIDX - Return DE-> value register, HL = contents
;
VALIDX:	LXI	H,VALREG
	LDA	REGNUM
	CALL	ADAAHL		;HL-> register
	PUSH	H
	CALL	MVINHL		;HL = value
	POP	D		;DE-> register
	RET
	]			;<IFNOT VPLUS>
;
;	Y - MISC commands
;
YCMD:	CALL	NXCMUC		;;Get second char; convert to UC
	LXI	H,YDEC		;HL-> decode table
	CALL	CHOICE		;HL-> address if found, 'C' if not found
	JC	C2BR8K		;No, give error and BR8K out
	PCHL			;Jump to routine
;
;  YDEC
;
YDEC:
	IF	VPLUS, [
	DB	'D'		;Display as ASCII char?
	DW	YDCMD
	]

	DB	'E'		;CRT Emulation
	DW	YECMD		;In -V4
	DB	'F'		;Format paragraph
	DW	YFCMD
	DB	'I'		;Control Text insert
	DW	YICMD
;
	DB	'K'		;Save keystroke macros
	DW	YKCMD
	DB	'L'		;Load keystroke macros
	DW	YLCMD
;
	DB	'M'		;Find matching group delimiter
	DW	YMCMD
	DB	'P'		;Output to printer
	DW	YPCMD
	DB	'S'		;Strip top bit
	DW	YSCMD
	DB	'T'		;Type string
	DW	YTCMD
;
	IF	WINDOO, [
	DB	'W'		;New Window
	DW	YWCMD
	]
;
	DB	KFF		;End of table
;
;	nYD - Display n on terminal as ASCII character.
;
	IF	VPLUS, [
YDCMD:	LDA	LVLRD
	CALL	SETLST
	CALL	GETITR		;C = char
	CALL	PCHAR		;Output the char
	CALL	RSTOUT
	JMP	IFCRLF		;Output CR,LF unless colon-suppressed
	]
;
;	YE - CRT Emulation - Code is in -V4
;
;
;	nYF - Format paragraph, indenting 'n', wrapping at WRPCOL.
;	      (Default 'n' is current INDPOS)
;	      Set new edit point to start of next paragraph.
;
	BPROC	YFCMD
YFCMD:	LDA	INDPOS		;Save current indent pos
	PUSHA
	TST$	NUMFLG		;New indent pos?
	BEQ	..1		;No, skip
	CALL	GETITR		;C = new pos
	MOV	A,C
	DCR	A		;Allow user to specify "1" for 1st col
	STA	INDPOS
;
..1:	CMPMB	INDPOS,WRAPCL	;Is indent pos >= wrap col?
	JRNC	..9		;Yes, ignore command
	LHLD	EDTPTR		;BX-> current position
	CALL	FORMAT		;Format the paragraph, HL-> next para
	SHLD	EDTPTR		;Set new edit point
	CALL	STNSNL		;Make sure to rewrite visual screen
	MVIB$	CNTBFL,1	;Ensure screen table is re-established
;
..9:	POPA
	STA	INDPOS		;Restore original indent pos
	RET
	EPROC	YFCMD
;
;	YI - Change ROUTAD to insert text, -YI - change back to console.
;
YICMD:	CALL	CHKITR		;Is this 0YI or -YI?
	JZ	RSTOUT		;Yes, reset to console
	LXI	H,INSCHR	;Use insert character routine
	LDA	LSTFLG		;Use current LSTFLG
	JMP	SETOUT		;Set new ROUTAD
;
;	YK file - Save keyboard layout.
;
YKCMD:	LXI	H,RSCMD
YKCMD1:	MVI	A,REGKEY	;Special key-table register
	STA	REGNUM		;Save as register to use

	IF	VPLUS,[
	STA	AUXREG
	]

	CLR	REGAPP		;Make sure we don't append

	IFNOT	P8086, [
	PCHL			;Then use normal register load/save routines
	] [
;6	CALL	BX		;Then use normal register load/save routines
;6	JMP	RSTSEG		;Back to current segment
	]
;
;	YL file - Load new keyboard layout.
;
YLCMD:	LXI	H,RLCMD		;Use RL command
	CALL	YKCMD1
	CALL	MACRST		;;Reset keystroke macro pointer
	JMP	POLRST		;;Clear any KEYPTRs in type-ahead buffer
;
;	YM - Find matching group terminator.
;

	DSEG	$
GRP0PT:	DB	'|', 'G', EOF	;0-th level group delimiter pattern
GRPGPT:	DB	'|', '@', EOF	;General pattern for internal group delim searching
GRPPAT:	DS	3
LVLCNT:	DS	1
	CSEG	$

	BPROC	YMCMD
YMCMD:	LHLD	EDTPTR		;HL-> active char
	MOV	A,M		;Get it
	LXI	D,GRPTBL	;DE-> table of (redundant) group terminators
				;Each symbol followed by its companion
	CALL	LOOKCH		;Group terminator?
	JRZ	..1		;Yes, branch
;
;	Find first grouping delimiter.
;
	LXI	H,TARGST	;Setup target string
	LDA	WILDCH
	MOV	M,A
	INX$	H
	MVI	M,'G'		;|G is grouping delimiter set
	INX$	H
	MVI	M,EOF
	MVIW	SRCHCN,00
	SHLD	SRFAIL
	CALL	FCMD0		;Find first occurrence
	TST$	ERRFLG		;Successful?
	RNZ			;No, just return
	TST$	MINUS		;Forward search?
	RNZ			;No, return
	LHLD	EDTPTR		;Yes, backup EDTPTR
	DCX$	H
	SHLD	EDTPTR
	CLR$	FNDFLG		;;Flag that TARGST destroyed
	RET
;
;	Setup pattern ( []EOF, e.g. )
;
..1:	STA	GRPPAT		;Store open/close delimiter
	INX$	D		;DE-> companion delimiter
	XCHG			;DE-> active char
	CMP	M		;Closing ASCII delim always > opening delim
	MOV	A,M
	STA	GRPPAT+1
	MVIB$	GRPPAT+2,EOF	;This pattern accessed by MATCH(|@)
;
;	Setup direction for searching for companion delimiter.
;
	MVIB$	LVLCNT,1	;Initially at level 1
	JRNC	..OPEN		;Branch if searching for opening delim
..NEXT:	INX$	D		;Advance past delimiter
..CONT:	LXI	H,GRPGPT	;HL-> |@EOF
	CALL	SEARCH		;Find next group delimiter
	JRC	..2		;Quit if unsuccessful
	CALL	..LEVL
	JRNZ	..NEXT
	SDED	EDTPTR		;Save pointer to closing delimiter
	RET
;
..2:	CALL	CHKGLB		;Global search & more text from disk?
	LDED	SRFAIL		;Continue where search left off, if so
	JRZ	..CONT		;Yes, continue
	MVI	E,2		;Error code
	JMP	ERRHND		;Give error

..OPEN:	CALL	SETBOT		;Put fence EOF ahead of [TXTFLR]
..OP1:	LDED	EDTPTR		;DE-> current edit position
..OP2:	LHLD	TXTFLR		;HL-> start of visible text
	CALL	CMHLDE		;At begin of buffer?
	JRNC	..OP3		;Yes, skip
;
;	Attempt to match
;
	DCX$	D		;No, backup one char before starting search
	LXI	H,GRPGPT
	CALL	PMATCH		;Match current position?
	JRNZ	..OP2		;No, try again

	CALL	..LEVL
	JRNZ	..OP2
	SDED	EDTPTR		;Save pointer to closing delimiter
	CALL	CHKBOT
	RET
;
;	Failed in this buffer.  Check more?
;
..OP3:	SDED	SRFAIL		;Save current search position
	CALL	CHKGLB		;More text from disk & global search?
	LDED	SRFAIL		;Retrieve adjusted search position
	JRZ	..2		;Yes, continue searching
	CALL	CHKBOT		;No, restore text buffer
	MVI	E,2
	JMP	ERRHND		;Start of buffer/file reached, give error

..LEVL:	LDAX	D		;Get char just matched
	LXI	H,GRPPAT	;HL-> original delimiter
	CMP	M		;Same?
	LDA	LVLCNT		;Get current level
	JRNZ	..DWN		;No, branch
..UP:	INR	A		;Increment level count
	INR	A
..DWN:	DCR	A		;Decrement level count
	STA	LVLCNT
	RET
	EPROC	YMCMD
;
;	YP - Change LSTFLG to print, -YP - change back to console
;
YPCMD:	CALL	CHKITR		;Is this 0YP or -YP?
	JNZ	LSTSET		;Yes, set ROUTAD to printer
	JMP	ENDLST		;Pop OUTPUT stack and detach printer
;
;	YS - Strip out high bits to read WordStar files
;
	BPROC	YSCMD
YSCMD:	CALL	FNDLIN		;HL-> where to strip to
	LDED	EDTPTR		;+DE-> edit point
	CALL	POSDIF		;Set HL > DE, BC = HL-DE
	XCHG			;HL-> begin of convert
;
..1:	MOV	A,B		;Is count zero?
	ORA	C
	RZ			;Yes, all done
	MOV	A,M		;Get next char
	ANI	07FH		;Strip top bit
	MOV	M,A		;Save char
	INX$	H		;Bump PTR
	DCX$	B		;Count down
	JMPR	..1		;Continue
	EPROC	YSCMD
;
;	YT - Type following string on console
;
	PUBLIC	YTCMD,YTCMD0
	BPROC	YTCMD
YTCMD:	LXI	H,STADON	;Address of routine to close status line
	PUSH	H		;Execute STADON() on exit
;
				;{RQCMD,XKCMD,XQCMD via OUTPMT)
YTCMD0:	CPI%ES	CMDGET,ESC	;Null string?
	RZ			;Yes, return
;
;	+YT, +RQ, +XK, +XQ go to status line.
;
	CPIB$	RPLUS,'+'	;Status line option?
	BNE	..3		;No, branch
	XRA	A		;Yes, start at column 0 of status line
	CALL	STASET		;Setup status line for output
	CALL	..3		;Call rest of routine
	CALL	ATTBCK		;Allow user reply in normal video
	JMP	WINEOL		;EOL rest of status line
;
..3:	MVIB$	PARMFL,0FFH	;Flag parameter-expected for NXCMCH
	CALL	SETTRM		;No, set terminator
	MOV	B,A		;Save in B
..4:	CALL	NXCMCH		;Get next command char
	CMP	B		;Reached terminator?
	JZ	ENDTRM		;Yes, CLR SRCHFL
	CALL	PCHARA		;No, type on console
	JMPR	..4
	EPROC	YTCMD
;
;
;	Z - Set edit pointer to end of buffer/file.
;
ZCMD:	CALL	CHKGLB		;More text to read?
	JRZ	ZCMD		;Yes, go get it
	LHLD	TXTCEL		;HL-> end of buffer/file
	JMP	SETEPT		;Set new value for EDTPTR
	.PAGE
;********************************
;*				*
;*	Extended Commands.	*
;*				*
;********************************
;
;
;	EA - Restart Editor
;	EY - Write out and close file
;
EACMD	=	RESTRT
EYCMD	=	CLOSER
;
;
;	EB - Open input and output files.
;
;       (Display I/O names if no parameters)
;
EBCMD:	CALL	CKSPRM		;Any parameter strings?
	JRNZ	EBCMD0		;Yes, branch
	CALL	ERCMD1		;Display input filename
	JMP	EWCMD1		;Display output filename
;
				;{BEGIN}
EBCMD0:	CALL	WTCLCH		;Error if output file open
	CALL	SETIOX		;Setup INFCB & RENFCB
	JMP	OPNIOX		;Open the files, do auto read
;
;
; CKSPRM - Check for command parameter string.  'Z' if not.
;	   ('NO' if next non-white command char is CR or ESC)
;
				;{EBCMD,ERCMD,EWCMD}
CKSPRM:	MVIB$	PARMFL,0FFH	;Flag parameter-expected for NXCMCH
	CALL	FCBCHX		;Get next non-white command char.
	CALL	CMPCE		;CR or ESC?
	RZ			;Yes, return 'Z'
	CALL	BAKGET		;Put char back into command stream
	ORI	1		;'NZ'
	RET
;
;	EC - Change disks.
;
	BPROC	ECCMD
ECCMD:	CALL	WTCLCH		;Error if output file open
	CALL	RVOPCH		;Error if .REV file open
;
	IF	MPM, [
	CALL	MPMCHK		;Is this MP/M?
	JRZ	..1		;No, branch
;
;	For MP/M need to perform dummy RESET to see if disk in use by other process
;
	CALL	CLSAUX		;Make sure AUX file closed
	CALL	..2		;Perform dummy RESET
	ORA	A		;Is disk in use?
	JRZ	..1		;No, all OK
	LXI	H,MPMDMS	;HL-> MPM error message
	JMP	MSGBRK		;Give error
	]
;
..1:	LXI	H,CHDMSG	;HL-> Change disks message
	CALL	PREPLY		;Give message, wait for reply
;
..2:	CALL	RELSRC		;Close any input file, release buffer
	MVI	C,INTDSK
	CALL	BDOS		;Get logged in disk
	PUSHA			;Save on stack
	MVI	C,RESET		;Reset the disk system
	CALL	BDOS
	POPA			;Get logged in disk
	MOV	E,A		;Place desired disk # in E
	MVI	C,SETDSK
	JMP	BDOS		;Select originally logged in disk
	EPROC	ECCMD
;
;	mED filespec - Display directory on the console in 'm' columns.
;	       VPLUS:  Don't issue header when 'm' is negative.
;
	BPROC	EDCMD
EDCMD:	MVIB$	PARMFL,0FFH	;Inform NXCMCH() a string parameter is expected
	CALL	SETAUX		;Put the file name into AUXFCB
	LDAM	AUXFCB+1	;File name specified?
	CPI	' '
	BNE	..2		;Yes, display directory
	LXI	B,11		;Nope, set name to "????????.???"
	MVI	A,'?'
	CALL	FILL
..2:	JMP	DIR		;Display directory
	EPROC	EDCMD

	IF	VPLUS, [
;
;   nEEr - Edit buffer 'r', @=main text buffer.		[8/31/85]
;
;	Performs auto buffering as specified by 'n' unless '-' inhibited.
;
;	Displays 'EDITING BUFFER 'r' followed by full output filename.
;
;	Display is disabled by setting COLFLG=':' (set automatically by
;	EXA and EQA).  Also disabled when executing in a macro.
;
;
EECMD:	CALL	GETCRN		;Get buffer number
	IFNOT	P8086, [
	TST$	OUTFLG		;Output file open?
	BEQ	EECMD0		;No, can't auto-buffer
	CPIB$	RMINUS,'-'	;Need to make space?
	CNZ	ENCMD		;Yes, since user didn't say otherwise
	]
;
				;{EXTCHK}
EECMD0:	CALL	EECMD1		;Activate buffer 'r'
	IF	MMIBM, [
	CALL	HCRSOF		;Turn off hardware cursor
	]
;
;	Display buffer/filename?
;
	TST$	REGSTK		;Macro executing?
	RNZ			;Yes, return quietly
	CPIB$	COLFLG,':'	;No, explicitly told to be quiet (EXA,EQA)?
	RZ			;Yes, return
	TST$	OUTFLG		;Open output file?
	JRZ	EECM01		;No, just display buffer name
;
;	Display  buffer name and output filename preceded by blank line.
;
	CALL	CRLF		;Display <newline>
	CALL	EECM01		;Display buffer name<newline>
	CALL	PRTFNM		;Display output filename
	JMP	CRLF		;Display <newline>
;
;	Display 'EDITING BUFFER r'<newline>.
;
EECM01:	LXI	H,EDTMSG	;HL-> 'EDITING '
	CALL	PRTSTR		;
	LDA	REGNUM		;Get buffer number 'r'
	CALL	PRTBNM		;'BUFFER r'
	JMP	CRLF		;<newline>
;
;	Activate buffer 'r'.
;
EECMD1:	CALL	CHKEXE		;Break out if buffer is on command stack
	CALL	STBRFL		;Set flag to rewrite status line ("En" message)
	CALL	STUPFL		;Set WWNDUP to update vis-mode window later
;
				;{UPDVIS & WIBORD via UPEEB}
EECMD2:	CALL	STNSNL		;Set flag to rewrite Visual Mode
				;We don't have screen tables for each window!!
	CALL	PNTTRG		;Does the buffer exist?
	JZ	MAKEBF		;No, create it & establish it as edit buffer
	JC	OPENBF		;Yes, make it the current edit buffer
	JMP	CONVBF		;Convert text register into edit buffer
	]			;<IF VPLUS>
;
;	EF - Close output file.	[9/19/86]
;
	BPROC	EFCMD
EFCMD:	CALL	WTOPCH		;Error if file not open
	CALL	NXCMCH		;Get next char
	ANI	5FH		;Capitalize
	CPI	'Y'		;Override?
	BEQ	..1		;Yes, skip query
	CALL	BAKGET		;Restore look ahead char
	LXI	H,EFMSG		;Get warning query
	CALL	PREPLY		;Give message, get reply
	RNZ			;Return if not 'Y'
..1:	JMP	CLOSE2		;Continue as before
	EPROC	EFCMD
;
;
;	EL - Look at line range of another file.
;
	BPROC	ELCMD
ELCMD:	MVI	A,1
	STA	ELLNFL		;Flag to start with line #
;
;	EG - Insert line range of another file.
;
EGCMD:	STA	ELFLG		;0 if EG, 1 if EL
	MVIW	EGINFL,00	;Reset Insert Flag and EOF Flag to OFF
	INX$	H		;Get a one
	SHLD	EGLEND		;File read begins at line # one
	MVI	A,LF		;Get the LF char
	STA	DEFDMA + 080H	;Put LF at end of DEFDMA area
	MVIB$	PARMFL,0FFH	;Inform NXCMCH() a string parameter is expected
	CALL	SETAUX		;Get file name for FCB
	LXI	D,1		;Default begin line #
	LXI	H,-1		;Default ending line #
	BEQ	..1		;Branch if ESC or CR found
	CPI	'['		;Is line range spec?
	BEQ	..0		;Yes, branch
..ERR:	JMP	PRMBRK		;Else error
;
..0:	CALL	GETDEC		;Get the beginning line number
	JRC	..ERR		;Error if no number
	XCHG			;DE = begin line #
	CALL	GETNXP		;Get the ending line number
	JRNC	..1		;Branch if success
	LXI	H,-1		;Else default to end of file
;
..1:	CPIB$	CMDCHR,']'	;Line range given?
	CZ	NXCMCH		;Yes, get past terminator
				;PARMFL effect.  Also MSDOS-CP/M synch problem.
	CALL	CMHLDE		;Is END < BEGIN?
	JRC	..ERR		;Yes give error
;
	SDED	EGBGLN		;+Save begining line #
	INX$	H		;HL = line # to stop on
	SHLD	EGENLN		;Save ending line #
;
	CALL	OPNAUX		;Open file with auxiliary FCB

	IF	VPLUS, [
	JC	FNFBRK		;No file, give error and abort
	]

	IFNOT	VPLUS, [
	JC	MSGBRK		;No file, give error and abort
	]
;
;	Read in another sector.
;
..2:	TST$	EGEOFF		;Have we reached end of file read?
	JRNZ	..8		;Yes, all done
;
	CALL	AXREAD		;Read next sector into DEFDMA
				;'Z' if NULL record read
				;DE-> begin, HL-> past end = (LF or EOF)
	JRZ	..8		;Branch if null (EOF) record read
;
	XCHG			;BX-> begin of sector
	LBCD$	EGLEND		;BC = line # at end of last sector
	TST$	EGINFL		;Is begin of sector to be inserted?
	JRNZ	..4		;Yes, branch to set insert pointer
;
;	Check if begin of insert has been found.
;
..3:	PUSH	H		;Save -> sector
	LHLD	EGBGLN		;HL = first line # to insert
	DSUB	B		;Found begin of insert?
	POP	H		;Restore -> sector
	JRNZ	..5		;No, branch
	MVIB$	EGINFL,1	;Set flag that we are now inserting
..4:	SHLD	EGBGPT		;Save insertion pointer
;
;	Count over the LFs in sector, check for last line #.
;
..5:	CALL	NEXTLF		;HL-> LF in sector or delimiting LF
	JRC	..6		;Branch if EOF found
;
	CALL	CMHLDE		;Was end of sector reached?
	JRZ	..7		;Yes, check if block is to be inserted
	INX$	B		;No, increment the line # counter
	INX$	H		;HL-> char past LF found
	PUSH	H		;Save -> sector
	LHLD	EGENLN		;Get last line # to insert
	DSUB	B		;Have we reached end of insert?
	POP	H		;Restore -> sector
	JRNZ	..3		;No, keep looking
;
..6:	MVIB$	EGEOFF,1	;Set flag that end of insert reached
;
..7:				;HL-> past last char. to insert
	SBCD	EGLEND		;+Save line # for end of sector
	TST$	EGINFL		;Is there anything to insert?
	JRZ	..2		;No, read in another sector
	LDED	EGBGPT		;+DE-> begin char to insert
	CALL	SBHLDE		;BC = count of chars to insert
;
;	Check if EG or EL command
;
	TST$	ELFLG		;Is this EL command?
	JRNZ	..EL1		;Yes branch out
	XCHG			;HL-> chars to insert
	CALL	INSBLK		;No, insert the char block
				;Causes *BREAK* if no space
	JMPR	..2		;Continue reading in file

..8:	JMP	CLSAUX		;Close AUXFCB file
;
;	EL - Type the file block to the screen with line numbers
;
..EL1:	JCXZ	..EL3		;Branch if count is zero
;
	TSTM	ELLNFL		;Is line # to be displayed?
	JRZ	..EL2		;No, branch
	MVI	M,00		;Yes, clear flag
;
	PUSH	D		;Save PTR
	PUSH	B		;Save count
	LBCD$	EGBGLN		;Get beginning line #
	CALL	PRTDEC
	LXI	H,SPSMSG	;Type out a few spaces
	CALL	PRTSTR
	CALL	RSTPOS		;Reset LOGPOS = 1 for tabbing
	POP	B		;Restore char count
	POP	D		;Restore PTR
;
..EL2:	LDAX	D		;Get next char
	CALL	PCHARA		;Type it (all regs saved)
	DCX$	B
	INX$	D		;Bump PTR
	CPI	LF		;Was last char a LF?
	BNE	..EL1		;No, keep going
;
	INCW$	EGBGLN		;Yes, increment our line counter
	MVIB$	ELLNFL,1	;Set to type next line #
	JMPR	..EL1		;First check if count exhausted
;
..EL3:	JMP	..2
	EPROC	ELCMD

;
;	EH - HELP by topics.
;
EHCMD:	LXI	H,VEHELP	;HL-> "VEHELP.HLP" or "VPEHELP.HLP"
	JMP	HELPEX		;Go process it

;
;	EI - Insert character corresponding to decimal value.
;
EICMD:	CALL	GETITR		;Get the value to insert
				;{XICMD-}
EICMD2:	MOV	A,C		;Get the char to insert
	CPI	EOF		;Trying to insert EOF?
	JZ	PRMBRK		;Yes, give error
	TST$	MINUS		;Is this -EI to overwrite old char?
	JZ	INSCHR		;No, insert the char, BREAK out if no room
;
	LHLD	EDTPTR		;HL-> where to overstrike
	MOV	A,M		;Get old char?
	CPI	EOF		;Is it EOF?

	IF	VPLUS, [
	JZ	BREAK		;Treat same as -I failure
	]

	IFNOT	VPLUS, [
	JZ	ERRHND		;Yes, handle the error
	]

	MOV	M,C		;No, put in new char
	INX$	H		;Bump EDTPTR
	SHLD	EDTPTR		;Save new one
	JMP	STNSNL		;Make sure screen rewritten

	IF	VPLUS, [
;
;	EJ - Set edit pointer directly.
;
EJCMD:	LHLD	ITRCNT		;HL = edit pointer offset from TXTBEG
	CALL	MNEPTX		;HL = minimum of (new EDTPTR, TXTCEL)
	JMP	SETEPT		;Set new edit pointer
	]
;
;	EK - Delete a file on disk.
;
	BPROC	EKCMD
EKCMD:	MVIB$	PARMFL,0FFH	;Inform NXCMCH() a string parameter is expected
	CALL	SETAUX		;Get file name and setup FCB
	CLR$	COLFLG		;DIR(:) would set RETVAL only
;
;	Determine whether user is to be queried: -EK or EK ... Y ...
;
	CPIB$	RMINUS,'-'	;Skip query?
	BEQ	..2		;Yes, branch

	IF	VPLUS, [
	CALL	FCBCHX		;Get next nonblank upper case char
	CPI	'Y'		;Is it 'Y'?
	BNE	..1		;No, user is to be queried
	CALL	FCBCHR		;Yes, and is next char a separator?
	BEQ	..2		;Yes, just erase the file(s)
	CALL	BAKGET		;No, backup to the 'Y'
..1:	CALL	BAKGET		;Backup to filename terminator
	]			;<IF VPLUS>
;
;	Display matching filenames, query user for delete permission
;
	CALL	DIR		;Display all matching filenames
	TST$	TEMPFL		;FNF flag from EDCMD?
	RNZ			;Yes, just return
	LXI	H,EKMSG		;HL-> verify message
	CALL	PREPLY		;Give message, get reply
	RNZ			;Return unless 'Y'
;
..2:	CALL	DELAUX		;Delete the file(s)
	INR	A		;File(s) found?
	RNZ			;Yes, return
	LXI	H,FNFMSG	;HL -> FILE NOT FOUND
	JMP	PRTMSG		;Display message. No BREAK
	EPROC	EKCMD

	IFNOT	VPLUS, [
FNFBRK:	LXI	H,FNFMSG	;No, HL-> error
	JMP	MSGBRK		;Give error
	]

;
;	EL - List line range of specified file.  See EG.
;

	IF	VPLUS, [
;
;	EMstring - Match string against text at the edit pointer.
;		Return .n  = # chars matched if success.
;		Return .rv = {1,0,2,3} for pattern {<,=,>,code failure}.
;
	BPROC	EMCMD
EMCMD:	CALL	STSEAR		;Setup for match
..1:	CALL	..3		;Perform pattern match, get return values
	JRC	..2		;Branch if match failed
	CALL	DECITR		;Decrement remaining-iteration count
	JRNZ	..1
	RET
;
..2:	CZ	CHKGLB		;At EOB w/more text on disk & global flag set?
	BEQ	..1		;Yes, continue searching
	CALL	CKMANY		;No, '#' and any matches?
	JC	SETERR		;No, set error flag
	RET
;
;	Returns 'NC' for success.  'C' & 'Z' if matching when EOB reached.
;	Else 'C' & 'NZ' for outright failure.
;
..3:	CALL	MATCHT		;Does pattern match text at edit point?
	LXI	H,0
	JRNC	..4		;No, branch
	SHLD	RETVAL		;Yes, set QV for user access (zero => match)
	LHLD	EDTPTR		;Yes, set HL -> start of matched text
	XCHG			;HL -> past end of matched text
	SHLD	EDTPTR		;Update edit pointer
	JMP	SEADN2		;Set QN = # bytes matched, return 'NC'
;
..4:	MOV	L,A		;HL={1,2,3} => pattern {<,>,code @ failure}
	SHLD	RETVAL		;Save for user access
	JMP	SEAER1		;Set SRCERR, return 'C'
	EPROC	EMCMD
	]			;IF VPLUS

	IF	VPLUS, [
;
;	EN - Need memory space:  make it available.    [10/10/86]
;
	BPROC	ENCMD
ENCMD:	TST$	OUTFLG		;Output file open?
	RZ			;No, return
	CALL	GETITR		;Is the command "0EN"?
	LHLD	ISPARE		;Assume Yes, have ISPARE bytes free
	JRZ	..1		;Yes, branch
;
	DCX%BC			;Was command "1EN"?
	JRNZ	..0		;No, branch
;
;	Compute value to leave (MINSEC * 128) + 1024 in current buffer.
;
	SUB%BC	TXTRWF,TXTBAS	;BC = # bytes in current edit buffer
	PUSH	B
	CALL	FREESP		;BC = # bytes of free space
	POP	H
	DAD	B		;HL = total memory we have to work with
	PUSH	H
	LBCD$	MINSEC		;BC = # sectors for preferred I/O
	CALL	MUL128		;BC = # bytes
	LXI	H,1024		;Give us a 1K margin
	DAD	B		;Add MINSEC back
	XCHG			;DE = amount to reserve for edit buffer
	POP	H		;HL = total memory to work with
	DSUB	D		;Compute value for "nEN"
	JMPR	..1		;Merge below
				;In case of underflow - cut buffer to minimum
;
..0:	INX$	B
	MOV	H,B
	MOV	L,C
..1:	PUSH	H		;Save
	CALL	FREESP		;BC = # free bytes
	POP	H		;HL = # bytes to have free
	DSUB	B		;HL = # bytes to free up
	RC			;Return if # bytes is free
	MOV	B,H		;Save in BC
	MOV	C,L
;
;	First write out from begin of text buffer.
;
	LHLD	EDTPTR		;HL-> edit position
	LXI	D,-2048-127	;Leave 2K bytes of text alone
	DAD	D		;HL-> lower limit
	JRNC	..4		;Branch if underflow
	LDED	TXTBAS		;DE-> begin of text buffer
	CALL	CMHLDE		;Too close to begin of buffer?
	BLT	..4		;Yes, write from end of buffer
;
	XCHG			;DE-> lower limit, HL = TXTBAS
	DAD	B		;HL-> where to write out to
	JRC	..2		;Too much if overflow
	CALL	CMHLDE		;Too much?
	BLT	..3		;No, branch
..2:	XCHG			;Yes, HL = max pointer
;
;	Write out text from TXTBAS to HL.
;
..3:	PUSH	B		;Save # bytes to free up
	LXI	D,127		;Ensure enough gets written out even if
	DAD	D		;almost 1 sector left between BASe & CEiLing
	CALL	NEXTLF		;Prevent truncation
	INX$	H
	CALL	WRTTXT		;Write the text out
				;BC = # bytes that were written
	POP	H		;HL = # bytes to free up
	DSUB	B		;Subtract # bytes written
	RC			;Return if enough written
	MOV	B,H		;Save in BC
	MOV	C,L
;
;	Now write from end of buffer.
;
..4:	LHLD	EDTPTR		;HL-> edit position
	LXI	D,2048+127	;Leave 2K alone
	DAD	D		;HL-> upper limit
	RC			;Return if overflow
	LDED	TXTRWF		;+DE-> end of text
	CALL	CMHLDE		;Too close to end of buffer?
	RNC			;Yes, return, did our best
;
	XCHG			;DE-> upper limit, HL = TXTRWF
	DSUB	B		;HL-> where to write out to
	JRC	..5		;Too much if overflow
	CALL	CMHLDE		;Too much?
	BGE	..6		;No, branch
..5:	XCHG			;Yes, HL = max pointer
;
;	Write out text from HL to TXTRWF to .$R$ file.
;
..6:	LXI	D,-128		;Ensure enough written out even though
	DAD	D		;upto 1 sector "written" may still be in memory
	CALL	PREVLF		;Prevent line truncation
	INX$	H
	CALL	WRTBND		;Write the text out
	RET
	EPROC	ENCMD
	]			;IF VPLUS

	IFNOT	VPLUS, [
	IF	DEBUG, [
;
;	EN - Need Memory Space
;
	BPROC	ENCMD
ENCMD:	CALL	GETITR		;Is the command "0EN"?
	MOV	H,B
	MOV	L,C
	JRNZ	..1		;No, HL = # bytes to have free
	LHLD	ISPARE		;Yes, have ISPARE bytes free
..1:	LXI	D,128		;Sector size
	DAD	D		;Add one sector worth for worst case
	PUSH	H		;Save
	CALL	FREESP		;BC = # free bytes
	POP	H		;HL = # bytes to have free
	DSUB	B		;HL = # bytes to free up
	RC			;Return if # bytes is free
	MOV	B,H		;Save in BC
	MOV	C,L
;
;	First write out from begin of text buffer
;
	LHLD	EDTPTR		;HL-> edit position
	LXI	D,-2048		;Leave 2K bytes of text alone
	DAD	D		;HL-> lower limit
	LDED	TXTBAS		;DE-> begin of text buffer
	CALL	CMHLDE		;Too close to begin of buffer?
	BLT	..4		;Yes, write from end of buffer
;
	XCHG			;DE-> lower limit, HL = TXTBAS
	DAD	B		;HL-> where to write out to
	JRC	..2		;Too much if overflow
	CALL	CMHLDE		;Too much?
	BLT	..3		;No, branch
..2:	XCHG			;Yes, HL = max pointer
;
;	Write out text from TXTBAS to HL
;
..3:	PUSH	B		;Save # bytes to free up
	CALL	WRTTXT		;Write the text out
				;BC = # bytes that were written
	POP	H		;HL = # bytes to free up
	DSUB	B		;Subtract # bytes written
	RC			;Return if all + extra written (???)
	LXI	D,128		;DE = sector size
	CALL	CMHLDE
	RC			;Return if < 128 bytes remain
	MOV	B,H		;Save in BC
	MOV	C,L
;
;	Now write from end of buffer
;
..4:	LHLD	EDTPTR		;HL-> edit position
	LXI	D,2048		;Leave 2K alone
	DAD	D		;HL-> upper limit
	RC			;Return if overflow
	LDED	TXTRWF		;+DE-> end of text
	CALL	CMHLDE		;Too close to end of buffer
	RNC			;Yes, return, did our best
;
	XCHG			;DE-> upper limit, HL = TXTRWF
	DSUB	B		;HL-> where to write out to
	JRC	..5		;Too much if overflow
	CALL	CMHLDE		;Too much?
	BGE	..6		;No, branch
..5:	XCHG			;Yes, HL = max pointer
;
;	Write out text from HL to TXTRWF to .$R$ file
;
..6:	CALL	WRTBND		;Write the text out
	RET
	EPROC	ENCMD
	]			;IF DEBUG
	]			;IFNOT VPLUS
;
EOCMD	=	PRCMD
;
;	EP - Change edit parameters.
;
EPCMD:	LXI	H,PRMTBL	;HL-> INSTALLed parameter values
	LXI	D,EPTBL		;DE-> parameter table
	LXI	B,PARMNM	;C = max. parameter number
	JMP	ESCMD1		;Merge in below to set new parameter
;
;	EQ - QUIT
;
QALL:	DB	0
;
	BPROC	EZCMD
EZCMD:	INR	A		;A = 1
EQCMD:	STA	TEMPFL		;0 for EQ, 1 for EZ
;
	MVIB$	PARMFL,0FFH	;Flag parameter-expected for NXCMCH
	LXI	H,QUTMSG	;HL-> Quit message
	CALL	NXCMUC		;Get next command char; convert to UC
;
;	Quit all buffers?
;
	IF	VPLUS, [
	CPI	'A'		;Quit all buffers at once?
	BNE	..1		;No, skip
	MVIB$	COLFLG,':'	;Yes, tell EECMD to be quiet
	TST$	QALL		;Told YES already?
	JRNZ	..2		;Yes, branch
;
	CLR$	REGSTK		;Avoid E.B./macro execution error (last command)
	MVIB$	PARMFL,0FFH	;Flag parameter-expected for NXCMCH
	STA	QALL		;Conditionally set Quit All flag
	LXI	H,QTAMSG	;HL-> Quit All?
	CALL	NXCMCH		;Get next command char
	ANI	5FH		;Change LC to UC
..1:
	]
;
;	Check if really OK.
;
	CPI	'Y'		;Is it 'Y'
	BEQ	..2		;Yes, don't prompt
	CALL	PREPLY		;Give message, get reply
	JRZ	..2		;Branch if 'Y'
	CLR$	QALL		;Reset Quit All flag (EQA?...NO)
	RET
;
..2:	CALL	DELREV		;Delete any VEDIT.REV
	CALL	DELOUT		;Delete the .$$$ file
;
;	Handle EZ command.
;
	IFNOT	VPLUS, [
	TST$	TEMPFL		;Performing an EQ?
	JZ	EXITV		;Yes, back to CP/M after VEXIT()
				;Used to give extra CRLF
	]

	CALL	RELSRC		;Release input file and buffer
	CALL	STNSNL		;Make sure visual window rewritten
;
	IF	MPM, [
	CALL	CLSREN		;Release output file holding FCB
	]

	IFNOT	VPLUS, [
	CALL	INITRT		;Initialize variables
	JMP	BEGIN6		;Start empty edit session
	]

	IF	VPLUS, [
	CALL	INITXB		;Init edit buffer/file variables
	TST$	TEMPFL		;Performing an EQ?
	JNZ	TOCMD		;No, start empty edit session
	CALL	UPDVIS		;Yes, clear window
	CALL	EDTNAM		;;A = ASCII edit buffer name
	CALL	WSTCAL		;ES:HL-> corresponding window
	JRC	..END		;Branch if no such window
	INX$	H		;HL->.WWMODE
	MVI%ES	M,0		;Disable UPDVIS() in this window
..END:	JMP	EXTCHK		;Remove buffer, check for others open
	]
	EPROC	EZCMD
;
;	ER - Set up for new input file.
;
;       (Display input filname if no parameter)
;
ERCMD:	CALL	CKSPRM		;Any parameter string?
	JRZ	ERCMD1		;No, branch
;
	CALL	SETINP		;Setup INFCB (breaks out if name too long)
	CALL	OPNSRC		;Try to open the source file
	JZ	FNFBRK		;If no file, give error
	RET
;
;	Display input file name.
;
				;{EBCMD}
ERCMD1:	TST$	INFLG		;Is an input file open?
	RZ			;No, return
	LXI	H,INFMSG	;HL-> "INPUT FILE: "
	LXI	D,INFCB		;DE-> Input file name
	JMP	EWCMD2		;Merge with EW$
;
;	ES - Get new values for switches.  [1/31/86]
;
ESCMD:	LXI	H,SWTBL		;HL-> Installed switches
	LXI	D,ESTBL		;DE-> switch table
	LXI	B,SWCHNM	;C = max. switch number (EXTRN:ABS problem)
;
; ESCMD1 - Set new switch value & default value.  Leave default value if '-'.
;
				;{EPCMD,ESCMD}
	BPROC	ESCMD1
ESCMD1:	CPIB$	RMINUS,'-'	;Skip default-switch processing?
	BEQ	..ES1		;Yes, branch
	XCHG			;DE-> default switch table
	PUSH	H		;Save-> current switch table
	PUSH	B		;Save switch #
	LHLD	CMDGET		;Save-> parameter value
	PUSH	H
	MVIB$	PARMFL,0FFH	;Flag parameter-expected for NXCMCH
	CALL	GETDEC		;Just display?
	JRC	..ES0		;Yes, don't display default settings
	POP	H		;Backup to start of parameter
	SHLD	CMDGET
	PUSH	H		;Resave position
	CALL	..0		;Set default switch
..ES0:	POP	H
	SHLD	CMDGET		;Restore-> parameter value
	POP	B		;Retrieve parameter #
	POP	D		;Retrieve-> current switch table
..ES1:
	IF	MEMVRS, [
	CALL	..0
	JMP	HCRSOF		;IBM PC's sometime activated HW cursor
	]

..0:	MVIB$	PARMFL,0FFH	;Flag parameter-expected for NXCMCH
	CALL	GETDEC		;HL = switch number, BC & DE saved
	MOV	A,L		;Put it in A
;
;	Check for display-settings (no arguments).
;
	JRC	..3		;Branch if no argument given
	DCR	A		;Change to one less
	CMP	C		;Is switch number valid?
	JNC	PRMBRK		;No, give error
	XCHG			;HL-> proper table
	CALL	ADDAHL		;HL-> desired switch
;
;	Get new value.
;
	PUSH	H		;Save HL
	CALL	GETNXP		;HL = new switch value from scan line
	MOV	A,L		;Put it in A
	POP	H		;HL-> desired switch
;
;	Set/reset new value, set return value = old value.
;
	IF	VPLUS, [
	JRNC	..2		;Branch if 2d parm entered
	MOV	A,M		;Else retrieve current value
..2:	MOV	E,M		;Get old value
	MOV	M,A		;Set new value
	MVI	D,0		;DE = old value
	SDED$	RETVAL		;Make available externally via QV
	RET
	]

	IFNOT	VPLUS, [
	JRC	..3		;Branch if no argument given
	MOV	M,A		;Save new value
	RET
	]
;
;	Display current settings.
;
..3:	CMP	C		;Reached max?
	RNC			;Yes, return
	INR	A		;Bump switch #
	PUSHA			;Save switch #
	PUSH	B		;Save count
	PUSH	D		;Save -> table
	LXI	H,10		;Set to convert two digits
	MOV	C,A
	MVI	B,00		;BC = 8 bit switch #
	CALL	PRTDEC		;Convert and display #
	MVI	A,':'		;Type ":"
	CALL	PCHARA
	CALL	PSPACE
	POP	H		;HL-> table
	PUSH	H		;Save again
	MOV	C,M		;Get table value
	MVI	B,00		;HL = 8 bit value
	CALL	UCMD2		;Output value and CRLF
	POP	D		;DE-> table
	INX$	D		;Bump ->
	POP	B		;Restore counter
	POPA			;Restore switch #
	JMPR	..3		;Loop
	EPROC	ESCMD1
;
; 	ET - Get new positions for Tabs.	[9/19/86]
;
	BPROC	ETCMD
ETCMD:	CALL	BAKGET		;Prepare for GETNXP()
	LHLD	CMDGET
	PUSH	H
	LXI	D,TABTBL	;DE-> tab position table
	CALL	..TPRC		;Process for current edit buffer
;
;	Set default tabs.
;
	POP	H		;Retrieve-> tab values
	CPIB$	RMINUS,'-'	;Also process default tabs?
	RZ			;No, return
;
	SHLD	CMDGET		;Reset GET to start of tabs
	LXI	D,TABPOS	;DE-> default tabs
;
..TPRC:	MVI	B,33		;Up to 33 tab positions
..INDV:	CALL	GETNXP		;Get next number in HL
	JRC	..NOMO		;Branch if last argument reached
	MOV	A,L		;A = value
	STAX	D		;No, save in table
	INX$	D		;Bump PTR
	DCR	B		;Decrement count
	JRNZ	..INDV		;Continue unless count exhausted
;
;	If no #'s entered, give error.
;	If only one # entered, setup uniform positions.
;
..NOMO:	MOV	A,B		;Get remaining count
	CPI	32		;Was 0 or 1 # entered?
	BLT	..TEND		;No, two or more - branch
	JNZ	SHWTAB		;0 entered - display current settings
	DCX$	D		;DE-> first number
	LDAX	D		;Get # as first tab pos
	MOV	C,A		;Get number as "delta"
	INR	A		;Adjust
	INR	B		;Adjust
;
..UNFM:	STAX	D		;Save position in table
	INX$	D		;Bump PTR
	ADD	C		;Compute next pos
	JRC	..TEND		;Branch if > 255
	DCR	B		;Decrement count
	JRNZ	..UNFM		;Repeat until done
;
..TEND:	MVI	A,0FFH		;Get table fence = 0FFH
	STAX	D		;Save fence in table
	RET
	EPROC	ETCMD
;
; SHWTAB - Display tab settings from DE-> table.
;
NTPROW	=	8		;# tabs displayed per row

				;{SHWTAB}
	BPROC	SHWTAB
SHWTAB:	LXI	H,TABPOS	;Default tabs?
	CALL	CMHLDE
	RZ			;Yes, don't display
;
	XCHG			;HL-> next tab pos
	MVI	B,0		;MSB of tabs = 0
;
..1:	MVI	D,NTPROW	;# tabs displayed per row
;
..2:	MOV	A,M		;Get next tab pos
	CPI	0FFH		;All done>
	JZ	CRLF		;Yes, output CR LF
	MOV	C,A		;BC = tab pos
	CALL	PRTDEC		;Display it, right justified
	INX$	H		;HL-> next tab pos
	DCR	D		;No, reached end of line?
	BNE	..2		;No, loop
;
	CALL	CRLF		;Start new line
	JMPR	..1		;Loop
	EPROC	SHWTAB
;
;	EU - Set/Show Drive/User-# (See VEDITF1)
;
;
;	EV - Print VEDIT version number.
;
EVCMD:	LXI	H,VERSMS	;HL-> version message
	JMP	PRTMSG		;Print it without initial CRLF
;
;	EW - Set up for new output file.
;
;       (Display output filname if no parameter)
;
	BPROC	EWCMD
EWCMD:	CALL	CKSPRM		;Any parameter string?
	JRZ	EWCMD1		;No, branch
;
	CALL	WTCLCH		;Error if output file open
	CALL	SETREN		;Setup RENFCB (breaks out if name too long)
	JMP	CREOUT		;Create the output file
;
;	Display output file name.
;
				;{EBCMD}
EWCMD1:	TST$	OUTFLG		;Is an output file open?
	RZ			;No, return
	LXI	H,OUFMSG	;HL-> "OUTPUT FILE: "
	LXI	D,RENFCB	;DE-> Output file name
;
				;{ERCMD}
EWCMD2:	TST$	COLFLG		;":" option?
	JRNZ	..3		;Yes, skip first part of message
	CALL	PRTSTR		;No, give "OUTPUT/INPUT FILE: "
..3:	XCHG			;HL-> File name
	CPIB$	RPLUS,'+'	;Want drive/user#/path too?
	BEQ	..4		;Yes, branch
	CALL	PRTFCB		;No, display just the filename
	JMPR	..DONE
..4:	CALL	PRTFN1		;Yes, display the drive... & filename
..DONE:	JMP	IFCRLF		;Give CR-LF unless ":" option
	EPROC	EWCMD
;
;	EX - Exit to CP/M.
;
	BPROC	EXCMD
EXCMD:
	IFNOT	VPLUS, [
	CALL	WTOPCH		;Error if output file not open
	POP	H		;Ignore return address
	JMP	RESTOP		;Close files on return
	]

	IF	VPLUS, [
	MVIB$	PARMFL,0FFH	;Flag parameter-expected for NXCMCH
	CALL	NXCMUC		;;Advance GET to possible "A";  convert to UC
	CPI	'A'		;All?
	JRZ	..1		;Yes, branch
	CALL	WTOPCH		;No, error if output file not open
	JMPR	..2		;Branch
;
..1:	MVIB	COLFLG,':'	;Tell EECMD to be quiet
	STA	QALL		;Set Quit All flag
	CLR$	REGSTK		;Avoid E.B./macro execution error (last command)

	TST$	OUTFLG		;Open output file?
	BEQ	EXTCHK		;No, skip
;
..2:	LXI	H,SAVMSG	;HL -> 'SAVING: '
	CALL	PRTSTR
	CALL	PRTFNM		;Display full output filename
	CALL	CRLF
	CALL	CLOSER		;Close files
				;;BREAK out if write/close error
;;	JMPR	EXTCHK
	EPROC	EXCMD
;
; EXTCHK - Exit program unless more open edit buffers.
;
				;{EXCMD^, EQCMD} [TOCMD, FRMCMD]
	BPROC	EXTCHK
EXTCHK:	CALL	FRSTBF		;Only one buffer left?
	JZ	FRMCMD		;Yes, exit to operating system
;
;	More open edit buffers.  Delete current (empty) buffer.
;
	CALL	DELBUF		;Remove current edit buffer from list
;
;	Determine which edit buffer to activate.
;
	CPIB$	ATTFLG,'@'	;Explicitly ordered to the main buffer?
	MVI	A,MAINRG	;Presume yes
	BEQ	..1		;Yes, activate the main edit buffer
;
;	Activate main edit buffer if EXY or EQY issued from a macro.
;
	CPIB$	REGEXN,0FFH	;Macro running?
	BEQ	..0		;No, skip
	TST$	QALL		;All?
	MVI	A,MAINRG	;'@' in case not
	JRZ	..1		;No, go open main buffer
;
;	Exit to 'first' buffer.
;
..0:	CALL	FRSTBF		;Point HL to 1st edit buffer in BUFTBL
	CALL	P2N		;A = buffer name
;
;	Open up selected buffer for further editing operations.
;
..1:	STA	REGNUM		;Save buffer # for EECMD
	CALL	EECMD0		;Activate the selected edit buffer
;
;	ALL buffers to be processed?
;
	TST$	QALL		;All?
	JRZ	..2		;No, skip
	LHLD	BR8KPT		;HL-> past 'E' of EX,EQ
	DCX$	H
	SHLD	CMDGET
	JMPR	..3
;
;	+EX, +EY?
;
..2:	CPIB$	RPLUS,'+'
	BNE	..3
	CALL	FRSTBF		;Just '@' buffer left?
	JRNZ	..3		;No, branch
	TST$	OUTFLG		;Open output file?
	JRNZ	..3		;Yes, branch
	SUB%BC	TXTCEL,TXTFLR	;Any text in the buffer?
	JZ	FRMCMD		;No, exit
;
;	Reset to command mode.
;
..3:
	JMP	TOCMD
	EPROC	EXTCHK
	]			;<IF VPLUS>

;	EY - See CLOSER.


;	EZ - See EQ.

