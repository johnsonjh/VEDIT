	.PAGE
	.TITLE	'VEDIT-C0'
;
;	Entry and BREAK for Command Mode
;
;
PRMBRK:	LXI	H,PRMMSG	;HL-> error message
	JMPR	MSGBRK		;Give error
;
;	B R E A K - Aborts any execution and returns to command mode
;		    Stack is reset, so don't worry about stack contents
;
	IFNOT	VPLUS, [
				;{BRKCHK...}
BREAK:	LXI	H,BRKMSG	;HL-> BREAK mesage
;
MSGBRK:	CALL	RSTCON		;Turn off listing, output to console
	CLR	VISFLG		;Now in command mode
;
	STA	RIFLG		;No longer RI command
	CALL	PCRMSG		;Print CRLF and the message
;
	IF	MPM,[
	CALL	CLSAUX		;Close AUXFCB
	]
;
	CALL	PURGEK		;Purge any pending key strokes
;
;	Clear Search string if waiting for terminator
;
	TSTM	SRCHFL		;Waiting for terminator?
	MVI	M,0		;Clear flag
	JRZ	..9		;No, branch
	MVIB$	TARGST,SRCEOS	;Yes, clear search string
..9:	
;	JMP	MAIN0		;Stack will be reset
	]			;<IFNOT VPLUS>
;
;	C O M M A N D   M O D E   Main Loop
;
				;{BREAK^,CMDERR,NOTSUC}
ENDBRK:
BR8K:
MAIN0:	CALL	RESTOP		;Restore top of window if not already.
	CALL	SETTOP		;Make sure text window set.
	CLR$	ITRSTK		;Make sure no iterations executing
	CALL	NEWCMD		;Get next command, init pointers.
;
MAIN:	LXI	SP,STACK-2	;Reset stack.
	CALL	PROCMD		;Process commands forever.
	JMPR	MAIN
