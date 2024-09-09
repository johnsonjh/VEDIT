	.PAGE
;
;	Last Change: Ted - Jan. 11, 1985
;
; SETKEY - Customize the Keyboard layout.
;
;	   Get up to two Escape-sequence lead in characters
;	   and any character to ignore in escape sequences.
;
SETKEY:	MVI	A,255		;Flag control-C will not abort
	STA	CTRLCF		;
	XRA	A		;Get a zero.
	STA	ESCFLG		;Make sure escape sequ. flag is reset.
	STA	ULCFLG		;Clear flag in case no ESC chars specified
	LHLD	CHRTBL		;HL-> first entry in character table.
	XCHG			;DE-> ESCCHR #1 in special character table.
	LXI	B,ES1MSG	;BC-> ESC char. message.
	XRA	A		;
	STA	CURCOL		;
	CALL	PRTMSG		;Print the message.
	CALL	TABOVR		;Tab over
	CALL	CONINR		;Get the reply.
	CALL	OUTKYP		;Display the reply
;
;	Ignore any characters immediately following the ESC char
;
	PUSHA			;Save ESC char
SETY1:	CALL	CONSTR		;Is another char ready?
	JZ	SETY2		;No, branch
	CALL	CONINR		;Yes, get and ignore it
	JMP	SETY1		;Check for another
;
SETY2:	POPA			;Restore ESC char
	CPI	CR		;Using escape sequences?
	CZ	SETZER		;No, change to 'unused' char.
	STA	ESCCH1		;Save internal copy of Escape char.
	CALL	MVAPAR		;Put char. into table.
	CZ	MVAPAR		;No, clear second Escape char. too.
	JZ	SETKE1		;No, branch around.
;
	LXI	B,ES2MSG	;BC-> ESC char. #2 message.
	XRA	A		;
	STA	CURCOL		;
	CALL	PRTMSG		;Print the message.
	CALL	TABOVR		;Tab over
	CALL	CONINR		;Get the reply.
	CALL	OUTKYP		;Display the reply
;
;	Ignore any characters immediately following the ESC char
;
	PUSHA			;Save ESC char
SETY3:	CALL	CONSTR		;Is another char ready?
	JZ	SETY4		;No, branch
	CALL	CONINR		;Yes, get and ignore it
	JMP	SETY3		;Check for another
;
SETY4:	POPA			;Restore ESC char
	CPI	CR		;Using escape sequences?
	CZ	SETZER		;No, change to 'unused' char.
	STA	ESCCH2		;Save internal copy of Escape char.
	CALL	MVAPAR		;Put char. into table.
;
;	Get two NOOP characters.
;
	LXI	B,ES3MSG	;BC-> ESC NOOP message.
	XRA	A		;
	STA	CURCOL		;
	CALL	PRTMSG		;Print the message.
	CALL	TABOVR		;Tab over
	CALL	CONINR		;Get the reply.
	CALL	OUTKYP		;Display the reply
	ORI	080H		;Set bit 7 for our purposes.
	CPI	CR+80H		;Is there a NOOP char.?
	CZ	SETZER		;No, change to 'unused' char.
	STA	ESCNP1		;Save internal copy of NOOP #1 char.
	CALL	MVAPAR		;Put char. into table.
	JZ	SETKE0		;No, clear NOOP #2 char too.
;
	LXI	B,ES4MSG	;BC-> ESC NOOP  # 2 message.
	XRA	A		;
	STA	CURCOL		;
	CALL	PRTMSG		;Print the message.
	CALL	TABOVR		;Tab over
	CALL	CONINR		;Get the reply.
	CALL	OUTKYP		;Display the reply
	ORI	080H		;Set bit 7 for our purposes.
	CPI	CR+80H		;Is there a NOOP char.?
	CZ	SETZER		;No, change to 'unused' char.
SETKE0:	STA	ESCNP2		;Save internal copy of NOOP #1 char.
	CALL	MVAPAR		;Put char. into table.
;
; Check if escape sequences are same for upper/lower case
;
SETK00:	LXI	B,ULCMSG	;Print message
	CALL	PRDCCH		;and get response
	JZ	SETK00		;Rubout, ask again
	MOV	A,L		;Put response into A
	CPI	4+1		;Too big?
	JNC	SETK00		;Yup ...
	STA	ULCFLG		;Save flags
;
;	Check if Escape char #1 is displayable.  If it is make
;	<esc><esc> the literal <esc> character.
;
SETKE1:	LXI	B,MESSAG	;BC-> control function messages.
	LHLD	CNTADD		;HL-> address of first routine.
	LXI	D,6		;Offset to skip over addresses.
	DAD	D		;HL-> LITESC routine address.
	SHLD	FNCADD		;Save function address
;	XCHG			;DE-> LITESC routine address.
;
	LDA	ESCCH1		;Get the Escape char. back.
	CPI	SPACE		;Is escape char. displayable?
	JC	SETK11		;No, branch to skip over LITESC.
	ORI	080H		;Yes, enter LITESC into ESC Decode table #1
	STA	TMPCHR		;Save char
	MVI	A,1		;Use table #1
	STA	ESCNUM		;Save table #
	JMP	SETKE7		;Branch for entry.
;
;	Entry for "B" to start from beginning
;
SETK1:	CALL	KEYINI		;Clear out old layout
	LXI	B,MESSAG	;BC-> control function messages.
	LHLD	CNTADD		;HL-> address of first routine.
	LXI	D,6		;Offset to skip over addresses.
	DAD	D		;HL-> LITESC routine address.
	SHLD	FNCADD		;Save function address
;
;	Prompt for each key, and receive either control char.,
;	escape sequence or Bit 8 character.
;
SETK11:	LHLD	FNCADD		;Get function address
	INX	H		;Point to next function routine addr.
	INX	H
	SHLD	FNCADD		;Save new function address
;
SETKE2:	XRA	A		;Clear current column
	STA	CURCOL		;
;
	MVI	A,2		;Bit 8 chars goto table #2
	STA	ESCNUM		;Save initial ESCNUM
;
	CALL	PRTMSG		;Print the next message.
	RZ			;Return if done here.
	CALL	TABOVR
SETKE3:	CALL	CONINR		;Get control character.
	CALL	OUTKYP		;Display the character
	STA	TMPCHR		;Save char
	ORA	A		;Is it a Bit 8 char?
	JM	SETKE5		;Yes, branch now, ESCNUM = 2
	LXI	H,ESCFLG	;HL-> Escape mode flag, 80H if set.
	ORA	M		;OR escape mode bit.
	STA	TMPCHR		;Save char
	JM	SETK9		;Branch if part of escape sequence
;
;	Check for Escape lead-in char. ;(11/26/84) - check for ESC #1 first
;
	MVI	E,1		;Assume ESC table # 1
	LXI	H,ESCCH1	;HL-> escape leadin char #1
	CMP	M		;Have the escape char?
	JZ	SETKE4		;Yes, branch.
	INR	E		;E = 2, assume ESC table # 2
	INX	H		;HL-> ESCCH2, escape char #2
	CMP	M		;Have the other escape char?
	JNZ	SETKE6		;No, branch.
;
SETKE4:	LXI	H,ESCFLG	;Yes, HL-> ESCFLG, escape mode flag.
	MVI	M,80H		;Set the escape flag.
SETK4:	MOV	A,E		;Get current ESCNUM
	STA	ESCNUM		;Save it for later
	JMP	SETKE3		;Get next char.
;
;	Begin decoding ESC sequences, check for NOOP characters first
;
SETK9:	MVI	E,2		;Assume a NOOP will be found
	LXI	H,ESCNP2	;HL-> ESCNP2, escape NOOP #2.
	CMP	M		;Have NOOP #2?
	JZ	SETK4		;Yes, ignore it, but set ESCNUM = 2
	DCX	H		;HL-> ESCNP1, escape NOOP #1.
	CMP	M		;Have NOOP #1?
	JZ	SETK4		;Yes, ignore it, but set ESCNUM = 2
;
;	Check if ANSI escape sequence
;
	IF	DECRAIN, [
	STA	TMPCHR		;;Save char with Bit 8
	ANI	07FH		;;Strip ESC flag bit
	CALL	DIGCHK		;;Check if its a digit
	JNZ	SETKE5		;;No, branch
	MVI	A,5		;;Setup to delay 5 mS
	CALL	DELAY		;;Delay and Poll keyboard
	CALL	CONSTR		;;Is a char. ready?
	ORA	A		;;A = 00 if no char
	JZ	SETKE5		;;No, branch
	CALL	CONINR		;;Yes, get char
	CALL	OUTKYP		;;Display it
	CALL	DIGCHK		;;Check if its a digit
	JNZ	SETKE5		;;No, branch
	PUSH	B
	MOV	B,A		;;B = second digit
	LDA	TMPCHR
	MOV	C,A		;;C = first digit
	CALL	CVANSI		;;Convert to single char in A
	POP	B
	STA	TMPCHR		;;Save char
	]			;<IF DECRAIN>
;
;	Look for escape sequences ending in <CR>.
;
SETKE5:
	IF	CRTVRS ! DECRAIN, [
	MVI	A,30		;Yes, get value for next char. delay at 300 baud.
	CALL	DELAY		;Delay for # milliseconds in reg. A.
	CALL	CONSTR		;Did escape sequ. end in CR?
	ORA	A		;Make sure flags are set.
	JZ	SETKE6		;No CR, branch.
;
;	Make the entry in the ESC - CR table, based on ESCNUM
;
	CALL	CONINR		;Yes, get the CR to clear it out
	LHLD	ESCTB1		;HL-> ESC - CR table #1 in VEDIT
;
	IF	TAB138, [
	LDA	ESCNUM		;Get ESC table #
	CPI	1		;Is it # 1?
	JZ	SETK5		;Yes, branch
	LHLD	ESCTB2		;No, use ESC - CR table #2
	]
;
SETK5:	LDA	TMPCHR		;Get the char. back in A.
	ANI	7FH		;Strip top bit.
	CALL	ADDAHL		;HL-> entry for escape sequ. char.
	MVI	M,CR		;Set table to specify that sequ. ends in CR.
	]			;<IF CRTVRS ! DECRAIN>
;
;
; If Upper,lower case are different for character entered, clear ESCFLG
; if same, set ESCFLG = 80H
;
SETKE6:	LXI	H,ESCFLG	;Escape character?
	MOV	A,M
	ORA	A
	JP	SETKNE		;No ...
	LDA	ULCFLG		;Yes, U/L same?
	ANI	1
	JNZ	SETKE7		;Yes ...
	MVI	M,0		;Nope, clear escape flag
	JMP	SETKE7		;Esc char same for U/L
;
;	NOT DOCUMENTED -
; 	If Bit 7 char, UCLFLG = 2 makes UC/LC equivalent
;
SETKNE:	LDA	TMPCHR		;"AND" escape character bit with bit 7 if character
	ORA	A		;Bit 7 character?
	JP	SETKE7		;Nope ...
	LDA	ULCFLG		;Yes, U/L same?
	ANI	2
	JZ	SETKE7		;Nope ...
	MVI	M,80H		;Yes, set escape flag
;
;	Decode which table to use for char in TMPCHR, also based on ESCNUM
;
SETKE7:	LDA	TMPCHR		;Get character
	ORA	A		;Is top bit set?
	JM	SETK8		;Yes, branch.
	LHLD	CTLDEC		;No, use <CTRL> decode table
	CPI	7FH		;Is char a RUBOUT?
	PUSHF			;Save flags
	PUSHA			;Save character
	MVI	A,40H		;Get offset to RUBROU.
	CALL	ADDAHL		;HL-> RUBROU.
	POPA			;Restore character
	POPF			;Restore flags
	JZ	SETKE9		;Yes, a RUBOUT - branch.
	LHLD	CTLDEC		;No, HL-> <CTRL> decode table.
	CPI	20H		;Is char. valid?
	JNC	SETK2		;No, give error message.
	CPI	CR		;Is char. a CR?
	JZ	SETK11		;Yes, ignore this function.
	JMP	SETKE8		;No, enter into <CTRL> decode table
;
SETK2:	ANI	5FH
	CPI	'B'
	JZ	SETK1
	JMP	VALERR		;No, give error
;
;	Decode Escape Sequences and Bit 7 chars based on ESCNUM
;
SETK8:	LHLD	ESCDC1		;HL-> ESC decode table #1 in VEDIT.
;
	IF	TAB138, [
	LDA	ESCNUM		;Get ESC table #
	CPI	1		;Is it # 1?
	JZ	SETKE8		;Yes, branch
	LHLD	ESCDC2		;No, use ESC decode table #2
	]
;
SETKE8:	LDA	TMPCHR		;Get char
	ANI	07FH		;Strip any high bit
	ADD	A		;Create word index into table.
	CALL	ADDAHL		;Compute address in table.
;
;	Make sure that table entry not already used.
;
SETKE9:	PUSH	H		;Save -> VEDIT table.
	CALL	MVINHL		;Get current table address.
	XCHG			;Temp save in DE.
;
	LHLD	CNTADD		;HL-> VNOOP address in VEDIT.
	CALL	MVINHL		;Get the VNOOP address in HL.
	CALL	CMHLDE		;Is table currently empty?
	POP	H		;Restore PTR
	JNZ	DUPERR		;No, give error and reprompt.
;
;	Make the Table entry.
;
SETK12:	PUSH	H		;Save HL
	LHLD	FNCADD		;HL = address of routine
	XCHG			;DE = address of routine
	POP	H		;Restore HL
	CALL	MVDETB		;Move routine addr. to table.
;
;	Make upper and lower case escape sequences equivalent, if
;	ESCFLG = 80H
;
	LXI	H,ESCFLG	;HL-> Escape Mode flag.
	MOV	A,M		;Get the flag (80H if set).
	MVI	M,00		;Turn the flag OFF.
	ORA	A		;Was char part of escape sequence?
	JP	SETK11		;No, branch.
	LDA	TMPCHR		;Yes, get the char. back.
	CPI	'A'+80H		;Is it at least an 'A'?
	JC	SETK11		;No, not a letter.
	CPI	'z'+81H		;Is it greater than 'z'?
	JNC	SETK11		;Yes, not a letter.
	CPI	'a'+80H		;Is it lower case?
	JNC	SETK10		;Yes, reverse to upper case.
	CPI	'Z'+81H		;Is it upper case?
	JNC	SETK11		;No, not a letter.
;
SETK10:	XRI	020H		;Reverse upper and lower case.
	STA	TMPCHR		;Save char
	JMP	SETKE7		;Insert reverse letter into table.
;
; VALERR, DUPERR - Print error message, move BC back to message
;	   beginning.
;
VALERR:	PUSH	B		;Save message pointer.
	LXI	B,VALMSG	;BC-> invalid char. error message.
	JMP	DUPER1		;Merge in below to process error.
;
DUPERR:	PUSH	B		;Save message pointer.
	LXI	B,DUPMSG	;BC-> duplicate char. error message.
DUPER1:	CALL	PRTMSG		;Print it.
	POP	B		;Get message pointer back.
	DCX	B		;Move to last delimiter.
DUPER2:	DCX	B		;Move one char back.
	LDAX	B		;Get character.
	ANI	80H		;Is it the last delimiter?
	JZ	DUPER2		;No, continue looking.
;
	INX	B		;BC-> beginning of message.
	XRA	A		; Clear ESC flag
	STA	ESCFLG
	JMP	SETKE2		;Ask for character again.
;
; CVANSI - Convert ASCII digits in BC to single char in C
;
	IF	DECRAIN, [
CVANSI:	LXI	H,0		;;Init counter to 0000
	MOV	A,C		;;Get TENS digit
	ANI	7FH
	CALL	MULT10
	MOV	A,B		;;Get ONES digit
	ANI	7FH
	CALL	MULT10
	MOV	A,L
	ORI	080H		;;Set top bit as ESC flag
	MOV	C,A		;;Return char. in C and A
	RET
	]			;<IF DECRAIN>
;
;
; DIGCHK - Check for decimal digit.  Return: 'Z' if digit.
;
DIGCHK:	CPI	'0'
	RC			;Return 'NZ' if < '0'
	CPI	'9'+1
	JNC	RET%NZ		;Return 'NZ' if > '9'
	CMP	A		;Set 'Z'
	RET
;
RET%NZ:	ORA	A
	RNZ			;Return 'NZ' and 'NC'
	CPI	1		;If A = 00, this will set 'NZ'
	RET			;Return 'NZ' and 'C'
;
; MULT10 - Multiply HL by 10 and add digit in reg. A
;
				;{CVANSI}
MULT10:	MOV	D,H
	MOV	E,L
	DAD	H
	DAD	H
	DAD	D		;;Mult. by 5
	DAD	H		;;Mult by 10
	SUI	'0'
	JMP	ADDAHL
;
