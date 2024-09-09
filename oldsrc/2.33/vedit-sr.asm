	.TITLE	'VEDIT-SR'
	.PAGE
;****************************************
;*					*
;*	Search Routines			*
;*					*
;****************************************
;
; Copyright (C) 1986 by CompuView Products, Inc.
;			1955 Pauline Blvd.
;			Ann Arbor, MI 48103
;
;	Written by: Theodore J. Green
;
;	Last Change: Tom - May  15, 1986 - PARMFL into STSEAR
;
;
; STSEAR - Set up for search by placing search string in TARGST.
;
STSEAR:	MVIW	SRCHCN,00	;Reset search count
	SHLD	SRFAIL		;Reset fail PTR
	CLR$	EOBFLG		;Flag that |L matched unterminated last line
	CPI%M	CMDGET,ESC	;Is next char ESC?
	RZ			;Yes, use previous string (or null)
	CALL	SETTRM		;No, set terminator
	MOV	B,A		;Save in B
	CLR$	FNDFLG		;Turn off visual FIND flag
	LXI	H,TARGST	;HL-> string storage
	LXI	D,TARGST+TARGLN	;Max value for TARSET
;
STSEA1:	CALL	NXTCHR		;Get next string char
	CMP	B		;Reached terminator?
	BEQ	STSEA2		;Yes, end of search string found
	CALL	TARSET		;Put char into TARGST
	JMPR	STSEA1		;Get rest of string
;
STSEA2:	MOV	M,B		;Yes, terminate new string
				;{ERRHND,ICMD,RICMD,YTCMD}
ENDTRM:	CLR$	SRCHFL		;Clear flag - terminator found
	RET
;
; TARSET - Insert char in A into string <- HL.  Check HL against DE.
;		[3/22/86]
;
				;{STSEAR}
TARSET:	MOV	C,A		;Save char in C
	CALL	CMHLDE		;Reached limit?
	CMC
	RC			;Yes, return 'C'
;
				;{FORCML}
CHKUCC:	TST$	SRCNSW		;Search to ignore upper/lower case?
	MOV	A,C		;Get char. back in A
	CNZ	CONVUC		;Yes, convert lower to upper case
	MOV	M,A		;No, save char in storage
	INX$	H		;Bump string PTR
	ORA	A		;Clear 'C'
	RET
;
; SETTRM - Check for and set terminating character.
;	   Return: Terminator in A, HL clobbered.
;
				;{ICMD,RICMD,STSEAR,YTCMD}
SETTRM:	TST$	ATTFLG		;Is terminator specified?
	LXI	H,TERMCH	;HL-> terminator
	MVI	A,ESC		;Get default ESC
	JRZ	SETTR1		;Set to ESC
	CALL	NXCMCH		;Yes, get terminator
SETTR1:	MOV	M,A		;Set terminator
	STA	SRCHFL		;Flag - waiting for terminator
	RET
	.PAGE
;
; SEARTX - Perform search for string in TARGST.
;	   Checks if Upper/lower case distinction to be ignored.
;	   Return: 'C' if string not found, SRFAIL -> possible partial match
;		   DE-> begin of string, HL-> end of string + 1
;
;	Changed: 11/8/85
;
				;{FMCD,SCMD,FNDEXE}
SEARTX:	LHLD	SRFAIL		;HL-> where last search failed
	MOV	A,H		;Is PTR set?
	ORA	L
	JRNZ	SEARC1		;Yes, use it
	LHLD	EDTPTR		;No, start search at EDTPTR
SEARC1:	CLR	MANYFL		;Clear |M match flag
	STA	SRFAIL		;Clear SRFAIL
	STA	SRFAIL+1
;
;	Loop here when entire string not found
;
SEARC2:	XCHG			;DE-> Begin of search
	MVI	B,5FH		;Assume doing Lower/ Upper case ignored
	TST$	SRCNSW		;Is upper /lower case ignored?
	JRNZ	SEARC0		;Yes, branch
	MVI	B,0FFH		;No, set that ANA B has no effect
SEARC0:	MVI	C,EOF		;C = text terminator
	LXI	H,TARGST	;HL-> search string
;
;	Use separate loop if first search char is wildcard
;
	LDA	TERMCH+1	;A = wildcard "|"
	CMP	M		;Start with wildcard?
	JRNZ	SEARW2		;No, use regular loop
;
	MVIW$	TEMPW,00	;Clear flag used for |W & |L
SEARW6:	LXI	H,TARGST	;Back to begin of search string
				;Change SMATCH so LXI H,TARGST not needed
	CALL	SMATCH
	JRZ	SEARC3		;Branch if match
	LDAX	D		;Get char that didn't match
	CMP	C		;Is it EOF?
	BEQ	SEAERR		;Yes, give error
	INX$	D		;No, DE-> next byte in text buffer
	JMPR	SEARW6
;
;	A leading |W or |L requires a kludge because it may match multiple characters
;
SEARC3:	LBCD	TEMPW		;BC-> possible begin of |W or |L
	MOV	A,B
	ORA	C		;Was it a |W or |L?
	JRZ	SEARW3		;No, branch
	PUSH	B		;Yes, save position of first byte
	JMPR	SEARW5		;Pretty kludgy
;
;	Search for first character, HL-> first char of search string
;	DE-> text, C = EOF, B = mask for upper / lower case ignore
;
SEARW1:	CMP	C		;Is non-matching char. an EOF?
	JRZ	SEAERR		;Yes, give error
	INX$	D		;No, DE-> next byte in text buffer
SEARW2:	LDAX	D		;Get char. from text buffer
	CPI	'a'		;Is char less than 'a'?
	JRC	SEARC4		;Yes, branch
	CPI	'z'+1		;Is char greater than 'z'?
	JRNC	SEARC4		;Yes, branch
	ANA	B		;No, convert to upper case if B = 5FH
SEARC4:	CMP	M		;Does it match?
	BNE	SEARW1		;No, check next text byte
;
;	Check rest of string for match
;
SEARW3:	PUSH	D		;Save position of first byte
SEARW5:	LBCD	TERMCH		;C = terminator, B = '|' wildcard
;
SEARW4:	INX$	D		;DE-> next byte in text buffer
	INX$	H		;HL-> next byte of search string
	MOV	A,M		;Get search char
	CMP	B		;Is search byte a wildcard?
	BEQ	SEARC9		;Yes, branch
	CMP	C		;Reached end of search string?
	BEQ	SEARDN		;Yes, search is done
;
	TST$	SRCNSW		;Upper / lower case ignored?
	LDAX	D		;Get next text byte
	CNZ	CONVUC		;Yes, convert lower to upper case
	CMP	M		;Do we still match?
	BEQ	SEARW4		;Yes, keep going
;
;	If |M was previously encountered, MANYFL will be set
;
NOMATC:	LDAX	D		;Get non-matching text char
	CPI	EOF		;At end of file?
	BEQ	SEAER1		;Yes, give error
;
	TST$	MANYFL		;Was |M encountered?
	JRNZ	SEARW7		;Yes, keep matching
;
	POP	H		;HL-> first text char to match
	INX$	H		;HL-> where to restart entire search
	JMPR	SEARC2		;Restore target PTR and continue
;
;	If |M was encountered, set HL-> past |M, bump DE by one
;
SEARW7:	LDED$	STEMP2		;DE = last match due to |M
	INCW$	STEMP2		;Account for this |M match
;	LHLD	TXTTOP		;HL-> end of text
;	CALL	CMHLDE		;Reached end?
;	BLT	SEAER1		;Yes, give error
	LHLD	STEMP1		;HL-> pattern to match
	JMPR	SEARW4		;Keep trying to match
;
SEARC9:	CALL	SMATCH		;Check for special match
	JRNZ	NOMATC		;Branch if no match
	JMPR	SEARW4		;Branch if match
;
;	DE-> past found string
;
SEARDN:	INCW$	SRCHCN		;Increment the search count
	CLR$	SRCERR		;Clear error flag
	LHLD	TXTEND		;HL-> past end of text
	CALL	MNHLDE		;HL must not be greater than TXTTEND
	POP	D		;DE-> begin of string
	ORA	A		;Set 'NC'
	RET			;Return DE-> begin, HL-> past end of string
;
SEAER1:	POP	D		;DE-> where failed search started
SEAERR:	SDED$	SRFAIL		;Save -> start search after disk buffering
	MVIB$	SRCERR,0FFH	;Needed by VFNEXE
	STC			;Not found, return with 'C' error flag
	RET
;
; SMATCH - Check if match for special catagories of characters
;	   HL-> search string, DE-> text
;	   Return: 'Z' if match, 'NZ' if not match.
;
SMATCH:	INX$	H		;HL-> match control char
	MOV	A,M		;A = match control char
	ANI	05FH		;Make lower to upper case
	CPI	'X'		;Match any char?
	RZ			;Yes, return 'Z' now
	CPI	'A'		;Match alphabetic?
	JZ	SLETCH		;Check if letter
	CPI	'D'		;Match decimal digit?
	JZ	SDIGCH		;Check if digit
	CPI	'L'		;Match line terminator?
	JZ	SLINTM
	CPI	'C'		;Match control char?
	JRZ	SCTLCH
	CPI	'S'		;Match separator?
	JRZ	SSEPCH
	CPI	'F'		;Match alphanumeric?
	JRZ	SALPCH
	CPI	'U'		;Match UC?
	JZ	SUCCH
	CPI	'V'		;Match LC?
	JZ	SLCCH
	CPI	'B'		;Match single blank or tab?
	JZ	SBLKCH
	CPI	'W'		;Match white space?
	JRZ	SWHTCH
	CPI	'M'		;Match multiple chars?
	JZ	SMNYCH
	CPI	'N'		;Match "not" char?
	JRZ	SNOTCH
				;Else use char literally
	LDAX	D		;Get text char
	DCX$	H		;HL-> wildcard char
	CMP	M		;Does it match?
	INX	H		;(FLAGS) restore PTR
	RET
;
;	Individual match routines - 'Z' means match, 'NZ' means no match
;
SCTLCH:	LDAX	D
	CPI	' '		;A control char?
SCTLC1:	JNC	RET%NZ		;No, return NZ
	CMP	A
	RET			;Yes, return Z
;
SSEPCH:	LDAX	D
	CALL	LETCHK		;Is it a letter?
	JZ	RET%NZ		;Yes, return NZ
	CALL	DIGCHK		;Is it a number?
;
;	Change Z to NZ and NZ to Z
;
C%Z%NZ:	JZ	RET%NZ		;Yes, return NZ
	CMP	A
	RET			;Else, return Z
;
SALPCH:	LDAX	D
	CALL	LETCHK		;Is it a letter?
	RZ			;Yes, return Z
	JMP	DIGCHK		;Is it a number?
;
SWHTCH:	CALL	SBLKCH		;Is it a tab or space?
	RNZ			;No, return NZ
	SDED	TEMPW		;Save -> to begin of |W match
SWHTC1:	INX$	D		;Bump text PTR
	CALL	SBLKCH		;Is next char also tab or space?
	JRZ	SWHTC1		;Yes, scan over it
	DCX$	D		;DE-> last matched char
	CMP	A
	RET			;Now return with Z
;
SNOTCH:	INX$	H		;HL-> char not to match
	LDA	TERMCH+1	;Get wildcard
	CMP	M		;Another wildcard?
	BEQ	SNOTC1		;Yes, branch
	TST$	SRCNSW		;Search to ignore upper/lower case?
	LDAX	D		;Get text char
	CNZ	CONVUC		;Yes, convert lower to upper case
	CMP	M		;Does it match?
	JMPR	C%Z%NZ		;Invert Z code
;
SNOTC1:	CALL	SMATCH		;Recursively check for match
	JMPR	C%Z%NZ		;Invert Z code
;
SUCCH:	CALL	SLETCH		;Is it a letter?
	RNZ			;No ,return 'NZ'
	CPI	'Z'+1		;Is it upper case?
	JMP	SCTLC1		;Return 'Z' if UC
;
SLINTM:	SDED	TEMPW		;Save -> to begin of |L match
	XCHG			;HL-> char to match
	CALL	CRLFPR		;Is it CR-LF or LF-CR?
	XCHG			;DE = old DE+1
	RZ			;Yes, return 'Z' and DE+, TEMPW-> match begin
	CLR	TEMPW
	STA	TEMPW+1
	DCX$	D
	CALL	SLINCH		;Is it FF, LF or EOF
	RNZ			;No, return
	CPI	EOF		;Is it EOF?
	JRNZ	SLNCHG		;No, return 'Z'
;
;	First time EOF encountered at end of unterminated line when search
;	began before EOB?
;
	TST$	EOBFLG
	JRNZ	SLNRET
	INR	A
	STA	EOBFLG
;
	DCX$	D		;Unterminated last line? (empty buff has LF fence)
	CALL	SLINCH
	INX	D
	JRZ	SLNCHG
;
	PUSH	D		;Search start before EOB?
	PUSH	H
	CMPW	EDTPTR,TXTTOP
	POP	H
	POP	D
;
SLNCHG:	CALL	C%Z%NZ		;Change state of 'Z' flag
SLNRET:	LDAX	D		;Retrieve char
	RET
;
SLCCH:	CALL	SLETCH		;Is it a letter?
	RNZ			;No ,return 'NZ'
	CPI	'Z'+1		;Is it lower case?
	JC	RET%NZ		;No, return 'NZ'
	CMP	A
	RET			;'Z'
;
SMNYCH:	MVIB$	MANYFL,1	;Set flag
	SHLD	STEMP1		;Save -> search pattern
	SDED$	STEMP2		;Save -> text being searched
	DCX$	D		;Adjust for upcoming INX$ D
				;|M is allowed to match NULL
	CMP	A		;'Z'
	RET

