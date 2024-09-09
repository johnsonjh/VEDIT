;
; Keyboard dump routine
;
DMPKEY:	LXI	B,OTDVMSG	; Find out if printer, or console
	CALL	PRDCCH
	JZ	BEGIN3		; Rubout, assume they don't want a dump
	MOV	A,L
	STA	OUTFLG

	LHLD	CHRTBL		; HL-> first entry in character table
	XCHG
	LXI	B,ES1MSG	; Output escape characters first
	XRA	A
	STA	CURCOL
	CALL	PRTKEY		;;Print message, check for <CTRL-S>
	CALL	TABOVR
	LDAX	D		; Get the character
	INX	D
	ORA	A
	CNZ	OUTKEY		; Display it
	LXI	B,ES2MSG	; Output second escape character
	XRA	A
	STA	CURCOL
	CALL	PRTKEY		;;Print message, check for <CTRL-S>
	CALL	TABOVR
	LDAX	D
	INX	D
	ORA	A
	CNZ	OUTKEY

	LXI	B,ES3MSG	; Now, the noop characters
	XRA	A
	STA	CURCOL
	CALL	PRTKEY		;;Print message, check for <CTRL-S>
	CALL	TABOVR
	LDAX	D
	INX	D
	ORA	A
	CNZ	OUTKEY

	LXI	B,ES4MSG
	XRA	A
	STA	CURCOL
	CALL	PRTKEY		;;Print message, check for <CTRL-S>
	CALL	TABOVR
	LDAX	D
	INX	D
	ORA	A
	CNZ	OUTKEY
;
; Now, dump the function keys
;
	LXI	B,MESSG1		; Point to the messages
	LHLD	CNTADD			; Point to first entry - 2
	LXI	D,6
	DAD	D

DMP1:	INX	H			; Point to next entry
	INX	H
	XRA	A
	STA	CURCOL

	CALL	PRTKEY			; Output the function name
	JZ	DMPDNE
	CALL	TABOVR			; Tab over to correct column
	PUSH	H			; Save table pointer
	PUSH	B			; Save message pointer
	CALL	DMP2			; Output the key(s)
	POP	B			; Restore message pointer
	POP	H			; Restore table pointer

	JMP	DMP1			; Output next function

DMPDNE:	JMP	BEGIN3
;
; Dump key(s) for function whos address is (HL)
;
DMP2:	XRA	A			; Flag first definition
	STA	DMPFLG
	MOV	C,M			; Get address of the function
	INX	H
	MOV	B,M

	LHLD	CTLDEC			; Scan control character table
	MVI	E,0			; Starting character
	MVI	D,33			;;Number of entries ++ DEL
	CALL	DMP4

	LHLD	ESCDC1			; Scan ESC decode table #1
	MVI	E,80H			; Starting character
	MVI	D,128			; Number of entries
	CALL	DMP4

 	LHLD	ESCDC2			; Scan ESC decode table #2
	MVI	E,80H			; Starting character
	MVI	D,128			; Number of entries
	CALL	DMP4

	LDA	DMPFLG			; Any keys defined?
	ORA	A
	RNZ				; Yes, all done
	LXI	B,NONEMS		; Nope, output [NONE]
	JMP	PRTMS0
;
; Scan table at HL for BC
;
DMP4:	MOV	A,M			; Is it this entry?
	INX	H
	CMP	C
	JNZ	DMP5			; Nope ...
	MOV	A,M
	CMP	B
	JNZ	DMP5			; Nope ...

	LDA	DMPFLG			; Is it first key?
	ORA	A
	MVI	A,255
	STA	DMPFLG
	JZ	DMP4A			; Nope, some space first
	PUSH	H
	PUSH	D
	PUSH	B
	LXI	B,SEPMSG
	CALL	PRTMS0
	POP	B
	POP	D
	POP	H
DMP4A:	MOV	A,E			; Now, dump to key
	CPI	020H			;;Is it offset to DEL?
	JNZ	DMP4B			;;No, branch
	MVI	A,7FH			;;Yes, change to DEL value
DMP4B:	CALL	OUTKEY

DMP5:	INX	H
	INR	E
	DCR	D
	JNZ	DMP4
	RET

DMPFLG:	DS	1

