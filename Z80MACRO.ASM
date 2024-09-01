;
; Last Addition:  APENDC, APENDS 9/30/86 by Thomas C. Burt
;
;	TDL Assembler Directives.
;
	.PABS
	.PHEX
	.LADDR
	.SALL
	.OPSYN	.LOC,ORG
	.OPSYN	.BLKB,DS
	.OPSYN	.BLKW,DSW
	.OPSYN	.ASCII,DC
	.OPSYN	.BYTE,DB
	.OPSYN	.WORD,DW
	.OPSYN	.INTERN,PUBLIC
;
;	B - Byte
;	W - Word (16 bit)
;	M - (HL) set HL (2 exceptions)
;	$ - No registers set.
;
;	Macro Definitions.
;
	.DEFINE LBCD$[X] =
	[LBCD	X]
;
	.DEFINE LDED$[X] =
	[LDED	X]
;
	.DEFINE SBCD$[X] =
	[SBCD	X]
;
	.DEFINE SDED$[X] =
	[SDED	X]
;
;	INX$ - Flags not preserved.
;
	.DEFINE INX$[X] =
	[INX	X]
;
	.DEFINE DCX$[X] =
	[DCX	X]
;
;	PUSHA - Save A.
;
	.DEFINE PUSHA =
	[PUSH	PSW]
;
	.DEFINE	POPA =
	[POP	PSW]
;
;	PUSHF - Save Flags.
;
	.DEFINE PUSHF =
	[PUSH	PSW]
;
	.DEFINE	POPF =
	[POP	PSW]
;
	.DEFINE	PUSH24 =
	[PUSH	H
	PUSH	B]
;
	.DEFINE	POP24 =
	[POP	H
	MOV	C,L
	POP	H]
;
;	CLR - Set A.
;
	.DEFINE	CLR[X] =
	[XRA	A
	STA	X]
;
	.DEFINE	CLR$[X] =
	[XRA	A
	STA	X]
;
;	MVIB - Set A.
;
	.DEFINE	MVIB[X,Y] =
	[MVI	A,Y
	STA	X]
;
	.DEFINE	MVIB$[X,Y] =
	[MVI	A,Y
	STA	X]
;
;	MVIM - HL set.
;
	.DEFINE	MVIM[X,Y] =
	[LXI	H,X
	MVI	M,Y]
;
;	MVIW - Set HL.
;
	.DEFINE	MVIW[X,Y] =
	[LXI	H,Y
	SHLD	X]
;
;	MVIW$ - HL not set.
;
	.DEFINE	MVIW$[X,Y] =
	[LXI	H,Y
	SHLD	X]
;
;	MOVB - Set A.
;
	.DEFINE	MOVB[X,Y] =
	[LDA	Y
	STA	X]
;
;	MOVW - HL set.
;
	.DEFINE	MOVW[X,Y] =
	[LHLD	Y
	SHLD	X]
;
;	LDAM - HL set.
;
	.DEFINE	LDAM[X] =
	[LXI	H,X
	MOV	A,M]
;
;	CPIB - A set.
;
	.DEFINE	CPIB[X,Y] =
	[LDA	X
	CPI	Y]
;
;	CPIB$ - A not set.
;
	.DEFINE	CPIB$[X,Y] =
	[LDA	X
	CPI	Y]
;
;	CPIM - A and HL set.
;
	.DEFINE	CPIM[X,Y] =
	[LXI	H,X
	MOV	A,M
	CPI	Y]
;
;	CPI%M - A set, HL set.
;
	.DEFINE CPI%M[X,Y] =
	[LHLD	X
	MOV	A,M
	CPI	Y]
;
;	CPIW - HL and DE not set.
;
	.DEFINE	CPIW[X,Y] =
	[LHLD	X
	LXI	D,Y
	CALL	CMHLDE]
;
;	CMPM - HL set.
;
	.DEFINE	CMPM[X] =
	[LXI	H,X
	CMP	M]
;
;	CMPMB - Set A and HL.
;
	.DEFINE	CMPMB[X,Y] =
	[LXI	H,Y
	LDA	X
	CMP	M]
;
;	CMPW - Set HL and DE.
;
	.DEFINE	CMPW[X,Y] =
	[LDED	Y
	LHLD	X
	CALL	CMHLDE]
;
;	TST - A set.
;
	.DEFINE	TST[X] =
	[LDA	X
	ORA	A]
;
;	TST$ - A not set.
;
	.DEFINE	TST$[X] =
	[LDA	X
	ORA	A]
;
;	TSTM - HL set.
;
	.DEFINE	TSTM[X] =
	[LXI	H,X
	MOV	A,M
	ORA	A]
;
;	TSTW - HL set!
;
	.DEFINE	TSTW[X] =
	[LHLD	X
	MOV	A,L
	ORA	H]
;
;	TSTW$ - HL not set.
;
	.DEFINE	TSTW$[X] =
	[LHLD	X
	MOV	A,L
	ORA	H]
;
;	INCB$ - A not set.
;
	.DEFINE	INCB[X] =
	[LDA	X
	INR	A
	STA	X]
;
;	INCM - HL set.
;
	.DEFINE	INCM[X] =
	[LXI	H,X
	INR	M]
;
	.DEFINE	DCX%BC =
	[DCX	B
	MOV	A,B
	ORA	C]
;
;	LOOP - 8086 Equiv.
;
	.DEFINE	LOOP[X] =
	[DCX	B
	MOV	A,B
	ORA	C
	JRNZ	X]
;
;	JCXZ - 8086 Equiv.
;
	.DEFINE	JCXZ[X] =
	[MOV	A,B
	ORA	C
	JRZ	X]
;
;	INCW$ - HL not set.
;
	.DEFINE	INCW$[X] =
	[LHLD	X
	INX	H
	SHLD	X]
;
;	ANIB - A set.
;
	.DEFINE ANIB[X,Y] =
	[LDA	X
	ANI	Y]
;
;	ANIB$ - A not set.
;
	.DEFINE ANIB$[X,Y] =
	[LDA	X
	ANI	Y]
;
;	ANIM - A and HL set.
;
	.DEFINE	ANIM[X,Y] =
	[LXI	H,X
	MOV	A,M
	ANI	Y]
;
;	ADDB - HL set. (see INDENT)
;
	.DEFINE	ADDB[X,Y] =
	[LXI	H,Y
	LDA	X
	ADD	M]
;
;	SUBB - HL set.
;
	.DEFINE	SUBB[X,Y] =
	[LXI	H,Y
	LDA	X
	SUB	M]
;
	.DEFINE	DSUB[X] =
	[XRA	A
	DSBC	X]
;
;	SUI%BC - BC = X - Y.
;		HL and DE set.
;
	.DEFINE SUI%BC[X,Y] =
	[LHLD	X
	LXI	D,Y
	CALL	SBHLDE]
;
;	SUB%BC - BC = X - Y.
;		HL and DE set.
;
	.DEFINE SUB%BC[X,Y] =
	[LDED	Y
	LHLD	X
	CALL	SBHLDE]
;
	.DEFINE	BGE[X] =
	[JRNC	X]
;
	.DEFINE	BLT[X] =
	[JRC	X]
;
	.DEFINE	BEQ[X] =
	[JRZ	X]
;
	.DEFINE	BNE[X] =
	[JRNZ	X]
;
	.DEFINE	JRP[X] =
	[JP	X]
;
	.DEFINE	JRM[X] =
	[JM	X]
;
;	APPEND -
;
	.DEFINE	APENDC[CH] =
	[MVI	M,CH
	INX	H]
;
	.DEFINE	APENDS[S,EOS] =
	[LXI	B,EOS
	LXI	D,S
	XCHG
	CALL	SAPEND
	XCHG]
