;
;	MAC1 - 8086 translation macro layer, part 1 of 2.
;
;	RECONSTRUCTED 2026 to restore the CP/M-86 / MS-DOS (8086) build.
;
;	Historically these two files (MAC1.ASM, MAC2.ASM) were inserted in
;	place of Z80MACRO when assembling with P8086 defined, then the
;	assembled mnemonic stream was passed through CompuView's Z80->8086
;	source translator (see zilint/z80-8086.vdm) to produce 8086 source.
;
;	They are the 8086 peer of Z80MACRO/808MACRO: the mid-level mnemonic
;	layer beneath the high-level translatability macros in P86MACRO
;	(which is always inserted, before this file).  In the P8086 path the
;	assembler stays in its default (Z80) mode, so native Z80 mnemonics
;	(LBCD, LDED, SBCD, SDED, DSBC, JRcc, etc.) survive into the output.
;	These are remapped to into 8086 by the translator.  The expansions
;	mirror Z80MACRO and are able to be auto-translated cleanly per the
;	documented register map (i.e. A->AL, BC->CX, DE->DX, HL->BX, M->[BX]).
;
;	Last Addition:  APENDC, APENDS 9/30/86 by Thomas C. Burt (??)
;
;	TDL Assembler Directives.		(8086 path: Z80 mode)
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
