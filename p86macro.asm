;
;	Macros for 8086 translatability
;
	.DEFINE DSEG	=	[]
	.DEFINE	CSEG	=	[]

	.DEFINE	SETORG =
	[ORG	BASE + 100H]

	.DEFINE	BPROC[NAME] =
	[.ENTRY	NAME
	]

	.DEFINE	EPROC[NAME] =
	[
	]

	.DEFINE	REPT[N,X] = [
	.IFDIF	"'N" "0", [
	X
	REPT	\N-1,(X)
	]
	]

	.DEFINE	PUSHXF = [
	PUSH	PSW
	]

	.DEFINE	POPXF  = [
	POP	PSW
	]
;
;	For instructions that are out of range for 8086.
;
	.DEFINE	BGE8[X] = [BGE	X]
	.DEFINE	BLT8[X] = [BLT	X]
	.DEFINE	BEQ8[X] = [BEQ	X]
	.DEFINE	BNE8[X] = [BNE	X]
	.DEFINE	JRP8[X] = [JRP	X]
	.DEFINE	JRM8[X] = [JRM	X]
	.DEFINE JRC8[X] = [JRC  X]
	.DEFINE JRZ8[X] = [JRZ  X]
	.DEFINE JRNZ8[X] = [JRNZ X]
	.DEFINE JRNC8[X] = [JRNC X]
	.DEFINE JMPR8[X] = [JMPR X]
;
;	For accessing 8 bits of 16-bit defined data
;
;	LD8
;
	.DEFINE	LD8[S]	= [
	LDA	S
	]
;
;	ST8
;
	.DEFINE	ST8[D]	= [
	STA	D
	]

;
;	For accessing 2 8-bit bytes at once
;
;
;	LD16[R,S]
;
	.DEFINE	LD16[R,S] = [
	.IFIDN	"R"  "B",  [
	LBCD	S
	]
	.IFIDN	"R"  "D",  [
	LDED	S
	]
	.IFIDN	"R"  "H",  [
	LHLD	S
	]
	]

	.DEFINE	LD16$[R,S] = [
	LD16	R,S
	]
;
;	ST16[Dest,Reg]
;
	.DEFINE	ST16[DS,RG] = [
	.IFIDN	"RG" "B",  [
	SBCD	DS
	]
	.IFIDN	"RG" "D",  [
	SDED	DS
	]
	.IFIDN	"RG" "H",  [
	SHLD	DS
	]
	]

	.DEFINE	ST16$[DS,RG] = [
	ST16	DS,RG
	]

;
;	For accessing 32-bit values: reg & segr.
;

;	STDS[w,r] - Store Reg's r & DS into 32-bit ptr w.

	.DEFINE	STDS[MW,RG] = [
	ST16	MW,RG
	]


;	STDS$[w,r] - Store Reg's r & DS into 32-bit ptr w.

	.DEFINE	STDS$[MW,RG] = [
	ST16	MW,RG
	]

;
;	LES - Load ES & 16-bit register
;
	.DEFINE LES[R,S] = [
	LD16	R,S
	]
	
;	STES[w,r] - Store Reg's r & ES into 32-bit ptr w.

	.DEFINE	STES[MW,RG] = [
	ST16	MW,RG
	]

;	STES$[w,r] - Store Reg's r & ES into 32-bit ptr w.

	.DEFINE	STES$[MW,RG] = [
	ST16	MW,RG
	]

;
;	For accessing data that is in 8086's extra segment.
;

;
;	MOV%ES - MOV A,ES:M  or  MOV ES:M,A
;
	.DEFINE MOV%ES[D,S] = [
	MOV%CS	D,S
	]

;
;	MVI%ES - MVI ES:M,CONST
;
	.DEFINE MVI%ES[M,C] = [
	MVI	M,C
	]

;
;	MVHL%ES - MVINHL
;
	.DEFINE MVHL%ES = [
	CALL	MVINHL
	]

;
;	MVDE%ES - Move [ES:HL] into DE.  HL trashed vis a vis 8086.
;
	.DEFINE MVDE%ES = [
	MOV	E,M
	INX$	H
	MOV	D,M
	]

;
;	LDA%ES
;
	.DEFINE LDA%ES[S] = [
	LDA	S
	]
;
;	LDX%ES - LDAX from ES segment.
;
	.DEFINE LDX%ES[RX] = [
	LDAX	RX
	]

;
;	LD%ES - LxxD from ES segment.
;
	.DEFINE LD%ES[R,S] = [
	LD16	R,S
	]

;
;	CPI%ES - CPI%M when M is in the extra segment.
;
	.DEFINE CPI%ES[PTR,VAL] = [
	CPI%M	PTR,VAL
	]

;
;	CMP%ES[M] - CMP	M when M is in the extra segment.
;
	.DEFINE CMP%ES[M] = [
	CMP	M
	]

;
;	JMPES - Before invoking routine, switch to ES: segment.  Restore
;		on return (8086)
;
	.DEFINE JMPES[dest] = [
	JMP	DEST
	]

;
;	CALLES - Before invoking routine, switch to ES: segment.  Restore
;		 on return.
;
	.DEFINE CALLES[dest] = [
	CALL	DEST
	]

;
;	MVXO - Copy out to destination segment (ES:D).
;
	.DEFINE MVXO = [
	CALL	RTLDIR
	]

;
;	MVXI - Copy in from source segment (ES:H).
;
	.DEFINE MVXI = [
	CALL	RTLDIR
	]


;
;	For accessing data that is in 8086's code segment.
;

;
;	MOV%CS - MOV A,CS:M  or  MOV CS:M,A
;
	.DEFINE MOV%CS[D,S] = [
	.IFIDN	"D" "M", [
	MOV	M,S
	]
	.IFIDN  "S" "M", [
	MOV	D,M
	]
	]

;
;	MVI%CS - MVI CS:M,CONST
;
	.DEFINE MVI%CS[M,C] = [
	MVI	M,C
	]

;
;	MVIM%CS - MVIM CODEB,CONST (HL set)
;
	.DEFINE MVIM%CS[CB,C] = [
	MVIM	CB,C
	]
;
;	ST16%CS - Store 16 bit register into code section.
;
	.DEFINE ST16%CS[MW,RG] = [
	ST16	MW,RG
	]

;
;	CALLC - Call with DS<-CS
;	(Warning:  all data accesses are from/to code segment)
;
	.DEFINE CALLC[DEST] = [
	CALL	DEST
	]

;
;	For initialization only.
;

;
;	XMOVC - Load register indirect from CSEG (MSDOS) or DSEG (CP/M-86).
;		Used by SETPRM.
;
	.DEFINE XMOVC[DR,M] = [
	MOV	DR,M
	]

;
;	XMOVSB - Move byte-string from CSEG (MSDOS) or DSEG (CP/M-86) to DSEG.
;		 Used by SETPRM.
;
	.DEFINE	XMOVSB	= [
	CALL	MOVE
	]

;
;	Setup temporary stack (and segment) registers.
;
	.DEFINE XTSTAK = [

	IF	VPLUS, [
	LHLD	WSABEG
	] [
	LHLD	TXTBAS
	]

	LXI	B,0500H
	DAD	B
	SPHL
	]


;	Setup operation stack and segment registers

	.DEFINE XOSTAK = [
	LXI	SP,STACK
	]

