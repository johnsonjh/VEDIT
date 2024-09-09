;
;	VEDIT/VEDIT PLUS Constants
;
GTXDLM	=	80H		;End of string marker for GETEXC

	IF	VPLUS, [
MAXINT	=	65535		;Largest possible integer = 0FFFFh
	] [
MAXINT	=	32767		;Largest possible integer = 0FFFFh
	]
;
	IFNOT	P8086, [
ACTLEN	=	260		;Max active line length
	] [
ACTLEN	=	1026		;Max active line length
	]

INISPR	=	6144		;Spare text space for insert
CMDSPR	=	1		;Spare bytes for command overflow
CMDSIZ	=	80		;Command buffer increment size
TARGLN	=	80		;Search/replace buffer sizes
	IFNOT	P8086, [
PTRSIZ	=	1		;1 16-bit ptr suffices
	] [
PTRSIZ	=	2		;Need 20 bits to fully span available memory
	]
;
	IF	VPLUS, [
ISTKMX	=	25		;Max iteration/if-then-else stack levels
XSTKMX	=	25		;Max register exection stack levels
OSTKMX	=	3		;Max visual-operation stack levels
USRGMX	=	36		;Number of user registers: 0-9, A-Z
;
	IFNOT	P8086, [
MAINRG	=	USRGMX		;Main edit buffer
BFDSIZ	=	3		;Size of buffer-descriptor entry in BUFTBL
STAKSZ	=	5		;Size of ITRSTK, REGSTK, OPSTK:  but see POPUC2
MSTKMX	=	230		;Size of main stack area
				;PIICEON scroll line stored here too
	] [
DOSREG	=	USRGMX		;DOS output trapping buffer
MAINRG	=	DOSREG+1	;Main edit buffer
BFDSIZ	=	5		;Size of buffer-descriptor entry in BUFTBL
STAKSZ	=	9		;Size of ITRSTK, REGSTK, OPSTK:  but see POPUC2
MSTKMX	=	256
	]

REGMAX	=	MAINRG+1	;Total number of text registers/buffers
QRGMAX	=	26		;Max user-definable numerical registers
	]

	IFNOT	VPLUS, [
BFDSIZ	=	4		;Size of buffer-descriptor entry in BUFTBL
REGMAX	=	10		;Number of text registers
STAKSZ	=	5		;Size of ITRSTK, REGSTK, OPSTK:  but see POPUC2
MSTKMX	=	150		;Size of main stack area
				;PIICEON scroll line stored here too
XSTKMX	=	6		;Max register exection stack levels
				;One level needed to open FCBs
	]

	IF	MSDOS, [
PATHBG	=	33+4		;Start of pathname at end of FCB
DIRLEN	=	64+17		;d:\path64\file8.ex3,0
	]

PNTMAX	=	10		;Number of text markers
;
REGKEY	=	REGMAX		;Register # for KEYTBL
REGWIN	=	REGMAX+1	;Structure for windows
REGHED	=	REGMAX+2	;Register # for header
REGSHD	=	REGMAX+3	;Register # for sub-header
REGFOT	=	REGMAX+4	;Register # for footer
;
EOF	=	01AH
EOT	=	EOF		;End of tables for LOOKCH
EOS	=	EOF		;End of tables for LOOKCH
ESC	=	01BH
KFF	=	0FFH		;Char used to terminate keyboard strings
	IF	VPLUS, [
SRCEOS	=	EOF		;Search terminate character
	] [
SRCEOS	=	ESC		;Search terminate character
	]

VMCODE	=	'V'+080H	;Visual Macro running (inserted into macro)

	IFNOT	P8086, [
RETINS	=	0C9H		;8080 RET code
	] [
RETINS	=	0C3H		;8086 RET code
	]
;
; BDOS CALLS
;
RDCONS	=	1
CONSIO	=	6
GETVER	=	12
RESET	=	13
SETDSK	=	14
OPENF	=	15
CLOSEF	=	16
SRCHF	=	17
SRCHN	=	18
DELETE	=	19
READF	=	20
WRITEF	=	21
CREATE	=	22
RENAME	=	23
INTDSK	=	25
SETDMA	=	26
CPMUSR	=	32

	IF	P8086,[
	IF	MSDOS,[
GETVER	=	30H
CHGDIR	=	3BH
	]
	]
;
CTRLA	=	01
CTRLB	=	02
CTRLC	=	03
CTRLD	=	04
CTRLE	=	05
CTRLF	=	06
CTRLG	=	07
CTRLH	=	08
CTRLK	=	11
CTRLL	=	12
CTRLN	=	14
CTRLO	=	15
CTRLP	=	16
CTRLQ	=	17
CTRLR	=	18
CTRLS	=	19
CTRLT	=	20
CTRLU	=	21
CTRLV	=	22
CTRLW	=	23
CTRLX	=	24
CTRLY	=	25
CTRLZ	=	26
;
BS	=	08
TAB	=	09
LF	=	0AH
CR	=	0DH
SPACE	=	020H
DEL	=	127
COMMA	=	2CH
TICMRK	=	27H		;1's complement suffix for expressions
SEMCOL	=	3BH

