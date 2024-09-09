	title	'Patch VDSETCRT.COM for TEC 511 - 31MAY81'

;	Generate hex file to use with DDT to patch the
;	VEDIT set up program.  Makes CRT number 24 be a
;	TEC 511.

null	macro
	;; set up a null parameter
	db	0,0,0,0,0,0
	endm

	org	11a6h	;console output string

	db	0dh,0ah,'(24)  TEC 511  <CRL> '

	org	1af5h	;start of CRT 24 table

;	table entries are in form:
;	count,ch1,ch2,ch3,ch4,delay
;	where count is number of bytes in string,
;	ch(x) are the characters,
;	and delay is number of milliseconds to
;	wait after this function
;	delay value follows last char sent ($+count+1)

addled:	db	2,1bh,3dh,0,0,0		;cursor address lead in
addmid:	null		;characters between x & y
addend:	null		;cursor address lead out

;	address offset - where do cursor addresses start?

addoff:	db	0	;1=column,row  0=row,column
	db	20h	;first offset
	db	20h	;second offset

cls:	db	1,18h,0afh,0,0,0		;clear screen
eos:	null	;erase end-of-screen
eol:	null	;erase end-of-line
ins:	null	;insert line
del:	null	;delete line
inmdb:	null	;begin insert mode
inmde:	null	;end insert mode
fscl:	db	1,0ah,0,0,0,0		;forward scroll
bscl:	null	;backward scroll
rvsv:	db	1,1dh,0,0,0,0		;reverse video
nmlv:	db	1,1ch,0,0,0,0		;normal video
ests:	null	;enable status line
dsts:	null	;disable status line

	end
