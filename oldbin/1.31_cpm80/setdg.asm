	title	'Patch VDSETCRT.COM for Emulog 200  12MAR82'

;	Generate hex file to use with DDT to patch the
;	VEDIT set up program.  Makes CRT number 23 be a
;	Data General 100 (Emulog 200)

null	macro
	;; set up a null parameter
	db	0,0,0,0,0,0
	endm

	org	118fh	;console output string

	db	0dh,0ah,'(23)  D.G. 100  <CRL>'

	org	1a92h	;start of CRT 24 table

;	table entries are in form:
;	count,ch1,ch2,ch3,ch4,delay
;	where count is number of bytes in string,
;	ch(x) are the characters,
;	and delay is number of milliseconds to
;	wait after this function
;	delay value follows last char sent ($+count+1)

addled:	db	1,10h,0,0,0,0		;cursor address lead in
addmid:	null		;characters between x & y
addend:	null		;cursor address lead out

;	address offset - where do cursor addresses start?

addoff:	db	1	;1=column,row  0=row,column
	db	0	;first offset
	db	0	;second offset

cls:	db	1,0ch,0,0,0,0		;clear screen
eos:	null	;erase end-of-screen
eol:	db	1,0bh,0,0,0,0		;erase end-of-line
ins:	null	;insert line
del:	null	;delete line
inmdb:	null	;begin insert mode
inmde:	null	;end insert mode
fscl:	db	1,0ah,0,0,0,0		;forward scroll
bscl:	null	;backward scroll
rvsv:	db	2,1eh,44h,0,0,0		;reverse video
nmlv:	db	2,1eh,45h,0,0,0		;normal video
ests:	null	;enable status line
dsts:	null	;disable status line

	end
