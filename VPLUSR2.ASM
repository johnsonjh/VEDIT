	.PAGE
	.TITLE	'VPLUSR2'
;************************************************
;*						*
;*	Multiple Edit Buffer Support		*
;*						*
;************************************************
;
; Copyright (C) 1986 by CompuView Products, Inc.
;			1955 Pauline Blvd.
;			Ann Arbor, MI 48103
;
;	Written by: Thomas C. Burt
;
;	Last Change: Tom - Nov. 07, 1985 - Lost FIXPUT patch of 9/28 reinstalled.
;			   Mar. 27 - Broken out from R1, made consistent with
;				     Ted's MALLOC & FREE and with simplified
;				     memory arrangement.
;		     TnT - Apr. 08 - Changes for new BUFTBL organization
;		     Tom - May  07 - PNTREG-CONVBF-SHUFFL bug:  call RASSOC in CONVBF
;			 - May  08 - OPENDW() +1 bug, optimization
;		     Ted - May  08 - STDEIN for STDEIN/STINDE
;		     Tom - Aug. 19 - COPYI new, MAKEBF handle tables SW,PRM,TAB
;				     now kept in edit buffer headers.
;			   Nov. 26 - STROFF(0) = 0
;
; NEEDBC - BC = # free bytes needed.
;	   Exit:  'C' if not available.
;
;	   All registers trashed.
;
				;{BUFFUP}
NEEDBC:	LXI	H,0
	DSUB	B		;HL = -bytes needed
	XCHG			;DE = -bytes needed
;
; NEEDTX - Ensure at least -DE bytes free.
;	   'C' if no room.

				;{ACMD,ICMD,MAKEBF,FREESP,NEEDBC^}
NEEDTX:	LHLD	MIDBAS		;HL -> beginning of middle buffers
	DAD	D		;HL -> lowest reserved byte
	CMC			;Negative underflow?
	RC			;Yes, return 'C'
	LDED	TXTRWF		;+DE -> end of text
	INX$	D		;DE -> first free byte
	JMP	CMHLDE		;Returns 'C' if no space
;
; MAKEBF - Create buffer to be used for editing purposes.
;	   Enter: EDTNUM = current edit buffer
;		  REGNUM = desired edit buffer
;		  REGPTR-> entry in BUFTBL for REGNUM
;
				;{EECMD}
MAKEBF:	CMPMB	REGNUM,EDTNUM	;Current edit buffer = one to be created?
	RZ			;Yes, return

				;{BEGIN}
MAKBF0:	LXI	D,-(EHDLEN+2)	;Header length, 1 byte text, EOF marker

				;{CONVBF}
MAKBF1:	CALL	NEEDTX		;Room?
	JC	BREAK		;No, give message
;
	CALL	SAVEBF		;Yes, save current editing environment
	MOVB	EDTNUM,REGNUM	;Set edit buffer number
	LHLD	REGPTR		;BX-> current edit buffer descriptor in BUFTBL
	MVI	M,2		;Set type byte for edit buffer
	INX$	H		;HL -> buffer pointer in descriptor entry
	LDED	EDTRWF		;+DE -> end of just saved edit buffer
	INX$	D		;DE -> start of free space
	SDED	EDTBAS		;+Set -> start of new edit buffer header
	CALL	STDEIN		;Put EDTBAS into BUFTBL
	XCHG			;HL = EDTBAS
	LXI	D,EHDLEN	;DE = length of edit buffer header
	CALL	STDEIN		;Initialize 1st value in the header
				;Others will be set on next EEr command
	CALL	INITXB		;Initialize rest of current edit environment
;
; COPYI - Copy INSTALLed tables SWTBL, PRMTBL, TABPOS into work tables.
;
				;{MAKEBF^,INIT0}
COPYI:	LXI	H,TINIBG	;HL-> start of installed tables (SW,PRM,TAB)
	LXI	D,INSTBG	;DX-> working tables
	LXI	B,TININM	;C = # bytes (PASM/PLINK require 16 bits)
	JMP	RTLDIR
;
; CONVBF - Convert text register into edit buffer.
;	   BC = # bytes in the text register.
;
CONVBF:	INX$	B		;BC now includes EOF marker at register end
	SBCD	NR		;Set for SHUFFL
	LXI	D,-(EHDLEN+2-REGHL1)
	CALL	MAKBF1		;Create empty edit buffer. (BREAK if no room)
	CALL	RASSOC		;Initialize TRGFLR
	CALL	SHUFFL		;Shuffle the T-Reg down into Ed-buf space
	XCHG			;HL-> past EOF marker
	DCX$	H		;HL-> EOF marker
	SHLD	TXTCEL		;Set upper edit buffer pointers
	SHLD	TXTRWF
	RET
;
; DELBUF - Remove current edit buffer from BUFTBL, BUT NOT MAINRG!  [4/8/86]
;	   The buffer must have been closed previously.
;
				;{EXTCHK}
DELBUF:	CPIB$	EDTNUM,MAINRG	;Main text buffer?
	RZ			;Yes, do nothing!

	STA	REGNUM
	CALL	PNTREG
	LHLD	REGPTR		;Zero type byte => null register
	MVI	M,0

	IF	DEBUG, [
	INX$	H
	LXI	D,0
	CALL	STDEIN		;Zero out base pointer also
	]

	MVIB$	EDTNUM,0FFH	;Set impossible buffer number
	LHLD	EDTBAS		;Reset roof ptr for free memory checking
	DCX$	H
	SHLD	EDTRWF
	RET
;
; FRSTBF - Return HL-> first buffer descriptor in BUFTBL.
;	   'Z' when first entry in BUFTBL is also the last.
;
				;{EXTCHK}
	BPROC	FRSTBF
FRSTBF:	LXI	H,BUFTBL	;HL -> first buffer-descriptor
	LXI	D,BUFTBL+BFDSIZ*MAINRG
	MVI	A,2		;Edit buffer type
..1:	CMP	M		;Found edit buffer yet?
	JZ	CMHLDE		;Yes, set 'Z' if main edit buffer
	INX$	H		;No, advance to previous descriptor
	INX$	H
	INX$	H
	JMPR	..1
	EPROC	FRSTBF
;
; OPENBF - Make REGNUM's buffer the current edit buffer. (11/8/84)
;
OPENBF:	CMPMB	REGNUM,EDTNUM	;Already the current edit buffer?
	RZ			;Yes, do nothing!
	CALL	SAVEBF		;Save current environment
	CMPW	EDTRWF,TRGRWF	;Is new buffer above current buffer?
	BEQ	LOADBF		;Happens when current buffer was just below
				;a buffer removed by DELBUF
	BLT	OPENDW		;Yes, must lower buffers
;
; OPENUP - Raise current edit buffer & any intervening buffers to make
;	   space for lower buffer to become edit buffer.
;
				;{OPENBF^}
OPENUP:	INX$	H		;HL-> 1st byte to be freed
	PUSH	H
	INX$	D		;DE -> base of lowest edit buffer to be raised
	LHLD	MIDBAS		;For computing # bytes to be freed
	SDED	MIDBAS		;Now edit buffer(s) part of text register space
	POP	D		;DE-> 1st free byte
	CALL	SBHLDE		;BC = # bytes to free
	CALL	FREEDE		;Move the buffers up, adjust pointers
	JMP	LOADBF		;Set current values from new edit header
;
; OPENDW - Lower buffers from middle buffers down to EDTRWF.  [5/8/86]
;	   Make REGNUM the current edit buffer.
;
OPENDW:	LHLD	TRGRWF		;HL -> roof of new edit buffer
	INX$	H		;Want EOF lowered too
	LXI	B,-1		;Specify as much as possible
;6	MOV	AX,DS
	CALL	MALLSG		;Lower the upper buffers, adjust pointers
	SDED	MIDBAS		;MIDBAS -> past end of allocated memory
;	JMPR	LOADBF		;Set current values from new edit header
;
; LOADBF - Set edit parameters from header of new edit buffer.
;
				;{OPENUP,OPENDW}
LOADBF:	LHLD	REGPTR		;Buffer descriptor pointer
	MOVB	EDTNUM,REGNUM	;Set current edit buffer number
	INX$	H		;HL -> base pointer in descriptor
	CALL	MVINHL		;HL -> buffer base
	SHLD	EDTBAS		;Establish base pointer
;
	MOV	B,H		;HL = swap source
	MOV	C,L		;BC = value to be added to header offsets
	CALL	NEGBC		;BC = -base.  Subtracting -base => adding it
	LXI	D,PTRBGN	;DE -> swap destination in main storage
;	JMP	SWAPBF		;Set buffer pointers
;
; SWAPBF - Transfer contents of edit buffer header to/from storage area.
;	    BC = buffer base for swap into header, = -base for other way.
;	    DE -> buffer base for swap into header.
;	    HL -> storage area for swap into header.
;	    DE,HL are exchanged for swapping in opposite direction.
;
;	    Presumes pointers needing offsets subtracted out are
;	    immediately followed by simple swap variables.
;
				;{LOADBF^,SAVEBF}
SWAPBF:	LDA	EDTNUM		;Get current buffer number?
	INR	A		;Does one exist?
	RZ			;No, return
;
	MVI	A,PTRNUM	;A = # pointer offsets to filter < 257
	CALL	STROFF		;Subtract out/add in base in BC to value
				;pointed to by HL and store where DE points
				;for REG A times.  HL,DE -> past their blocks
				;(Add by subtracting negative number in BC)
				;DE & HL point to simple swap blocks now
	LXI	B,SWPNUM	;BC = # variables in swap section
	JMP	RTLDIR		;Move them into main storage
;
; SAVEBF - Save current edit environment in EDTNUM's header.
;
				;{MAKEBF,OPENBF}
SAVEBF:	LDA	EDTNUM		;Does buffer exist?
	INR	A		;0FFH => not
	RZ			;No, return
;
	LBCD$	EDTBAS		;BC = base for calculating offsets
	MOV	D,B
	MOV	E,C		;DE -> edit buffer header
	LXI	H,PTRBGN	;HL -> source in current environment
	JMPR	SWAPBF
;
; SAVERG - BEWARE: saves register pointers TRGBS2 thru TRGCEL into
;	    buffer header pointed to by TRGBAS if there is room.
;	    Since only TRGBAS itself is updated by buffering routines
;	    this can be dangerous.  Must not be called except when there
;	    has been a preceeding call to PNTREG and no buffering has
;	    caused the pertinent buffer to be moved since that call.
;
;	    BC, DE, HL saved.
				;{RMCMD}
SAVERG:	PUSH	H
	LHLD	REGPTR		;HL-> register descriptor
	MOV	A,M		;Get type
	POP	H
	DCR	A		;Text register?
	STC			;Needed?
	RZ			;Yes, no room in header, return
;
	CALL	SV%BDH		;Yes, save registers for caller
	MVI	A,REGHL2/2	;# pointers = half # bytes
	LDED$	TRGBAS		;DE -> destination header block
	MOV	B,D
	MOV	C,E		;BC = base address to be subtracted out
	LXI	H,TRGBS2	;HL -> source pointer block
	JMP	STROFF		;Put relocatable pointer values into header
;
; SHUFFLE - Move current text register down into current edit buffer
;	    while shoving the lower registers and cmd buffer up.   [4/8/86]
;
;	    Enter:  (NR) = # bytes in text register (including EOF marker).
;
;		    TXTFLR, TRGBAS, TRGFLR defined.
;
;	    Exit:   Text moved, cmd & register pointers adjusted.
;		    DE-> past EOF marker in cmd buf
;
;   Done in installments to keep from corrupting the block contents.
;
	BPROC	SHUFFL
SHUFFL:	LHLD	MIDBAS		;HL -> free space ceiling
	LDED	TXTFLR		;DE-> free space floor (empty text buffer)
	DSUB	D		;HL = # empty bytes for shuffling into
	SHLD	NF		;# bytes available to be filled
;
;	Setup variables needed for first pass.
;
	MOVW	FREEPT,TRGBAS	;Pointer for calling FREE
	MVIW$	TEMPW,REGHL1	;Additional header bytes to be FREEd
	LHLD	TRGFLR		;Source for MOVE
	LDED	TXTFLR		;Destination
;
;	While bytes remain, do the VEDIT shuffle.
;
..1:	PUSH	H
	PUSH	D
;
;	Determine # bytes to shuffle for this pass.
;
	LHLD	NR		;HL = # text bytes remaining
	LDED	NF		;DE = # bytes free to be moved into
	CALL	MNHLDE		;HL = # bytes to move this time
	MOV	B,H
	MOV	C,L		;BC = # to move
;
;	Update # bytes remaining.
;
	JRC	..2		;Branch if all remaining bytes can be moved
	XCHG			;HL = NR
..2:	DSUB	B		;HL = # bytes that will be left after move
	SHLD	NR		;Update # bytes remaining
;
;	Return if no bytes remaining to be moved.
;
	MOV	A,B
	ORA	C		;Any?
	POP	D		;DE-> destination
	POP	H		;HL-> source
	RZ			;Return if not
;
;	Move text register bytes into edit buffer.
;
	PUSH	B		;Save #
	CALL	MOVEBC
;
	POP	B		;# moved
	PUSH	H		;Save -> next source byte
	PUSH	D		;Save -> next destination byte
;
;	Determine # bytes to be freed.
;
	LHLD	TEMPW		;HL = hdr len first time, 0 thereafter
	DAD	B		;# bytes to be freed up
	MOV	B,H
	MOV	C,L		;BC = # bytes to be freed
	MVIW$	TEMPW,0		;Only count header bytes once
;
;	Free the vacated bytes, adjust pointers.
;
	LHLD	FREEPT		;HL-> vacated bytes
	CALL	FREE		;Free the vacated bytes, adjust pointers
;
;	Adjust # free bytes, now that header has been squeezed out.
;	On second thought, let's not.  Minimize code instead.
;
;	LHLD	NF		;# bytes free
;	LDED	TEMPW		;# extra bytes from squeezed header (else 0)
;	DAD	D
;	SHLD	NF		;total free bytes
;
	POP	D		;Destination
	POP	H		;Source
	SHLD	FREEPT		;MOVE & FREE pointers same from now on
	JMPR	..1
	EPROC	SHUFFL
;
; STROFF - Store the 16 bit offset of (HL)-BC into (DE), DE++, HL++.
;;	   But STROFF(0) = 0.
;	   Do this Reg A times.  DE,HL -> past last table values processed.
;
STROF1:	MVI	A,1
	BPROC	STROFF
STROFF:	PUSHA
	PUSH	D		;Save destination pointer
	CALL	MVINDE		;Get pointer value, HL++
	XTHL			;HL -> destination, TOS-> next source
	XCHG			;HL = current pointer value
	MOV	A,H
	ORA	L		;;Is PTR set? (1-END and markers)
	JRZ	..1		;;No, don't adjust it
	DSUB	B		;HL = offset
..1:	XCHG			;DE = offset, HL -> destination
	CALL	STDEIN		;Store the offset, HL++
	XCHG			;DE -> next destination
	POP	H		;HL -> next source
	POPA			;Get count
	DCR	A		;More?
	BNE	STROFF		;Yes, loop
	RET			;No, return DE & HL
	EPROC	STROFF
