	.PAGE
	.TITLE	'VPLUSR1'
;********************************************************
;*							*
;*	Text Register & Edit Buffer Support		*
;*							*
;********************************************************
;
; Copyright (C) 1986 by CompuView Products, Inc.
;			1955 Pauline Blvd.
;			Ann Arbor, MI 48103
;
;	Written by: Theodore J. Green
;
;	Last Change: Tom - July 23, 1985 - MAKCPY:  RIr$ bug fixed
;			 - Nov. 07, 1985 - Lost FIXPUT patch of 9/28 reinstalled
;			 - Feb. 23 - 2.32a bug fixes:
;				     TEXTR1 check for underflow (TED found)
;				     GETTXT:  don't save ptr(r), recalculate
;			 - Mar. 16 - 2.23b:  GETTXT don't allow Gr where 'r' is edit buffer
;		         - Mar. 27 - 2.33, use Ted's MALLOC & FREE
;				     Simplified memory layout:  EB, free space, EB, CB, TR, MAXMEM
;				     GETTXT can once again save register start
;			 - Apr. 07 - New text register arrangement & handlers
;			 - Apr. 14 - P2N, new routine, compute reg num from buf ptr
;		     Ted - Apr. 17 - Debug, new RGREMV() - remove text from T-Reg
;			   Apr. 20 - Change CONVRG for '$', Misc in GETRNM
;		     Tom - Apr. 21 - RXOPEN (JC not JNC) bug
;			 - Apr. 25 - GETCR2 renamed SETRN
;				   - GETQRN
;		     Ted - May  13 - GETRNM() changes
;
;		     Tom - May  27 - REGLEN() bug (multiple edit buffers)
;			   Sept 05 - GETTXT bug:  return DE=EDTPTR for nonexistent register
;			   Oct. 15 - PNTTRG() new
;			   Oct. 16 - RAMRGP new local pointer.  RAMDWN(), RAMUP()
;		     Ted - Oct. 23 - GETRNM() - CTRL-C check
;			 - Nov. 25 - EDTNAM()
;			 - Dec. 02 - RGOPEN() Key tbl kludge
;		     Tom - Dec. 02 - KBUFFR()
;
;***************************************************************************
;
; Memory layout:
;
;	WSABEG -> start of lower buffers:
;		  main edit buffer
;		  other edit registers, if any.
;	EDTBAS -> start of current edit buffer's header.
;	TXTBAS -> start of current edit buffer
;		  TXTFLR,EDTPTR,TXTCEL
;	EDTRWF -> EOF marking end of current edit buffer (EDTRWF=TXTRWF).
;
;	(free space)
;
;	MIDBAS -> start of middle buffers:
;		  remaining edit registers, if any.
;		  command buffer
;	CMDBAS -> start of command buffer
;		  CMDGET,CMDPUT (actually point into whatever buffer is
;				being currently executed)
;	CMDRWF -> last byte reserved for command buffer.
;
;	text registers start at CMDRWF + 1
;
;	MAXMEM -> past end of upper buffers -> screen tables
;
; Text-register buffers:  4-byte  header, 1 or more bytes of text, EOF
;
;		Header: 2-byte offset to start of text (always = 4)
;			2-byte offset to EOF at end of register.
;
; Edit buffers:  large header, possible text, EOF
;
;		Header: 2 byte offset to text base = header length
;			2 byte offset to EOF at text roof.
;			2 byte offset to text floor
;			2 byte offset to edit point
;			2 byte offset to text ceiling
;
;			PNTMAX*2 byte offsets of text markers
;			2 byte offset of BLMVEN
;
;			1 byte flags (see main storage)
;		       33 byte FCBs: INFCB,OUTFCB,RENFCB,REVFCB
;			1 byte for buffer fencing LF.
;
; Buffer pointers:
;
;	BUFTBL contains entries for each possible buffer/register.  An entry
;	       consists of a TYPE byte followed by start-of-buffer pointer.
;			TYPE = 0 if the buffer does not exist
;			     = 1 if the buffer is a text register
;			     = 2 if the buffer is an edit buffer
;
;	REGPTR -> entry in BUFTBL for current REGNUM (set by PNTREG)
;
;***************************************************************************

	.PAGE
;
; MAKCPY - Copy text into text register.
;
;	   Enter: HL and DE -> ends of text to copy.
;		  Smaller -> first char to copy.
;		  Larger  -> past last char to copy.
;
;		  REGAPP = "+" if text to be appended to register.
;
;	   Exit: 'NC' if text was copied, 'C' if no room.
;		  BC = length of text added (not necessarily
;		       register length).
;		  DE = block's lower address (for VMVTXT).
;		  HL = block's upper address +1 (for VMVTXT).
;
				;{RCCMD,RICMD}
BRKCPY:	CALL	MAKCPY		;Dead even in byte count w 2 routines
	RNC			;Slightly more esthetically pleasing this way
	JMP	BREAK
				;{RLCMD2,BRKCPY,VCPTXT @ VSVTXT}+
	BPROC	MAKCPY
MAKCPY:	CALL	POSDIF		;BC = HL - DE after setting HL > DE
	CALL	SV%BDH		;Save registers
	PUSH	D
	PUSH	B
	CALL	IFCLRG		;Clear register first unless appending
	POP	B		;BC = length of text to be copied
	MOV	A,B		;Check # bytes to be inserted
	ORA	C		;Any?
	JRNZ	..1		;Yes, branch
	CLR$	RIFLG		;Fix @RIr// bug
	POP	D
	RET			;'NC' from OR above
;
..1:	CALL	RGOPEN		;Establish BC extra bytes in REGNUM's buffer
				;BC saved
	JRC	..ERR		;Return error if insufficient room
;
;	Set HL-> start of text block and move text block into T-Reg.
;
	XCHG			;DE -> destination for text move
	TSTM	RIFLG		;Is this a RI command?
	MVI	M,00		;Clear flag
	POP	H		;HL-> beginning of text block unless RI cmd
	JRZ	..2		;Not RI cmd, input pointer OK, branch
	LHLD	RIPTR		;Point to adjusted start of insert string
..2:	CALL	RTLDIR		;Move the block
	ORA	A		;'NC'
	RET			;Return thru SV%BDH
;
..ERR:	POP	H		;Adjust stack
	STC			;Set 'C' for error return
	RET
	EPROC	MAKCPY
	.PAGE
;
; IFCLRG - Clear register REGNUM unless appending to text register.
;	   BREAK if edit buffer.
;
; CLRREG - Remove text register REGNUM from register space and from BUFTBL
;	   and close up text registers if REGNUM exists.
;	   BREAK if edit buffer.		[4/7/86]
;
				;{MAKCPY,RAMDWN}
	BPROC	CLRREG
IFCLRG:
CLRREG:	TST$	REGAPP		;Appending to the register?
	RNZ			;Yes, don't clear it

	CALL	PNTTRG		;Does REGNUM exist in BUFTBL?
	RZ			;No, nothing to do
	JC	BUFBRK		;No, it's an edit buffer:  N.G

	SUB%BC	TRGRWF,TRGBAS	;BC = # bytes in register less EOF marker
	INX$	B		;BC = length of register including EOF
	CALL	FREEDE		;Free up the register space
DELREG:	LHLD	REGPTR		;HL -> register-descriptor entry in BUFTBL
	MVI	M,0		;It's gone
	RET
	EPROC	CLRREG
;
; RAMDWN - Open (create if necessary) register REGNUM as wide as possible.
;
;	   Return:  Doesn't if insufficient room or if REGNUM is an edit buffer.
;		    RAMTOP -> past end of opened space -> EOF = TRGRWF.
;		    RAMBOT -> 1st free byte.
;		    Pointers adjusted.
;
				;{RLCMD,RQCMD}
RAMDWN:	CALL	IFCLRG		;Clear T-Reg if not appending
	CALL	FREESP		;BC = # bytes free space
	LXI	D,20		;Be sure to create register (+ spare)
	CALL	CMBCDE		;Is this much available?
	JC	BREAK		;No, *BREAK* now
	DCX$	B		;Allow extra room for header/EOF
	DCX$	B
	DCX$	B
	DCX$	B
	DCX$	B
	CALL	RGOPEN		;Open/create register
	JC	BREAK		;Should never happen
	MVIB$	RAMFL,0FFH	;For BREAK
	SHLD	RAMBOT		;
	MOVW	RAMTOP,TRGRWF	;
	MOVW	RAMRGP,REGPTR	;
	RET

	DSEG	$
RAMRGP:	DSW	1		;For RQr-<CTRL-C>-UPDVIS() bug
	CSEG	$
				;{RLCMD,BREAK}
RAMUP:	LDED	RAMBOT
				;{RQCMD}
RAMUP0:	MOVW	REGPTR,RAMRGP	;Fix RQ()-BREAK()-UPDVIS() bug
	LHLD	RAMTOP
	CALL	SBHLDE		;BC = distance to raise buffers
	XCHG
	PUSH	D		;Save -> end of register
	CALL	FREE		;Raise buffers back up and adjust pointers
	CLR$	RAMFL		;
	POP	D		;DE-> end of register
	INX$	D		;DE-> past end of register
	JMPR	UPDRG0
;
; RGOPEN - Establish BC extra bytes in REGNUM's buffer.		[4/17/86]
;	   Create a buffer at CMDRWF if register doesn't exist.
;	   Update all pointers.
;
;	Exit with success:
;	   'NC'
;	    HL-> 1st empty byte at end of the register.
;
;	Exit when insufficient room:
;	   'C', FREE is called to recover space
;
;	BREAK out if register is also an edit buffer.
;
				;{MAKCPY, RLCMD&RCCMD via RXOPEN}
	BPROC	RGOPEN
RGOPEN:	PUSH	B		;Save # bytes desired
	CALL	PNTTRG		;Does text-register REGNUM exist?
	POP	B		;BC = size needed
	INX	H		;HL-> where to append memory
	JC	BUFBRK		;Yes, but it's an edit buffer, N.G
	PUSH	B		;Save original value for later
	JRNZ	..2		;Yes, create space by lowering register(s)
;
;	Create text register @ CMDRWF.
;
	LXI	H,REGHL1+1	;HL = extra bytes count, header & EOF marker
	DAD	B		;HL = total size needed for T-Reg
	MOV	B,H
	MOV	C,L
	LXI	H,0000		;Allocate memory after CMDRWF
	CPIB$	REGNUM,REGKEY	;;Keytable?
	BNE	..2		;;No, branch
	LHLD	MAXMEM		;;Yes, keep table above text registers
..2:	CALL	MALLHL		;BC = # bytes allocated, HL-> 1st, DE-> last+1
	JRNC	..3		;Branch if OK
;
..ERR:	CALL	FREE		;Restore whatever was allocated
	POP	B		;(Fix stack)
	STC
	RET			;Return 'C'
;
;	Compute HL-> begin of header.
;	DE currently -> past end of register.
;
..3:	POP	B		;Restore original BC (Fix stack)
				;{RXCLOS}
UPDRG0:	PUSH	D
	XCHG			;DE-> new space
	LHLD	REGPTR
	MOV	A,M
	ORA	A		;Does register already exist?
	XCHG			;Assume No, HL-> new space
	JRZ	..4		;No, branch
	XCHG			;Yes, HL-> into BUFTBL
	INX$	H
	CALL	MVINHL		;HL-> register header (updated by FIXALL)
;
..4:	POP	D		;DE-> past end of register
	CALL	UPDREG		;Initialize the text register
				;BC saved, HL = TRGRWF
	DSUB	B		;HL-> allocation for actual text
	RET			;Return 'NC'
	EPROC	RGOPEN
;
;
; UPDREG - Update/initialize text register pointers, header and BUFTBL.
;	   Put descriptor into BUFTBL at (REGPTR).
;	   (type=1, register header pointer)
;	   Update/init register header
;	   Update/init associated register pointers (TRGRWF, etc)
;	   Put EOF marker at end of the text register.
;
;	   Enter: REGNUM = register number, REGPTR-> entry in BUFTBL
;		  HL-> begin of register header, DE-> past end of register
;	   Retrn: HL = TRGRWF -> EOF marker, BC saved.
;
				;{RGOPEN}
UPDREG:	SHLD	TRGBAS		;Initialize register base pointer
	XCHG			;HL-> past end
	DCX$	H		;HL-> roof
	SHLD	TRGRWF		;Initialize register end pointer
	MVI	M,EOF		;Set register EOF marker at roof
	DSUB	D		;HL = offset of RWF from BAS
	PUSH	H		;Save RWF offset
	PUSH	D		;Save -> base of header
	LHLD	REGPTR		;HL-> register descriptor entry
	MVI	M,1		;Text-register type = 1
	INX$	H		;HL -> second byte of descriptor entry
	POP	D		;DE -> register base
	CALL	STDEIN		;Store register pointer into descriptor
;
	XCHG			;HL -> register header
	LXI	D,REGHL1	;DE = length of text register header = 4
	CALL	STDEIN		;Store into header, HL++
	POP	D		;DE = roof offset
	CALL	STDEIN		;Put roof offset into header
;
; RASSOC - Calculate edit buffer associated register pointers.
;
				;{INXREG^,RMCMD}
RASSOC:	LHLD	TRGBAS
	LXI	D,REGHL1
	DAD	D
	SHLD	TRGBS2
	SHLD	TRGFLR
	SHLD	TRGPTR
	LHLD	TRGRWF
	SHLD	TRGCEL
	RET			;Return HL = TRGRWF for RGOPEN()
	.PAGE
;
; RGSHNK - Shrink a text register by removing some text.
;	   Enter: A  = register number.
;		  HL-> text to remove.
;		  BC = # bytes to remove.
;
				;{LEARN,YWCLOS}
RGSHNK:	PUSH	H
	PUSH	B
	CALL	GETREG		;Set REGNUM and REGPTR
	POP	B
	XTHL			;HL-> text to remove, (SP)-> end of T-Reg
	CALL	FREE		;Free the memory
	POP	D		;DE-> EOF of T-Reg
	INX$	D		;UPDREG() needs this
	JMPR	UPDRG0		;Adjust register PTRs
;
; GETTXT - Copy the text register to current edit point.   [9/05/86]
;
;	   Entry:  HL-> where to insert text copy.
;	   Return: DE-> new edit point
;		   'NC' if text copied, 'C' if no room left.
;
				;{RGCMD,VINTXT}
	BPROC	GETTXT
GETTXT:	PUSH	H		;Save -> where to insert text reg
	CMPMB	EDTNUM,REGNUM	;Don't allow Gr where 'r'=current edit buffer
	JZ	BUFBRK		;BREAK out if so
;
	CALL	PNTREG		;Does the register exist?
	POP	H		;HL-> where to insert text
	JRNZ	..1		;Yes, continue
	XCHG			;DE-> edit point
	RET
;
..1:	PUSH	D		;Save text reg address
	CALL	BUFFUP		;Move text buffer up
;
;	Copy from the text register to main text.
;
	XCHG			;DE-> where to insert
	POP	H		;HL-> Text reg address
	RC			;Return 'C' if BUFFUP() had no space
	JMP	RTLDIR		;Perform the move
	EPROC	GETTXT
;
; CHKEXE - Check if REGNUM is being executed.
;	   BREAK out if so.
;
				;{RCCMD, RLCMD, RICMD, RQCMD, EECMD}
	BPROC	CHKEXE
CHKEXE:	CMPMB	REGNUM,REGEXN	;Is it current register?
	JRZ	..NG		;Yes, give error
	MOV	D,A		;Save register number being checked
	LXI	H,REGSTK	;Check if it's on the stack
	MOV	C,M		;Get length of the stack
	INX$	H		;HL-> max length
..1:	MOV	A,C		;Get length
	SUI	STAKSZ		;Decrement it
	MOV	C,A
	RC
	REPT	STAKSZ,(
	INX$	H)		;Point to next entry
	MOV	A,M		;Check if this entry
	CMP	D		;
	BNE	..1		;No, keep looking
..NG:	JMP	MACBR1		;Give error and BREAK
	EPROC	CHKEXE
;
;
; FREESP - Return BC = HL = # free bytes, 'Z' set if none.
;
				;{NEEDTX,RXOPEN,UCMD,FGET}
				;{NEXFIL,PRVFIL}
FREESP:	LHLD	MIDBAS		;-> start of middle buffers
	LDED	EDTRWF		;-> end of lower buffers
	INX$	D		;-> start of lower free space
	CALL	SBHLDE		;BC = # unused bytes
	RNC			;Return # bytes in BC
	LXI	B,0000		;Else set to zero (Should never get here)
	XRA	A		;'Z'
	RET
;
; REGLEN - Return BC = length of text registers proper.
;	   'NZ' if any text registers proper.		[7/30/86]
;
				;{UCMD,WRTSTA}
	BPROC	REGLEN
REGLEN:	LXI	H,0		;Initialize subtotal = 0
	XRA	A		;Start with reg 0
..1:	PUSHA			;Save reg #
	PUSH	H		;Save subtotal
	CALL	PNTRG0		;BC = register length (0 for edit buffers)
	POP	H		;HL = subtotal
	DAD	B		;HL = updated subtotal
	POPA			;Retrieve reg #
	INR	A		;Get next reg #
	CPI	USRGMX		;Done?
	BLT	..1		;No, loop
	MOV	B,H
	MOV	C,L		;BC = total register lengths
	MOV	A,H
	ORA	L		;'Z' => no text registers
	RET
	EPROC	REGLEN
;
; MORBUF - Return 'NC' if more edit buffers than just main '@' buffer exist.
;
				;{WRTSTA}
	BPROC	MORBUF
MORBUF:	LXI	H,BUFTBL	;HL-> start of buffer descriptor table
	LXI	D,BFDSIZ	;Size of each table entry
	MVI	C,USRGMX	;C = # text registers
	MVI	A,2		;Code for edit buffer
..1:	CMP	M		;Edit buffer?
	RZ			;Yes, return 'NC'
	DAD	D		;Advance to next entry
	DCR	C		;Done?
	JRNZ	..1		;No, loop
	STC			;'C' for no additional edit buffers
	RET
	EPROC	MORBUF
;
; GETRLN - Get BC = length of register/buffer %A.		[4/7/86]
;	   Return 'C' if edit buffer, 'NC' if text register.
;	   (DE-> 1st text byte)
;	   HL saved.
;
;	   Sets REGNUM.
				;{RUCMD,UGET}
GETRLN:	PUSH	H
	CALL	GETREG		;BC = text length, flags set
	POP	H
	RET

	.PAGE
;
; PNTREG - Point to text register REGNUM.		[4/17/86]
;
;	   Enter: REGNUM = text register # to use.
;
;	   Exit:  'Z' & BC = 00 if register does not exist.
;		  REGPTR-> register descriptor in BUFTBL.
;
;		  ELSE:
;		  BC = # text bytes in the register/buffer.
;		  DE -> 1st text byte.
;		  HL -> past last text byte.
;
;		  TRGBAS-> buffer header
;		  TRGRWF-> EOF marker past last byte in buffer.
;
;		  'NC' & 'NZ' if a text register.
;
;		  'C'  & 'NZ' & additional TRGxxx ptrs if an edit buffer
;
				;{BEGIN,GETRLN,RGSHNK}
GETREG:	STA	REGNUM
				;{CLRREG,RGOPEN,GETTXT,GETRLN}
				;{EKOCMD,GETSLM,cmds H,M,RS,RT,RC,RF}
PNTREG:	LDA	REGNUM		;Get binary register number
	CALL	KBUFFR		;;HL-> register descriptor, A=buffer type
	SHLD	REGPTR		;Set ptr to buffer descriptor
	ORA	A		;Does the buffer exist?
	LXI	B,0		;In case not
	RZ			;No, return BC = 00, 'Z'
;
	PUSHA			;Save buffer type
	INX$	H		;HL -> buffer pointer in descriptor
	CALL	MVINHL		;HL -> buffer
	SHLD	TRGBAS		;Set register base pointer
;
;	Compute TRGRWF & register return values.
;
	MOV	B,H
	MOV	C,L		;BC-> buffer header
	POPA			;Retrieve buffer type
	DCR	A		;Edit buffer?
	JRNZ	INXEBR		;Yes, branch
	CALL	MVINDE		;DE = buffer header length, HL++
	XCHG			;HL = text start offset
	DAD	B		;HL-> text start
	XCHG			;HL-> EOF offset
	CALL	MVINHL		;HL = EOF offset
	DAD	B		;HL-> past last text char, DE-> 1st text char
	SHLD	TRGRWF
	JMP	SBHLDE		;BC = # text chars, 'NZ' & 'NC' for success
;
; PNTTRG - Point to actual text register/edit buffer.
;	   Return 'Z' & BC = 0 if non existent text register/edit buffer.
;	   Else return 'NZ' and PNTREG() return values.
;
				;{IFCLRG}
PNTTRG:	CALL	PNTREG
	RNZ
	PUSH	H
	LHLD	REGPTR
	MOV	A,M
	POP	H
	ORA	A
	RET
;
;
; PNTRG0 - Return Register pointers without changing memory values.
;	   Assume existing text register (not edit buffer)
;	   Enter:  A = register #.
;	   Retrn:  DE-> 1st text byte, HL-> past last text byte.   [7/30/86]
;
				;{KEYMAT,MAXWIN,REGLEN+}
	BPROC	PNTRG0
PNTRG0:	CALL	KBUFFR		;;HL-> buffer descriptor, A=type
;
;	Handle edit buffer or nonexistent buffer.
;
	MOV	D,H
	MOV	E,L		;In case not text register
	CPI	1		;Text register?
	JRNZ	..END		;No, return HL=DE, BC=0, 'Z'
;
	INX$	H		;HL -> buffer pointer in descriptor
	CALL	MVINHL		;HL -> buffer
	MOV	B,H
	MOV	C,L		;BC-> buffer header
	CALL	MVINDE		;DE = buffer header length, HL++
	XCHG			;HL = text start offset
	DAD	B		;HL-> text start
	XCHG			;HL-> EOF offset
	CALL	MVINHL		;HL = EOF offset
	DAD	B		;HL-> past last text char, DE-> 1st text char
..END:	JMP	SBHLDE		;BC = # text chars, 'NZ' & 'NC' for success
	EPROC	PNTRG0
;;
;; KBUFFR - Return HL-> buffer descriptor, A = buffer type for A=bufnum.
;;
				;;{PNTREG,PNTRG0,RTGET}
	BPROC	KBUFFR
KBUFFR:	LXI	H,BUFTBL	;HL-> main edit buffer descriptor
	CALL	AD3AHL		;HL-> descriptor for register
	MOV	A,M		;A = buffer type
	RET
	EPROC	KBUFFR
;
; INXEBR - Initialize register pointers from BC & HL's edit buffer header.
;
;	   ENTRY: BC = HL-> buffer header.
;		  REGNUM has name of current buffer-register.
;		  EDTNUM has name of current edit buffer.
;
;	   EXIT:  BC = # text bytes in the buffer.
;		  DE-> text floor.
;		  HL-> text ceiling.
;
;		  TRGBS2, TRGFLR, TRGPTR, TRGCEL, TRGRWF defined.
;
;		  'C' & 'NZ' to indicate existing edit buffer.
;
				;{PNTREG}
	BPROC	INXEBR
INXEBR:	CALL	NEGBC		;BC = -base
	LXI	D,TRGBS2	;DE -> register pointers block
	PUSH	H		;Save -> header
	CMPMB	EDTNUM,REGNUM	;Current edit buffer?
	POP	H		;HL -> register header
	MVI	A,REGHL2/2	;A = # pointers in edit buffer header
	JRZ	..1		;Yes, header invalid, branch
	CALL	STROFF		;No, compute values from header & store
	JMPR	..2

..1:	LXI	H,TXTBAS	;Yes, HL -> edit-pointer block
	ADD	A		;Twice as many bytes as there are pointers
	MOV	C,A
	CALL	MOVE		;Move edit pointers into register block

..2:	SUB%BC	TRGCEL,TRGFLR	;BC = # bytes, DE-> 1st text char, HL-> past last
	ORI	1		;'NZ'
	STC			;'C'
	RET
	EPROC	INXEBR
;
; P2N - Ptr to Num:  return %A with number of reg/buf indexed in BUFTBL by %HL.
;
				;{EXTCHK}
P2N:	LXI	D,BUFTBL	;HL-> start of table
	DSUB	D		;HL = offset from start of table of reg/buf entry
	LXI	D,BFDSIZ	;DE = size of a reg/buf entry
	CALL	DVHLDE		;BC = buf/reg number
	MOV	A,C
	RET
;
; GETQRN - Get numerical register number into REGNUM.
;	   Exit:  CMDGET-> past end of register #.
;
				;{XCMD+}
GETQRN:	MOVW	EVLGET,CMDGET
;6	MOVW	EVLGET+2,CMDGET+2
	CALL	EGETQR
	MOVW	CMDGET,EVLGET
	RET
;
; GETCRN - Get text register number/letter in command mode and set REGNUM.
;	   '@' is the main text register
;
;	    Handles variables: #r where 'r' is a numerical variable.
;	    Numerical register must contain ASCII value of text register.
;
				;{MCMD,RCMDs,XCMD-,EECMD}
	BPROC	GETCRN
GETCRN:	MOVW	EVLGET,CMDGET
;6	MOVW	EVLGET+2,CMDGET+2
	CALL	GETERN
	MOVW	CMDGET,EVLGET
	RET
				;{GETCRN,UGET}
GETERN:	XRA	A		;Get zero
..1:	STA	REGAPP		;Set/reset appending flag
	CALL	ENXCHR		;Get next character
	CPI	'+'		;Appending?
	BEQ	..1		;Yes, set flag, skip any spaces
;
	CPI	'#'
	BNE	SETRN
	CALL	EGETQR		;CHL = register designation
	CALL	VALCHL		;L = register designation in ASCII
	MOV	A,L
	EPROC	GETCRN
;
; SETERN - Set REGNUM.
				;{GETERN^,AUTEXE-,LODEXE+,SDFSET}
	BPROC	SETRN
SETRN:	CALL	CONVRG		;Convert register # to binary
	CPI	REGMAX		;Valid register number?
	BLT	..1		;Yes, use it
	XRA	A		;No, use 0
..1:	STA	REGNUM		;Save register number
	RET
	EPROC	SETRN
;
; GETMNM - Get marker number to use and set REGNUM.
;
GETMNM:	CALL	SV%BDH		;Save registers
	MVIB$	GETRMX,PNTMAX	;Set maximum register number
	LXI	H,MRKMSG
	JMPR	GETRN0		;Merge
;
; GETVRN - Get text register number in visual mode and set REGNUM.  [5/20/86]
;
	BPROC	GETRNM
GETRNM:	CALL	SV%BDH		;Save registers
	MVIB$	GETRMX,REGMAX	;Set maximum register number
	LXI	H,REGMSG
;
; Set up status line to ask for register number.
;
GETRN0:	CALL	STAPRM		;Prompt for register/marker #
	XRA	A		;Get zero
..1:	STA	REGAPP		;Set/reset appending flag
..2:	CALL	GETCHR		;Get next key char, 'Z' if simple char
				;BC = function code, or C = char
;
;	Check if <CTRL-C> was pressed to abort
;
	CPIB$	DECDAT,CTRLC	;;User want to abort?
	JZ	CANCL1		;Yes, cancel, close status line
;
	MOV	A,B		;;Was function key pressed?
	ORA	A
	JRZ	..3		;;No, branch, C = char
;
	CALL	MACRST		;Kill kestroke macro
	TST$	CNCLFL		;Did user press [CANCEL]?
	JNZ	CANCL1		;Yes, cancel
	JMPR	..ZERO		;No, set REGNUM = 0 to use default register
;
..3:	MOV	A,C		;No, get char back
	CPI	'+'		;Append?
	BNE	..4		;No, branch
;
	LXI	H,RAPMSG	;HL-> "APPEND"
	LDA	PHYHOR		;A = position on status line
	CALL	WRTMSG		;Add to the prompt
	JMPR	..1
;
..4:	CALL	CONVRG		;Make binary
	LXI	H,GETRMX	;HL -> maximum register number
	CMP	M		;Register number OK?
	BGE	..2		;No, wait for good number
	JMPR	..DONE		;Set REGNUM
;
..ZERO:	XRA	A		;Use 0
..DONE:	STA	REGNUM		;Save register number
	JMP	STADON		;Close status line
	EPROC	GETRNM
;
RAPMSG:	DC	' APPEND ' [00]
	.PAGE
;
; CONVRG - Convert register # in A from ASCII to binary.
;
;	   Return: value 0-9 for ASCII digits, 10-35 for ASCII letters,
;		   255 for '$', MAINRG for '@' (or any other special char).
;
				;{GETCRN,GETRN0}
	BPROC	CONVRG
CONVRG:	CALL	DIGCHK		;Is it a digit?
	BNE	..1		;No, presumably a letter
	SUI	'0'		;Yes, make binary, in range 0-9
	RET			;'C', 'NC'
;
..1:	CALL	LETCHK		;Is it a letter?
	BNE	..2		;No, special char
	CALL	CONVUC		;Yes, convert to upper case
	SUI	'A'-10		;Convert to binary, in range 10-35
	RET			;(:;<=>?@) converted to 3-9. Ehhh
;
..2:	CPI	'$'		;Special command buffer designator?
	BNE	..3
	MVI	A,255
	RET
;
..3:	MVI	A,MAINRG	;Main text buffer, now, for any special char
	RET
	EPROC	CONVRG
;
; EDTNAM - Convert edit buffer # EDTNUM to ASCII
;
; RTOASC - Convert Text Register # in A to ASCII.
;
;	   Return: '$' for command buffer (255)
;		   '@' for MAINRG
;		   ASCII equiv. for all other values
;
EDTNAM:	LDA	EDTNUM		;;Get edit buffer #
;
	BPROC	RTOASC
RTOASC:	CPI	10		;Digit?
	BGE	..1		;No, skip
	ADI	'0'
	RET
;
..1:	CPI	USRGMX		;Letter?
	BGE	..2		;No, skip
	ADI	'A'-10
	RET
;
..2:	CPI	MAINRG		;Main edit buffer?
	BNE	..3		;No, branch
	MVI	A,'@'		;@ for main edit buffer
	RET
;
..3:	CPI	255		;Command buffer?
	RNZ			;No, return %A
	MVI	A,'$'		;$ for command buffer
	RET
	EPROC	RTOASC

