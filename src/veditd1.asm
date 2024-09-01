	.PAGE
	IF	VPLUS, [
	.TITLE	'VED-PLUS'
;****************************************
;*					*
;*	INITIALIZED STORAGE AREA	*
;*					*
;****************************************
;
; Data contained first in the storage area is of four types:
;
;	1) Constant (may need to be initialized)
;	2) Proper to the segment (MAXMEM,RGBASE,...)
;	3) Edit buffer dependent
;	4) Fleeting:  Will never be needed after an edit-buffer change.
;
; Thus, this area can be copied to a new segment at its creation
; and need not be renewed thereafter;   and also serves as a de
; facto storage area for the edit buffer variables when swapping
; to a new buffer in a new segment.  (These variables must be
; copied into the buffer header when swapping to a new buffer in
; the same segment).
;
;
; INIVLS - Table of initial values (can restart VEDIT from 100H...maybe)
;
INIVLS:	DW	TARGST		;TARGST
	DB	EOS
	DW	TERMCH		;TERMCH
	DB	ESC
	DW	SAVTRM		;SAVTRM
	DB	ESC
	DW	GETDCF		;GETDCF
	DB	0
	DW	TRACEF		;TRACEF
	DB	0
	DW	TRDENB		;TRDENB
	DB	0
;6	DW	ENBRFL		;ENBRFL
;6	DB	0
	DW	INXFLG		;INXFLG
	DB	0
;
	DW	EXCBUF		;EXCBUF
	DB	0
	DW	REGEXN		;REGEXN
	DB	0FFH
	DW	OLDEXN		;OLDEXN
	DB	0FFH
	DW	EDTNUM		;EDTNUM
	DB	0FFH
;
	DW	AUXFLG		;Used to be in GLBTBL
	DB	00
	DW	LSOPFL
	DB	00
;
	DW	RIFLG		;RIFLG
	DB	0
	DW	PRVFLG		;PRVFLG
	DB	0
	DW	PEEKCH		;PEEKCH
	DB	0
	DW	VISFLG		;VISFLG
	DB	0
	DW	LSTFLG		;LSTFLG
	DB	3
	DW	SRCHFL		;SRCHFL
	DB	0
	DW	SRCERR		;SRCERR
	DB	0
	DW	JUSDIR		;JUSDIR
	DB	0

	IF	MSDOS, [
	DW	BADDRF		;BADDRF
	DB	0
	]

	DW	RAMFL		;RAMFL
	DB	0
	DW	JAMFL		;JAMFL
	DB	0
	DW	PARMFL		;PARMFL
	DB	0
	DW	SAVPFL		;SAVPFL
	DB	0

	IF	DEMO, [
	DW	KEYTBL+1
	DB	0
	]

	DW	0		;End of byte values
;
;
;
	DW	REGSTK		;REGSTK
	DW	STAKSZ*XSTKMX*256
	DW	ITRSTK		;ITRSTK
	DW	STAKSZ*ISTKMX*256
	DW	OPSTCK		;OPSTCK
	DW	STAKSZ*OSTKMX*256
;
	DW	ROUTAD		;ROUTAD
	DW	CONOUT
;6	DW	RGSEG0
;6	DW	0
;6	DW	DLTDSG
;6	DW	0
	DW	0		;End of word values
;
; FILVLS - Table of values to be initialized when creating an edit buffer
;
FILVLS:	DW	DELFLG		;DELFLG
	DB	0
	DW	BOFFLG		;BOFFLG
	DB	0
	DW	REVFLG		;REVFLG
	DB	0
	DW	INFLG		;INFLG
	DB	0
	DW	OUTFLG		;OUTFLG
	DB	0
	DW	RENFLG		;RENFLG
	DB	0
	DW	TOPCHR		;TOPCHR
	DB	EOF
;
	DW	INDPOS
	DB	0
	DW	CNTBFL		;CNTBFL  (Count LF's from  beginning)
	DB	1
	DW	VSHZBG		;VSHZBG
	DB	0
	DW	NWSCFL		;NWSCFL
	DB	89H
	DW	0		;End of byte values
;
	DW	BLMVEN		;BLMVEN - could be 11th text marker
	DW	00
	DW	OLDACT		;OLDACT
	DW	00
	DW	00		;End of word values
	]			;<IF VPLUS>
	.PAGE
	IFNOT	VPLUS, [
	.TITLE	'VED-MAIN'
;****************************************
;*					*
;*	FIXED STORAGE AREA		*
;*					*
;****************************************
;
; INITTB - Table of initial values for some variables.
;
INITTB:	DB	00		;DELFLG
	DB	00		;BOFFLG
	DB	EOF		;TOPCHR
	DB	00		;LSOPFL
	DB	00		;FULLFG

	]			;<IFNOT VPLUS>
;
;****************************************
;					*
;	MSDOS Path Related Stuff	*
;					*
;****************************************
;
;	MSDOS special-directory values.
;
	IF	MSDOS, [
AUXDEF	DB	'd:',CR,0	;Default directory path
AUXDF0	DB	'd:\',CR,0	;Root directory
AUXVED	DB	'd:\VEDIT\',CR,0 ;\VEDIT ddirectory path
BADDRF	DS	1		;Flag to not report bad directory (\VEDIT)
	]			;<IF MSDOS>
	.PAGE
;
;	Messages.
;
MAKMSG:	DC	'NO DIRECTORY SPACE' [00]
WMSG:	DC	'NO DISK SPACE' [00]
CLSMSG:	DC	'CLOSE ERROR' [00]
RMSG:	DC	'READ ERROR' [00]
CREMSG:	DC	'NEW FILE' [00]
INFMSG:	DC	'INPUT FILE:  ' [00]
OUFMSG:	DC	'OUTPUT FILE:  ' [00]
CRMSG:	DC	'<CR>' [00]
LFMSG:	DC	'<LF>' [00]
CNTPMT:	DC	'-' [00]
BSSTRG:	DB	BS,SPACE,BS,00
BKKPRM:	DC	'<<'
	DB	00,00		;Spare byte
BAKSTR:	DC	'BAK'
;
	IF	P8086, [
RLSGMS	DC	'UNABLE TO RELEASE MEMORY SEGMENT' [CR] [LF]
	DC	'SAVE FILES (EXA) AND REBOOT' [00]
	]
;
NOTVMS:	DC	'UNABLE TO ENTER VISUAL MODE' [00]
ILGMSG:	DC	'INVALID COMMAND "  ' [00]	;Extra space needed!
ISFMSG:	DC	'NESTING ERROR' [00]
BRKMSG:	DC	'*BREAK*' [00]
QUTMSG:	DC	'ABANDON FILE (Y/N)? ' [00]
QTAMSG:	DC	'ABANDON ALL FILES (Y/N)? ' [00]
CHDMSG:	DC	[CR] [LF] 'INSERT NEW DISK AND PRESS <RETURN> ' [00]
EFMSG:	DC	'OK TO CLOSE (ERASE) OUTPUT FILE (Y/N)? ' [00]
EKMSG:	DC	'OK TO ERASE THESE FILES (Y/N)? ' [00]
RSMSG:	DC	'OK TO OVERWRITE EXISTING FILE (Y/N)? ' [00]
LMSG:	DC	'END OF EDIT BUFFER REACHED' [00]
REVMSG:	DC	'REV FILE OPEN' [00]
NOIMSG:	DC	'NO INPUT FILE' [00]
NOOMSG:	DC	'NO OUTPUT FILE' [00]
OTOMSG:	DC	'CANNOT OPEN TWO FILES' [00]
NOMEMO:	DC	'NO MEMORY TO AUTO-BUFFER' [00]
PRMMSG:	DC	'BAD PARAMETER' [00]
MPMFMS:	DC	'FILE IS R/O - OUTPUT '
NOPMSG:	DC	'FILE NOT OPENED' [00]
;
BDFMSG:	DC	'BAD FILENAME' [00]
FNFMS2:	DC	'FILE NOT FOUND' [00]
FNFMSG:	DC	'FILE NOT FOUND'
SPSMSG:	DC	':  ' [00]
;
PRTUMS:	DC	'PRINTING -- Press <CTRL-C> to Abort ' [00]

	IF	MSDOS, [
DRLERR:	DC	'PATHNAME TOO LONG' [00]
BADDRV:	DC	'INVALID DRIVE' [00]
	]
;
HLPMSG:	DC	'NOT FOUND IN HELP FILE' [00]
WAITMS:	DC	'Press any key to continue' [00]
HLPPMT:	DC	'"EX" to Exit, "EQ" to Quit, "V" to Edit, "H" or "EH" '
	DC	'for Help' [CR] [LF] [00]
;
	IF	MPM, [
MPMDMS:	DC	'DISK IN USE BY MP/M' [00]
MPMLMS:	DC	'WAITING FOR PRINTER -- Press <CTRL-C> to Abort ' [00]
	]
;
	IF	DEMO, [
DEMMSG:	DC	'FILE IS TOO LARGE FOR DEMO VERSION - GIVE COMMAND "EQ" TO QUIT' [00]
DEMMS2:	DC	'"W", "RS" AND "YK" COMMANDS ARE INOPERATIVE IN DEMO VERSION' [00]
	]
;
;
	IF	VPLUS, [
MCRMSG:	DC	[CR] [LF] 'MACRO ERROR IN   ' [00]
NULMSG:	DC	' ' [00]
;
ILVMSG:	DC	'UNKNOWN INTERNAL VALUE " " ' [00]
ILDMSG:	DC	'INVALID DELIMITER FOR JP COMMAND:  " " ' [00]
ILGOP:	DC	'INVALID EDIT BUFFER OPERATION' [00]
ILLQRN:	DC	'INVALID NUMERIC REGISTER' [00]

ILCSYN:	DC	'END OF MACRO REACHED, STRUCTURE STILL OPEN ' [00]
STROPM:	DC	'END OF MACRO REACHED, STRING STILL OPEN ' [00]
BRINMS:	DC	'JUMPING INTO A FLOW CONTROL STRUCTURE?  ' [00]
;
LBLMSG:	DC	'LABEL TERMINATOR "!" MISSING' [00]
LBMSNG:	DC	'LABEL MISSING: ' [00]
TRVERM:	DC	'UNABLE TO TRACE VISUAL-MODE' [00]

EDTMSG:	DC	'EDITING ' [00]
BUFRMS:	DC	'BUFFER ' [00]
SAVMSG:	DC	'SAVING ' [00]
;
VHLPMS:	DC	'INVALID [HELP] REQUEST' [00]
DIRMSG:	DC	[CR] [LF] 'DIRECTORY ' [00]

	IF	MSDOS, [
BADDIR:	DC	[CR] [LF] 'DIRECTORY NOT FOUND: ' [00]
	]

VHELP:	DC	'VPHELP.HLP' [CR] [00]
VEHELP:	DC	'VPEHELP.HLP' [CR] [00]
	]
;
	IFNOT	VPLUS, [
MCRMSG:	DC	'MACRO ERROR ' [00]
;
VHELP:	DC	'VHELP.HLP' [CR] [00]
VEHELP:	DC	'VEHELP.HLP' [CR] [00]
	]
;
VVHELP:	DC	'VVHELP.HLP' [CR] [00]
;
;
;	Status line messages
;
LINMSG:	DC	'LINE:     ' [00]
COLMSG:	DC	'COL:   ' [00]
FILMS1:	DC	'FILE: ' [00]
FILMS2:	DC	'File: ' [00]
;
	IF	VPLUS, [
RGNMSG:	DC	'E#' [00]
REGMSG:	DC	'REGISTER [+] 0-9 A-Z ' [00]
	] [
REGMSG:	DC	'REGISTER [+] 0-9 ' [00]
	]
;
ENDMSG: DC	'1 END' [00]
TXTMSG: DC	'TEXT' [00]
MORMSG: DC	'MORE' [00]
INSMSG: DC	'INSERT' [00]
FULMSG:	DC	'FULL' [00]
DISKMS:	DC	'DISK' [00]
;
MRKMSG:	DC	'MARKER NUMBER 0-9 ' [00]
FNDMSG:	DC	'FIND? ' [00]
RPLMSG:	DC	'REPLACE WITH? ' [00]
RPLPMT:	DC	'REPLACE?  [Y]es  [N]o  [R]est  [C]ancel ' [00]
CNFMSG:	DC	'CANNOT FIND: ' [00]
OPTMSG:	DC	'OPTIONS: [A]gain  [B]egin  [G]lobal  [R]everse ' [00]
DSKMSG:	DC	'PLEASE WAIT FOR DISK' [00]
CMMMSG:	DC	'COMMAND' [00]
	.PAGE
	.XLIST
	IF	LSBUF, [
	.LIST
	]
	.PAGE
;****************************************
;*					*
;*	SEGMENT RESIDENT STORAGE	*
;*					*
;****************************************
MAXMEM:	DSW	1		;End of allowable memory pointer
				;Actually, screen tables are above MAXMEM
CMDRWF:	DSW	PTRSIZ		;Command buffer roof
CMDBAS:	DSW	PTRSIZ		;Command buffer base (first of the five)
CMDMAX	==	CMDRWF

	IF	VPLUS, [
MIDBAS:	DSW	PTRSIZ		;Base for middle buffers/roof for free space
EDTBAS:	DSW	PTRSIZ		;-> edit-buffer header
;6BASE0	DW	?
;6DSEG0	DW	?		;Main edit segment
EDTNUM:	DS	1		;Name of current edit buffer
	]
;			5 in a row for FIXCMD
	IFNOT	VPLUS, [
MIDBAS	==	CMDBAS		;CMDBAS is bottom of high memory stuff
	]
	IF	VPLUS, [
;
;************************************************************************
;									*
;	E D I T    B U F F E R    H E A D E R				*
;									*
;************************************************************************
;
HDRBGN:				;Start of swappable edit environment
;
;****************************************
;*					*
;*	POINTER SWAP STORAGE AREA	*
;*					*
;****************************************
;
;	Main text pointers.	Watch order!!
;	Order determined by FIXMRK,RDPRV,SAVEBF,LOADBF,INXTRP,PUTTPT.
;
PTRBGN:
TXTBAS:	DSW	1		;-> Beginning of text buffer (above EDTBAS)
TXTRWF:	DSW	1		;-> text buffer roof (-> EOF)
EDTRWF	==	TXTRWF
;
TXTFLR:	DSW	1		;-> text window floor         v (RDPRV)
EDTPTR:	DSW	1		;-> Command cursor position   ^ (RDPRV)
TXTCEL:	DSW	1		;-> text window ceiling (-> EOF)
TXTBGN	==	TXTFLR
TXTTOP	==	TXTCEL
;
;
;
;	The Text Markers must be followed by BLMVEN!
;
PNTTBL:	DS	PNTMAX*2	;Text marker table
BLMVEN:	DSW	1		;Block marker for visual mode
				;Some routines treat this as 11th marker
OLDACT:	DSW	1		;OLDACT  (previous TXACTV)
;
;	Other pointers may be placed here.
;
PTREND:
PTRNUM	=	(PTREND-PTRBGN)/2
;
;****************************************
;*					*
;*	SIMPLE SWAP STORAGE AREA	*
;*					*
;****************************************
;
;	Initialized pointers and flags.
;
SWPBGN:				;FILE RELATED VALUES
DELFLG:	DS	1		;.BAK file to be deleted flag
BOFFLG:	DS	1		;Begin of file reached (0=YES)
TOPCHR:	DS	1		;Char just after text window
;
CNTBFL:	DS	1		;(1) Flag to count all LFs for LINFIL
CNTOUT:	DSW	1		;(0) # of LFs written out to disk
VSHZBG:	DS	1		;Value for HZSCBG when window used for vm
;
;	Misc. variables
;
LINFIL:	DSW	1		;Line number in file
INDPOS:	DS	1		;Indent position
HORFLG:	DS	1		;Cursor positioning option / flag
;
;
;	Various FCBs for CP/M communication.
;
INFLG:	DS	1		;FCB is open flag (0 = No)
INUSR:	DS	1		;User # for input  FCB, not yet used
INFCB:	DB	0
	DC	'           '	;;Blank filename
	DS	21

	IF	MSDOS, [
	DS	4		;For random block i/o
	DS	2		;Offset to filename in path string
	DS	DIRLEN		;For storing pathname
	]
;
OUTFLG:	DS	1		;FCB is open flag (0 = No)
OUTUSR:	DS	1		;User # for output FCB
OUTFCB:	DS	9		;Output FCB
	DC	'$$$'
	DB	0,0,0,0
	DS	16
	DB	0

	IF	MSDOS, [
	DS	4		;For random block i/o
	DS	2		;Offset to filename in path string
	DS	DIRLEN
	]
;
RENFLG:	DS	1		;FCB is open flag (0 = No)
RENUSR:	DS	1		;User # for REN FCB
RENFCB:	DS	16		;Rename FCB
	DS	12
	DB	0,0,0,0,0

	IF	MSDOS, [
	DS	4		;For random block i/o
	DS	2		;Offset to filename in path string
	DS	DIRLEN
	]
;
; Reverse FCB
;
REVFLG:	DS	1		;FCB is open flag (0 = No)
REVUSR:	DS	1		;User # for .REV file
REVFCB:	DB	0		;Drive
	DS	8		;Use output filename
	DC	'$R$'
	DB	0,0,0,0
	DS	16
	DB	0

	IF	MSDOS, [
	DS	4		;For random block i/o
	DS	2		;Offset to filename in path string
	DS	DIRLEN
	]
	]			;<IF VPLUS>

VARBUF:				;Everything below here gets carried along with
				;each segment change
SWPXNM	=	VARBUF-SWPBGN	;For AUTSQZ() [8086]
;
NWSCFL:	DS	1		;Screen writing flag (Multibit)
;
;	Installed values
;
INSTBG:	DS	1		;1st tab position
TABTBL:	DS	34		;TAB POSITIONS
;
ESTBL:				;SWITCHES
EXPTSW:	DS	1		;Expand Tab with spaces?
ATBFSW:	DS	1		;Auto buffering?
AUTIND:	DS	1		;Auto-Indent?
PTAFSW:	DS	1		;Set edit pointer after register insert?
SRCNSW:	DS	1		;Ignore upper/lower case in searches
MSDOSW:	DS	1		;MS-DOS record oriented write
SWCCNV:	DS	1		;Reverse upper and lower case
COLSW:	DS	1		;':' modifier
ATTSW:	DS	1		;'@' modifier
GLOBSW:	DS	1		;'_' modifier
JUSTSW:	DS	1		;Justify option (0 = OFF) (1 = YES) (2 = UNDO)
;
	DS	4		;For diddling
;
EPTBL:				;PARAMETERS
CURTYP:	DS	1		;Cursor type
BLNKRT:	DS	1		;Cursor blink rate
INDINC:	DS	1		;Indent increment
UCNVSW:	DS	1		;Lower to upper case convert
CMNTCH:	DS	1		;Character for conditional convert
CRDELY:	DS	1		;Delay for CR in command mode
WRAPCL:	DS	1		;Column for word wrap. 00 = off
BIT7AL:	DS	1		;Allow Bit 7 on input
HOROPT:	DS	1		;Horizontal cursor pos. option
HZSCLN:	DS	1		;Line length with horizontal scolling
HZSCIC:	DS	1		;Horizontal scroll increment
;
	DS	4		;For diddling
;
	IF	VPLUS, [	;Above values are kept in buffer header
SWPEND:
HDREND:
SWPNUM	=	SWPEND-SWPBGN
EHDLEN	=	HDREND-HDRBGN+1	;(1 for text fencing LF)
	]

	.PAGE
;****************************************
;*					*
;*	GLOBAL STORAGE AREA		*
;*					*
;****************************************
;
;	VEDIT Only - Initialized pointers and flags.
;
	IFNOT	VPLUS, [
FIRINI:				;{VEDIT}
DELFLG:	DS	1		;.BAK file to be deleted flag
BOFFLG:	DS	1		;Begin of file reached (0=YES)
TOPCHR:	DS	1		;Char. just after text window
	]			;<IFNOT VPLUS>
;
LSOPFL:	DS	1		;List device attached?
FULLFG:	DS	1		;Text buffer full flag. (00 = No)
;
LASINI:				;{VEDIT}
;
;	Globals for File Handling
;
CPMVER:	DSW	1		;Saved value of BDOS #12
;
;6	IF	MSDOS
;6MSDOSV	EQU	THIS WORD	;For storing/retrieving Major & Minor version #
;6VERMAJ	DB	?		;For accessing MSDOS major version #
;6VERMIN	DB	?		;For accessing MSDOS minor version #
ORGDRV:	DS	1			;For logged-in drive at startup
;6ORGDIR	DB	65 DUP (?)	;For ASCIIZ name of initial directory
;6	ENDIF
;
DEFUSR:	DS	1		;Current default user number
DEFDRV:	DS	1		;Current default drive (DEFUSR above)
CURUSR:	DS	1		;Current user number
;
	IF	VPLUS, [
CURFCB:	DSW	1		;-> FCB of last open attempt
	]

	IF	MSDOS, [
DEFDR3:	DS	3		;Header bytes for DEFIR
DEFDIR:	DS	65		;For currently selected default directory
	]

	.PAGE
;
;	Since the VEDITO1 code is only used for initialization, VEDIT (only)
;	can subsequently use it for storage.  This area is about 256
;	bytes large.  Most of the space is used by the stack.
;
	IFNOT	VPLUS, [
LASTSS:
	ORG	OVERLY
;
INFLG:	DS	1		;FCB is open flag (0 = No)
INUSR:	DS	1		;User # for input  FCB, not yet used
INFCB:	DS	33
	]
;
;	Search / Replace string storage
;
TARGST:	DS	TARGLN+1	;Search string buffer
REPLST:	DS	TARGLN+1	;Replace string buffer
;
;	String-input variables.
;
STRFLR:	DSW	1		;Floor of string buffer, -> first byte
STRPUT:	DSW	1		;Next available byte in string buffer
STRCEL:	DSW	1		;Ceiling of string buffer, -> past end

	IFNOT	P8086, [
STRBUF	==	BSTACK
STRBEN	==	STRBUF+40
	] [
STRBUF:	DS	80		;;For Help and V-file routines
STRBEN:
	]
;
;	Numeric value registers
;
	IF	VPLUS, [
VALREG:	DS	3 * QRGMAX	;24-bit user-definable value registers
	] [
VALREG:	DSW	10		;10 numeric registers
;
BSTACK:				;Bottom of stack used for temp storage
				;Top of stack set in VEDIT-B1
				;VPLUS stack must be 230 bytes large
	ORG	LASTSS
	]
;
	.PAGE
;
;	Segment pointers
;
;6EDTSEG	DW	?
;6TRGSEG	DW	?
;6RGSEG0	DW	0		;Initial segment for register usage
;6CMDSEG	DW	?		;Segment pointer to main input cmd buf
;6CURSEG	DW	?		;Segment pointer of last segment entered (EECMD)
;6LASTSG	DW	?		;Segment pointer of last segment allocated
;6DLTDSG	DW	?		;Pointer to segment that has been emptied
;
;	Other segment management variables
;
;6	IF	CPM86
;6DLTDSL	DW	?		;Length in paragraphs of emptied segment
;6MCB	EQU	THIS WORD		;Memory Control Block
;6MCBBAS	DW	?		;Block begin in paragraph units
;6MCBLEN	DW	?		;Block length	   "
;6MCBEXT	DB	?		;Extended communication parameter
;6	ENDIF
;
;	Command mode pointers.  Watch order.  See FIXCMD()
;
CMDGET:	DSW	PTRSIZ		;Command GET pointer
CMDPUT:	DSW	PTRSIZ		;Command PUT pointer
;
	IF	VPLUS, [
RIPTR:	DSW	PTRSIZ		;Pointer to start of insert string for RICMD
	]
;
DELPTR:	DSW	1		;-> Beginning of text to move
;
;	Text register pointers.  Watch order!!
;
	IF	VPLUS, [
TRGBAS:	DSW	1		;Base of current register (its header)
TRGBS2:	DSW	1		;For edit buffers			  v
TRGRWF:	DSW	1		;Roof of current text register (-> EOF). ^
TRGFLR:	DSW	1		;Text floor
TRGPTR:	DSW	1		;For edit buffers.  Used by RMCMD
TRGCEL:	DSW	1		;Text ceiling -> EOF
TRGCHR:	DS	1		;Character from text register ceiling
REGHL1	=	TRGRWF-TRGBAS	;Length of simple text register header = 4
REGHL2	=	TRGCEL-TRGBAS	;Length of text pointers in edit buffer header
	]
;
;	Text register associated values.
;
REGNUM:	DS	1		;Current text register
REGASC:	DS	1		;;Window # in ASCII (only valid for "#w")
REGEXN:	DS	1		;Currently executing register number
REGPTR:	DSW	1		;Pointer into REGTBL
;
REGAPP:	DS	1		;Append to register (0 = No)
MVCPFL:	DS	1		;Move or Copy text flag. (0 = Copy)
RIFLG:	DB	0		;Flag for RI command
;
;
;
	IF	VPLUS, [
GETRMX:	DS	1		;Maximum value for marker/register in GETRNM()
AUXREG:	DS	1		;Auxiliary register for |Rr in-line command
	DW	0		;Help Tom find things
BUFTBL:				;DB buffname, DW buf ptr, ...
	] [
REGTBL:				;DW ptr, DW length, ...
	]

	DS	BFDSIZ * REGMAX	;Buffer/Register pointer table
;
;	The KEYTBL and print headers are also made into text registers.
;
	DS	BFDSIZ		;KEYTBL - Address and length
	DS	BFDSIZ		;Window structure
;
	IF	WORDP, [
PPHEAD:	DS	BFDSIZ		;Header
PPSUBH:	DS	BFDSIZ		;Subheader
PPFOOT:	DS	BFDSIZ		;Footer
	]
;
REGTEN:
	IF	VPLUS, [
	DS	1		;For end-of-table marker (0ffh)
	]
;
;	Register stack used for nested "M" command.
;
	IF	VPLUS, [
REGSTK:	DS	1		;Top-of-stack offset
	DS	1		;Stack size: STAKSZ*XSTKMX
	DS	STAKSZ * XSTKMX	;DW CMDPUT
				;DW CMDGET
				;DB REGEXN
	] [
REGSTK:	DB	00		;Execution stack offset
	DB	5 * XSTKMX	;Size of stack
	DS	5 * XSTKMX	;Register execution table
				;DW CMDEND
				;DW CMDGET
				;DB REGEXN
	]
;
;	Iteration stack used for nested iteration loops
;
	IF	VPLUS, [
ITRSTK:	DS	1		;Top-of-stack offset
	DS	1		;Stack size:  STAKSZ * ISTKMX
	DS	STAKSZ * ISTKMX	;DW CMDGET  (0=>THEN, 1=>ELSE, other=>REPEAT)
	] [
ITRSTK:	DB	0		;Iteration stack offset
	DB	5 * 8		;Max stack size
	DS	5 * 8		;Stack area
	]
;
;	Listing routine stack for ROUTAD, LOGPOS and LSTFLG.
;
LSTSTK:	DB	0		;Listing stack offset
	DB	STAKSZ * 10	;Max stack size
	DS	STAKSZ * 10	;Stack area
;
;	Initial Listing goes directly to SYSTEM calls.
;
INPFLG:	DS	1		;Input conversion flag (Multibit)
LSTFLG:	DB	03H		;Listing flag (Multibit)
ROUTAD:	DW	CONOUT		;Address of output routine

	IF	DEMO, [
DEMCHK:	DW	0927AH		;;Checksum for F2
	]
;
;	Variables for each command executed.
;
ITRCNT:	DSW	1		;Absolute value of 1st command parameter
CMDCHR:	DS	1		;Current command char

MINUS:	DS	1		;Is # value negative? (0=No)        v (PROCMD MOVE3)
NUMFLG:	DS	1		;Explicit iteration? (0=No)	    ^ (Don't move)
RMINUS:	DS	1		;"REAL" minus flag ('-' = Yes)	    ^ (Order dependent & associated with XMINUS etc)
RPLUS	==	RMINUS		;"REAL" plus  flag ('+' = Yes)

NM2FLG:	DS	1		;2d command parameter? (',' = Yes)
PNDFLG:	DS	1		;"#" largest number
COLFLG:	DS	1		;":" error return?		   (ATTFLG v)
ATTFLG:	DS	1		;"@" explicit delimiters(GLBFLG v) (COLFLG ^)
GLBFLG:	DS	1		;"_" global search?		   (ATTFLG ^)
;
VGLBFL:	DS	1		;Visual mode global search flag    (RUSFLG v)
RUSFLG:	DS	1		;Visual mode re-use search flag    (VGLBFL ^)
BCKFLG:	DS	1		;[FIND] backward flag
BEGFLG:	DS	1		;Search from beginning flag	   (BCKFLG ^)
;
	IF	VPLUS, [
ITRCN2:	DSW	1		;Absolute value of 2d command parameter
	]
;
;	Variables local to expession evaluation routines.
;
	IF	VPLUS, [
EVLGET:	DSW	PTRSIZ
EVLCHR:	DS	1
OVFERR:	DS	1

CHL:	DSW	1		;24-bit expression value, including next byte
				;*** Don't insert anything between CHL & XMINUS ***
	]

XMINUS:	DS	1		;Sign byte for returned by GETNUM *** VPLUS says DON'T MOVE ***
XNMFLG:	DS	1		;Explicit expression encountered  ^ (Don't separate)
XRMINU:	DS	1		;Leading minus sign		  ^ (Order dependent, PROCMD, GETNUM)
;
;	Misc. Command Mode Variables.
;
PRVFLG:	DS	1		;Previous char
VISFLG:	DS	1		;Visual mode flag. (0 = command mode)
PARMFL:	DS	1		;Parm expected so NXCMCH won't POPCMD early
HELPFL:	DS	1		;Stop on "\" flag in HELP
HELPFF:	DB	00		;HELPEX() running?  (Used in FNDTAB())
TEMPFL:	DS	1		;Temporary flag within routines
TEMPW:	DSW	1
TEMPW2:	DSW	1
DECDIV	==	TEMPW		;Temporary for ITOA
DECBLZ	==	TEMPFL		;	"
;
WTERFL:	DS	1		;Write error flag. (0 = No)
SECCNT:	DSW	1		;Sector count used for I/O
JAMFL:	DS	1		;Jamup flag (JAMUP,JAMDWN,BREAK)
RAMFL:	DS	1		;Ram down flag (RAMDWN,RAMUP,BREAK)
RAMBOT:	DSW	1
RAMTOP:	DSW	1
;
	IF	VPLUS, [
LASTCH:	DS	1		;Used by CHKIRC, defined by CHKIRC & STSEAR
PEEKCH:	DS	1		;Look ahead char, local to NXTCHR,CHKIRC
STRPMT:	DB	ESC,0		;Waiting for string close prompt
PMTPTR:	DSW	1		;Pointer to current prompt message
EXCBUF:	DS	1		;Auto execution text register
;
;	Other temporary pointers
;
FNMGET:	DSW	PTRSIZ		;Used by FNFBRK, set in SETFCB
;
;
;	Tracing variables
;
TRACEF:	DS	1		;Trace mode flag (0FFH = on, 0 = off)
TRDENB:	DS	1		;Disable tracing (for tracing past macros)
TRSAVE:	DS	1		;For re-enabling tracing after "hopping" over macro
OLDEXN:	DS	1		;Save previous command buffer # for "?" query
OLDGET:	DSW	PTRSIZ		;Save previous pointer for "?" query
TRVFLG:	DS	1		;For 'V' trace option
TRVGET:	DSW	PTRSIZ		;	"
TRVPUT:	DSW	PTRSIZ		;	"
;
GETDCF:	DS	1		;GETDEC-running flag for NXCMCH
INXFLG:	DS	1		;For forcing .INI to be executed even when bad edit filename
;6ENBRFL	DB	?		;Enable BREAK only after memory management
;RSVAMT:	DSW	1		;Current # bytes to keep free under auto-buffering
;AUTAMT:	DSW	1		;Current # sectors to auto-buffer
;AMTFLG:	DB	0		;Amount to auto-buffer flag: 0=full,
				;1=page-stepping, 2=line-stepping
;
;	Block Shuffle-Swap Variables.
;
NR:	DSW	1		;# bytes remaining to be swapped
NF:	DSW	1		;# free bytes available to be filled
FREEPT:	DSW	1		;-> start of vacated bytes to be freed
	]			;<IF VPLUS>

	.PAGE
;
;	Search operation variables.
;
RPSTEN:	DSW	1		;-> to end of REPLST
COMPFL:	DS	1		;Compress after "S" flag
;
SRFAIL:	DSW	1		;-> begin of last search failure	v
FINDPT:	DSW	1		;== EDTPTR before [FIND] or [REPLACE]	^
SAVEPT:	DSW	1		;Adjusting PTR for "-n_T" and [REPLACE]	^
;
SRCHFL:	DS	1		;Waiting for string chars (0=No)
SRCHCN:	DW	0000		;Count of successful searches
SRCERR:	DS	1		;Search error flag
MANYFL:	DS	1		;|M encountered flag
EOBFLG:	DS	1		;|L matched unterminated line @ EOB
STEMP1:	DSW	1		;Temp for search
STEMP2:	DSW	1		;Temp for search
FNDFLG:	DB	0		;Visual FIND is setup (Multibit, 0=No)
REPLFL:	DB	00		;REPLACE active flag, used in V1
TERMCH:	DS	1		;Terminate char
;
	IF	VPLUS, [
SAVTRM:	DS	1		;Saving TERMCH for |Rr
SAVPFL:	DS	1		;Saving PARMFL for |Rr
BOTCHR:	DS	1		;Saving char preceeding TXTFLR for "-F" cmd
;
;	User accessible, read-only variables.
;
ERRFLG:	DW	0		;.ER:  function result, TRUE if not success
STRNGL:	DSW	1		;
NFOUND:	DSW	1		;.N:  # characters found by SEARCH or
				;     processed by EVLSTR
LFTOVR:	DSW	1		;.RM:  remainder from division in EXPR
RETVAL:	DSW	1		;.RV:  return value from {EM,RC,EP,ES}
	]
	.PAGE
;
;	VEDIT Variables which are part of each VEDIT PLUS edit buffer
;
	IFNOT	VPLUS, [
;
;	Main text pointers
;
TXTEND:	DSW	1		;-> EOF at end of edit buffer
TXTTOP:	DSW	1		;-> end of text window (-> EOF)
EDTPTR:	DSW	1		;Command Edit pointer
TXTBGN:	DSW	1		;-> Beginning of text window
TXTRWF	==	TXTEND
TXTCEL	==	TXTTOP
TXTFLR	==	TXTBGN
;
;
;	The Text Markers must be followed by BLMVEN!
;
PNTTBL:	DS	PNTMAX*2	;Text marker table
BLMVEN:	DSW	1		;Block marker for visual mode
				;Some routines treat this as 11th marker
;
;	Variables for Line and Column display.
;
CNTBFL:	DB	01		;Flag to count all LFs for LINFIL
CNTOUT:	DW	00		;# of LFs written out to disk
LINFIL:	DW	01		;Line number in file
;
;	Misc. needed for each edit buffer window
;
INDPOS:	DB	00		;Indent position
OLDACT:	DSW	1		;OLDACT  (previous TXACTV)
;
;
;	INFCB is in overlay space
;	AUXFCB is in ACTLIN space
;
OUTFLG:	DB	0		;FCB is open flag (0 = No)
OUTUSR:	DS	1		;User # for output FCB
OUTFCB:	DS	9		;Output FCB
	DC	'$$$'
	DB	0,0,0,0
	DS	16
	DB	0
;
RENFLG:	DB	0		;FCB is open flag (0 = No)
RENUSR:	DS	1		;User # for REN FCB
RENFCB:	DS	16		;Rename FCB
	DS	12
	DB	0,0,0,0,0
;
; Reverse FCB
;
REVFLG:	DB	0		;FCB is open flag (0 = No)
REVUSR:	DS	1		;User # for .REV file
REVFCB:	DB	0		;Use default drive
	DS	8		;Use output filename
	DC	'$R$'
	DB	0,0,0,0
	DS	16
	DB	0
	]			;<IFNOT VPLUS>

	.XLIST
	IF	LSBUF ! LSV0 ! LSV1 ! LSV2, [
	.LIST
	]
	.PAGE
;****************************************
;*					*
;*	Visual Mode Variables		*
;*					*
;****************************************
;
;	***** Window Screen writing variables *****
;		Need one copy per Edit Buffer
;
POSTBL:	DSW	PTRSIZ		;-> table of LOGPOS for each line
SCRCNT:	DSW	PTRSIZ		;-> table of flags for each line
SCRFCB:	DSW	PTRSIZ		;-> address table for each window line
;
LINCNT:	DS	1		;Number of lines displayed
UPDLIN:	DB	-1		;Line # from which to begin update
RRWLIN:	DB	-1		;Last line # rewritten (UPDLIN above)
EOFSCR:	DS	1		;EOF reached on screen write (1=YES)
;
;	Screen Line writing variables.
;
EOLFLG:	DB	00		;Need EOL on active line flag (0 = YES)
FAKFLG:	DB	00		;Writing fake line flag (80H = Yes)
WTACFL:	DS	1		;Writing active line flag (0=NO)
;
;	Variables for Cursor positions. (Watch Order!)
;
LOGHOR:	DW	01H		;Logical horizontal cursor pos
CURVER:	DS	1		;Cursor vertical pos
CURHOR:	DS	1		;Cursor horizontal pos
EDTHOR:	DS	1		;Horizontal edit pos
FRCFLG:	DB	00		;Flag to force cursor positioning
;
;	Screen display variables for Active line.
;
ACTCNT:	DS	1		;Total # continuations for active line
CURCNT:	DS	1		;Continuation line cursor is on
FSACSC:	DS	1		;First screen line for active line
LSACSC:	DS	1		;Last screen line for active line
;
;	***** Variables used within Screen write operation *****
;		One copy is enough
;
;
;	Pointers and variables for Active line.
;
TXACTV:	DSW	1		;Pointer into text where active line begins
TXACEN:	DSW	1		;Pointer into text where active line ends
ACTEND:	DSW	1		;End of active line pointer
ACTPNT:	DSW	1		;Edit pointer in active line
HOLSIZ:	DSW	1		;Text "hole" size
ACTSIZ:	DSW	1		;Active line length
;
;	Variables for writing partial Active line.
;
WACURV:	DS	1		;Active line vertical cursor pos
WAPWPN:	DSW	1		;Active line partial write pointer
WALHOR:	DSW	1		;Active line logical hor. pointer
;
;
	IF	VPLUS, [
OPSTCK:	DS	1		;Top-of-stack offset
	DS	1		;Stack size:  STAKSZ * OSTKMX
	DS	STAKSZ * OSTKMX
	] [
OPSTCK:	DB	00		;Stack offset
	DB	5 * 3		;Max stack size
	DS	5 * 3		;Just a 3 level stack
	]
;
;	Misc. Visual Mode Variables.
;
RPTFLG:	DB	00		;Flag that building REPEAT string
CNCLFL:	DB	00		;Flag that [CANCEL] is pending
CCOUNT:	DB	00		;Count of chars written since status check
DSKFUL:	DB	00		;;To cutout MAKSPC when true
WRITOK:	DB	01		;;To cutout EOFCHK()/CHKRVB() when false
VSTACK:	DSW	1		;Stack value in visual mode at VLOOP
VSAVE:	DS	1		;Save 'V' or 'v' overlaid by visual macro's VMCODE

	.XLIST
	IF	LSBUF ! LSV3 ! LSV4, [
	.LIST
	]
	.PAGE
;
;
;	***** Physical Screen writing variables *****
;
STLNFL:	DB	0		;Mode of status line (0 = needs rewrite)
NSCROL:	DB	1		;Number of lines to scroll and direction
LINCHG:	DS	1		;Screen # line change. -1= insert line, 1= delete line.
DISPLN:	DW	00		;Last displayed line #
DISPCL:	DW	00		;Last displayed column #
;
PHYVER:	DS	1		;CRT cursor vertical pos
PHYHOR:	DS	1		;CRT cursor horizontal pos (PHYVER ^)
;
;
; WSTRUC - Window structure table
;
;	   .WWNAME -  Name
;	   .WWMODE -  Mode, -1=empty, 0=command, Buffer# =visual
;	   .WWSPEC -  Specs for re-creating the window
;	   .WWBGLN -  First physical line on screen
;	   .WWENLN -  last physical line on screen
;	   .WWBGCO -  Beginning column of window
;	   .WWENCO -  last physical column on screen
;
;	   .ATTRIB -  Current character attribute
;	   .WWFRAT -  Attribute for text characters
;	   .WWBKAT -  Attribute for screen-erase character
;
;	   .UPDCUR -  Saved window address of update cursor
;	   .UPDSVC -  Saved char/attribute "under" update cursor
;	   .WWNDUP -  Flag that window needs visual mode update
;
;	   .WWUSLN -  Bottommost line in window with text on it
;	   .WINVER -  Virtual vertical pos. (within window)
;	   .WINHOR -  Virtual horizontal pos. (within window)
;	   .LOGPOS -  Position on virtual line
;	   .HZSCBG -  Beginning virtual column (visual mode only)
;
;
;	Parameters depending upon window #
;	Note: order must agree with window structure
;
WWNAME:	DB	'@'		;Start in default window
WWMODE:	DB	-1		;0 = command, Edit-buf # = visual
WWSPEC:	DB	00		;Parent window
	DB	00		;Location - B/T/R/L
	DB	00		;Size
WWBGLN:	DS	1
WWENLN:	DS	1
WWBGCO:	DB	0
WWENCO:	DS	1
;
ATTRIB:	DS	1		;Current character attribute
WWFRAT:	DS	1		;Foreground attibute for normal characters
WWBKAT:	DS	1		;Background attibute for empty screen
;
UPDCUR:	DW	0000		;Position of update cursor
UPDSVC:	DSW	1		;Character/attribute "under" update cursor
WWNDUP:	DB	0		;Flag that update needed
;
WWUSLN:	DS	1		;Number of window lines containing text

WINVER:	DS	1		;Vertical screen write pos. in window
WINHOR:	DS	1		;Horizontal screen write pos. in window
				;Horizontal screen write pos. in VIRTUAL window
				;Needed to support horizontal scrolling
LOGPOS:	DSW	1		;Logical line position
HZSCBG:	DB	0		;Virtual column for begin of displayed window
				;(HZSCEN below)
;
;	Parameters calculated from above parameters at ADJWIN()
;
HZSCEN:	DS	1		;Virtual column for end of display window
				;(HZSCBG above)  Not needed in structure
;
WWNLIN:	DS	1		;Number of lines in window
WWNLI1:	DS	1		;One less
WWLLEN:	DS	1		;Number of columns in window
	DB	00		;Someone may need 16 bit value
WWLLE1:	DS	1
WWPGSZ:	DS	1
CRVTTP:	DS	1
CRVTBT:	DS	1
;
;	MISC storage
;
SAVWIN:	DSW	1		;Save area for WINVER and WINHOR
;
	IF	WINDOO, [
COMMWI:	DS	1		;Window to use for command mode
WWUPFL:	DB	0		;Flag that only updating window
WWZMFL:	DB	0		;Flag that window zoomed to full screen
WIHLFL:	DB	0		;Flag to highlight WINDOW message
HILIWW:	DB	0		;# of window currently highlighted
SCNDUP:	DB	00		;Some window(s) need update (see WWNDUP)
;
WWWMSG:	DC	'WINDOW ' [00]
	]
;
	.XLIST
	IF	LSMAIN ! LSCUST, [
	.LIST
	]
	.PAGE
;
;	Tables which are customized by INSTALL.
;
CHRTBL:
KEYUCF:	DB	01		;Ignore UC/LC in control sequences
;
	IFNOT	IBMPC ! TIPC, [

	.IFE	DEBUG-2, [
BRDRCH:	DB	'-'
	] [
BRDRCH:	DB	' '		;Fill char for status line
	]

BRCHHR:	DB	'='		;Horizontal char for window border
BRCHVT:	DB	'|'		;Vertical char for window border
BRUPLF:	DB	'='		;Upper/left char for window
BRUPRT:	DB	'='		;Upper/right char for window
BRLWLF:	DB	'='		;Lower/left char for window
BRLWRT:	DB	'='		;Lower/right char for window
CONTCH:	DB	'-' + 80H	;Continuation char
	] [
BRDRCH:	DB	' '		;Fill char. for status line
BRCHHR:	DB	205		;Horizontal char for window border
BRCHVT:	DB	179		;Vertical char for window border
BRUPLF:	DB	213		;Upper/left char for window
BRUPRT:	DB	209		;Upper/right char for window
BRLWLF:	DB	198		;Lower/left char for window
BRLWRT:	DB	207		;Lower/right char for window
CONTCH:	DB	'-' + 80H	;Continuation char
	]

AUXESC:	DB	ESC		;Auxiliary escape char
ITRMCL:	DB	'['		;Iteration macro char
ITRMCR:	DB	']'
WILDCH:	DB	'|'		;Wildcard char
;
CURSCH:	DB	'_'		;Underline cursor char
FILLCH:	DB	SPACE		;Character to fill screen with
TABFIL:	DB	SPACE		;Char. used to fill tabs
EOLCHR:	DB	SPACE		;Char at end of lines
;
;	The tables TABPOS, SWTBL & PRMTBL are considered contiguous by
;	buffer initializing code.
;
TINIBG:	DB	1		;Need a "1" before TABPOS for indent code
TABPOS:	DB	  9, 17, 25, 33, 41, 49, 57, 65, 73, 81
	DB	 89, 97,105,113,121,129,137,145,153,161
	DB	169,177,185,193,201,209,217,225,233,241
	DB	249,254
	DB	0FFH,00		;Tab position table fence (34 bytes long)
;
;	Table of Switches.	0=>NO, 1=>YES   Connected to TABPOS & PRMTBL
;
SWTBL:	DB	FALSE		;EXPTSW
	DB	2		;ATBFSW
	DB	FALSE		;AUTIND
	DB	TRUE		;PTAFSW
	DB	TRUE		;SRCNSW
	DB	FALSE		;MSDOSW
	DB	00		;SWCCNV
	DB	00		;COLSW
	DB	00		;ATTSW
	DB	00		;GLOBSW
	DB	00		;JUSTSW
SWTEND:
SWCHNM	=	SWTEND - SWTBL
	DB	00		;Reserved bytes for INSTALL
	DB	00
	DB	00
	DB	00		;Must be 15 bytes to correspond w/buffer header
;
;	Table of Parameter Values.	Connected to SWTBL for buffer init
;
PRMTBL:	DB	01		;CURTYP
	DB	35		;BLNKRT
	DB	04		;INDINC
	DB	00		;UCNVSW
	DB	';'		;CMNTCH
	DB	00		;CRDELY
	DB	00		;WRAPCL

	IF	CRTVRS, [
	DB	01		;BIT7AL
	] [
	DB	03
	]

	DB	01		;HOROPT
	DB	210		;HZSCLN
	DB	20		;HZSCIC
PRMEND:
PARMNM	=	PRMEND - PRMTBL
TININM	=	PRMEND - TINIBG
;
	IF	CCPM86, [
DISTYP:	DB	3		;CCP/M-86 temporarily
	] [
	DB	00
	]

	DB	00
	DB	00
	DB	00
;
;
; Print parameter table
;
PRNTBL:
PPXPL:	DB	66		;Physical lines/page
PPLNCN:	DB	60		;Printed lines/page
PPLFMR:	DB	12		;Left margin when printing
PPFFFL:	DB	01		;Use Form/feed on printer
;
PRNEND:
PRNMNM	=	PRNEND - PRNTBL
;
;SIZTBL:	DW	28*1024		;Lower memory boundary for light usage
;	DW	12*1024		;Lower boundary for medium usage
;
ISPARE:	DW	INISPR		;Spare memory space to leave initially
;	DW	(INISPR*2)/3	;For medium usage
;	DW	INISPR/3	;For heavy usage
;
	IF	P8086,[
MINSEC:	DW	96		;Minimum sectors to write for NEXFIL
	] [
MINSEC:	DW	64		;Minimum sectors to write for NEXFIL
	]
;MINSEG:	DW	64		;Minumum sectors to write for full auto-buffering
;	DW	(64*2)/3	;Minimum sectors for medium usage
;	DW	64/3		;Minumum sectors for heavy usage
;
;MINPAG:	DW	32		;Minumum sectors to write when page-stepping
;	DW	(32*2)/3	;Page-stepping under medium usage
;	DW	32/3		;Page-stepping under heavy usage
;
;MINLIN:	DW	12		;Minumum sectors to write when line-stepping
;	DW	(12*2)/3	;Line-stepping under medium usage
;	DW	12/3		;Line-stepping under heavy usage
;
	IFNOT	P8086, [
INIDV0:	DB	01		;Look for special files on def drive on def user only
INIDVA:	DB	01		;Look for HELP/VEDIT.INI on drive 01 = "A:"
	] [
	IF	DEBUG, [
INIDV0:	DB	01		;Look for special files on def drive on def user only
INIDVA:	DB	3
	] [
INIDV0:	DB	02		;Look for special files on def drive on def user only
INIDVA:	DB	00
	]
	]
INIFLG:	DB	TRUE		;Look for VEDIT.INI
;
;
;
DLYVAL:	DB	76		;Delay constant for 1ms delay
	.IFE	DEBUG-2, [
INSFLG:	DB	TRUE		;Begin in Insert mode flag
BGCMSW:	DB	TRUE		;Begin in Command mode
	] [
INSFLG:	DB	FALSE		;Begin in Insert mode flag
BGCMSW:	DB	FALSE		;Begin in Command mode
	]
;
	IF	IBMPC, [
KBINSW:	DB	00		;0=ROM, 1=System, 2=MP/M
	] [
KBINSW:	DB	01		;0=ROM, 1=System, 2=MP/M
	]
;
	IF	DEBUG, [
HLPCNT:	DB	0
	] [
HLPCNT:	DB	3		;Help prompt downcounter
	]
;
;
;	Screen variables set by INSTALL
;
PYLINE:	DB	NLINES
;
	IF	DEBUG, [
ACRVTTP:	DB	2
ACRVTBT:	DB	NLINES-2
APGSZ:		DB	NLINES-2
STLINE:		DB	1	;Line for status line
	] [
	IF	IBMPC, [
ACRVTTP:	DB	3
ACRVTBT:	DB	NLINES-4
APGSZ:		DB	20
STLINE:		DB	1	;Line for status line
	] [
ACRVTTP:	DB	3
ACRVTBT:	DB	NLINES-4
APGSZ:		DB	NLINES-4
STLINE:		DB	1	;Line for status line
	]
	]
;
PYLLEN:	DW	LINLEN
;
	IFNOT	P8086, [
	.IFE	DEBUG-2, [
SFORAT:	DB	0		;Foreground attribute for normal characters
SBCKAT:	DB	0		;Background attribute for empty screen
SSTAAT:	DB	0		;Attribute for status line
SSMSAT:	DB	1		;Attribute for status line messages
SBRDAT:	DB	0		;Attribute for window border
SBMSAT:	DB	1		;Attribute for WINDOW message
CURATR:	DB	00		;;Attribute for MM cursor
	] [
SFORAT:	DB	0		;Foreground attribute for normal characters
SBCKAT:	DB	0		;Background attribute for empty screen
SSTAAT:	DB	1		;Attribute for status line
SSMSAT:	DB	1		;Attribute for status line messages
SBRDAT:	DB	0		;Attribute for window border
SBMSAT:	DB	1		;Attribute for WINDOW message
CURATR:	DB	00		;;Attribute for MM cursor
	]
	] [
	IF	IBMPC, [
SFORAT:	DB	2		;Foreground attribute for normal characters
SBCKAT:	DB	2		;Background attribute for empty screen
SSTAAT:	DB	70H		;Attribute for status line
SSMSAT:	DB	70H		;Attribute for status line messages
SBRDAT:	DB	3		;Attribute for window border
SBMSAT:	DB	30H		;Attribute for WINDOW message
CURATR:	DB	00		;;Attribute for cursor
	] [
	IF	TANDY, [
SFORAT:	DB	0AH		;Foreground attribute for normal characters
SBCKAT:	DB	0AH		;Background attribute for empty screen
SSTAAT:	DB	8AH		;Attribute for status line
SSMSAT:	DB	8AH		;Attribute for status line messages
SBRDAT:	DB	0AH		;Attribute for window border
SBMSAT:	DB	8AH		;Attribute for WINDOW message
CURATR:	DB	8AH		;;Attribute for cursor
	] [
	IF	TIPC, [
SFORAT:	DB	0FH		;Foreground attribute for normal characters
SBCKAT:	DB	0FH		;Background attribute for empty screen
SSTAAT:	DB	1FH		;Attribute for status line
SSMSAT:	DB	1FH		;Attribute for status line messages
SBRDAT:	DB	0FH		;Attribute for window border
SBMSAT:	DB	1FH		;Attribute for WINDOW message
CURATR:	DB	00H		;;Attribute for cursor
	] [
	IF	DECRAIN, [
SFORAT:	DB	0EH		;Foreground attribute for normal characters
SBCKAT:	DB	0EH		;Background attribute for empty screen
SSTAAT:	DB	0FH		;Attribute for status line
SSMSAT:	DB	0FH		;Attribute for status line messages
SBRDAT:	DB	0EH		;Attribute for window border
SBMSAT:	DB	0FH		;Attribute for WINDOW message
CURATR:	DB	0BH		;;Attribute for cursor
	] [
SFORAT:	DB	0		;Foreground attribute for normal characters
SBCKAT:	DB	0		;Background attribute for empty screen
SSTAAT:	DB	1		;Attribute for status line
SSMSAT:	DB	1		;Attribute for status line messages
SBRDAT:	DB	0		;Attribute for window border
SBMSAT:	DB	1		;Attribute for WINDOW message
CURATR:	DB	00		;;Attribute for cursor
	]
	]
	]
	]
	]
;
	IF	MEMVRS, [
MMLLEN:	DW	LINLEN
SCRBAS:	DW	VRAM
FSTCGA:	DB	0		;Fast CGA/EGA board
	]
	.PAGE
;
;	ACTBUF space used for initialization and AUXFCB.
;
ACTLF:	DB	LF		;Active line LF prefix
;
ACTBUF:
;
INIMS1:	DC	'Copyright (C) 1987 CompuView Products, Inc.' [CR] [LF] [CR] [LF]
;
	IF	VPLUS, [
USRMSG:	DC	'Welcome to VEDIT PLUS '
	] [
USRMSG:	DC	'Welcome to VEDIT '
	]
	IF	IBMPC, [
	DC	'on the IBM PC'
	]
	DB	00

	ORG	USRMSG + 65
;
	DC	'Copyright (C) 1979,1987 by Theodore Green.'
	DC	DATE [ESC]
;
VDTMSG:	DC	'VEDIT.INI' [CR] [00]
;
	IF	MSDOS, [
;6	DB	0FFH, 4 DUP (0)	;Extended FCB (overlays AUXFLG & AUXUSR)
	]
;
AUXFLG:	DS	1		;FCB is open flag (0 = No)
AUXUSR:	DS	1		;Auxiliary-file user #
AUXFCB:	DS	33		;Auxiliary FCB for ED, EG, RL and RS commands

	IF	MSDOS, [
	DS	4		;For random block i/o
	DS	2		;Offset to filename in path string
	DS	DIRLEN
	]

AUXPUT:	DSW	1		;For HELP processing
AUXGET:	DSW	1		;	"
;
	IF	VPLUS, [
ROOTDF:	DS	1		;For optimizing special-file searching
ROOTSP:	DS	1		;	"
	]
;
;	Temp storage for EA command
;
EASAV1:	DSW	1
EASAV2:	DSW	1
INSCH	==	EASAV1		;For INSCHR routine
;
;	Pointers used for EG and EL commands.
;
EGINFL:	DS	1		;Currently inserting flag. (0 = No)
EGEOFF:	DS	1		;End of File Read found flag. (0 = No)
EGBGLN:	DSW	1		;Line # to begin insert at
EGENLN:	DSW	1		;Line # to end insert at
EGLEND:	DSW	1		;Line # at end of sector
EGBGPT:	DSW	1		;Begin pointer for insert
ELFLG:	DS	1		;Is it EL command
ELLNFL:	DS	1		;Type EL line # flag
;
;
;
	ORG	ACTBUF + ACTLEN
	DB	CR,LF		;Make sure ACTBUF always ends in CRLF
;
	IFNOT	P8086, [	;Following must be split off when xlating
;	This space can be used for a variable sized PATCH space
;	To do so, just move up the pointer at TXTBAS
;
;6	DB	LF		;LF needed just before TXTBUF
TXTBUF:	DB	01AH		;Rest of Memory upto BDOS for text buffer
;
;	The addresses used by INSTALL are saved here, since they
;	are of no use to VEDIT.
;
ADDTBL:	DW	VRSNUM
;
	IF	CRTVRS, [
	DW	ADDLED		;Address of CRT escape sequences
	]

	DW	KEYMSG		;Address of table of key messages, codes
	DW	KEYTBL		;Address of begin of key decode table
KEYTEN:	DW	KEYTBN		;Address past end of key decode table
	DW	TABPOS		;Address of Tab table
	DW	CHRTBL		;Address of Character table

	DW	PRNTBL		;Address of print parameter table
	DW	SWTBL		;Address of switch table
	DW	PRMTBL		;Address of parameter table
	DW	ISPARE		;Address of memory size values
	DW	PYLINE		;Address of screen parameter table
;
	IF	MEMVRS, [
	DW	SCRINI		;Address of screen init code
	]
	IF	PICVRS, [
	DW	VSTART		;DUMMY address for INSTALL
	]
	DW	USRMSG		;Address of user message
;
	IF	WORDP, [
	DW	PXHEAD
	DW	PXSUBH
	DW	PXFOOT
	DW	PXHEND
	]
;
; KEYMSG - Table of edit functions for INSTALL.
;	   This is ALL INSTALL needs to know about them.
;
KEYMSG:	DC	'CURSOR UP' [00] 'CU' [00]
	DC	'CURSOR DOWN' [00] 'CD' [00]
	DC	'CURSOR RIGHT' [00] 'CR' [00]
	DC	'CURSOR LEFT' [00] 'CL' [00]
	DC	'BACK TAB' [00] 'BT' [00]
	DC	'TAB CURSOR' [00] 'TC' [00]
	DC	'ZIP' [00] 'ZP' [00]
	DC	'LINE TOGGLE' [00] 'LT' [00]
	DC	'NEXT LINE' [00] 'NL' [00]
	DC	'SCROLL UP' [00] 'SU' [00]
	DC	'SCROLL DOWN' [00] 'SD' [00]
	DC	'SCROLL RIGHT' [00] 'SR' [00]
	DC	'SCROLL LEFT' [00] 'SL' [00]
	DC	'PREVIOUS WORD' [00] 'PW' [00]
	DC	'NEXT WORD' [00] 'NW' [00]
	DC	'PREVIOUS PARA' [00] 'PP' [00]
	DC	'NEXT PARA' [00] 'NP' [00]
	DC	'PAGE UP' [00] 'PU' [00]
	DC	'PAGE DOWN' [00] 'PD' [00]
	DC	'SCREEN TOGGLE' [00] 'ST' [00]
	DC	'INSERT' [00] 'IN' [00]
	DC	'DELETE' [00] 'DC' [00]
	DC	'BACKSPACE' [00] 'BS' [00]
	DC	'DEL PREVIOUS WORD' [00] 'DP' [00]
	DC	'DEL NEXT WORD' [00] 'DN' [00]
	DC	'ERASE EOL' [00] 'EE' [00]
	DC	'ERASE LINE' [00] 'EL' [00]
	DC	'UNDO' [00] 'UN' [00]
	DC	'TAB CHARACTER' [00] 'TB' [00]
	DC	'NEXT CHAR LITERAL' [00] 'LC' [00]
	DC	'REPEAT' [00] 'RE' [00]
	DC	'FIND' [00] 'FI' [00]
	DC	'REPLACE' [00] 'RP' [00]
	DC	'CANCEL' [00] 'CA' [00]
	DC	'INDENT' [00] 'ID' [00]
	DC	'UNDENT' [00] 'UD' [00]
	DC	'FORMAT PARAGRAPH' [00] 'FP' [00]
	DC	'MACRO' [00] 'VM' [00]
	DC	'VISUAL EXIT' [00] 'EX' [00]
	DC	'VISUAL ESCAPE' [00] 'ES' [00]
	DC	'DEFINE' [00] 'DF' [00]
	DC	'HELP' [00] 'HE' [00]
	DC	'GOTO' [00] 'GO' [00]
	DC	'FILE' [00] 'FL' [00]
	DC	'BLOCK' [00] 'BL' [00]
	DC	'PRINT' [00] 'PR' [00]
	DC	'WINDOW' [00] 'WI' [00]
	DC	'USER' [00] 'US' [00]
	DC	'MISC' [00] 'MI' [00]
	DB 	00,00
;
; KEYTBL - Variable sized table for keyboard decode and keystroke macros
;	   It is copied to high memory during VEDIT's initialization
;
;	Default keyboard layout
;
	IF	PICVRS & DEVLOP, [
KEYTBL:	DB	KFF				;Need this here
	DB	CR,      KFF, 'R','T',KFF
	DB	140,     KFF, 'C','U',KFF
	DB	141,	 KFF, 'C','D',KFF
	DB	144,     KFF, 'C','R',KFF
	DB	143,     KFF, 'C','L',KFF
	DB	134,     KFF, 'B','T',KFF
	DB	142,     KFF, 'T','C',KFF
	DB	145,     KFF, 'Z','P',KFF
	DB	139,     KFF, 'N','L',KFF
;
	DB	152,     KFF, 'S','U',KFF
	DB	153,     KFF, 'S','D',KFF
	DB	132,     KFF, 'S','R',KFF
	DB	131,     KFF, 'S','L',KFF
	DB	161,     KFF, 'P','W',KFF
	DB	162,     KFF, 'N','W',KFF
;
	DB	155,     KFF, 'P','P',KFF
	DB	156,     KFF, 'N','P',KFF
	DB	137,     KFF, 'P','U',KFF
	DB	138,     KFF, 'P','D',KFF
	DB	BS,      KFF, 'B','S',KFF
	DB	DEL,     KFF, 'D','C',KFF
	DB	135,     KFF, 'E','E',KFF
	DB	CTRLX,   KFF, 'E','L',KFF
	DB	158,	 KFF, 'D','P',KFF
	DB	159,     KFF, 'D','N',KFF
	DB	136,     KFF, 'U','N',KFF
;
	DB	TAB,	 KFF, 'T','B',KFF
	DB	177,     KFF, 'L','C',KFF
	DB	CTRLK,   KFF, 'I','N',KFF
	DB	164,     KFF, 'R','E',KFF
	DB	166,     KFF, 'I','D',KFF
	DB	165,     KFF, 'U','D',KFF
;
	DB	ESC, 'B',KFF, 'B','L',KFF
	DB	ESC, 'G',KFF, 'G','O',KFF
	DB	ESC, 'P',KFF, 'P','R',KFF
	DB	ESC, 'W',KFF, 'W','I',KFF
	DB	ESC, 'D',KFF, 'F','L',KFF
	DB	ESC, 'U',KFF, 'U','S',KFF
	DB	ESC, 'Y',KFF, 'M','I',KFF
;
	DB	149,     KFF, 'F','I',KFF
	DB	150,     KFF, 'R','P',KFF
	DB	151,     KFF, 'C','A',KFF
	DB	ESC, 'C',KFF, 'C','A',KFF
	DB	157,     KFF, 'F','P',KFF
	DB	ESC, 'K',KFF, 'D','F',KFF
;
	DB	167,     KFF, 'E','X',KFF
	DB	ESC, ESC,KFF, 'E','S',KFF
	DB	173,     KFF, 'V','M',KFF
	DB	133,     KFF, 'H','E',KFF
;
	DB	175,     KFF, 'G','O','H',00,KFF
	DB	176,     KFF, 'G','O','Z',00,KFF
	DB	171,     KFF, 'G','O','S',00,KFF
	DB	172,     KFF, 'G','O','J',00,KFF
	DB	168,     KFF, 'B','L','C',00,KFF
	DB	169,     KFF, 'B','L','M',00,KFF
	DB	170,     KFF, 'B','L','I',00,KFF
	DB	174,     KFF, 'P','R','B',00,KFF
	DB	128,	 KFF, 'M','I','I',00,KFF
	DB	129,	 KFF, 'M','I','O',00,KFF
;
	] [
;
KEYTBL:	DB	KFF				;Need this here
	DB	CR,      KFF, 'R','T',KFF
	DB	CTRLE,   KFF, 'C','U',KFF
	DB	CTRLC,   KFF, 'C','D',KFF
	DB	CTRLD,   KFF, 'C','R',KFF
	DB	CTRLS,   KFF, 'C','L',KFF
	DB	CTRLQ,   KFF, 'B','T',KFF
	DB	CTRLB,   KFF, 'T','C',KFF
	DB	CTRLG,   KFF, 'Z','P',KFF
	DB	LF,      KFF, 'N','L',KFF
;
	DB	CTRLR,   KFF, 'S','U',KFF
	DB	CTRLV,	 KFF, 'S','D',KFF
	DB	CTRLY,   KFF, 'S','R',KFF
	DB	CTRLT,   KFF, 'S','L',KFF
	DB	CTRLA,   KFF, 'P','W',KFF
	DB	CTRLF,   KFF, 'N','W',KFF
;
	DB	ESC, 'W',KFF, 'P','P',KFF
	DB	ESC, 'X',KFF, 'N','P',KFF
	DB	CTRLW,   KFF, 'P','U',KFF
	DB	CTRLX,   KFF, 'P','D',KFF
	DB	CTRLN,   KFF, 'I','N',KFF
	DB	DEL,     KFF, 'D','C',KFF
	DB	BS,      KFF, 'B','S',KFF
	DB	ESC, 'A',KFF, 'D','P',KFF
	DB	ESC, 'F',KFF, 'D','N',KFF
	DB	CTRLL,   KFF, 'E','E',KFF
	DB	CTRLK,   KFF, 'E','L',KFF
	DB	CTRLU,   KFF, 'U','N',KFF
;
	DB	TAB,	 KFF, 'T','B',KFF
	DB	ESC, 'Q',KFF, 'L','C',KFF
	DB	ESC, 'R',KFF, 'R','E',KFF
	DB	ESC, '2',KFF, 'F','I',KFF
	DB	ESC, '3',KFF, 'R','P',KFF
	DB	'^','Z', KFF, 'C','A',KFF
;
	DB	CTRLP,   KFF, 'I','D',KFF
	DB	CTRLO,   KFF, 'U','D',KFF
	DB	ESC, 'J',KFF, 'F','P',KFF
;
	DB	ESC, 'C',KFF, 'V','M',KFF
	DB	ESC, 'E',KFF, 'E','X',KFF
	DB	ESC, ESC,KFF, 'E','S',KFF
	DB	ESC, 'D',KFF, 'D','F',KFF
	DB	ESC, '?',KFF, 'H','E',KFF
;
	DB	ESC, 'G',KFF, 'G','O',KFF
	DB	ESC, '0',KFF, 'F','L',KFF
	DB	ESC, 'B',KFF, 'B','L',KFF
	DB	ESC, 'P',KFF, 'P','R',KFF
	DB	ESC, '9',KFF, 'W','I',KFF
	DB	ESC, 'U',KFF, 'U','S',KFF
	DB	ESC, 'M',KFF, 'M','I',KFF
;
	DB	ESC, '1',KFF, 'C','A',KFF
	DB	ESC, 'H',KFF, 'G','O','H',00,KFF
	DB	ESC, 'Z',KFF, 'G','O','Z',00,KFF
	DB	ESC, '7',KFF, 'G','O','S',00,KFF
	DB	ESC, '8',KFF, 'G','O','J',00,KFF
	DB	ESC, '4',KFF, 'B','L','C',00,KFF
	DB	ESC, '5',KFF, 'B','L','M',00,KFF
	DB	ESC, '6',KFF, 'B','L','I',00,KFF
	]
;
KEYTBN:
;
;	Temporary storage for headers
;
	IF	WORDP, [
PXHEAD:	DC	[00] [00] 'Page #' [00]
PXSUBH:	DC	[00] [00] [00]
PXFOOT:	DC	[00] '-#-' [00] [00]
PXHEND:
	]
	]			;<IFNOT P8086> (Must be split off before xlation)

