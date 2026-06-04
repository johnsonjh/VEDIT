# VEDIT terminal-table format (`*CRT.TBL*` / `install.ini`)

## Container

```
offset  bytes  field
------  -----  ---------------------------------------------------------------
  0      10    magic   "*CRT.TBL*\0"
 10      var   count   decimal ASCII, NUL-terminated  (DOS "64", CP/M-86 "60")
 var     var   namelen decimal ASCII, NUL-terminated  = byte length of the name
                       block that follows  (DOS "1103", CP/M-86 "1004")
 H       NL    names   `count` NUL-terminated terminal names, in menu order
 H+NL    ...   records `count` fixed CRT-capability tables, one per name
```

`H` = length of the three NUL-terminated header fields (18 for both shipped
files: `"*CRT.TBL*\0"`=10, `"64\0"`=3, `"1103\0"`=5). Skip three
NUL-terminated fields, then `namelen` is the size of the name block.
The records begin immediately after.

## Per-terminal record (the CRT table)

Per `crt.asm` and confirmed byte-for-byte via `install.ini` the first DOS
record (`ACT-IV`) is `01 14 00 00 00 00 00` ... = `crt.asm:57`
`DB 1,14H,0,0,0,0,0 ;CURSOR LEADIN`, then table is 16 escape-sequence slots
of 7 bytes, plus a 3-byte cursor-addressing field inserted after the third:

```c
/* one escape-sequence slot: a length byte + up to 6 sequence bytes (7 total). */
typedef struct {
  vbyte len;  /* number of valid bytes in seq[]; see CLEAR_SCREEN note */
  vbyte seq[6];
} crt_seq;

/* one terminal's capability table (the crt.asm "CRT.TBL" record). */
typedef struct {            /* off  crt.asm field                             */
  crt_seq cursor_leadin;    /*   0  before a cursor-position address          */
  crt_seq cursor_between;   /*   7  emitted between the row and the column    */
  crt_seq cursor_leadout;   /*  14  after a cursor-position address           */
  vbyte   addoff[3];        /*  21  cursor-address encoding (see below)       */
  crt_seq clear_screen;     /*  24  CLEAR SCREEN (len high bit = flag, below) */
  crt_seq erase_to_eos;     /*  31  erase to end of screen                    */
  crt_seq erase_to_eol;     /*  38  erase to end of line                      */
  crt_seq insert_line;      /*  45                                            */
  crt_seq delete_line;      /*  52                                            */
  crt_seq forward_scroll;   /*  59                                            */
  crt_seq reverse_scroll;   /*  66                                            */
  crt_seq begin_reverse;    /*  73  begin reverse video                       */
  crt_seq end_reverse;      /*  80  end reverse video                         */
  crt_seq enable_status;    /*  87  enable status line                        */
  crt_seq disable_status;   /*  94  disable status line                       */
  crt_seq enter_visual;     /* 101  enter visual mode                         */
  crt_seq exit_visual;      /* 108  exit visual mode                          */
} crt_table;                /* 115 bytes (= TBLLEN in utl/1.40/vedset.asm)    */
```

### Field notes (overloaded bytes! decode and never expose raw)

- **`crt_seq.len`** is a count of `seq[]` bytes (0 = capability absent). A
  sequence is emitted by sending `len` bytes of `seq`.
- **`clear_screen.len` high bit** is overloaded: when set, "do not pre-clear the
  screen when repainting" (`crt.asm:41-43`), expose as *(sequence)* + *(boolean
  flag)*, never as one raw byte.
- **`addoff[3]`** = `[mode, row_bias, col_bias]`. `mode` selects how the cursor
  address is encoded (binary vs. ASCII, row/col order), `row_bias`/`col_bias`
  are added to the coordinates (e.g. `0x20`, classic VT52 bias. `mode` packs
  sub-fields, so decode it; the biases are plain bytes.

### Record detail

The 2.33 records are **115 bytes**, identical to 1.40 (see `TBLLEN` in
`vedset.asm`).  Verified by alignment at stride 115, the first record's
`CURSOR LEADIN` reads `01 14 00` (ACT-IV `1,14H`), followed by `02 1B 59`
four times (ESC `Y` for the ADDS terminals). This matches `crt.asm` exactly.
All 64 records yield a well-formed leadin, confirming that each record is
exactly the 115-byte `crt_table` above and that there is no per-record header.

The binary section is `count * 115` plus a trailer; for DOS, `64*115 = 7360`
of 7583 leaves 223 after the last record.  This 115-byte record is also the
format of the editor `ADDLED` table (see `src/veditt3.asm` for the same
16 sequence slots plus ADDOFF).  Installing a terminal is a direct copy.

## Notes

The tools are **not** a re-creation of `INSTALL` version-gated menus and UI.
The `vcfg` and `vintmod` expose every field a table defines, regardless of
the target build, so you can set any capability even if your build would
ever use it.

Also note the descriptor (`tfield` in `table.c`) carries, per field name,
byte offset, kind (`FK_BYTE` | `FK_WORD` | `FK_SEQ` | `FK_ADDOFF`), overload
flags, and a help string.  The overloads are decoded rather than shown raw:
the `clear_screen` sequence's count high bit (`FF_CLRBIT`, via `seq_clr`),
the `addoff` mode byte, and the `INSTALL`-specific version-flags word which
is a bitfield (edited as named bits) and the conditional `ADDLED` and `SCRINI`
slots (present per build type, `flags & CRT`).

Everything else is a single-purpose field and is freely editable.
