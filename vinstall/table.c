/*
 * VINSTALL - table.c
 * Copyright (c) 2026 Jeffrey H. Johnson <johnsonjh.dev@gmail.com>
 * SPDX-License-Identifier: MIT-0
 * scspell-id: d4fa6526-6290-11f1-80d0-80ee73e9b8e7
 */

/******************************************************************************/

#include <string.h>
#define TABLE_OWNER
#include "table.h"

/******************************************************************************/

/* ---- the 115-byte terminal capability table (install.ini record) ---------------- */
static tfield crt_fields[] = {
  { "cursor_leadin",    0, FK_SEQ,    0,         "before a cursor address"           },
  { "cursor_between",   7, FK_SEQ,    0,         "between the row and the column"    },
  { "cursor_leadout",  14, FK_SEQ,    0,         "after a cursor address"            },
  { "addoff",          21, FK_ADDOFF, 0,         "cursor encoding [mode,row,col]"    },
  { "clear_screen",    24, FK_SEQ,    FF_CLRBIT, "clear screen (hi-bit=no pre-clear)"},
  { "erase_eos",       31, FK_SEQ,    0,         "erase to end of screen"            },
  { "erase_eol",       38, FK_SEQ,    0,         "erase to end of line"              },
  { "insert_line",     45, FK_SEQ,    0,         "insert line"                       },
  { "delete_line",     52, FK_SEQ,    0,         "delete line"                       },
  { "forward_scroll",  59, FK_SEQ,    0,         "forward scroll"                    },
  { "reverse_scroll",  66, FK_SEQ,    0,         "reverse scroll"                    },
  { "begin_reverse",   73, FK_SEQ,    0,         "begin reverse video"               },
  { "end_reverse",     80, FK_SEQ,    0,         "end reverse video"                 },
  { "enable_status",   87, FK_SEQ,    0,         "enable status line"                },
  { "disable_status",  94, FK_SEQ,    0,         "disable status line"               },
  { "enter_visual",   101, FK_SEQ,    0,         "enter visual mode"                 },
  { "exit_visual",    108, FK_SEQ,    0,         "exit visual mode"                  }
};
tdesc CRT_TABLE = { "crt", 115, crt_fields, 17 };

/******************************************************************************/

/* ---- the editor's screen-parameter block (PYLINE) ---------------------- */
static tfield pyline_fields[] = {
  { "nlines",       0, FK_BYTE, 0, "number of screen lines (NLINES)"        },
  { "vtop",         1, FK_BYTE, 0, "visual window top row (ACRVTTP)"        },
  { "vbottom",      2, FK_BYTE, 0, "visual window bottom row (ACRVTBT)"     },
  { "pagesize",     3, FK_BYTE, 0, "page size (APGSZ)"                      },
  { "statline",     4, FK_BYTE, 0, "status-line row (STLINE)"               },
  { "linelen",      5, FK_WORD, 0, "line length (PYLLEN)"                   },
  { "text_attr",    7, FK_BYTE, 0, "text foreground attribute (SFORAT)"     },
  { "erase_attr",   8, FK_BYTE, 0, "screen-erase background (SBCKAT)"       },
  { "status_attr",  9, FK_BYTE, 0, "status line attribute (SSTAAT)"         },
  { "statmsg_attr",10, FK_BYTE, 0, "status message attribute (SSMSAT)"      },
  { "border_attr", 11, FK_BYTE, 0, "window border attribute (SBRDAT)"       },
  { "bordmsg_attr",12, FK_BYTE, 0, "border message attribute (SBMSAT)"      },
  { "cursor_attr", 13, FK_BYTE, 0, "memory-mapped cursor attribute (CURATR)"}
};
tdesc PYLINE_TABLE = { "pyline", 14, pyline_fields, 13 };

/******************************************************************************/

/* ---- edit-switch settings (SWTBL, ES commands; 0=off/1=on) ------------ */
/* lrbcsw sits in SWTBL's reserved tail (offset 11), past the SWCHNM=11    */
/* switches the ES command and CompuView INSTALL know about.  Bits:        */
/* 1 = don't use CP/M 3+ byte counts when reading, 2 = don't set them when */
/* writing, 4 = ISX convention (# bytes unused) instead of DOS Plus (used) */
static tfield swtbl_fields[] = {
  { "exptsw",  0, FK_BYTE, 0, "expand tabs to spaces"        },
  { "atbfsw",  1, FK_BYTE, 0, "auto buffer fill"             },
  { "autind",  2, FK_BYTE, 0, "auto indent"                  },
  { "ptafsw",  3, FK_BYTE, 0, "put-after switch"             },
  { "srcnsw",  4, FK_BYTE, 0, "search options"               },
  { "msdosw",  5, FK_BYTE, 0, "MS-DOS file convention"       },
  { "swccnv",  6, FK_BYTE, 0, "case-conversion switch"       },
  { "colsw",   7, FK_BYTE, 0, "column mode"                  },
  { "attsw",   8, FK_BYTE, 0, "attribute mode"               },
  { "globsw",  9, FK_BYTE, 0, "global mode"                  },
  { "justsw", 10, FK_BYTE, 0, "justify mode"                 },
  { "lrbcsw", 11, FK_BYTE, 0, "CP/M 3+ byte count: 1=no read, 2=no write, 4=ISX" }
};
tdesc SWTBL_TABLE = { "swtbl", 15, swtbl_fields, 12 };

/******************************************************************************/

/* ---- edit parameter values (PRMTBL, EP commands) ------------------------- */
static tfield prmtbl_fields[] = {
  { "curtyp",  0, FK_BYTE, 0, "cursor type"                  },
  { "blnkrt",  1, FK_BYTE, 0, "cursor blink rate"            },
  { "indinc",  2, FK_BYTE, 0, "indent increment"             },
  { "ucnvsw",  3, FK_BYTE, 0, "upper-case convert"           },
  { "cmntch",  4, FK_BYTE, 0, "comment character"            },
  { "crdely",  5, FK_BYTE, 0, "carriage-return delay"        },
  { "wrapcl",  6, FK_BYTE, 0, "word-wrap column"             },
  { "bit7al",  7, FK_BYTE, 0, "bit-7 / meta handling"        },
  { "horopt",  8, FK_BYTE, 0, "horizontal-scroll option"     },
  { "hzscln",  9, FK_BYTE, 0, "horizontal scroll length"     },
  { "hzscic", 10, FK_BYTE, 0, "horizontal scroll increment"  }
};
tdesc PRMTBL_TABLE = { "prmtbl", 11, prmtbl_fields, 11 };

/******************************************************************************/

/* ---- print parameters (PRNTBL, PP commands) ------------------------------ */
static tfield prntbl_fields[] = {
  { "ppxpl",  0, FK_BYTE, 0, "physical lines per page"       },
  { "pplncn", 1, FK_BYTE, 0, "printed lines per page"        },
  { "pplfmr", 2, FK_BYTE, 0, "left margin when printing"     },
  { "ppfffl", 3, FK_BYTE, 0, "use form-feed on printer"      }
};
tdesc PRNTBL_TABLE = { "prntbl", 4, prntbl_fields, 4 };

/******************************************************************************/

/* ---- the INSTALL version-flags word (an overloaded bitfield) -------------- */
vflag VFLAGS[] = {
  { 0x0001, "8080mm" }, { 0x0002, "8086mm" }, { 0x0004, "crt"     },
  { 0x0008, "msdos"  }, { 0x0010, "cpm86"  }, { 0x0020, "ibm"     },
  { 0x0040, "tipc"   }, { 0x0080, "trs80"  }, { 0x0100, "piiceon" },
  { 0x0000, (char *) 0 }
};

/******************************************************************************/

int
#ifdef ANSI_COMPILER
tf_size (
  const tfield *f)
#else
tf_size (f)
  const tfield *f;
#endif
{
  switch (f->kind)
    {
    case FK_WORD:   return 2;
    case FK_SEQ:    return 7;
    case FK_ADDOFF: return 3;
    default:        return 1;
    }
}

/******************************************************************************/

const tfield *
#ifdef ANSI_COMPILER
tbl_field (
  const tdesc *t,
  const char *name)
#else
tbl_field (t, name)
  const tdesc *t;
  const char *name;
#endif
{
  int i;

  for (i = 0; i < t->nfields; i++)
    if (0 == strcmp (t->fields[i].name, name))
      return &t->fields[i];

  return (tfield *) 0;
}

/******************************************************************************/

int
#ifdef ANSI_COMPILER
tbl_get (
  const tdesc *t,
  const vbyte *buf,
  const char *name,
  vbyte *dst)
#else
tbl_get (t, buf, name, dst)
  const tdesc *t;
  const vbyte *buf;
  const char *name;
  vbyte *dst;
#endif
{
  const tfield *f;
  int n, i;

  f = tbl_field (t, name);

  if ((tfield *) 0 == f)
    return -1;

  n = tf_size (f);

  for (i = 0; i < n; i++)
    dst[i] = buf[f->off + i];

  return n;
}

/******************************************************************************/

int
#ifdef ANSI_COMPILER
tbl_set (
  const tdesc *t,
  vbyte *buf,
  const char *name,
  const vbyte *src,
  int n)
#else
tbl_set (t, buf, name, src, n)
  const tdesc *t;
  vbyte *buf;
  const char *name;
  const vbyte *src;
  int n;
#endif
{
  const tfield *f;
  int i, sz;

  f = tbl_field (t, name);

  if ((tfield *) 0 == f)
    return -1;

  if (FK_SEQ == f->kind)
    {
      if (6 < n)
        return -2;

      buf[f->off] = (vbyte) ((buf[f->off] & 0x80) | (n & 0x7F));

      for (i = 0; i < 6; i++)
        buf[f->off + 1 + i] = (vbyte) (i < n ? src[i] : 0);

      return 0;
    }

  sz = tf_size (f);

  if (n > sz)
    return -2;

  for (i = 0; i < sz; i++)
    buf[f->off + i] = (vbyte) (i < n ? src[i] : 0);

  return 0;
}

/******************************************************************************/

int
#ifdef ANSI_COMPILER
seq_count (
  const vbyte *slot)
#else
seq_count (slot)
  const vbyte *slot;
#endif
{
  return slot[0] & 0x7F;
}

/******************************************************************************/

int
#ifdef ANSI_COMPILER
seq_clr (
  const vbyte *slot)
#else
seq_clr (slot)
  const vbyte *slot;
#endif
{
  return ((slot[0] & 0x80) ? 1 : 0);
}

/******************************************************************************/

#define C_ALWAYS 0
#define C_CRT 1
#define C_MEM 2
#define C_PRINT 3

static struct { char *name; int cond; } chain_slots[] = {
  { "VRSNUM", C_ALWAYS }, { "ADDLED", C_CRT    }, { "KEYMSG", C_ALWAYS },
  { "KEYTBL", C_ALWAYS }, { "KEYTBN", C_ALWAYS }, { "TABPOS", C_ALWAYS },
  { "CHRTBL", C_ALWAYS }, { "PRNTBL", C_ALWAYS }, { "SWTBL",  C_ALWAYS },
  { "PRMTBL", C_ALWAYS }, { "ISPARE", C_ALWAYS }, { "PYLINE", C_ALWAYS },
  { "SCRINI", C_MEM    }, { "USRMSG", C_ALWAYS }, { "PXHEAD", C_PRINT  },
  { "PXSUBH", C_PRINT  }, { "PXFOOT", C_PRINT  }, { "PXHEND", C_PRINT  }
};

#define NCHAIN ((int) (sizeof (chain_slots) / sizeof (chain_slots[0])))

/******************************************************************************/

static int
#ifdef ANSI_COMPILER
chain_take (
  int cond,
  int has_crt)
#else
chain_take (cond, has_crt)
  int cond;
  int has_crt;
#endif
{
  switch (cond)
    {
    case C_CRT: return has_crt;
    case C_MEM: return !has_crt;
    default:    return 1;
    }
}

/******************************************************************************/

const char *
#ifdef ANSI_COMPILER
chain_name (
  int has_crt,
  int idx)
#else
chain_name (has_crt, idx)
  int has_crt;
  int idx;
#endif
{
  int i, k;

  k = 0;

  for (i = 0; i < NCHAIN; i++)
    if (chain_take (chain_slots[i].cond, has_crt))
      {
        if (k == idx)
          return chain_slots[i].name;

        k++;
      }

  return (char *) 0;
}

/******************************************************************************/

int
#ifdef ANSI_COMPILER
chain_index (
  int has_crt,
  const char *name)
#else
chain_index (has_crt, name)
  int has_crt;
  const char *name;
#endif
{
  int i, k;

  k = 0;

  for (i = 0; i < NCHAIN; i++)
    if (chain_take (chain_slots[i].cond, has_crt))
      {
        if (0 == strcmp (chain_slots[i].name, name))
          return k;

        k++;
      }

  return -1;
}

/******************************************************************************/

int
#ifdef ANSI_COMPILER
chain_count (
  int has_crt)
#else
chain_count (has_crt)
  int has_crt;
#endif
{
  int i, k;

  k = 0;

  for (i = 0; i < NCHAIN; i++)
    if (chain_take (chain_slots[i].cond, has_crt))
      k++;

  return k;
}

/******************************************************************************/
