/*
 * VINSTALL - table.h
 * Copyright (c) 2026 Jeffrey H. Johnson <johnsonjh.dev@gmail.com>
 * SPDX-License-Identifier: MIT-0
 * scspell-id: da9c5b74-6290-11f1-ac48-80ee73e9b8e7
 */

/******************************************************************************/

#ifndef TABLE_H
#define TABLE_H

/******************************************************************************/

#include "port.h"

/******************************************************************************/

/* field types */
#define FK_BYTE 0
#define FK_WORD 1
#define FK_SEQ 2
#define FK_ADDOFF 3

/******************************************************************************/

/* field flags */
#define FF_CLRBIT 0x01

/******************************************************************************/

typedef struct {
  char *name;
  int off;
  int kind;
  int flags;
  char *help;
} tfield;

/******************************************************************************/

typedef struct {
  char *name;
  int len;
  tfield *fields;
  int nfields;
} tdesc;

/******************************************************************************/

typedef struct { vword bit; char *name; } vflag;

/******************************************************************************/

#ifndef TABLE_OWNER
extern tdesc CRT_TABLE;
extern tdesc PYLINE_TABLE;
extern tdesc SWTBL_TABLE;
extern tdesc PRMTBL_TABLE;
extern tdesc PRNTBL_TABLE;
extern vflag VFLAGS[];
#endif

/******************************************************************************/

/* int tf_size P_((const tfield *f)); */
const tfield *tbl_field P_((const tdesc *t, const char *name));
/* int tbl_get P_((const tdesc *t, const vbyte *buf, const char *name, vbyte *dst)); */ /* -> nbytes, -1 */
int tbl_set P_((const tdesc *t, vbyte *buf, const char *name, const vbyte *src, int n));

/******************************************************************************/

int seq_count P_((const vbyte *slot));
int seq_clr P_((const vbyte *slot));

/******************************************************************************/

const char *chain_name  P_((int has_crt, int idx)); /* NULL past end */
int chain_index P_((int has_crt, const char *name)); /* -1 if absent */
int chain_count P_((int has_crt));

/******************************************************************************/

#endif

/******************************************************************************/
