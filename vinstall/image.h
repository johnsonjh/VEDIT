/*
 * VINSTALL - image.h
 * Copyright (c) 2026 Jeffrey H. Johnson <johnsonjh.dev@gmail.com>
 * SPDX-License-Identifier: MIT-0
 * scspell-id: 644b691a-6290-11f1-9a9c-80ee73e9b8e7
 */

/******************************************************************************/

#ifndef IMAGE_H
#define IMAGE_H

/******************************************************************************/

#include <stdio.h>
#include "port.h"

/******************************************************************************/

typedef struct {
  FILE *fp;
  long  origin; /* file_offset = origin + runtime_address */
  long  size;
  int   writable;
  int   err;
  char *fmt;
} image;

/******************************************************************************/

int   img_open  P_((image *im, const char *path, int writable));  /* 0 ok, <0 error */
void  img_close P_((image *im));

/******************************************************************************/

/* int   img_get_byte  P_((image *im, vword addr)); */ /* -1 error */
vword img_get_word  P_((image *im, vword addr));
int   img_put_byte  P_((image *im, vword addr, int val)); /* 0 ok */
int   img_put_word  P_((image *im, vword addr, vword val));
int   img_get_block P_((image *im, vword addr, vbyte *buf, int n));
int   img_put_block P_((image *im, vword addr, const vbyte *buf, int n));

/******************************************************************************/

vword img_addtbl  P_((image *im)); /* runtime addr of ADDTBL */
vword img_vrsnum  P_((image *im));
vword img_flags   P_((image *im));
int   img_has_crt P_((image *im));
vword img_table   P_((image *im, const char *name)); /* runtime addr of a table, 0 if missing */
int   img_set_table P_((image *im, const char *name, vword addr)); /* rewrite a chain pointer; 0 ok */

/******************************************************************************/

#endif

/******************************************************************************/
