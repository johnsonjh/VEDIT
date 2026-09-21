/*
 * VINSTALL - image.c
 * Copyright (c) 2026 Jeffrey H. Johnson <johnsonjh.dev@gmail.com>
 * SPDX-License-Identifier: MIT-0
 * scspell-id: 4ab591c4-6290-11f1-8053-80ee73e9b8e7
 */

/******************************************************************************/

#include "image.h"
#include "table.h"

/******************************************************************************/

#define V_LOAD   0x0100 /* 8080-model programs ORG at 0100H */
#define V_PADDR  0x0106 /* runtime addr of "DW ADDTBL" */
#define V_PFLAG  0x0108 /* runtime addr of the INSTALL version-flags word */
#define V_DSEGBG 0x010E /* runtime addr of historical 8086 DSEGBG word */
#define V_MSDOS  0x0008 /* INSTALL version-flag bit for MS-DOS */
#define V_CORE_CHAIN 13 /* VRSNUM through USRMSG */

/******************************************************************************/

static long
#ifdef ANSI_COMPILER
fileoff (
  const image *im,
  vword addr)
#else
fileoff (im, addr)
  const image *im;
  vword addr;
#endif
{
  return im->origin + (long) addr;
}

/******************************************************************************/

static long
#ifdef ANSI_COMPILER
headeroff (
  const image *im,
  vword addr)
#else
headeroff (im, addr)
  const image *im;
  vword addr;
#endif
{
  return im->header_origin + (long) addr;
}

/******************************************************************************/

static int
#ifdef ANSI_COMPILER
raw_byte (
  image *im,
  long off)
#else
raw_byte (im, off)
  image *im;
  long off;
#endif
{
  int c;

  if (0 != fseek (im->fp, off, SEEK_SET))
    {
      im->err++;

      return -1;
    }

  c = getc (im->fp);

  if (EOF == c)
    {
      im->err++;

      return -1;
    }

  return c & 0xFF;
}

/******************************************************************************/

static int
#ifdef ANSI_COMPILER
raw_word (
  image *im,
  long off)
#else
raw_word (im, off)
  image *im;
  long off;
#endif
{
  int lo, hi;

  lo = raw_byte (im, off);
  hi = raw_byte (im, off + 1L);

  if (0 > lo || 0 > hi)
    return -1;

  return lo | (hi << 8);
}

/******************************************************************************/

static int
#ifdef ANSI_COMPILER
valid_chain_candidate (
  image *im,
  long origin,
  vword addtbl)
#else
valid_chain_candidate (im, origin, addtbl)
  image *im;
  long origin;
  vword addtbl;
#endif
{
  long off, p;
  int v, i;

  off = origin + (long) addtbl;
  if (0L > off || off > im->size - (long) (2 * V_CORE_CHAIN))
    return 0;

  v = raw_word (im, off);
  if (0 > v || 1 > v || 1000 <= v)
    return 0;

  for (i = 1; i < V_CORE_CHAIN; i++)
    {
      p = raw_word (im, off + (long) (2 * i));
      if (0 >= p || 0L > origin + p || origin + p >= im->size)
        return 0;
    }

  return 1;
}

/******************************************************************************/

int
#ifdef ANSI_COMPILER
img_open (
  image *im,
  const char *path,
  int writable)
#else
img_open (im, path, writable)
  image *im;
  const char *path;
  int writable;
#endif
{
  int b0;

  im->fp = vfopen (path, (writable ? "rb+" : "rb"));

  if ((FILE *) 0 == im->fp)
    return -1;

  im->writable = writable;
  im->err = 0;
  im->origin = 0;
  im->header_origin = 0;
  (void)fseek (im->fp, 0L, SEEK_END);
  im->size = ftell (im->fp);

  b0 = raw_byte (im, 0L);

  if (0xC3 == b0) /* 8080 JMP -> flat .COM */
    {
      im->origin = -(long) V_LOAD;
      im->header_origin = -(long) V_LOAD;
      im->fmt = "flat .COM (8080, CP/M-80)";

      return 0;
    }

  if (0xE9 == b0) /* 8086 JMP -> flat or historical segmented .COM */
    {
      vword addtbl, flags, dsegbg;

      im->origin = -(long) V_LOAD;
      im->header_origin = -(long) V_LOAD;

      /*
       * Use the modern flat representation when
       * its ADDTBL chain decodes correctly.
       */

      addtbl = img_get_header_word (im, (vword) V_PADDR);
      flags = img_get_header_word (im, (vword) V_PFLAG);
      dsegbg = img_get_header_word (im, (vword) V_DSEGBG);

      if (0 != addtbl
          && valid_chain_candidate (im, im->origin, addtbl))
        {
          im->fmt = "flat .COM (8086, MS-DOS)";

          return 0;
        }

      /*
       * Historical PASM86(???) used to assemble stores ADDTBL entries as offsets
       * within a paragraph-aligned data segment.  Real INSTALL.EXE rounds DSEGBG
       * up to the next paragraph, then subtracts the load COM origin.
       */

      if ((flags & V_MSDOS) && 0 != dsegbg)
        {
          long dseg_origin;

          dseg_origin = (((long) dsegbg + 0x0FL) & ~0x0FL)
                       - (long) V_LOAD;

          if (0 != addtbl
              && valid_chain_candidate (im, dseg_origin, addtbl))
            {
              im->origin = dseg_origin;
              im->fmt = "\"historical\" segmented .COM (8086, MS-DOS)";

              return 0;
            }
        }
    }

  if (0x01 == b0 || 0x02 == b0) /* CP/M-86 .CMD group descriptor */
    {
      int b180;

      b180 = raw_byte (im, 0x0180L);

      if (0xC3 == b180 || 0xE9 == b180) /* flat 8080-model layout */
        {
          im->origin = 0x0180L - (long) V_LOAD; /* runtime 0100H is at file 0180H */
          im->header_origin = im->origin;
          im->fmt = "CP/M-86 .CMD (flat 8080 model)";

          return 0;
        }
    }

  (void)fclose (im->fp);
  im->fp = (FILE *) 0;

  return -2; /* unrecognized (e.g., segment .CMD) */
}

/******************************************************************************/

void
#ifdef ANSI_COMPILER
img_close (
  image *im)
#else
img_close (im)
  image *im;
#endif
{
  if ((FILE *) 0 != im->fp)
    {
      (void)fclose (im->fp);
      im->fp = (FILE *) 0;
    }
}

/******************************************************************************/

static int
#ifdef ANSI_COMPILER
img_get_byte (
  image *im,
  vword addr)
#else
img_get_byte (im, addr)
  image *im;
  vword addr;
#endif
{
  return raw_byte (im, fileoff (im, addr));
}

/******************************************************************************/

vword
#ifdef ANSI_COMPILER
img_get_header_word (
  image *im,
  vword addr)
#else
img_get_header_word (im, addr)
  image *im;
  vword addr;
#endif
{
  int lo, hi;

  lo = raw_byte (im, headeroff (im, addr));
  hi = raw_byte (im, headeroff (im, (vword) (addr + 1)));

  if (0 > lo || 0 > hi)
    return 0;

  return (vword) lo | ((vword) hi << 8);
}

/******************************************************************************/

vword
#ifdef ANSI_COMPILER
img_get_word (
  image *im,
  vword addr)
#else
img_get_word (im, addr)
  image *im;
  vword addr;
#endif
{
  int lo, hi;

  lo = img_get_byte (im, addr);
  hi = img_get_byte (im, (vword) (addr + 1));

  if (0 > lo || 0 > hi)
    return 0;

  return (vword) lo | ((vword) hi << 8);
}

/******************************************************************************/

int
#ifdef ANSI_COMPILER
img_put_byte (
  image *im,
  vword addr,
  int val)
#else
img_put_byte (im, addr, val)
  image *im;
  vword addr;
  int val;
#endif
{
  if (!im->writable)
    {
      im->err++;

      return -1;
    }

  if (0 != fseek (im->fp, fileoff (im, addr), SEEK_SET))
    {
      im->err++;

      return -1;
    }

  if (EOF == putc (val & 0xFF, im->fp))
    {
      im->err++;

      return -1;
    }

  return 0;
}

/******************************************************************************/

int
#ifdef ANSI_COMPILER
img_put_word (
  image *im,
  vword addr,
  vword val)
#else
img_put_word (im, addr, val)
  image *im;
  vword addr;
  vword val;
#endif
{
  if (0 != img_put_byte (im, addr, (int) (val & 0xFF)))
    return -1;

  return img_put_byte (im, (vword) (addr + 1), (int) ((val >> 8) & 0xFF));
}

/******************************************************************************/

int
#ifdef ANSI_COMPILER
img_put_header_word (
  image *im,
  vword addr,
  vword val)
#else
img_put_header_word (im, addr, val)
  image *im;
  vword addr;
  vword val;
#endif
{
  if (!im->writable)
    {
      im->err++;

      return -1;
    }

  if (0 != fseek (im->fp, headeroff (im, addr), SEEK_SET))
    {
      im->err++;

      return -1;
    }

  if (EOF == putc (val & 0xFF, im->fp)
      || EOF == putc ((val >> 8) & 0xFF, im->fp))
    {
      im->err++;

      return -1;
    }

  return 0;
}

/******************************************************************************/

int
#ifdef ANSI_COMPILER
img_get_block (
  image *im,
  vword addr,
  vbyte *buf,
  int n)
#else
img_get_block (im, addr, buf, n)
  image *im;
  vword addr;
  vbyte *buf;
  int n;
#endif
{
  int i;

  for (i = 0; i < n; i++)
    {
      int c;

      c = img_get_byte (im, (vword) (addr + (vword) i));

      if (0 > c)
        return -1;

      buf[i] = (vbyte) c;
    }

  return 0;
}

/******************************************************************************/

int
#ifdef ANSI_COMPILER
img_put_block (
  image *im,
  vword addr,
  const vbyte *buf,
  int n)
#else
img_put_block (im, addr, buf, n)
  image *im;
  vword addr;
  const vbyte *buf;
  int n;
#endif
{
  int i;

  for (i = 0; i < n; i++)
    if (0 != img_put_byte (im, (vword) (addr + (vword) i), buf[i]))
      return -1;

  return 0;
}

/******************************************************************************/

vword
#ifdef ANSI_COMPILER
img_addtbl (
  image *im )
#else
img_addtbl (im)
  image *im;
#endif
{
  return img_get_header_word (im, (vword) V_PADDR);
}

/******************************************************************************/

vword
#ifdef ANSI_COMPILER
img_flags (
  image *im)
#else
img_flags (im)
  image *im;
#endif
{
  return img_get_header_word (im, (vword) V_PFLAG);
}

/******************************************************************************/

vword
#ifdef ANSI_COMPILER
img_vrsnum (
  image *im)
#else
img_vrsnum (im)
  image *im;
#endif
{
  return img_get_word (im, img_addtbl (im));
}

/******************************************************************************/

int
#ifdef ANSI_COMPILER
img_has_crt (
  image *im)
#else
img_has_crt (im)
  image *im;
#endif
{
  return ((img_flags (im) & 0x0004) ? 1 : 0);
}

/******************************************************************************/

vword
#ifdef ANSI_COMPILER
img_table (
  image *im,
  const char *name)
#else
img_table (im, name)
  image *im;
  const char *name;
#endif
{
  int idx;
  vword at;

  idx = chain_index (img_has_crt (im), name);

  if (0 > idx)
    return 0;

  at = img_addtbl (im);

  return img_get_word (im, (vword) (at + (vword) (2 * idx)));
}

/******************************************************************************/

/* rewrite the ADDTBL pointer for a named table (e.g., KEYTBN). */
int
#ifdef ANSI_COMPILER
img_set_table (
  image *im,
  const char *name,
  vword addr)
#else
img_set_table (im, name, addr)
  image *im;
  const char *name;
  vword addr;
#endif
{
  int idx;

  idx = chain_index (img_has_crt (im), name);

  if (0 > idx)
    return -1;

  return img_put_word (im, (vword) (img_addtbl (im) + (vword) (2 * idx)), addr);
}

/******************************************************************************/
