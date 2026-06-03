#include "image.h"
#include "table.h"

#define V_LOAD  0x0100 /* 8080-model programs ORG at 0100H */
#define V_PADDR 0x0106 /* runtime addr of "DW ADDTBL" */
#define V_PFLAG 0x0108 /* runtime addr of the INSTALL version-flags word */

static long
#ifdef ANSI_COMPILER
fileoff (
  const image *im,
  vword        addr)
#else
fileoff (im, addr)
  const image *im;
  vword        addr;
#endif
{
  return im->origin + (long) addr;
}

static int
#ifdef ANSI_COMPILER
raw_byte (
  image *im,
  long   off)
#else
raw_byte (im, off)
  image *im;
  long   off;
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

int
#ifdef ANSI_COMPILER
img_open (
  image      *im,
  const char *path,
  int         writable)
#else
img_open (im, path, writable)
  image      *im;
  const char *path;
  int         writable;
#endif
{
  int b0;

  im->fp = vfopen (path, (writable ? "rb+" : "rb"));
  if ((FILE *) 0 == im->fp)
    return -1;
  im->writable = writable;
  im->err = 0;
  im->origin = 0;
  fseek (im->fp, 0L, SEEK_END);
  im->size = ftell (im->fp);

  b0 = raw_byte (im, 0L);

  if (0xC3 == b0 || 0xE9 == b0) /* 8080/8086 JMP -> flat .COM */
    {
      im->origin = -(long) V_LOAD;
      im->fmt = ((0xC3 == b0) ? "flat .COM (8080, CP/M-80)"
                              : "flat .COM (8086, MS-DOS)");

      return 0;
    }

  if (0x01 == b0 || 0x02 == b0) /* CP/M-86 .CMD group descriptor */
    {
      int b180;

      b180 = raw_byte (im, 0x0180L);

      if (0xC3 == b180 || 0xE9 == b180) /* flat 8080-model layout */
        {
          im->origin = 0x0180L - (long) V_LOAD; /* runtime 0100H is at file 0180H */
          im->fmt = "CP/M-86 .CMD (flat 8080 model)";

          return 0;
        }
    }

  fclose (im->fp);
  im->fp = (FILE *) 0;

  return -2; /* unrecognized (e.g., segment .CMD) */
}

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
      fclose (im->fp);
      im->fp = (FILE *) 0;
    }
}

int
#ifdef ANSI_COMPILER
img_get_byte (
  image *im,
  vword  addr)
#else
img_get_byte (im, addr)
  image *im;
  vword  addr;
#endif
{
  return raw_byte (im, fileoff (im, addr));
}

vword
#ifdef ANSI_COMPILER
img_get_word (
  image *im,
  vword  addr)
#else
img_get_word (im, addr)
  image *im;
  vword  addr;
#endif
{
  int lo, hi;

  lo = img_get_byte (im, addr);
  hi = img_get_byte (im, (vword) (addr + 1));

  if (0 > lo || 0 > hi)
    return 0;

  return (vword) lo | ((vword) hi << 8);
}

int
#ifdef ANSI_COMPILER
img_put_byte (
  image *im,
  vword  addr,
  int    val)
#else
img_put_byte (im, addr, val)
  image *im;
  vword  addr;
  int    val;
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

int
#ifdef ANSI_COMPILER
img_put_word (
  image *im,
  vword  addr,
  vword  val)
#else
img_put_word (im, addr, val)
  image *im;
  vword  addr;
  vword  val;
#endif
{
  if (0 != img_put_byte (im, addr, (int) (val & 0xFF)))
    return -1;

  return img_put_byte (im, (vword) (addr + 1), (int) ((val >> 8) & 0xFF));
}

int
#ifdef ANSI_COMPILER
img_get_block (
  image *im,
  vword  addr,
  vbyte *buf,
  int    n)
#else
img_get_block (im, addr, buf, n)
  image *im;
  vword  addr;
  vbyte *buf;
  int    n;
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

int
#ifdef ANSI_COMPILER
img_put_block (
  image       *im,
  vword        addr,
  const vbyte *buf,
  int          n)
#else
img_put_block (im, addr, buf, n)
  image       *im;
  vword        addr;
  const vbyte *buf;
  int          n;
#endif
{
  int i;

  for (i = 0; i < n; i++)
    if (0 != img_put_byte (im, (vword) (addr + (vword) i), buf[i]))
      return -1;

  return 0;
}

vword
#ifdef ANSI_COMPILER
img_addtbl ( image *im )
#else
img_addtbl (im) image *im;
#endif
{
  return img_get_word (im, (vword) V_PADDR);
}

vword
#ifdef ANSI_COMPILER
img_flags ( image *im )
#else
img_flags (im) image *im;
#endif
{
  return img_get_word (im, (vword) V_PFLAG);
}

vword
#ifdef ANSI_COMPILER
img_vrsnum ( image *im )
#else
img_vrsnum (im) image *im;
#endif
{
  return img_get_word (im, img_addtbl (im));
}

int
#ifdef ANSI_COMPILER
img_has_crt ( image *im )
#else
img_has_crt (im) image *im;
#endif
{
  return ((img_flags (im) & 0x0004) ? 1 : 0);
}

vword
#ifdef ANSI_COMPILER
img_table (
  image      *im,
  const char *name)
#else
img_table (im, name)
  image      *im;
  const char *name;
#endif
{
  int   idx;
  vword at;

  idx = chain_index (img_has_crt (im), name);

  if (0 > idx)
    return 0;

  at = img_addtbl (im);

  return img_get_word (im, (vword) (at + (vword) (2 * idx)));
}

/* rewrite the ADDTBL pointer for a named table (e.g., KEYTBN). */
int
#ifdef ANSI_COMPILER
img_set_table (
  image      *im,
  const char *name,
  vword       addr)
#else
img_set_table (im, name, addr)
  image      *im;
  const char *name;
  vword       addr;
#endif
{
  int idx;

  idx = chain_index (img_has_crt (im), name);

  if (0 > idx)
    return -1;

  return img_put_word (im, (vword) (img_addtbl (im) + (vword) (2 * idx)), addr);
}
