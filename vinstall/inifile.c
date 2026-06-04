#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "inifile.h"

#define MAGIC "*CRT.TBL*"
#define MAGLEN 9

int
#ifdef ANSI_COMPILER
ini_load (
  inifile *f,
  const char *path)
#else
ini_load (f, path)
  inifile *f;
  const char *path;
#endif
{
  FILE *fp;
  long n;
  vbyte *p;
  char *q;

  fp = vfopen (path, "rb");

  if ((FILE *) 0 == fp)
    return -1;

  fseek (fp, 0L, SEEK_END);
  n = ftell (fp);
  fseek (fp, 0L, SEEK_SET);

  if (0L > n || 1000000L < n) /* ftell failed or far too big for a *CRT.TBL* */
    {
      fclose (fp);

      return -6;
    }

  p = (vbyte *) malloc ((size_t) n + 1);

  if ((vbyte *) 0 == p)
    {
      fclose (fp);

      return -2;
    }

  if (fread (p, 1, (size_t) n, fp) != (size_t) n)
    {
      fclose (fp);
      FREE (p);

      return -3;
    }

  fclose (fp);
  p[n] = 0;

  if (0 != strncmp ((char *) p, MAGIC, MAGLEN))
    {
      FREE (p);

      return -4;
    }

  q = (char *) p;
  q += strlen (q) + 1; /* past the magic */
  f->count = atoi (q);
  q += strlen (q) + 1; /* past the count */
  f->namelen = atol (q);
  q += strlen (q) + 1; /* past the name-block len */
  f->buf = p;
  f->size = n;
  f->name_base = (long) (q - (char *) p);
  f->rec_base = f->name_base + f->namelen;

  if (0 >= f->count || f->rec_base + (long) f->count * INI_RECLEN > n)
    {
      FREE (p);
      f->buf = (vbyte *) 0;

      return -5;
    }

  return 0;
}

int
#ifdef ANSI_COMPILER
ini_save (
  const inifile *f,
  const char *path)
#else
ini_save (f, path)
  const inifile *f;
  const char *path;
#endif
{
  FILE *fp;

  fp = vfopen (path, "wb");

  if ((FILE *) 0 == fp)
    return -1;

  if (fwrite (f->buf, 1, (size_t) f->size, fp) != (size_t) f->size)
    {
      fclose (fp);
      return -2;
    }

  fclose (fp);

  return 0;
}

void
#ifdef ANSI_COMPILER
ini_free (
  inifile *f)
#else
ini_free (f)
  inifile *f;
#endif
{
  if ((vbyte *) 0 != f->buf)
    FREE (f->buf);
}

int
#ifdef ANSI_COMPILER
ini_count (
  const inifile *f )
#else
ini_count (f)
  const inifile *f;
#endif
{
  return f->count;
}

const char *
#ifdef ANSI_COMPILER
ini_name (
  const inifile *f,
  int k)
#else
ini_name (f, k)
  const inifile *f;
  int k;
#endif
{
  const char *q;
  int i;

  if (0 > k || k >= f->count)
    return (const char *) 0;

  q = (const char *) f->buf + f->name_base;

  for (i = 0; i < k; i++)
    q += strlen (q) + 1;

  return q;
}

static int
#ifdef ANSI_COMPILER
ci_prefix (
  const char *a,
  const char *b)
#else
ci_prefix (a, b)
  const char *a;
  const char *b;
#endif
{
  int i;

  for (i = 0; b[i]; i++)
    {
      int ca, cb;

      ca = a[i];
      cb = b[i];

      if (ca >= 'a' && ca <= 'z')
        ca -= 32;

      if (cb >= 'a' && cb <= 'z')
        cb -= 32;

      if (ca != cb)
        return 0;
    }

  return 1;
}

int
#ifdef ANSI_COMPILER
ini_find (
  const inifile *f,
  const char *name)
#else
ini_find (f, name)
  const inifile *f;
  const char *name;
#endif
{
  int k;
  const char *nm;

  for (k = 0; k < f->count; k++)
    {
      nm = ini_name (f, k);

      if ((char *) 0 != nm && 0 == strcmp (nm, name))
        return k;
    }

  for (k = 0; k < f->count; k++)
    {
      nm = ini_name (f, k);

      if ((char *) 0 != nm && ci_prefix (nm, name))
        return k;
    }

  return -1;
}

vbyte *
#ifdef ANSI_COMPILER
ini_record (
  const inifile *f,
  int k)
#else
ini_record (f, k)
  const inifile *f;
  int k;
#endif
{
  if (0 > k || k >= f->count)
    return (vbyte *) 0;

  return f->buf + f->rec_base + (long) k * INI_RECLEN;
}

int
#ifdef ANSI_COMPILER
ini_add (
  inifile *f,
  const char *name,
  const vbyte *rec)
#else
ini_add (f, name, rec)
  inifile *f;
  const char *name;
  const vbyte *rec;
#endif
{
  long add_nl, old_rec, trailer_off, trailer_len, hdr_len, new_size, pos;
  int new_count;
  char cbuf[24], nbuf[24]; /* room for any int/long decimal + NUL */
  vbyte *nb;

  add_nl = (long) strlen (name) + 1;
  new_count = f->count + 1;
  old_rec = (long) f->count * INI_RECLEN;
  trailer_off = f->rec_base + old_rec;
  trailer_len = f->size - trailer_off;

  if (0 > trailer_len)
    trailer_len = 0;

  sprintf (cbuf, "%d", new_count);
  sprintf (nbuf, "%ld", f->namelen + add_nl);
  hdr_len = 10 + (long) strlen (cbuf) + 1 + (long) strlen (nbuf) + 1;
  new_size = hdr_len + (f->namelen + add_nl)
           + (long) new_count * INI_RECLEN + trailer_len;
  nb = (vbyte *) malloc ((size_t) new_size + 1);

  if ((vbyte *) 0 == nb)
    return -1;

  memcpy (nb, MAGIC, MAGLEN);
  nb[MAGLEN] = 0;
  pos = 10;
  strcpy ((char *) nb + pos, cbuf); pos += (long) strlen (cbuf) + 1;
  strcpy ((char *) nb + pos, nbuf); pos += (long) strlen (nbuf) + 1;
  memcpy (nb + pos, f->buf + f->name_base, (size_t) f->namelen); pos += f->namelen;
  memcpy (nb + pos, name, (size_t) add_nl); pos += add_nl;
  memcpy (nb + pos, f->buf + f->rec_base, (size_t) old_rec); pos += old_rec;

  if ((vbyte *) 0 != rec)
    memcpy (nb + pos, rec, INI_RECLEN);
  else
    memset (nb + pos, 0, INI_RECLEN);

  pos += INI_RECLEN;

  if (0 < trailer_len)
    {
      memcpy (nb + pos, f->buf + trailer_off, (size_t) trailer_len);
      pos += trailer_len;
    }

  nb[pos] = 0;

  FREE (f->buf);
  f->buf = nb;
  f->size = new_size;
  f->count = new_count;
  f->namelen += add_nl;
  f->name_base = hdr_len;
  f->rec_base = hdr_len + f->namelen;

  return new_count - 1;
}
