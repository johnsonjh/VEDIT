#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "image.h"
#include "table.h"
#include "inifile.h"

#define TABPOS_LEN 34

static struct { char *name; tdesc *desc; } g_tables[] = {
  { "PYLINE", &PYLINE_TABLE },
  { "SWTBL",  &SWTBL_TABLE  },
  { "PRMTBL", &PRMTBL_TABLE },
  { "PRNTBL", &PRNTBL_TABLE }
};
#define NTAB ((int) (sizeof (g_tables) / sizeof (g_tables[0])))

#define KFF 0xFF

static vbyte g_keymsg[768];
static int   g_keymsg_len;
static vbyte g_kbuf[640];
static vbyte g_obuf[640];

static int
#ifdef ANSI_COMPILER
ieq (
  const char *a,
  const char *b)
#else
ieq (a, b)
  const char *a;
  const char *b;
#endif
{
  int i, r;

  r = 1;

  for (i = 0;; i++)
    {
      int ca, cb;

      ca = a[i];
      cb = b[i];

      if ('a' <= ca && 'z' >= ca) ca -= 32;
      if ('a' <= cb && 'z' >= cb) cb -= 32;

      if (ca != cb)
        {
          r = 0;
          break;
        }

      if ('\0' == ca)
        break;
    }

  return r;
}

static void
#ifdef ANSI_COMPILER
print_flags (
  vword f)
#else
print_flags (f)
  vword f;
#endif
{
  int i;

  printf ("flags     0x%04X =", f);

  for (i = 0; VFLAGS[i].name; i++)
    if (f & VFLAGS[i].bit)
      printf (" %s", VFLAGS[i].name);

  printf ("\n");
}

static void
#ifdef ANSI_COMPILER
dump_table (
  image *im,
  const char *name,
  tdesc *t)
#else
dump_table (im, name, t)
  image *im;
  const char *name;
  tdesc *t;
#endif
{
  vbyte blk[128];
  vword at;
  int i;

  at = img_table (im, name);

  if (0 == at)
    return; /* table missing in this build */

  if (0 != img_get_block (im, at, blk, t->len))
    return;

  printf ("%-7s @ 0x%04X:\n", name, at);

  for (i = 0; i < t->nfields; i++)
    {
      const tfield *fld;
      vword v;

      fld = &t->fields[i];

      if (FK_WORD == fld->kind)
        v = (vword) blk[fld->off] | ((vword) blk[fld->off + 1] << 8);
      else
        v = blk[fld->off];

      printf ("  %-12s = %3u  (0x%02X)   %s\n", fld->name, v, v, fld->help);
    }
}

static void
#ifdef ANSI_COMPILER
show_tabs (
  image *im)
#else
show_tabs (im)
  image *im;
#endif
{
  vbyte tab[TABPOS_LEN];
  vword at;
  int i;

  at = img_table (im, "TABPOS");

  if (0 == at)
    return;

  if (0 != img_get_block (im, at, tab, TABPOS_LEN))
    return;

  printf ("TABPOS  @ 0x%04X:", at);

  for (i = 0; i < 32 && 0xFF != tab[i] && 0 != tab[i]; i++)
    printf (" %u", tab[i]);

  printf ("\n");
}

static int
#ifdef ANSI_COMPILER
do_show (
  image *im)
#else
do_show (im)
  image *im;
#endif
{
  int i;

  printf ("format    %s (%ld bytes)\n", im->fmt, im->size);
  printf ("VRSNUM    %u  (VEDIT %u.%02u)\n",
          img_vrsnum (im), img_vrsnum (im) / 100, img_vrsnum (im) % 100);

  print_flags (img_flags (im));

  for (i = 0; i < NTAB; i++)
    dump_table (im, g_tables[i].name, g_tables[i].desc);

  show_tabs (im);

  return 0;
}

static int
#ifdef ANSI_COMPILER
table_index (
  const char *name)
#else
table_index (name)
  const char *name;
#endif
{
  int i;

  for (i = 0; i < NTAB; i++)
    if (ieq (g_tables[i].name, name))
      return i;

  return -1;
}

static int
#ifdef ANSI_COMPILER
do_set (
  image *im,
  const char *tname,
  const char *field,
  const char *valstr)
#else
do_set (im, tname, field, valstr)
  image *im;
  const char *tname;
  const char *field;
  const char *valstr;
#endif
{
  const tfield *fld;
  vword at;
  long val;
  int ti;

  ti = table_index (tname);

  if (0 > ti)
    {
      error_msg ("unknown table", tname, 0);

      return 1;
    }

  fld = tbl_field (g_tables[ti].desc, field);

  if ((const tfield *) 0 == fld)
    {
      error_msg ("unknown field", field, 0);

      return 1;
    }

  at = img_table (im, g_tables[ti].name);

  if (0 == at)
    {
      error_msg ("this build has no table", g_tables[ti].name, 0);

      return 1;
    }

  val = strtol (valstr, (char **) 0, 0);

  if (FK_WORD == fld->kind)
    img_put_word (im, (vword) (at + (vword) fld->off), (vword) val);
  else
    img_put_byte (im, (vword) (at + (vword) fld->off), (int) val);

  if (im->err)
    {
      error_msg ("write error", (char *) 0, errno);

      return 1;
    }

  printf ("set %s.%s = %ld (0x%lX)\n", g_tables[ti].name, field, val, (unsigned long) val);

  return 0;
}

static int
#ifdef ANSI_COMPILER
do_flag (
  image *im,
  const char *name,
  int on)
#else
do_flag (im, name, on)
  image *im;
  const char *name;
  int on;
#endif
{
  vword f, bit;
  int i;

  bit = 0;

  for (i = 0; VFLAGS[i].name; i++)
    if (0 == strcmp (VFLAGS[i].name, name))
      bit = VFLAGS[i].bit;

  if (0 == bit)
    {
      error_msg ("unknown flag", name, 0);
      return 1;
    }

  f = img_flags (im);
  f = (vword) ((on) ? (f | bit) : (f & ~bit));

  img_put_word (im, (vword) 0x0108, f);

  if (im->err)
    {
      error_msg ("write error", (char *) 0, errno);
      return 1;
    }

  print_flags (img_flags (im));

  return 0;
}

static int
#ifdef ANSI_COMPILER
do_tabs (
  image *im,
  int argc,
  char **argv)
#else
do_tabs (im, argc, argv)
  image *im;
  int argc;
  char **argv;
#endif
{
  vbyte tab[TABPOS_LEN];
  vword at;
  int i, n;

  at = img_table (im, "TABPOS");

  if (0 == at)
    {
      error_msg ("this build has no TABPOS table", (char *) 0, 0);

      return 1;
    }

  if (4 > argc) /* no columns given, just show */
    {
      show_tabs (im);

      return 0;
    }

  memset (tab, 0, TABPOS_LEN);
  n = argc - 3;

  if (32 < n)
    n = 32;

  for (i = 0; i < n; i++)
    tab[i] = (vbyte) atoi (argv[3 + i]);

  tab[n] = 0xFF;

  if (0 != img_put_block (im, at, tab, TABPOS_LEN))
    {
      error_msg ("write error", (char *) 0, errno);

      return 1;
    }

  printf ("set %d tab stop(s)\n", n);

  return 0;
}

static int
#ifdef ANSI_COMPILER
resolve_term (
  const inifile *f,
  const char *s)
#else
resolve_term (f, s)
  const inifile *f;
  const char *s;
#endif
{
  int i;

  for (i = 0; s[i]; i++)
    if ('0' > s[i] || '9' < s[i])
      return ini_find (f, s);

  return ((s[0]) ? atoi (s) : -1);
}

static int
#ifdef ANSI_COMPILER
do_apply (
  image *im,
  const char *inipath,
  const char *term)
#else
do_apply (im, inipath, term)
  image *im;
  const char *inipath;
  const char *term;
#endif
{
  inifile f;
  vword at;
  int idx, rc;

  at = img_table (im, "ADDLED");

  if (0 == at)
    {
      error_msg ("this build has no ADDLED (not a CRT build)", (char *) 0, 0);

      return 1;
    }

  if (0 > ini_load (&f, inipath))
    {
      error_msg ("cannot load", inipath, errno);

      return 1;
    }

  rc = 0;
  idx = resolve_term (&f, term);

  if (0 > idx || idx >= ini_count (&f))
    {
      error_msg ("no such terminal", term, 0);
      rc = 1;
    }
  else if (0 != img_put_block (im, at, ini_record (&f, idx), CRT_TABLE.len))
    {
      error_msg ("write error", (char *) 0, errno);
      rc = 1;
    }
  else
    printf ("applied terminal '%s' to ADDLED @ 0x%04X (%d bytes)\n",
            ini_name (&f, idx), at, CRT_TABLE.len);

  ini_free (&f);

  return rc;
}

static void
#ifdef ANSI_COMPILER
load_keymsg (
  image *im)
#else
load_keymsg (im)
  image *im;
#endif
{
  vword msg, kt;
  int len;

  g_keymsg_len = 0;
  msg = img_table (im, "KEYMSG");
  kt = img_table (im, "KEYTBL"); /* KEYMSG runs up to KEYTBL */

  if (0 == msg || 0 == kt || kt <= msg)
    return;

  len = (int) (kt - msg);

  if ((int) sizeof (g_keymsg) < len)
    len = (int) sizeof (g_keymsg);

  if (0 == img_get_block (im, msg, g_keymsg, len))
    g_keymsg_len = len;
}

static const char *
#ifdef ANSI_COMPILER
find_name (
  int c0,
  int c1)
#else
find_name (c0, c1)
  int c0;
  int c1;
#endif
{
  int i;

  i = 0;

  while (i < g_keymsg_len && g_keymsg[i])
    {
      int ns;

      ns = i;

      while (i < g_keymsg_len && g_keymsg[i])
        i++; /* the name */

      i++; /* its NUL */

      if (i + 1 <= g_keymsg_len
          && (int) g_keymsg[i] == c0 && (int) g_keymsg[i + 1] == c1)
        return (const char *) &g_keymsg[ns];

      i += 2; /* the 2-char code */

      if (i < g_keymsg_len && 0 == g_keymsg[i])
        i++; /* the codes NUL */
    }

  return (const char *) 0;
}

static int
#ifdef ANSI_COMPILER
load_keytbl (
  image *im,
  vword *at)
#else
load_keytbl (im, at)
  image *im;
  vword *at;
#endif
{
  vword kt, kn;
  int len;

  kt = img_table (im, "KEYTBL");
  kn = img_table (im, "KEYTBN");

  if (0 == kt || 0 == kn || kn <= kt)
    return 0;

  len = (int) (kn - kt);

  if ((int) sizeof (g_kbuf) < len)
    len = (int) sizeof (g_kbuf);

  if (0 != img_get_block (im, kt, g_kbuf, len))
    return 0;

  *at = kt;

  return len;
}

static int
#ifdef ANSI_COMPILER
do_keys (
  image *im)
#else
do_keys (im)
  image *im;
#endif
{
  vword kt;
  int len, pos, i;

  len = load_keytbl (im, &kt);

  if (0 == len)
    {
      error_msg ("this build has no keyboard table", (char *) 0, 0);
      return 1;
    }

  load_keymsg (im);
  printf ("KEYTBL @ 0x%04X (%d bytes):  key bytes -> code  function\n", kt, len);
  pos = 0;

  if (pos < len && KFF == g_kbuf[pos])
    pos++; /* leading KFF */

  while (pos < len)
    {
      const char *nm;
      int ks, ke, cs, ce;

      ks = pos;

      while (pos < len && KFF != g_kbuf[pos])
        pos++; /* key bytes */

      ke = pos;

      if (pos >= len)
        break;

      pos++; /* KFF after the key */
      cs = pos;
      while (pos < len && KFF != g_kbuf[pos])
        pos++; /* code bytes (variable length) */

      ce = pos;

      if (pos >= len)
        break;

      pos++; /* KFF after the code */
      printf ("  ");

      for (i = ks; i < ke; i++)
        printf ("%02X ", g_kbuf[i]);

      for (i = ke - ks; i < 6; i++)
        printf ("   "); /* align the code column */

      printf ("-> ");

      for (i = cs; i < ce; i++)
        printf ("%c", ((32 <= g_kbuf[i] && 127 > g_kbuf[i]) ? (char) g_kbuf[i] : '.'));

      for (i = ce - cs; i < 4; i++)
        printf (" "); /* align the name column */

      nm = find_name ((int) g_kbuf[cs], ((2 <= ce - cs) ? (int) g_kbuf[cs + 1] : 0));

      printf ("  %s\n", (((const char *) 0 != nm) ? nm : ""));
    }
  return 0;
}

static vword
#ifdef ANSI_COMPILER
key_maxend (
  image *im,
  vword  kt)
#else
key_maxend (im, kt)
  image *im;
  vword  kt;
#endif
{
  vword at, a, best;
  int has, n, i;

  at = img_addtbl (im);
  has = img_has_crt (im);
  n = chain_count (has);
  best = 0;

  for (i = 1; i < n; i++) /* skip VRSNUM at index 0 */
    {
      if (0 == strcmp (chain_name (has, i), "KEYTBN"))
        continue;

      a = img_get_word (im, (vword) (at + (vword) (2 * i)));

      if (a > kt && (0 == best || a < best))
        best = a;
    }

  return best;
}

static int
#ifdef ANSI_COMPILER
do_bind (
  image *im,
  const char  *code,
  const vbyte *bytes,
  int n)
#else
do_bind (im, code, bytes, n)
  image *im;
  const char  *code;
  const vbyte *bytes;
  int n;
#endif
{
  vword kt, kn, maxend;
  int region, max_region, pos, out, found, clen, i, write_len;

  clen = (int) strlen (code);
  kt = img_table (im, "KEYTBL");
  kn = img_table (im, "KEYTBN");

  if (0 == kt || 0 == kn || kn <= kt)
    {
      error_msg ("this build has no keyboard table", (char *) 0, 0);
      return 1;
    }

  region = (int) (kn - kt); /* current table content */
  maxend = key_maxend (im, kt); /* physical bound = next structure (not KEYTBN) */
  max_region = ((maxend > kt) ? (int) (maxend - kt) : region);

  if ((int) sizeof (g_kbuf) < region || (int) sizeof (g_obuf) < max_region)
    {
      error_msg ("keyboard table too large for this tool", (char *) 0, 0);

      return 1;
    }

  if (0 != img_get_block (im, kt, g_kbuf, region))
    {
      error_msg ("read error", (char *) 0, errno);

      return 1;
    }

  /* re-encode into g_obuf, replacing the matched code's key with `bytes` */
  out = 0;
  found = 0;
  pos = 0;

  if (pos < region && KFF == g_kbuf[pos])
    pos++;

  g_obuf[out++] = KFF; /* leading marker */

  while (pos < region && (int) sizeof (g_obuf) > out + 8)
    {
      int ks, ke, cs, ce, j, hit;

      ks = pos;

      while (pos < region && KFF != g_kbuf[pos])
        pos++; /* key bytes */

      ke = pos;

      if (pos >= region)
        break;

      pos++;
      cs = pos;

      while (pos < region && KFF != g_kbuf[pos])
        pos++; /* code bytes (variable) */

      ce = pos;

      if (pos >= region)
        break;

      pos++;

      /* match: stored code == `code`, or == `code` + a trailing NUL (macros) */
      hit = 0;

      if (ce - cs == clen && 0 == memcmp (&g_kbuf[cs], code, (size_t) clen))
        hit = 1;
      else if (ce - cs == clen + 1 && 0 == g_kbuf[ce - 1]
               && 0 == memcmp (&g_kbuf[cs], code, (size_t) clen))
        hit = 1;

      if (hit && !found)
        {
          found = 1;
          for (j = 0; j < n && (int) sizeof (g_obuf) > out; j++)
            g_obuf[out++] = bytes[j];    /* the new key */
        }
      else
        for (j = ks; j < ke && (int) sizeof (g_obuf) > out; j++)
          g_obuf[out++] = g_kbuf[j];     /* unchanged key */

      g_obuf[out++] = KFF;

      for (j = cs; j < ce && (int) sizeof (g_obuf) > out; j++)
        g_obuf[out++] = g_kbuf[j];       /* code unchanged */
      g_obuf[out++] = KFF;
    }

  if (!found)
    {
      error_msg ("function code not found in keyboard table", code, 0);

      return 1;
    }

  if (max_region < out)
    {
      error_msg ("relayout exceeds the reserved keyboard space", code, 0);

      return 1;
    }

  write_len = ((out > region) ? out : region); /* also clear old bytes if shrinking */

  for (i = out; i < write_len; i++)
    g_obuf[i] = 0;

  if (0 != img_put_block (im, kt, g_obuf, write_len))
    {
      error_msg ("write error", (char *) 0, errno);

      return 1;
    }

  img_set_table (im, "KEYTBN", (vword) (kt + (vword) out)); /* move the end pointer */

  if (im->err)
    {
      error_msg ("write error", (char *) 0, errno);

      return 1;
    }

  printf ("rebound %s (keyboard table now %d, reserved %d bytes)\n", code, out, max_region);

  return 0;
}

static void
#ifdef ANSI_COMPILER
usage ( void )
#else
usage ()
#endif
{
  fprintf (stderr,
    "usage: vcfg <image> show\n"
    "       vcfg <image> set <table> <field> <value>   (PYLINE SWTBL PRMTBL PRNTBL)\n"
    "       vcfg <image> flag <name> <0|1>             (8080mm crt msdos cpm86 ...)\n"
    "       vcfg <image> tabs [<col> ...]\n"
    "       vcfg <image> apply <install.ini> <terminal>\n"
    "       vcfg <image> keys\n"
    "       vcfg <image> bind <code> <hexbyte>...\n");
}

int
#ifdef ANSI_COMPILER
main (
  int    argc,
  char **argv)
#else
main (argc, argv)
  int    argc;
  char **argv;
#endif
{
  image im;
  const char *cmd;
  int rw, rc;

  if (3 > argc)
    {
      usage ();
      return 2;
    }

  cmd = argv[2];
  rw = 1;

  if (0 == strcmp (cmd, "show"))
    rw = 0;
  else if (0 == strcmp (cmd, "tabs") && 4 > argc)
    rw = 0;
  else if (0 == strcmp (cmd, "keys"))
    rw = 0;

  if (0 > img_open (&im, argv[1], rw))
    {
      error_msg ("cannot open or unsupported image", argv[1], errno);

      return 1;
    }

  if (0 == strcmp (cmd, "show"))
    rc = do_show (&im);
  else if (0 == strcmp (cmd, "set") && 6 <= argc)
    rc = do_set (&im, argv[3], argv[4], argv[5]);
  else if (0 == strcmp (cmd, "flag") && 5 <= argc)
    rc = do_flag (&im, argv[3], atoi (argv[4]));
  else if (0 == strcmp (cmd, "tabs"))
    rc = do_tabs (&im, argc, argv);
  else if (0 == strcmp (cmd, "apply") && 5 <= argc)
    rc = do_apply (&im, argv[3], argv[4]);
  else if (0 == strcmp (cmd, "keys"))
    rc = do_keys (&im);
  else if (0 == strcmp (cmd, "bind") && 5 <= argc)
    {
      vbyte b[16];
      int i, nb;

      memset (b, 0, sizeof (b));
      nb = argc - 4;

      if (16 < nb)
        nb = 16;

      for (i = 0; i < nb; i++)
        b[i] = (vbyte) strtol (argv[4 + i], (char **) 0, 16);

      rc = do_bind (&im, argv[3], b, nb);
    }
  else
    {
      usage ();
      rc = 2;
    }

  img_close (&im);

  return rc;
}
