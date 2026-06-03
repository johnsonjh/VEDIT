#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "inifile.h"
#include "table.h"

static int
#ifdef ANSI_COMPILER
resolve (
  const inifile *f,
  const char *s)
#else
resolve (f, s)
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

static void
#ifdef ANSI_COMPILER
show_term (
  const inifile *f,
  int idx)
#else
show_term (f, idx)
  const inifile *f;
  int idx;
#endif
{
  const vbyte *rec;
  int i, j, n;

  rec = ini_record (f, idx);
  printf ("[%d] %s\n", idx, ini_name (f, idx));

  for (i = 0; i < CRT_TABLE.nfields; i++)
    {
      const tfield *fld;
      const vbyte  *slot;

      fld = &CRT_TABLE.fields[i];
      printf ("  %-14s ", fld->name);
      slot = rec + fld->off;

      if (FK_ADDOFF == fld->kind)
        printf ("mode=0x%02X  row+=%d  col+=%d", slot[0], slot[1], slot[2]);
      else
        {
          n = seq_count (slot);
          if (0 == n)
            printf ("(none)");

          for (j = 0; j < n; j++)
            printf ("%02X ", slot[1 + j]);

          if (fld->flags & FF_CLRBIT)
            printf (" [no-preclear=%d]", seq_clr (slot));
        }

      printf ("   ; %s\n", fld->help);
    }
}

static int
#ifdef ANSI_COMPILER
do_set (
  const inifile *f,
  int idx,
  const char *field,
  int argc,
  char **argv)
#else
do_set (f, idx, field, argc, argv)
  const inifile *f;
  int idx;
  const char *field;
  int argc;
  char **argv;
#endif
{
  vbyte bytes[8];
  int i, n, r;

  memset (bytes, 0, sizeof (bytes));
  n = argc - 5;

  if (8 < n)
    n = 8;

  for (i = 0; i < n; i++)
    bytes[i] = (vbyte) strtol (argv[5 + i], (char **) 0, 16);

  r = tbl_set (&CRT_TABLE, ini_record (f, idx), field, bytes, n);

  if (-1 == r)
    {
      error_msg ("unknown field", field, 0);

      return 1;
    }

  if (-2 == r)
    {
      error_msg ("value too long for field", field, 0);

      return 1;
    }

  return 0;
}

static int
#ifdef ANSI_COMPILER
do_clr (
  const inifile *f,
  int idx,
  int on)
#else
do_clr (f, idx, on)
  const inifile *f;
  int idx;
  int on;
#endif
{
  const tfield *fld;
  vbyte *slot;

  fld = tbl_field (&CRT_TABLE, "clear_screen");

  if ((tfield *) 0 == fld)
    return 1;

  slot = ini_record (f, idx) + fld->off;

  if (on)
    slot[0] = (vbyte) (slot[0] | 0x80);
  else
    slot[0] = (vbyte) (slot[0] & 0x7F);

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
    "usage: vintmod <ini> list\n"
    "       vintmod <ini> show  <n|name>\n"
    "       vintmod <ini> set   <n|name> <field> <hexbyte>...\n"
    "       vintmod <ini> clr   <n|name> <0|1>\n"
    "       vintmod <ini> add   <name>\n"
    "       vintmod <ini> clone <n|name> <newname>\n");
}

int
#ifdef ANSI_COMPILER
main (
  int argc,
  char **argv)
#else
main (argc, argv)
  int argc;
  char **argv;
#endif
{
  inifile f;
  const char *cmd;
  int idx, rc, save;

  if (3 > argc)
    {
      usage ();

      return 2;
    }

  if (0 > ini_load (&f, argv[1]))
    {
      error_msg ("cannot load or parse *CRT.TBL*", argv[1], errno);

      return 1;
    }

  cmd = argv[2];
  rc = 0;
  save = 0;

  if (0 == strcmp (cmd, "list"))
    {
      for (idx = 0; idx < ini_count (&f); idx++)
        printf ("%3d  %s\n", idx, ini_name (&f, idx));
    }
  else if (0 == strcmp (cmd, "show") && 4 <= argc)
    {
      idx = resolve (&f, argv[3]);

      if (0 > idx || idx >= ini_count (&f))
        { error_msg ("no such terminal", argv[3], 0); rc = 1; }
      else
        show_term (&f, idx);
    }
  else if (0 == strcmp (cmd, "set") && 6 <= argc)
    {
      idx = resolve (&f, argv[3]);

      if (0 > idx || idx >= ini_count (&f))
        { error_msg ("no such terminal", argv[3], 0); rc = 1; }
      else
        { rc = do_set (&f, idx, argv[4], argc, argv); save = (0 == rc); }
    }
  else if (0 == strcmp (cmd, "clr") && 5 <= argc)
    {
      idx = resolve (&f, argv[3]);

      if (0 > idx || idx >= ini_count (&f))
        { error_msg ("no such terminal", argv[3], 0); rc = 1; }
      else
        { rc = do_clr (&f, idx, atoi (argv[4])); save = (0 == rc); }
    }
  else if (0 == strcmp (cmd, "add") && 4 <= argc)
    {
      idx = ini_add (&f, argv[3], (vbyte *) 0);

      if (0 > idx) { error_msg ("add failed", (char *) 0, 0); rc = 1; }
      else { printf ("added [%d] %s\n", idx, argv[3]); save = 1; }
    }
  else if (0 == strcmp (cmd, "clone") && 5 <= argc)
    {
      idx = resolve (&f, argv[3]);

      if (0 > idx || idx >= ini_count (&f))
        { error_msg ("no such terminal", argv[3], 0); rc = 1; }
      else
        {
          vbyte tmp[INI_RECLEN];
          memcpy (tmp, ini_record (&f, idx), INI_RECLEN);
          idx = ini_add (&f, argv[4], tmp);

          if (0 > idx) { error_msg ("clone failed", (char *) 0, 0); rc = 1; }
          else { printf ("cloned to [%d] %s\n", idx, argv[4]); save = 1; }
        }
    }
  else
    {
      usage ();
      rc = 2;
    }

  if (save && 0 != ini_save (&f, argv[1]))
    {
      error_msg ("write error", (char *) 0, errno);
      rc = 1;
    }

  ini_free (&f);

  return rc;
}
