/*
 * VINSTALL - instcore.c
 * Copyright (c) 2026 Jeffrey H. Johnson <johnsonjh.dev@gmail.com>
 * SPDX-License-Identifier: MIT-0
 * scspell-id: b743552e-6290-11f1-911f-80ee73e9b8e7
 */

/******************************************************************************/

#include <stdio.h>
#include "image.h"
#include "table.h"

/******************************************************************************/

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
  image im;
  vword addtbl, flags;
  int has_crt, n, i;

  if (2 > argc)
    {
      (void)fprintf (stderr, "usage: instcore <vedit-image.com|.cmd>\n");

      return 2;
    }

  if (0 > img_open (&im, argv[1], 0))
    {
      error_msg ("cannot open or unsupported image", argv[1], errno);

      return 1;
    }

  addtbl  = img_addtbl (&im);
  flags   = img_flags (&im);
  has_crt = img_has_crt (&im);

  (void)printf ("file     : %s (%ld bytes)\n", argv[1], im.size);
  (void)printf ("format   : %s\n", im.fmt);
  (void)printf ("ADDTBL   : 0x%04X  (file 0x%lX)\n",
                addtbl, (unsigned long) (im.origin + (long) addtbl));
  (void)printf ("VRSNUM   : %u  (VEDIT %u.%02u)\n",
                img_vrsnum (&im), img_vrsnum (&im) / 100, img_vrsnum (&im) % 100);
  (void)printf ("flags    : 0x%04X =", flags);

  for (i = 0; VFLAGS[i].name; i++)
    if (flags & VFLAGS[i].bit)
      (void)printf (" %s", VFLAGS[i].name);

  (void)printf ("\n");

  n = chain_count (has_crt);
  (void)printf ("\nADDTBL: %d entries (%s build)\n",
                n, (has_crt ? "CRT" : "memory-mapped"));
  (void)printf ("  idx  entry    value    file-off   note\n");

  for (i = 0; i < n; i++)
    {
      vword v;
      long  fo;

      v = img_get_word (&im, (vword) (addtbl + (vword) (2 * i)));

      if (0 == i)
        {
          (void)printf ("  %2d   %-7s  %5u    --         version\n",
                        i, chain_name (has_crt, i), v);

          continue;
        }

      fo = im.origin + (long) v;

      if (0 <= fo && fo < im.size)
        (void)printf ("  %2d   %-7s  0x%04X   0x%05lX  in image\n",
                      i, chain_name (has_crt, i), v, (unsigned long) fo);
      else
        (void)printf ("  %2d   %-7s  0x%04X   --         out of image\n",
                      i, chain_name (has_crt, i), v);
    }

  img_close (&im);

  return (im.err ? 1 : 0);
}

/******************************************************************************/
