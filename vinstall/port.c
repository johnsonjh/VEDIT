/*
 * VINSTALL - port.c
 * Copyright (c) 2026 Jeffrey H. Johnson <johnsonjh.dev@gmail.com>
 * SPDX-License-Identifier: MIT-0
 * scspell-id: c5f31aa0-6290-11f1-9298-80ee73e9b8e7
 */

/******************************************************************************/

#include <stdio.h>
#include <string.h>
#include "port.h"

/******************************************************************************/

static char *
#ifdef ANSI_COMPILER
trim_str (
  const char *s)
#else
trim_str (s)
  const char *s;
#endif
{
  static char bufs[TRIM_RING][TRIM_BUFSIZE];
  static int idx = 0;
  const char *p;
  const char *q;
  const char *last;
  char *buf;
  char *d;

  buf = bufs[idx];
  idx++;

  if (TRIM_RING <= idx)
    idx = 0;

  if ((const char *) 0 == s)
    {
      buf[0] = '\0';
      return buf;
    }

  p = s;

  while (' ' == *p || '\t' == *p || '\r' == *p || '\n' == *p)
    p++;

  if ('\0' == *p)
    {
      buf[0] = '\0';
      return buf;
    }

  q = p;
  last = p;

  while ('\0' != *q)
    {
      if (' ' != *q && '\t' != *q && '\r' != *q && '\n' != *q)
        last = q;
      q++;
    }

  d = buf;

  while (p <= last && d < buf + (TRIM_BUFSIZE - 1))
    {
      if ('\r' == *p || '\n' == *p)
        *d++ = ' ';
      else
        *d++ = *p;

      p++;
    }

  *d = '\0';

  return buf;
}

/******************************************************************************/

void
#ifdef ANSI_COMPILER
error_msg (
  const char *m,
  const char *n,
  int e)
#else
error_msg (m, n, e)
  const char *m;
  const char *n;
  int e;
#endif
{
  (void)fprintf (stderr, "ERROR: %s", m);

  if ((const char *) 0 != n)
    (void)fprintf (stderr, " %s", n);

  if (0 != e)
    {
      (void)fprintf (stderr, " (error %d", e);
#if defined(ANSI_COMPILER) && !defined(NO_STRERROR)
      (void)fprintf (stderr, ": %s", trim_str (strerror (e)));
#endif
      (void)fprintf (stderr, ")");
    }

  (void)fprintf (stderr, ".\n");
}

/******************************************************************************/

FILE *
#ifdef ANSI_COMPILER
vfopen (
  const char *path,
  const char *mode)
#else
vfopen (path, mode)
  const char *path;
  const char *mode;
#endif
{
  FILE *fp;
  char m[8];
  int i, j;

  fp = fopen (path, mode);

  if ((FILE *) 0 != fp)
    return fp;

  j = 0;

  for (i = 0; mode[i] && 7 > j; i++)
    if ('b' != mode[i])
      m[j++] = mode[i];

  m[j] = 0;

  return fopen (path, m);
}

/******************************************************************************/
