/*
 * VINSTALL - port.h
 * Copyright (c) 2026 Jeffrey H. Johnson <johnsonjh.dev@gmail.com>
 * SPDX-License-Identifier: MIT-0
 * scspell-id: cd063eee-6290-11f1-801b-80ee73e9b8e7
 */

/******************************************************************************/

#ifndef PORT_H
#define PORT_H

/******************************************************************************/

#include <stdio.h>

/******************************************************************************/

#if defined(KR_COMPILER)
# undef ANSI_COMPILER
#elif !defined(ANSI_COMPILER)
# if defined(__STDC__) || defined(__cplusplus)
#  define ANSI_COMPILER
# endif
#endif

/******************************************************************************/

#ifdef ANSI_COMPILER
# define P_(args) args
#else
# define P_(args) ()
#endif

/******************************************************************************/

#ifndef ANSI_COMPILER
# define const /* //-V1059 */
#endif

/******************************************************************************/

typedef unsigned char vbyte;
typedef unsigned int vword;
typedef unsigned long vaddr;

/******************************************************************************/

#define VLE16(p) ((vword)((vbyte)(p)[0] | ((vword)(vbyte)(p)[1] << 8)))

/******************************************************************************/

#if defined(ANSI_COMPILER) && !defined(NO_ERRNO)
# include <errno.h>
#else
# ifndef errno
#  define errno 0
# endif
#endif

/******************************************************************************/

#define TRIM_BUFSIZE 256
#define TRIM_RING 2

/******************************************************************************/

/* char *trim_str P_((const char *s)); */
void  error_msg P_((const char *m, const char *n, int e));
FILE *vfopen P_((const char *path, const char *mode));

/******************************************************************************/

static const int never = 0;

#define FREE(p) \
  do {          \
    free ((p)); \
    (p) = NULL; \
  } while (never)

/******************************************************************************/

#endif

/******************************************************************************/

