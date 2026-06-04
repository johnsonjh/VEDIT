#ifndef INIFILE_H
#define INIFILE_H

#include "port.h"

#define INI_RECLEN 115 /* == CRT_TABLE.len */

typedef struct {
  vbyte *buf;
  long size;
  int count; /* number of terminals */
  long name_base; /* offset of the first name */
  long namelen; /* byte length of the name block */
  long rec_base; /* offset of the first record */
} inifile;

int ini_load P_((inifile *f, const char *path)); /* 0 ok, <0 error */
int ini_save P_((const inifile *f, const char *path)); /* 0 ok */
void ini_free P_((inifile *f));
int ini_count P_((const inifile *f));
const char *ini_name P_((const inifile *f, int k)); /* pointer into buf, NULL if k out of range */
int ini_find P_((const inifile *f, const char *name)); /* exact then case-insensitive prefix; -1 */
vbyte *ini_record P_((const inifile *f, int k)); /* mutable pointer to the k-th 115-byte record */
int ini_add P_((inifile *f, const char *name, const vbyte *rec)); /* append; rec NULL = blank; -> index or <0 */

#endif
