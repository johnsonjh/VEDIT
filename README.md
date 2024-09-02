# VEDIT / VEDIT-PLUS for CP/M

```
COMMAND: EV
VEDIT PLUS Ver. 2.33b 04/27/87
```

## Overview

This is the source code for **VEDIT** / **VEDIT-PLUS** for CP/M,
version 2.33b (04/27/87), the commercial text editing system and
text-oriented programming language, produced by Theodore Green
and Greenview Data Inc. (formerly CompuView Products, Inc.) of
Ann Arbor, Michigan.

This incarnation of VEDIT was created in 1979 and was commercially
supported until 1988 (when it was superseded by VEDIT 3) and is the
direct predecessor of the current [vEdit](https://www.vedit.com/)
product available from VEDIT, Inc.

This VEDIT / VEDIT-PLUS source code can be assembled to produce
binaries compatible with various processors (8080, Z80, etc.) and
operating systems (CP/M, MP/M, CDOS, etc.).

Historically, compatible VEDIT / VEDIT-PLUS versions were adapted
for CP/M-80, MP/M-80, CDOS, CP/M-86, MP/M-86, Concurrent CP/M-86,
SCP 86-DOS, MS-DOS, Cromix, and Turbo DOS.

Later versions were available for PC-MOS/386, CP/M-68K, IBM
4960/FlexOS, IBM OS/2, UNIX/Xenix, QNX, and Microsoft Windows (both
16-bit Windows 3.1 and 32-bit Windows 95/NT).

Companion programs (**V-PRINT**, **V-SPELL**, etc.) were also produced.

## Introduction

(*from the VEDIT User's Manual*)

VEDIT is an editor designed to take full advantage of a CRT display to
make your word processing and program development editing as fast and
easy as possible.  VEDIT's Visual Mode offers true "What you see is what
you get" type editing, which continuously displays a region of your file
on the screen and allows any changes made to the screen display to become
the changes in the file.  You can change the screen display by moving the
displayed cursor to any place in the file and then typing in new text or
typing an edit function key.  These insertions, deletions, and corrections
are immediately seen on the screen and become the changes to the file.

You can also perform the common word processing operations of wrapping
words at the end of lines and formatting paragraphs between right and
left margins.  It is easy to print any portion of the text being worked
on. Horizontal scrolling allows editing of very long lines.  Ten
scratchpad buffers may be used for extensive "cut and paste" operations.
Powerful search and selective replace operations simplify editing.
Other features, such as automatic indenting for structured programming
languages, simplify and enhance program development editing.

VEDIT also provides a very flexible and powerful Command Mode, which
serves the dual purpose of separating the less commonly used functions
from the Visual Mode, and of making VEDIT a text oriented programming
language.  Repetitive editing operations can be performed and blocks of
text may be copied or moved within the current file and other files in
an almost unlimited manner.  The extensive file handling allows multiple
files to be edited, split, and merged, other files to be viewed, and
specified portions of other files to be extracted.  The command macro
capability allows complex editing tasks to be performed automatically.
Examples of such tasks include numerous search/replace operations on
multiple files and source code translations.  The command macros can be
saved on disk for future use.  Online help is available.

You can edit files of virtually any size with little concern over the
actual size of the files.  You can also recover from common disk write
errors, such as running out of disk space, by deleting files or inserting
another disk.

Since so many different hardware configurations, keyboards, editing
applications, and personal preferences exist, VEDIT is supplied with a
customization (installation) program in order to let users create
versions of VEDIT which are most suitable to their hardware, keyboard,
applications, and preferences.

## Building

Currently, only the full VEDIT-PLUS builds (for both Z80 and 8080)
have been tested, using the Technical Design Labs, Inc. Z80
Relocating/Linking Disk Assembler (TDL ZASM) version 2.21 (1978).

The VEDIT / VEDIT-PLUS sources are closely integrated with the TDL
ZASM conditional build system, and rely heavily on this assembler's
quirks.

TDL ZASM only outputs diagnostics to the CP/M `LIST` device, usually
a line printer.  Be sure to have a list device configured and
available if you need to examine the assembler output.

*Be aware that TDL ZASM makes no real distinction between non-fatal
warnings and fatal errors in the build summary.  Some (non-fatal)
errors currently occur and are not a major concern.  Determining the
actual error severity requires examining the output sent to the
list device.*

The source code was very lightly modified to support building with
this particular version of the assembler.  These changes include
removing the `DATE` definition, and a small patch to `VEDITT3` to
expose the `HCRSOF` symbol when targeting the 8080/Z80.

The HEX output of the assembler can be directly converted to an
executable COM file using the `HEXCOM` utility.

Working versions of these tools are included in the `dev` directory of
this distribution for convenience - they are *not* an official part of
the VEDIT / VEDIT-PLUS source distribution.

* Build example:

  ```
  >ZASM VEDPLUS.ASM

  VEDIT (0) or VEDIT PLUS (1) ?: 1

  Full version, Z-80, CRT           (1)
  Full version, Z-80, Memory mapped (2)
  Full version, 8080, CRT           (3)   note:Versions 1-8 have I/O polling
  Full version, 8080, Memory mapped (4)        set on and org. @ 0000H. CRT
  Mini version, Z-80, CRT           (5)        versions are 24X80 while MEM
  Mini version, Z-80, Memory mapped (6)        mapp`d versions are 16X64.
  Mini version, 8080, CRT           (7)
  Mini version, 8080, Memory mapped (8)        CRT emulation is always ON.
  Full version, Z-80, Model II, P&T (9)
  Full version, Z-80, Model II, Gen (10)
  Full version, Z-80, Piiceon @ 90H (11)
  Other version made to custom specs.(12)
         enter a version number (1 to 12) : 3

  INCLUDE PRINT FORMATTER? (0=NO) (1=YES): 1

  INCLUDE WINDOWS? (0=NO) (1=YES): 1

  DEMO VERSION? (0=NO) (1=YES): 0

  DEVELOPMENT VERSION?  (0=NO) (1=DEVELOPMENT) (2=ALPHA) (3=BETA): 0

  PRODUCE LISTING?  (0=NO) (1=YES) (2=CUSTOMER PATCH ONLY): 0

  VEDIT
  VEDITT3
  VEDITIO
  VPLUSB1
  VEDITF1
  VEDITF2
  VEDITC1
  VPLUSE1
  VEDITC2
  VEDITC4
  VPLUSSR
  VEDIT-CP
  VPLUS-R1
  VEDITV0
  VEDITV1
  VEDITV2
  VEDITW1
  VEDITV3
  VEDITV4
  VEDITG2
   ERRORS WERE DETECTED *****

  >HEXCOM VEDPLUS

  HEXCOM  VERS: 3.00

  FIRST ADDRESS 0100
  LAST  ADDRESS 89CD
  BYTES READ    833E
  RECORDS WRITTEN 12
  ```

With any luck, you will now have a working `VEDPLUS.COM` executable.

Help files are included in the `hlp` directory.  These help files
should be verified and possibly customized to ensure the key
bindings match what you are "shipping to the customer".

## Future

* Building for CP/M-86 and DOS should be figured out and documented.

## User's Manuals

* [VEDIT User's Manual (1981)](http://www.bitsavers.org/pdf/compuview/Compuview_VEDIT_1981.pdf)
* [VEDIT User's Manual (1983)](http://www.bitsavers.org/pdf/picklesAndTrout/PT_VEDIT_Users_Manual_1983.pdf)
* [VEDIT User's Manual (1984)](http://www.bitsavers.org/pdf/compuview/VEDIT_Users_Manual_Nov84.pdf)

## Links

* [VEDIT, Inc.](https://www.vedit.com/)
* [VEDIT History](https://web.archive.org/web/20130805180830/http://vedit.com/20Years.htm)
* [Wikipedia: VEDIT](https://en.wikipedia.org/wiki/VEDIT#cite_note-2)
* [Rosetta Code: VEDIT Language](https://rosettacode.org/wiki/Category:Vedit_macro_language)
* [EDM2: VEDIT](https://web.archive.org/web/20210324052916/http://www.edm2.com/index.php/VEDIT)
