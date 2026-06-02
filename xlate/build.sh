#!/bin/sh
# Rebuild VEDIT-PLUS 2.33b for 16-bit x86 from the recovered CP/M-80 sources
# in ../src, using the modern translator (expand.py + xlate.py) and the
# Intel ASM-86 DOS toolchain runs under emu2.
#
#   Usage:  build.sh [dos|cpm86|dosvid|cpm86vid]   (default: dos)
#   Output: build/out-<target>/vedit.com     (MS-DOS .COM:  dos=CRT, dosvid=direct video)
#           build/out-<target>/vedit.cmd     (CP/M-86 .CMD: cpm86=CRT, cpm86vid=direct video)
#           build/out-cpm86*/vedit-cpm.com   (flat image for the DOS install.exe round-trip)
#
# Prerequisites:
#   - emu2 in PATH  (the cpm86-enabled fork, runs the DOS/CP/M-86 tools)
#   - Period build tools are vendored in ../dev (lower-case names):
#       Intel ASM-86 V3.2:   asm86.exe link86.exe loc86.exe oh86.exe
#       Digital Research:    gencmd.cmd   (CP/M-86 target only)
#     (${ASM86DIR} overrides where the Intel .exe tools are found)
#
set -e
TARGET=${1:-dos}
case "$TARGET" in dos|cpm86|dosvid|cpm86vid) ;; *) echo "usage: $0 [dos|cpm86|dosvid|cpm86vid]" >&2; exit 1 ;; esac

# Per target: CONFIG (CRT console vs DOSVID = IBM-PC B800 direct video), xlate
# OS-INTERFACE (dos = INT 21h shim, cpm86 = INT 224), package KIND (flat .COM vs
# CP/M-86 .CMD), the binary's version-FLAGS, and -- for .CMD targets -- the flat
# install-image flags presented to the DOS install.exe (CFLAGS).
case "$TARGET" in
    dos)      CFG=vedita1.cfg        ; XTARGET=dos   ; KIND=com ; VFLAGS=0x0C ;;
    dosvid)   CFG=vedita1-dosvid.cfg ; XTARGET=dos   ; KIND=com ; VFLAGS=0x08 ;;
    cpm86)    CFG=vedita1.cfg        ; XTARGET=cpm86 ; KIND=cmd ; VFLAGS=0x14 ; CFLAGS=0x0C ;;
    cpm86vid) CFG=vedita1-dosvid.cfg ; XTARGET=cpm86 ; KIND=cmd ; VFLAGS=0x10 ; CFLAGS=0x08 ;;
esac

KIT=$(cd "$(dirname "$0")" && pwd)          # this kit (xlate/)
ROOT=$(cd "$KIT/.." && pwd)                 # repo root
SRC=$ROOT/src
DEV=$ROOT/dev                               # period build tools, vendored in dev/
ASM86DIR=${ASM86DIR:-$DEV}                  # Intel ASM-86 V3.2 .exe (override with $ASM86DIR)
WORK=$ROOT/build/out-$TARGET

command -v emu2 >/dev/null || { echo "error: emu2 not on PATH" >&2; exit 1; }
for t in asm86.exe link86.exe loc86.exe oh86.exe; do
    [ -f "$ASM86DIR/$t" ] || { echo "error: $ASM86DIR/$t missing (set ASM86DIR)" >&2; exit 1; }
done

echo "[build $TARGET] staging src + config + tools -> $WORK"
rm -rf "$WORK"; mkdir -p "$WORK"
cp "$SRC"/*.asm "$SRC"/*.tbl "$WORK"/ 2>/dev/null || true
cp "$KIT/$CFG" "$WORK/vedita1.asm"          # non-interactive config (8080 model)
cp "$ASM86DIR"/asm86.exe "$ASM86DIR"/link86.exe "$ASM86DIR"/loc86.exe "$ASM86DIR"/oh86.exe "$WORK"/
cd "$WORK"

echo "[1/4] expand.py : flatten .INSERT / .DEFINE / REPT / conditionals -> 8080 stream"
python3 "$KIT/expand.py" vedplus.asm . > expanded.asm

echo "[2/4] xlate.py  : 8080 -> Intel ASM-86 (target=$XTARGET)"
python3 "$KIT/xlate.py" expanded.asm "$XTARGET" > vplus.a86

echo "[3/4] ASM86 / LINK86 / LOC86 / OH86  (Intel toolchain under emu2)"
# NOTE: emu2 writes its output files LOWERCASE (vplus.obj/lst/lnk/hex), while
# the emu2 tool ARGS match host files case-insensitively.  So pass the tools
# upper-case names but reference the lower-case outputs from host-side scripts.
sed 's/$/\r/' vplus.a86 > VPLUS.A86            # Intel ASM86 requires CRLF
emu2 asm86.exe VPLUS.A86 > asm86.log 2>&1
grep -q "NO ERRORS" asm86.log || { echo "  *** assembly errors:"; grep -i "ERROR #" vplus.lst 2>/dev/null | head; exit 1; }
emu2 link86.exe VPLUS.OBJ > link86.log 2>&1
# LOC86 returns rc1 on its benign WARNING 66 (no start addr -- expected, the
# entry is the byte at 100H); locate CODE at 0 so segment offset == address.
emu2 loc86.exe VPLUS.LNK TO VPLUS 'AD(SM(CODE(0)))' > loc86.log 2>&1 || true
emu2 oh86.exe VPLUS > oh86.log 2>&1
grep -q "NO ERRORS" oh86.log || { echo "  *** OH86 failed:"; cat oh86.log; exit 1; }

if [ "$KIND" = com ]; then         # dos + dosvid -> flat MS-DOS .COM
    echo "[4/4] mkcom.py  : Intel HEX -> flat MS-DOS .COM (entry at 100H)"
    python3 "$KIT/mkcom.py" vplus.hex vedit.com
    # Pre-set the INSTALL version-flags word (file offset 8 = runtime 0108H,
    # vedplus.asm "Multi-bit storage for INSTALL") so install.exe auto-detects the
    # build type instead of prompting.  8H=MS-DOS OS and 4H=CRT video are orthogonal
    # bits:  dos = 0Ch (Crt|MS-DOS),  dosvid = 08h (MS-DOS, no Crt = direct video).
    python3 -c "f=open('vedit.com','r+b'); f.seek(8); f.write(bytes([$VFLAGS,0])); f.close()"
    echo "      INSTALL version flags (offset 8) = $VFLAGS"
    OUT=vedit.com
else                               # cpm86 + cpm86vid -> CP/M-86 .CMD
    echo "[4/4] GENCMD -> runnable CP/M-86 .CMD  +  flat vedit-cpm.com for install.exe"
    cp "$DEV/gencmd.cmd" .; cp vplus.hex vplus.h86
    emu2 gencmd.cmd VPLUS 8080 > gencmd.log 2>&1
    python3 "$KIT/mkcmd.py" vplus.cmd vedit.cmd                  # runnable .CMD (G-Max=64KB)
    python3 -c "f=open('vedit.cmd','r+b'); f.seek(0x188); f.write(bytes([$VFLAGS,0])); f.close()"
    # The DOS install.exe configures our CP/M-86 build because it is the SAME flat
    # 8080-model image -- so emit a .COM-style image (with install.exe-recognised
    # CFLAGS) and round-trip:  emu2 install.exe vedit-cpm.com newvedit.com  then
    # wrapcmd.py wrap newvedit.com vedit-installed.cmd vedit-cpm.com  (re-wrap to .CMD).
    python3 "$KIT/mkcom.py" vplus.hex vedit-cpm.com
    python3 -c "f=open('vedit-cpm.com','r+b'); f.seek(8); f.write(bytes([$CFLAGS,0])); f.close()"
    echo "      .cmd flags (off 0188H) = $VFLAGS ;  install-image flags = $CFLAGS"
    OUT="vedit.cmd and vedit-cpm.com"
fi
echo "[build $TARGET] DONE -> $WORK  ($OUT)"
ls -l "$WORK"/vedit.* 2>/dev/null
