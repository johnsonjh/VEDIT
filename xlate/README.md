# `xlate` 8080 to 8086 translation tool

A modern reimplementation of CompuView's Z80 to 8086 source translator.

It is **not** part of any historical VEDIT distribution; it is a build
aid, like the contents of [`../dev`](../dev).

## Building

One command rebuilds the whole editor from [`../src`](../src):

```sh
./build.sh           # -> ../build/out-dos/vedit.com       (MS-DOS CRT)
./build.sh dosvid    # -> ../build/out-dosvid/vedit.com    (MS-DOS PC direct)
./build.sh cpm86     # -> ../build/out-cpm86/vedit.cmd     (CP/M-86 CRT)
./build.sh cpm86vid  # -> ../build/out-cpm86vid/vedit.cmd  (CP/M-86 PC direct)
```

* Prerequisites (the period tools are run under emu2):
  * **emu2** in your `PATH`.
  * The **Intel ASM-86 V3.2** tools (`asm86.exe link86.exe loc86.exe oh86.exe`)
    and **Digital Research** `gencmd.cmd` are provided in [`../dev`](../dev);
    set `${ASM86DIR}` can override where the Intel tools are found.

* What `build.sh` does:
  1. `expand.py` flattens `.INSERT`/`.DEFINE`/`REPT`/conditionals to Z80 stream.
  2. `xlate.py expanded.asm dos` does 8080 to Intel ASM86 with the MS-DOS `INT 21h` shim.
  3. `asm86` then `link86` then `loc86` (CODE at 0 -> offset == address) then `oh86` to Intel HEX-86
  4. `mkcom.py` takes Intel HEX and makes flat MS-DOS `.COM` (loaded at 100h)

`vedita1.cfg` is `src/vedita1.asm` with the interactive install prompts answered
non-interactively (VEDIT-PLUS, "Full, 8080, CRT").  Expanding it reproduces the
committed `expanded.asm` byte-for-byte.

NOTE that `loc86` returns non-zero but is normal:
WARNING 66 (no start address so expected for a 100h image), just ignore.

`build.sh dosvid` runs the same but with `vedita1-dosvid.cfg` (8080 memory-mapped,
IBM-PC 24x80, `DOSVID=1`), it keeps the MS-DOS `INT 21h` OS interface but swaps
the CRT console for a direct-video screen backend.

`build.sh cpm86` / `cpm86vid` use the **CP/M-86** OS interface (`xlate.py ...
cpm86` -> `INT 224` BDOS) and package with `gencmd.cmd` into an 8080 model
`.CMD` (`cpm86vid` adds the same `DOSVID` direct-video backend).  Each emits
a flat `vedit-cpm.com` for the install round-trip described below.

## Running and configuring

**CRT build** (`build/out-dos/vedit.com`) does console output escapes and is
portable to any terminal:

```sh
emu2 install.exe vedit.com newvedit.com
```

**Direct-video build** (`build/out-dosvid/vedit.com`) writes directly to the
IBM-PC video memory at `B800` so it renders fast with **no** terminal install:

```sh
emu2 install.exe vedit.com newvedit.com
```

Both builds **pre-set the INSTALL flags** (file offset 8) so `install.exe`
auto-detects the type as `0Ch` (Crt|MS-DOS) or `8h` (MS-DOS) instead of
prompting "Enter a version number" (8H=MS-DOS OS and 4H=CRT video are
combined bits; the CRT build sets both, matching the shipped binary).

That the original installer accepts our from-source binaries finding every
table through the binary's own `ADDTBL` pointer chain (`DW ADDTBL` at offset
6 -> per-table pointers, never a fixed offset or byte-signature)

The binary we build here is ~5% larger than the shipped ones (44,002 vs.
41,856 bytes) that we know of from CompuView.  This is overheadfrom the 582
`INX`/`DCX` wrapped into `PUSHF ... POPF` (the 8080 16-bit inc/dec don't
touch flags, but 8086's do) and then every unconditional `JMP` forced to 3 bytes.

**CP/M-86 builds** (`build/out-cpm86/vedit.cmd`, `build/out-cpm86vid/vedit.cmd`)
tested to run under my custom emu2 CP/M-86 mode.  To *configure* one (terminal,
keyboard, colours): `install.cmd` **can't** parse the flat 8080-model image,
**but** the DOS **`install.exe` can** because it's the identical flat format, so
the build also emits `vedit-cpm.com`.

Configure on "DOS", then **re-wrap** it into a runnable `.CMD` with my emu2 fork:

```sh
emu2 install.exe vedit-cpm.com newvedit.com
python3 wrapcmd.py wrap newvedit.com vedit-installed.cmd vedit-cpm.com
emu2 vedit-installed.cmd hello.txt
```

The `wrapcmd.py` works both ways, so `wrapcmd.py unwrap <a.cmd> a.com`
actually recovers a flat image from any `.cmd` to reconfigure it again later.
