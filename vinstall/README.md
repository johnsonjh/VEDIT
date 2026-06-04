# `vinstall` — a portable VEDIT configuration toolkit

A modern, portable ANSI C reimplementation of CompuView's `INSTALL` and
`INTMOD` utilities.

These are the programs that tailor a new or existing VEDIT‑PLUS binary to
a specific terminal map, keyboard, and non-default preferences, and include
the tooling to maintain the terminal database.

Like [`../xlate`](../xlate) and [`../dev`](../dev), **nothing here** is part
of any historical VEDIT source distribution.  The 2.33 utility sources remain
unrecovered as of this writing.

## The tools

|      Tools | Description                                             |
|-----------:|:--------------------------------------------------------|
| `instcore` | Dump a VEDIT binary `ADDTBL` chain                      |
|     `vcfg` | Configure an editor binary                              |
|  `vintmod` | View or modify the terminal database (*i.e.*, `INTMOD`) |

### instcore

```sh
instcore <image>
```

Follows the binary's `DW ADDTBL` pointer (starting at file offset 6) and
reports `VRSNUM`, the INSTALL version‑flags, and every config table with
its address for a flat `.COM` (8080 or 8086) or our own flat CP/M‑86 `.CMD`.

### vcfg

```sh
vcfg <image> show                        # inventory all decoded tables
vcfg <image> set <table> <field> <val>   # PYLINE / SWTBL / PRMTBL / PRNTBL
vcfg <image> flag <name> <0|1>           # version-flag bit
vcfg <image> tabs [<col> ...]            # show or set tab stops
vcfg <image> apply <install.ini> <term>  # install terminal CRT table (ADDLED)
vcfg <image> keys                        # show the keyboard layout (KEYTBL)
vcfg <image> bind <code> <hexbyte>...    # rebind a functions key
```

* `keys` decodes the special variable‑length key table
  (`[key bytes] FF [code bytes] FF`, both fields variable, compound bindings
  carry a longer code) and names each function via `KEYMSG`.

* `bind` reencodes the whole table with the chosen key replaced by a sequence
  of *any* length and moves the `KEYTBN` end pointer (the editor loads `KEYTBL`
  into a growable register at start).

* `apply` is the `INSTALL`‑clone.  The `ADDLED` escape table is encoded
  byte‑for‑byte as the 115‑byte `install.ini` record, so a chosen terminal
  is installed by just copying it in, except on direct‑video builds, which
  have no `ADDLED`.

### vintmod

```
vintmod <install.ini> list
vintmod <install.ini> show  <n|name>
vintmod <install.ini> set   <n|name> <field> <hexbyte>...
vintmod <install.ini> clr   <n|name> <0|1>  # clear-screen "no pre-clear" flag
vintmod <install.ini> add   <name>
vintmod <install.ini> clone <n|name> <newname>
```

Header‑driven, so it handles the shipped 60‑ and 64‑terminal files and any
expanded ones.  See [`crttab-format.md`](crttab-format.md) for the
`*CRT.TBL*` container and the 115‑byte per‑terminal record.

## Building

```sh
make  # builds instcore, vcfg, and vintmod
```

## Future

* Create a nice menu‑driven TUI front‑end?
* Add keystroke macro editing capability (ensure the editor grows `KEYTBL`
  in the runtime register beyond the file's reserved space).
