#!/usr/bin/env python3
# Intel HEX-86 (oh86 output) -> flat .COM image (the MS-DOS .COM for the dos/
# dosvid targets, and the flat install-round-trip image for the CP/M-86 targets).
# The located image places CODE at linear 0 with the program ORG'd at 100H, so
# the first emitted byte is at linear 0x100 (the .COM load address).  We map
# each byte at linear address A to file offset A-0x100.  Type-02 records set the
# extended segment base (linear = base*16 + offset16); type-00 are data; 01 EOF.
import sys

ORIGIN = 0x100


def main(hexpath, compath):
    mem = bytearray()
    base = 0
    lo = None
    hi = 0
    for ln in open(hexpath):
        ln = ln.strip()
        if not ln or ln[0] != ":":
            continue
        n = int(ln[1:3], 16)
        off = int(ln[3:7], 16)
        typ = int(ln[7:9], 16)
        data = bytes(int(ln[9 + 2 * i : 11 + 2 * i], 16) for i in range(n))
        if typ == 0x02:  # extended segment address
            base = int(ln[9:13], 16) << 4
        elif typ == 0x01:  # EOF
            break
        elif typ == 0x00:  # data
            linear = base + off
            if linear < ORIGIN:
                sys.exit("error: data below .COM origin at linear %05X" % linear)
            pos = linear - ORIGIN
            end = pos + n
            if end > len(mem):
                mem.extend(b"\0" * (end - len(mem)))
            mem[pos:end] = data
            lo = pos if lo is None else min(lo, pos)
            hi = max(hi, end)
    open(compath, "wb").write(mem)
    print(
        "%s: %d bytes (load 0100H..%04XH, entry 0100H, first byte %02X)"
        % (compath, len(mem), ORIGIN + len(mem), mem[0] if mem else 0)
    )


if __name__ == "__main__":
    main(
        sys.argv[1] if len(sys.argv) > 1 else "vplus.hex",
        sys.argv[2] if len(sys.argv) > 2 else "vedit.com",
    )
