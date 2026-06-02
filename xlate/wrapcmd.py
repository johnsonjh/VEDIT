#!/usr/bin/env python3
# Wrap / unwrap the VEDIT CP/M-86 .CMD  <->  flat .com, so the DOS install.exe can
# (re)configure it any number of times -- terminal, keyboard, edit params, screen
# colours, etc.  Both are the same flat 8080-model image (only the BDOS interface
# differs); install.exe patches only OS-agnostic config tables, so the round-trip
# is faithful.  The version-flags word is mapped between the CP/M-86 (10H) bit and
# the MS-DOS (8H) bit that the DOS install.exe expects.
#
#   wrapcmd.py unwrap vedit.cmd      vedit-cpm.com               # .CMD  -> flat .com (for install.exe)
#   emu2 install.exe  vedit-cpm.com  newvedit.com                # change any options on DOS
#   wrapcmd.py wrap   newvedit.com   vedit.cmd  vedit-cpm.com    # flat .com -> runnable .CMD (+ check)
#
# A CP/M-86 8080-model .CMD is: 128-byte header + 256-byte base page + code(0x100..).
import sys


def cmd_to_install(f):
    return (f & ~0x10) | 0x08  # .cmd 0x14->0x0C (Crt), 0x10->0x08 (direct video)


def install_to_cmd(f):
    return (f & ~0x08) | 0x10  # inverse


def unwrap(cmd_path, out_path):
    cmd = open(cmd_path, "rb").read()
    flat = bytearray(cmd[0x180:])  # code from runtime 0x100 = flat .com
    assert flat[0] == 0xE9 and flat[3] == 0xE9, "no JMP/JMP entry at .CMD offset 0x180"
    flat[8], flat[9] = (
        cmd_to_install(flat[8]),
        0,
    )  # CP/M-86 flag -> MS-DOS flag for install.exe
    open(out_path, "wb").write(bytes(flat))
    print(
        "unwrap %s -> %s : %d bytes, install flags = 0x%02X"
        % (cmd_path, out_path, len(flat), flat[8])
    )


def wrap(flat_path, out_path, orig_path=None):
    flat = open(flat_path, "rb").read()
    if orig_path:
        _check(open(orig_path, "rb").read(), flat, orig_path)
    image = bytearray(256) + bytearray(
        flat
    )  # base page (runtime 0..FF) + code (0x100..)
    image[0x108], image[0x109] = (
        install_to_cmd(image[0x108]),
        0,
    )  # MS-DOS flag -> CP/M-86 flag
    glen = (len(image) + 15) // 16
    hdr = bytearray(128)
    hdr[0] = 0x01  # one 8080-model code Group Descriptor
    hdr[1], hdr[2] = glen & 0xFF, glen >> 8  # G-Length
    hdr[7], hdr[8] = 0xFF, 0x0F  # G-Max = 0FFFh (edit buffer grows to 64KB)
    open(out_path, "wb").write(bytes(hdr) + bytes(image))
    print(
        "wrap   %s -> %s : %d bytes, G-Length 0x%X, .cmd flags = 0x%02X"
        % (flat_path, out_path, 128 + len(image), glen, image[0x108])
    )


def _check(orig, flat, name):
    # confirm install.exe touched only config tables (low CRT-escape + high data
    # tables) and left the executable code byte-identical -- growth is data, not bugs.
    n = min(len(orig), len(flat))
    regs = []
    i = 0
    while i < n:
        if orig[i] != flat[i]:
            j = i
            while j < n and orig[j] != flat[j]:
                j += 1
            regs.append((i + 0x100, j - i))
            i = j
        else:
            i += 1
    code_hits = [r for r in regs if 0x600 <= r[0] < 0xA600]
    print(
        "[check] vs %s : %d changed region(s), appended tail +%d bytes"
        % (name, len(regs), max(0, len(flat) - len(orig)))
    )
    if code_hits:
        print(
            "  *** WARNING: %d change(s) in the executable-code region: %s -- inspect!"
            % (len(code_hits), [hex(o) for o, _ in code_hits[:6]])
        )
    else:
        print(
            "  OK: changes only in install tables; executable code byte-identical (not corruption)."
        )


if __name__ == "__main__":
    if len(sys.argv) >= 4 and sys.argv[1] == "unwrap":
        unwrap(sys.argv[2], sys.argv[3])
    elif len(sys.argv) >= 4 and sys.argv[1] == "wrap":
        wrap(sys.argv[2], sys.argv[3], sys.argv[4] if len(sys.argv) > 4 else None)
    else:
        sys.exit(
            "usage: wrapcmd.py unwrap <in.cmd> <out.com>          # .CMD -> flat .com for install.exe\n"
            "       wrapcmd.py wrap   <flat.com> <out.cmd> [orig]  # configured flat .com -> runnable .CMD"
        )
