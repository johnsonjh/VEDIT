#!/usr/bin/env python3
# Patch a GENCMD-produced CP/M-86 .CMD (8080 model) so the single code group
# can grow to the full 64KB segment -- VEDIT places its edit buffer in the
# memory above the program (it reads top-of-memory from the base page), so the
# group needs G-Max = 0FFFh like the shipped product.  GENCMD's "8080" mode
# leaves G-Min = G-Length and G-Max = 0 (no growth); we set G-Min = 0,
# G-Max = 0FFFh, matching oldbin/1.34_cpm86/vedit.cmd.
#
# .CMD header = 128 bytes of up to eight 9-byte Group Descriptors:
#   off 0   G-Form  (1=code, 2=data, ...)
#   off 1-2 G-Length (paragraphs)   off 3-4 A-Base   off 5-6 G-Min   off 7-8 G-Max
import sys


def main(src, dst):
    d = bytearray(open(src, "rb").read())
    if d[0] != 1:
        sys.exit(
            "mkcmd: first group is not a code group (G-Form=%d); not an 8080-model .CMD"
            % d[0]
        )
    glen = d[1] | d[2] << 8
    d[5] = 0x00
    d[6] = 0x00  # G-Min = 0
    d[7] = 0xFF
    d[8] = 0x0F  # G-Max = 0FFFh (64KB)
    open(dst, "wb").write(d)
    print(
        "%s: code group %d paragraphs (%d bytes), min=0 max=0FFFh -> %s"
        % (dst, glen, glen * 16, dst)
    )


if __name__ == "__main__":
    main(
        sys.argv[1] if len(sys.argv) > 1 else "VPLUS.CMD",
        sys.argv[2] if len(sys.argv) > 2 else "vedit.cmd",
    )
