#!/usr/bin/env python3
# Convert xlate.py's Intel ASM-86 output (.a86) to NASM (>= 2.16) source whose
# single  `nasm -f bin -Ox`  assembly reproduces the period Intel toolchain
# (ASM86 V3.2 -> LINK86 -> LOC86 `CODE(0)` -> OH86 -> mkcom.py) BYTE-FOR-BYTE.
# xlate.py keeps emitting ASM-86; this is a separate optional stage, so the
# vendored Intel tools still remain fully usable.
#
# Gemini AI produced documentation / description follows:
# Byte-identity with ASM86 V3.2 means replicating three of its one-pass
# artifacts (all derived empirically by assembling controlled cases with
# dev/asm86.exe under emu2 and reading the listing -- the same method to use
# if the source ever grows a construct this tool rejects):
#
#  1. Unconditional `JMP label` sizing (xlate's conditional-jump skips; the
#     jump TABLES are already forced to `DB 0E9H + DW` by xlate.py and need
#     nothing here):
#        backward, fits rel8  ->  EB disp8           (2 bytes)
#        forward,  fits rel8  ->  EB disp8 + NOP 90  (3 bytes, one-pass:
#                                 forward distance unknown, 3 bytes reserved)
#        otherwise            ->  E9 disp16          (3 bytes)
#     Computed as a fixpoint: emit explicit JMP SHORT/STRICT NEAR (+NOP), assemble
#     with `nasm -l`, re-derive each choice from the listing addresses,
#     repeat until stable (converges in 2 iterations in practice).
#     "STRICT NEAR" is required (rather than bare "NEAR") because NASM >=3
#     optimizes "JMP NEAR" to short form when it fits; using STRICT forces the
#     3-byte layout during trials so fit decisions are made against worst-case
#     (3-byte) preceding sizes and the fixpoint does not propose out-of-range
#     SHORTs.
#  2. A symbolic immediate not yet resolvable when ASM86 reaches the line
#     ("forward": label/EQU defined later, or an EQU whose RHS chain isn't
#     resolvable yet) reserves the WORST-CASE form -- the symbol might turn
#     out to be a memory variable -- then NOP-pads the difference:
#        MOV AL,fwd   3 bytes total, 1 NOP   (worst case A0 moffs    = 3)
#        MOV r8,fwd   4 bytes total, 2 NOPs  (worst case 8A /r d16   = 4)
#        MOV r16,fwd  4 bytes total, 1 NOP   (worst case 8B /r d16   = 4)
#        MOV AX,fwd   3 bytes total, 0 NOPs  (worst case A1 moffs    = 3)
#     Backward/numeric immediates never pad.  The destination REGISTER
#     decides, never the expression shape.  Other shapes measured but absent
#     from VEDIT (this tool errors if one ever appears, see pad_set):
#        AND/TEST/CMP/ADD AL,fwd  +2 NOPs;  ALU r8,fwd  +1;  ADD r16,fwd
#        (value fits imm8) 83-form +1;  IN/OUT/MOV AX  +0 (no memory form
#        or worst case == immediate form).
#  3. Register-register MOV/ALU have two encodings; ASM86 picks reg<-r/m
#     (8A/8B, ALU base+2/+3), NASM the r/m<-reg direction (88/89, base+0/+1).
#     NASM has no direction switch, so these are emitted as DB with the
#     mnemonic kept in a comment (1565 sites).  Everything else encodes
#     identically under -Ox: moffs A0-A3, 8B 1E direct, AL-immediate short
#     forms 24/A8/3C..., XCHG 87 /r, D0-rotates, E8 calls.
#
# NASM-dialect deltas handled here: bare symbol = its offset (strip OFFSET),
# `BYTE/WORD PTR x` -> `BYTE/WORD [x]`, `ES:[BX]` -> `[ES:BX]`, `DB/DW (n)
# DUP (?)` -> `TIMES n DB/DW 0`, NAME/SEGMENT/ASSUME/ENDS/END dropped,
# BITS 16 + CPU 8086 prepended.  NASM's EQU is a critical expression (no
# forward references), so the rare EQU alias of a later label becomes a
# %define; and NASM is case-sensitive where ASM86 is not, so identifiers are
# upper-cased outside string literals (one real mixed-case site: LL572_func).
import bisect
import re
import subprocess
import sys

IDENT = re.compile(r"[A-Za-z_$@?][A-Za-z0-9_$@?]*")
NUMLIT = re.compile(r"[0-9][0-9A-Fa-f]*[HhBbQqOo]?")
# in x86 ModRM encoding order (index == register number)
R8 = ("AL", "CL", "DL", "BL", "AH", "CH", "DH", "BH")
R16 = ("AX", "CX", "DX", "BX", "SP", "BP", "SI", "DI")
PTR_RE = re.compile(r"\b(BYTE|WORD)\s+PTR\s+(\[[^\]]+\]|[^,]+?)(\s*,|\s*$)")
DUP_RE = re.compile(r"^(DB|DW)\s+\((.*)\)\s+DUP\s+\(\?\)$")
EQU_RE = re.compile(r"^([A-Za-z_$@?][A-Za-z0-9_$@?]*)\s+EQU\s+(.*)$")
DROP_RE = re.compile(r"^(NAME\s|ASSUME\s|END$)|^CODE\s+(SEGMENT|ENDS)$")
ALU = {
    "ADD": 0x00,
    "OR": 0x08,
    "ADC": 0x10,
    "SBB": 0x18,
    "AND": 0x20,
    "SUB": 0x28,
    "XOR": 0x30,
    "CMP": 0x38,
}


def split_code_comment(line):
    """Split at the first ';' outside a '...' string literal."""
    i, n = 0, len(line)
    while i < n:
        c = line[i]
        if c == "'":
            j = line.find("'", i + 1)
            if j < 0:
                return line, ""
            i = j + 1
        elif c == ";":
            return line[:i], line[i:]
        else:
            i += 1
    return line, ""


def map_nonstring(code, fn):
    """Apply fn to the segments of code outside '...' string literals."""
    out, i, n = [], 0, len(code)
    while i < n:
        if code[i] == "'":
            j = code.find("'", i + 1)
            j = j if j >= 0 else n - 1
            out.append(code[i : j + 1])
            i = j + 1
        else:
            j = i
            while j < n and code[j] != "'":
                j += 1
            out.append(fn(code[i:j]))
            i = j
    return "".join(out)


def xform(code):
    """ASM-86 -> NASM operand syntax, string-aware."""

    def seg(s):
        s = IDENT.sub(lambda m: m.group(0).upper(), s)  # NASM is case-sensitive
        s = re.sub(r"\bOFFSET\s+", "", s)  # bare symbol = its offset in NASM
        s = s.replace("ES:[BX]", "[ES:BX]")  # seg override goes inside [ ]
        return s

    code = map_nonstring(code, seg)

    def ptr(m):
        op = m.group(2).strip()
        op = op if op.startswith("[") else "[" + op + "]"
        return "%s %s%s" % (m.group(1), op, m.group(3))

    return map_nonstring(code, lambda s: PTR_RE.sub(ptr, s))


def imm_syms(expr):
    """Symbols (not numbers, registers, '$') in an immediate expression."""
    toks = [
        t.upper()
        for t in IDENT.findall(re.sub(r"'[^']*'", "", expr))
        if not NUMLIT.fullmatch(t)
    ]
    return [t for t in toks if t not in R8 + R16 and t != "OFFSET"]


def parse(lines):
    """Lines -> records: asis/equ/label/jmp/movimm/aluimm/dual/code."""
    recs = []
    for ln, raw in enumerate(lines, 1):
        code, comment = split_code_comment(raw)
        if not code.strip():
            recs.append({"kind": "asis", "text": raw, "ln": ln})
            continue
        s = code.strip()
        if DROP_RE.match(s):
            recs.append({"kind": "asis", "text": "; [a86] " + s, "ln": ln})
            continue
        m = EQU_RE.match(s)
        if m:
            recs.append(
                {"kind": "equ", "name": m.group(1).upper(), "rhs": m.group(2), "ln": ln}
            )
            continue
        m = re.match(r"^([A-Za-z_$@?][A-Za-z0-9_$@?]*):$", s)
        if m:
            recs.append(
                {
                    "kind": "label",
                    "name": m.group(1).upper(),
                    "text": xform(code),
                    "ln": ln,
                }
            )
            continue
        m = DUP_RE.match(s)
        if m:  # DB/DW (n) DUP (?) reserves n zeroed bytes/words
            recs.append(
                {
                    "kind": "code",
                    "ln": ln,
                    "text": "\tTIMES %s %s 0" % (xform(m.group(2)), m.group(1)),
                }
            )
            continue
        m = re.match(r"^JMP\s+([A-Za-z_$@?][A-Za-z0-9_$@?]*)$", s, re.I)
        if m and m.group(1).upper() not in R16:  # JMP BX (PCHL) is plain code
            recs.append({"kind": "jmp", "target": m.group(1).upper(), "ln": ln})
            continue
        # MOV reg,immediate-expression -- the forward-padding cases (rule 2)
        m = re.match(r"^MOV\s+(%s)\s*,\s*(.+)$" % "|".join(R8 + R16), s, re.I)
        if (
            m
            and "[" not in m.group(2)
            and m.group(2).strip().upper() not in R8 + R16
            and not re.match(r"^(BYTE|WORD)\b", m.group(2).strip(), re.I)
        ):
            syms = imm_syms(m.group(2))
            if syms:
                reg = m.group(1).upper()
                npad = 0 if reg == "AX" else 1 if reg == "AL" else 2 if reg in R8 else 1
                recs.append(
                    {
                        "kind": "movimm",
                        "text": xform(code) + comment,
                        "syms": syms,
                        "npad": npad,
                        "ln": ln,
                    }
                )
                continue
        # ALU/TEST reg,imm and MOV mem,imm with symbols: no pad rule is
        # verified for these (none occur in VEDIT) -> pad_set() errors if one
        # is ever a forward reference.  Resolvable ones are plain code.
        m = re.match(
            r"^(ADD|OR|ADC|SBB|AND|SUB|XOR|CMP|TEST)\s+(%s)\s*,\s*(.+)$"
            % "|".join(R8 + R16),
            s,
            re.I,
        ) or re.match(r"^(MOV)\s+((?:BYTE|WORD)\s+PTR\s+\S+)\s*,\s*(.+)$", s, re.I)
        if m and "[" not in m.group(3) and m.group(3).strip().upper() not in R8 + R16:
            syms = imm_syms(m.group(3))
            if syms:
                recs.append(
                    {
                        "kind": "aluimm",
                        "text": xform(code) + comment,
                        "syms": syms,
                        "src": s,
                        "ln": ln,
                    }
                )
                continue
        # reg-reg MOV/ALU duals (rule 3): emit ASM86's reg<-r/m encoding as DB
        m = re.match(
            r"^(MOV|ADD|OR|ADC|SBB|AND|SUB|XOR|CMP)\s+(\w+)\s*,\s*(\w+)$", s, re.I
        )
        if m:
            op = m.group(1).upper()
            d, sr = m.group(2).upper(), m.group(3).upper()
            enc = None
            if d in R8 and sr in R8:
                enc = (
                    0x8A if op == "MOV" else ALU[op] + 2,
                    0xC0 | (R8.index(d) << 3) | R8.index(sr),
                )
            elif d in R16 and sr in R16:
                enc = (
                    0x8B if op == "MOV" else ALU[op] + 3,
                    0xC0 | (R16.index(d) << 3) | R16.index(sr),
                )
            if enc:
                recs.append(
                    {
                        "kind": "dual",
                        "ln": ln,
                        "text": "\tDB 0%02XH,0%02XH ; %s%s"
                        % (enc[0], enc[1], s, comment),
                    }
                )
                continue
        recs.append({"kind": "code", "text": xform(code) + comment, "ln": ln})
    return recs


def def_index(recs):
    """symbol -> (record index of its definition, kind, EQU RHS symbols)."""
    defs = {}
    for i, r in enumerate(recs):
        if r["kind"] == "label":
            defs.setdefault(r["name"], (i, "label", None))
        elif r["kind"] == "equ":
            defs.setdefault(r["name"], (i, "equ", imm_syms(r["rhs"])))
    return defs


def fwd_equs(recs, defs):
    """EQUs referencing a later-defined symbol -> NASM %define (EQU is a
    critical expression in NASM and may not forward-reference)."""
    out = set()
    for i, r in enumerate(recs):
        if r["kind"] != "equ":
            continue
        for t in imm_syms(r["rhs"]):
            d = defs.get(t)
            if d and d[0] > i:
                out.add(i)
                break
    return out


def pad_set(recs, defs):
    """Record indices of movimm lines ASM86 NOP-pads (rule 2), i.e. whose
    immediate isn't resolvable yet at that line.  Errors out on a forward
    symbolic immediate with no verified pad rule (aluimm)."""

    def resolvable(sym, at, depth=0):
        if depth > 10 or sym == "$":
            return True
        d = defs.get(sym)
        if d is None:  # undefined: NASM will fault it with a real message
            return True
        i, kind, syms = d
        if i > at:
            return False
        if kind == "label":
            return True
        return all(resolvable(t, at, depth + 1) for t in syms)

    pads, errors = set(), []
    for i, r in enumerate(recs):
        if r["kind"] == "movimm":
            if not all(resolvable(t, i) for t in r["syms"]):
                pads.add(i)
        elif r["kind"] == "aluimm":
            if not all(resolvable(t, i) for t in r["syms"]):
                errors.append("line %d: `%s`" % (r["ln"], r["src"]))
    if errors:
        sys.exit(
            "a86tonasm: forward-referenced immediate in a shape with no "
            "verified ASM86 pad rule (measure it with dev/asm86.exe and "
            "extend pad_set -- see header):\n  " + "\n  ".join(errors)
        )
    return pads


def emit(recs, choices, fwd, pads):
    """Render NASM source.  Returns (lines, jmp line numbers, label lines)."""
    lines = ["BITS 16", "CPU 8086"]
    jmpline, labelline = {}, {}
    for i, r in enumerate(recs):
        k = r["kind"]
        if k == "equ":
            if i in fwd:
                lines.append("%%define %s (%s)" % (r["name"], xform(r["rhs"])))
            else:
                lines.append("%-7s EQU %s" % (r["name"], xform(r["rhs"])))
        elif k == "label":
            labelline[r["name"]] = len(lines) + 1  # 1-based; emits next line
            lines.append(r["text"])
        elif k == "jmp":
            jmpline[i] = len(lines) + 1
            ch = choices.get(i, "near")
            lines.append(
                "\tJMP %s %s" % ("SHORT" if ch != "near" else "STRICT NEAR", r["target"])
            )
            if ch == "shortnop":
                lines.append("\tNOP")
        elif k == "movimm":
            lines.append(r["text"])
            if i in pads:
                lines.extend(["\tNOP"] * r["npad"])
        else:  # asis / code / aluimm / dual
            lines.append(r["text"])
    return lines, jmpline, labelline


def assemble(nasm, path, binout, lstout):
    p = subprocess.run(
        [nasm, "-f", "bin", "-Ox", path, "-o", binout, "-l", lstout],
        capture_output=True,
        text=True,
    )
    if p.returncode:
        sys.exit("a86tonasm: nasm failed on %s:\n%s" % (path, p.stderr[:4000]))
    if p.stderr.strip():
        sys.stderr.write(p.stderr)


def listing_addrs(lst):
    """Listing line number -> offset.  Label-only lines print no address, so
    a label's address is that of the first addressed line at/after it."""
    addr = {}
    for ln in open(lst, errors="replace"):
        m = re.match(r"^\s*(\d+) ([0-9A-F]{8}) ", ln)
        if m:
            addr.setdefault(int(m.group(1)), int(m.group(2), 16))
    return addr


def fixpoint(recs, fwd, pads, out, nasm):
    """Iterate JMP SHORT/NEAR choices until they reproduce ASM86's rule 1."""
    labrec = {}
    for i, r in enumerate(recs):
        if r["kind"] == "label":
            labrec.setdefault(r["name"], i)
    for i, r in enumerate(recs):
        if r["kind"] == "jmp" and r["target"] not in labrec:
            sys.exit(
                "a86tonasm: line %d: JMP target %s is not a label"
                % (r["ln"], r["target"])
            )
    scratch_bin, scratch_lst = out + ".fix.bin", out + ".fix.lst"
    choices = {}
    for it in range(1, 17):
        lines, jmpline, labelline = emit(recs, choices, fwd, pads)
        open(out, "w").write("\n".join(lines) + "\n")
        assemble(nasm, out, scratch_bin, scratch_lst)
        addr = listing_addrs(scratch_lst)
        lns = sorted(addr)
        laddr = {}
        for name, ln in labelline.items():
            k = bisect.bisect_left(lns, ln)
            laddr[name] = addr[lns[k]] if k < len(lns) else None
        nc = {}
        for i, r in enumerate(recs):
            if r["kind"] != "jmp":
                continue
            a, t = addr.get(jmpline[i]), laddr.get(r["target"])
            if a is None or t is None:
                sys.exit(
                    "a86tonasm: line %d: no listing address for JMP %s"
                    % (r["ln"], r["target"])
                )
            disp = t - (a + 2)  # rel8 is from the end of a 2-byte EB jump
            if labrec[r["target"]] < i:  # backward: short iff it fits
                nc[i] = "short" if disp >= -128 else "near"
            else:  # forward: 3 bytes either way (EB+NOP if it fits, else E9)
                nc[i] = "shortnop" if disp <= 127 else "near"
        if nc == choices:
            import os

            os.remove(scratch_bin)
            os.remove(scratch_lst)
            return it, choices
        choices = nc
    sys.exit("a86tonasm: JMP sizing did not converge after 16 iterations")


def main(src, out, nasm="nasm"):
    lines = open(src, errors="replace").read().splitlines()
    recs = parse(lines)
    defs = def_index(recs)
    fwd = fwd_equs(recs, defs)
    pads = pad_set(recs, defs)
    iters, choices = fixpoint(recs, fwd, pads, out, nasm)
    n = {}
    for c in choices.values():
        n[c] = n.get(c, 0) + 1
    sys.stderr.write(
        "[a86tonasm] %d lines -> %s: %d jmps (%d short, %d short+nop, %d near),"
        " %d nop-padded fwd-imm, %d dual-encoded reg-reg, %d %%define,"
        " fixpoint %d iters\n"
        % (
            len(lines),
            out,
            len(choices),
            n.get("short", 0),
            n.get("shortnop", 0),
            n.get("near", 0),
            len(pads),
            sum(1 for r in recs if r["kind"] == "dual"),
            len(fwd),
            iters,
        )
    )


if __name__ == "__main__":
    if len(sys.argv) < 3:
        sys.exit(
            "usage: a86tonasm.py <in.a86> <out.nasm> [nasm-binary]\n"
            "then:  nasm -f bin -Ox <out.nasm> -o <flat-image>"
        )
    main(sys.argv[1], sys.argv[2], sys.argv[3] if len(sys.argv) > 3 else "nasm")
