#!/usr/bin/env python3
# VEDIT 8080 -> 8086 translator (Path B, stage 2).
# Input: flat 8080 source from expand.py.  Output: Intel ASM-86 (.A86) in the
# 8080 memory model (CS=DS=ES=SS, ORG 100H), assembled by Intel ASM-86 V3.2.
# The CP/M `CALL 5` BDOS dispatch is retargeted per OS (see `target` below):
#   'dos' -> INT 21h shim (flat MS-DOS .COM);  'cpm86' -> INT 224 (CP/M-86 .CMD).
# Faithful modern reimplementation of zilint/z80-8086.vdm's documented rules:
#   regs A->AL B->CH C->CL D->DH E->DL H->BH L->BL  M->[BX]  SP->SP
#        BC->CX DE->DX HL->BX  PSW->AX (LAHF/SAHF)
#   cond jumps/calls/rets -> inverse-skip + near JMP/CALL/RET (8086 short range)
import sys, re

R8 = {"A": "AL", "B": "CH", "C": "CL", "D": "DH", "E": "DL", "H": "BH", "L": "BL"}
R16 = {"B": "CX", "D": "DX", "H": "BX", "SP": "SP", "PSW": "AX"}


def r8(r):
    return R8.get(r.upper().strip(), r)


def r16(r):
    return R16.get(r.upper().strip(), r)


def split_fields(line):
    """Return (label, op, operand). Label only if col 1 non-space."""
    label = ""
    if line[:1] not in (" ", "\t", ""):
        m = re.match(r"^([A-Za-z_.%$][A-Za-z0-9_.%$]*:?)(.*)$", line)
        if m and (m.group(1).endswith(":") or re.match(r"\s*==?", m.group(2))):
            label = m.group(1)
            line = m.group(2)
    s = line.strip()
    if not s:
        return label, "", ""
    parts = s.split(None, 1)
    return label, parts[0], (parts[1] if len(parts) > 1 else "")


class Xlate:
    def __init__(self, lines, target="dos"):
        self.lines = lines
        self.target = target  # OS-interface target: 'dos' (.COM/INT 21h) or 'cpm86' (.CMD/INT 224)
        self.labels = set()  # address labels (NAME:)
        self.equ = set()  # equated constants (NAME =)
        self.out = []
        self.scope = 0  # current local-label scope (reset at each global label)
        self.lineno = 0  # index into self.lines during translate()
        self.equ_last = {}  # equate name -> index of its last definition
        self.addr_equ = set()  # equates that alias an address (need OFFSET)
        self.defined = set()  # labels already emitted (for dup rename)
        self.dupct = 0
        self.skipn = 0  # counter for conditional-jump skip labels
        self.vtable = None  # name of the vector table currently being emitted
        self.vscn = 0  # counter for direct-video VSPUTC skip labels

    def localize(self, s):
        # TDL local labels ..name -> unique LLscope_name (DR ASM-86 has no ..locals)
        return re.sub(
            r"\.\.([A-Za-z0-9]+)", lambda m: "LL%d_%s" % (self.scope, m.group(1)), s
        )

    def scan(self):
        equ_rhs = {}
        for i, ln in enumerate(self.lines):
            m = re.match(r"^([A-Za-z_.%$][A-Za-z0-9_.%$]*):", ln)
            if m:
                self.labels.add(m.group(1).upper())
            m = re.match(r"^([A-Za-z_.%$][A-Za-z0-9_.%$]*)\s*==?\s*(.*)$", ln)
            if m:
                self.equ.add(m.group(1).upper())
                self.equ_last[m.group(1).upper()] = (
                    i  # last definition wins (Intel EQU is define-once)
                )
                equ_rhs[m.group(1).upper()] = m.group(2)
        # classify equates that ALIAS an address (RHS references a label or an
        # address-equate) -> these need OFFSET in LXI just like labels do.
        # address-like ONLY if RHS is a single label/addr-equate (+ optional
        # constant): TRVCMD=VERSMS, STRBUF==BSTACK, STRBEN==STRBUF+40.
        # NOT a label difference (EHDLEN=EHDEND-EHDBEG is a length = constant).
        self.addr_equ = set()
        single = re.compile(
            r"^\s*([A-Za-z_.%$][A-Za-z0-9_.%$]*)\s*(\+\s*[0-9A-Fa-f]+[Hh]?\s*)?$"
        )
        changed = True
        while changed:
            changed = False
            for n, r in equ_rhs.items():
                if n in self.addr_equ:
                    continue
                m = single.match(r)
                if m and (
                    m.group(1).upper() in self.labels
                    or m.group(1).upper() in self.addr_equ
                ):
                    self.addr_equ.add(n)
                    changed = True

    def add_offset(self, expr):
        """Prefix OFFSET to address-label references in a 16-bit immediate."""

        def repl(m):
            t = m.group(0)
            if t.upper() in self.labels or t.upper() in self.addr_equ:
                return "OFFSET " + t
            return t

        return re.sub(r"[A-Za-z_.%$][A-Za-z0-9_.%$]*", repl, expr)

    def mem(self, operand, size):
        """Direct memory operand; add size PTR if it's a bare label/expr that
        ASM-86 might mis-size. Keep simple: rely on register size, but force
        WORD/BYTE PTR for indexed [BX]."""
        return operand

    RESV = re.compile(r"(?<![A-Za-z0-9_%$@.])(COMMON|ESC|STACK)(?![A-Za-z0-9_%$@.])")

    # 8080 opcode bytes stored as data for self-modifying code, remapped to the
    # 8086 opcode that has the same effect.
    OPCODE_REMAP = {"RETINS": "0C3H"}  # 8080 RET 0C9H -> 8086 near RET 0C3H

    def fix_charlit(self, s):
        # single-char literal with a control char or '%' -> numeric (Intel
        # ASM86 rejects TAB/control chars and treats '%' as macro-eval).
        if len(s) == 3 and s[0] == "'" and s[2] == "'":
            return str(
                ord(s[1])
            )  # single-char literal -> numeric (robust vs \, space, %, ctrl)
        return self.split_pct(s)

    def split_pct(self, s):
        # Intel ASM86 treats '%' as its macro-eval char even inside quotes.
        # 'a%b' -> 'a',25H,'b' ;  '%' -> 25H
        if "%" not in s:
            return s
        parts = s[1:-1].split("%")
        out = []
        for k, p in enumerate(parts):
            if p:
                out.append("'%s'" % p)
            if k < len(parts) - 1:
                out.append("25H")
        return ",".join(out) if out else "25H"

    def sanitize(self, line):
        # Make symbols legal for Intel ASM86, OUTSIDE string literals:
        #   reserved-word collisions -> append '#'   (e.g. ESC -> ESC#)
        #   '%' (illegal in idents)  -> '@'
        #   standalone '.' (TDL loc counter) -> '$'
        res = ""
        i = 0
        n = len(line)
        while i < n:
            if line[i] == "'":
                j = line.find("'", i + 1)
                j = j if j >= 0 else n - 1
                res += self.fix_charlit(line[i : j + 1])
                i = j + 1
            else:
                j = i
                while j < n and line[j] != "'":
                    j += 1
                seg = line[i:j]
                seg = self.RESV.sub(
                    lambda m: m.group(1) + "X", seg
                )  # ESC -> ESCX (legal, non-reserved)
                seg = seg.replace("%", "@")
                seg = re.sub(r"(?<![A-Za-z0-9_%$@.])\.(?![A-Za-z0-9_%$@.])", "$", seg)
                res += seg
                i = j
        return res

    def emit(self, s):
        self.out.append(self.sanitize(s))

    def uniq_label(self, label):
        # rename duplicate label definitions (pre-existing source quirks like
        # PCMD/PRCMD) so Intel ASM86 doesn't error; refs resolve to the first.
        # Also rename a label that collides with an EQUATE of the same name:
        # EOCMD is both `EOCMD = PRCMD` and a later `EOCMD:` label with NO
        # references after the label, so the equate (-> PRCMD) wins and the
        # stray fall-through label is renamed.
        base = label[:-1] if label.endswith(":") else label
        if base in self.defined or base.upper() in self.equ:
            self.dupct += 1
            return "%sDUP%d:" % (base, self.dupct)
        self.defined.add(base)
        return base + ":"

    def emitl(self, label, s):
        if label:
            self.out.append(label if not s else "%-7s %s" % (label, s))
        elif s:
            self.out.append("\t" + s)

    def translate(self):
        self.scan()
        # header: single segment, 8080 model (CS=DS=ES=SS), .COM-style ORG 100H.
        # CALL 5 BDOS works on both MS-DOS (PSP:5) and CP/M-86 8080 model.
        self.emit(
            "; VEDIT-PLUS  8086 translation (8080 model, Intel ASM86), target=%s"
            % self.target
        )
        self.emit("; auto-generated by xlate.py from expand.py output")
        self.emit("\tNAME VEDITPLUS")
        self.emit("CODE\tSEGMENT")
        self.emit("\tASSUME\tCS:CODE,DS:CODE,ES:CODE,SS:CODE")
        for i, raw in enumerate(self.lines):
            self.lineno = i
            self.do_line(raw)
        self.emit("CODE\tENDS")

    def do_line(self, raw):
        label, op, opnd = split_fields(raw)
        U = op.upper()
        # local-label scoping: a global label opens a new scope
        if label.endswith(":") and not label.startswith(".."):
            self.scope += 1
        label = self.localize(label)
        opnd = self.localize(opnd)
        # vector-table tracking: CPMTBL/MPMTBL are BYTE-COPIED to CONSTA at
        # runtime (SETIO/MOVE12) to select the console backend.  Their JMP
        # entries must be pre-relocated for the CONSTA destination (a copied
        # rel16 jump is otherwise wrong -- 8086 near jumps aren't position-
        # independent, unlike 8080's absolute JMP).  These tables are only ever
        # copied, never executed in place, so pre-relocation is safe.
        lb = (label[:-1] if label.endswith(":") else label).upper()
        if lb in ("CPMTBL", "MPMTBL"):
            self.vtable = lb
        elif label:  # any other label ends the table
            self.vtable = None
        # ---- equate ---------------------------------------------------
        meq = re.match(r"^([A-Za-z_.%$][A-Za-z0-9_.%$]*)\s*==?\s*(.*)$", raw.strip())
        if (
            meq
            and "=" in raw.split(";")[0]
            and not raw.lstrip().startswith(("DB", "DW", "DC", "DS"))
        ):
            name = meq.group(1).upper()
            # Intel EQU is define-once: emit only the LAST definition of each name
            if self.equ_last.get(name, self.lineno) == self.lineno:
                rhs = self.localize(meq.group(2))
                # 8086 opcode-byte remap for self-modifying code: the source
                # stores instruction opcodes as data (e.g. MVI M,RETINS patches
                # a routine to start with a RET).  These are 8080 opcode values;
                # remap to the 8086 equivalent.  RETINS: 8080 RET 0C9H -> 8086
                # near RET 0C3H (0C9H is LEAVE on the 80186+ that emu2 runs).
                if name in self.OPCODE_REMAP:
                    rhs = self.OPCODE_REMAP[name]
                self.emit("%-7s EQU %s" % (meq.group(1), rhs))
            return
        if op == "" and label:
            self.emit(self.uniq_label(label))
            return
        if op == "":
            return

        a = (
            self.split_args(opnd) if opnd else []
        )  # quote-aware (handles ',' char literal)

        out = self.xop(U, a, opnd)
        if out is None:
            out = ["; ??? " + raw.strip()]
        if isinstance(out, str):
            out = [out]
        if label:
            self.emit(self.uniq_label(label))
        for o in out:
            self.emit("\t" + o)

    def split_args(self, s):
        # split on top-level commas, skipping quotes and (), so a ',' char
        # literal or grouped operand isn't broken.
        out = []
        d = 0
        cur = ""
        i = 0
        while i < len(s):
            c = s[i]
            if c == "'":
                j = s.find("'", i + 1)
                j = j if j >= 0 else len(s) - 1
                cur += s[i : j + 1]
                i = j + 1
                continue
            if c in "([":
                d += 1
                cur += c
            elif c in ")]":
                d -= 1
                cur += c
            elif c == "," and d == 0:
                out.append(cur.strip())
                cur = ""
            else:
                cur += c
            i += 1
        if cur.strip() or out:
            out.append(cur.strip())
        return out

    def bp(self, opnd, sz):
        # direct memory operand with size PTR (operand is a defined data label,
        # reachable from DS).
        return "%s PTR %s" % (sz, opnd)

    def is_basepage(self, opnd):
        # True if operand is an absolute base-page address (not a data label):
        # BASE, BASE+1, BASE+6, DEFDMA+080H, DEFFCB, EXIT, or a bare number.
        # Intel ASM86 can't segment-resolve such absolute constants ("operand
        # not reachable from segment registers"), so LDA/STA/LHLD/SHLD to them
        # are addressed register-indirect through SI -- DS-relative, which is
        # exactly right since the base page IS at DS:0 in the .COM/8080 model
        # (DS=PSP).  SI is otherwise unused by the 8080->8086 register mapping.
        syms = []
        for t in re.findall(r"[A-Za-z0-9_.%$@]+", opnd):
            if re.fullmatch(
                r"[0-9][0-9A-Fa-f]*[Hh]?", t
            ):  # numeric literal (80, 080H, 5CH)
                continue
            syms.append(t)
        if not syms:
            return True  # bare numeric address
        return all(s.upper() in ("BASE", "DEFDMA", "DEFFCB", "EXIT") for s in syms)

    def is_bdos(self, opnd):
        # CP/M BDOS dispatch target: JMP 5 (BASE+5).
        return re.sub(r"\s+", "", opnd).upper() in ("BASE+5", "BASE+05", "BASE+0005")

    def xop(self, U, a, opnd):
        # ---- data movement ------------------------------------------
        if U == "MOV":
            d, s = a[0], a[1]
            dd = "[BX]" if d.upper() == "M" else r8(d)
            ss = "[BX]" if s.upper() == "M" else r8(s)
            return "MOV %s,%s" % (dd, ss)
        if U == "MVI":
            d, s = a[0], a[1]
            dd = "BYTE PTR [BX]" if d.upper() == "M" else r8(d)
            return "MOV %s,%s" % (dd, s)
        if U == "LXI":
            return "MOV %s,%s" % (r16(a[0]), self.add_offset(a[1]))
        if U == "LDA":
            if self.is_basepage(opnd):
                return ["MOV SI,%s" % opnd, "MOV AL,BYTE PTR [SI]"]
            return "MOV AL,%s" % self.bp(opnd, "BYTE")
        if U == "STA":
            if self.is_basepage(opnd):
                return ["MOV SI,%s" % opnd, "MOV BYTE PTR [SI],AL"]
            return "MOV %s,AL" % self.bp(opnd, "BYTE")
        if U == "LHLD":
            if self.is_basepage(opnd):
                return ["MOV SI,%s" % opnd, "MOV BX,WORD PTR [SI]"]
            return "MOV BX,%s" % self.bp(opnd, "WORD")
        if U == "SHLD":
            if self.is_basepage(opnd):
                return ["MOV SI,%s" % opnd, "MOV WORD PTR [SI],BX"]
            return "MOV %s,BX" % self.bp(opnd, "WORD")
        if U == "LDAX":
            return ["MOV DI,%s" % r16(a[0]), "MOV AL,[DI]"]
        if U == "STAX":
            return ["MOV DI,%s" % r16(a[0]), "MOV [DI],AL"]
        if U == "XCHG":
            return "XCHG DX,BX"
        if U == "XTHL":
            return ["MOV DI,SP", "XCHG [DI],BX"]
        if U == "SPHL":
            return "MOV SP,BX"
        if U == "PCHL":
            return "JMP BX"
        # ---- 16-bit arith -------------------------------------------
        if U == "DAD":
            return "ADD BX,%s" % r16(a[0])
        if U == "INX":
            return ["PUSHF", "INC %s" % r16(a[0]), "POPF"]
        if U == "DCX":
            return ["PUSHF", "DEC %s" % r16(a[0]), "POPF"]
        if U == "INR":
            return "INC %s" % ("BYTE PTR [BX]" if a[0].upper() == "M" else r8(a[0]))
        if U == "DCR":
            return "DEC %s" % ("BYTE PTR [BX]" if a[0].upper() == "M" else r8(a[0]))
        # ---- 8-bit ALU (reg) ----------------------------------------
        alu = {
            "ADD": "ADD",
            "ADC": "ADC",
            "SUB": "SUB",
            "SBB": "SBB",
            "ANA": "AND",
            "XRA": "XOR",
            "ORA": "OR",
            "CMP": "CMP",
        }
        if U in alu:
            s = "[BX]" if a[0].upper() == "M" else r8(a[0])
            return "%s AL,%s" % (alu[U], s)
        alui = {
            "ADI": "ADD",
            "ACI": "ADC",
            "SUI": "SUB",
            "SBI": "SBB",
            "ANI": "AND",
            "XRI": "XOR",
            "ORI": "OR",
            "CPI": "CMP",
        }
        if U in alui:
            return "%s AL,%s" % (alui[U], opnd)
        if U == "CMA":
            return "NOT AL"
        if U == "NEG":
            return "NEG AL"
        if U == "INRA":
            return "INC AL"
        rot = {"RLC": "ROL", "RRC": "ROR", "RAL": "RCL", "RAR": "RCR"}
        if U in rot:
            return "%s AL,1" % rot[U]
        if U == "STC":
            return "STC"
        if U == "CMC":
            return "CMC"
        if U == "DAA":
            return "DAA"
        # ---- stack --------------------------------------------------
        if U == "PUSH":
            if a[0].upper() == "PSW":
                return ["LAHF", "PUSH AX"]
            return "PUSH %s" % r16(a[0])
        if U == "POP":
            if a[0].upper() == "PSW":
                return ["POP AX", "SAHF"]
            return "POP %s" % r16(a[0])
        # ---- jumps / calls / rets -----------------------------------
        if U == "JMP" and opnd.strip().upper() in ("EXIT", "BASE"):
            # CP/M warm-boot (JMP 0):
            if self.target == "cpm86":
                return ["MOV CL,0", "INT 0E0H"]  # CP/M-86 system reset (BDOS func 0)
            return "INT 20H"  # MS-DOS terminate
        if U == "JMP" and self.is_bdos(opnd):
            # CP/M BDOS dispatch (JMP 5).  VEDIT calls with CL=function, DX=param.
            if self.target == "cpm86":
                # CP/M-86 BDOS is INT 224 (0E0H) with the SAME CL/DX convention,
                # and returns AL (byte) + BX (word) matching the 8080 A/HL -- so
                # the dispatch is a direct INT 0E0H.  No function-number remap or
                # interception: 12 (version) and 32 (user) are native CP/M here.
                # Reached via `JMP BDOS`; the trailing RET returns to the caller.
                return ["INT 0E0H", "RET"]
            # MS-DOS: INT 21h shim.  The CP/M function numbers VEDIT uses (1-9
            # console, 13-26 disk/FCB) are identical under DOS and pass straight
            # through.  Two collide with INCOMPATIBLE DOS calls and are
            # intercepted: function 12 (CP/M get-version -- but DOS 0Ch = flush
            # buffer + invoke input, which would block at startup) and function
            # 32 (CP/M get/set user code -- but DOS 20h is reserved).  The byte
            # result is mirrored to HL (BX=0:AL) as CP/M BDOS does.
            return [
                "CMP CL,12",
                "JNE $+5",
                "JMP BDOSF12",
                "CMP CL,32",
                "JNE $+5",
                "JMP BDOSF32",
                "MOV AH,CL",
                "INT 21H",
                "MOV BL,AL",
                "MOV BH,0",
                "RET",
                "BDOSF12:",
                "MOV BX,0022H",
                "RET",  # report CP/M 2.2 (H=0)
                "BDOSF32:",
                "MOV AX,0",
                "MOV BX,0",
                "RET",
            ]  # user 0, no-op
        if U == "JMP" and opnd.strip() in (".", "$"):
            # JMP . (jump-to-self) -- a 3-byte jump-table SLOT that SETIO/MOVE12
            # overwrites at runtime (CONSTA/CONINA/CONOTA/LSTOTA console
            # vectors).  Must stay 3 bytes; ASM86 shrinks `JMP $` to EB FE.
            return ["DB 0E9H", "DW 0FFFDH"]  # E9 FD FF = near JMP to self
        if U == "JMP" and self.vtable:
            # Vector-table entry (CPMTBL/MPMTBL) byte-copied to CONSTA.  Encode
            # the rel16 so it is correct AFTER the copy: target - (dest+3) where
            # dest = CONSTA + (this entry's offset in the table).  $ is the DW's
            # address, dest-DW = $ + (CONSTA - table), so disp = target -
            # ($ + (CONSTA-table) + 2).
            return ["DB 0E9H", "DW (%s)-($+(CONSTA-%s)+2)" % (opnd, self.vtable)]
        if U == "JMP":
            # Force a 3-byte NEAR jump.  The 8080 model needs EVERY unconditional
            # JMP to be 3 bytes: jump tables (e.g. CPMTBL, copied by MOVE12's
            # fixed 12-byte move and indexed by 3-byte stride) depend on it.
            # ASM86 otherwise shrinks backward/short-range JMPs to 2-byte EB --
            # and `NEAR PTR` does NOT override that -- so emit the raw E9+rel16.
            return ["DB 0E9H", "DW (%s)-($+2)" % opnd]
        if U == "CALL":
            return "CALL %s" % opnd
        if U == "RET":
            return "RET"
        # conditional jumps: inverse-skip + near JMP
        # inverse-condition map (the SKIP condition, opposite of the 8080 jump).
        # 8080 JP=Plus(SF=0)=8086 JNS, JM=Minus(SF=1)=8086 JS -- so the *inverse*
        # used for the skip is JP->JS and JM->JNS (NOT JP->JNS/JM->JS, which
        # would be the same condition and invert the whole branch).
        cj = {
            "JZ": "JNE",
            "JNZ": "JE",
            "JC": "JNB",
            "JNC": "JB",
            "JP": "JS",
            "JM": "JNS",
            "JPE": "JNP",
            "JPO": "JP",
        }
        if U in cj:
            # 8080 cond jump (far range) -> inverse-condition skip over a near
            # JMP.  The skip MUST target an explicit label, NOT $+5: Intel ASM86
            # assembles a *backward* JMP as a 2-byte short jump (EB rel8) with
            # no NOP padding, while a forward JMP is 3 bytes (E9, or EB rel8 +
            # NOP) -- so a fixed $+5 overshoots backward jumps by one byte and
            # lands mid-instruction.  A label is size-independent.
            self.skipn += 1
            lbl = "JSKIP%d" % self.skipn
            return ["%s %s" % (cj[U], lbl), "JMP %s" % opnd, lbl + ":"]
        cc = {
            "CZ": "JNZ",
            "CNZ": "JZ",
            "CC": "JNB",
            "CNC": "JB",
            "CP": "JS",
            "CM": "JNS",
            "CPE": "JNP",
            "CPO": "JP",
        }
        if U in cc:
            return ["%s $+5" % cc[U], "CALL %s" % opnd]
        cr = {
            "RZ": "JNZ",
            "RNZ": "JZ",
            "RC": "JNB",
            "RNC": "JB",
            "RP": "JS",
            "RM": "JNS",
            "RPE": "JNP",
            "RPO": "JP",
        }
        if U in cr:
            return ["%s $+3" % cr[U], "RET"]
        # ---- I/O / misc ---------------------------------------------
        if U == "IN":
            return "IN AL,%s" % opnd
        if U == "OUT":
            return "OUT %s,AL" % opnd
        if U == "EI":
            return "STI"
        if U == "DI":
            return "CLI"
        if U == "HLT":
            return "HLT"
        if U == "NOP":
            return "NOP"
        # ---- direct-video (IBM-PC B800) screen pseudo-ops --------------
        # These appear ONLY in the DOSVID-configured memory-mapped screen
        # module (veditm2/m3, gated `IF DOSVID`); they are absent from the
        # dos/cpm86 builds.  The 8080->8086 register map puts the screen
        # pointer (HL=SCRPNT) in BX, so screen cells are ES:[BX].  ES is set
        # once to 0B800H (VESSEG) and never used implicitly by the codegen.
        # A cell is 2 bytes: char (AL) + attribute (AH).  Reverse video is
        # carried as bit 7 of the char (VEDIT's MM convention) and mapped to
        # the IBM-PC attribute 70H (else 07H), with the glyph masked to 7 bits.
        if U == "VESSEG":  # ES = screen segment from SCRBAS
            # SCRBAS is the screen SEGMENT (install "Address of screen"); the
            # cell offset is computed from 0 (VERSC1 omits the base for DOSVID).
            return ["MOV AX,WORD PTR SCRBAS", "MOV ES,AX"]
        if U == "VMODE3":  # set 80x25 colour text mode (BIOS)
            # Once at startup: puts the PC (and emu2) into 80x25 text video so
            # the B800 framebuffer is actually displayed; also clears the screen.
            return ["MOV AX,0003H", "INT 10H"]
        if U == "VSPUTC":  # ES:[BX] = char(AL) + ATTRIB
            # Cell attribute = ATTRIB, the editor's current character attribute,
            # set per context by ATTSET (ATTFOR->WWFRAT text, status->SSTAAT,
            # ATTBRD->SBRDAT border).  Strip the glyph's bit 7 (the MM reverse
            # bit is now carried by the attribute byte, not the char).
            return ["MOV AH,BYTE PTR ATTRIB", "AND AL,7FH", "MOV ES:[BX],AX"]
        if U == "VSPUTE":  # ES:[BX] = char(AL) + WWBKAT (erase)
            return ["MOV AH,BYTE PTR WWBKAT", "AND AL,7FH", "MOV ES:[BX],AX"]
        if U == "VSPUTK":  # ES:[BX] = char(AL) + cursor attribute
            # Cursor/highlight cell (glyph bit 7 set): CURATR if non-zero, else
            # the reverse (nibble-swap) of ATTRIB.  Plain cells (bit 7 clear) use
            # ATTRIB, so CURSO5 restores the underlying cell when the cursor moves.
            self.vscn += 1
            a, b = "VSK%dA" % self.vscn, "VSK%dB" % self.vscn
            return [
                "MOV AH,BYTE PTR ATTRIB",
                "TEST AL,80H",
                "JZ %s" % b,
                "MOV AH,BYTE PTR CURATR",
                "OR AH,AH",
                "JNZ %s" % a,
                "MOV AH,BYTE PTR ATTRIB",
                "ROL AH,1",
                "ROL AH,1",
                "ROL AH,1",
                "ROL AH,1",
                a + ":",
                b + ":",
                "AND AL,7FH",
                "MOV ES:[BX],AX",
            ]
        if U == "VSGETC":  # AL = char byte at ES:[BX]
            return "MOV AL,ES:[BX]"
        if U == "VSETCUR":  # BIOS hardware cursor <- PHYVER
            # PHYVER word = (col<<8)|row; INT 10H AH=02 wants DH=row,DL=col.
            return [
                "PUSH AX",
                "PUSH BX",
                "PUSH DX",
                "MOV AX,WORD PTR PHYVER",
                "XCHG AL,AH",
                "MOV DX,AX",
                "MOV BH,0",
                "MOV AH,2",
                "INT 10H",
                "POP DX",
                "POP BX",
                "POP AX",
            ]
        if U == "VDSSCR":  # DS = screen seg (for RTLDIR scroll)
            return ["PUSH DS", "PUSH ES", "POP DS"]
        if U == "VDSRST":  # restore DS
            return "POP DS"
        if U == "VBCX2":  # double byte count (2 bytes/cell)
            return "ADD CX,CX"
        if U == "RST":
            return "; RST %s  (NEEDS MANUAL)" % opnd
        # ---- data / directives --------------------------------------
        if U == "DB":
            return "DB %s" % self.fixdata(opnd)
        if U == "DW":
            return "DW %s" % self.fixdata(opnd)
        if U == "DC":
            return "DB %s" % self.fixdc(opnd)
        if U == "DCS":
            return "DB %s" % self.fixdc(opnd)
        if U == "DS":
            return "DB (%s) DUP (?)" % opnd
        if U == "DSW":
            return "DW (%s) DUP (?)" % opnd
        if U == "ORG":
            # Intel ORG needs an absolute constant; label-relative ORGs in the
            # data area just pad to a buffer end (buffers reserve their own
            # space) -> drop with a note.
            if any(
                t.upper() in self.labels
                for t in re.findall(r"[A-Za-z_.%$@][A-Za-z0-9_.%$@]*", opnd)
            ):
                return "; ORG %s  (dropped: label-relative)" % opnd
            return "ORG %s" % opnd
        if U == ".END" or U == "END":
            return None  # emit END at very end
        if U in (
            ".PABS",
            ".PHEX",
            ".I8080",
            ".LADDR",
            ".SALL",
            ".LALL",
            ".XLIST",
            ".LIST",
            ".PAGE",
            ".SBTTL",
            ".IDENT",
            ".TITLE",
            ".PRNTX",
            ".ENTRY",
        ):
            return "; " + (U + " " + opnd).strip()
        return None

    def fixdata(self, opnd):
        # DB: comma-separated byte expressions (may contain spaces, e.g.
        # "'0' + VRSNUM/100"). Pass through unless TDL [NN] byte-syntax present.
        if "[" in opnd:
            return self.fixdc(opnd)
        return opnd

    def fixdc(self, opnd):
        # DC: space/comma-separated items (strings, [byte], bare tokens),
        # concatenated -> comma-joined DB list. 'str' [00] 'CU' -> 'str',0,'CU'
        items = []
        i = 0
        n = len(opnd)
        while i < n:
            c = opnd[i]
            if c in " \t,":
                i += 1
                continue
            if c == "'":
                j = i + 1
                while j < n and opnd[j] != "'":
                    j += 1
                items.append(opnd[i : j + 1])
                i = j + 1
            elif c == "[":
                j = opnd.find("]", i)
                j = j if j >= 0 else n
                inner = opnd[i + 1 : j].strip()
                items.append(inner if inner else "0")
                i = j + 1
            else:
                j = i
                while j < n and opnd[j] not in " \t,['":
                    j += 1
                items.append(opnd[i:j])
                i = j
        return ",".join(items)


if __name__ == "__main__":
    # usage: xlate.py expanded.asm [dos|cpm86]   (OS-interface target; default dos)
    target = sys.argv[2] if len(sys.argv) > 2 else "dos"
    if target not in ("dos", "cpm86"):
        sys.exit("xlate: target must be 'dos' or 'cpm86'")
    lines = open(sys.argv[1]).read().split("\n")
    x = Xlate(lines, target=target)
    x.translate()
    x.emit("\tEND")
    sys.stdout.write("\n".join(x.out) + "\n")
    sys.stderr.write(
        "[xlate] %d lines -> %d, %d labels, %d equ (target=%s)\n"
        % (len(lines), len(x.out), len(x.labels), len(x.equ), target)
    )
