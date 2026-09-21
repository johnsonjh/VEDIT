#!/usr/bin/env python3
# VEDIT 8080-source expander (Path B, stage 1).
# Flattens .INSERT, applies .OPSYN, resolves conditional assembly
# (IF/IFNOT/.IFE/.IFN/.IFIDN/.IFDIF) and expands .DEFINE macros (+ REPT)
# into a flat TDL/8080 mnemonic stream.  Comments are stripped up front so
# bracket matching is clean.  Output is re-assembled by PASM and diffed
# against the known-good HEX to prove the expansion is faithful.
import sys, os, re

SRCDIR = sys.argv[2] if len(sys.argv) > 2 else "."
IDCH = r"A-Za-z0-9_%$."


def skip_quote(s, i):
    """s[i] is ' or " . If it closes on the same line it's a string/char
    literal -> return index after closer. Else it's a lone TDL operator
    (concatenation / 1's-complement suffix) -> return i+1."""
    qc = s[i]
    j = i + 1
    while j < len(s) and s[j] != "\n":
        if s[j] == qc:
            return j + 1
        j += 1
    return i + 1


def split_comment(line):
    i = 0
    while i < len(line):
        c = line[i]
        if c in "'\"":
            i = skip_quote(line, i)
            continue
        if c == ";":
            return line[:i]
        i += 1
    return line


def find_file(name):
    name = name.strip().strip("'\"")
    base = name.split(".")[0].lower()
    for cand in (name.lower(), base + ".asm", base + ".tbl", base):
        p = os.path.join(SRCDIR, cand)
        if os.path.exists(p):
            return p
    raise FileNotFoundError(name)


def load(name):
    path = find_file(name)
    with open(path, "rb") as f:
        text = "".join(chr(b & 0x7F) for b in f.read())
    out = []
    for line in text.split("\n"):
        s = split_comment(line.rstrip("\r")).rstrip()
        m = re.match(r"\s*\.INSERT\s+(\S+)\s*$", s, re.I)
        if m:
            out.extend(load(m.group(1)))
        elif s.strip():
            out.append(s)
    return out


def find_match(s, i, op="[", cl="]"):
    depth = 0
    while i < len(s):
        c = s[i]
        if c in "'\"":
            i = skip_quote(s, i)
            continue
        if c == op:
            depth += 1
        elif c == cl:
            depth -= 1
            if depth == 0:
                return i
        i += 1
    return len(s) - 1


class Expander:
    def __init__(self):
        self.sym = {}
        self.opsyn = {}
        self.macros = {}
        self.out = []
        self.S = ""
        self.pos = 0
        self.N = 0

    def canon(self, op):
        return self.opsyn.get(op.lower(), op.upper())

    def emit(self, line):
        s = line.rstrip()
        if not s:
            return
        # TDL column 1 = label field.  Keep labels (NAME:) and equates
        # (NAME = / ==) flush-left; indent everything else (ops/directives).
        if re.match(r"^[A-Za-z_.%$][A-Za-z0-9_.%$]*:", s) or re.match(
            r"^[A-Za-z_.%$][A-Za-z0-9_.%$]*\s*==?(\s|$)", s
        ):
            self.out.append(s)
        else:
            self.out.append("\t" + s)

    def evnum(self, t):
        t = t.strip()
        if not t:
            return 0
        if re.fullmatch(r"[0-9A-Fa-f]+[Hh]", t):
            return int(t[:-1], 16)
        if re.fullmatch(r"[01]+[Bb]", t):
            return int(t[:-1], 2)
        if re.fullmatch(r"\d+", t):
            return int(t)
        if re.fullmatch(r"'.'", t):
            return ord(t[1])
        if t in ("$", "."):
            raise ValueError("loc")
        u = t.upper()
        if u in self.sym:
            return self.sym[u]
        return None

    def eval(self, expr):
        toks = re.findall(r"'.'|[%s]+|<<|>>|[-+*/&!()]" % IDCH, expr)
        pos = [0]

        def peek():
            return toks[pos[0]] if pos[0] < len(toks) else None

        def nxt():
            t = peek()
            pos[0] += 1
            return t

        def atom():
            t = nxt()
            if t == "(":
                v = orr()
                if peek() == ")":
                    nxt()
                return v
            if t == "-":
                return -atom()
            if t == "+":
                return atom()
            v = self.evnum(t)
            return 0 if v is None else v

        def term():
            v = atom()
            while peek() in ("*", "/"):
                o = nxt()
                r = atom()
                v = v * r if o == "*" else (v // r if r else 0)
            return v

        def add():
            v = term()
            while peek() in ("+", "-"):
                o = nxt()
                r = term()
                v = v + r if o == "+" else v - r
            return v

        def orr():
            v = add()
            while peek() in ("&", "!", "<<", ">>"):
                o = nxt()
                r = add()
                v = (
                    (v & r)
                    if o == "&"
                    else (v | r) if o == "!" else (v << r) if o == "<<" else (v >> r)
                )
            return v

        return orr()

    def subst(self, body, params, args):
        for p, a in zip(params, args):
            # TDL macro params are case-insensitive (e.g. body 'DEST' vs param 'dest')
            body = re.sub(
                r"(?<![%s])%s(?![%s])" % (IDCH, re.escape(p), IDCH),
                lambda m: a,
                body,
                flags=re.IGNORECASE,
            )
        # TDL '-concatenation between identifiers:  DSBC'X -> DSBCB
        body = re.sub(r"(?<=[%s])'(?=[%s])" % (IDCH, IDCH), "", body)

        def ev(m):
            try:
                return str(self.eval(m.group(1)))
            except Exception:
                return m.group(0)

        return re.sub(r"\\([0-9A-Za-z_%$.+\-*/]+)", ev, body)

    def split_args(self, s):
        out = []
        d = 0
        cur = ""
        i = 0
        while i < len(s):
            c = s[i]
            if c in "'\"":
                k = skip_quote(s, i)
                cur += s[i:k]
                i = k
                continue
            if c in "([":
                d += 1
                cur += c
            elif c in ")]":
                d -= 1
                cur += c
            elif c == "," and d == 0:
                out.append(cur)
                cur = ""
            else:
                cur += c
            i += 1
        if cur.strip() or out:
            out.append(cur)
        return [a.strip() for a in out]

    def process(self, text):
        sS, sp, sN = self.S, self.pos, self.N
        self.S, self.pos, self.N = text, 0, len(text)
        while self.pos < self.N:
            self.statement()
        self.S, self.pos, self.N = sS, sp, sN

    def statement(self):
        S, N = self.S, self.N
        p = self.pos
        while p < N and S[p] in " \t\r\n":
            p += 1
        if p >= N:
            self.pos = N
            return
        eol = S.find("\n", p)
        if eol < 0:
            eol = N
        line = S[p:eol]
        stripped = line.strip()
        if not stripped:
            self.pos = eol + 1
            return
        start = p

        # equate:  NAME = expr   or  NAME == expr
        meq = re.match(r"^([%s]+)\s*(==?)\s*(\S.*)$" % IDCH, stripped)
        if meq and ":" not in meq.group(1):
            name = meq.group(1).upper()
            try:
                self.sym[name] = self.eval(meq.group(3))
            except Exception:
                pass
            self.emit(stripped)
            self.pos = eol + 1
            return

        # label prefix
        label = ""
        body = stripped
        ml = re.match(r"^([%s]+:)\s*(.*)$" % IDCH, stripped)
        if ml:
            label = ml.group(1)
            body = ml.group(2)
            if not body:
                self.emit(label)
                self.pos = eol + 1
                return

        parts = body.split(None, 1)
        op0 = parts[0]
        operand = parts[1] if len(parts) > 1 else ""
        U = op0.upper()
        C = self.canon(op0)

        if U == ".OPSYN":
            a = operand.split(",")
            if len(a) == 2:
                self.opsyn[a[0].strip().lower()] = a[1].strip().upper()
            self.pos = eol + 1
            return
        if U == ".DEFINE":
            self.def_macro(start)
            return
        if C in ("IF", "IFNOT"):
            if label:
                self.emit(label)
            self.do_if(start, C)
            return
        if U in (".IFIDN", ".IFDIF"):
            if label:
                self.emit(label)
            self.do_ifid(start, U)
            return
        if C == "REPT" or U == "REPT":
            if label:
                self.emit(label)
            self.do_rept(start)
            return
        if C in self.macros:
            params, mbody = self.macros[C]
            if label:
                self.emit(label)
            self.process(self.subst(mbody, params, self.split_args(operand)) + "\n")
            self.pos = eol + 1
            return

        self.emit(stripped)
        self.pos = eol + 1
        return

    def def_macro(self, start):
        S = self.S
        eq = S.find("=", start)
        head = S[start:eq]
        m = re.search(r"\.DEFINE\s+([%s]+)\s*(?:\[([^\]]*)\])?" % IDCH, head, re.I)
        name = m.group(1).upper()
        params = [x.strip() for x in (m.group(2) or "").split(",") if x.strip()]
        br = S.find("[", eq)
        end = find_match(S, br)
        self.macros[name] = (params, S[br + 1 : end])
        self.pos = end + 1

    def cond_and_block(self, start):
        S = self.S
        br = S.find("[", start)
        cond = S[start:br]
        cond = re.sub(r"^[%s]*:?\s*" % IDCH, "", cond, count=1)  # drop leading kw/label
        cond = cond.strip().rstrip(",").strip()
        end = find_match(S, br)
        inner = S[br + 1 : end]
        # optional else block
        k = end + 1
        while k < len(S) and S[k] in " \t\r\n":
            k += 1
        els = None
        if k < len(S) and S[k] == "[":
            e2 = find_match(S, k)
            els = S[k + 1 : e2]
            end = e2
        return cond, inner, els, end + 1

    def do_if(self, start, kind):
        cond, inner, els, nxt = self.cond_and_block(start)
        try:
            v = self.eval(cond)
        except Exception:
            v = 0
        truth = (v != 0) if kind == "IF" else (v == 0)
        if truth:
            self.process(inner + "\n")
        elif els is not None:
            self.process(els + "\n")
        self.pos = nxt

    def do_ifid(self, start, kind):
        S = self.S
        br = S.find("[", start)
        strs = re.findall(r'"([^"]*)"', S[start:br])
        a = strs[0].strip() if len(strs) > 0 else ""
        b = strs[1].strip() if len(strs) > 1 else ""
        end = find_match(S, br)
        inner = S[br + 1 : end]
        same = a == b
        if same if kind == ".IFIDN" else not same:
            self.process(inner + "\n")
        self.pos = end + 1

    def do_rept(self, start):
        S = self.S
        eol = S.find("\n", start)
        kw = re.match(
            r"\s*[%s]*:?\s*REPT\s+(.*)$" % IDCH,
            S[start : eol if eol > 0 else len(S)],
            re.I,
        )
        # count is up to the first comma; body is in (...) which may span lines
        rest = kw.group(1) if kw else ""
        comma = rest.find(",")
        cnt_txt = rest[:comma] if comma >= 0 else rest
        try:
            cnt = self.eval(cnt_txt)
        except Exception:
            cnt = 0
        op = S.find("(", start)
        end = find_match(S, op, "(", ")")
        body = S[op + 1 : end]
        for _ in range(max(0, cnt)):
            self.process(body + "\n")
        self.pos = end + 1


if __name__ == "__main__":
    lines = load(sys.argv[1])
    ex = Expander()
    ex.process("\n".join(lines) + "\n")
    sys.stdout.write("\n".join(ex.out) + "\n")
    sys.stderr.write(
        "[expand] %d src -> %d out, %d macros, %d syms\n"
        % (len(lines), len(ex.out), len(ex.macros), len(ex.sym))
    )
