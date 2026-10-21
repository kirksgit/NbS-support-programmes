#!/usr/bin/env python3
"""Compare a local pipeline run against the packaged expected output.

Usage:  python3 verify/compare_run.py expected/tables output/tables

Counts and verdicts must match EXACTLY. Model coefficients are compared with a
small tolerance, because BLAS/LAPACK differences across platforms move the last
digits of an optimiser result without changing anything substantive.
"""
import sys, os, glob
import pandas as pd
import numpy as np

TOL = 1e-6

# DECLARED ENVIRONMENT-VARIANT CELLS. Everything else must match byte for
# byte. `hcpc_automatic` is a DIAGNOSTIC reporting whether FactoMineR's own
# automatic-k rule would have chosen each k; that rule is implemented differently
# across FactoMineR releases, so the flag can differ between machines while
# `selected` -- the column that actually drives the analysis -- does not. It is
# declared here WITH ITS REASON rather than excluded silently, it is reported as
# a note, and any OTHER difference in the same table still fails the comparison.
ENV_VARIANT = {
    ("08_cluster_diagnostics.csv", "hcpc_automatic"):
        "FactoMineR automatic-k rule differs across releases; `selected` is the "
        "operative column and must still match",
}

# --------------------------------------------------------------------------
# SELF-TEST.  `python3 compare_run.py --self-test <expected_dir>`
#
# The missing-value branch below DELIBERATELY treats "absent on both sides" as a
# match. Any such relaxation risks hiding a genuine difference, and the first
# version of this fix was written after sampling only some of the flagged
# columns rather than all of them. So the relaxation is not taken on trust: this
# mode copies the reference tables, plants four errors of the classes that
# matter -- an integer count, a float, a string cell, and a value blanked on ONE
# side only -- and asserts that every one is still caught.
# --------------------------------------------------------------------------
def _self_test(ref_dir):
    import shutil, tempfile, subprocess
    tmp = tempfile.mkdtemp()
    for f in glob.glob(os.path.join(ref_dir, "*.csv")):
        shutil.copy(f, tmp)

    def edit(fname, fn):
        p = os.path.join(tmp, fname)
        if not os.path.exists(p):
            return None
        d = pd.read_csv(p)
        d = fn(d)
        if d is None:
            return None
        d.to_csv(p, index=False)
        return fname

    planted = []
    def _int(d):
        for c in d.columns:
            if pd.api.types.is_integer_dtype(d[c]) and len(d):
                d.loc[0, c] = int(d.loc[0, c]) + 1
                return d
    def _float(d):
        for c in d.columns:
            if pd.api.types.is_float_dtype(d[c]) and d[c].notna().any():
                i = d[c].first_valid_index()
                d.loc[i, c] = float(d.loc[i, c]) + 1.0
                return d
    def _str(d):
        for c in d.columns:
            if d[c].dtype == object and d[c].notna().any():
                d.loc[d[c].first_valid_index(), c] = "TAMPERED_BY_SELF_TEST"
                return d
    def _blank(d):
        for c in d.columns:
            if d[c].dtype == object and d[c].notna().any():
                d.loc[d[c].first_valid_index(), c] = None
                return d

    names = sorted(os.path.basename(f) for f in glob.glob(os.path.join(ref_dir, "*.csv")))
    for fn, label in ((_int, "integer"), (_float, "float"),
                      (_str, "string"), (_blank, "one-sided missing")):
        for nm in names:
            if nm in [p[0] for p in planted]:
                continue
            if edit(nm, fn):
                planted.append((nm, label))
                break

    r = subprocess.run([sys.executable, os.path.abspath(__file__), ref_dir, tmp],
                       capture_output=True, text=True)
    shutil.rmtree(tmp)
    caught = {ln.split("::")[0].strip(" -") for ln in r.stdout.splitlines() if "::" in ln}
    ok = True
    for nm, label in planted:
        hit = nm in caught
        print("  %s  planted %-18s in %s" % ("PASS " if hit else "FAIL ", label, nm))
        ok = ok and hit
    if r.returncode == 0:
        print("  FAIL   the comparator exited 0 despite planted errors")
        ok = False
    print("self-test: %s (%d planted)" % ("PASS" if ok else "FAIL", len(planted)))
    sys.exit(0 if ok else 1)


if len(sys.argv) > 1 and sys.argv[1] == "--self-test":
    _self_test(sys.argv[2])

exp_dir, got_dir = sys.argv[1], sys.argv[2]
exp = {os.path.basename(f): f for f in glob.glob(os.path.join(exp_dir, "*.csv"))}
got = {os.path.basename(f): f for f in glob.glob(os.path.join(got_dir, "*.csv"))}

missing = sorted(set(exp) - set(got))
extra   = sorted(set(got) - set(exp))
diffs   = []
notes   = []

for name in sorted(set(exp) & set(got)):
    a = pd.read_csv(exp[name])
    b = pd.read_csv(got[name])
    if a.shape != b.shape:
        diffs.append((name, "shape %s vs %s" % (a.shape, b.shape)))
        continue
    if list(a.columns) != list(b.columns):
        diffs.append((name, "column names differ"))
        continue
    for c in a.columns:
        ca, cb = a[c], b[c]
        if pd.api.types.is_numeric_dtype(ca) and pd.api.types.is_numeric_dtype(cb):
            # exact for integers (counts, verdict tallies), tolerant for floats
            if pd.api.types.is_integer_dtype(ca) and pd.api.types.is_integer_dtype(cb):
                bad = int((ca.fillna(-999) != cb.fillna(-999)).sum())
            else:
                bad = int((~np.isclose(ca.astype(float), cb.astype(float),
                                       rtol=TOL, atol=TOL, equal_nan=True)).sum())
        else:
            # A column that is all-NaN in both files loads with dtype object, so
            # it lands here. astype(str) turns each missing value into the string
            # "nan" -- but a float NaN and a None both stringify differently
            # depending on how the column was parsed, so identical files reported
            # thousands of spurious differences. Missing on BOTH sides is a match;
            # missing on ONE side is a real difference and still fails.
            na_a, na_b = ca.isna(), cb.isna()
            same_missing = na_a & na_b
            one_missing = na_a ^ na_b
            # Text is compared after NORMALISING ESCAPED UNICODE. R writes
            # non-ASCII either as UTF-8 ("Anjo" with a macron, the multiplication
            # sign) or as an escaped code point ("<U+014D>", "<U+00D7>") depending
            # on the locale the session was launched under -- the SAME character,
            # written two ways. Comparing raw text reported 92 false differences
            # between two runs of identical code on the same machine. Decoding the
            # escape makes the comparison about the data rather than the encoding.
            def _norm(s):
                return (s.astype(str)
                        .str.replace(r"<U\+([0-9A-Fa-f]{4,6})>",
                                     lambda m: chr(int(m.group(1), 16)), regex=True)
                        .str.normalize("NFC"))
            differs = (_norm(ca) != _norm(cb)) & ~same_missing
            bad = int((differs | one_missing).sum())
        if bad:
            if (name, c) in ENV_VARIANT:
                notes.append("%s :: column '%s': %d rows differ -- DECLARED ENVIRONMENT-VARIANT (%s)"
                             % (name, c, bad, ENV_VARIANT[(name, c)]))
            else:
                diffs.append((name, "column '%s': %d differing rows" % (c, bad)))

print("tables compared: %d" % len(set(exp) & set(got)))
for _n in notes:
    print("NOTE: " + _n)
if missing:
    print("MISSING from your run (%d):" % len(missing))
    for m in missing[:20]:
        print("  - " + m)
if extra:
    print("EXTRA in your run (%d):" % len(extra))
    for m in extra[:20]:
        print("  - " + m)
if diffs:
    print("DIFFERENCES (%d):" % len(diffs))
    for n, d in diffs[:40]:
        print("  - %s :: %s" % (n, d))
    sys.exit(1)
if missing or extra:
    sys.exit(1)
print("IDENTICAL -- every table matches the packaged run.")
