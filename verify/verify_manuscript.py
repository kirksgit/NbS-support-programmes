#!/usr/bin/env python3
"""verify_manuscript.py -- every number in a manuscript must come from the run.

WHY THIS EXISTS
---------------
The risk in a long analysis is not that the analysis is wrong but that the
documents describing it drift away from it, by quoting a construct the framework
no longer uses, a flag rather than the canonical verdict, or a place the corpus
does not contain. Re-reading does not converge on these. This script replaces it
with a check that either passes or names the offending token.

THREE CHECKS
------------
1. NUMBER PROVENANCE -- every integer and percentage in the document must
   appear in the run's fact registry (18_quotable_facts.csv) or in one of the
   result tables. A number that appears nowhere in the run is, by definition,
   a number nobody computed.

2. PLACE PROVENANCE -- every proper name used in a factual claim about a place
   must exist in the coded city/country fields. This catches asserted
   exemplars.

3. RETIRED CONSTRUCTS -- the document must not name a construct the framework
   no longer computes. These read as legitimate to a reviewer and are the
   hardest class to spot by eye.

Usage:
    python verify_manuscript.py DATASET.csv TABLES_DIR DOC [DOC ...]

Exit 0 only when all three checks pass on every document.
"""
import sys, os, re, glob
import pandas as pd

RETIRED = {
    "programme-fit tension": "the seven tensions were replaced by seven relational fit components",
    "Justice_Score":         "no composite justice index is computed",
    "pfit":                  "no composite fit score is computed",
    "n_enablers":            "provision-counting construct retired at v13",
    "admin_profile":         "provision-counting construct; D3c uses requirement matching",
    "learn_score":           "retired with the counting design",
    "comply_score":          "retired with the counting design",
    "st_support":            "provision flag; D4a uses the canonical verdict",
    "eco_fit_strict":        "retired at v25 when [inferred] codes became equal evidence",
}

CLAIM = re.compile(r"publish|no eligibility|no technical standard|no payment clause|"
                   r"range[sd]? from|highest|lowest|exemplar", re.I)

# Sentence-initial and table-header words are grammar, not place claims. Only a
# capitalised token appearing INSIDE a sentence can be a place assertion; a token
# at position 0, or in a pipe-delimited table row, is structure.
def _candidate_spans(sent):
    if "|" in sent:            # table row: headers are not prose claims
        return []
    return [m for m in re.finditer(
        r"(?<=[a-z,;)\u2014-]\s)([A-Z\u00C0-\u00DD][a-z\u00E0-\u00FF]+"
        r"(?:[ -][A-Z\u00C0-\u00DD][a-z\u00E0-\u00FF]+)*)", sent)]


def doc_text(path):
    if path.endswith(".docx"):
        from docx import Document
        d = Document(path)
        tx = "\n".join(p.text for p in d.paragraphs)
        for t in d.tables:
            for row in t.rows:
                tx += "\n" + " | ".join(c.text for c in row.cells)
        return tx
    return open(path, encoding="utf-8", errors="ignore").read()


def run_numbers(tables_dir):
    """Every integer and one-decimal value the run emitted, from any table.

    EXTERNAL_FACTS_v30.csv is included deliberately. Some legitimate manuscript
    numbers cannot come from the pipeline: the pre-pipeline screening frame, the
    two blind reliability studies run during coding, and the reconciliation
    against the author's population reference. Those are not exempt from the
    rule -- they are registered, with their source named, in a file the verifier
    reads. The invariant is unchanged: every number traces to a recorded source.
    A number that is in neither the run nor the external register is a number
    nobody computed.
    """
    truth = set()
    ## v35: the register filename was PINNED to v30, so once the registers were
    ## re-cut at v32 the verifier silently stopped reading any of them and every
    ## externally-sourced number -- the reliability studies, the screening frame,
    ## the population reconciliation -- was reported as "appears in no run table".
    ## The check was not wrong about the registers being authoritative; it was
    ## looking at a file that no longer existed. It now reads the LATEST register
    ## of each kind, and both the value and its percentage column count as truth
    ## (a registered count of 40 legitimises the 5.6% the manuscript derives from
    ## it against a stated denominator).
    # The registers were once version-tagged (EXTERNAL_FACTS_v63.csv). The clean
    # distribution drops version suffixes, so both spellings are accepted --
    # otherwise renaming the file makes this verifier abort with "no register
    # found", which reproduce.sh reported only as "needs review" with nothing
    # above it to read.
    regs = (sorted(glob.glob("EXTERNAL_FACTS*.csv")) +
            sorted(glob.glob("QUOTABLE_FACTS*.csv")))
    regs += sorted(glob.glob(os.path.join(os.path.dirname(tables_dir) or ".", "..",
                                          "EXTERNAL_FACTS*.csv")))
    seen = set()
    for cand in regs:
        if not os.path.exists(cand) or os.path.basename(cand) in seen:
            continue
        seen.add(os.path.basename(cand))
        reg = pd.read_csv(cand)
        for col in ("value", "pct"):
            if col not in reg.columns:
                continue
            for v in pd.to_numeric(reg[col], errors="coerce").dropna():
                truth.add(str(int(v)) if float(v).is_integer() else f"{v:.1f}")
                truth.add(f"{v:.1f}")
    if not seen:
        raise SystemExit("no EXTERNAL_FACTS/QUOTABLE_FACTS register found -- "
                         "the verifier cannot certify externally-sourced numbers")
    for f in glob.glob(os.path.join(tables_dir, "*.csv")):
        try:
            t = pd.read_csv(f)
        except Exception:
            continue
        for col in t.columns:
            s = pd.to_numeric(t[col], errors="coerce").dropna()
            for v in s:
                if abs(v) < 1e7:
                    truth.add(str(int(v)) if float(v).is_integer() else f"{v:.1f}")
                    truth.add(f"{v:.1f}")
    return truth


def corpus_places(dataset):
    df = pd.read_csv(dataset, low_memory=False)
    cols = [c for c in df.columns
            if c.strip().lower().startswith(("a-1", "city", "country", "a-2"))]
    vals = set()
    for c in cols:
        s = (df[c].dropna().astype(str)
             .str.replace(r"\s*\[inferred\]", "", regex=True).str.strip())
        vals |= set(s[~s.str.lower().str.startswith("not stated")])
    toks = set()
    for v in vals:
        toks |= set(re.findall(r"[A-ZÀ-Ý][\w'À-ÿ-]+", v))
    return vals | toks


def check_numbers(tx, truth):
    ## v45: THOUSANDS SEPARATORS WERE SPLITTING NUMBERS APART.
    ##
    ## The old pattern had no comma branch, so "4,776 judgements" was tokenised as
    ## "4" and "776" and the verifier then asked whether 776 appears in a table.
    ## It does not -- the manuscript never claims 776 -- so a correct sentence was
    ## flagged. Worse in the other direction: "3,458" was checked as "458", which
    ## happens to occur somewhere among the tables, so a genuinely wrong grouped
    ## number could pass on the strength of its own last three digits. Grouped
    ## forms are now matched whole and compared with the separator stripped, since
    ## tables carry bare digits.
    bad = []
    for m in re.finditer(r"(?<![\d.\w])(\d{1,3}(?:,\d{3})+|\d{2,6}(?:\.\d)?)(?![\d\w])", tx):
        tok = m.group(1).replace(",", "")
        val = float(tok)
        if val < 20:                      # small counts read as prose, not claims
            continue
        if 1900 <= val <= 2035 and "." not in tok:   # years
            continue
        if re.search(r"(References|doi|Scientific Data|Journal|pp\.)[^.]{0,120}$",
                     tx[max(0, m.start()-160):m.start()]):
            continue   # bibliography: volume, issue and article numbers are not claims
        if re.search(r"[Ww]ord count[^.]{0,40}$", tx[max(0, m.start()-60):m.start()]):
            continue   # a document stating its own length is metadata, not a claim
        if tok in truth or f"{val:.1f}" in truth:
            continue
        ctx = tx[max(0, m.start() - 60):m.start() + 60].replace("\n", " ")
        bad.append((tok, ctx))
    return bad


## ---------------------------------------------------------------------------
## ANCHORED CLAIMS -- the check that would have caught the informal-groups error.
##
## check_numbers() asks whether a number appears ANYWHERE in ANY table. That is a
## presence test, not a provenance test: "12 Dutch programmes admit informal
## groups (28%)" passed cleanly because 12 and 28 both occur somewhere in the 143
## tables, while the cell the sentence actually describes read 14 and 33. A number
## that exists somewhere is not the same as the right number for this sentence.
##
## An anchor pins ONE phrase to ONE cell: locate the row by its key columns, read
## the named column, and require the manuscript's number beside that phrase to
## equal it. Anchors are cheap to add and each one closes a claim permanently, so
## add one whenever a sentence quotes a specific cell rather than a headline.
ANCHORS = [
    # (regex over the sentence, table, {column: value} row selector, column, tolerance)
    ## v45: the held-out re-test. Each of these numbers is quoted in BOTH the
    ## Results and the supplementary, from the same cell, so an anchor here binds
    ## the pair -- exactly the drift the informal-groups error demonstrated.
    (r"agreement is (\d+\.\d)% \(\u03ba = 0\.343\)",
     "19_retest_by_stratum.csv", {"stratum": "binary"}, "agree_pct", 0.05),
    (r"\u03ba = (0\.\d+)\) on the (?:16 )?binary variables",
     "19_retest_by_stratum.csv", {"stratum": "binary"}, "kappa", 0.001),
    (r"binary agreement is (\d+\.\d)%",
     "19_retest_by_stratum.csv", {"stratum": "binary"}, "both_agree_pct", 0.05),
    (r"\u03ba = (0\.\d+), n = 675",
     "19_retest_by_stratum.csv", {"stratum": "binary"}, "both_kappa", 0.001),
    (r"contradictions (?:number )?(\d+) of 3,458",
     "19_retest_by_stratum.csv", {"stratum": "binary"}, "true_contradictions", 0),
    (r"(\d+\.\d)% (?:of committed categorical pairs shar|sharing at least one categ)",
     "19_retest_by_stratum.csv", {"stratum": "categorical"}, "both_agree_pct", 0.05),
    (r"(\d+) Dutch programmes admit informal groups",
     "11_rare_provision_concentration.csv",
     {"provision": "Informal groups eligible or acknowledged", "country": "Netherlands"},
     "n_documenting", 0),
    (r"informal groups \((\d+)%\)",
     "11_rare_provision_concentration.csv",
     {"provision": "Informal groups eligible or acknowledged", "country": "Netherlands"},
     "pct_of_country", 0.5),
    (r"(\d+) of the (?:\d+) programmes admitting social-housing providers are Dutch",
     "11_rare_provision_concentration.csv",
     {"provision": "Social housing eligible", "country": "Netherlands"},
     "n_documenting", 0),
    (r"social-housing providers are Dutch, (\d+)%",
     "11_rare_provision_concentration.csv",
     {"provision": "Social housing eligible", "country": "Netherlands"},
     "pct_of_country", 0.5),
    (r"against (\d+)% of the US corpus",
     "11_rare_provision_concentration.csv",
     {"provision": "Social housing eligible", "country": "United States"},
     "pct_of_country", 0.5),
]


def check_anchors(tx, tables_dir):
    """Each anchored phrase must carry the number in ITS OWN cell, not merely a
    number that exists somewhere in the corpus of tables."""
    bad = []
    for pat, table, sel, col, tol in ANCHORS:
        m = re.search(pat, tx)
        if not m:
            continue                      # sentence absent from this document
        path = os.path.join(tables_dir, table)
        if not os.path.exists(path):
            bad.append((pat, f"table {table} not found"))
            continue
        t = pd.read_csv(path)
        for k, v in sel.items():
            t = t[t[k].astype(str) == str(v)]
        if len(t) != 1:
            bad.append((pat, f"row selector matched {len(t)} rows in {table}"))
            continue
        want = float(t.iloc[0][col])
        got = float(m.group(1))
        if abs(got - want) > tol:
            bad.append((pat, f"manuscript says {got:g}, {table}[{col}] says {want:g}"))
    return bad


## v56: EPONYMOUS STATISTIC NAMES ARE NOT PLACE CLAIMS.
##
## This guard exists to catch a fabricated city or country -- a capitalised span in a
## claim sentence that appears nowhere in the corpus. Statistics named after their
## authors trip it for a structural reason: "Calinski-Harabasz" and "Jaccard" are
## surnames, so they are capitalised, they are absent from the corpus place list, and
## they are absent from the system word list. Flagging them is a FALSE POSITIVE, and
## the fix belongs here rather than in the prose -- removing the statistic's name from
## a methods sentence to satisfy a place check would make the manuscript worse.
##
## The allowlist is deliberately narrow: only statistics and conventions this project
## actually reports, each named explicitly. It is NOT a general capitalised-word
## bypass, and a fabricated place name still fails. Anything added here must be a term
## whose referent is a method, not a location.
STAT_EPONYMS = {
    "Calinski-Harabasz",   # cluster-separation criterion, figure S11 / 08_cluster_diagnostics
    "Jaccard",             # bootstrap cluster-stability index, 08_bootstrap_jaccard
    "Ward",                # hierarchical clustering linkage
    "Benjamini-Hochberg",  # multiple-comparison correction
    "Cohen",               # kappa
    "Cramer",              # V, association measure
    "Fisher",              # exact test
    "Bayes",               # empirical-Bayes shrinkage, figure 8b
}


def check_places(tx, places, endict):
    bad = []
    for sent in re.split(r"(?<=[.;])\s+", tx):
        if not CLAIM.search(sent):
            continue
        for m in _candidate_spans(sent):
            t = m.group(1)
            if t in places:
                continue
            if t in STAT_EPONYMS or t.split("-")[0] in STAT_EPONYMS:
                continue
            words = [w for w in re.split(r"[ -]", t) if w]
            if any(w in places for w in words):
                continue
            if t.replace("-", " ") in places or t.replace(" ", "-") in places:
                continue
            if all(w.lower() in endict for w in words):
                continue
            bad.append((t, sent.strip()[:150]))
    return bad


def check_retired(tx):
    """Flag a retired construct only where the document USES it, not where it
    records that it was removed. A methods section that explains what was
    retired is doing the right thing and must not be penalised for saying so."""
    out = []
    for k, why in RETIRED.items():
        for m in re.finditer(r"\b" + re.escape(k), tx, re.I):
            sent_start = tx.rfind(".", 0, m.start()) + 1
            sent = tx[sent_start:tx.find(".", m.end()) + 1 or None]
            if re.search(r"retire|remov|replac|no longer|abandon|superseded|"
                         r"withdraw|earlier|former|not computed", sent, re.I):
                continue
            out.append((k, why))
            break
    return out


def main():
    dataset, tables_dir, docs = sys.argv[1], sys.argv[2], sys.argv[3:]
    truth = run_numbers(tables_dir)
    places = corpus_places(dataset)
    endict = set()
    if os.path.exists("/usr/share/dict/words"):
        endict = {w.strip().lower()
                  for w in open("/usr/share/dict/words", encoding="utf-8", errors="ignore")}

    total = 0
    for doc in docs:
        tx = doc_text(doc)
        name = os.path.basename(doc)
        nb = check_numbers(tx, truth)
        pb = check_places(tx, places, endict)
        rb = check_retired(tx)
        ab = check_anchors(tx, tables_dir)
        for tok, ctx in nb:
            print(f"  NUMBER   {name}: {tok} appears in no run table -- ...{ctx}...")
        for tok, sent in pb:
            print(f"  PLACE    {name}: '{tok}' is not in the corpus -- {sent}")
        for k, why in rb:
            print(f"  RETIRED  {name}: names '{k}' -- {why}")
        for pat, why in ab:
            print(f"  ANCHOR   {name}: {why}  [{pat[:52]}]")
        n = len(nb) + len(pb) + len(rb) + len(ab)
        print(f"  {'PASS' if n == 0 else 'FAIL'}  {name}: "
              f"{len(nb)} number, {len(pb)} place, {len(rb)} retired-construct, "
              f"{len(ab)} anchored-claim findings")
        total += n

    print(f"\ntotal findings: {total}")
    sys.exit(1 if total else 0)


if __name__ == "__main__":
    main()
