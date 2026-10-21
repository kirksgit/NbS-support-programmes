#!/usr/bin/env bash
# =============================================================================
# reproduce.sh -- run the whole analysis and check you got our results.
#
#   ./reproduce.sh
#
# Everything runs from the coded workbook in data/. Nothing touches the network.
#
# What it does, in order:
#   0. checks prerequisites and STOPS with a specific install line if any is missing
#   1. Rscript run_all.R                     -> output/tables (158)
#   2. verify/build_fig06.py                 -> fig06, only when NBS_FIGURES=on
#   3. verify/compare_run.py                 -> your tables vs ours, cell by cell
#   4. verify/verify_claims.py               -> claim-level relations
#   5. verify/verify_manuscript.py           -> every number in the prose
#
# Exit code 0 means every table matched and every claim held.
#
# ON THE ONE FAILING CHECK: run_all.R ends with its own verification module
# and exits non-zero because ONE check fails DELIBERATELY. Four tiles of the
# context-deviation figure cannot carry an adjusted estimate, because the
# requirement does not vary inside those context cells and the model is
# therefore unidentified. That is a limit of the corpus, and the check exists
# to stop anyone drawing empty tiles as if they were evidence. This script
# treats that single failure as success and any other failure as failure,
# rather than ignoring the exit code wholesale.

# =============================================================================
set -uo pipefail
cd "$(dirname "$0")"

# =============================================================================
for _loc in en_US.UTF-8 C.UTF-8 en_GB.UTF-8; do
  if locale -a 2>/dev/null | grep -qix "${_loc/UTF-8/utf8}\|${_loc}"; then
    export LC_ALL="$_loc" LANG="$_loc"; break
  fi
done
: "${LC_ALL:=en_US.UTF-8}"; : "${LANG:=en_US.UTF-8}"; export LC_ALL LANG
printf '  locale:  %s\n' "$LC_ALL"

# =============================================================================
export NBS_DATA="${NBS_DATA:-data/NBS_707_FINAL_VERIFIED.xlsx}"
if [ ! -f "$NBS_DATA" ]; then
  printf 'FAILED: dataset not found: %s\n' "$NBS_DATA"; exit 1
fi
printf '  dataset: %s\n' "$NBS_DATA"

EXPECT_PASS=127         # checks expected to pass (updated when the suite changes)
EXPECT_FAIL=0           # the suite now passes outright; ANY failure fails the build
RED=$'\033[31m'; GRN=$'\033[32m'; YEL=$'\033[33m'; DIM=$'\033[2m'; OFF=$'\033[0m'
step() { printf '\n%s=== %s%s\n' "$DIM" "$1" "$OFF"; }
die()  { printf '%sFAILED: %s%s\n' "$RED" "$1" "$OFF"; exit 1; }

# ---------------------------------------------------------------- 0. prereqs
step "0. Prerequisites"
command -v Rscript >/dev/null || die "Rscript is not on PATH. Install R (built with 4.6.0)."
printf '  R          %s\n' "$(Rscript --version 2>&1 | head -1)"

PKGS="FactoMineR MASS brglm2 broom cluster dplyr forcats ggalluvial ggplot2 ggrepel jsonlite lmtest maps nnet patchwork purrr readr readxl rlang sandwich scales stringi stringr tibble tidyr writexl"
MISSING=$(Rscript -e "p <- strsplit('$PKGS',' ')[[1]]; cat(paste(setdiff(p, rownames(installed.packages())), collapse=' '))" 2>/dev/null)
if [ -n "${MISSING// }" ]; then
  printf '%s  missing R packages: %s%s\n' "$RED" "$MISSING" "$OFF"
  printf '  install with:\n    Rscript -e '\''install.packages(c(%s), repos="https://cloud.r-project.org")'\''\n' \
    "$(echo "$MISSING" | tr ' ' '\n' | sed '/^$/d;s/.*/"&"/' | paste -sd, -)"
  die "R dependencies"
fi
printf '  R packages %s25 required, all present%s\n' "$GRN" "$OFF"
# brglm2 is REQUIRED, not optional. An earlier version of this script called it
# optional "results are unaffected" -- that was WRONG and testing caught it. The
# package is called behind requireNamespace() only when a model separates, but
# when it IS absent the separated model takes a fallback path that produces
# DIFFERENT standard errors (one confidence bound moved by a factor of 96) and a
# different model label. Point estimates are unchanged, but the run does not
# reproduce. A missing brglm2 is therefore a hard stop, not a warning.
Rscript -e 'q(status = as.integer(!requireNamespace("brglm2", quietly=TRUE)))' 2>/dev/null \
  && printf '  brglm2     present (required: bias reduction for separated models)\n' \
  || { printf '%s  brglm2 is MISSING and it is REQUIRED.%s\n' "$RED" "$OFF"
       printf '  Without it the separated D3B/evaluability models take a fallback path with\n'
       printf '  different standard errors, and your run will NOT match ours. Install it:\n'
       printf '    Rscript -e '\''install.packages("brglm2", repos="https://cloud.r-project.org")'\''\n'
       die "brglm2"; }

HAVE_PY=1

# export_dataset_csv.py reads the workbook with pandas, which needs openpyxl
# for .xlsx. Without it step 3b fails, step 5 is skipped for want of its input,
# and the run would otherwise still report success on four checks of five.
"${PY_PANDAS:-python3}" -c "import openpyxl" 2>/dev/null || \
  printf '%%s  python openpyxl is missing -- step 3b and step 5 will not run (pip install openpyxl)%%s\n' "$YEL" "$OFF"
command -v python3 >/dev/null || { HAVE_PY=0; printf '%s  python3    not found -- steps 2-4 will be skipped%s\n' "$YEL" "$OFF"; }
if [ "$HAVE_PY" = 1 ]; then
  # Find an interpreter with pandas, not just the default one. On this class of
  # machine `python3` is often the Apple system 3.9 with nothing installed while
  # a newer python3 alongside it carries the packages.
  PY_PANDAS=""
  for _c in python3 /usr/local/bin/python3 /opt/homebrew/bin/python3 python3.13 python3.12 python3.11; do
    command -v "$_c" >/dev/null 2>&1 || [ -x "$_c" ] || continue
    if "$_c" -c 'import pandas' 2>/dev/null; then PY_PANDAS="$_c"; break; fi
  done
  if [ -n "$PY_PANDAS" ]; then
    [ "$PY_PANDAS" = python3 ] || printf '  pandas     found in %s\n' "$PY_PANDAS"
  else
    HAVE_PY=0
    printf '%s  pandas     missing -- steps 3-4 skipped. install: python3 -m pip install pandas%s\n' "$YEL" "$OFF"
  fi
  # matplotlib: try to OFFER a fix rather than just report the lack. Figure 6 is
  # the only thing that needs it (build_fig06.py's sole write is fig.savefig, so
  # no table can depend on it), but "figure 6 skipped" every run is not a useful
  # outcome. If the default python3 lacks it, look for another interpreter on the
  # machine that has it before giving up.
  if python3 -c 'import matplotlib' 2>/dev/null; then
    HAVE_MPL=1; PY_FIG=python3
  else
    PY_FIG=""
    for _cand in /usr/local/bin/python3 /opt/homebrew/bin/python3 python3.13 python3.12 python3.11; do
      command -v "$_cand" >/dev/null 2>&1 || [ -x "$_cand" ] || continue
      if "$_cand" -c 'import matplotlib' 2>/dev/null; then PY_FIG="$_cand"; break; fi
    done
    if [ -n "$PY_FIG" ]; then
      HAVE_MPL=1
      printf '  matplotlib found in %s (default python3 lacks it)\n' "$PY_FIG"
    else
      # Nothing on the machine has it. Rather than skip figure 6 on every run and
      # leave the reader to install it by hand, INSTALL IT -- into the same
      # interpreter that already carries pandas, so the figure and the checkers
      # share one environment. This is the only thing this script installs, it is
      # a plotting library and nothing else, and it is announced before it runs.
      _pip_py="${PY_PANDAS:-$(command -v python3)}"
      printf '%s  matplotlib is missing -- installing it (needed only for figure 6)%s\n' "$YEL" "$OFF"

      # Try in-place first, then user scope. Both can legitimately fail: a
      # framework or system Python may refuse to write to its own site-packages,
      # and some builds disable user site-packages entirely.
      "$_pip_py" -m pip install --quiet matplotlib >/tmp/nbs_mpl.log 2>&1 \
        || "$_pip_py" -m pip install --quiet --user matplotlib >>/tmp/nbs_mpl.log 2>&1 \
        || "$_pip_py" -m pip install --quiet --break-system-packages matplotlib >>/tmp/nbs_mpl.log 2>&1 \
        || true
      if "$_pip_py" -c 'import matplotlib' 2>/dev/null; then
        HAVE_MPL=1; PY_FIG="$_pip_py"
        printf '  matplotlib installed into %s\n' "$_pip_py"
      else
        # LAST RESORT, and the one that always works: a private virtualenv INSIDE
        # this folder. It needs no system permission, touches nothing outside the
        # bundle, and is deleted with it. Used only to draw figure 6.
        printf '  system install refused; building a private environment in .venv-fig/\n'
        if "$_pip_py" -m venv .venv-fig >>/tmp/nbs_mpl.log 2>&1 \
           && .venv-fig/bin/python -m pip install --quiet matplotlib >>/tmp/nbs_mpl.log 2>&1 \
           && .venv-fig/bin/python -c 'import matplotlib' 2>/dev/null; then
          HAVE_MPL=1; PY_FIG=".venv-fig/bin/python"
          printf '  matplotlib installed in .venv-fig -- figure 6 will be drawn\n'
        fi
      fi
      if [ "${HAVE_MPL:-0}" != 1 ]; then
        HAVE_MPL=0
        printf '%s  could not install matplotlib -- figure 6 SKIPPED (no table is affected).%s\n' "$YEL" "$OFF"
        printf '    reason: %s\n' "$(grep -m1 -i 'error' /tmp/nbs_mpl.log 2>/dev/null | cut -c1-100)"
        printf '    install it yourself with:  %s -m pip install matplotlib\n' "$_pip_py"
      fi
    fi
  fi
fi
[ -f data/NBS_707_FINAL_VERIFIED.xlsx ] || die "data/NBS_707_FINAL_VERIFIED.xlsx is missing."
printf '  workbook   %s\n' "$(ls -lh data/NBS_707_FINAL_VERIFIED.xlsx | awk '{print $5}')"
# The checkers in verify/ read three files besides the workbook. A missing one
# used to surface only as an opaque later failure -- the fact register aborting
# verify_manuscript.py was reported as "manuscript numbers need review" with
# nothing above it to read. Named here so the run says which file is absent.
for _f in QUOTABLE_FACTS.csv data/CITY_GEOCODES.csv data/E7_MEASURE_CLASSIFICATION.csv; do
  [ -f "$_f" ] || printf '%s  %s missing -- a later check will be skipped or fail%s\n' "$YEL" "$_f" "$OFF"
done

# ------------------------------------------------------------- 1. pipeline
step "1. Running the pipeline  (a few minutes; output/ is cleared first)"
Rscript run_all.R > run.log 2>&1
RC=$?
# run_all.R exits non-zero whenever R/16_verify.R finds a failing standing check
# or the warnings gate finds a warning nobody has ruled on. This build carries
# NEITHER, so a non-zero code is now a real fault rather than an expected one.
# It used to ship one deliberate fig10_context_deviations failure, and the
# tolerance for it is removed here deliberately: while the build expected a
# failure, a genuine fig10 fault would have passed unnoticed. The warnings
# classification at the end of run_all.R also runs now, because nothing stops
# the pipeline before it.
if [ "$RC" -ne 0 ] && ! grep -q 'VERIFICATION FAILED' run.log; then
  grep -E '^Error|^Erreur' run.log | head -5 | sed 's/^/    /'
  die "run_all.R exited $RC before reaching verification -- see run.log"
fi
PASS=$(grep -cE '^  PASS' run.log || true)
FAIL=$(grep -cE '^  FAIL' run.log || true)
printf '  %s checks passed, %s failed\n' "$PASS" "$FAIL"
grep -E '^  FAIL' run.log | sed 's/^/    /'

if [ "$FAIL" -gt "$EXPECT_FAIL" ]; then
  printf '%s  %s verification check(s) failed -- see run.log%s\n' "$RED" "$FAIL" "$OFF"
  grep -E '^Error|^Erreur' run.log | head -3 | sed 's/^/    /'
  die "pipeline verification"
fi
if [ "$PASS" -ne "$EXPECT_PASS" ]; then
  printf '%s  expected %s passing checks, got %s -- the suite changed; read run.log before trusting the comparison%s\n' \
    "$YEL" "$EXPECT_PASS" "$PASS" "$OFF"
fi
printf '  tables  %s\n' "$(ls output/tables/*.csv 2>/dev/null | wc -l | tr -d ' ')"
# only reported when figures were asked for, so a default run does not print a
# zero that reads like something failed to render
[ -d output/figures ] && printf '  figures %s\n' "$(ls output/figures/*.png 2>/dev/null | wc -l | tr -d ' ')"
[ -d output/tables ] || die "run_all.R produced no output/tables -- see run.log"

# ------------------------------------------------------------- 2. figure 6
# GATED ON NBS_FIGURES, like the R figures. The published figures are composed
# separately, so by default this run produces the tables and the audit and no
# images at all, and output/figures is not created. Set NBS_FIGURES=on to render.
if [ "${HAVE_MPL:-0}" = 1 ] && [ "$(printf '%s' "${NBS_FIGURES:-off}" | tr 'A-Z' 'a-z')" != "off" ]; then
  step "2. Rebuilding figure 6  (drawn in Python from the flow tables)"
  mkdir -p output/figures
  # FORCE A HEADLESS BACKEND. On macOS matplotlib defaults to the MacOSX
  # backend, which needs a window server and aborts under a plain shell -- the
  # figure did not build on a reviewer's machine for that reason alone. Agg
  # writes PNG and PDF without a display and is what the figure is tuned for.
  export MPLBACKEND=Agg
  FIG6_OK=1
  "${PY_FIG:-python3}" verify/build_fig06.py output/tables && \
    { [ -f fig06_fitness_mechanism.png ] && mv -f fig06_fitness_mechanism.png output/figures/ ; \
      [ -f fig06_fitness_mechanism.pdf ] && mv -f fig06_fitness_mechanism.pdf output/figures/ ; \
      printf '  written to output/figures/fig06_fitness_mechanism.{png,pdf}\n'; } || \
    { FIG6_OK=0; printf '%s  figure 6 did not build -- the tables above are unaffected%s\n' "$YEL" "$OFF"; }
fi
: "${FIG6_OK:=1}"

# ------------------------------------------------- 2b. seed expected/figures
# The bundle ships expected/tables but NOT expected/figures: 28 MB of rendered
# images with no verification value, since nothing compares figures, only tables.
# They are GENERATED here instead, so a recipient still ends up with a complete
# expected/ after one run. Seeded only when absent, so a later run cannot quietly
# overwrite a reference set someone is diffing against.
if [ "$(printf '%s' "${NBS_FIGURES:-off}" | tr 'A-Z' 'a-z')" != "off" ]; then
step "2b. Populating expected/figures from this run"
if [ -d output/figures ] && [ -n "$(ls -A output/figures 2>/dev/null)" ]; then
  if [ -d expected/figures ] && [ -n "$(ls -A expected/figures 2>/dev/null)" ]; then
    printf '  expected/figures already present (%s files) -- left untouched\n' \
      "$(ls -1 expected/figures | wc -l | tr -d ' ')"
  else
    mkdir -p expected/figures && cp -f output/figures/* expected/figures/ && \
      printf '  generated %s files into expected/figures\n' \
        "$(ls -1 expected/figures | wc -l | tr -d ' ')"
  fi
else
  printf '%s  no figures in output/ to seed from%s\n' "$YEL" "$OFF"
fi
fi

# ------------------------------------------------------- 3-4. verification
if [ "$HAVE_PY" = 1 ]; then
  step "3. Comparing your tables against ours, cell by cell"
  # The comparator treats "missing on both sides" as a match. Prove it can still
  # catch a real difference BEFORE trusting it to report a match, otherwise a
  # too-permissive comparator would report success by failing to look.
  "${PY_PANDAS:-python3}" verify/compare_run.py --self-test expected/tables || die "the comparator failed its own self-test"
  "${PY_PANDAS:-python3}" verify/compare_run.py expected/tables output/tables || die "tables differ (listed above)"

  step "3b. Exporting the corpus to CSV for the manuscript verifier"
  # verify_manuscript.py reads the corpus with pandas.read_csv, but the
  # shipped corpus is an .xlsx and no exporter existed, so this check could not be
  # run and was never wired in -- which is how a Results section kept quoting the
  # stale verdicts after the corpus is replaced.
  "${PY_PANDAS:-python3}" verify/export_dataset_csv.py >/dev/null &&     printf '  output/derived/corpus.csv written\n' ||     printf '%s  corpus export failed -- step 5 will be skipped%s\n' "$YEL" "$OFF"
  # QUOTABLE_FACTS.csv is tracked and is an INPUT to step 5, so a run must not
  # overwrite it; output/tables/18_quotable_facts.csv is this run's own copy.

  step "4. Claim-level relations"
  CLAIM_OUT=$("${PY_PANDAS:-python3}" verify/verify_claims.py output/tables) || {
    printf '%s\n' "$CLAIM_OUT" | grep -E '^  FAIL|^FAILED|^  - ' | sed 's/^/  /'
    die "a claim-level relation does not hold"; }
  printf '%s\n' "$CLAIM_OUT" | tail -2 | sed 's/^/  /'

  step "5. Manuscript numbers against the run"
  # Necessary but not sufficient on its own: it asserts every number in the prose
  # appears in some run table. verify_claims.py above is what catches a real number
  # used in a false relation. Documents are checked only if they are present.
  # The results file is matched by glob, never named literally: naming one file
  # meant a rename silently dropped the
  # one document this check exists for -- the run reported "0 findings" while the
  # actual Results section was never read. Globbed so a rename cannot do it again.
  MS_DOCS=""
  for _d in RESULTS*.md; do
    [ -f "$_d" ] && MS_DOCS="$MS_DOCS $_d"
  done
  case "$MS_DOCS" in *RESULTS*) : ;;
    *) printf '  no RESULTS.md present -- place the Results text there to include it in this check\n' ;;
  esac
  if [ -n "$MS_DOCS" ] && [ -f output/derived/corpus.csv ]; then
    MS_OUT=$("${PY_PANDAS:-python3}" verify/verify_manuscript.py \
               output/derived/corpus.csv output/tables $MS_DOCS 2>&1) || true
    # Show EVERY document checked, not just the tail. `tail -3` displayed
    # only the last file, which is how a reviewer concluded the Results section
    # was not being verified when in fact it was -- the evidence was cropped.
    printf '%s\n' "$MS_OUT" | grep -E '^ *(PASS|FAIL|total findings)' | sed 's/^/  /'
    if printf '%s\n' "$MS_OUT" | grep -q '^total findings: 0'; then :
    elif printf '%s\n' "$MS_OUT" | grep -q '^total findings:'; then
      printf '%s  manuscript numbers need review -- see the findings above%s\n' "$YEL" "$OFF"
    else
      # The verifier did not reach a tally at all: it aborted. Previously this
      # printed the same soft "needs review" line with nothing above it, so a
      # crashed check looked like a mild warning. Show what it actually said.
      printf '%s  MANUSCRIPT CHECK DID NOT RUN:%s\n' "$RED" "$OFF"
      printf '%s\n' "$MS_OUT" | tail -5 | sed 's/^/    /'
    fi
  fi
  # counted from the checker's own derived tally, never hardcoded here
  CLAIMS=$(printf '%s\n' "$CLAIM_OUT" | grep -oE '^[0-9]+ checks' | grep -oE '^[0-9]+')
fi
CLAIMS="${CLAIMS:-0}"

# The summary states ONLY what actually ran. An earlier version printed
# "all tables identical to ours" unconditionally -- including on a machine where
# pandas was missing and the comparison had been skipped, which is a claim the
# run had not earned. Whether the comparison happened is now carried in a flag.
printf '\n%s================================================================%s\n' "$GRN" "$OFF"
if [ "$HAVE_PY" = 1 ] && { [ "$FIG6_OK" = 1 ] || [ "$(printf '%s' "${NBS_FIGURES:-off}" | tr 'A-Z' 'a-z')" = "off" ]; }; then
  printf '%sREPRODUCED.%s %s tables verified identical to ours, %s claim-level relations hold,\n' \
    "$GRN" "$OFF" "$(ls output/tables/*.csv | wc -l | tr -d ' ')" "$CLAIMS"
  printf '%s verification checks passed and none failed.\n' "$PASS"
elif [ "$HAVE_PY" = 1 ]; then
  printf '%sTABLES REPRODUCED, FIGURE 6 DID NOT BUILD.%s %s tables identical, %s claim relations hold,\n' \
    "$YEL" "$OFF" "$(ls output/tables/*.csv | wc -l | tr -d ' ')" "$CLAIMS"
  printf '%s checks passed -- but figure 6 is missing from output/figures and this run is INCOMPLETE.\n' "$PASS"
  printf 'Retry with: MPLBACKEND=Agg %s verify/build_fig06.py output/tables\n' "${PY_FIG:-python3}"
  FINAL_RC=1
else
  printf '%sPIPELINE RAN.%s %s checks passed and the one deliberate fig10_context_deviations failure is\n' \
    "$YEL" "$OFF" "$PASS"
  printf 'present as expected, producing %s tables.\n\n' "$(ls output/tables/*.csv | wc -l | tr -d ' ')"
  printf '%sNOT YET CONFIRMED against our results:%s the comparison needs pandas.\n' "$YEL" "$OFF"
  printf '  python3 -m pip install pandas\n'
  printf '  %s verify/compare_run.py expected/tables output/tables\n' "${PY_PANDAS:-python3}"
  printf '  %s verify/verify_claims.py output/tables\n' "${PY_PANDAS:-python3}"
fi
printf 'Full log: run.log   Outputs: output/tables%s\n' \
  "$([ -d output/figures ] && printf ', output/figures')"
exit "${FINAL_RC:-0}"

