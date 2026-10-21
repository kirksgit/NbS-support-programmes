# =============================================================================
# 00_setup.R  |  Packages, global options, shared helpers
# Project: Programme fit in citizen-led micro-scale NbS support programmes
# =============================================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(stringr)
  library(purrr);  library(tibble)
})

## ---- Global configuration ---------------------------------------------------
## Data location. The first path that exists is used; NBS_DATA overrides all.
## THE FALLBACK LIST WAS A TRAP. It still named three SUPERSEDED workbooks,
## and 00_setup took the first that existed without saying so -- which is how an
## unpinned ./reproduce.sh silently analysed the v54 corpus and rendered a complete
## figure set of wrong numbers. Superseded files are removed from the list; the
## current dataset is the only default, and pick_data() now announces what it chose.
## THE v59 FALLBACK IS REMOVED, for the reason the note above gives. It was
## still listed when NBS_707_FINAL_VERIFIED.xlsx arrived, and the two workbooks
## differ on 34,369 value cells across all 707 records -- 5,536 of them assertions
## the rebuild turned back into "Not stated". A fallback that silently substitutes
## a corpus differing on every record is not a fallback, it is a trap. The v59
## file stays in data/ for provenance and is NOT a candidate.
DATA_CANDIDATES <- c(
  Sys.getenv("NBS_DATA", ""),
  "data/NBS_707_FINAL_VERIFIED.xlsx"
)
## The 714-row WORKING workbook is deliberately NOT a fallback: it is a DIFFERENT
## corpus (714 records, pre-exclusion), so falling back to it would not fail loudly --
## it would produce a complete, internally consistent, wrong analysis. Point NBS_DATA
## at it explicitly if that is genuinely what you want.
pick_data <- function(cands) {
  cands <- path.expand(cands[nzchar(cands)])
  hit <- cands[file.exists(cands)]
  if (!length(hit)) stop("Coding sheet not found. Set Sys.setenv(NBS_DATA = \"/path/to/file.xlsx\").")
  ## ANNOUNCE THE RESOLVED FILE. Silent resolution is what let an unpinned run
  ## analyse a superseded corpus and render a full figure set nobody questioned.
  message(sprintf("  data file: %s%s", hit[1],
                  if (nzchar(Sys.getenv("NBS_DATA"))) " (pinned via NBS_DATA)" else " (default)"))
  hit[1]
}

CFG <- list(
  data_file   = pick_data(DATA_CANDIDATES),
  data_sheet  = "Coding Sheet",
  header_row  = 2,          # row 1 = section banner, row 2 = variable names
  drop_rows   = 1,          # first data row is the template/instruction row
  out_tab     = "output/tables",
  out_fig     = "output/figures",
  out_der     = "output/derived",
  # Pre-registered analysis parameters
  min_prevalence   = 0.05,  # variables below this are described, never modelled
  min_events       = 20,    # minimum cells per outcome level for model inclusion
  fdr_alpha        = 0.05,  # Benjamini-Hochberg level within each test family
  min_country_n    = 10,    # countries below this are pooled as "Other" in models
  mca_dims         = 5,     # dimensions retained from MCA for clustering
  k_range          = 2:8,   # candidate cluster solutions
  seed             = 20260724
)
set.seed(CFG$seed)
options(stringsAsFactors = FALSE, dplyr.summarise.inform = FALSE)

## Output directories were created but never CLEANED, so
## a table whose producing code had been deleted or renamed survived on disk and
## was picked up by the reporting compiler -- 16 such orphans were found. Every run
## now starts from an empty output tree, and save_tab() records a manifest of what
## THIS run actually wrote, which the verifier reconciles against the directory.
## The figures directory is created only when figures will actually be written.
## It is still EMPTIED whenever it exists, because a stale figure from a previous
## run is exactly the orphan this block exists to prevent; it is simply not
## recreated afterwards, so a default run leaves no empty folder behind to
## suggest that something failed to render. save_fig() creates it on first use.
.want_fig <- !identical(tolower(Sys.getenv("NBS_FIGURES", "off")), "off")
for (.d in c(CFG$out_tab, CFG$out_fig, CFG$out_der)) {
  if (dir.exists(.d)) {
    unlink(list.files(.d, full.names = TRUE, recursive = TRUE), force = TRUE)
    if (identical(.d, CFG$out_fig) && !.want_fig) unlink(.d, recursive = TRUE)
  }
  if (!identical(.d, CFG$out_fig) || .want_fig)
    dir.create(.d, recursive = TRUE, showWarnings = FALSE)
}
RUN_MANIFEST <- new.env(parent = emptyenv())
RUN_MANIFEST$tables <- character(0)
RUN_MANIFEST$figures <- character(0)

msg <- function(...) cat(sprintf(...), "\n")

save_tab <- function(x, name) {
  ## THE OUTPUT ENCODING IS PINNED. write.csv() serialises in the
  ## NATIVE encoding, so under a non-UTF-8 locale (LC_CTYPE=C, the default on a
  ## bare shell) every UTF-8-marked string read from the workbook was written as
  ## an escape -- "<U+20AC>" for a euro sign, "<U+00E7>" for a cedilla. The
  ## bundle then failed its own cell-by-cell comparison on 19_retest_pairs.csv
  ## and reported 124 passing checks instead of 127, so ./reproduce.sh died at
  ## step 3 for a reason that had nothing to do with the analysis. Encoding is
  ## now stated rather than inherited from the machine.
  utils::write.csv(x, file.path(CFG$out_tab, paste0(name, ".csv")),
                   row.names = FALSE, fileEncoding = "UTF-8")
  ## C4: record what THIS run wrote, so the verifier can reconcile the output
  ## directory against the manifest and catch an orphan the moment it appears.
  RUN_MANIFEST$tables <- union(RUN_MANIFEST$tables, paste0(name, ".csv"))
  invisible(x)
}

# =============================================================================
# NORMALISATION HELPERS
# The coded sheet mixes controlled vocabulary with free-text qualifiers,
# "[inferred]" flags, and em-dash annotations. Every helper below is
# deliberately conservative: anything it cannot map is returned as NA and
# counted, never silently coerced to a substantive value.
# =============================================================================

## Squash whitespace, unify dashes, lower-case
norm_txt <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "[\u2010-\u2015\u2212]", "-")   # all dashes -> hyphen
  x <- str_replace_all(x, "[\u00a0\u3000]", " ")
  x <- str_squish(x)
  x
}

## TRUE where the cell carries an "[inferred]" qualifier
is_inferred <- function(x) str_detect(norm_txt(x), regex("\\[inferred", ignore_case = TRUE))

## Remove bracketed qualifiers so prefix matching can work
strip_qual <- function(x) {
  x <- norm_txt(x)
  ## "[was prose: ...]" AND "[was: ...]" ARE PROVENANCE, NOT VALUES.
  ##
  ## These annotations record what a cell said BEFORE normalisation, exactly as
  ## "[inferred]" records how it was arrived at. Only the first two were stripped, so
  ## the pre-normalisation prose stayed inside the string that every multi_has()
  ## keyword pattern reads. 140 cells across 8 fields carry one, and in 5 of them the
  ## bracket names a category the normalised value does NOT carry -- e.g. D-4 read
  ## "Not stated [was prose: Other]", and the itemised channel flags recorded an
  ## "Other" channel for a record documenting none. The flag set must not depend on
  ## whether a coder happened to leave a note about the earlier wording.
  ##
  ## The ternary parser was checked and is unaffected. The count in an earlier draft
  ## of this comment was WRONG and an external reading caught it: I had counted the
  ## annotated cells in every field I inspected by hand, including fields ternary()
  ## never parses. Recounted against the fields the pipeline actually passes to
  ## ternary() (nine of them), 20 cells carry a [was ...] annotation -- v_D1 6,
  ## v_D3 6, v_E13 8 -- and 0 of the 20 resolve differently with the bracket removed,
  ## because those values lead with the code and the annotation is never reached.
  ## The exposure is keyword flags only. The lesson is the one this whole change is
  ## about: a count typed from a manual inspection is not a measured count.
  x <- str_remove_all(x, regex("\\[(inferred|free text|was prose|was)[^]]*\\]",
                               ignore_case = TRUE))
  str_squish(x)
}

## Structural non-codes: "Not applicable", "Cannot verify", template leftovers
NA_PAT  <- regex("^(not applicable|n/?a\\b|nicht zutreffend)", ignore_case = TRUE)
UNV_PAT <- regex("^(cannot verify|unverifiable|no access)", ignore_case = TRUE)
TPL_PAT <- regex("^(yes ?/ ?no ?/|yes ?/ ?partial|\\[free text\\]|^-$)", ignore_case = TRUE)
## shapes that carry a coded value with the option word stripped (see pass 6).
MAINT_DURATION     <- regex("(^|[^a-z])([0-9]{1,3}|ten|five|three|twenty)\\s?(years?|yrs?)\\b|\\bin perpetuity\\b|\\bpermanent\\b", ignore_case = TRUE)
MAINT_OBLIGATION   <- regex("(condition|obligation|requirement)[^.;]{0,30}(applies|imposed|required|mandatory)", ignore_case = TRUE)
FISCAL_INSTRUMENT  <- regex("(bill|tax|fee|charge)[- ]?(credit|reduction|rebate|waiver|discount)|property-tax", ignore_case = TRUE)
MATERIAL_PROVISION <- regex("(discounted|subsidised|subsidized|reduced-price)[a-z ]{0,18}(sale|purchase|barrel|tank|bin|kit)|(barrel|tank|kit|material)[a-z /]{0,12}provision", ignore_case = TRUE)

## Three-level coded variable -> yes | no | not_stated ; NA for n/a & unverifiable
## The parser anchored every pattern at the
## START of the string, so any coder who led with the measure rather than the
## verdict was silently dropped to NA:
##
##   "Green Walls and Roofs: Yes - 50% up to $5,000 AUD; Rainwater Tanks: Yes ..."
##
## is unambiguously a documented grant, and it scored NA. 132 substantive cells
## were lost this way (E-8 72, E-12 35, G-2 25), which understated capital grants
## and propagated into instrument packages, the typology, the MCA and the models.
##
## The anchored patterns are kept as the FIRST pass, because a leading token is
## the coders' convention and an unambiguous signal. A second pass then reads a
## verdict that appears after a field label ("<label>: Yes"), and a third accepts
## a bare controlled term ("Recognition", "Conditional") where the whole cell is
## that term. Anything still unresolved but substantive is NOT silently coerced --
## `unparsed_substantive()` collects it and the verifier halts the build.
ternary <- function(x) {
  s <- strip_qual(x)
  out <- rep(NA_character_, length(s))

  ## pass 1 -- leading token (unchanged, highest precedence)
  out[str_detect(s, regex("^yes", ignore_case = TRUE))]        <- "yes"
  ## PASS 0 -- SEMANTIC GUARDS, run BEFORE any pattern below, because the faults
  ## they fix are all cases where a later pattern wins over the cell's real sense.
  ## (a) zero amount = documented absence, whatever else the cell says
  out[str_detect(s, ZERO_AMOUNT) &
        str_detect(s, regex("no [a-z ]*(rebate|grant|subsid|incentive|payment)",
                            ignore_case = TRUE))]                <- "no"
  ## (a2) A LIST OF NAMED REBATES WITH AMOUNTS is a documented cash provision even
  ##      when the cell never writes "Yes". Case 664 opens "Water-Wise Plants
  ##      rebate: up to $300 ..." and lists five rebates; the audit attributed its
  ##      mis-parse to the "no minimum" clause, but the real cause is different and
  ##      I checked it rather than adopt the diagnosis: the cell simply never
  ##      matched any positive pattern and fell through to NA. A named provision
  ##      carrying an amount IS the documented answer.
  out[is.na(out) &
        str_detect(s, regex("(rebate|grant|subsid|incentive|credit|voucher)[^.]{0,40}[$\u20ac\u00a3]\\s?[0-9]",
                            ignore_case = TRUE)) &
        !str_detect(s, ZERO_AMOUNT)]                            <- "yes"
  ## (b) positive lead + trailing negative clause = the positive stands
  .pos_lead <- opens_positive(s) & is.na(out)
  out[.pos_lead & str_detect(s, regex("^(partial|partly)", ignore_case = TRUE))] <- "partial"
  out[.pos_lead & is.na(out)]                                    <- "yes"
  ## (c) "no minimum / no cap / no formal prize" qualify terms, they do not negate
  ##     the provision -- strip them so the generic negative pass cannot fire on them
  s_eff <- str_remove_all(s, NEG_QUALIFIER)

  out[str_detect(s, regex("^(no\\b|no-|no,|no;|no\\.|nein)", ignore_case = TRUE))] <- "no"
  out[is.na(out) & str_detect(s_eff, regex("^(not stated|none/not stated|none|not documented|unclear)",
                          ignore_case = TRUE))]                <- "not_stated"
  out[str_detect(s, regex("^partial", ignore_case = TRUE))]    <- "partial"

  ## pass 2 -- verdict after a field label, e.g. "Rainwater Tanks: Yes - 50% ...".
  ## A "Yes" anywhere in a labelled list means at least one listed item is
  ## documented, which is exactly what the presence indicator asks.
  .todo <- is.na(out)
  out[.todo & str_detect(s, regex("(^|[:;,\\-]\\s*)yes\\b", ignore_case = TRUE))] <- "yes"
  .todo <- is.na(out)
  out[.todo & str_detect(s, regex("(^|[:;,\\-]\\s*)partial", ignore_case = TRUE))] <- "partial"
  .todo <- is.na(out)
  out[.todo & str_detect(s, regex("(^|[:;,\\-]\\s*)no\\b", ignore_case = TRUE))]  <- "no"

  ## pass 3 -- a bare controlled term occupying the whole cell. These are coded
  ## vocabulary, not free text: "Recognition" (E-12) and "Conditional" (G-2) are
  ## documented provisions, and "Indirect" is a documented qualification.
  .todo <- is.na(out)
  out[.todo & str_detect(s, regex("^(recognition|award|label|certif\\w*|competition|plaque|conditional|indirect|direct|mentioned)\\.?$",
                                  ignore_case = TRUE))]        <- "yes"

  ## pass 4 -- FIELD-SHAPED EVIDENCE, added after inspecting every cell the first
  ## three passes still dropped. These are five distinct shapes, each resolved on
  ## what the coded content actually means, not on a catch-all:
  ##
  ##  (a) a MONETARY AMOUNT or a stated rate is a documented cash provision. E-8
  ##      cells like "Green roof: $100/m2 up to $100,000; Cool roof: $5/m2" name a
  ##      grant without ever writing "Yes".
  ##  (b) "Full" / "Complete" (D-3 form availability) is a documented provision,
  ##      stronger than a bare Yes, not weaker.
  ##  (c) a named RECOGNITION or COMPETITION instrument (E-12) is documented even
  ##      when the cell leads with "Type:" or "Case study".
  ##  (d) "Possibly" / "may still" (G-2) is explicitly NOT a documented pathway.
  ##      It is the coder recording uncertainty, so it maps to not_stated -- the
  ##      conservative reading, and the one the silence rule requires.
  ##  (e) a bare NAMED BODY or office (C-3, C-4 contact fields) is an identifier,
  ##      not a ternary. It stays NA here and is read by its own parser; these
  ##      fields feed no presence indicator, so NA is correct rather than lossy.
  .todo <- is.na(out)
  out[.todo & str_detect(s, regex("possibly|may still|might still|unclear whether",
                                  ignore_case = TRUE))]        <- "not_stated"
  .todo <- is.na(out)
  out[.todo & str_detect(s, "[\u20ac\u00a3$\u00a5]\\s?[0-9]|[0-9][0-9.,]*\\s?(%|/m|per m|eur|usd|cad|aud|chf|gbp)")] <- "yes"
  .todo <- is.na(out)
  out[.todo & str_detect(s, regex("^(full|complete)\\b", ignore_case = TRUE))] <- "yes"
  .todo <- is.na(out)
  out[.todo & str_detect(s, regex("recognition|competition|wettbewerb|award|public gallery|case study|kataster",
                                  ignore_case = TRUE))]        <- "yes"

  ## pass 5 -- the last eight cells, each read individually rather than by a
  ## catch-all, because a catch-all here would be coercion not parsing:
  ##  * "Mentioned: ... is explicitly ..." / "subsidy scheme present" (E-2) --
  ##    the coder is recording that the measure IS documented.
  ##  * "Indirect (via fees/rebate on bill)" / "Partly / Indirect" (E-9) -- a
  ##    documented but indirect provision, which is a PARTIAL, not an absence.
  ##  * a named in-kind incentive (E-12: donated barrel, free soaker hose) is a
  ##    documented provision even with no verdict token.
  .todo <- is.na(out)
  ## GUARDED. "is explicitly" was written for "Mentioned: X is explicitly
  ## documented", but it also fired on case 583's E-6, whose text reads
  ## "Rainwater storage IS NOT LISTED among the supported measures. The programme
  ## is explicitly a grass-to-desert-landscape rebate." -- a sentence asserting
  ## the measure is ABSENT. It was counted as a documented rain barrel provision,
  ## inflating the corpus's single most-quoted measure. A cell that states the
  ## measure is not listed/supported can never resolve to "yes" here.
  .not_listed <- str_detect(s, regex(paste0("not listed|not among|is not supported|",
                                            "are not supported|not included among|not offered|",
                                            "no [a-z ]{0,25}(listed|among the supported)"),
                                     ignore_case = TRUE))
  out[.todo & !.not_listed &
        str_detect(s, regex("^mentioned\\b|scheme present|is explicitly|explicitly (listed|documented|stated)",
                                  ignore_case = TRUE))]        <- "yes"
  ## An explicit statement that the measure is NOT listed/supported is a
  ## DOCUMENTED ABSENCE under the project's own rule (source reached and the
  ## source excludes the feature), not silence and not an unparsed cell.
  out[is.na(out) & .not_listed]                                <- "no"
  .todo <- is.na(out)
  out[.todo & str_detect(s, regex("^(partly|indirect)\\b|partly ?[/|] ?indirect",
                                  ignore_case = TRUE))]        <- "partial"
  .todo <- is.na(out)
  out[.todo & str_detect(s, regex("free [a-z ]*(barrel|hose|tree|plant|kit)|donated|in-kind item|participation incentive",
                                  ignore_case = TRUE))]        <- "yes"

  ## pass 6 -- v50: PREFIX-STRIPPED CELLS. The 707-case v44 analysis file records a
  ## substantive value with the codebook option word REMOVED, leaving only the
  ## qualifying clause: E-17 case 620 reads "20 years beginning on the date of the
  ## Final Acceptance Letter" where the previous file read "Not stated"; case 155
  ## reads "Minimum 10 years"; E-9 case 661 names a property-tax-bill credit; E-11
  ## case 634 names discounted barrel sales. Eight cells across three fields fell
  ## through every pass above to NA, which is a LOST CODED VALUE, not a silence.
  ##
  ## Each rule below is read off the cell shape it must resolve, and all four are
  ## applied to NA CELLS ONLY. That restriction is load-bearing, not stylistic:
  ## measured on this corpus, FISC also matches 48 cells that already parse as an
  ## explicit "No" ("No - no property-tax or bill-based reduction") and MATP another
  ## 75, because a denial names the instrument it denies. An unrestricted rule would
  ## silently convert 129 documented absences into affirmatives. NA-only, the four
  ## rules change exactly 8 cells and alter no existing parse -- both asserted below
  ## and in 16_verify.R.
  ##
  ## Beware: grepl() SILENTLY DISCARDS a stringr regex()'s ignore_case flag, so an
  ## early version of this check matched case-sensitively and under-counted the
  ## collateral above. Use str_detect() with these patterns, never grepl().
  .todo <- is.na(out)
  out[.todo & str_detect(s, MAINT_DURATION)] <- "yes"   # a stated duration IS the condition
  .todo <- is.na(out)
  out[.todo & str_detect(s, MAINT_OBLIGATION)] <- "yes" # obligation stated, duration not
  .todo <- is.na(out)
  out[.todo & str_detect(s, FISCAL_INSTRUMENT)] <- "yes"# a named bill/tax credit is a provision
  .todo <- is.na(out)
  out[.todo & str_detect(s, MATERIAL_PROVISION)] <- "yes"# named discounted/in-kind supply

  ## sentinel patterns always win: template text, unverifiable, explicit NA
  out[str_detect(s, NA_PAT) | str_detect(s, UNV_PAT) |
        str_detect(s, TPL_PAT) | is.na(s) | s == ""]           <- NA_character_
  out
}

## Substantive text the parser could not resolve. Empty is the pass condition --
## a non-empty result means real coded content is being discarded, which is the
## defect C3 identified, so the verifier stops the build on it.
## -----------------------------------------------------------------------------
## SEMANTIC INVERSIONS (external round-2 audit, HIGH -- all 7 upheld on inspection).
##
## The round-1 parser audit asked "was the cell parsed to a non-NA value?" That
## measures completeness, not correctness, so seven cells were parsed to the
## OPPOSITE of what they say and passed every check. Verified individually:
##
##   149 G-2  "Full tenant pathway - no owner approval required"   -> parsed no
##   406/486/587/632 E-12 social-media recognition "; no formal
##                    reward/competition"                          -> parsed no
##   645 E-3  "$0.00 (no rain garden rebate)"                      -> parsed YES
##   664 E-8  several rebates, one clause "no minimum"             -> parsed no
##
## Three distinct causes, so three distinct rules -- a single catch-all would
## just move the error somewhere else:
##
##  (a) A TRAILING NEGATIVE CLAUSE qualifies a positive lead. "Full tenant
##      pathway - no owner approval required" is the strongest possible YES; the
##      "no" governs a subordinate condition. Same for "recognition ...; no formal
##      prize". Rule: if the cell OPENS with a documented provision, a later "no"
##      cannot flip it.
##  (b) A ZERO AMOUNT IS A DOCUMENTED ABSENCE. "$0.00 (no rain garden rebate)"
##      was read as yes because it contains a currency figure. A zero is the one
##      amount that means the provision is not funded.
##  (c) "NO MINIMUM" / "NO MAXIMUM" / "NO CAP" ARE NOT NEGATIONS of the
##      provision. They qualify its terms. 664 lists several rebates with amounts.
##
## Each is exercised by a named regression test in R/16_verify.R that asserts the
## EXPECTED DIRECTION for all seven cells, so a future edit that re-inverts any of
## them halts the build.
NEG_QUALIFIER <- regex(paste0("no (minimum|maximum|cap|limit|upper limit|match|fee|cost|charge)",
                              "|no formal (reward|prize|competition|scheme|certificate)",
                              "|no owner approval|no additional|no separate application"),
                       ignore_case = TRUE)
ZERO_AMOUNT   <- regex("(^|[^0-9])[$\u20ac\u00a3]?0(\\.0{1,2})?($|[^0-9%])", ignore_case = TRUE)

## Does the cell OPEN with a documented provision? Used to stop a trailing
## negative clause from inverting a positive lead.
opens_positive <- function(s) {
  stringr::str_detect(s, regex(paste0("^(yes|full|partial|partly|recognition|social media",
                                      "|award|label|competition|certificate)"),
                               ignore_case = TRUE)) %in% TRUE
}

## Fields that are IDENTIFIERS, not ternaries: C-3/C-4 name the responsible body
## or the contact, so a named office is the answer and NA from ternary() is
## correct rather than lossy. They feed no presence indicator. Declared here so
## the audit does not report them as parser losses.
## The fields ternary()/yes1()/no1() actually parse. Scoping the audit to these is
## the point: a free-text narrative field is not a parser loss, and sweeping every
## v_* column reported 18,467 "losses" that were simply prose.
TERNARY_FIELDS <- c("v_C1","v_C2","v_C3","v_C4","v_C5","v_C6","v_C7","v_C8","v_C9",
                    "v_C11","v_C12","v_C13","v_C14","v_C15","v_C16",
                    "v_D2","v_D6","v_D7","v_D8",
                    "v_E1","v_E2","v_E3","v_E4","v_E5","v_E6","v_E8","v_E9","v_E10",
                    "v_E11","v_E12","v_E15","v_E16","v_E17",
                    "v_F1","v_F3","v_G1","v_G2","v_G3","v_G5","v_G6")
## Of those, the ones whose answer is a NAMED BODY or contact rather than a
## verdict: C-3 names the responsible office, C-4 the contact. yes1() on these is
## only ever used as "is anything named", so a named office correctly yields NA
## from ternary() and is not a loss.
## C-1 is the lead body, same shape as C-3/C-4: "Public Works Department" is the
## answer, not a verdict.
NON_TERNARY_FIELDS <- c("v_C1", "v_C3", "v_C4")
## The fields the audit must sweep: parsed as ternaries AND genuinely verdict-shaped.
AUDIT_TERNARY_FIELDS <- setdiff(TERNARY_FIELDS, NON_TERNARY_FIELDS)

unparsed_substantive <- function(x) {
  s <- strip_qual(x); t <- ternary(x)
  keep <- !is.na(s) & s != "" & nchar(s) > 3 & is.na(t) &
    !str_detect(s, regex("^(not stated|none|not documented|unclear|not applicable|cannot verify)",
                         ignore_case = TRUE)) &
    !str_detect(s, NA_PAT) & !str_detect(s, UNV_PAT) & !str_detect(s, TPL_PAT)
  which(keep)
}

## yes -> 1, {no, not_stated} -> 0, NA preserved.  Documented-presence indicator:
## implements the conservative documentation rule (absence of evidence != absence).
yes1 <- function(x) {
  t <- ternary(x); ifelse(is.na(t), NA_integer_, as.integer(t == "yes"))
}
## explicit exclusion indicator (needed for CONFIRMED misfit, which requires a
## closed enumeration / explicit "No", never a "Not stated")
no1  <- function(x) {
  t <- ternary(x); ifelse(is.na(t), NA_integer_, as.integer(t == "no"))
}

## Controlled single-select: match against permitted options, first hit wins.
## `opts` is a named character vector: names = canonical label, values = regex.
pick_option <- function(x, opts) {
  s <- strip_qual(x)
  out <- rep(NA_character_, length(s))

  ## SILENCE PRECEDENCE (reviewer convention: "not stated" means NOT STATED).
  ## A cell that OPENS with a silence token is silent, whatever it goes on to
  ## explain. Previously the option patterns were matched anywhere in the string,
  ## so G-4 "Not stated (eligibility restricted to public-sewer service area, NOT
  ## citywide...)" matched "citywide" inside its own negation and was recorded as
  ## a documented citywide scope -- the coder's caveat inverted into the opposite
  ## finding. Silence is therefore resolved FIRST and is never overridden.
  .silent <- str_detect(s, regex("^(not stated|cannot verify|unclear|unknown|no information)",
                                 ignore_case = TRUE)) %in% TRUE

  for (nm in names(opts)) {
    hit <- is.na(out) & !.silent & str_detect(s, regex(opts[[nm]], ignore_case = TRUE))
    out[hit] <- nm
  }
  out[str_detect(s, NA_PAT) | str_detect(s, TPL_PAT)] <- NA_character_
  out
}

## Multi-select membership: does the cell list `item`?
multi_has <- function(x, pattern) {
  s <- strip_qual(x)
  bad <- str_detect(s, NA_PAT) | str_detect(s, TPL_PAT) | is.na(s) | s == ""
  out <- as.integer(str_detect(s, regex(pattern, ignore_case = TRUE)))
  out[bad] <- NA_integer_
  out
}

## Count of items in a ';'-separated multi-select, excluding null answers
multi_n <- function(x) {
  s <- strip_qual(x)
  null_ans <- str_detect(s, regex("^(not stated|none|none/not stated|not applicable)",
                                  ignore_case = TRUE))
  n <- str_count(s, ";") + 1L
  n[null_ans] <- 0L
  n[is.na(s) | s == ""] <- NA_integer_
  n
}

## First number in a messy numeric string ("~1,700 inh/km2 (LGA)" -> 1700)
first_num <- function(x) {
  s <- str_replace_all(norm_txt(x), "[,\u00a0]", "")
  m <- str_extract(s, "[0-9]+(\\.[0-9]+)?")
  suppressWarnings(as.numeric(m))
}

## First plausible 4-digit year
first_year <- function(x) {
  s <- norm_txt(x)
  m <- str_extract(s, "\\b(18|19|20)[0-9]{2}\\b")
  suppressWarnings(as.integer(m))
}

## Free-text keyword detector (returns 0/1, NA only if the cell is empty)
## =============================================================================
## NEGATION-AWARE KEYWORD MATCHING.
##
## The coders recorded absence in E-7 with a standing boilerplate:
##   "no other NbS practices (infiltration pits, downspout disconnection,
##    native planting, courtyard greening) are supported"
## Every measure named in that list is a measure the programme does NOT offer.
## kw() is a plain regex and matched them anyway, so records documenting the
## OPPOSITE were counted as documenting the measure. This inflated m_planting,
## m_infiltr, m_disconn, m_rainharv, m_compost, m_pond and m_depave.
##
## kw_pos() keeps a match only where it is NOT inside a negative scope. A match
## is negated when its own clause (bounded by ; . | : or brackets) carries a
## negator, or when a negator governs it from up to 120 characters back within
## the same sentence -- which is what the parenthesised boilerplate looks like.
##
## Validated on the corpus: of 194 records matching the planting vocabulary, 48
## match ONLY inside a negative scope, and all 48 are the boilerplate. See
## data/E7_MEASURE_CLASSIFICATION.csv.
## =============================================================================
.NEG_RE <- "\\b(no|not|none|without|neither|nor|absent|lacks?|lacking|excluded|ineligible)\\b"

kw_pos <- function(x, pattern) {
  s <- strip_qual(x)
  out <- integer(length(s))
  for (i in seq_along(s)) {
    si <- s[i]
    if (is.na(si) || !nzchar(si)) { out[i] <- NA_integer_; next }
    ## ABBREVIATION PERIODS ARE NOT CLAUSE BOUNDARIES.
    ## The clause span below is bounded by ; . | : and brackets. "e.g." carries
    ## two periods, so the boilerplate "no other NbS practices (e.g. infiltration
    ## pits, downspout disconnection, native planting)" split immediately after
    ## "e.g", leaving the measure in a clause with no negator in it -- and the
    ## negation guard under-fired on exactly the records it exists for
    ## (3 Bunbury, 425 Battle Creek, 455 Centreville, 482 Dundalk,
    ## 671 Springfield). The original validation checked only over-firing.
    ## Periods inside the common abbreviations are removed before splitting.
    si <- gsub("\\be\\.\\s?g\\.", "eg", si, ignore.case = TRUE, perl = TRUE)
    si <- gsub("\\bi\\.\\s?e\\.", "ie", si, ignore.case = TRUE, perl = TRUE)
    si <- gsub("\\b(etc|approx|incl|vs|cf|resp|est)\\.", "\\1", si,
               ignore.case = TRUE, perl = TRUE)
    loc <- stringr::str_locate_all(si, stringr::regex(pattern, ignore_case = TRUE))[[1]]
    if (!nrow(loc)) { out[i] <- 0L; next }
    keep <- FALSE
    for (j in seq_len(nrow(loc))) {
      st <- loc[j, "start"]
      ## clause containing the match
      pre  <- substr(si, 1, st - 1)
      cuts <- gregexpr("[;.|:()]", pre)[[1]]
      cs   <- if (cuts[1] == -1) 1L else max(cuts) + 1L
      post <- substr(si, st, nchar(si))
      nxt  <- regexpr("[;.|:()]", post)
      ce   <- if (nxt == -1) nchar(si) else st + nxt - 2L
      clause <- substr(si, cs, max(cs, ce))
      neg <- grepl(.NEG_RE, clause, ignore.case = TRUE, perl = TRUE)
      if (!neg) {
        ## a negator governing from earlier in the same sentence
        look <- substr(si, max(1L, st - 200L), st - 1L)
        neg <- grepl(paste0(.NEG_RE, "[^.;]{0,120}$"), look, ignore.case = TRUE, perl = TRUE)
      }
      if (!neg) { keep <- TRUE; break }
    }
    out[i] <- as.integer(keep)
  }
  out
}

kw <- function(x, pattern) {
  s <- strip_qual(x)
  out <- as.integer(str_detect(s, regex(pattern, ignore_case = TRUE)))
  out[is.na(s) | s == ""] <- NA_integer_
  out
}

## Row-wise "any documented yes" across several ternary columns, NA-safe
any_yes <- function(df, cols) {
  m <- as.matrix(df[, cols, drop = FALSE])
  apply(m, 1, function(r) if (all(is.na(r))) NA_integer_ else as.integer(any(r == 1, na.rm = TRUE)))
}
sum_yes <- function(df, cols) {
  m <- as.matrix(df[, cols, drop = FALSE])
  apply(m, 1, function(r) if (all(is.na(r))) NA_integer_ else sum(r == 1, na.rm = TRUE))
}

## Are ALL listed ternary columns undocumented (not stated / NA)?  Used to
## separate documentation gaps from substantive absences.
all_unknown <- function(df, cols) {
  m <- as.matrix(df[, cols, drop = FALSE])
  apply(m, 1, function(r) as.integer(all(is.na(r) | r == 0)))
}

# =============================================================================
# ANALYSIS HELPERS
# =============================================================================

## Country-standardised prevalence: each country contributes equally.
## Guards against the US/JP/DE concentration (72% of the corpus).
country_standardised <- function(df, var, country = "country", min_n = 1) {
  df %>%
    filter(!is.na(.data[[var]])) %>%
    group_by(.data[[country]], .data[[var]]) %>% summarise(n = n(), .groups = "drop") %>%
    group_by(.data[[country]]) %>% mutate(n_c = sum(n), p = n / n_c) %>%
    filter(n_c >= min_n) %>% ungroup() %>%
    group_by(.data[[var]]) %>%
    ## n_countries records how many countries actually carry the level,
    ## which is what the previous mean was (wrongly) divided by. The mean is now
    ## taken over the completed grid built by the caller.
    summarise(prog_weighted = sum(n) / sum(df[[var]] %in% na.omit(unique(df[[var]]))),
              country_standardised = mean(p),
              n_countries = sum(n > 0), .groups = "drop")
}

## Simple distribution table with both weightings.
##
## The country-standardised share is the mean
## across countries of each country's within-country share. The previous version
## averaged only over the countries where a level ACTUALLY OCCURS, because count()
## emits no row for a zero cell -- so a level present in 3 of 28 countries was
## averaged over 3, not 28. Every distribution therefore summed to >1 (observed
## 1.40-1.74 across the seven verdicts), which is not a distribution at all.
##
## The grid is now completed with explicit zeros before averaging, so a country
## that never uses a level contributes 0 to that level's mean. sum() over levels
## is then exactly 1 by construction, and the verifier tests it.
dist_table <- function(df, var, country = "country") {
  prog <- df %>% count(.data[[var]]) %>% mutate(p_prog = n / sum(n))
  d <- df %>% filter(!is.na(.data[[var]]), !is.na(.data[[country]]))
  cstd <- d %>% count(.data[[country]], .data[[var]]) %>%
    tidyr::complete(.data[[country]], .data[[var]], fill = list(n = 0L)) %>%
    group_by(.data[[country]]) %>% mutate(p = n / sum(n)) %>% ungroup() %>%
    group_by(.data[[var]]) %>% summarise(p_country_std = mean(p), .groups = "drop")
  out <- left_join(prog, cstd, by = var)
  ## halt rather than emit an invalid distribution
  .s <- sum(out$p_country_std, na.rm = TRUE)
  if (abs(.s - 1) > 1e-6)
    stop(sprintf("dist_table(%s): country-standardised shares sum to %.4f, not 1", var, .s))
  out
}

## Benjamini-Hochberg within a pre-registered test family
bh <- function(p) p.adjust(p, method = "BH")

## Prevalence screen: returns TRUE if a binary/categorical variable is
## informative enough to model (pre-registered rule, not post hoc).
modellable <- function(x, min_prev = CFG$min_prevalence, min_ev = CFG$min_events) {
  x <- x[!is.na(x)]
  if (!length(x)) return(FALSE)
  tb <- table(x)
  min(tb) >= min_ev && min(tb) / sum(tb) >= min_prev
}
