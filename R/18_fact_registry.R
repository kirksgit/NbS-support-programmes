## =============================================================================
## 18_fact_registry.R  --  THE SINGLE SOURCE OF EVERY QUOTABLE NUMBER
##
## WHY THIS MODULE EXISTS.
## Every error found in this project's write-up phase had one shape: a number or
## a place name sat in a DOCUMENT and no longer traced to what the CODE computed.
## Figures 11/12 quoted a retired D3c construct. Figure 7 quoted st_support for
## D4a. A compounding claim quoted a phi computed on retired weakness flags. Two
## city names were asserted that are not in the corpus, and a tenure range was
## attributed to two cities that do not hold those values. None of these was a
## bug in the analysis: the analysis was right every time. They were drift
## between prose and pipeline, and they were caught only by re-reading.
##
## Re-reading does not scale and does not converge. So the rule changes:
##
##   A NUMBER MAY APPEAR IN A MANUSCRIPT ONLY IF THIS MODULE EMITS IT.
##
## Every figure a human might quote is registered here with a stable key, its
## value, its denominator where it has one, and the table or column it came
## from. Documents are then GENERATED from this registry, and a verifier checks
## that every number in a finished document is a registry value. Prose can no
## longer drift, because prose no longer holds numbers of its own.
##
## The registry is deliberately narrow. It holds what a reader could quote: the
## verdict counts, the rates a claim would rest on, corpus totals, reliability,
## typology sizes and stability, justice provisions. It does not hold every cell
## of every table -- those stay in the 139 result tables, which remain the
## detailed record.
## =============================================================================

.facts <- new.env(parent = emptyenv())
.facts$rows <- list()

## fact(key, value, unit, denom, source) -- register one quotable number.
## `source` names the table/column the value is computed from, so a reader can
## go from a sentence in the paper to the line of code behind it.
fact <- function(key, value, unit = "count", denom = NA_real_, source = "") {
  stopifnot(is.character(key), length(key) == 1L)
  if (!is.null(.facts$rows[[key]]))
    stop(sprintf("fact key '%s' registered twice -- keys must be unique", key))
  .facts$rows[[key]] <- data.frame(
    key = key,
    value = as.numeric(value),
    unit = unit,
    denom = as.numeric(denom),
    pct = if (is.na(denom) || denom == 0) NA_real_ else round(100 * as.numeric(value) / as.numeric(denom), 1),
    source = source,
    stringsAsFactors = FALSE)
  invisible(value)
}

## ---- Corpus scale -----------------------------------------------------------
fact("corpus.n",            nrow(A),                              "count", source = "A")
fact("corpus.n_countries",  length(unique(stats::na.omit(A$country))), "count", source = "A$country")

## ---- Verdict counts, per component -----------------------------------------
## These are the numbers the Results section leads with. Each is read from the
## canonical fit_D* column -- never from a configuration or provision count.
.comp <- c("D1","D2","D3a","D3b","D3c","D4a","D4b")
for (k in .comp) {
  v <- A[[paste0("fit_", k)]]
  evaluable <- sum(!(v %in% c("Indeterminate", "Not applicable")))
  fact(sprintf("fit.%s.documented", k),  sum(v == "Documented fit"),   "count", evaluable, sprintf("A$fit_%s", k))
  fact(sprintf("fit.%s.partial", k),     sum(v == "Partial fit"),      "count", evaluable, sprintf("A$fit_%s", k))
  fact(sprintf("fit.%s.misfit", k),      sum(v == "Confirmed misfit"), "count", evaluable, sprintf("A$fit_%s", k))
  fact(sprintf("fit.%s.indeterminate", k), sum(v == "Indeterminate"),  "count", nrow(A),   sprintf("A$fit_%s", k))
  fact(sprintf("fit.%s.notapplicable", k), sum(v == "Not applicable"), "count", nrow(A),   sprintf("A$fit_%s", k))
  fact(sprintf("fit.%s.evaluable", k),   evaluable,                    "count", nrow(A),   sprintf("A$fit_%s", k))
}

## ---- Compound misfit --------------------------------------------------------
.nm <- rowSums(sapply(.comp, function(k) A[[paste0("fit_", k)]] == "Confirmed misfit"), na.rm = TRUE)
for (i in 0:4) fact(sprintf("compound.n%d", i), sum(.nm == i), "count", nrow(A), "fit_D* rowwise")
fact("compound.two_plus", sum(.nm >= 2), "count", nrow(A), "fit_D* rowwise")
fact("compound.max",      max(.nm),      "count", NA,       "fit_D* rowwise")

## ---- Clearing every condition ----------------------------------------------
## Counted on DOCUMENTED FIT, not on absence of misfit. A programme can carry no
## confirmed misfit while several of its components sit at partial or
## indeterminate, and 11 of the 60 zero-misfit records do, so the zero-misfit
## count above answers a different question from this one and the two must not be
## quoted interchangeably. Registered because the supplement's exemplar table
## states both, and an unregistered number is one the manuscript verifier cannot
## check.
.nf <- rowSums(sapply(.comp, function(k) A[[paste0("fit_", k)]] == "Documented fit"),
               na.rm = TRUE)
fact("allfit.n7",        sum(.nf == 7), "count", nrow(A), "fit_D* rowwise")
fact("allfit.n6_plus",   sum(.nf >= 6), "count", nrow(A), "fit_D* rowwise")
fact("allfit.mean_zero_misfit", round(mean(.nf[.nm == 0]), 2), "components", 7,
     "fit_D* rowwise, zero-misfit subset")
fact("allfit.mean_rest",        round(mean(.nf[.nm >  0]), 2), "components", 7,
     "fit_D* rowwise, remainder")
## Documented-fit pool per component. These are the denominators the exemplar
## table reports, and the spread between them (D1 against D4a) is itself quoted.
for (k in .comp)
  fact(sprintf("fitpool.%s", k), sum(A[[paste0("fit_", k)]] == "Documented fit"),
       "count", nrow(A), sprintf("A$fit_%s", k))

## ---- Instrument families ----------------------------------------------------
## Instrument families, justice provisions and typology sizes are read back from
## the tables the pipeline has already written. Reading the EMITTED table rather
## than an in-memory object means the registry certifies what was actually
## published, not what a variable happened to hold at this point in the run.
.read_out <- function(nm) {
  p <- file.path(CFG$out_tab, paste0(nm, ".csv"))
  if (file.exists(p)) utils::read.csv(p, stringsAsFactors = FALSE) else NULL
}
## COMPUTED FROM A, NOT READ BACK FROM 14_a3_instrument_family_totals.
## This module runs at position 18 in run_all.R and the 14* figure modules run at
## 19-21, while 00_setup CLEARS output/tables at the start of every run. So the
## previous .read_out("14_a3_instrument_family_totals") returned NULL on any clean
## run and these six family facts were SILENTLY ABSENT from the registry -- the
## `if (!is.null(.it))` guard turned a missing input into a missing output with no
## error. They only appeared when a stale file from an earlier run happened to
## survive, which is worse than absent: the registry would then certify numbers
## from a DIFFERENT run than the one it was emitted by. Same definition as 14a's
## ifam_tot (a programme counts once per family if it offers any member).
for (.g in names(PAL_INSTR)) {
  .vars <- INSTRUMENTS$var[INSTRUMENTS$family == .g]
  .vars <- .vars[.vars %in% names(A)]
  if (!length(.vars)) next
  fact(sprintf("instrument.%s", tolower(gsub("[^A-Za-z]", "", .g))),
       sum(rowSums(A[, .vars, drop = FALSE] == 1, na.rm = TRUE) > 0),
       "count", nrow(A), "A[INSTRUMENTS$var by family]")
}

## ---- Justice provisions -----------------------------------------------------
.eq <- .read_out("11_equity_design_provisions")
if (!is.null(.eq))
  for (i in seq_len(nrow(.eq))) {
    k <- tolower(gsub("[^A-Za-z]+", "_", .eq$provision[i]))
    k <- gsub("^_|_$", "", substr(k, 1, 40))
    fact(sprintf("justice.%s", k), .eq$n_documented[i], "count", nrow(A),
         "11_equity_design_provisions")
  }

## ---- Typology ---------------------------------------------------------------
.ms <- .read_out("08_macro_architecture_sizes")
if (!is.null(.ms))
  for (i in seq_len(nrow(.ms)))
    fact(sprintf("typology.%s", tolower(gsub("[^A-Za-z0-9]", "", .ms$typology_macro[i]))),
         .ms$n[i], "count", sum(.ms$n), "08_macro_architecture_sizes")
.bj <- .read_out("08_bootstrap_jaccard")
if (!is.null(.bj) && "k" %in% names(.bj)) {
  .b3 <- .bj[.bj$k == 3, ]
  if (nrow(.b3)) {
    fact("typology.jaccard_min", round(min(.b3$jaccard), 3), "ratio", source = "08_bootstrap_jaccard")
    fact("typology.jaccard_max", round(max(.b3$jaccard), 3), "ratio", source = "08_bootstrap_jaccard")
  }
}

## PER-CLASS PROVISION RATES, AS PERCENTAGES. The profile table stores these as
## proportions (0.846) while the Results quote them as percentages (84.6%), so
## the manuscript checker reported five correct numbers as unfindable. The fault
## was the registry's, not the prose's: a share a reader will quote has to exist
## here in the form it is quoted in. Registering the rounded percentage fixes
## that without touching the stored proportion the figures are drawn from.
.mp <- .read_out("08_macro_architecture_profiles")
if (!is.null(.mp) && all(c("name", "ten", "grant", "supp", "std") %in% names(.mp)))
  for (i in seq_len(nrow(.mp))) {
    .k <- tolower(gsub("[^A-Za-z0-9]", "", .mp$name[i]))
    for (.v in c("ten", "grant", "supp", "std"))
      fact(sprintf("archpct.%s.%s", .k, .v), round(100 * as.numeric(.mp[[.v]][i]), 1),
           "percent", 100, "08_macro_architecture_profiles")
  }

## EVIDENTIARY ROUTE TOTALS. Section 3 quotes the 1,330 misfit verdicts and the
## 724 of them the documents evidence directly, both of which are sums across
## the route table rather than cells in it, so neither could be checked.
.rt <- .read_out("05_misfit_route_composition")
if (!is.null(.rt) && all(c("route", "n") %in% names(.rt))) {
  fact("routes.total", sum(.rt$n), "count", NA, "05_misfit_route_composition")
  fact("routes.direct_evidence", sum(.rt$n[grepl("^\\((a|b)\\)", .rt$route)]), "count",
       sum(.rt$n), "05_misfit_route_composition")
  fact("routes.documentation_evidenced", sum(.rt$n[grepl("^\\(c\\)", .rt$route)]), "count",
       sum(.rt$n), "05_misfit_route_composition")
}

## NOT REGISTERED: the density range the Results quote, "139 to 13,500
## inhabitants per km2". This pipeline computes no density variable at all --
## the city frame carries population and land-use mix, not area -- so there is
## nothing here to register and no table the figure could be checked against.
## Inventing a fact to silence the checker would defeat the checker. The number
## has to be sourced in the manuscript or dropped from it.

## ---- Reliability ------------------------------------------------------------
## Reliability comes from the parser/coder concordance frame (`agree`).
if (exists("agree") && is.data.frame(agree)) {
  fact("reliability.parser_value_agreement",  round(mean(agree$pct_value_agreement), 1), "percent",
       source = "13_parser_provenance_agreement$pct_value_agreement")
  fact("reliability.parser_concordant",       round(mean(agree$pct_concordant), 1), "percent",
       source = "13_parser_provenance_agreement$pct_concordant")
  fact("reliability.parser_n_variables",      nrow(agree), "count",
       source = "13_parser_provenance_agreement")
}


## ---- Reliability: the DISTINCTION that a mislabel already cost us ----------
## Module 13 measures the PARSER against the coders' confidence grades. It is a
## validation of the normalisation layer. It is NOT inter-rater reliability, and
## a draft of the Results reported it as "blind double-coding" -- real numbers,
## wrong construct. The two blind studies live outside the R pipeline (they were
## run during coding, against sources and against five named coders), so they are
## registered here as constants WITH their provenance, and keyed so that no
## document can quote one while meaning the other.
fact("reliability.blind_vs_source.pct",    90.0,  "percent", source = "IRR_v138_REPORT.md - 67 fresh blind cases, 15 core binary vars")
fact("reliability.blind_vs_source.kappa",  0.727, "kappa",   source = "IRR_v138_REPORT.md")
fact("reliability.blind_vs_source.n",      1005,  "count",   source = "IRR_v138_REPORT.md")
fact("reliability.blind_vs_coders.pct",    89.3,  "percent", source = "IRR_v138_REPORT.md - 5 named coders, 137 cases, 17 vars")
fact("reliability.blind_vs_coders.kappa",  0.702, "kappa",   source = "IRR_v138_REPORT.md")
fact("reliability.blind_vs_coders.n",      2103,  "count",   source = "IRR_v138_REPORT.md")

## ---- v43: numbers added to the Results in the co-author merge ---------------
## Each is registered here for the same reason as every other fact: a number that
## appears in the manuscript but in no table is a number nobody can check.
## Computed from A, NOT from 14_a3_instruments_hierarchy: this module runs BEFORE
## the figure modules (run_all.R order), so any .read_out() of a 14_* table here
## either returns the PREVIOUS run's file or NULL. Reading A is the only way these
## two facts are guaranteed to describe the run that emits them.
fact("instrument.permitfasttrack", sum(A$r_fasttrack == 1, na.rm = TRUE),
     "count", nrow(A), "A$r_fasttrack")
fact("instrument.permitfeewaiver", sum(A$r_feewaiver == 1, na.rm = TRUE),
     "count", nrow(A), "A$r_feewaiver")
## The DENOMINATOR is registered alongside the median, because the first draft of
## the Results sentence said "two of the seven harmonised measure types" -- seven
## being the abstract's framing, not the pipeline's taxonomy. n_measures is built
## from MEASURE_VARS and the harmonised prevalence table enumerates the same set,
## so the count is computed here and the sentence quotes it.
fact("measures.median_per_programme", stats::median(A$n_measures, na.rm = TRUE),
     "count", source = "A$n_measures")
## PARSED FROM THE SOURCE that builds n_measures, not typed. The taxonomy is
## deliberately layered: n_measures sums a TEN-measure primary set, while
## n_measures_sec holds five secondary measures (turf replacement, greywater,
## rainwater harvesting, compost, soil) that enter applicability but not the
## primary count -- which is why 14_measure_prevalence.csv shows eleven rows.
## Quoting the prevalence row count as the median's denominator would therefore
## mix the two layers, and quoting the abstract's "seven" (the first draft of
## this sentence) matches neither.
## READ THE CANONICAL SET, DO NOT RE-DERIVE IT.
## This block used to SCRAPE the column list out of the text of
## 03_architecture.R with a regex over the source file. That is exactly the
## independent enumeration R5 forbids: it broke the moment n_measures stopped
## being a literal cbind(), and while it worked it silently encoded the TEN-
## measure set that finding 26 identified as wrong. There is now one canonical
## vector, MEASURES$var in 00b_design.R, and every consumer reads it.
.mvars <- MEASURES$var
stopifnot("canonical measure set is empty"        = length(.mvars) > 1,
          "canonical measure vars are not m_*"    = all(grepl("^m_", .mvars)),
          "canonical measure vars absent from A"  = all(.mvars %in% names(A)))
fact("measures.n_primary_types", length(.mvars), "count",
     source = "MEASURES$var in R/00b_design.R -- the canonical measure set (R5)")

## Rare recognition provisions are NATIONALLY CONCENTRATED, not thinly spread.
## The corpus rate alone cannot distinguish those two, so the country share and
## the within-country rate are both registered.
## 11_reporting.R is module 14, so this table DOES exist by the time module 18
## runs -- unlike the 14* figure tables above. Guarded anyway.
.rc <- .read_out("11_rare_provision_concentration")
stopifnot("11_rare_provision_concentration missing at registry time" = !is.null(.rc))
for (.p in list(c("Social housing eligible", "socho", "Netherlands"),
                c("Informal groups eligible or acknowledged", "inform", "Netherlands"))) {
  .s <- .rc[.rc$provision == .p[1], ]
  .row <- .s[.s$country == .p[3], ]
  if (nrow(.row) == 1) {
    fact(sprintf("recognition.%s.total", .p[2]), .row$n_corpus_total[1], "count", nrow(A),
         "11_rare_provision_concentration")
    fact(sprintf("recognition.%s.nl_n", .p[2]), .row$n_documenting[1], "count",
         .row$n_country_corpus[1], "11_rare_provision_concentration")
  }
}

## ---- v45: the blind-audit re-test, binary and categorical separately --------
## 20_reliability_retest.R runs at position 18 (before this module), so these are
## safe to read back -- unlike the 14* figure tables, which do not yet exist here.
.rt <- .read_out("19_retest_by_stratum")
if (!is.null(.rt)) for (.i in seq_len(nrow(.rt))) {
  .s <- .rt$stratum[.i]
  fact(sprintf("retest.%s.pairs", .s),       .rt$n_pairs[.i],   "count",  NA, "19_retest_by_stratum")
  fact(sprintf("retest.%s.variables", .s),   .rt$n_variables[.i], "count", NA, "19_retest_by_stratum")
  fact(sprintf("retest.%s.agree_pct", .s),   .rt$agree_pct[.i], "percent", .rt$n_pairs[.i], "19_retest_by_stratum")
  if (!is.na(.rt$kappa[.i]))
    fact(sprintf("retest.%s.kappa", .s),     .rt$kappa[.i],     "kappa",  NA, "19_retest_by_stratum")
  fact(sprintf("retest.%s.both_committed_n", .s),   .rt$n_both_committed[.i], "count", NA, "19_retest_by_stratum")
  fact(sprintf("retest.%s.both_agree_pct", .s),     .rt$both_agree_pct[.i], "percent", .rt$n_both_committed[.i], "19_retest_by_stratum")
  if (!is.na(.rt$both_kappa[.i]))
    fact(sprintf("retest.%s.both_kappa", .s),       .rt$both_kappa[.i], "kappa", NA, "19_retest_by_stratum")
  fact(sprintf("retest.%s.true_contradictions", .s), .rt$true_contradictions[.i], "count",
       .rt$n_pairs[.i], "19_retest_by_stratum")
}
.rtp <- .read_out("19_retest_pairs")
if (!is.null(.rtp)) {
  fact("retest.cases", length(unique(.rtp$case)), "count", nrow(A), "19_retest_pairs")
  fact("retest.pairs_total", nrow(.rtp), "count", NA, "19_retest_pairs")
}

## ---- Emit -------------------------------------------------------------------
FACTS <- do.call(rbind, .facts$rows)
FACTS <- FACTS[order(FACTS$key), ]
save_tab(FACTS, "18_quotable_facts")

msg("Fact registry: %d quotable numbers emitted to 18_quotable_facts.csv", nrow(FACTS))
msg("  A number may appear in a manuscript ONLY if it appears in this table.")
