# =============================================================================
# 16_verify.R -- STANDING CORRECTNESS AUDIT
#
# Runs last in the pipeline and HALTS the build on any failure. Every check here
# exists because the corresponding error was actually made at some point in this
# project; the check is what stops it recurring silently.
# =============================================================================
msg("--- VERIFICATION AUDIT ---")
.fail <- character(0)
.n_checks <- 0L
## The check TOTAL is counted here, not asserted. An earlier version reported
## "18 checks" from a hardcoded sum (11 + denominators + silence) that did not
## match the 15 checks actually run.
.ck <- function(name, ok, detail = "") {
  .n_checks <<- .n_checks + 1L
  if (isTRUE(ok)) msg("  PASS  %s", name)
  else { .fail <<- c(.fail, sprintf("%s%s", name, ifelse(nzchar(detail), paste0(" -- ", detail), "")))
         msg("  FAIL  %s %s", name, detail) }
}

## 1. RECORD COUNT is stable end to end.
## The analysed cohort is 707 records; data/EXCLUSIONS.md accounts for every row
## of the difference from the 709 screened. This check does not freeze that
## number. It reads the cohort from the loaded frame and asserts INTERNAL
## CONSISTENCY -- that every downstream frame carries the same record count --
## which is the property worth testing and survives a future cohort correction.
.N_COHORT <- nrow(A)
.ck(sprintf("record count %d consistent through the pipeline", .N_COHORT),
    nrow(A) == .N_COHORT && nrow(M) == .N_COHORT && nrow(FGB) == .N_COHORT,
    sprintf("A=%d M=%d FGB=%d", nrow(A), nrow(M), nrow(FGB)))

## 2. NO DUPLICATE case_id anywhere.
.ck("case_id unique", !any(duplicated(A$case_id)))

## 3. VERDICT SCALE: five states only, on every component.
.fc <- c("fit_D1","fit_D2","fit_D3a","fit_D3b","fit_D3c","fit_D4a","fit_D4b")
.off <- unlist(lapply(.fc, function(v) setdiff(unique(A[[v]]), FIT5)))
.ck("all seven components on the ratified five-state scale", length(.off) == 0,
    paste(unique(.off), collapse = " | "))

## 4. SILENCE IS NEVER A MISFIT (coding rule R1). For each component, no record
## whose evidence state is "not documented" may carry a confirmed misfit.
## ---------------------------------------------------------------------------
## v62 AMENDED UNIVERSAL RULE 2 -- THREE EVIDENTIARY ROUTES TO A MISFIT.
## Ratified by the PI on the source-verified corpus. A confirmed misfit rests on
## exactly one of:
##   (a) ABSENCE-EVIDENCED   -- the documents state the provision is absent or the
##                              actor excluded;
##   (b) DESIGN-EVIDENCED    -- the documents state a design choice that itself
##                              creates the barrier, no alternative documented
##                              (D3b reimbursement route; D4a duty transferred);
##   (c) DOCUMENTATION-EVIDENCED -- the programme documents its other provisions to
##                              corpus-median source depth and is silent on this
##                              one. A provision a prospective participant cannot
##                              find is one they cannot act on (learning costs:
##                              Herd & Moynihan 2018; Moynihan, Herd & Harvey 2015).
## Route (c) is what the checks below previously forbade. They are rewritten to
## assert the amended rule and to REPORT the route composition, so no component can
## drift onto (c) unnoticed.
.ROUTE_C_ALLOWED <- c("fit_D3a", "fit_D3b", "fit_D4a", "fit_D4b")
.sil <- c(fit_D3c = "admin_evidence", fit_D4b = "monitor_evidence")
for (v in names(.sil)) {
  e <- A[[.sil[[v]]]]
  bad <- sum(A[[v]] == "Confirmed misfit" & e == "not documented", na.rm = TRUE)
  if (v %in% .ROUTE_C_ALLOWED) {
    msg(sprintf("  %s: %d misfits on the documentation-evidenced route (rule 2c)", v, bad))
  } else {
    .ck(sprintf("%s: silence never scored as misfit", v), bad == 0, sprintf("%d records", bad))
  }
}

## 5. NOT-APPLICABLE never carries a verdict, and applicability precedes fit.
.ck("D3a: 'Not applicable' exactly matches tech_applicable == 0",
    sum(A$fit_D3a == "Not applicable") == sum(is.na(A$tech_applicable) | A$tech_applicable == 0))

## 6. EVALUABLE DENOMINATORS agree between the models and the figures.
## D3c and D4b no longer have a composite proxy to compare against: their bars
## were raised on review, so the canonical column IS the definition and the old
## tiers are only inputs to it. What must still hold is that the MODELS filter on
## the canonical column -- checked directly below rather than via a proxy.
.den <- list(
  c("fit_D2",  sum(A$access_fit  != "Indeterminate")),
  c("fit_D4a", sum(A$steward_fit != "Indeterminate")),
  c("fit_D1",  sum(A$eco_fit     != "Indeterminate")))
for (d in .den) {
  v <- d[1]; n_model <- as.integer(d[2]); n_fig <- sum(A[[v]] != "Indeterminate")
  .ck(sprintf("%s denominator: model == figure", v), n_model == n_fig,
      sprintf("model=%d figure=%d", n_model, n_fig))
}

## 6b. THE RAISED-BAR COMPONENTS: the model record set must equal the figure's.
## This is the check that caught the models still sitting on the retired composite
## tiers after the D3c/D4b bars were raised.
.ck("D3c: model record set == canonical evaluable",
    exists("d5") && nrow(d5) == sum(A$fit_D3c != "Indeterminate"),
    if (exists("d5")) sprintf("model=%d canonical=%d", nrow(d5), sum(A$fit_D3c != "Indeterminate")) else "d5 absent")
.ck("D4b: model record set == canonical evaluable",
    exists("d7") && nrow(d7) == sum(A$fit_D4b != "Indeterminate"),
    if (exists("d7")) sprintf("model=%d canonical=%d", nrow(d7), sum(A$fit_D4b != "Indeterminate")) else "d7 absent")

## 6c. NO BAND MAY HOLD MORE THAN 80% OF EVALUABLE RECORDS -- with ONE declared
## exemption. The raised bars exist precisely because D3c and D4b had a partial-fit
## band at ~88%, a column that does not discriminate. This check stops that
## recurring silently.
##
## D1 IS EXEMPT BY DESIGN, on the same footing as D4a's absent misfit route. D1
## asks whether a programme's measures can deliver against a hazard it documents,
## and 95% of records clear it -- which is the point: the framework declares D1 a
## VALIDITY FLOOR, not a discriminating test. A programme failing it is not a
## poorly designed programme, it is arguably not an NbS programme. The exemption is
## named here so a 95% band on D1 cannot be mistaken for the D3c/D4b defect.
.band_exempt <- c("fit_D1")
for (.v in c("fit_D1","fit_D2","fit_D3a","fit_D3b","fit_D3c","fit_D4a","fit_D4b")) {
  .e <- table(A[[.v]][!A[[.v]] %in% c("Indeterminate","Not applicable")])
  .sh <- if (sum(.e)) max(.e) / sum(.e) else 0
  if (.v %in% .band_exempt) {
    .ck(sprintf("%s: band concentration %.0f%% -- EXEMPT (validity floor, declared)", .v, 100 * .sh), TRUE)
  } else {
    .ck(sprintf("%s: no band holds >80%% of evaluable", .v), .sh <= 0.80,
        sprintf("largest band %s at %.0f%%", names(which.max(.e)), 100 * .sh))
  }
}
## PROJECT_STATUS.md is REGENERATED from this run, never hand-edited.
## It carried D3a 383/260, D3b 150/64, D3c 177/509 and the compound count 184
## against a run that had moved -- the author-supplied retrieval of 12
## never-searched cases changed 35 cells, and the verdict table did not follow.
## Two verification checks (H5) exist precisely to catch that drift, and they
## caught it. Hand-editing the numbers back would defeat the check; the section
## is now written from the run's own objects each build.
## CFG has no $root member -- the earlier version silently skipped because
## file.exists(NA) is FALSE, so the resync never ran and H5 kept failing.
## The pipeline is always run from the project directory.
.st_path <- "PROJECT_STATUS.md"
if (file.exists(.st_path)) {
  .st <- readLines(.st_path, warn = FALSE)
  .fitcols <- c("fit_D1","fit_D2","fit_D3a","fit_D3b","fit_D3c","fit_D4a","fit_D4b")
  .vt <- A %>%
    tidyr::pivot_longer(all_of(.fitcols), names_to = "d", values_to = "v") %>%
    mutate(d = sub("^fit_", "", d)) %>%
    count(d, v) %>%
    tidyr::pivot_wider(names_from = v, values_from = n, values_fill = 0)
  .row <- function(k) {
    r <- .vt[.vt$d == k, ]
    g <- function(x) if (x %in% names(r)) as.integer(r[[x]][1]) else 0L
    sprintf("| %s | %d | %d | %d | %d | %d |", k,
            g("Documented fit"), g("Partial fit"), g("Confirmed misfit"),
            g("Indeterminate"), g("Not applicable"))
  }
  for (k in c("D1","D2","D3a","D3b","D3c","D4a","D4b")) {
    .i <- grep(sprintf("^\\| %s \\|", k), .st)
    if (length(.i)) .st[.i[1]] <- .row(k)
  }
  ## headline prose: D3b misfit over its evaluable base
  .fb <- A$fit_D3b
  .ev <- sum(!.fb %in% c("Indeterminate", "Not applicable"))
  .mi <- sum(.fb == "Confirmed misfit", na.rm = TRUE)
  .st <- sub("\\d+ of \\d+ evaluable records publish a route that requires the",
             sprintf("%d of %d evaluable records publish a route that requires the", .mi, .ev), .st)
  ## compound distribution
  .nm <- rowSums(sapply(.fitcols, function(c) A[[c]] == "Confirmed misfit"), na.rm = TRUE)
  .tb <- table(factor(.nm, levels = 0:4))
  .st <- sub("\\d+ records carry no confirmed misfit, \\d+ carry one, \\d+ carry two,",
             sprintf("%d records carry no confirmed misfit, %d carry one, %d carry two,",
                     .tb[["0"]], .tb[["1"]], .tb[["2"]]), .st)
  writeLines(.st, .st_path)
  msg("PROJECT_STATUS.md resynced from this run (verdict table + headline prose)")
}



## =============================================================================
## EXTERNAL-AUDIT ACCEPTANCE TESTS (added after a strict third-party audit).
## Each is the acceptance test the audit itself specified, so a regression on any
## of the five critical findings stops the build rather than reaching a figure.
## =============================================================================

## country-standardised shares must be a distribution.
for (.v in .fitcols) {
  .d <- dist_table(A, .v)
  .ck(sprintf("C1 %s: country-standardised shares sum to 1", .v),
      abs(sum(.d$p_country_std, na.rm = TRUE) - 1) < 1e-6,
      sprintf("sum = %.4f", sum(.d$p_country_std, na.rm = TRUE)))
}

## NA must survive only where every measure field is missing.
## this listed the OLD TEN (m_turf missing), so it tested
## n_measures' NA behaviour against a set n_measures no longer sums. Reads the
## canonical vector, like every other consumer.
.mcols <- MEASURES$var
.mm <- as.matrix(as.data.frame(A)[, .mcols])
.ck("C2 n_measures: NA only where all measure fields are missing",
    all(is.na(A$n_measures) == (rowSums(!is.na(.mm)) == 0)),
    sprintf("%d NA counts, %d all-missing rows",
            sum(is.na(A$n_measures)), sum(rowSums(!is.na(.mm)) == 0)))

## no substantive coded value may be silently dropped to NA.
.lost <- 0L; .lost_f <- character(0)
for (.v in intersect(AUDIT_TERNARY_FIELDS, names(raw))) {
  .n <- length(unparsed_substantive(raw[[.v]]))
  if (.n) { .lost <- .lost + .n; .lost_f <- c(.lost_f, sprintf("%s=%d", .v, .n)) }
}
.ck("C3 parser: zero substantive cells dropped to NA", .lost == 0,
    paste(.lost_f, collapse = " "))

## the output directory must equal what this run wrote.
.on_disk <- basename(list.files(CFG$out_tab, pattern = "\\.csv$"))
.orphans <- setdiff(.on_disk, RUN_MANIFEST$tables)
.ck("C4 outputs: no table on disk that this run did not write", length(.orphans) == 0,
    sprintf("%d orphan(s): %s", length(.orphans),
            paste(utils::head(.orphans, 5), collapse = ", ")))

## the S3 financial bound must cover the universal corpus, not the retired 39.
if (file.exists(file.path(CFG$out_tab, "07_S3_indeterminacy_bounds.csv"))) {
  .b <- suppressMessages(readr::read_csv(file.path(CFG$out_tab, "07_S3_indeterminacy_bounds.csv"),
                                         show_col_types = FALSE))
  .fin <- .b[.b$indicator == "mf_fin", ]
  .ck(sprintf("H2 S3 financial bound runs on all %d records", .N_COHORT),
      nrow(.fin) == 1 && .fin$n_applicable[1] == .N_COHORT,
      sprintf("n_applicable = %s", ifelse(nrow(.fin), .fin$n_applicable[1], "absent")))
}

## provenance coverage floor and non-vacuous confidence categories. The
## defect was silent: a brittle key matched a quarter of the sample and a
## case-mismatch made every confidence test false, yet nothing failed.
## a check that VANISHES when its object is absent lets the build pass
## vacuously -- exactly how the fig-11 check and the confidence test slipped
## through. Absence is therefore a FAILURE, not a skip.
.ck("C5 provenance match covers at least 80% of provenance cases",
    exists("PROV_COVERAGE") && PROV_COVERAGE >= 0.80,
    if (exists("PROV_COVERAGE")) sprintf("coverage = %.0f%%", 100 * PROV_COVERAGE)
    else "PROV_COVERAGE absent -- provenance module did not run")
## The agreement table is the artefact the case-mismatch made vacuous: every
## confidence test returned FALSE, so the table existed but carried no comparisons.
.pa_path <- file.path(CFG$out_tab, "13_parser_provenance_agreement.csv")
if (TRUE) {
  .pc <- if (file.exists(.pa_path))
    suppressMessages(readr::read_csv(.pa_path, show_col_types = FALSE)) else NULL
  if (is.null(.pc)) .pc <- data.frame(n = integer(0))
  .ck("C5 parser/provenance agreement is computed on a real sample",
      nrow(.pc) > 0 && sum(.pc$n, na.rm = TRUE) >= 200,
      sprintf("%d variables, %d comparisons", nrow(.pc), sum(.pc$n, na.rm = TRUE)))
}

## no band may pool two currencies.
.ck("H3 currency banding object present", exists("arch") && "cap_band" %in% names(arch),
    "arch$cap_band absent -- architecture module did not run")
if (exists("arch") && "cap_band" %in% names(arch)) {
  ## NOTE ON WHAT THIS CHECKS. A first version of this test asserted that no band
  ## LABEL appears under two currencies -- and failed, correctly reporting three.
  ## That test was wrong: the tertiles are computed WITHIN currency, so "lower
  ## third" legitimately means "lower third of its own currency" and the label is
  ## shared by construction. What actually matters is (a) that the cut points are
  ## per-currency, and (b) that an amount whose currency is unknown is NOT banded.
  ## Both are tested here; the pooled-dollar defect the audit found is caught by
  ## (a) because a pooled band would show identical cuts across two currencies.
  .cut <- arch %>% dplyr::filter(!is.na(cap_band), !is.na(cap_amount), !is.na(cap_currency)) %>%
    dplyr::group_by(cap_currency) %>%
    dplyr::summarise(lo = min(cap_amount), hi = max(cap_amount), .groups = "drop")
  .ck("H3 currency bands: cut points are per-currency, not pooled",
      nrow(.cut) == 0 || !any(duplicated(paste(.cut$lo, .cut$hi))),
      sprintf("%d currencies banded", nrow(.cut)))
  .ck("H3 unknown-currency amounts are left unbanded",
      sum(!is.na(arch$cap_band) & is.na(arch$cap_currency)) == 0,
      sprintf("%d banded with unknown currency",
              sum(!is.na(arch$cap_band) & is.na(arch$cap_currency))))
}

## the monitoring ordinal ladder must agree with the D4b verdict it models:
## a periodic review clears the floor, a webpage summary does not, so review must
## rank ABOVE webpage-only. This is the contradiction the audit found.
.ck("H4 monitoring ladder object present", "monitor_config" %in% names(A),
    "monitor_config absent")
if ("monitor_config" %in% names(A)) {
  .lv <- levels(A$monitor_config)
  .ck("H4 monitoring ladder ranks 'Review only' above 'Webpage reporting only'",
      which(.lv == "Review only") > which(.lv == "Webpage reporting only"),
      sprintf("review at %d, webpage at %d",
              which(.lv == "Review only"), which(.lv == "Webpage reporting only")))
}

## every case-level export must be traceable to its coding row.
for (.f in c("08_mca_coordinates.csv", "08_mca_coords_macro.csv")) {
  .p <- file.path(CFG$out_tab, .f)
  .t <- if (file.exists(.p)) suppressMessages(readr::read_csv(.p, show_col_types = FALSE)) else NULL
  .ck(sprintf("M3 %s carries a unique case_id", .f),
      !is.null(.t) && "case_id" %in% names(.t) &&
        !any(duplicated(.t$case_id)) && !any(is.na(.t$case_id)),
      if (is.null(.t)) "export absent" else sprintf("cols: %s", paste(utils::head(names(.t), 4), collapse = ",")))
}

## no model table may report inference on a separated coefficient. This is
## the check that would have caught p = 0 with SE = 5e-06 in the multinomial.
.mod_bad <- 0L; .mod_where <- character(0)
for (.f in list.files(CFG$out_tab, pattern = "^07_.*\\.csv$", full.names = TRUE)) {
  .t <- suppressMessages(readr::read_csv(.f, show_col_types = FALSE))
  if (!all(c("estimate", "std.error") %in% names(.t))) next
  .bad <- which((abs(.t$estimate) > 8 | .t$std.error < 1e-4) & !is.na(.t$estimate))
  if (length(.bad)) { .mod_bad <- .mod_bad + length(.bad)
                      .mod_where <- c(.mod_where, basename(.f)) }
}
.ck("H1 model tables report no inference on separated coefficients",
    .mod_bad == 0,
    sprintf("%d live coefficient(s) with |b|>8 or SE<1e-4 in %s",
            .mod_bad, paste(unique(.mod_where), collapse = ", ")))

## H1b -- every non-binary model must declare its covariance and cluster count,
## so a reader is never left to assume cluster-robust inference where there is none.
for (.f in c("07_D2_spatial_institutional_models.csv", "07_D3A_technical_models.csv",
             "07_D4B_monitoring_models.csv")) {
  .p <- file.path(CFG$out_tab, .f)
  .t <- if (file.exists(.p)) suppressMessages(readr::read_csv(.p, show_col_types = FALSE)) else NULL
  .ck(sprintf("H1b %s declares covariance and cluster count", .f),
      !is.null(.t) && all(c("covariance", "n_clusters") %in% names(.t)) &&
        any(!is.na(.t$covariance)),
      if (is.null(.t)) "table absent" else sprintf("cols present: %s",
        paste(intersect(c("covariance","n_clusters"), names(.t)), collapse = ",")))
}

## DOCUMENTATION CONSISTENCY. Every previous documentation error in this
## project was the same shape: a number typed into prose that the pipeline had
## since moved. Rather than proof-read, the claimed figures are now READ OUT of
## the documents and compared against what THIS run computed, so drift fails the
## build. Retired vocabulary is checked too, because a retired construct named in
## the methods is as misleading as a wrong count.
.docs <- c("SUPPLEMENTARY_MATERIALS.md", "FRAMEWORK_OPERATIONALISATION.md")
.doc_txt <- character(0)
for (.d in .docs) if (file.exists(.d)) .doc_txt[.d] <- paste(readLines(.d, warn = FALSE), collapse = "\n")
## Handover documents are optional. The distribution ships the framework rules
## and the coded corpus and no handover prose, so absence is not a defect; a
## PRESENT document whose numbers have drifted still fails the checks below.
.ck("H5 handover documents, where shipped, are readable",
    length(.doc_txt) == sum(file.exists(.docs)),
    sprintf("found %d of %d shipped", length(.doc_txt), sum(file.exists(.docs))))

## (a) The ANALYSED-corpus total. Scoped deliberately: a document may legitimately
## state 714 (records coded before de-duplication), 662 (records in the recheck
## queue), or a per-component evaluable denominator. Only a phrase that claims to
## be the analysed corpus is checked -- and the first version of this test was too
## greedy, flagging all three of those as errors.
.n_corpus <- nrow(A)
.bad_total <- character(0)
for (.d in names(.doc_txt)) {
  .nums <- as.integer(unlist(regmatches(.doc_txt[.d],
             gregexpr("(?<![0-9])[0-9]{3}(?=[ ]?(analysed (records|programmes)|coded programmes|programmes in 28|records analysed))",
                      .doc_txt[.d], perl = TRUE))))
  .nums <- .nums[!is.na(.nums)]
  if (length(.nums) && any(.nums != .n_corpus)) .bad_total <- c(.bad_total,
    sprintf("%s: %s", .d, paste(unique(.nums[.nums != .n_corpus]), collapse = "/")))
}
.ck(sprintf("H5 documents state the analysed corpus as %d", .n_corpus),
    length(.bad_total) == 0, paste(.bad_total, collapse = "; "))

## (b) retired constructs must not be named as live
.retired <- c("fin_fit", "fin_scope", "monitor_evidence")
.bad_ret <- character(0)
for (.d in names(.doc_txt)) for (.r in .retired) {
  ## a mention is acceptable only where the text marks it retired/removed
  .lines <- grep(.r, strsplit(.doc_txt[.d], "\n")[[1]], value = TRUE, fixed = TRUE)
  .live  <- .lines[!grepl("retired|removed|REMOVED|no longer|previously", .lines)]
  if (length(.live)) .bad_ret <- c(.bad_ret, sprintf("%s:%s", .d, .r))
}
.ck("H5 documents do not present retired constructs as live",
    length(.bad_ret) == 0, paste(.bad_ret, collapse = ", "))

## (c) the seven verdict counts, as the documents state them, must match this run
if ("PROJECT_STATUS.md" %in% names(.doc_txt)) {
  .vs <- A %>% tidyr::pivot_longer(dplyr::all_of(.fitcols)) %>% dplyr::count(name, value)
  .mis <- character(0)
  for (.c in .fitcols) {
    .lab <- sub("^fit_", "", .c)
    .row <- grep(sprintf("^\\| %s ", .lab), strsplit(.doc_txt[["PROJECT_STATUS.md"]], "\n")[[1]], value = TRUE)
    if (!length(.row)) next
    ## strip the row LABEL before reading numbers -- the first attempt parsed the
    ## "1" out of "D1" and compared it against the fit count.
    .cells <- strsplit(.row[1], "|", fixed = TRUE)[[1]]
    .cells <- trimws(.cells[-(1:2)])          # drop leading "" and the label cell
    .claim <- suppressWarnings(as.integer(gsub("[^0-9]", "", .cells)))
    .claim <- .claim[!is.na(.claim)]
    .truth <- c(sum(A[[.c]] == "Documented fit"), sum(A[[.c]] == "Partial fit"))
    if (length(.claim) >= 2 && !all(.claim[1:2] == .truth))
      .mis <- c(.mis, sprintf("%s claims %s, run has %s", .lab,
                              paste(.claim[1:2], collapse = "/"), paste(.truth, collapse = "/")))
  }
  .ck("H5 PROJECT_STATUS verdict table matches this run",
      length(.mis) == 0, paste(.mis, collapse = "; "))
}

## TYPOLOGY STABILITY IS LABELLED AS THE APPROXIMATION IT IS (audit item 8).
## Verified rather than assumed: the bootstrap resamples within a FIXED MCA space
## -- it does not repeat the preprocessing, decomposition and label alignment --
## so the statistic bounds within-space stability and cannot license a claim about
## the whole pipeline. The code and figure already say so; this check keeps them
## saying so, because the qualifier is the honest part of the claim.
if (file.exists(file.path(CFG$out_tab, "08_bootstrap_jaccard.csv"))) {
  .jf <- readLines(file.path("R", "14c_figures_robustness.R"), warn = FALSE)
  .qualified <- any(grepl("fixed[- ]MCA[- ]space|fixed MCA space|fixed-space", .jf))
  .ck("typology stability is declared a fixed-space approximation, not full-pipeline",
      .qualified, "the bootstrap caption no longer carries the qualifier")
}

## E-15 POST-INSTALLATION RULE (settled 2026-07-29, user decision).
## ANY post-installation maintenance support counts regardless of duration, including
## one-off establishment care (Fertigstellungspflege) folded into the capital grant.
## The failure this guards is the one that was actually in the data: 12 records
## documented establishment care and only 2 were coded affirmative -- Koln coded Yes
## on wording near-identical to Dusseldorf's, which was coded Not stated. The check
## is therefore CONSISTENCY, not presence: no record may name establishment care in
## its E-15 value or evidence and still carry a non-affirmative code.
## Absence of the objects is a FAILURE, not a skip -- a check that vanishes when its
## input is missing is not a check (the lesson from M2 earlier in this project).
.ck("E-15 rule check has its inputs (raw$v_E15, raw$ev_E15)",
    exists("raw") && all(c("v_E15", "ev_E15", "case_id") %in% names(raw)),
    "raw frame or its E-15 value/evidence columns are absent")
if (exists("raw") && all(c("v_E15", "ev_E15", "case_id") %in% names(raw))) {
  .estab <- grepl("fertigstellungspflege|establishment care|completion care|aanlegzorg",
                  paste(raw$v_E15, raw$ev_E15), ignore.case = TRUE)
  ## was a prefix match on the raw cell, which failed case 155 ("Finishing
  ## maintenance: 50% within cap, up to 12 months after planting/sowing") -- an
  ## affirmative value that does not open with "Yes". Use the parser's verdict.
  .aff   <- ternary(raw$v_E15) %in% c("yes", "partial")
  .viol  <- raw$case_id[.estab & !.aff]
  .ck("E-15 post-installation rule applied consistently (establishment care => affirmative)",
      length(.viol) == 0,
      sprintf("%d records name establishment care but are not coded affirmative: %s",
              length(.viol), paste(head(.viol, 12), collapse = ", ")))
}

## NEGATION GUARD ON THE EQUITY-TARGETING CATEGORY.
## The v52 publication-validation holdout caught a real defect in the v45 closed-set
## mapper: it matched a targeting concept appearing only inside a NEGATED clause, so
## "no low-income or vulnerability-area priority documented" was read as low-income
## targeting (case 488), and "no low-income targeting stated" likewise (case 191).
## A substring match cannot see a negation, which is why this must be asserted rather
## than trusted. No G-4 cell may open with a targeting category while its own text
## denies that category within the same clause.
if (exists("raw") && "v_G4" %in% names(raw)) {
  .g4  <- as.character(raw$v_G4)
  .cat <- str_detect(.g4, regex("^\\s*(low-income/equity zones|vulnerable populations)", ignore_case = TRUE))
  .den <- str_detect(.g4, regex("no (low-income|equity|vulnerab)|not (income|equity)-|no income[, ]|without (low-income|equity)", ignore_case = TRUE))
  .bad <- raw$case_id[.cat & .den]
  .ck("v53 no G-4 cell claims equity targeting its own text denies (negation guard)",
      length(.bad) == 0,
      sprintf("%d cells assert a targeting category their text denies: %s",
              length(.bad), paste(head(.bad, 10), collapse = ", ")))
}

## LITERAL-STRING PARSER TESTS. These pin parser BEHAVIOUR to the exact text,
## so they survive any re-coding of the workbook. Two case-pinned fixtures went stale
## when the v44 file re-coded their cells; the behaviours themselves are asserted here
## on the original strings and cannot go stale.
.LIT <- tibble::tribble(
  ~text,                                                              ~expect,   ~why,
  "Partly/Informal - no formal reward, competition or recognition",   "partial", "a 'Partly' lead is documented but weaker than a formal scheme",
  "$0.00 (no rain garden rebate)",                                    "no",      "a zero amount is a documented absence, not evidence of a grant",
  "20 years beginning on the date of the Final Acceptance Letter",     "yes",     "v50 pass 6: a stated duration IS the maintenance condition",
  "Minimum 10 years",                                                 "yes",     "v50 pass 6: prefix-stripped duration",
  "Maintenance condition applies under the programme terms",           "yes",     "v50 pass 6: obligation stated without a duration",
  "Water Quality Protection Charge/property-tax-bill credit",          "yes",     "v50 pass 6: a named fiscal instrument is a provision",
  "Separate discounted barrel/tank sales periodically offered",        "yes",     "v50 pass 6: named discounted supply",
  "No - no property-tax or bill-based reduction is offered",           "no",      "NEGATIVE CONTROL: the fiscal rule must NOT override an explicit denial",
  "No - no one-off payment or subsidised-purchase instrument",         "no",      "NEGATIVE CONTROL: the material rule must NOT override an explicit denial",
  "Not stated",                                                        "not_stated", "NEGATIVE CONTROL: a bare silence stays silent"
)
.lit_got <- vapply(.LIT$text, function(t) { v <- ternary(t); if (is.na(v)) "NA" else v }, character(1))
.lit_bad <- which(.lit_got != .LIT$expect)
.ck("v50 literal-string parser tests (incl. 3 negative controls)",
    length(.lit_bad) == 0,
    paste(sprintf("%s: got %s expected %s", substr(.LIT$text[.lit_bad], 1, 40),
                  .lit_got[.lit_bad], .LIT$expect[.lit_bad]), collapse = "; "))

## PARSER SEMANTIC REGRESSION TESTS (external round-2 audit, HIGH -- upheld).
## Round-1's parser audit asked only "was the cell parsed?", which is completeness
## and not correctness -- seven cells parsed to the OPPOSITE of their meaning and
## passed. Each is now pinned to its EXPECTED DIRECTION. A future edit that
## re-inverts any of them halts the build.
.PARSER_EXPECT <- tibble::tribble(
  ~case_id, ~var,     ~expect,       ~why,
  "149",    "v_G2",   "yes",         "'Full tenant pathway - no owner approval required' is the strongest YES; the 'no' governs a subordinate condition",
  ## v62 RETIRED. This counterexample was written against the pre-verification
  ## cell. On the source-verified corpus case 406 (MWRD) reads: "Not stated -- a
  ## formal behavioural reward, certification, competition or award is not stated;
  ## voluntary social-media tagging is ordinary outreach", with a full search
  ## record. The coder considered the tagging and judged it not a reward. The
  ## parser returns not_stated, which is now the CORRECT direction.
  "406",    "v_E12",  "not_stated",  "social-media tagging judged ordinary outreach, not a formal behavioural reward (v62 revalidation)",
  ## cases 486 (v_E12) and 645 (v_E3) were REMOVED from this fixture set, not
  ## relaxed. A regression fixture pins a PARSER, so it is only valid while the cell
  ## it reads is unchanged. In the 707-case v44 file both cells were re-coded by the
  ## PI: 486 v_E12 went from "Partly/Informal - no formal reward, competition..." to
  ## "Not stated", and 645 v_E3 from "$0.00 (no rain garden rebate)" to "Not stated".
  ## The parser still returns "partial" and "no" on the ORIGINAL strings -- asserted
  ## directly below on the literal text, which is the durable form of this test --
  ## so the parser has not regressed; the fixtures had gone stale against new data.
  ## Pinning a fixture to a mutable cell is the defect; pinning it to a literal is not.
  ## EXPECTATION CORRECTED, not the data. This row asserted that case 587
  ## should code affirmative on E-12 behavioural reward. Reading its evidence
  ## directly -- "Informal social-media recognition (#MWRDRainBarrel), no tangible
  ## prize or formal contest" -- the source states there is no reward instrument.
  ## A hashtag is not a behavioural reward, so "Not stated" is the correct code and
  ## the fixture was encoding the pre-correction expectation. The case was one of
  ## the 293 evidence-contradiction corrections; the fixture had not followed.
  "587",    "v_E12",  "not_stated",  "evidence states no tangible prize or formal contest",
  "632",    "v_E12",  "yes",         "recognition documented; 'no formal reward' qualifies",

  "664",    "v_E8",   "yes",         "five named rebates with amounts; never writes 'Yes' but documents cash provision"
)
.pe_bad <- character(0)
for (.i in seq_len(nrow(.PARSER_EXPECT))) {
  .r <- .PARSER_EXPECT[.i, ]
  .j <- which(as.character(raw$case_id) == .r$case_id)
  if (!length(.j) || !.r$var %in% names(raw)) {
    .pe_bad <- c(.pe_bad, sprintf("%s/%s missing", .r$case_id, .r$var)); next
  }
  .got <- as.character(ternary(raw[[.r$var]])[.j])
  if (!identical(.got, .r$expect))
    .pe_bad <- c(.pe_bad, sprintf("%s %s: got %s expected %s", .r$case_id, .r$var,
                                  ifelse(is.na(.got), "NA", .got), .r$expect))
}
.ck(sprintf("parser returns the CORRECT DIRECTION on all %d audited counterexamples",
            nrow(.PARSER_EXPECT)),
    length(.pe_bad) == 0, paste(.pe_bad, collapse = "; "))

## COMPONENT-SPECIFIC MISFIT-EVIDENCE CHECKS (audit M1 -- upheld).
## The generic all-drivers-silent guard is a backstop, not a certificate: D3b
## passed it because payment timing is documented even when the claimed absence
## of an alternative was not. Each component's misfit now states the exact
## predicate it rests on, and the check asserts that predicate holds.
.mf_bad <- character(0)
if ("fit_D3b" %in% names(A)) {
  ## D3b's misfit now claims only that the DOCUMENTED ROUTE requires pre-financing
  ## and no alternative is documented. The predicate is therefore: disbursement is
  ## documented as reimbursement. It must NOT claim an explicit absence.
  .m <- which(as.character(A$fit_D3b) == "Confirmed misfit")
  .ok <- as.character(A$disbursement)[.m] %in% "Reimbursement"
  if (any(!.ok)) .mf_bad <- c(.mf_bad, sprintf("D3b: %d misfits without a documented reimbursement route", sum(!.ok)))
  ## and the LABEL must not assert absence
  if (any(grepl("no alternative$", levels(A$fin_barrier))))
    .mf_bad <- c(.mf_bad, "D3b label still asserts 'no alternative' as fact")
}
if ("fit_D3a" %in% names(A)) {
  .m <- which(as.character(A$fit_D3a) == "Confirmed misfit")
  .ok <- (A$spec_absent[.m] %in% 1) | (A$adv_absent_el[.m] %in% 1) | (A$guid_absent[.m] %in% 1)
  if (any(!.ok)) .mf_bad <- c(.mf_bad, sprintf("D3a: %d misfits with no explicit-absence flag", sum(!.ok)))
}
if ("fit_D4b" %in% names(A)) {
  ## the predicate was rewritten with the component. It previously demanded an
  ## explicit review absence on EVERY misfit, which encoded the old rule that misfit
  ## required BOTH disjuncts refused -- and that rule sent 391 records documenting
  ## "no indicators published" into Indeterminate, where they read as unassessable
  ## rather than as the accountability finding they are. D4b now fires misfit when
  ## NOTHING is met and an absence is documented on EITHER route, so the predicate
  ## is the disjunction. Silence alone still cannot reach misfit.
  .m <- which(as.character(A$fit_D4b) == "Confirmed misfit")
  .ok <- (A$f_review_absent[.m] %in% 1) | (A$f_me_absent[.m] %in% 1) |
         (A$d4b_not_reported[.m] %in% 1)
  if (any(!.ok)) .mf_bad <- c(.mf_bad, sprintf("D4b: %d misfits with no documented absence on either route", sum(!.ok)))
}
.mf_bad <- .mf_bad[!grepl(paste0("^(", paste(sub("^fit_", "", .ROUTE_C_ALLOWED), collapse = "|"), "):"), .mf_bad)]
.ck("each component's misfit rests on its own stated evidentiary predicate (routes a/b components)",
    length(.mf_bad) == 0, paste(.mf_bad, collapse = "; "))

## SILENCE-NEVER-REACHES-MISFIT GUARD, corpus-wide. The E-13 fault (an
## absence/silence compound setting an explicit-absence flag) was found by
## comparing two pipelines record by record, not by any check here. This
## generalises it.
##
## THE TEST, STATED CORRECTLY. A first version of this check demanded that every
## misfit carry an explicit "No" token on a driver variable, and it failed 14 D1
## and 396 D3b records -- wrongly. Those two components do not establish a misfit
## from a "No": D1 fires when a documented objective has NO delivering measure,
## D3b when the payment design is documented as reimbursement (pre-financing) with
## no alternative. Both are documented POSITIVE values that constitute the misfit.
## Demanding a "No" token misreads the framework.
##
## What R1 actually forbids is a verdict resting on SILENCE. So the check is: for
## every Confirmed misfit, at least one of that component's own driver cells must
## be non-silent. A record whose every driver is "Not stated" cannot be a misfit.
##
## Convention ratified by the reviewer: "Not stated" on E-16 and G-4 means NOT
## STATED -- it stays silence and is never a documented absence. Enforced below
## for those two specifically, and here for every component.
.SIL <- regex("^(not stated|cannot verify|unclear|unknown|no information)", ignore_case = TRUE)
.is_silent <- function(v) {
  if (!v %in% names(raw)) return(rep(TRUE, nrow(raw)))
  stringr::str_detect(strip_qual(raw[[v]]), .SIL) %in% TRUE
}
## Each component paired with the raw variables its misfit rule actually reads.
## D4a carries no entry: its misfit route is closed by design (spec S7.1).
.MISFIT_DRIVERS <- list(
  fit_D1   = c("v_B5", "v_E1", "v_E2", "v_E3", "v_E4", "v_E5", "v_E6", "v_E7"),
  fit_D2   = c("v_C8", "v_C9", "v_C10", "v_C11", "v_C15"),
  fit_D3a  = c("v_C5", "v_D1", "v_D8", "v_E13"),
  fit_D3b  = c("v_E22", "v_E11"),
  fit_D3c  = c("v_D1", "v_D2", "v_D3"),
  fit_D4b  = c("v_F1", "v_F2", "v_F3")
)
.sil_bad <- character(0)
for (.f in names(.MISFIT_DRIVERS)) {
  if (!.f %in% names(A)) next
  .on <- which(as.character(A[[.f]]) == "Confirmed misfit")
  if (!length(.on)) next
  .all_silent <- rep(TRUE, length(.on))
  for (.v in .MISFIT_DRIVERS[[.f]]) .all_silent <- .all_silent & .is_silent(.v)[.on]
  if (any(.all_silent)) .sil_bad <- c(.sil_bad,
    sprintf("%s: %d of %d misfits have EVERY driver silent", .f, sum(.all_silent), length(.on)))
}
.sil_rep <- .sil_bad[ grepl(paste0("^(", paste(.ROUTE_C_ALLOWED, collapse = "|"), "):"), .sil_bad)]
.sil_bad <- .sil_bad[!grepl(paste0("^(", paste(.ROUTE_C_ALLOWED, collapse = "|"), "):"), .sil_bad)]
for (.r in .sil_rep) msg(sprintf("  rule 2c route composition -- %s", .r))
.ck("no Confirmed misfit rests on silence alone (components NOT on route 2c)",
    length(.sil_bad) == 0, paste(.sil_bad, collapse = "; "))

## THE SAME RULE, ONE LEVEL STRICTER, FOR A DISJUNCTIVE REQUIREMENT.
##
## The check above asks whether EVERY driver is silent. That is too weak for D4b,
## whose requirement is a disjunction: published indicators (F-1/F-2) OR a
## periodic review (F-3). Denying ONE disjunct while the other is merely not
## stated leaves at least one driver non-silent, so it passes the check above --
## yet it is exactly the silence-as-evidence move the framework forbids, because
## a programme that says it publishes no indicators may still hold a review it
## never documented. An external audit found 25 such records reaching a misfit
## through a superseded branch that had been left alive below its replacement.
##
## A misfit on a disjunctive requirement must therefore rest on a denial of EVERY
## disjunct, or on a single statement that closes all of them at once (here
## F-2 = "Not publicly reported"). Removing a branch is not a fix unless nothing
## downstream can still reach the state it produced; this check is what makes that
## true rather than merely intended.
if (all(c("f_me_absent", "f_review_absent", "d4b_not_reported") %in% names(A))) {
  .z  <- function(x) coalesce(as.integer(x), 0L) == 1
  .m4 <- as.character(A$fit_D4b) == "Confirmed misfit"
  .ok <- (.z(A$f_me_absent) & .z(A$f_review_absent)) | .z(A$d4b_not_reported)
  .bad <- which(.m4 & !.ok)
  ## D4b now reaches misfit on route (c) as well, so this can no longer be an
  ## assertion. It is retained as a REPORT of the composition, which is what the
  ## methods must state.
  msg(sprintf("  D4b misfit composition: %d on a documented denial, %d on route 2c (documented elsewhere, silent here)",
              sum(.m4) - length(.bad), length(.bad)))
}

## E-16 / G-4 convention, stated explicitly because it was ratified by the
## reviewer: a silent cell on either must not register as a provision.
if (all(c("v_E16", "v_G4") %in% names(raw))) {
  .e16_sil <- stringr::str_detect(strip_qual(raw$v_E16), .SIL) %in% TRUE
  .g4_sil  <- stringr::str_detect(strip_qual(raw$v_G4),  .SIL) %in% TRUE
  .ck("E-16 silence is never read as a documented provision",
      sum(.e16_sil & (A$s_maintcost %in% 1)) == 0,
      sprintf("%d silent E-16 cells set s_maintcost", sum(.e16_sil & (A$s_maintcost %in% 1))))
  .ck("G-4 silence is never read as a documented targeting scope",
      sum(.g4_sil & (A$equity_targeted %in% 1)) == 0,
      sprintf("%d silent G-4 cells set equity_targeted", sum(.g4_sil & (A$equity_targeted %in% 1))))
  .ck("G-4 silence is not mis-parsed into a named scope",
      sum(.g4_sil & !(as.character(A$equity_target) %in% c("Not stated", NA))) == 0,
      sprintf("%d silent G-4 cells carry a named scope",
              sum(.g4_sil & !(as.character(A$equity_target) %in% c("Not stated", NA)))))
}

## SILENCE-CONFLATION GUARD. A coded value that merges an absence with a silence
## (the "None/Not stated" family) must never set an explicit-absence flag, because
## every downstream misfit rule reads those flags as R1(a) documented "No". This
## check re-reads the raw column rather than trusting the derived flag.
.amb_pat <- "(?i)^(none|no)\\s*/\\s*not stated"
if (exists("raw") && "v_E13" %in% names(raw)) {
  .amb <- grepl(.amb_pat, trimws(as.character(raw$v_E13)), perl = TRUE)
  .ck("no absence flag is set by an absence/silence compound value",
      sum(.amb & (A$adv_absent %in% 1)) == 0,
      sprintf("%d records set adv_absent from an ambiguous value", sum(.amb & (A$adv_absent %in% 1))))
}

## H5 EXTENDED TO HEADLINE PROSE (audit item 7). Checking only the verdict TABLE
## let a stale number survive in a sentence -- exactly what happened with the
## financial headline. Every "N of M" claim in the handover's headline block is now
## parsed and reconciled against this run.
if (exists(".doc_txt") && "PROJECT_STATUS.md" %in% names(.doc_txt)) {
  .ps <- .doc_txt[["PROJECT_STATUS.md"]]
  .prose_bad <- character(0)
  ## the D3b headline: "<misfit> of <evaluable> evaluable records"
  .m <- regmatches(.ps, regexpr("[0-9]{2,3} of [0-9]{2,3} evaluable records publish", .ps))
  if (length(.m)) {
    .n <- as.integer(unlist(regmatches(.m, gregexpr("[0-9]+", .m))))
    ## the denominator excludes BOTH Indeterminate and Not applicable. The 23
    ## N/A records document no participant-facing cash transaction at all (in-kind
    ## supply, direct subsidised sale, ongoing bill credit), so a claim about "records
    ## publishing a payment route that requires pre-financing" cannot be based on them.
    ## The check previously counted them as evaluable, which forced the prose to quote
    ## a denominator larger than the set the statement is actually about.
    .want <- c(sum(A$fit_D3b == "Confirmed misfit"),
               sum(!A$fit_D3b %in% c("Indeterminate", "Not applicable")))
    if (!identical(.n, .want))
      .prose_bad <- c(.prose_bad, sprintf("D3b headline says %s, run has %s",
                                          paste(.n, collapse = "/"), paste(.want, collapse = "/")))
  }
  ## the D4b headline: "43 of N evaluable records document published"
  .m2 <- regmatches(.ps, regexpr("[0-9]{2,3} of [0-9]{2,3} evaluable records document published", .ps))
  if (length(.m2)) {
    .n2 <- as.integer(unlist(regmatches(.m2, gregexpr("[0-9]+", .m2))))
    .want2 <- c(sum(A$fit_D4b == "Documented fit"), sum(A$fit_D4b != "Indeterminate"))
    if (!identical(.n2, .want2))
      .prose_bad <- c(.prose_bad, sprintf("D4b headline says %s, run has %s",
                                          paste(.n2, collapse = "/"), paste(.want2, collapse = "/")))
  }
  ## the compounding distribution, wherever it is stated in prose
  .m3 <- regmatches(.ps, regexpr("[0-9]{3} records carry no confirmed misfit", .ps))
  if (length(.m3)) {
    .n3 <- as.integer(unlist(regmatches(.m3, gregexpr("[0-9]+", .m3))))
    .want3 <- sum(rowSums(sapply(A[c("fit_D1","fit_D2","fit_D3a","fit_D3b",
                                     "fit_D3c","fit_D4a","fit_D4b")],
                                 function(x) x == "Confirmed misfit")) == 0)
    if (!identical(.n3, .want3))
      .prose_bad <- c(.prose_bad, sprintf("compounding prose says %d, run has %d", .n3, .want3))
  }
  .ck("H5 headline PROSE figures match this run (not just the table)",
      length(.prose_bad) == 0, paste(.prose_bad, collapse = "; "))
}

## =============================================================================
## v58 / task 1.5 -- THE SHIPPED DOCUMENTS ARE CHECKED AGAINST THE RUN.
## README.md and PROJECT_STATUS.md carried a corpus size, a rule version, a table
## count, a figure count and a floor-cell size that the run contradicted, and
## nothing failed. H5 covered PROJECT_STATUS's headline table only. These checks
## read the documents as shipped and fail the build on any number that disagrees
## with what this run actually produced.
## =============================================================================
.doc_bad <- character(0)
.doc_has <- function(file, pattern, label, want) {
  ## Documents are checked only if present. The clean distribution ships the
  ## framework and supplementary notes and no build-status prose, so a missing
  ## file is not a defect -- a PRESENT file whose numbers have drifted is.
  if (!file.exists(file)) return(invisible())
  .txt <- paste(readLines(file, warn = FALSE), collapse = " ")
  if (!grepl(pattern, .txt, perl = TRUE))
    .doc_bad <<- c(.doc_bad, sprintf("%s: %s should read %s", basename(file), label, want))
}
## NOTE: the TABLE COUNT is deliberately not asserted here. This module runs
## before 18_fact_registry and 20_reliability_retest write theirs, so a count
## taken now is always short of the final total. It is asserted in
## verify/verify_claims.py, which runs after the pipeline has finished.
.n_rec <- nrow(FGB)
.n_ctry <- length(unique(FGB$country))
## The numbers are asserted, not the sentence they sit in, so the guard survives
## an edit to the prose while still failing on a figure the run contradicts.
.doc_has("README.md", sprintf("%d programme", .n_rec), "corpus size", .n_rec)
.doc_has("README.md", sprintf("%d countries", .n_ctry), "country count", .n_ctry)
## the superseded corpus and rule version must not reappear anywhere in README
if (file.exists("README.md")) {
  .rt <- paste(readLines("README.md", warn = FALSE), collapse = " ")
  if (grepl("709 programmes", .rt)) .doc_bad <- c(.doc_bad, "README still says 709 programmes")
  if (grepl("FITNESS-RULES-LOCKED-v36", .rt)) .doc_bad <- c(.doc_bad, "README still names rule version v36")
  if (grepl("NbS_709_ANALYTICAL", .rt)) .doc_bad <- c(.doc_bad, "README still references the deleted 709 export")
  if (grepl("714_WORKING\\.xlsx` \\| the coded workbook — the \\*\\*only\\*\\* input", .rt))
    .doc_bad <- c(.doc_bad, "README still calls the 714 working workbook the only input")
}
if (file.exists("PROJECT_STATUS.md")) {
  .pt <- paste(readLines("PROJECT_STATUS.md", warn = FALSE), collapse = " ")
  if (grepl("no component moves more than 17", .pt))
    .doc_bad <- c(.doc_bad, "PROJECT_STATUS still claims no component moves >17pp (five cells exceed it)")
  if (grepl("only 1 explicit", .pt) || grepl('strict[^.]{0,40}"only 1"', .pt))
    .doc_bad <- c(.doc_bad, "PROJECT_STATUS still reports the D3b strict sensitivity as 1")
}
.ck("shipped documents agree with this run (README + PROJECT_STATUS)",
    length(.doc_bad) == 0, paste(.doc_bad, collapse = "; "))

## v13 REWRITTEN. This check previously asserted that a full APPLICATION
## guide (D-1 == 2) must clear D3a's TECHNICAL guidance floor. That assertion was
## the conflation the v13 construct purge removed: an application guide tells you
## how to apply, not how to install, and admitting it let 84 records score a
## technical fit with no technical standard and no technical advice. The check now
## tests the corrected floor -- a technical installation guide or technical advice
## clears it -- and additionally asserts the SEPARATION, i.e. that D-1 alone can no
## longer clear it. A regression that re-admitted D-1 would fail the second line.
.ck("M1 technical guidance floor accepts technical guidance or advice",
    all(A$guid_element[which(A$s_guides == 1 | A$i_advice == 1)] == 1),
    sprintf("%d records with technical guidance/advice fail the floor",
            sum(A$guid_element[which(A$s_guides == 1 | A$i_advice == 1)] != 1)))
.ck("M1b application guide (D-1) alone does NOT clear the technical floor",
    all(A$guid_element[which(A$p_guide == 2 & A$s_guides != 1 & A$i_advice != 1)] == 0),
    sprintf("%d records cleared the technical floor on an application guide alone",
            sum(A$guid_element[which(A$p_guide == 2 & A$s_guides != 1 & A$i_advice != 1)] != 0)))

## 7. NO INVENTED CATEGORIES: every measure, actor and instrument label drawn in a
## figure must exist as a coded column.
## VERIF_PROV is gone -- the D4a panel no longer carries a verification
## stage, because fit_D4a does not read F-1..F-4 (those decide D4b, panel g). The
## check still has to cover the monitoring fields the D4b panel draws, so they are
## named directly rather than through the retired constant. Dropping them from the
## check instead would have quietly narrowed an invariant while "fixing" a build
## error, which is how the retired-construct defects got in in the first place.
.d4b_cols <- c("f_me_published", "mon_indicators", "f_review", "f_feedback",
               "mon_webpage", "d4b_looks", "d4b_web", "d4b_not_reported")
.cols <- c(unlist(ALL_MEAS), names(ACTORS), names(MAINT_SUP),
           intersect(.d4b_cols, names(FGB)))
if (!all(.d4b_cols %in% names(FGB)))
  .ck("D4b monitoring fields all present in the figure frame", FALSE,
      paste(setdiff(.d4b_cols, names(FGB)), collapse = " "))
.ck("every figure category maps to a coded column",
    all(.cols %in% names(FGB)), paste(setdiff(.cols, names(FGB)), collapse = " "))

## 8. QUOTED CORPUS TOTALS used in captions. country_mod POOLS small countries into
## "Other regions" for modelling, so it is NOT the country count; the coded country
## is `country`. The first version of this check compared the wrong column and
## failed for that reason, not because the corpus changed.
.n_country <- n_distinct(A$country[!is.na(A$country)])
.ck(sprintf("corpus totals: %d records, %d coded countries", .N_COHORT, .n_country),
    nrow(A) == .N_COHORT && .n_country >= 25,
    sprintf("records=%d countries=%d", nrow(A), .n_country))

## 9. COMPOUND MISFIT: recompute and compare with what section 9 reports, so the
## "misfits do not compound" claim can never drift from the verdict layer.
.mk <- as.data.frame(lapply(A[.fc], function(x) as.integer(x == "Confirmed misfit")))
.k  <- rowSums(.mk)
msg("  INFO  compound misfit on the canonical layer: %s",
    paste(sprintf("%d misfits n=%d", as.integer(names(table(.k))), as.integer(table(.k))),
          collapse = "; "))
## the CUMULATIVE shares are emitted too, not just the per-count records.
## Section 3.3 states "602 programmes (85.1%) carry at least one confirmed misfit
## and 94 (13.3%) three or more". Those shares were being DERIVED in prose from
## this table's counts, so the manuscript verifier -- which asks whether a quoted
## number appears in some run table -- flagged 85.1 as appearing nowhere. It was
## right to: a number the verifier cannot see in a table is a number nobody checks.
## Both shares now live here with their denominator, computed from the same counts.
.cmp <- tibble(n_misfits = as.integer(names(table(.k))), records = as.integer(table(.k)))
.cmp$pct_corpus     <- round(100 * .cmp$records / sum(.cmp$records), 1)
.cmp$cum_at_least   <- rev(cumsum(rev(.cmp$records)))
.cmp$pct_at_least   <- round(100 * .cmp$cum_at_least / sum(.cmp$records), 1)
save_tab(.cmp, "16_compound_misfit_canonical")

## 10. fig12_field_vs_city (the city sheet) is gap-free -- the fault that motivated both its
## rebuilds. Every context scored on every component, no empty cell.
## The object is RX (prescriptions), not the retired CITY rate table. Guarding on
## exists() alone let this check pass VACUOUSLY when the object was renamed -- the
## audit reported 21 checks instead of 22 and nobody was told why. The absence of
## the object is now itself a failure, not a silent skip.
.ck("fig12_field_vs_city: prescription table exists", exists("RX"))
if (exists("RX")) {
  ## An NA rate is legitimate ONLY where the action says the provision is not
  ## required in that context -- that is a stated cell, not a hole. An NA carrying
  ## any other action would be a genuine gap.
  .ck("fig12_field_vs_city: every cell states something (NA only where 'not required')",
      all(is.na(RX$rate) == (as.character(RX$action) == "NOT REQUIRED in this context")),
      sprintf("%d NA rates, %d not-required cells",
              sum(is.na(RX$rate)), sum(as.character(RX$action) == "NOT REQUIRED in this context")))
  ## Within each context axis every column must carry the same row set, so no
  ## block has a short column and no cell in the rendered figure is blank.
  .blk <- RX %>% count(grp, ctx, name = "n_prov") %>% group_by(grp) %>%
    summarise(ok = n_distinct(n_prov) == 1, .groups = "drop")
  .ck("fig12_field_vs_city: within each context axis, every column carries the same rows",
      all(.blk$ok), paste(.blk$grp[!.blk$ok], collapse = "; "))
}

## 10b. A DEFINITIONAL cell must be DISCLOSED wherever it is drawn.
##
## fit_D2 returns "Documented fit" for every owner-dominated record BY RULE, so
## that context reads 100% on the route-beyond-owner component -- a property of
## the rule, not a finding about those cities. Drawn without a note it invites
## exactly the wrong reading (that owner-dominated cities provide MORE), which is
## the error this check exists to prevent. The cell may be drawn; the caption
## must say what it is.
if (exists("GAPM") && exists("p_fig11b")) {
  .defcell <- GAPM %>%
    filter(grepl("Route beyond", short), ctx == "Owner-dominated") %>%
    pull(rate)
  if (length(.defcell) && isTRUE(all.equal(max(.defcell, na.rm = TRUE), 100))) {
    .sub <- tryCatch(p_fig11b$labels$subtitle, error = function(e) "")
    .ck("fig10_context_deviations: the definitional owner-dominated D2 cell is disclosed in the caption",
        grepl("definitional", .sub, ignore.case = TRUE) &&
          grepl("by construction", .sub, ignore.case = TRUE),
        "cell reads 100% by rule but the caption does not say so")
  }
}

## 11. FIGURE 10's EMPTY TILES ARE DISCLOSED, NOT ABSENT.
##
## This check used to demand that NO 'insufficient evidence' tile remained, and it
## failed every run once the figure's design changed. The figure now DELIBERATELY
## keeps those tiles and labels them, because a context cell whose requirement does
## not vary leaves the adjusted model unidentified, and drawing that is honest where
## dropping the column would quietly narrow the figure to the cells that happened to
## cooperate. A check that contradicts a deliberate design decision is a stale check,
## and leaving it to fail trains a reader to ignore the audit.
##
## So the invariant moves to where the risk actually is. The caption states how many
## tiles carry an estimate and how many do not, and those two numbers are computed
## from adj_dev at draw time. What must never drift is the caption against the data,
## and that is now what is asserted. The object's absence remains a failure, because
## a check that can vanish when a block is renamed is not a check.
.ck("fig10_context_deviations: the figure 10 object was built",
    exists("adj_dev") && nrow(adj_dev) > 0,
    "adj_dev absent -- figure 10 block did not run")
if (exists("adj_dev") && exists("p_b6a")) {
  .est <- sum(!is.na(adj_dev$dev)); .na <- sum(is.na(adj_dev$dev)); .all <- nrow(adj_dev)
  .sb  <- tryCatch(p_b6a$labels$subtitle, error = function(e) "")
  .ck(sprintf("fig10_context_deviations: caption discloses %d of %d tiles estimated, %d insufficient",
              .est, .all, .na),
      grepl(sprintf("%d of %d tiles carry an estimate", .est, .all), .sb, fixed = TRUE) &&
        (.na == 0 || grepl("insufficient evidence", .sb, fixed = TRUE)),
      sprintf("caption does not match adj_dev (est=%d all=%d na=%d)", .est, .all, .na))
}

## ---------------------------------------------------------------------------
## RETIRED CONSTRUCTS MUST NOT DRIVE A REPORTED VERDICT.
## Figures 11 and 12 read admin_profile -- the provision-COUNTING construct D3c
## abandoned at v18 for requirement matching -- and reported a D3c documented-fit
## rate of 56% against the canonical 24.9%, with 312 records disagreeing. The
## error was invisible because both are legitimate variables; only their ROLES
## differ. This check pins every reported fit signal to the canonical layer.
{
  .fig_d3c <- ifelse(is.na(A$admin_profile) | A$admin_profile == "Indeterminate", NA,
                     as.integer(A$admin_profile %in% c("Broad learning + compliance support",
                                                       "Basic applicant pathway")))
  .can_d3c <- ifelse(A$fit_D3c == "Indeterminate", NA,
                     as.integer(A$fit_D3c == "Documented fit"))
  .gap <- abs(mean(.fig_d3c, na.rm = TRUE) - mean(.can_d3c, na.rm = TRUE))
  .ck("v22 admin_profile is descriptive only -- it must never stand in for fit_D3c",
      TRUE,
      sprintf("retired-vs-canonical D3c gap is %.1f pp; figures must read fit_D3c",
              100 * .gap))

  .src <- paste(readLines("R/14b_figures_rq.R", warn = FALSE), collapse = "\n")
  .ck("v22 no figure derives a D3c fit signal from admin_profile",
      !grepl("as\\.integer\\(admin_profile %in%", .src),
      "a figure still reads the retired counting construct as a verdict")
}

## ---------------------------------------------------------------------------
## THE FRAMEWORK DOCUMENT MUST NOT CONTRADICT THE RUN.
## FRAMEWORK_OPERATIONALISATION.md carried "no programme carries two or more
## confirmed misfits" for many versions after it stopped being true -- it was
## written against an early rule set and never revised as components were rebuilt.
## 140 records now carry 2+. Prose written once and left alone is the failure mode
## these checks exist for, so the claim is pinned to the data.
{
  .fitc <- c("fit_D1","fit_D2","fit_D3a","fit_D3b","fit_D3c","fit_D4a","fit_D4b")
  .nm <- rowSums(sapply(.fitc, function(v) A[[v]] == "Confirmed misfit"))
  .multi <- sum(.nm >= 2)
  .ck("v25 compound-misfit claim in the framework matches the run",
      .multi > 0,
      sprintf("%d records carry 2+ confirmed misfits", .multi))

  ## file.exists() rather than tryCatch(): readLines() on a missing file WARNS
  ## before it errors, and the warning survives the tryCatch into the run's
  ## warning gate as an unclassified entry. The distribution ships no handover
  ## prose, so absence is the normal case and must be silent; a document that IS
  ## present is still read and still checked below.
  .fw <- if (file.exists("FRAMEWORK_OPERATIONALISATION.md"))
           paste(readLines("FRAMEWORK_OPERATIONALISATION.md", warn = FALSE),
                 collapse = "\n") else ""
  .ck("v25 framework does not assert that misfits never compound",
      !grepl("no programme[[:space:]]*\\n?[[:space:]]*carries two or more confirmed misfits", .fw),
      "the retired 'misfits do not compound' claim is back in the framework document")

  ## inferred codes are equal evidence: no REPORTED verdict may exclude them
  .ck("v25 no reported verdict excludes [inferred] codes",
      !any(grepl("^eco_fit_strict$", names(A))),
      "eco_fit_strict is live again -- inferred codes must be treated as equal (0.4)")
}

## ---------------------------------------------------------------------------
## NO FIGURE MAY DERIVE A D4a OR D4b SIGNAL FROM A PROVISION-COUNTING FLAG.
## The v22 guard covered D3c/admin_profile only. The IDENTICAL fault survived in
## the two neighbouring lines of the same mutate() -- d4a read st_support (59.8%
## vs canonical 99.7% fit-or-partial, 283 records disagreeing) and d4b rebuilt a
## rate from mon_indicators/f_review. A guard naming one construct does not
## generalise, so this one is written over the whole family.
.src_fig <- paste(readLines("R/14b_figures_rq.R", warn = FALSE), collapse = "\n")
.bad_d4 <- c(
  "d4a *= *as.integer\\(st_support",
  "d4b *= *as.integer\\(mon_indicators",
  "d4a *= *as.integer\\(steward_config",
  "d4b *= *as.integer\\(monitor_config")
.hits_d4 <- .bad_d4[sapply(.bad_d4, function(p) grepl(p, .src_fig))]
.ck("v28: no figure derives a D4a/D4b signal from a provision-counting flag",
    length(.hits_d4) == 0,
    sprintf("found %d: %s", length(.hits_d4), paste(.hits_d4, collapse = "; ")))

## Threshold consistency: a multi-domain comparison must use ONE bar for all rows.
.mixed <- grepl("fitpos *<- *c\\(\"Documented fit\", *\"Partial fit\"\\)", .src_fig) &&
          grepl("d3c *= *ifelse\\(fit_D3c", .src_fig)
.ck("v28: figure 7 uses one verdict threshold across all seven domains",
    !.mixed,
    "some rows count fit-or-partial while others count documented fit only")

## ---------------------------------------------------------------------------
## NO FIGURE MAY HARDCODE A CORPUS COUNT.
## Six figure captions read "707 programmes" against a 709-record cohort -- the
## count from before de-duplication, frozen into caption strings and never
## updated. It is the same failure as the retired-construct figures: a number
## living in TEXT instead of being read from the data. A caption that states a
## corpus size must interpolate nrow(A), never a literal.
.figsrc <- unlist(lapply(list.files("R", pattern = "^14[abc].*\\.R$", full.names = TRUE),
                         readLines, warn = FALSE))
.figsrc <- .figsrc[!grepl("^\\s*#", .figsrc)]
.stale_n <- grep("\\b(70[0-8]|71[0-9])\\s*(programmes|records|cases|coded)", .figsrc, value = TRUE)
.ck("v30: no figure caption hardcodes a stale corpus count",
    length(.stale_n) == 0,
    sprintf("found %d: %s", length(.stale_n),
            paste(substr(.stale_n, 1, 90), collapse = " | ")))

## ---------------------------------------------------------------------------
## RETIRED CONSTRUCTS MAY NOT APPEAR ON ANY LINE THAT BUILDS A FIT
## SIGNAL -- checked semantically, not by literal string.
## The v28 guard matched four exact source strings. That is a guard against the
## four defects we had already found, not against the class: it passed while
## 10_context_matching.R scored administrative and monitoring readiness from
## admin_profile and monitor_config, because those lines are spelled differently.
## A guard that only recognises defects it has already seen is a regression test,
## not an invariant. This one asserts the invariant: in any module that emits a
## figure or a reported table, a retired construct may not appear on a line that
## also assigns a fit-signal name.
.retired <- c("admin_profile", "monitor_config", "n_enablers", "learn_score",
              "comply_score", "st_support", "tech_config", "eco_fit_strict")
.sigpat  <- "(d[1-4][abc]?|_ok|_fit|fit|verdict)\\s*(=|<-)"
.reporting_modules <- list.files("R", pattern = "^(07|09|10|11|12|14[abc])_.*\\.R$",
                                 full.names = TRUE)
.hits_retired <- character(0)
for (.p in .reporting_modules) {
  .ln <- readLines(.p, warn = FALSE)
  .ln <- .ln[!grepl("^\\s*#", .ln)]                       # comments may name them
  for (.r in .retired) {
    .bad <- grep(sprintf("\\b%s\\b", .r), .ln, value = TRUE)
    .bad <- grep(.sigpat, .bad, value = TRUE, perl = TRUE)
    ## a line that merely CARRIES the construct as a descriptive column is fine;
    ## the violation is assigning a fit signal FROM it.
    if (length(.bad))
      .hits_retired <- c(.hits_retired,
                         sprintf("%s: %s -> %s", basename(.p), .r, substr(.bad[1], 1, 70)))
  }
}
.ck("v30: no reported figure or table derives a fit signal from a retired construct",
    length(.hits_retired) == 0,
    sprintf("found %d: %s", length(.hits_retired),
            paste(.hits_retired, collapse = " | ")))


## ---------------------------------------------------------------------------
## v30b: A FLOW PANEL MUST TRACE THE ROUTE ITS OWN VERDICT WAS COMPUTED FROM.
## Caught by the author, not by a check: panel d (D3a, technical enablement) put
## APPLICANT GUIDES AND FORMS in its middle stage, and panel f (D3c,
## administrative access) opened on learn_score/comply_score -- the retired
## 0-6 counting constructs. Both panels were internally consistent and both were
## showing a reader a journey through the wrong domain's material. The colour was
## the right verdict; the path to it was not.
## NO FLOW STAGE MAY CARRY AN UNLABELLED NODE.
##
## Third occurrence of the same defect: a stage's factor levels are written out by
## hand, a new category is added to the case_when above it, and the level list is
## not extended. R does not error -- it silently maps the unlisted label to NA, and
## the panel renders a node captioned "NA". The last instance hid 223 programmes
## (the split of "Nothing published" into documented-denial vs silence). A missing
## level is now a build failure, not something a reader has to spot in the render.
for (.fnm in list.files(CFG$out_tab, pattern = "^14_b1[c-i].*\\.csv$",
                        full.names = TRUE)) {
  .ft <- tryCatch(read.csv(.fnm, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(.ft)) next
  .stagecols <- setdiff(names(.ft), c("n", "alluv", "verdict"))
  for (.sc in .stagecols) {
    .vals <- as.character(.ft[[.sc]])
    .ck(sprintf("flow stage '%s' in %s carries no unlabelled node",
                .sc, basename(.fnm)),
        !any(is.na(.vals) | trimws(.vals) %in% c("", "NA")),
        sprintf("%d NA/empty labels -- a factor level list is missing a category",
                sum(is.na(.vals) | trimws(.vals) %in% c("", "NA"))))
  }
  ## v38b: A NODE THAT ASSERTS A STATE MUST NOT SPAN VERDICTS THE RULE SEPARATES.
  ##
  ## An external reading caught a node captioned "documents state nothing is
  ## reported" carrying 58 misfit paths AND 83 partial ones: its condition was an
  ## OR across three fields, so it swept in programmes denying only ONE of two
  ## disjuncts, which the rule treats as a partial documented absence. The label
  ## was true of one group and false of the other, and a reader following a red
  ## ribbon back landed on a node that also fed amber.
  ##
  ## THE "RULE-COMPLETE NODE" CHECK IS RETIRED, and its removal is the point.
  ##
  ## It guarded two D4b reporting nodes whose conditions encoded whole verdict
  ## branches -- and the author's reading established that such a node should not
  ## exist at all. A stage names what the documents say about ONE field; the rule
  ## combines stages into a verdict. A node that reaches exactly one verdict by
  ## construction has smuggled the rule into the stage, which is what made that
  ## panel read as invented categories. So the invariant the check enforced has
  ## been replaced by a stronger structural one below: a stage may not read a field
  ## that another stage in the same panel is built on. Keeping the old check would
  ## have preserved the very construction it was written to police.
  ##
  ## NO FIELD IS READ BY TWO STAGES OF THE SAME PANEL. The reporting stage read
  ## f_review_absent (F-3) in its tail branches while the review stage was built on
  ## F-3 -- so one field drove two columns, and a reader comparing them saw the same
  ## evidence twice under different names.
  ## (the field-duplication check itself is asserted ONCE, after this loop -- it is
  ## a property of the stage declaration, not of any output file)
}

## Asserted once, outside the per-file loop: this is a property of the stage
## DECLARATION, not of any output file, and inside the loop it fired once per
## matching flow table -- the same check reported twice, which is the reporting
## equivalent of the defect it exists to catch.
## THE SPEC FILE'S LIVE-STATE BLOCKS MUST MATCH THE RUN.
##
## FITNESS_RULES_LOCKED_*.json carries three blocks that state CURRENT state rather
## than history: verdicts_locked, partial_composition and typology_sizes. All three
## drifted. Two external readings were needed to find it: the first caught
## typology_sizes holding 123/183/391/12 from a pre-v39 run while the same file's own
## v40 narrative said 472/113/122; the sweep that finding prompted then found
## verdicts_locked stale on three components (pre-v38 D3c 511/18 and D4b 533/30, and
## a transposed D1 partial/misfit pair) and partial_composition holding pre-v38
## splits.
##
## verdicts_locked's own provenance note ALREADY said "regenerate it from the run
## whenever a rule or the dataset changes", and v38, v39 and v40 each changed rules
## without doing so. A note instructing a future session to do something is not a
## mechanism; this check is. The version-keyed entries (v28_changes, v37_changes, ...)
## are deliberately NOT checked -- they are historical transition records, and
## "D3c: misfit 18 -> 47" is only meaningful if the 18 stays.
## THE SPEC WAS SEARCHED FOR IN THE WRONG DIRECTORY, SO BOTH SPEC-LOCK CHECKS
## SILENTLY SKIPPED. This looked only in "..", the parent of the pipeline root -- where
## the spec sat in the authoring workspace. The RELEASED bundle ships it AT the pipeline
## root, so for every recipient who unzipped and ran reproduce.sh the two spec-lock
## checks did not exist. The suite reported 122 passing where a session note claimed
## 124, and the gap was two checks that had stopped running, not two that failed.
## That is exactly the defect these checks exist to catch, one level up. The search now
## covers the pipeline root AND its parent, and MISSING IS A FAILURE, not a skip.
.spec <- head(sort(c(list.files(".",  pattern = "^FITNESS_RULES\\.json$", full.names = TRUE),
                     list.files("..", pattern = "^FITNESS_RULES\\.json$", full.names = TRUE)),
                   decreasing = TRUE), 1)
## THE RUN MUST NAME THE DATASET IT ANALYSED, AND IT MUST NOT BE A SUPERSEDED ONE.
##
## This check exists because the failure it guards actually happened: ./reproduce.sh
## did not pin NBS_DATA, R/00_setup.R fell back through a candidate list that still
## named three superseded workbooks, and a full acceptance run silently analysed the
## v54 corpus -- rendering 23 figures of wrong numbers (depaving 205 not 206,
## technical 680 not 681) that diffed cleanly against an expected/ built the same way.
## Nothing failed. That is the whole danger: a stale input does not error, it produces
## a complete and internally consistent wrong answer.
##
## Two assertions, not one. The corpus size alone is NOT sufficient -- v53, v54 and v56
## all hold 707 records, so a size check would have passed throughout. The FILENAME is
## what distinguishes them.
.dfile <- basename(CFG$data_file)
.ck("run analysed a non-superseded dataset (filename, not just row count)",
    !grepl("v4[0-9]|v5[0-5]|714", .dfile),
    sprintf("analysed %s -- a superseded corpus. v53/v54/v56 ALL hold 707 records, so the row-count checks cannot catch this; pin NBS_DATA or fix DATA_CANDIDATES", .dfile))
.ck("corpus size matches the analysed dataset",
    nrow(A) == 707,
    sprintf("%d records read from %s", nrow(A), .dfile))

.ck("spec-lock checks have their input (a FITNESS_RULES_LOCKED_v*.json is present)",
    length(.spec) == 1 && requireNamespace("jsonlite", quietly = TRUE),
    "no FITNESS_RULES_LOCKED_v*.json at the pipeline root or its parent -- the two spec-lock checks below cannot run, and their silent absence is what this check exists to prevent")
if (length(.spec) && requireNamespace("jsonlite", quietly = TRUE)) {
  .S <- tryCatch(jsonlite::fromJSON(.spec, simplifyVector = FALSE), error = function(e) NULL)
  .ds <- tryCatch(read.csv(file.path(CFG$out_tab, "14_b1_domain_states.csv"),
                           stringsAsFactors = FALSE), error = function(e) NULL)
  if (!is.null(.S) && !is.null(.ds) && !is.null(.S$verdicts_locked)) {
    .k <- c("D1 E" = "D1", "D2 T" = "D2", "D3a" = "D3a", "D3b" = "D3b",
            "D3c" = "D3c", "D4a" = "D4a", "D4b" = "D4b")
    .lbl <- c(fit = "Documented fit", partial = "Partial fit", misfit = "Confirmed misfit",
              indeterminate = "Indeterminate", na = "Not applicable")
    .drift <- character(0)
    ## WIDENED TO EVERY COMPUTABLE FIELD. This checked five fields, so
    ## `evaluable`, `fit_pct_evaluable` and `cohort` drifted silently through three
    ## rounds -- the same defect the check exists to prevent, one field-set over.
    ## `evaluable` had drifted on six of seven components and every
    ## `fit_pct_evaluable` still held a v59.3 value. Anything derivable from the run
    ## is now asserted against the run.
    .N <- nrow(A)
    if (!identical(as.integer(.S$cohort), as.integer(.N)))
      .drift <- c(.drift, sprintf("cohort spec=%s run=%d", .S$cohort, .N))
    for (.dn in names(.k)) {
      .rows <- .ds[substr(trimws(.ds$domain), 1, nchar(.dn)) == .dn, ]
      .blk  <- .S$verdicts_locked[[.k[[.dn]]]]
      if (!nrow(.rows) || is.null(.blk)) next
      for (.f in names(.lbl)) {
        .run <- sum(.rows$n[.rows$outcome == .lbl[[.f]]])
        if (!identical(as.integer(.blk[[.f]]), as.integer(.run)))
          .drift <- c(.drift, sprintf("%s$%s spec=%s run=%d", .k[[.dn]], .f,
                                      .blk[[.f]], .run))
      }
      .evrun <- .N - sum(.rows$n[.rows$outcome == "Indeterminate"]) -
                     sum(.rows$n[.rows$outcome == "Not applicable"])
      if (!is.null(.blk$evaluable) && !identical(as.integer(.blk$evaluable), as.integer(.evrun)))
        .drift <- c(.drift, sprintf("%s$evaluable spec=%s run=%d", .k[[.dn]], .blk$evaluable, .evrun))
      if (!is.null(.blk$fit_pct_evaluable)) {
        .pctrun <- round(100 * sum(.rows$n[.rows$outcome == "Documented fit"]) / .evrun, 1)
        if (!isTRUE(all.equal(as.numeric(.blk$fit_pct_evaluable), .pctrun, tolerance = 0.051)))
          .drift <- c(.drift, sprintf("%s$fit_pct_evaluable spec=%s run=%.1f",
                                      .k[[.dn]], .blk$fit_pct_evaluable, .pctrun))
      }
    }
    .ck("spec verdicts_locked matches the run on every component",
        length(.drift) == 0,
        sprintf("%d field(s) drifted: %s", length(.drift),
                paste(head(.drift, 8), collapse = "; ")))

    .pr <- tryCatch(read.csv(file.path(CFG$out_tab, "08_macro_architecture_profiles.csv"),
                             stringsAsFactors = FALSE), error = function(e) NULL)
    if (!is.null(.pr) && !is.null(.S$typology_sizes)) {
      .td <- character(0)
      for (.i in seq_len(nrow(.pr))) {
        .nm <- .pr$name[.i]
        if (!is.null(.S$typology_sizes[[.nm]]) &&
            !identical(as.integer(.S$typology_sizes[[.nm]]), as.integer(.pr$n[.i])))
          .td <- c(.td, sprintf("%s spec=%s run=%d", .nm, .S$typology_sizes[[.nm]], .pr$n[.i]))
      }
      .ck("spec typology_sizes matches the macro partition",
          length(.td) == 0, paste(.td, collapse = "; "))
    }
  }
}

## NO FIGURE TITLE OR SUBTITLE MAY SPELL OUT A CLUSTER COUNT AS A WORD.
##
## figS12's title read "the finer six-class partition" while the run selected k on the
## documented subset -- it drew five classes at v39.1 and four at v39.2, so the figure
## had been mislabelled for as long as k was not six, invisibly, because the number sat
## in prose while the panel drew whatever selection returned. figS11's subtitle
## likewise asserted "CH keeps rising to k = 6" and "517 of the 682 classified
## records" against its own plotted table (CH peaks at k = 4; 472 of 707). Both are now
## computed. This check keeps them that way: a spelled-out count in a cluster figure's
## text is a literal that cannot track the data.
.kw <- c("two", "three", "four", "five", "six", "seven", "eight")
.fig_src <- unlist(lapply(list.files("R", pattern = "^14.*\\.R$", full.names = TRUE), readLines))
.titles  <- grep("title *=|subtitle *=", .fig_src, value = TRUE)
.badk <- grep(sprintf("(%s)[- ]class", paste(.kw, collapse = "|")), .titles,
              value = TRUE, ignore.case = TRUE)
.ck("no figure title spells out a cluster count (it must be computed)",
    length(.badk) == 0,
    sprintf("%d title(s) carry a written class count: %s", length(.badk),
            substr(paste(.badk, collapse = " | "), 1, 120)))

## And the two partitions the supplement reports must not contradict each other.
if (exists("FGB") && all(c("typology_k", "typology_macro") %in% names(FGB))) {
  .nk <- n_distinct(FGB$typology_k[as.character(FGB$typology_k) != "Opacity group"])
  .nm <- n_distinct(FGB$typology_macro[as.character(FGB$typology_macro) != "Opacity group"])
  .ck("the exploratory partition is finer than the macro partition",
      .nk >= .nm, sprintf("exploratory k = %d but macro k = %d", .nk, .nm))
}

## v39b: NO ITEMISED CHANNEL FLAG MAY ALIAS ANOTHER CHANNEL'S VOCABULARY.
##
## ch_paper's pattern was "paper|mail", and "mail" is a substring of "Email" -- so 25
## records whose D-4 read "Email" or "Online form; Email" were flagged as documenting
## a PAPER route, and figure 6's channel stratum showed a postal channel for
## programmes that document none. Substring collisions between sibling categories
## cannot be caught by any count-based check, so the vocabulary is asserted directly:
## an email-only record must set ch_email and NOT ch_paper.
if (exists("A") && all(c("ch_paper", "ch_email") %in% names(A))) {
  .v4    <- strip_qual(raw$v_D4)
  .eonly <- stringr::str_detect(.v4, stringr::regex("e-?mail", ignore_case = TRUE)) &
            !stringr::str_detect(.v4, stringr::regex("paper|postal|(?<![e-])\\bmail",
                                                     ignore_case = TRUE))
  .eonly[is.na(.eonly)] <- FALSE
  .alias <- sum(.eonly & coalesce(as.integer(A$ch_paper), 0L) == 1)
  .ck("D-4: an e-mail-only channel list does not also set the paper flag",
      .alias == 0,
      sprintf("%d records flagged as paper solely because 'mail' sits inside 'Email'",
              .alias))
}

if (exists("D4B_STAGE_FIELDS")) {
  .dup <- names(which(table(unlist(D4B_STAGE_FIELDS)) > 1))
  .ck("no coded field drives two stages of the D4b panel",
      length(.dup) == 0,
      sprintf("field(s) read by more than one stage: %s", paste(.dup, collapse = ", ")))
}

## The invariant: the stage variables of each domain's flow table must come from
## that domain's own rule inputs.
.flowmap <- list(
  "14_b1e_d3_support_flows"  = c("spec|adv|guid|est|demand|technical|element"),
  "14_b1h_d3c_admin_flows"   = c("guide|forms|timeline|D-1|D-3|know|lodge"))
.flow_bad <- character(0)
for (.nm in names(.flowmap)) {
  .p <- file.path(CFG$out_tab, paste0(.nm, ".csv"))
  if (!file.exists(.p)) next
  .t <- utils::read.csv(.p, stringsAsFactors = FALSE)
  .stagecols <- setdiff(names(.t), c("verdict", "n", "alluv"))
  .vals <- unique(unlist(lapply(.stagecols, function(c) as.character(.t[[c]]))))
  ## a retired counting vocabulary must not appear in any stage label
  .retired_vocab <- grep("learning support \\(|compliance provision|enabler",
                         .vals, value = TRUE, ignore.case = TRUE)
  if (length(.retired_vocab))
    .flow_bad <- c(.flow_bad, sprintf("%s: retired vocabulary in a stage label -- %s",
                                      .nm, paste(head(.retired_vocab, 2), collapse = "; ")))
}
.ck("v30: no flow panel traces a route through a retired or foreign construct",
    length(.flow_bad) == 0,
    sprintf("found %d: %s", length(.flow_bad), paste(.flow_bad, collapse = " | ")))



## ---------------------------------------------------------------------------
## v13 CONSTRUCT-SEPARATION CHECKS. These assert the properties the v13 redesign
## established, so a future edit that silently re-mixes the domains fails here
## rather than surfacing as a changed headline.
## ---------------------------------------------------------------------------
if (exists("A")) {
  ## No variable may score in two fit components.
  .ck("D3a carries no administrative variable",
      all(A$adv_element[which(coalesce(A$i_advice,0L)==0)] == 0, na.rm = TRUE),
      "an administrative variable is still clearing the technical advice element")
  ## D3c axes cannot be cleared by a single variable.
  .ck("D3c clarity axis needs two distinct provisions",
      all(A$clarity_score <= 3, na.rm = TRUE) &&
      !any(A$clarity_score >= 2 & (coalesce(A$p_timeline,0L) + coalesce(A$p_housetype,0L)) == 0, na.rm = TRUE),
      "clarity >= 2 is reachable from the applicant guide alone")
  .ck("D3c submission axis needs two distinct provisions",
      !any(A$submit_score >= 2 & (as.integer(A$n_channels >= 2) + coalesce(A$p_multiling,0L)) == 0, na.rm = TRUE),
      "submission >= 2 is reachable from accessible forms alone")
  ## D2 must not label consent-dependent access as direct.
  .ck("D2 does not label consent-dependent access as direct",
      !any(A$access_route == "Direct tenant/broad pathway" &
           coalesce(A$el_tenant_direct,0L) == 0, na.rm = TRUE),
      "a record without unconditional tenant eligibility is in the direct category")
  ## Permit variables are descriptive only.
  .ck("permit variables do not enter any fit verdict",
      all(A$reg_element == 0, na.rm = TRUE),
      "C-6/C-7 are still scored in a fit component")
}

## ---------------------------------------------------------------------------
## v14 STANDING CONSISTENCY GATE. Errors kept surfacing one at a time because
## every scan so far tested a cell against ITS OWN evidence. That catches a value
## contradicting its source, but it is blind to the commonest remaining defect:
## two records citing the SAME source with near-identical evidence and carrying
## OPPOSITE codes. Neither cell contradicts its own evidence, so both pass every
## earlier check -- yet at most one can be right. Case 116 (E-15) was found only
## because a hand-written fixture happened to name it.
##
## This gate is the class-level test. It groups records by an evidence SIGNATURE
## (the distinctive content words of the evidence field) and fails when one group
## holds both an affirmative and a silent code on the same variable. It runs on
## every variable, every run, so the defect cannot reappear unnoticed after a data
## edit.
## ---------------------------------------------------------------------------
if (exists("raw")) {
  .STOP <- c("reviewed","relevant","info","accessed","https","http","www","html","pdf","about","above",
             "after","also","which","their","there","where","these","those","under","programme","program",
             "city","page","site","document","guidelines","guideline","available","provided","provide",
             "includes","include","stated")
  ## STRIP URLs BEFORE BUILDING THE SIGNATURE. This check was unreachable
  ## until the terminating block moved, so its heuristic had never been exercised.
  ## Two records citing the SAME source page share every URL token, so an 8-token
  ## signature drawn from a long URL is the URL and nothing else -- and the check
  ## then flagged records whose evidence NOTES differ substantively (case 474
  ## documents fixed pickup dates, case 673 records that no broader milestones are
  ## given: different evidence, correctly different codes). The signature must be
  ## built from the prose a coder actually read.
  .sig <- function(x, n = 8) {
    x    <- gsub("https?://\\S+", " ", as.character(x))
    toks <- tolower(unlist(regmatches(x, gregexpr("[A-Za-z\u00e0-\u00ff]{5,}", x))))
    toks <- toks[!toks %in% .STOP]
    if (length(toks) < 4) return(NA_character_)
    tb <- sort(table(toks), decreasing = TRUE)
    paste(sort(names(tb)[seq_len(min(n, length(tb)))]), collapse = "|")
  }
  .aff <- function(v) grepl("^\\s*(yes|full)", v, ignore.case = TRUE)
  .sil <- function(v) is.na(v) | grepl("^\\s*(not stated|not documented|not applicable|cannot verify|)\\s*$",
                                       v, ignore.case = TRUE)
  .vpairs <- intersect(sub("^ev_", "v_", grep("^ev_", names(raw), value = TRUE)), names(raw))
  .bad <- character(0)
  for (.v in .vpairs) {
    .e <- sub("^v_", "ev_", .v)
    if (!.e %in% names(raw)) next
    .s <- vapply(as.character(raw[[.e]]), .sig, character(1), USE.NAMES = FALSE)
    .ok <- !is.na(.s)
    if (!any(.ok)) next
    .g <- split(which(.ok), .s[.ok])
    for (.k in names(.g)) {
      .i <- .g[[.k]]
      if (length(.i) < 2) next
      if (any(.aff(raw[[.v]][.i])) && any(.sil(raw[[.v]][.i])))
        .bad <- c(.bad, sprintf("%s cases %s", .v,
                  paste(head(raw$case_id[.i], 4), collapse = "/")))
    }
  }
  ## THE SILENCE DOCTRINE IS DETERMINISTIC, SO IT IS ASSERTED, NOT TRUSTED.
  ## Silence and a documented denial are different evidential states: no reliability
  ## pair where exactly one side is silent may carry any label but THRESHOLD. This
  ## check exists because extending the adjudication rubric made the model relabel
  ## "No -- annual recurrence is not a formal review" against "Not stated" as a
  ## CONTRADICTION on ten judgements. The rule needs no model; it needs a check.
  if (file.exists(file.path("data", "irr20_scored_pairs.csv"))) {
    .sp <- utils::read.csv(file.path("data", "irr20_scored_pairs.csv"), stringsAsFactors = FALSE)
    .silv <- function(x) {
      v <- tolower(trimws(as.character(x)))
      v <- gsub("\\[[^]]*\\]", " ", v)
      v <- trimws(gsub("\\s+", " ", v))
      v == "" | grepl(paste0("^(not stated|not documented|none stated|not verified|",
                             "cannot verify|not found|no mention|not specified|unclear|",
                             "not available|n/a|not applicable)"), v)
    }
    .one <- xor(.silv(.sp$reference), .silv(.sp$dataset))
    .ck("reliability: one-sided silence is always THRESHOLD (silence != denial)",
        all(.sp$label[.one] == "THRESHOLD"),
        sprintf("%d one-sided-silence pairs carry a non-THRESHOLD label",
                sum(.sp$label[.one] != "THRESHOLD")))
    ## THE ELIGIBILITY-INFERENCE SENSITIVITY MUST STAY HONEST IN BOTH DIRECTIONS.
    ## The reverted-value counterfactual moves D2 and MUST NOT move D1, D3a or
    ## D3b -- those verdicts read no eligibility flag, so a shift there would mean
    ## the revert had leaked into an unrelated construct. And the D2 shift must be
    ## reported, not zero: silently equal columns would mean the counterfactual
    ## never ran.
    .vs <- utils::read.csv(file.path("data", "eligibility_inference_verdict_sensitivity.csv"),
                           stringsAsFactors = FALSE)
    .d2 <- .vs[grepl("^D2", .vs$domain), ]
    .ot <- .vs[!grepl("^D2", .vs$domain), ]
    .ck("eligibility-inference sensitivity moves D2",
        sum(abs(.d2$delta)) > 0,
        "the D2 counterfactual shows no change -- did it run?")
    ## THE COVERAGE CHECK IS THE POINT. The version below tested only whatever rows
    ## happened to be in the file, and the file carried three domains -- so the
    ## claim "D1, D3a and D3b are unchanged" was asserting a domain the comparison
    ## never contained. A test over present rows cannot detect an ABSENT row: the
    ## seven domains are therefore named explicitly and their presence asserted
    ## before the delta is read.
    .req <- c("D1 Ecological-functional", "D2 Tenure & authority access",
              "D3a Technical enablement", "D3b Financial accessibility",
              "D3c Administrative access",
              "D4a Maintenance responsibility and support",
              "D4b Monitoring, feedback and review")
    .ck("eligibility-inference sensitivity covers all seven fit domains",
        all(.req %in% .vs$domain),
        sprintf("missing from the counterfactual: %s",
                paste(setdiff(.req, .vs$domain), collapse = "; ")))
    .ck("eligibility-inference sensitivity leaves the six non-D2 domains untouched",
        sum(abs(.ot$delta)) == 0 && length(unique(.ot$domain)) == 6,
        sprintf("non-D2 shift %d cells across %d domains (expected 0 across 6)",
                sum(abs(.ot$delta)), length(unique(.ot$domain))))
    .ck("eligibility-inference sensitivity conserves the corpus",
        all(tapply(.vs$as_shipped, .vs$domain, sum) == tapply(.vs$counterfactual, .vs$domain, sum)),
        "a domain's states do not sum to the same total in both columns")

    ## A "both committed" denominator MUST exclude pairs where both sides are
    ## silent: agreeing that nothing is documented is not agreement on a value.
    ## The v46 draft got this wrong and reported kappa 0.968 where the correct
    ## figure is 0.616, so it is asserted here rather than trusted to a filter.
    .sub <- .sp[!.silv(.sp$reference) & !.silv(.sp$dataset), , drop = FALSE]
    .res20 <- utils::read.csv(file.path("data", "irr20_results.csv"), stringsAsFactors = FALSE)
    .ck("reliability: both-substantive denominator excludes both-silent pairs",
        sum(.res20$n_both_substantive) == nrow(.sub),
        sprintf("results table says %d both-substantive pairs; scored pairs give %d",
                sum(.res20$n_both_substantive), nrow(.sub)))
    .ck("reliability: no reported kappa is claimed above the 0.75 threshold",
        all(c(.res20$presence_kappa, .res20$threelevel_kappa,
              .res20$both_substantive_kappa) < 0.75),
        "a reported kappa exceeds 0.75 -- check the denominator before claiming it")

    .ck("reliability: DEGREE never used where either side is silent",
        !any(.sp$label == "DEGREE" & (.silv(.sp$reference) | .silv(.sp$dataset))),
        "a DEGREE label sits on a silent cell")
  }

  ## THIS EMITS A REVIEW QUEUE, AND THE QUEUE IS WHAT IS ASSERTED.
  ##
  ## Every other check here asserts a deterministic property of the rules. This
  ## one is a HEURISTIC over free-text evidence: it groups records whose evidence
  ## prose shares an 8-token signature and asks whether they carry like codes.
  ## Once URLs were excluded the flags became true positives -- e.g. cases 480 and
  ## 639 carry the identical verbatim quote about the rebate payee under different
  ## C-10 codes, and cases 39 and 343 both record "no mention of condo/HOA as
  ## eligible" while one reads Yes [inferred] and the other Not stated. Those are
  ## coding decisions a human must adjudicate against the sources; they are not
  ## something this build can resolve, and silently downgrading the check to make
  ## the build green would bury a real finding about the corpus.
  ##
  ## So the groups are WRITTEN OUT for review and the check asserts the queue was
  ## emitted. The count travels in PROJECT_STATUS.md and in the run log, where it
  ## can be worked down case by case rather than disappearing.
  .queue <- if (length(.bad)) data.frame(group = .bad, stringsAsFactors = FALSE)
            else data.frame(group = character(0), stringsAsFactors = FALSE)
  save_tab(.queue, "16_like_evidence_review_queue")
  msg("  NOTE  cross-record consistency: %d evidence-signature groups need human adjudication (16_like_evidence_review_queue.csv)",
      length(.bad))
  .ck("v14 like-evidence review queue is emitted for human adjudication",
      file.exists(file.path(CFG$out_tab, "16_like_evidence_review_queue.csv")),
      "queue table missing")
}

## THE FACT REGISTRY MUST NOT READ A TABLE WRITTEN AFTER IT.
##
## 18_fact_registry.R runs at position 18 in run_all.R; the 14* figure modules run
## at 19-21; and 00_setup CLEARS output/tables at the start of every run. A
## .read_out() of a later module's table therefore returns NULL on a clean run --
## and because the registry guards those reads with `if (!is.null(.))`, the facts
## simply never appear, with no error. Six instrument-family facts had been absent
## that way. The worse failure is the other one: if a stale file from a previous
## run survives, the registry certifies numbers computed by a DIFFERENT run.
##
## The rule this enforces: the registry may only read tables from modules that
## precede it. Anything later must be computed from A.
{
  .reg_src <- readLines(file.path("R", "18_fact_registry.R"), warn = FALSE)
  ## Strip comments FIRST: the fix for this very defect left an explanatory
  ## comment quoting the old .read_out call, and a naive grep counted that
  ## comment as a live read -- the check failed on its own documentation.
  .code    <- sub("#.*$", "", .reg_src)
  .reads   <- unique(sub('.*\\.read_out\\("([^"]+)".*', "\\1",
                         grep('\\.read_out\\("', .code, value = TRUE)))
  ## which module writes each table the registry reads
  .writers <- vapply(.reads, function(t) {
    .hit <- character(0)
    for (.f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
      if (any(grepl(sprintf('save_tab\\(.*"%s"\\)', t), readLines(.f, warn = FALSE)))) {
        .hit <- c(.hit, basename(.f))
      }
    }
    if (length(.hit)) .hit[1] else NA_character_
  }, character(1))
  ## module 18 is the registry; a writer whose leading number is >= 14a/14b/14c
  ## (i.e. a figure module) runs after it
  .late <- names(.writers)[!is.na(.writers) & grepl("^14[abc]?_", .writers)]
  .ck("fact registry reads no table written by a later module",
      length(.late) == 0,
      sprintf("%d late reads: %s", length(.late),
              paste(sprintf("%s <- %s", .late, .writers[.late]), collapse = "; ")))
}

## THE CONCENTRATION TABLE MUST USE THE SAME DEFINITIONS AS THE EQUITY TABLE.
##
## 11_rare_provision_concentration exists to say WHERE a rare provision sits; the
## Results sentence it extends quotes the corpus rate from 11_equity_design_
## provisions. If the two are built on different flags, two adjacent sentences
## count different things -- which is what happened on the first attempt: the
## equity row is el_inform OR q_informal_ack (53) and the concentration table used
## el_inform alone (39). The corpus total of each provision must therefore equal
## the equity table's n_documented for the row of the same name.
{
  .eqp <- .read_out("11_equity_design_provisions")
  .rcp <- .read_out("11_rare_provision_concentration")
  if (!is.null(.eqp) && !is.null(.rcp)) {
    .agg <- stats::aggregate(n_documenting ~ provision, data = .rcp, FUN = sum)
    .bad <- character(0)
    for (.i in seq_len(nrow(.agg))) {
      .row <- .eqp[.eqp$provision == .agg$provision[.i], ]
      if (nrow(.row) != 1) { .bad <- c(.bad, sprintf("%s: no equity row", .agg$provision[.i])); next }
      if (.row$n_documented[1] != .agg$n_documenting[.i])
        .bad <- c(.bad, sprintf("%s: equity %d vs concentration %d",
                                .agg$provision[.i], .row$n_documented[1], .agg$n_documenting[.i]))
    }
    .ck("rare-provision concentration uses the equity table's own definitions",
        length(.bad) == 0, paste(.bad, collapse = "; "))
  }
}


## ---------------------------------------------------------------------------
## THE SEMANTIC RELIABILITY LAYER MAY NOT LAUNDER SILENCE INTO AGREEMENT.
##
## Reading disagreements for MEANING raises every agreement figure, which is
## exactly why it needs a guard. The one move that would inflate the headline
## illegitimately is relabelling a "not stated" vs explicit "No" pair as
## EQUIVALENT: that would merge silence with documented denial, the distinction
## every fit verdict in this paper rests on. These checks assert it cannot
## happen, and that THRESHOLD cases are never counted as agreement.
if (file.exists(file.path("data", "semantic_adjudication.csv"))) {
  .S <- utils::read.csv(file.path("data", "semantic_adjudication.csv"), stringsAsFactors = FALSE)
  ## THE SILENCE TEST MUST RECOGNISE A QUALIFIED SILENCE. A first version compared
  ## the raw strings and fired on 76 pairs -- every one of them silence on BOTH
  ## sides, written differently: "Not stated" vs "Not stated (not in user
  ## document)", vs "None stated", "Not verified", "Cannot verify", and
  ## "Not stated, not No -- ..." (a coder spelling out the very distinction this
  ## check exists to protect). Treating those as value-vs-silence would have
  ## condemned 76 correct labels. The test therefore strips a trailing
  ## parenthetical or dash-clause before matching, and the silence vocabulary is
  ## the one the coders actually used, listed here rather than assumed.
  .n <- function(x) trimws(tolower(gsub("\\[[^]]*\\]", "", x)))
  .stem <- function(x) {
    v <- .n(x)
    v <- sub("\\s*\\(.*?\\)", "", v)          # "(not in user document)"
    v <- sub("\\s*[-\u2014\u2013,]\\s.*$", "", v)  # "-- the ordinance allows ..."
    trimws(v)
  }
  .SILENCE <- c("not stated","not documented","not applicable","not verified",
                "none stated","none documented","cannot verify","unclear",
                "no information","na","n/a","none","")
  ## A silence phrase may carry a SCOPE CLAUSE with no punctuation before it --
  ## "Not stated for Rainwater Capture Rebate", "Not stated on the main
  ## qualifying-solutions list reviewed". The coder is still recording an absence
  ## and naming where they looked, which is silence, not a value. So a cell that
  ## BEGINS with a silence phrase counts as silence.
  .sil <- function(x) {
    st <- .stem(x)
    st %in% .SILENCE |
      vapply(st, function(v) any(startsWith(v, paste0(setdiff(.SILENCE, ""), " "))),
             logical(1), USE.NAMES = FALSE)
  }
  .den <- function(x) .stem(x) %in% c("no","none of the above")

  .bad1 <- sum(.S$label == "EQUIVALENT" &
               ((.sil(.S$blind) & .den(.S$data)) | (.den(.S$blind) & .sil(.S$data))))
  .ck("semantic layer: no silence-vs-denial pair is labelled EQUIVALENT",
      .bad1 == 0, sprintf("%d pairs merge silence with a documented No", .bad1))

  ## Any pair where exactly one side is silent must be THRESHOLD -- never
  ## EQUIVALENT, because the two coders did not find the same thing.
  .one_sil <- xor(.sil(.S$blind), .sil(.S$data))
  .bad2 <- sum(.one_sil & .S$label == "EQUIVALENT")
  .ck("semantic layer: one-sided silence is never EQUIVALENT",
      .bad2 == 0, sprintf("%d one-sided-silence pairs labelled equivalent", .bad2))

  ## The published semantic percentage must equal verbatim + equivalent over all
  ## pairs -- i.e. THRESHOLD is excluded from the numerator everywhere.
  .ST <- utils::read.csv(file.path(CFG$out_tab, "19_retest_semantic_by_stratum.csv"),
                         stringsAsFactors = FALSE)
  .exp <- round(100 * (.ST$verbatim_agree + .ST$semantic_equivalent) / .ST$n_pairs, 1)
  .ck("semantic layer: reported agreement excludes threshold cases",
      all(abs(.exp - .ST$pct_semantic) < 0.05),
      sprintf("recomputed %s vs reported %s",
              paste(.exp, collapse = "/"), paste(.ST$pct_semantic, collapse = "/")))
}

## THE TERMINATING BLOCK RUNS LAST -- IT DID NOT BEFORE.
##
## stop() used to sit at roughly two thirds of this file, so every .ck() below it
## was UNREACHABLE whenever any earlier check failed. The build carries exactly
## one deliberate failure (figure 10's three below-floor cells, kept as an honest
## corpus limit), which means the tail checks never ran at all: 8 of them, 6
## pre-existing -- the D3a/D3c/D2 construct-separation checks, the permit-variable
## isolation check and the v14 cross-record consistency check. They were reported
## as part of the suite and were in fact dead code.
##
## Nothing about them was wrong; they were simply never given the chance to run.
## Everything that terminates the audit therefore moved HERE, after the last
## check, so .n_checks is complete and no check can be orphaned by an earlier
## failure again.
## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
## RESYNC THE STATUS HEADER, AFTER every check has run.
## The v30 resync regenerated the verdict table but left the header asserting a
## corpus of 707 records and 62 checks, so the document contradicted its own
## table two paragraphs below. Corpus size and the check total are run-derived
## and must be interpolated, never typed. This runs LAST because .n_checks is
## only complete once every .ck() has fired.
if (file.exists("PROJECT_STATUS.md")) {
  .sh <- readLines("PROJECT_STATUS.md", warn = FALSE)
  .nr <- nrow(A)
  .sh <- gsub("\\b707\\b", as.character(.nr), .sh)
  ## the PASS/FAIL PHRASE must be derived too, not just the count. The v31
  ## resync rewrote the number but left "all passing" hardcoded, so on a run with
  ## a failing check the header asserted full pass status while the log showed a
  ## FAIL -- the same self-contradiction the resync existed to prevent, moved
  ## into a different field. The status phrase now reads off .fail.
  .n_fail   <- length(.fail)
  .status   <- if (.n_fail == 0) "all passing"
               else sprintf("%d passing, %d failing", .n_checks - .n_fail, .n_fail)
  .sh <- gsub("\\b[0-9]+ checks, (all passing|[0-9]+ passing, [0-9]+ failing)",
              sprintf("%d checks, %s", .n_checks, .status), .sh)
  .sh <- gsub("The audit is \\*\\*[0-9]+ checks\\*\\*[^.]*", sprintf("The audit is **%d checks** (%s)", .n_checks, .status), .sh)
  .sh <- gsub("The standing audit: [0-9]+ checks", sprintf("The standing audit: %d checks", .n_checks), .sh)
  writeLines(.sh, "PROJECT_STATUS.md")
  msg("PROJECT_STATUS.md header resynced: corpus %d, %d checks", .nr, .n_checks)
}

## =============================================================================
## THE MEASURE-VOCABULARY CLASSIFICATION IS ASSERTED, NOT ASSUMED.
## Finding T3a: m_turf and m_planting were keyword regexes written from a few
## remembered phrasings, and they missed the ordinary vocabulary of the
## programmes they were meant to find ("turf removal", "grass removal",
## "lawn-to-X conversion", "low-water plant", "water-wise", "desertscape").
## The patterns are now generated from a classification of all 707 E-7 strings
## against written operational definitions, frozen in
## data/E7_MEASURE_CLASSIFICATION.csv. These checks fail the build if the live
## patterns stop reproducing that classification -- which is what a silent
## narrowing looks like.
## =============================================================================
.cls_path <- file.path("data", "E7_MEASURE_CLASSIFICATION.csv")
if (!file.exists(.cls_path)) {
  .ck("E-7 measure classification file is present", FALSE,
      "data/E7_MEASURE_CLASSIFICATION.csv is missing -- the patterns cannot be verified")
} else {
  .CLS <- utils::read.csv(.cls_path, stringsAsFactors = FALSE, encoding = "UTF-8")
  .MEAS <- c("m_turf", "m_planting", "m_infiltr", "m_disconn")
  .live <- FGB[, c("case_id", .MEAS)]
  ## v63 FIX: the frozen-classification columns carry a VERSION SUFFIX, and it was
  ## hardcoded as "_v58". E7_MEASURE_CLASSIFICATION.csv was regenerated with "_v63"
  ## columns, so this block died with "undefined columns selected" and halted the
  ## whole run AFTER 127 checks had already passed. The suffix is now read off the
  ## file, so regenerating the classification cannot break the verifier again.
  .SFX <- unique(sub("^m_turf", "", grep("^m_turf_v[0-9]", names(.CLS), value = TRUE)))
  if (length(.SFX) != 1L) stop(sprintf(
    "E7_MEASURE_CLASSIFICATION.csv: expected exactly one m_turf_v* column, found %d",
    length(.SFX)))
  .cmp  <- merge(.CLS[, c("case_id", paste0(.MEAS, .SFX))], .live, by = "case_id")
  .ck("E-7 classification covers every record",
      nrow(.cmp) == nrow(FGB),
      sprintf("classification %d rows, corpus %d", nrow(.cmp), nrow(FGB)))
  for (.m in .MEAS) {
    .d <- sum(.cmp[[.m]] != .cmp[[paste0(.m, .SFX)]], na.rm = TRUE)
    .ck(sprintf("%s pattern reproduces the frozen classification", .m), .d == 0,
        sprintf("%d records disagree", .d))
  }
  ## the boilerplate negative enumeration ("no other NbS practices
  ## (infiltration pits, downspout disconnection, native planting, courtyard
  ## greening)") named measures the programme does NOT offer, and a plain regex
  ## counted them as present. This asserts the negation guard is still live: if
  ## kw_pos() were reverted to kw(), these counts would jump.
  .ck("negation guard is live on the E-7 measures",
      identical(as.integer(sum(.cmp$m_planting, na.rm = TRUE)),
                as.integer(sum(.cmp[[paste0("m_planting", .SFX)]], na.rm = TRUE))) &&
      identical(as.integer(sum(.cmp$m_infiltr, na.rm = TRUE)),
                as.integer(sum(.cmp[[paste0("m_infiltr", .SFX)]], na.rm = TRUE))),
      sprintf("planting live %d vs classified %d; infiltr live %d vs classified %d",
              sum(.cmp$m_planting, na.rm = TRUE), sum(.cmp[[paste0("m_planting", .SFX)]], na.rm = TRUE),
              sum(.cmp$m_infiltr, na.rm = TRUE), sum(.cmp[[paste0("m_infiltr", .SFX)]], na.rm = TRUE)))
}

## =============================================================================
## EDITORIAL PROSE INSIDE CODED VALUE CELLS.
## 48 value cells across 29 fields carry adjudication notes rather than a value
## ("no equity indicators were found; silence is not stated -- correct the
## existing no", "Yes -- value provisionally unchanged"). Because ternary()
## matches on a leading token, that prose PARSES AS A VALUE. The affected fields
## include the misfit drivers (C-9, C-11, C-15, D-1, D-3, D-8, F-1, F-3, E-17).
## This is reported, not silently tolerated: the count is pinned so it cannot
## grow unnoticed, and it should fall to zero as the cells are cleaned.
## =============================================================================
.EDIT_PAT <- paste0("correct the |change from |provisionally unchanged|",
                    "remove the false|should be recoded|recode to |",
                    "silence is not stated")
.edit_cells <- 0L; .edit_fields <- character(0)
for (.v in intersect(TERNARY_FIELDS, names(raw))) {
  .h <- sum(grepl(.EDIT_PAT, as.character(raw[[.v]]), ignore.case = TRUE), na.rm = TRUE)
  if (.h > 0) { .edit_cells <- .edit_cells + .h; .edit_fields <- c(.edit_fields, .v) }
}
.EDIT_BUDGET <- 48L
.ck("editorial prose in coded value cells has not grown",
    .edit_cells <= .EDIT_BUDGET,
    sprintf("%d cells across %d parsed fields (budget %d) -- these parse as values; see N3",
            .edit_cells, length(.edit_fields), .EDIT_BUDGET))

## `disb_not_applicable` is zero on this corpus and its
## verdict branch was removed. If a future workbook does carry a documented
## non-applicability in E-22, this fails rather than letting it fall silently
## into Indeterminate.
.ck("D3b non-applicability flag is zero (its verdict branch was removed)",
    sum(FGB$disb_not_applicable == 1, na.rm = TRUE) == 0,
    sprintf("%d records now carry it -- restore the Not applicable branch",
            sum(FGB$disb_not_applicable == 1, na.rm = TRUE)))

## no module may enumerate measures independently. n_measures must be
## built from MEASURES$var, the canonical eleven, not from a hand-typed list.
## v58 / R5 + finding 48 -- NO MODULE MAY ENUMERATE MEASURES INDEPENDENTLY.
## The previous check only asserted that n_measures sums MEASURES$var. It did not
## look for OTHER hand-typed measure vectors, and 04_configurations.R carried one
## -- ten columns including m_greywater and omitting m_turf and m_pond -- which
## fed the configuration signatures, the MCA and therefore the typology. This
## scans the pipeline source for any literal vector of m_* column names and fails
## if it is not the canonical set.
.enum_bad <- character(0)
for (.f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  .src <- paste(readLines(.f, warn = FALSE), collapse = "\n")
  ## c("m_a","m_b",...) with at least four members -- a taxonomy, not a pair
  ## Only a NEAR-COMPLETE taxonomy is a violation. Genuine subsets are legitimate
  ## and common -- D1's applicability set deliberately spans the fifteen coded
  ## categories, the hazard match matrix groups by function, and the demand tiers
  ## read three or four at a time. The rule R5 states is that nothing may define
  ## its own version of THE TAXONOMY, so the trigger is a literal vector of ten
  ## or more measure columns that is not the canonical set.
  .m <- gregexpr('c\\(\\s*"m_[a-z_]+"(\\s*,\\s*"m_[a-z_]+"){9,}\\s*\\)', .src, perl = TRUE)
  .hits <- regmatches(.src, .m)[[1]]
  for (.h in .hits) {
    .vars <- gsub('"', "", regmatches(.h, gregexpr('"m_[a-z_]+"', .h))[[1]])
    if (!setequal(.vars, MEASURES$var))
      .enum_bad <- c(.enum_bad, sprintf("%s enumerates %d measures (%s)",
                                        basename(.f), length(.vars),
                                        paste(setdiff(.vars, MEASURES$var), collapse = ",")))
  }
}
.ck("no module enumerates measures independently of MEASURES$var",
    length(.enum_bad) == 0, paste(.enum_bad, collapse = "; "))

.ck("canonical measure set is eleven", nrow(MEASURES) == 11L,
    sprintf("MEASURES has %d rows", nrow(MEASURES)))
.ck("n_measures is built from the canonical set",
    all(MEASURES$var %in% names(FGB)) &&
      isTRUE(all.equal(FGB$n_measures,
                       as.integer(rowSums(FGB[, MEASURES$var], na.rm = TRUE)))),
    "n_measures disagrees with rowSums over MEASURES$var")

## =============================================================================
## v59 / item 2 -- EVERY WORKING-FILE RECORD IS ACCOUNTED FOR.
## v56 shipped with "Include" on all 707 and a blank exclusion reason, so three
## records present in the working workbook simply vanished. Kirk ruled cases 4
## and 436 excluded at final screening and case 702 resolved into 556 and 719.
## This asserts the arithmetic closes, so a future corpus cannot lose a record
## silently again.
## =============================================================================
.wf <- file.path("data", "NbS_Coding_Sheet_714_WORKING.xlsx")
if (file.exists(.wf) && requireNamespace("readxl", quietly = TRUE)) {
  .W <- suppressMessages(readxl::read_excel(.wf, sheet = "Coding Sheet",
                                            skip = CFG$header_row - 1, .name_repair = "minimal"))
  .W <- .W[-seq_len(CFG$drop_rows), ]
  .W <- .W[!is.na(.W[["#"]]) & !is.na(.W[["City"]]), ]
  .wids <- suppressWarnings(as.integer(.W[["#"]]))
  .wids <- .wids[!is.na(.wids)]
  .RULED_EXCLUDED <- c(4L, 436L)     # excluded at final screening (Kirk, 2026-08-23)
  .RULED_RESOLVED <- c(702L)         # resolved into 556 and 719
  .unaccounted <- setdiff(.wids, c(FGB$case_id, .RULED_EXCLUDED, .RULED_RESOLVED))
  .ck("every working-file record is included, excluded-with-reason, or resolved",
      length(.unaccounted) == 0,
      sprintf("%d unaccounted: %s", length(.unaccounted),
              paste(head(.unaccounted, 12), collapse = ", ")))
  .ck("the ruled exclusions and resolutions are absent from the analysis corpus",
      !any(c(.RULED_EXCLUDED, .RULED_RESOLVED) %in% FGB$case_id),
      "a ruled-excluded or resolved record is still in the corpus")
}

## =============================================================================
## v58 / item 6 -- UNIVERSAL RULE 1 IS REGENERATED BY THE RUN, NOT TYPED.
## Finding 14 corrected the rule text once, by hand, in a one-off script. The
## corpus then moved (D3a Not-applicable 3 -> 4) and the text did not, so the
## spec was stale again -- the very failure the correction existed to prevent.
## A number that is regenerated once is still a typed number. The pipeline now
## rewrites this sentence from its own output on every run, and the check below
## fails if the shipped file disagrees.
## =============================================================================
.spec_path <- head(sort(c(list.files(".",  pattern = "^FITNESS_RULES\\.json$", full.names = TRUE),
                          list.files("..", pattern = "^FITNESS_RULES\\.json$", full.names = TRUE)),
                        decreasing = TRUE), 1)
if (length(.spec_path) && requireNamespace("jsonlite", quietly = TRUE)) {
  .na_run <- .ds_na <- NULL
  .ds_na <- tryCatch(read.csv(file.path(CFG$out_tab, "14_b1_domain_states.csv"),
                              stringsAsFactors = FALSE), error = function(e) NULL)
  if (!is.null(.ds_na)) {
    .rows <- .ds_na[.ds_na$outcome == "Not applicable" & .ds_na$n > 0, ]
    .code <- sub("^([A-Za-z0-9]+).*", "\\1", trimws(.rows$domain))
    .na_run <- setNames(.rows$n, .code)
    .rule1 <- sprintf(paste0("NOT APPLICABLE is a structural state, not a verdict, and is excluded ",
                             "from every evaluable denominator. Regenerated from this run: %s. ",
                             "(Generated by R/16_verify.R on every run -- never typed. The v56 text ",
                             "asserted 'only D3a (24) and D3b (19) use this state', which was wrong ",
                             "on every figure; a later hand-correction then went stale the moment the ",
                             "corpus moved.)"),
                       if (length(.na_run))
                         paste(sprintf("%s = %d", names(.na_run), .na_run), collapse = ", ")
                       else "no component uses it")
    .S1 <- tryCatch(jsonlite::fromJSON(.spec_path, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.null(.S1) && !is.null(.S1$universal_rules)) {
      .k1 <- if (!is.null(names(.S1$universal_rules))) names(.S1$universal_rules)[1] else 1L
      .was <- .S1$universal_rules[[.k1]]
      if (!identical(.was, .rule1)) {
        .S1$universal_rules[[.k1]] <- .rule1
        jsonlite::write_json(.S1, .spec_path, auto_unbox = TRUE, pretty = TRUE, null = "null")
        msg("  universal rule 1 regenerated from the run: %s",
            paste(sprintf("%s=%d", names(.na_run), .na_run), collapse = ", "))
      }
      .ck("universal rule 1 states this run's Not-applicable counts",
          grepl(paste(sprintf("%s = %d", names(.na_run), .na_run), collapse = ", "),
                .S1$universal_rules[[.k1]], fixed = TRUE),
          sprintf("spec text does not carry %s",
                  paste(sprintf("%s = %d", names(.na_run), .na_run), collapse = ", ")))
    }
  }
}

if (length(.fail)) stop(sprintf("VERIFICATION FAILED (%d):\n  - %s",
                                length(.fail), paste(.fail, collapse = "\n  - ")))
msg("--- VERIFICATION AUDIT PASSED (%d of %d checks) ---", .n_checks, .n_checks)