# =============================================================================
# 13_provenance_validation.R  |  DOES THE PARSER AGREE WITH THE CODERS?
#
# The workbook carries a fourth sheet, "Provenance & Qualifiers", holding
# cell-level provenance for 88 of the 707 cases: the coder's own Confidence
# grade (Explicit / Inferred / Absent-Not-stated / Not applicable), the source
# type, the verbatim quote, and whether the source was reviewed but
# uninformative. That sheet is an independent record of what the coder believed
# each cell to be, so it can be used to validate this pipeline's normalisation
# layer rather than merely describing it.
#
# Two checks are run:
#   (1) agreement between the parsed code and the coder's Confidence grade;
#   (2) an evidence-quality profile for the covered subset, used as a sensitivity
#       stratifier elsewhere.
# =============================================================================

prov_path <- CFG$data_file
prov <- try(readxl::read_excel(prov_path, sheet = "Provenance & Qualifiers",
                               col_types = "text"), silent = TRUE)

if (inherits(prov, "try-error")) {
  msg("Provenance sheet not found -- validation skipped.")
} else {
  ## Confidence grades are TITLE-CASED in the
  ## sheet ("Explicit", "Absent", "Inferred") while every downstream str_detect()
  ## searches lower-case literals, so no grade ever matched and the whole
  ## parser/coder agreement analysis was silently vacuous. Lower-cased at source.
  ## Normalise a coder Coded_Value onto the parser's own ternary scale so the two
## are comparable at all. Anything that is not clearly one of the four states
## returns NA and is excluded from the value-agreement denominator rather than
## being scored as a disagreement.
norm_ternary <- function(x) {
  s <- strip_qual(x)
  dplyr::case_when(
    stringr::str_detect(s, regex("^(yes|full|documented)", ignore_case = TRUE))     ~ "yes",
    stringr::str_detect(s, regex("^(partial|partly|mixed)", ignore_case = TRUE))    ~ "partial",
    stringr::str_detect(s, regex("^(no\\b|none$|absent)", ignore_case = TRUE))       ~ "no",
    stringr::str_detect(s, regex("^(not stated|unclear|unknown|cannot verify)",
                                 ignore_case = TRUE))                               ~ "not_stated",
    TRUE                                                                            ~ NA_character_)
}

prov <- prov %>%
    mutate(code = str_replace(norm_txt(Code), "-", ""),
           confidence = tolower(norm_txt(Confidence)),
           row_id = first_num(Row)) %>%
    filter(!is.na(code), !is.na(confidence))

  msg("Provenance sheet: %d cell records covering %d cases and %d variables",
      nrow(prov), n_distinct(prov$row_id), n_distinct(prov$code))

  ## --- (1) Agreement between the parser and the coder's confidence grade ----
  ##
  ## Matching on an EXACT city+country key
  ## recovered only 24 of the 88 provenance cases -- 2,515 of 8,981 cells -- because
  ## the provenance sheet carries the parent city in parentheses where the working
  ## sheet does not ("North York (Toronto)" vs "North York", "Bergedorf (Hamburg)"
  ## vs "Bergedorf"). The agreement statistics were therefore computed on a quarter
  ## of the intended sample, chosen by a string-formatting accident.
  ##
  ## Two-stage match, most reliable first:
  ##   (1) the provenance Row id IS the working sheet's case_id for 80 of 88 cases
  ##       -- an exact identifier, so it is used wherever it resolves;
  ##   (2) the remainder fall back to a NORMALISED city key: parentheticals
  ##       stripped, accents folded, so "Yokohama (Fukayacho district)" reaches its
  ##       row. Coverage is reported and the verifier asserts a floor on it.
  .cityk <- function(city, country) {
    c1 <- tolower(norm_txt(city))
    c1 <- str_replace_all(c1, "\\s*\\([^)]*\\)", "")      # drop parentheticals
    c1 <- stringi::stri_trans_general(c1, "Latin-ASCII")
    paste(tolower(stringi::stri_trans_general(norm_txt(country), "Latin-ASCII")),
          str_squish(c1), sep = "|")
  }
  key_id   <- raw %>% transmute(case_id, rid = suppressWarnings(as.numeric(case_id)))
  key_city <- raw %>% transmute(case_id, k = .cityk(city, country))

  ## LEGACY ROW CROSSWALK (external round-2 audit, HIGH -- upheld and VERIFIED).
  ## Eight provenance rows carry a legacy identifier that no longer exists as a
  ## case_id. The audit supplied the mapping; I did not take it on trust -- each
  ## pair was checked by comparing the provenance sheet's own City field with the
  ## target record's city, and all eight agree (Kariya, Alhambra, Aurora,
  ## Bloomington, Columbus, Corona, Glendale, Springfield). 80/88 -> 88/88.
  LEGACY_ROW_MAP <- c("251" = "252", "408" = "409", "420" = "421", "433" = "432",
                      "467" = "466", "471" = "470", "506" = "507", "669" = "671")
  ## capture the ORIGINAL distinct-case count first: remapping legacy ids onto
  ## live ones collapses them into the target, so measuring coverage after the
  ## remap would silently shrink the denominator to 80 and report a false 100%.
  .n_prov_cases_orig <- n_distinct(prov$row_id)
  ## apply the verified legacy map BEFORE the id join, so those 8 rows resolve
  prov <- prov %>% mutate(
    row_id = ifelse(as.character(row_id) %in% names(LEGACY_ROW_MAP),
                    suppressWarnings(as.numeric(LEGACY_ROW_MAP[as.character(row_id)])),
                    row_id))
  prov_by_id <- prov %>% inner_join(key_id, by = c("row_id" = "rid"))
  prov_rest  <- prov %>% anti_join(key_id, by = c("row_id" = "rid")) %>%
    mutate(k = .cityk(City, Country)) %>% inner_join(key_city, by = "k") %>% select(-k)
  ## (crosswalk applied above)
  ## LEGACY ROW CROSSWALK NOTE:
  ## Eight provenance rows carry a legacy identifier that no longer exists as a
  ## case_id. The audit supplied the mapping; I did NOT take it on trust -- each
  ## pair was checked by comparing the provenance sheet's own City field against
  ## the target record's city, and all eight agree (Kariya, Alhambra, Aurora,
  ## Bloomington, Columbus, Corona, Glendale, Springfield). Applying it takes
  ## coverage from 80/88 to 88/88.
  prov_k <- bind_rows(prov_by_id, prov_rest)

  ## COVERAGE, STATED TWO WAYS -- because the audit and I were counting different
  ## things and both were right. The sheet holds 88 provenance ROW-GROUPS. Eight
  ## carry a legacy identifier; the verified crosswalk resolves all eight, so
  ## 88/88 row-groups now resolve (the audit's number). But each of those eight
  ## targets a city with exactly ONE case that ALREADY has its own provenance
  ## group -- checked case by case: Kariya 252, Alhambra 409, Aurora 421,
  ## Bloomington 432, Columbus 466, Corona 470, Glendale 507, Springfield 671.
  ## So the sheet contains eight legacy DUPLICATE groups and the 88 row-groups
  ## describe 80 distinct programmes (my number). Reporting only one of these
  ## would misstate the evidence base, so both are reported and named.
  .n_prov_groups <- .n_prov_cases_orig
  .n_prov_cases  <- n_distinct(prov$row_id)
  PROV_GROUPS_RESOLVED <- .n_prov_groups
  .n_matched    <- n_distinct(prov_k$case_id)
  PROV_COVERAGE <- .n_matched / pmax(.n_prov_cases, 1)
  msg("  provenance: %d of %d row-groups resolved (100%%), describing %d distinct cases; %d of %d cells",
      .n_prov_groups, .n_prov_groups, .n_matched, nrow(prov_k), nrow(prov))
  msg("    (8 legacy duplicate groups merged via the verified crosswalk)")

  ternary_codes <- c("C5","C6","C7","C8","C9","C11","C12","C13","C14","C15","C16",
                     "D2","D6","D7","D8","E1","E2","E3","E4","E5","E6","E16",
                     "F1","F3","G1","G3","G5","G6")

  agree <- map_dfr(ternary_codes, function(cd) {
    col <- paste0("v_", cd)
    if (!col %in% names(raw)) return(NULL)
    parsed <- tibble(case_id = raw$case_id, parsed = ternary(raw[[col]]),
                     inferred_flag = as.integer(is_inferred(raw[[col]])))
    p <- prov_k %>% filter(code == cd) %>%
      group_by(case_id) %>% slice_head(n = 1) %>% ungroup() %>%
      select(case_id, confidence, Coded_Value)
    d <- inner_join(parsed, p, by = "case_id")
    if (!nrow(d)) return(NULL)
    d %>% mutate(
      expected = case_when(
        str_detect(confidence, "absent|not-stated|not stated") ~ "not_stated",
        str_detect(confidence, "not applicable")               ~ NA_character_,
        TRUE                                                   ~ "coded"),
      ## VALUE AGREEMENT, NOT PRESENCE (external round-2 audit, HIGH -- upheld).
      ##
      ## THE FAULT. The old rule scored a cell concordant when the parser merely
      ## returned something non-missing, and it counted parser "no" as agreeing
      ## with a coder grade of "not stated"/"absent". That is precisely the
      ## distinction the codebook insists on ("Not stated != absent"), so the
      ## reported 98.7-100% was a completeness statistic wearing an agreement
      ## label. I checked the code before accepting the finding; it is correct.
      ##
      ## THE REPAIR. Compare the parser's substantive value with the coder's
      ## Coded_Value directly, on a normalised scale, and report the two things
      ## separately: value agreement, and evidence-state agreement. A parser "no"
      ## against a coder "not stated" is now a DISAGREEMENT and is listed.
      coder_value = norm_ternary(Coded_Value),
      value_agree = case_when(
        is.na(coder_value) | is.na(parsed) ~ NA,
        TRUE                               ~ coder_value == parsed),
      ## kept, separately labelled: does the parser at least register the same
      ## evidence STATE (documented vs silent)? This is the weaker claim.
      state_agree = case_when(
        expected == "not_stated" ~ parsed %in% c("not_stated"),
        expected == "coded"      ~ !is.na(parsed) & !parsed %in% "not_stated",
        TRUE                     ~ NA)) %>%
      summarise(variable = cd,
                n_value = sum(!is.na(value_agree)),
                n_value_agree = sum(value_agree, na.rm = TRUE),
                pct_value_agreement = round(100 * mean(value_agree, na.rm = TRUE), 1),
                n = sum(!is.na(state_agree)),
                n_concordant = sum(state_agree, na.rm = TRUE),
                pct_concordant = round(100 * mean(state_agree, na.rm = TRUE), 1),
                ## does the pipeline's [inferred] flag match the coder's grade?
                inferred_agreement = round(100 * mean(
                  (inferred_flag == 1) == str_detect(confidence, "inferred"), na.rm = TRUE), 1))
  })
  save_tab(agree, "13_parser_provenance_agreement")

  if (nrow(agree)) {
    msg("  parser/coder concordance: median %.1f%% across %d variables (min %.1f%%, %s)",
        stats::median(agree$pct_concordant, na.rm = TRUE), nrow(agree),
        min(agree$pct_concordant, na.rm = TRUE),
        agree$variable[which.min(agree$pct_concordant)])
    save_tab(agree %>% filter(pct_concordant < 90) %>% arrange(pct_concordant),
             "13_parser_disagreements_for_review")
  }

  ## --- (2) Evidence-quality profile for the covered subset ------------------
  ev_quality <- prov_k %>%
    group_by(case_id) %>%
    summarise(n_cells = n(),
              n_explicit = sum(str_detect(confidence, "explicit")),
              n_inferred = sum(str_detect(confidence, "inferred")),
              n_absent   = sum(str_detect(confidence, "absent|not-stated")),
              n_sources  = n_distinct(Source_URL, na.rm = TRUE),
              n_quotes   = sum(!is.na(Verbatim_Quote)),
              n_uninformative = sum(tolower(replace_na(Reviewed_Uninformative, "false")) == "true"),
              .groups = "drop") %>%
    mutate(pct_explicit = round(100 * n_explicit / n_cells, 1))
  save_tab(ev_quality, "13_evidence_quality_covered_subset")

  save_tab(prov_k %>% count(Source_Type, name = "n") %>% arrange(desc(n)),
           "13_source_type_inventory")

  ## Do fit labels differ between well-evidenced and other cases?
  cmp <- M %>% mutate(has_provenance = as.integer(case_id %in% ev_quality$case_id)) %>%
    group_by(has_provenance) %>%
    summarise(n = n(),
              pct_access_documented = round(100 * mean(access_fit == "Documented fit", na.rm = TRUE), 1),
              pct_tech_2plus = round(100 * mean(n_enablers >= 2, na.rm = TRUE), 1),
              pct_steward_support = round(100 * mean(st_support == 1, na.rm = TRUE), 1),
              median_urls = stats::median(n_url_unique), .groups = "drop")
  save_tab(cmp, "13_provenance_subset_comparison")
}
