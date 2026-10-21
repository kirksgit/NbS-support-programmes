# =============================================================================
# 01_load_clean.R  |  Read the coding sheet, harmonise names, build the
#                     documentation-intensity block used in Stage-1 models.
# Produces: raw (one row per programme), doc (documentation measures)
# =============================================================================

msg("Reading %s", CFG$data_file)

sheet <- read_excel(CFG$data_file, sheet = CFG$data_sheet, skip = CFG$header_row - 1,
                    col_types = "text", .name_repair = "minimal")

## Drop the template/instruction row(s) sitting above the first real case
sheet <- sheet[-seq_len(CFG$drop_rows), ]
## Drop any fully empty rows and rows without a case id
sheet <- sheet[!is.na(sheet[["#"]]) & !is.na(sheet[["City"]]), ]
msg("  %d programme rows read", nrow(sheet))

nms <- names(sheet)

## ---- Map column names to variable codes -------------------------------------
code_of <- function(nm) {
  m <- str_match(nm, "^([A-G])-?([0-9]+)(i{1,2})?\\b")
  ifelse(is.na(m[, 1]), NA_character_, paste0(m[, 2], m[, 3], ifelse(is.na(m[, 4]), "", m[, 4])))
}
is_ev   <- str_detect(nms, "Evidence")
codes   <- code_of(nms)

new_nms <- nms
new_nms[!is.na(codes) & !is_ev] <- paste0("v_", codes[!is.na(codes) & !is_ev])
new_nms[!is.na(codes) &  is_ev] <- paste0("ev_", codes[!is.na(codes) & is_ev])
## identifiers and appended context columns
id_map <- c("#" = "case_id", "City" = "city_ss", "Country" = "country_ss",
            "Programme Name" = "prog_ss", "Population" = "city_population",
            "Owner-occupied (%)" = "owner_pct", "Renter (%)" = "renter_pct",
            "Tenure year" = "tenure_year", "Tenure data level" = "tenure_level",
            ## "Tenure source" and "Tenure grade" are no longer carried. Both were
            ## mapped here and neither reached a reported result: the grade was
            ## selected into the context frame and read by nothing after it, and the
            ## source name was never read at all. The tenure provenance a reader
            ## needs -- agency, table, geography, year, extract, access date and the
            ## link -- lives in data/TENURE_SOURCES.csv, which is its proper home.
            ## COLUMN ALIAS, NOT A DATA CHANGE. The 707-case v44 analysis file renamed
            ## the plain "Population" context column to "Tenure population" (the value
            ## is the same clean integer: 702 of 705 shared cases identical, 3 updated
            ## population figures). A-3 is NOT a substitute -- it carries prose such as
            ## "~300,000 (2022 census, Gran Jujuy)" and would not coerce to a number.
            ## Both names map to city_population so either vintage of the workbook loads.
            "Tenure population" = "city_population")
for (k in names(id_map)) new_nms[nms == k] <- id_map[[k]]
## Guard: exactly one column must resolve to city_population, or a rename has
## silently produced two sources of truth for the model's size covariate.
stopifnot(sum(new_nms == "city_population") == 1)
new_nms[str_detect(nms, "^Screening URL")]           <- "url_screening"
new_nms[str_detect(nms, "^Primary Official URL")]    <- "url_primary"
new_nms[str_detect(nms, "^Secondary Official URL")]  <- "url_secondary"

## de-duplicate any residual collisions
new_nms <- make.unique(new_nms, sep = "_dup")
names(sheet) <- new_nms

raw <- sheet %>%
  mutate(case_id = as.integer(first_num(case_id)),
         city    = norm_txt(city_ss),
         country = norm_txt(country_ss),
         programme = norm_txt(prog_ss),
         city_population = first_num(city_population),
         owner_pct = first_num(owner_pct),
         renter_pct = first_num(renter_pct),
         tenure_level = norm_txt(tenure_level))

vcols <- names(raw)[str_starts(names(raw), "v_")]
ecols <- names(raw)[str_starts(names(raw), "ev_")]
msg("  %d coded variables, %d evidence columns", length(vcols), length(ecols))

# =============================================================================
# DOCUMENTATION INTENSITY  (Stage-1 evaluability predictors + sensitivity)
# =============================================================================
ev_mat <- raw[, ecols, drop = FALSE]

## URL EXTRACTION FIX (found while generating source links for manual spot-checking).
## The character class did NOT exclude newlines or quote marks, so a URL followed by
## a newline and the verbatim quote swallowed the quote's first word: one cited page
## appearing 27 times in a record's evidence produced 27 "unique" URLs that differed
## only in the trailing text. n_url_unique was inflated corpus-wide (median 35 vs a
## true median of 2), and it feeds doc_depth -- a predictor in every stage-1
## evaluability model -- and the compound-misfit documentation tertiles.
url_of_row <- function(r) {
  u <- unlist(str_extract_all(paste(r, collapse = " || "),
                              '[Hh][Tt][Tt][Pp][Ss]?://[^[:space:]|\\]\\),;"\']+'))
  u <- str_remove(u, "[.,;:]+$")
  unique(tolower(u))
}
url_lists <- apply(ev_mat, 1, url_of_row)

doc <- tibble(
  case_id      = raw$case_id,
  n_url_unique = vapply(url_lists, length, integer(1)),
  n_domains    = vapply(url_lists, function(u)
    length(unique(str_extract(u, "(?<=://)[^/]+"))), integer(1)),
  n_quotes     = apply(ev_mat, 1, function(r) sum(str_count(paste(r, collapse = " "), '"[^"]{5,}"'))),
  ev_chars     = apply(ev_mat, 1, function(r) sum(nchar(replace_na(r, "")))),
  n_cannot_verify = apply(ev_mat, 1, function(r)
    sum(str_detect(replace_na(r, ""), regex("cannot verify|no relevant info", ignore_case = TRUE))))
)

## Share of the 74 coded cells that are "Not stated" (documentation gap density)
notstated_share <- apply(raw[, vcols], 1, function(r)
  mean(str_detect(strip_qual(r), regex("^(not stated|none/not stated)", ignore_case = TRUE)),
       na.rm = TRUE))
inferred_share  <- apply(raw[, vcols], 1, function(r) mean(is_inferred(r), na.rm = TRUE))

doc <- doc %>%
  mutate(notstated_share = as.numeric(notstated_share),
         inferred_share  = as.numeric(inferred_share),
         doc_depth       = as.numeric(scale(log1p(n_url_unique))))

msg("  documentation: median %d unique URLs, mean 'not stated' share %.2f, mean inferred share %.3f",
    stats::median(doc$n_url_unique), mean(doc$notstated_share), mean(doc$inferred_share))

## Case-level [inferred] flags for every coded variable (used in sensitivity S2)
inf_flags <- raw[, vcols] %>% mutate(across(everything(), ~ as.integer(is_inferred(.x))))
names(inf_flags) <- str_replace(names(inf_flags), "^v_", "inf_")
inf_flags$case_id <- raw$case_id

save_tab(doc, "01_documentation_intensity")
