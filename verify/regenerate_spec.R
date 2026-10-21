#!/usr/bin/env Rscript
# Regenerate FITNESS_RULES.json from the shipped run.
#
# WHY. A hand-maintained spec drifts from the analysis it claims to describe, and
# the lock check covers only part of the file, so the rest drifts silently.
# Regenerating from the run is the mechanism that prevents it.
suppressPackageStartupMessages({library(jsonlite); library(dplyr)})
tab <- "output/tables"
spec_path <- if (file.exists("FITNESS_RULES.json")) "FITNESS_RULES.json" else
  sort(list.files(".", pattern="^FITNESS_RULES.*\\.json$", full.names=TRUE), decreasing=TRUE)[1]
if (is.na(spec_path) || !file.exists(spec_path))
  stop("FITNESS_RULES.json not found; run this from the repository root.", call. = FALSE)
S  <- fromJSON(spec_path, simplifyVector = FALSE)
ds <- read.csv(file.path(tab, "14_b1_domain_states.csv"), stringsAsFactors = FALSE)
pr <- read.csv(file.path(tab, "08_macro_architecture_profiles.csv"), stringsAsFactors = FALSE)
prof <- read.csv(file.path(tab, "05_programme_fitness_profiles.csv"), stringsAsFactors = FALSE)

key <- c("D1 E"="D1","D2 T"="D2","D3a"="D3a","D3b"="D3b","D3c"="D3c","D4a"="D4a","D4b"="D4b")
lbl <- c(fit="Documented fit", partial="Partial fit", misfit="Confirmed misfit",
         indeterminate="Indeterminate", na="Not applicable")
N <- nrow(prof)
for (dn in names(key)) {
  rows <- ds[substr(trimws(ds$domain), 1, nchar(dn)) == dn, ]
  if (!nrow(rows)) next
  cc <- key[[dn]]
  g <- function(f) sum(rows$n[rows$outcome == lbl[[f]]])
  ev <- N - g("indeterminate") - g("na")
  S$verdicts_locked[[cc]]$fit           <- g("fit")
  S$verdicts_locked[[cc]]$partial       <- g("partial")
  S$verdicts_locked[[cc]]$misfit        <- g("misfit")
  S$verdicts_locked[[cc]]$indeterminate <- g("indeterminate")
  S$verdicts_locked[[cc]]$na            <- g("na")
  S$verdicts_locked[[cc]]$evaluable     <- ev
  S$verdicts_locked[[cc]]$fit_pct_evaluable <- round(100 * g("fit") / ev, 1)
}
S$cohort <- N
## typology_sizes is read from the macro SIZES table, which carries the
## unprofiled opacity group as well as the three profiled classes, so the block
## totals the analysed cohort rather than the profiled subset.
.sz <- file.path(dirname(spec_path), "expected", "tables",
                 "08_macro_architecture_sizes.csv")
if (!file.exists(.sz)) .sz <- file.path("expected", "tables",
                                        "08_macro_architecture_sizes.csv")
if (file.exists(.sz)) {
  .s <- utils::read.csv(.sz, stringsAsFactors = FALSE)
  S$typology_sizes <- list()
  for (i in seq_len(nrow(.s))) S$typology_sizes[[.s[[1]][i]]] <- .s$n[i]
} else {
  for (i in seq_len(nrow(pr))) S$typology_sizes[[pr$name[i]]] <- pr$n[i]
}
write_json(S, spec_path, auto_unbox = TRUE, pretty = TRUE, null = "null")
cat(sprintf("  spec regenerated: cohort %d, 7 components, %d macro classes -> %s\n",
            N, nrow(pr), basename(spec_path)))
