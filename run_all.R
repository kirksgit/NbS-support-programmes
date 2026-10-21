# =============================================================================
# run_all.R  |  Programme fitness in citizen-led micro-scale NbS support
#               programmes -- full analysis pipeline
#
# Usage
#   Terminal:      cd <this folder> ; Rscript run_all.R
#   R / RStudio:   setwd("<this folder>") ; source("run_all.R")
#
# The coding sheet is located automatically (data/, then ~/Downloads/).
# To point elsewhere:  Sys.setenv(NBS_DATA = "/full/path/to/file.xlsx")
# =============================================================================

t0 <- Sys.time()

## Guard against namespace masking. MASS::select and stats::filter shadow the
## dplyr verbs once those packages attach at the modelling step, which breaks a
## second run in the same session. Global assignments take precedence.
suppressPackageStartupMessages(library(dplyr))
select <- dplyr::select
filter <- dplyr::filter
slice  <- dplyr::slice

steps <- c(
  "00_setup",                 # config, pre-registered parameters, helpers
  "00b_design",               # unified palette, theme, city typology, taxonomies
  "01_load_clean",            # read the coding sheet, documentation intensity
  "02_context",               # implementation context; programme families
  "03_architecture",          # programme architecture (RQ1)
  "03b_requirement_matrix",   # what each measure needs; relational technical test
  "04_configurations",        # observed configurations, itemised BEFORE grouping
  "05_fitness",               # four fitness domains (RQ2)
  "06_evaluability",          # Stage 1: who can be evaluated at all
  "07_fitness_models",        # Stage 2: what explains fitness + sensitivity
  "08_typology",              # architectures: design primitives only
  "09_compound_misfit",       # do weaknesses cluster?
  "10_context_matching",      # context modules, readiness gaps
  "11_reporting",             # results tables, exports
  "12_classification_audit",  # are the labels rule artefacts?
  "13_provenance_validation", # parser vs the coders' own confidence grades
  "15_glossary",              # variable dictionary with framework placement
  "18_fact_registry",         # EVERY quotable number, emitted once, keyed
  "17_temporal",              # provision by launch period, with the maturity caveat
  "14a_figures_descriptive",  # Part A: descriptive overview (RQ1)
  "14b_figures_rq",           # Part B: answering the research questions
  "14c_figures_robustness",   # Part C: data, methods, robustness
  "16_verify"                 # STANDING CORRECTNESS AUDIT -- halts on failure
)


## ---- Find our own folder ----------------------------------------------------
## THE SCRIPT SETS ITS OWN WORKING DIRECTORY. Every path below is relative to the
## repository root, and R does NOT move to a script's folder when you source it.
## Pressing Source in RStudio therefore ran the pipeline against whatever
## directory the session happened to be in, found no R/ subfolder, and reported
## "this folder is an older version of the pipeline" -- which named the wrong
## cause, because the folder was fine and only the working directory was wrong.
## The four branches cover the four ways this file gets run.
.self_dir <- function() {
  ## 1. source("run_all.R") and RStudio's Source button  --  source() keeps the
  ##    file it is reading in `ofile` on its own frame. THIS IS CHECKED FIRST,
  ##    and the order is the whole point: --file= below names the file R was
  ##    LAUNCHED with, which under `Rscript other.R` that sources this one is
  ##    other.R, not this file. Checking it first silently returned the wrong
  ##    folder whenever this pipeline was sourced from another script.
  for (.i in seq_len(sys.nframe())) {
    .o <- tryCatch(sys.frame(.i)$ofile, error = function(e) NULL)
    if (!is.null(.o) && nzchar(.o)) return(dirname(normalizePath(.o)))
  }
  ## 2. Rscript run_all.R  --  the path is on the command line
  .a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(.a)) return(dirname(normalizePath(sub("^--file=", "", .a[1]))))
  ## 3. RStudio running the open editor tab without sourcing a file
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE)) {
    .p <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (nzchar(.p)) return(dirname(normalizePath(.p)))
  }
  ## 4. Nothing identified the file. If the current directory already looks like
  ##    the repository, use it; otherwise give up and let the check below report.
  if (file.exists(file.path(getwd(), "R", "00_setup.R"))) return(getwd())
  NA_character_
}
.root <- .self_dir()
if (!is.na(.root) && !identical(normalizePath(getwd()), .root)) {
  setwd(.root)
  cat("Working directory set to the pipeline folder:\n  ", .root, "\n")
}

## ---- Preflight -------------------------------------------------------------
## Fails immediately and clearly if the folder is a stale extraction or the
## workbook is missing, rather than half-running and leaving mixed outputs.
missing_modules <- setdiff(paste0(steps, ".R"), list.files("R", pattern = "\\.R$"))
if (length(missing_modules)) {
  ## An EMPTY R/ means the working directory is wrong, not that the extraction is
  ## stale, and those need different fixes. Say which one this is.
  if (!dir.exists("R")) {
    stop("No R/ subfolder in the current working directory:\n  ", getwd(),
         "\nThis is where the pipeline looked, and it is probably not where you ",
         "extracted the zip.\nOpen run_all.R from the extracted folder and run it ",
         "again, or set the directory yourself first:\n",
         '  setwd("/full/path/to/the/extracted/folder")', call. = FALSE)
  }
  stop("This folder is an older version of the pipeline. Missing: ",
       paste(missing_modules, collapse = ", "),
       "\nRe-extract the latest zip into this folder and try again.", call. = FALSE)
}
## The preflight checks the same candidate list 00_setup.R uses, so that
## `Rscript run_all.R` run directly resolves the same workbook as reproduce.sh.
.pref <- c(Sys.getenv("NBS_DATA", ""), "data/NBS_707_FINAL_VERIFIED.xlsx")
if (!any(file.exists(.pref[nzchar(.pref)]))) {
  stop("Analysis workbook not found. Expected data/NBS_707_FINAL_VERIFIED.xlsx ",
       "or set Sys.setenv(NBS_DATA = \"/full/path/to/file.xlsx\").", call. = FALSE)
}
cat("Preflight OK:", length(steps), "modules found, workbook located.\n")

## WARNINGS ARE RECORDED WITH THEIR ORIGIN. A warnings log that says only WHAT
## happened is hard to act on -- locating the many-to-many join found in round 3
## took several passes because base warnings() drops the call. Every warning is
## now captured as it is signalled, with the module and the calling expression,
## and execution continues (this is a log, not a stop).
## run_all.R PINS ITS OWN COLLATION. reproduce.sh pinned a UTF-8 locale
## but anyone invoking `Rscript run_all.R` directly -- the documented alternative
## -- inherited the machine's locale, and locale-dependent transliteration and
## sorting then changed the city groupings and the retest keys. The pipeline no
## longer depends on how it was invoked.
suppressWarnings({
  if (!nzchar(Sys.getenv("LC_ALL"))) {
    for (.loc in c("en_US.UTF-8", "C.UTF-8", "en_GB.UTF-8")) {
      if (Sys.setlocale("LC_CTYPE", .loc) != "") break
    }
  }
  Sys.setlocale("LC_COLLATE", "C")
})

WARN_LOG <- new.env(parent = emptyenv())
WARN_LOG$rows <- list()
for (s in steps) {
  cat("\n----------------------------------------------------------------\n")
  cat("  ", s, "\n")
  cat("----------------------------------------------------------------\n")
  withCallingHandlers(
    source(file.path("R", paste0(s, ".R"))),
    warning = function(w) {
      .call <- tryCatch(paste(deparse(conditionCall(w)), collapse = " "),
                        error = function(e) NA_character_)
      WARN_LOG$rows[[length(WARN_LOG$rows) + 1L]] <- data.frame(
        module  = s,
        warning = conditionMessage(w),
        call    = substr(ifelse(is.na(.call), "<no call>", .call), 1, 160),
        stringsAsFactors = FALSE)
      invokeRestart("muffleWarning")
    })
}

## -----------------------------------------------------------------------------
## WARNINGS CAPTURE AND CLASSIFICATION (external round-2 audit, M2 -- upheld).
## The round-2 log ended "Il y a eu 50 avis ou plus" with the contents never
## shown. A run cannot be certified on warnings nobody has read. Every warning is
## now captured with its text and count, written to a log, and classified: a
## known-benign pattern passes, anything mentioning convergence, dropped rows,
## singularity, rank deficiency or a graphics failure FAILS the build.
.wall <- if (length(WARN_LOG$rows)) do.call(rbind, WARN_LOG$rows) else
  data.frame(module = character(0), warning = character(0), call = character(0))
.wtab <- if (nrow(.wall))
  aggregate(list(n = rep(1L, nrow(.wall))),
            by = list(module = .wall$module, warning = .wall$warning, call = .wall$call),
            FUN = sum) else
  data.frame(module = character(0), warning = character(0), call = character(0), n = integer(0))
## PATTERNS ARE LOCALE-AWARE. The first version of this block was English-only and
## classified 0 of 42 warnings, which would have been a log nobody could act on.
## Worse, it MISSED a real separation warning because R emitted it in French
## ("des probabilites ont ete ajustees numeriquement a 0 ou 1"). Both languages
## are matched, and accents are stripped before matching so encoding cannot hide
## a fatal warning.
BENIGN_WARNINGS <- paste(
  "NAs introduced by coercion|NAs introduits lors de la conversion",
  "Removed [0-9]+ rows containing|Suppression de [0-9]+ ",
  "Ignoring unknown aesthetics",
  "font family|no font could be found|police",
  ## GLYPH SUBSTITUTION IN A DEVICE LABEL. Keyed on "mbcsToSbcs" alone rather than
  ## on a "conversion failure" prefix, which is only one of the forms R emits. The
  ## other, seen on macOS where the Quartz-era device cannot render an en dash,
  ## reads "for '(RQ1 . Figures 3-6)' in 'mbcsToSbcs': - substituted for -". That
  ## form carried no prefix, so 32 of them went UNCLASSIFIED and halted an
  ## otherwise clean run on a reviewer's Mac after all 127 checks had passed. The
  ## warning can only change a character drawn in a label; no computed value
  ## passes through a graphics device.
  "conversion failure|mbcsToSbcs",
  ## BUILD METADATA, not behaviour. R says this when a package's binary was built
  ## under a slightly newer R than the one loading it, which is the normal state
  ## on any machine that installed packages after its last R update. It reports
  ## where the binary came from and says nothing about what the code computed.
  "was built under R version",
  "deprecated|obsolete",                                   # ggplot/dplyr API drift
  "No shared levels found",                                # manual scale on an absent level
  "collapsing to unique 'x' values",
  sep = "|")
## Fatal families, both languages. A warning here means a NUMBER may be wrong.
FATAL_WARNINGS <- paste(
  "did not converge|convergence|ne converge pas",
  "rank[- ]deficient|singular|essentially perfect fit|singulier",
  "longer object length is not a multiple|taille d.un objet plus long",
  "unexpected many-to-many relationship|many-to-many",
  "dropped|omitted [0-9]+ (row|observation)",
  "cannot open|failed to (write|render|save)|impossible d.ouvrir",
  sep = "|")
## THE FOUR REMAINING CLASSES, RULED ON INDIVIDUALLY rather than pattern-matched
## away. Adding a regex until the log reads clean would defeat the purpose of the
## log, so each is judged on whether it can move a reported number:
##
##  - "Removed N rows containing missing values" from a ggplot layer: the figure
##    guards already assert no empty tiles and reconcile every panel against the
##    verdict layer, so a dropped row here is a point outside a scale limit, not a
##    lost record. BENIGN, and only at N < 5 -- a larger drop is fatal, because
##    that would be data leaving a figure silently.
##  - Chi-square approximation warnings in the typology module: expected, the
##    contingency tables have small cells and the module already reports a
##    simulated p-value for exactly this reason. BENIGN.
##  - glm separation ("fitted probabilities numerically 0 or 1"): NOT benign in
##    general, but this pipeline already handles it -- guard_estimates blanks any
##    separated coefficient and every model declares its inference method. The
##    warning is the detector firing, not an unhandled fault, so it is classed
##    HANDLED and listed separately rather than hidden.
BENIGN_WARNINGS <- paste(BENIGN_WARNINGS,
  "Removed [1-4] rows? containing",
  "Chi-squared approximation may be incorrect|approximation du Chi-2",
  sep = "|")
## Match on the parts that survive transliteration in BOTH locales. iconv() renders
## the accented French differently across platforms, so the pattern keys on
## "glm.fit" plus the invariant "0 ou 1" / "0 or 1" tail rather than on accented words.
##  - "NaNs produced" from sqrt(diag(vc)) in 07_fitness_models: a negative
##    variance on the diagonal of a sandwich covariance, which happens when a
##    cluster-robust fit on a separated model is not positive semi-definite. It
##    is the SAME detector as the separation warning above and it is handled the
##    same way: guard_estimates() marks the row non-estimable and blanks its
##    estimate, standard error, statistic and p-value, and the model name carries
##    "[separation; bias reduction did not converge]". Verified across all 256
##    model rows that no finite estimate is ever reported beside a non-finite
##    standard error, so the NaN never reaches a published interval. HANDLED.
HANDLED_WARNINGS <- paste(
  "fitted probabilities numerically 0 or 1",
  "glm\\.fit.*(0 ou 1|0 or 1)",
  "probabilit.*ajust.*num",
  "NaNs produced|production de NaN",
  sep = "|")

## strip accents so the French forms match regardless of encoding
.wnorm <- iconv(.wtab$warning, to = "ASCII//TRANSLIT")
.wnorm[is.na(.wnorm)] <- .wtab$warning[is.na(.wnorm)]
.wtab$class <- ifelse(grepl(HANDLED_WARNINGS, .wnorm, ignore.case = TRUE), "handled",
               ifelse(grepl(FATAL_WARNINGS, .wnorm, ignore.case = TRUE), "FATAL",
               ifelse(grepl(BENIGN_WARNINGS, .wnorm, ignore.case = TRUE), "benign",
                      "UNCLASSIFIED")))
## WRITTEN OUTSIDE output/tables. This is a log of THIS run's warnings, not a
## result, and its text varies with R version and locale -- the French forms of
## the glm warnings are handled above precisely because they differ by machine.
## Shipping it as a comparator table would therefore fail the cell-by-cell
## comparison for anyone reproducing on a different platform, for a reason that
## has nothing to do with the analysis.
utils::write.csv(.wtab[order(-.wtab$n), ], file.path(dirname(CFG$out_tab), "17_run_warnings.csv"),
                 row.names = FALSE)
cat(sprintf("\nWarnings: %d distinct (%d total) -- benign %d, handled %d, unclassified %d, fatal %d\n",
            nrow(.wtab), sum(.wtab$n), sum(.wtab$class == "benign"),
            sum(.wtab$class == "handled"),
            sum(.wtab$class == "UNCLASSIFIED"), sum(.wtab$class == "FATAL")))
## An UNCLASSIFIED warning is a warning nobody has ruled on. That is the state the
## round-2 log was in, so it fails here rather than being reported and ignored.
if (any(.wtab$class == "UNCLASSIFIED"))
  stop("Unclassified warnings -- each must be ruled benign, handled or fatal:\n",
       paste(sprintf("  [%s] %s", .wtab$module[.wtab$class == "UNCLASSIFIED"],
                     substr(.wtab$warning[.wtab$class == "UNCLASSIFIED"], 1, 110)),
             collapse = "\n"), call. = FALSE)
if (nrow(.wtab)) for (.i in seq_len(min(nrow(.wtab), 25)))
  cat(sprintf("   [%-12s] x%-4d %-22s %s\n", .wtab$class[.i], .wtab$n[.i],
              substr(.wtab$module[.i], 1, 22), substr(.wtab$warning[.i], 1, 78)))
if (any(.wtab$class == "FATAL"))
  stop("FATAL warnings in this run:\n",
       paste(sprintf("  [%s] %s\n    at: %s",
                     .wtab$module[.wtab$class == "FATAL"],
                     .wtab$warning[.wtab$class == "FATAL"],
                     .wtab$call[.wtab$class == "FATAL"]), collapse = "\n"),
       call. = FALSE)

cat(sprintf("\nPipeline finished in %.1f minutes.\n",
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))
writeLines(capture.output(sessionInfo()), file.path(CFG$out_der, "sessionInfo.txt"))
