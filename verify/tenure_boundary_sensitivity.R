## =============================================================================
## tenure_boundary_sensitivity.R  --  does the D2 verdict depend on where the
## tenure bands are cut?
##
## WHY THIS EXISTS. tenure_class bands a continuous figure, renter_pct, at 35 and
## 55, and d2_required reads that band, so a record sitting on a cut has its D2
## requirement decided by a rounding convention rather than by its documents.
## Nine of the 707 records sit EXACTLY on a cut and 198 sit within three points of
## one, which is enough exposure that a reviewer is entitled to ask what the cuts
## are doing. This script answers it by moving every on-cut record to the other
## side and recomputing the whole fit layer, so the reported movement is the
## worst case rather than an estimate.
##
## Run from the repository root:  Rscript verify/tenure_boundary_sensitivity.R
## =============================================================================
suppressPackageStartupMessages(library(dplyr))
select <- dplyr::select; filter <- dplyr::filter; slice <- dplyr::slice
FC   <- c("fit_D1","fit_D2","fit_D3a","fit_D3b","fit_D3c","fit_D4a","fit_D4b")
CUTS <- c(35, 55)

## Each arm runs in its own environment. source() defaults to local = FALSE and
## would otherwise populate the global environment, so the second arm would read
## the first arm's objects and the comparison would be vacuous.
run_arm <- function(flip) {
  e <- new.env(parent = globalenv())
  for (nm in c("select","filter","slice")) assign(nm, get(nm), envir = e)
  for (s in c("00_setup","00b_design","01_load_clean"))
    suppressMessages(suppressWarnings(capture.output(
      source(file.path("R", paste0(s, ".R")), local = e), file = nullfile())))
  if (flip) {
    v <- e$raw$renter_pct
    i <- which(pmin(abs(v - CUTS[1]), abs(v - CUTS[2])) < 0.001)
    e$raw$renter_pct[i] <- ifelse(v[i] >= CUTS[2], CUTS[2] - 0.1,
                           ifelse(v[i] >= CUTS[1], CUTS[1] - 0.1, CUTS[1] + 0.1))
    cat(sprintf("  moved %d on-cut records to the other side\n", length(i)))
  }
  for (s in c("02_context","03_architecture","03b_requirement_matrix",
              "04_configurations","05_fitness"))
    suppressMessages(suppressWarnings(capture.output(
      source(file.path("R", paste0(s, ".R")), local = e), file = nullfile())))
  e$A
}
cat("baseline arm\n"); A0 <- run_arm(FALSE)
cat("flipped arm\n");  A1 <- run_arm(TRUE)

d <- pmin(abs(A0$renter_pct - CUTS[1]), abs(A0$renter_pct - CUTS[2]))
cat("\nexposure\n")
for (k in c(0.001, 1, 2, 3, 5))
  cat(sprintf("  within %-5.3g pp of a cut: %3d of %d\n", k, sum(d < k, na.rm = TRUE), nrow(A0)))

out <- list()
for (k in FC) for (l in sort(union(unique(A0[[k]]), unique(A1[[k]])))) {
  a <- sum(A0[[k]] == l, na.rm = TRUE); b <- sum(A1[[k]] == l, na.rm = TRUE)
  out[[length(out) + 1]] <- data.frame(component = k, level = l,
                                       baseline = a, flipped = b, diff = b - a)
}
res <- do.call(rbind, out)
cat("\nverdict cells that move\n")
print(res[res$diff != 0, ], row.names = FALSE)
cat(sprintf("\ntotal verdict cells moved: %d\n", sum(abs(res$diff))))

cnt <- function(A, lv) rowSums(as.data.frame(lapply(FC, function(k) A[[k]] == lv)), na.rm = TRUE)
m0 <- cnt(A0, "Confirmed misfit"); m1 <- cnt(A1, "Confirmed misfit")
f0 <- cnt(A0, "Documented fit");   f1 <- cnt(A1, "Documented fit")
hd <- data.frame(
  quantity = c("records with at least one confirmed misfit",
               "records with three or more confirmed misfits",
               "programmes documented fit on all seven"),
  baseline = c(sum(m0 >= 1), sum(m0 >= 3), sum(f0 == 7)),
  flipped  = c(sum(m1 >= 1), sum(m1 >= 3), sum(f1 == 7)))
hd$diff <- hd$flipped - hd$baseline
cat("\nheadline quantities\n"); print(hd, row.names = FALSE)

dir.create("output/sensitivity", recursive = TRUE, showWarnings = FALSE)
utils::write.csv(rbind(
  data.frame(quantity = paste0("verdict: ", res$component, " ", res$level),
             baseline = res$baseline, flipped = res$flipped, diff = res$diff),
  hd), "output/sensitivity/19_tenure_boundary_sensitivity.csv",
  row.names = FALSE, fileEncoding = "UTF-8")
cat("\nwritten: output/sensitivity/19_tenure_boundary_sensitivity.csv\n")
