# =============================================================================
# 06_evaluability.R  |  STAGE 1 -- MODELLING EVALUABILITY
# Documentation is not a nuisance. Whether a programme CAN be assessed is itself
# an accountability outcome, and conditioning Stage-2 on evaluability without
# modelling it would confuse opacity with poor design.
# =============================================================================

suppressPackageStartupMessages({ library(sandwich); library(lmtest); library(broom) })

## Country-clustered robust SEs (city clustering is not feasible: >99% of cities
## contribute a single programme -- see 02_context.R diagnostic).
## H1 FIX, third part (external audit). The separation guard was applied at
## individual CALL SITES, so eleven tidy_cl() calls each had to remember it and
## several did not -- which is why a coefficient of -14.19 with p = 8e-61 reached
## a sensitivity table. The guard now lives INSIDE tidy_cl, so no call site can
## omit it, and the covariance and cluster count are recorded on every row rather
## than left for a reader to assume.
##
## `separation_limit` is the SAME threshold is_separated() uses to detect
## separation (|b| > 8). A coefficient past it, or with an SE at the numerical
## signature of a zero cell, is marked non-estimable and its inference blanked.
## v37b: is_separated() is defined in 07_fitness_models.R, which run_all.R sources
## AFTER this file -- so calling it here would fail on an undefined function. It is
## defined once here, with the SAME rule (|b| > 8, or a fitted probability pinned
## at 0 or 1), and 07 redefines it identically when it loads. Keeping the two
## definitions textually identical matters: a separation screen that differed
## between the two model blocks would be worse than not having one here at all.
if (!exists("is_separated")) {
  is_separated <- function(m) {
    fv <- fitted(m)
    any(abs(stats::coef(m)) > 8, na.rm = TRUE) || any(fv < 1e-8 | fv > 1 - 1e-8)
  }
}

tidy_cl <- function(model, cluster, label, separation_limit = 8) {
  V  <- sandwich::vcovCL(model, cluster = cluster, type = "HC0")
  ct <- lmtest::coeftest(model, vcov. = V)
  tibble(model = label,
         term = rownames(ct), estimate = ct[, 1], std.error = ct[, 2],
         statistic = ct[, 3], p.value = ct[, 4]) %>%
    mutate(or = exp(estimate),
           or_lo = exp(estimate - 1.96 * std.error),
           or_hi = exp(estimate + 1.96 * std.error),
           covariance = "cluster-robust (HC0)",
           n_clusters = dplyr::n_distinct(cluster),
           ## v37b: the SE side of this test was incomplete -- it caught an SE that
           ## had COLLAPSED toward zero but not one that was non-finite or had
           ## blown up. Both are now caught.
           ##
           ## THIS GUARD IS NOT WHAT FIXED THE CROSS-INSTALL DIVERGENCE, and an
           ## earlier version of this comment wrongly claimed it was. The divergence
           ## (std.error 0.97 vs 3.31 on lead_bodyOther; or_hi 6.4 vs 618) sat at
           ## SEs of 1-3, far below any threshold here, so this guard never fired on
           ## those rows and adding it changed nothing about them. What fixed them is
           ## the Firth correction applied at the fitting step below: with the
           ## separated model bias-reduced, the covariance is identifiable and both
           ## installs return the same SE (0.57, or_hi 3.16). The guard is retained
           ## because a non-finite or exploded SE is still not an interval and must
           ## not be reported as one -- but it is a backstop, not the fix.
           non_estimable = !is.finite(estimate) | abs(estimate) > separation_limit |
                           !is.finite(std.error) |
                           (is.finite(std.error) & std.error < 1e-4) |
                           (is.finite(std.error) & std.error > separation_limit),
           separation_flag = non_estimable,
           across(c(estimate, std.error, statistic, p.value, or, or_lo, or_hi),
                  ~ ifelse(non_estimable, NA_real_, .x)))
}

ev_predictors <- c("pop_log", "doc_depth", "prog_age_c", "lead_body", "delivery_arch",
                   "n_measures", "region")

D <- A %>%
  mutate(prog_age_c = ifelse(is.na(prog_age), stats::median(prog_age, na.rm = TRUE), prog_age),
         prog_age_missing = as.integer(is.na(prog_age)),
         lead_body     = factor(lead_body),
         delivery_arch = factor(delivery_arch),
         ## pooled versions used throughout Stage 1 and Stage 2: sparse regions
         ## and delivery forms produce quasi-separation in binary models
         region_mod = { r <- as.character(region); tb <- table(r)
                        r[r %in% names(tb)[tb < 25]] <- "Other regions"
                        stats::relevel(factor(r), ref = "North America") },
         delivery_arch3 = case_when(
           delivery_arch %in% c("Coordinated (named office)", "Multi-department",
                                "Multi-department + partners") ~ "Coordinated/multi-department",
           delivery_arch == "Partnered delivery"               ~ "Partnered delivery",
           TRUE                                                ~ "Single body"),
         delivery_arch3 = stats::relevel(factor(delivery_arch3), ref = "Single body"))

ev_outcomes <- c(ev_eco = "Ecological-functional", ev_access = "Tenure & authority access",
                 ev_tech = "Technical enablement", ev_admin = "Administrative")

eval_models <- list(); eval_tab <- list()
for (o in names(ev_outcomes)) {
  d <- D %>% filter(!is.na(.data[[o]]))
  if (length(unique(d[[o]])) < 2 || min(table(d[[o]])) < CFG$min_events) {
    msg("  [skip] %s: outcome too rare to model (min cell = %d)",
        ev_outcomes[[o]], min(table(d[[o]])))
    next
  }
  f <- as.formula(paste(o, "~ pop_log + doc_depth + prog_age_c + prog_age_missing +",
                        "lead_body + delivery_arch3 + n_measures + region_mod"))
  m <- glm(f, data = d, family = binomial())
  ## v37b: SEPARATED MODELS ARE FIRTH-CORRECTED HERE TOO.
  ##
  ## This block fitted a plain glm() and never checked for separation, unlike
  ## 07_fitness_models.R which has corrected for it since v13. The "Technical
  ## enablement" model separates ("fitted probabilities numerically 0 or 1"), so
  ## its cluster-robust covariance is near-singular and its standard errors are
  ## decided by the last bits of a matrix decomposition rather than by the data:
  ## running the SAME code under R 4.5.3 linked to OpenBLAS and R 4.6.0 linked to
  ## the reference LAPACK gave std.error NaN vs 2.46 on one term and 0.97 vs 3.31
  ## on another (or_hi 6.4 vs 618). Point estimates agreed to 1e-6 -- the fit was
  ## stable, the variance was not. Neither answer was more correct; both were
  ## noise. Applying the same bias reduction the other model block already uses
  ## makes the variance identifiable, so the result is a property of the data
  ## rather than of the BLAS the reader happens to have linked.
  if (is_separated(m) && requireNamespace("brglm2", quietly = TRUE)) {
    .mb <- try(suppressWarnings(
      glm(f, data = d, family = binomial(), method = brglm2::brglmFit,
          type = "AS_mean", maxit = 1000, slowit = 0.5)), silent = TRUE)
    if (!inherits(.mb, "try-error") && isTRUE(.mb$converged)) {
      m <- .mb
      msg("  %s: separation detected -- Firth-corrected (brglm2)", ev_outcomes[[o]])
    } else {
      msg("  %s: separation detected and bias reduction did NOT converge -- inference blanked",
          ev_outcomes[[o]])
    }
  }
  eval_models[[o]] <- m
  eval_tab[[o]] <- tidy_cl(m, cluster = d$country, label = ev_outcomes[[o]]) %>%
    mutate(n = nobs(m), outcome = o)
}
## EVERY OUTCOME CAN BE SKIPPED, AND THEN THIS BLOCK HAS NO INPUT.
## The loop above skips an outcome whose rarer level falls below CFG$min_events, and
## this line assumed at least one model survived: bind_rows(list()) returns a frame
## with NO COLUMNS, so group_by(model) aborted the whole run with "Column `model` is
## not found" -- a message that says nothing about the real cause. On the v52
## source-revalidated corpus all four indeterminacy outcomes fell below the floor
## (min cells 15, 3, 17, 1) because the restoration review moved ~2,500 cells back to
## substantive values, which is exactly the direction that makes INDETERMINACY rare.
## An empty model set is a legitimate finding about the corpus, not a crash: it means
## indeterminacy is too rare to model. The table is emitted empty with its schema so
## downstream readers and the acceptance diff still find it.
if (length(eval_tab)) {
  eval_out <- bind_rows(eval_tab) %>% group_by(model) %>%
    mutate(p_bh = bh(p.value)) %>% ungroup()
} else {
  msg("  NOTE  every evaluability outcome fell below the %d-event floor -- no model is estimable on this corpus; emitting the empty table with its schema",
      CFG$min_events)
  eval_out <- tibble(model = character(0), term = character(0), estimate = numeric(0),
                     std.error = numeric(0), statistic = numeric(0), p.value = numeric(0),
                     or = numeric(0), or_lo = numeric(0), or_hi = numeric(0),
                     covariance = character(0), n_clusters = integer(0),
                     n = integer(0), outcome = character(0), p_bh = numeric(0))
}
save_tab(eval_out, "06_stage1_evaluability_models")

## Descriptive companion: documentation coverage by country and by lead body
doc_cov <- D %>%
  group_by(country) %>%
  summarise(n = n(),
            median_urls = stats::median(n_url_unique),
            mean_notstated = mean(notstated_share),
            mean_inferred  = mean(inferred_share),
            pct_evaluable_all = 100 * mean(n_domains_evaluable == 5), .groups = "drop") %>%
  arrange(desc(n))
save_tab(doc_cov, "06_documentation_coverage_by_country")

msg("Stage-1 evaluability models fitted for %d domains.", length(eval_models))
