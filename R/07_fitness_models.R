# =============================================================================
# 07_fitness_models.R  |  STAGE 2 -- EXPLAINING FIT AMONG EVALUABLE CASES
# One theoretically specified predictor set per domain (no indiscriminate
# variable search), country-clustered inference, and ONE pre-registered
# interaction per domain, corrected together as a single test family.
# =============================================================================

suppressPackageStartupMessages({ library(nnet); library(MASS); library(broom) })
select <- dplyr::select   # MASS masks select()

M <- D %>%
  mutate(
    bldg_integrated = as.integer(m_greenroof == 1 | m_facade == 1),
    renter_c   = as.numeric(scale(renter_pct)),
    density_l  = log10(pmax(density, 100)),
    density_c  = as.numeric(scale(ifelse(is.na(density_l), stats::median(density_l, na.rm = TRUE), density_l))),
    pop_c      = as.numeric(scale(pop_log)),
    n_meas_c   = as.numeric(scale(n_measures)),
    region     = relevel(factor(region), ref = "North America"),
    funding_horizon = case_when(funding_horizon %in% c("Fixed-term", "Pilot") ~ "Fixed-term/pilot",
                                is.na(funding_horizon) ~ "Not stated", TRUE ~ funding_horizon),
    funding_horizon = relevel(factor(funding_horizon), ref = "Not stated"),
    funding_source  = relevel(factor(replace_na(funding_source,  "Not stated")), ref = "Municipal budget"),
    climate_grp     = relevel(factor(replace_na(climate_grp, "Temperate humid")), ref = "Temperate humid"),
    lead_body       = relevel(factor(replace_na(as.character(lead_body), "Municipality")), ref = "Municipality"),
    delivery_arch   = relevel(factor(replace_na(as.character(delivery_arch), "Single body")), ref = "Single body")
  )

interaction_family <- list()   # collects the pre-registered interaction tests

## Coefficients that survive neither the raw fit nor bias reduction are reported
## as non-estimable rather than as effects.
## H1 FIX, second part (external audit). Two thresholds disagreed: separation was
## DETECTED at |b| > 8 (is_separated, below) but coefficients were only blanked at
## |b| > 20. So a Firth-corrected estimate of -15.02 with SE 0.81 and p = 5e-77
## survived as a reportable effect -- flagged as separated, yet printed with full
## inference, and it was the most "significant" number in the D2 table.
##
## Firth-type bias reduction makes such a fit CONVERGE; it does not make the
## coefficient an interpretable effect size when the underlying cell is empty or
## near-empty. The limit is therefore aligned with the detection threshold: one
## rule, one number, applied wherever separation is judged. An SE below 1e-4 is
## treated the same way, since that is the numerical signature of a zero cell.
guard_estimates <- function(tab, limit = 8) {
  tab %>% mutate(non_estimable = !is.finite(estimate) | abs(estimate) > limit |
                                 (is.finite(std.error) & std.error < 1e-4),
                 across(c(estimate, std.error, statistic, p.value),
                        ~ ifelse(non_estimable, NA_real_, .x)))
}

## Binary models are refitted with Firth-type bias reduction whenever complete
## or quasi-complete separation is detected. Sparse interaction cells otherwise
## produce coefficients of |b| > 10 with tiny standard errors, which look highly
## significant and are meaningless.
is_separated <- function(m) {
  fv <- fitted(m)
  any(abs(stats::coef(m)) > 8, na.rm = TRUE) || any(fv < 1e-8 | fv > 1 - 1e-8)
}

## The separation screen and the cluster-robust
## covariance applied ONLY to the binary models. The multinomial and ordinal
## fits were tabulated with raw model standard errors and no screen at all, so
## the D2 access-route multinomial reported coefficients of -16 to -19 with
## standard errors of 5e-06 and p = 0 exactly -- the signature of a zero cell,
## printed as the most significant result in the study.
##
## This guard is applied to EVERY non-binary table before it is written. It does
## not silently rescue the estimate: a separated coefficient is marked
## non-estimable and its inference blanked, because there is no finite MLE to
## report. It also records, on every row, which covariance was used and how many
## clusters support it -- 28 countries is a small-cluster setting and a reader
## must be told rather than left to assume.
guard_nonbinary <- function(tab, label, n_clusters = NA_integer_,
                            covariance = "model-based (not cluster-robust)") {
  stopifnot(all(c("estimate", "std.error") %in% names(tab)))
  tab <- tab %>%
    mutate(separation_flag = abs(estimate) > 8 | std.error < 1e-4 |
                             !is.finite(estimate) | !is.finite(std.error),
           non_estimable   = separation_flag,
           covariance      = covariance,
           n_clusters      = n_clusters) %>%
    mutate(across(c(estimate, std.error, p.value, any_of(c("statistic", "or",
                                                           "or_lo", "or_hi"))),
                  ~ ifelse(non_estimable, NA_real_, .x)))
  .k <- sum(tab$non_estimable, na.rm = TRUE)
  if (.k) msg("  %s: %d of %d coefficients marked NON-ESTIMABLE (separation)",
              label, .k, nrow(tab))
  tab
}

logit_cl <- function(f, data, label, cluster = "country") {
  m   <- glm(f, data = data, family = binomial())
  sep <- is_separated(m)

  if (sep && requireNamespace("brglm2", quietly = TRUE)) {
    mb <- try(suppressWarnings(
      glm(f, data = data, family = binomial(), method = brglm2::brglmFit,
          type = "AS_mean", maxit = 1000, slowit = 0.5)), silent = TRUE)
    ok <- !inherits(mb, "try-error") && isTRUE(mb$converged)
    if (ok) {
      ct  <- summary(mb)$coefficients
      tab <- tibble(model = paste0(label, " [Firth-corrected: separation detected]"),
                    term = rownames(ct), estimate = ct[, 1], std.error = ct[, 2],
                    statistic = ct[, 3], p.value = ct[, 4]) %>%
        mutate(or = exp(estimate), or_lo = exp(estimate - 1.96 * std.error),
               or_hi = exp(estimate + 1.96 * std.error),
               n = nobs(mb), separation_flag = TRUE)
      return(list(model = mb, tab = guard_estimates(tab), separated = TRUE))
    }
    ## Bias reduction failed to converge: report the model but mark every
    ## coefficient as non-estimable, so nothing here can be read as an effect.
    tab <- tidy_cl(m, cluster = data[[cluster]],
                   label = paste0(label, " [separation; bias reduction did not converge]")) %>%
      mutate(n = nobs(m), separation_flag = TRUE, non_estimable = TRUE,
             across(c(estimate, std.error, statistic, p.value), ~ NA_real_))
    return(list(model = m, tab = tab, separated = TRUE))
  }

  out <- list(model = m, separated = sep,
              tab = tidy_cl(m, cluster = data[[cluster]], label = label) %>%
                mutate(n = nobs(m), separation_flag = sep))
  out$tab <- guard_estimates(out$tab)
  out
}


# -----------------------------------------------------------------------------
# DOMAIN 1 | Ecological-functional responsiveness
#   Q: do stated hazards track independently documented hazard exposure?
#   Q: is spatial targeting associated with documented exposure?
# -----------------------------------------------------------------------------
d1_tabs <- list()
hx_map <- c(storm = "hx_flood", heat = "hx_heat", drought = "hx_drought")
for (h in names(hx_map)) {
  yv <- paste0("haz_", h); xv <- hx_map[[h]]
  d  <- M %>% filter(!is.na(.data[[yv]]), !is.na(.data[[xv]]))
  if (!modellable(d[[yv]])) { msg("  [skip] hazard responsiveness: %s", h); next }
  f <- as.formula(sprintf("%s ~ %s + climate_grp + density_c + pop_c + region", yv, xv))
  r <- logit_cl(f, d, paste0("D1 hazard responsiveness: ", h))
  d1_tabs[[h]] <- r$tab
}
## spatial targeting ~ exposure, with the pre-registered exposure x density test
d_t <- M %>% filter(!is.na(targeted_prog))
r_t <- logit_cl(targeted_prog ~ hx_n * density_c + pop_c + lead_body + region_mod,
                d_t, "D1 spatial targeting (exposure x density)")
interaction_family[["D1 exposure x density"]] <- r_t$tab %>% filter(str_detect(term, ":"))

d1 <- bind_rows(d1_tabs, r_t$tab)
save_tab(d1, "07_D1_ecological_functional_models")

# -----------------------------------------------------------------------------
# DOMAIN 2 | Tenure & authority access
#   Central proposition: where programmes fund work ON BUILDINGS in renter-
#   dominant cities, do they open a corresponding controller/tenant route?
# -----------------------------------------------------------------------------
d2 <- M %>% filter(access_fit != "Indeterminate")

## (a) binary: any controller or tenant route documented
d2 <- d2 %>% mutate(route_open = as.integer(access_route %in%
       c("Direct tenant/broad pathway", "Controller/collective pathway",
         "Permission-dependent pathway")))
r2a <- logit_cl(route_open ~ renter_c * bldg_integrated + density_c + pop_c +
                  lead_body + delivery_arch3 + region_mod, d2,
                "D2 route open (renter x building-integrated)")
interaction_family[["D2 renter x building-integrated"]] <- r2a$tab %>% filter(str_detect(term, ":"))

## (b) multinomial over the full access-route typology
d2m <- d2 %>% mutate(route = relevel(factor(access_route), ref = "Owner-restricted pathway"))
m2b <- nnet::multinom(route ~ renter_c + bldg_integrated + density_c + pop_c +
                        lead_body + region_mod, data = d2m, trace = FALSE)
sm  <- summary(m2b)
z   <- sm$coefficients / sm$standard.errors
d2b_tab <- as.data.frame.table(sm$coefficients, stringsAsFactors = FALSE) %>%
  setNames(c("outcome_level", "term", "estimate")) %>%
  as_tibble() %>%
  mutate(std.error = as.data.frame.table(sm$standard.errors, stringsAsFactors = FALSE)$Freq,
         p.value   = 2 * (1 - pnorm(abs(as.data.frame.table(z, stringsAsFactors = FALSE)$Freq))),
         or = exp(estimate), model = "D2 access-route multinomial") %>%
  guard_nonbinary("D2 access-route multinomial", n_clusters = n_distinct(d2m$country))

save_tab(bind_rows(r2a$tab, d2b_tab), "07_D2_spatial_institutional_models")

# -----------------------------------------------------------------------------
# DOMAIN 3A | Technical enablement (ordinal grade among applicable cases)
# -----------------------------------------------------------------------------
d3 <- M %>% filter(tech_applicable == 1, tech_grade != "Indeterminate") %>%
  mutate(tech_grade  = droplevels(factor(tech_grade, ordered = TRUE)),
         tech_demand = droplevels(tech_demand),
         delivery_arch3 = droplevels(delivery_arch3))
m3 <- MASS::polr(tech_grade ~ tech_demand * delivery_arch3 + n_meas_c + funding_horizon +
                   prog_age_c + region_mod, data = d3, Hess = TRUE)
m3_tab <- broom::tidy(m3) %>%
  mutate(model = "D3A technical enablement (ordinal)",
         p.value = 2 * (1 - pnorm(abs(statistic)))) %>%
  guard_nonbinary("D3A technical enablement (ordinal)", n_clusters = n_distinct(d3$country))
interaction_family[["D3A demand x delivery"]] <- m3_tab %>% filter(str_detect(term, ":"))

## count model as a robustness check
m3b <- glm(n_enablers ~ tech_demand * delivery_arch3 + n_meas_c + funding_horizon + region_mod,
           data = d3, family = poisson())
m3b_tab <- tidy_cl(m3b, d3$country, "D3A enabler count (Poisson)")
save_tab(bind_rows(m3_tab, m3b_tab), "07_D3A_technical_models")

# -----------------------------------------------------------------------------
# DOMAIN 3B | Financial calibration -- universal cost-barrier test only
# -----------------------------------------------------------------------------
d4 <- M %>% filter(!is.na(prefinancing_required))
r4 <- logit_cl(prefinancing_required ~ equity_targeted * tech_demand + funding_source +
                 lead_body + pop_c + region_mod, d4,
               "D3B pre-financing (priority target x intervention class)")
interaction_family[["D3B priority x intervention class"]] <- r4$tab %>% filter(str_detect(term, ":"))
save_tab(r4$tab, "07_D3B_financial_models")

## D3b: THE RESTRICTED 39-CASE TEST IS REMOVED ENTIRELY (decision on review).
## It was an artefact of scoping the financial test to programmes that declare an
## equity target -- 39 of 707 records -- which discards 668 records on the grounds
## that a cost barrier only counts as a misfit where a programme has promised
## otherwise. Reporting it beside the universal test invited the reader to treat
## the small number as the "real" test. The universal cost-barrier test is the
## only financial fit result the study reports.
##
## Equity targeting is NOT discarded: it survives as the third axis of the D3b
## flow panel, where it does useful work -- it identifies the programmes that name
## financially constrained applicants AND still require the full cost up front,
## which is the sharpest reading of the misfit. That is a stratum of the universal
## test, not a separate test with its own denominator.
fin_universal <- M %>% count(fit_D3b, name = "n") %>% mutate(p = n / sum(n))
save_tab(fin_universal, "07_D3B_financial_fitness_test")
msg("  D3b universal cost-barrier test: n = %d evaluable, %d confirmed misfit (restricted test removed)",
    sum(fin_universal$n[fin_universal$fit_D3b != "Indeterminate"]),
    sum(fin_universal$n[fin_universal$fit_D3b == "Confirmed misfit"]))

# -----------------------------------------------------------------------------
# DOMAIN 3C | Administrative accessibility (learning and compliance separately)
# -----------------------------------------------------------------------------
## FILTER ON THE CANONICAL VERDICT, not the retired composite tier. The D3c bar
## was raised on review (basic-only pathways no longer count as a partial fit), so
## filtering on admin_profile would leave the models describing a different record
## set from the figures -- exactly the drift the reconciliation check exists to
## catch. The OUTCOMES stay the underlying provision counts, which is correct:
## the model asks what predicts how many provisions a programme documents, not
## what predicts its verdict.
d5 <- M %>% filter(fit_D3c != "Indeterminate") %>%
  mutate(actor_breadth = relevel(factor(replace_na(actor_breadth, "Individual property actors only")),
                                 ref = "Individual property actors only"))
m5a <- glm(learn_score  ~ actor_breadth + tech_demand + delivery_arch3 + pop_c + prog_age_c + region_mod,
           data = d5, family = poisson())
m5b <- glm(comply_score ~ actor_breadth + tech_demand + delivery_arch3 + pop_c + prog_age_c + region_mod,
           data = d5, family = poisson())
save_tab(bind_rows(tidy_cl(m5a, d5$country, "D3C learning provisions"),
                   tidy_cl(m5b, d5$country, "D3C compliance provisions")),
         "07_D3C_administrative_models")

# -----------------------------------------------------------------------------
# DOMAIN 4A | Post-installation responsibility and support: is maintenance demand matched by programme continuity?
# -----------------------------------------------------------------------------
d6 <- M %>% filter(!is.na(st_support))
## coverage_scope is excluded here: 680 of 707 programmes are "partial/capped",
## which is a corpus-level finding, not a usable predictor (prevalence screen).
r6 <- logit_cl(st_support ~ maint_demand * funding_horizon + delivery_arch3 +
                 lead_body + region_mod, d6,
               "D4A maintenance support (demand x funding horizon)")
interaction_family[["D4A maint demand x funding horizon"]] <- r6$tab %>% filter(str_detect(term, ":"))
save_tab(r6$tab, "07_D4A_stewardship_models")

# -----------------------------------------------------------------------------
# DOMAIN 4B | Monitoring and adaptive management (ordinal)
# -----------------------------------------------------------------------------
## Same correction as D3c: filter on the canonical verdict so the model and the
## figure describe the same records. The ordinal OUTCOME remains the monitoring
## configuration itself, which is the substantive ladder being modelled.
d7 <- M %>% filter(fit_D4b != "Indeterminate") %>%
  mutate(mc = droplevels(factor(monitor_config, ordered = TRUE)))
m7 <- MASS::polr(mc ~ lead_body + delivery_arch3 + funding_horizon + pop_c + prog_age_c + region_mod,
                 data = d7, Hess = TRUE)
save_tab(broom::tidy(m7) %>%
           mutate(model = "D4B monitoring (ordinal)",
                  p.value = 2 * (1 - pnorm(abs(statistic)))) %>%
           guard_nonbinary("D4B monitoring (ordinal)", n_clusters = n_distinct(d7$country)),
         "07_D4B_monitoring_models")

# -----------------------------------------------------------------------------
# PRE-REGISTERED INTERACTION FAMILY -- corrected together (BH)
# -----------------------------------------------------------------------------
inter <- bind_rows(imap(interaction_family, ~ mutate(.x, family_member = .y))) %>%
  mutate(separation_flag = if ("separation_flag" %in% names(.)) separation_flag else FALSE,
         separation_flag = replace_na(separation_flag, FALSE),
         non_estimable   = if ("non_estimable" %in% names(.)) replace_na(non_estimable, FALSE) else FALSE,
         ## a coefficient this large in a sparse cell is not an effect estimate
         non_estimable = non_estimable | is.na(estimate) | abs(estimate) > 8) %>%
  select(family_member, term, estimate, std.error, p.value, non_estimable) %>%
  mutate(p_bh = ifelse(non_estimable, NA_real_, bh(ifelse(non_estimable, NA, p.value))),
         significant_bh = !non_estimable & p_bh < CFG$fdr_alpha)
save_tab(inter, "07_interaction_family_BH")
msg("Interaction family: %d tests, %d survive BH at %.2f",
    nrow(inter), sum(inter$significant_bh, na.rm = TRUE), CFG$fdr_alpha)

# =============================================================================
# SENSITIVITY ANALYSES
# =============================================================================

## S1 | Leave-one-country-out for the three dominant corpora
loco <- map_dfr(c("United States", "Japan", "Germany"), function(cc) {
  dd <- d2 %>% filter(country != cc)
  m  <- glm(route_open ~ renter_c * bldg_integrated + density_c + pop_c + lead_body + region_mod,
            data = dd, family = binomial())
  tidy_cl(m, dd$country, paste0("D2 route_open | excluding ", cc)) %>%
    filter(str_detect(term, "renter_c|bldg_integrated")) %>% mutate(n = nobs(m))
})
save_tab(bind_rows(r2a$tab %>% filter(str_detect(term, "renter_c|bldg_integrated")) %>%
                     mutate(model = "D2 route_open | full corpus"), loco),
         "07_S1_leave_one_country_out")

## S2 | Refit on directly-stated codes only.
## v25 REFRAMED. This is NOT a "conservative" or preferred specification, and the
## excluded cases are not lower-quality data. An "[inferred]" qualifier marks a
## code the coder reached by reading the programme's own documents and said so,
## with an evidence quote attached like any other cell -- it is a transparency
## annotation, not a confidence grade. The baseline model therefore treats those
## codes as equal, and this refit exists only to show that the access finding does
## not depend on the subset of cells where the coder chose to annotate the reading.
## Report it as a coverage check; never substitute it for the baseline.
inf_join <- inf_flags %>% select(case_id, any_of(c("inf_C10", "inf_C9", "inf_C11", "inf_C15",
                                                   "inf_E1", "inf_E2", "inf_D7")))
d2_doc <- d2 %>% left_join(inf_join, by = "case_id") %>%
  filter(rowSums(across(starts_with("inf_")), na.rm = TRUE) == 0)
m_s2 <- glm(route_open ~ renter_c * bldg_integrated + density_c + pop_c + lead_body + region_mod,
            data = d2_doc, family = binomial())
save_tab(tidy_cl(m_s2, d2_doc$country, "D2 route_open | directly documented codes only") %>%
           mutate(n = nobs(m_s2)), "07_S2_documented_only")
msg("  S2: %d of %d cases retain directly documented codes on all focal variables",
    nrow(d2_doc), nrow(d2))

## S3 | Manski-style bounds on key prevalences under indeterminacy
## Bounds are computed over APPLICABLE cases only. Cases where a test does not
## apply are removed from the denominator; only genuinely indeterminate cases
## (applicable but undocumented) are bounded.
bound <- function(df, indicator, applicable = NULL) {
  d <- if (is.null(applicable)) df else df[!is.na(df[[applicable]]) & df[[applicable]] == 1, ]
  x <- d[[indicator]]
  n <- nrow(d); k <- sum(x == 1, na.rm = TRUE); miss <- sum(is.na(x))
  tibble(indicator = indicator, n_applicable = n, n_indeterminate = miss,
         observed = k / pmax(n - miss, 1),
         lower_bound = k / pmax(n, 1), upper_bound = (k + miss) / pmax(n, 1))
}
bounds <- bind_rows(
  bound(A, "mf_eco"),
  bound(A, "mf_access"),
  bound(A, "mf_tech",    "tech_applicable"),
  ## The restricted 39-case financial scope was removed
  ## from the study, but this bound still subset on it -- so the S3 sensitivity
  ## reported bounds over 39 records while the canonical D3b verdict covers all
  ## 707. The bound now runs on the universal corpus, matching fit_D3b.
  bound(A, "mf_fin"),
  bound(A, "wk_admin"),
  bound(A, "wk_steward"),
  bound(A, "wk_monitor")
)
save_tab(bounds, "07_S3_indeterminacy_bounds")

## S4 | Non-independence: some rows are the same programme coded once per
## municipality served. Refit the central access model (a) clustering on
## programme family rather than country, and (b) on one record per family.
s4_a <- glm(route_open ~ renter_c * bldg_integrated + density_c + pop_c + lead_body + region_mod,
            data = d2, family = binomial())
s4_tab_a <- tidy_cl(s4_a, cluster = d2$prog_family,
                    label = "D2 route_open | SEs clustered on programme family") %>%
  mutate(n = nobs(s4_a))
d2_uniq <- d2 %>% group_by(prog_family) %>% slice_head(n = 1) %>% ungroup()
s4_b <- glm(route_open ~ renter_c * bldg_integrated + density_c + pop_c + lead_body + region_mod,
            data = d2_uniq, family = binomial())
s4_tab_b <- tidy_cl(s4_b, cluster = d2_uniq$country,
                    label = "D2 route_open | one record per programme family") %>%
  mutate(n = nobs(s4_b))
save_tab(bind_rows(s4_tab_a, s4_tab_b) %>% filter(str_detect(term, "renter_c|bldg_integrated")),
         "07_S4_programme_family_nonindependence")
msg("  S4: %d cases reduce to %d distinct programmes after de-duplication",
    nrow(d2), nrow(d2_uniq))

msg("Stage-2 models and sensitivity analyses complete.")
