# =============================================================================
# 17_temporal.R  |  v58 / R7 -- the temporal finding.
#
# Programmes launched from 2018 onward document a route without pre-financing,
# and document monitoring, markedly LESS often than older ones. Both directions
# are reported, and the confound is stated rather than buried.
#
# THE CAVEAT IS LOAD-BEARING. D4b carries a MATURITY confound: a programme
# launched last year cannot yet have published a periodic review, so part of the
# monitoring gap is an artefact of age. D3b does NOT carry it -- payment design
# is fixed at launch, and a 2019 programme could have offered an upfront route
# from day one. The cost-barrier half is therefore the load-bearing half of this
# finding, and the manuscript must say so.
#
# The other obvious confound -- that year-unknown records are simply thinner --
# is testable and fails: year-KNOWN records carry a HIGHER median URL count than
# year-unknown ones, so this is not a documentation artefact.
# =============================================================================

.yr        <- M$year_launched
.post2018  <- ifelse(is.na(.yr), NA, as.integer(.yr >= 2018))
.n_known   <- sum(!is.na(.yr))

.temporal_row <- function(outcome, label, confound) {
  d <- data.frame(post = .post2018, y = outcome)
  d <- d[!is.na(d$post) & !is.na(d$y), ]
  if (!nrow(d) || length(unique(d$post)) < 2) return(NULL)
  tb <- table(factor(d$post, levels = c(0, 1)), factor(d$y, levels = c(0, 1)))
  ft <- tryCatch(fisher.test(tb), error = function(e) NULL)
  tibble(
    outcome        = label,
    pct_pre2018    = 100 * mean(d$y[d$post == 0]),
    n_pre2018      = sum(d$post == 0),
    pct_2018plus   = 100 * mean(d$y[d$post == 1]),
    n_2018plus     = sum(d$post == 1),
    diff_pp        = 100 * (mean(d$y[d$post == 1]) - mean(d$y[d$post == 0])),
    odds_ratio     = if (is.null(ft)) NA_real_ else unname(ft$estimate),
    fisher_p       = if (is.null(ft)) NA_real_ else ft$p.value,
    maturity_confound = confound)
}

TEMPORAL <- bind_rows(
  .temporal_row(as.integer(M$prefinancing_required == 0),
                "A route without pre-financing (D3b)",
                "NO -- payment design is fixed at launch; this is the load-bearing half"),
  .temporal_row(as.integer(M$fit_D4b == "Documented fit"),
                "Monitoring documented (D4b)",
                "YES -- a recently launched programme cannot yet have published a periodic review"))
save_tab(TEMPORAL, "17_temporal_by_launch_period")

## the documentation-artefact test, reported alongside so the caveat is checkable
.doc_test <- tibble(
  group          = c("year known", "year unknown"),
  n              = c(.n_known, nrow(M) - .n_known),
  median_urls    = c(median(M$n_url_unique[!is.na(.yr)], na.rm = TRUE),
                     median(M$n_url_unique[is.na(.yr)],  na.rm = TRUE)),
  mean_urls      = c(mean(M$n_url_unique[!is.na(.yr)], na.rm = TRUE),
                     mean(M$n_url_unique[is.na(.yr)],  na.rm = TRUE)))
save_tab(.doc_test, "17_temporal_documentation_check")

msg("R7 temporal: year known for %d of %d (%.0f%%); D3b %.1f%% -> %.1f%% (p=%.2g); D4b %.1f%% -> %.1f%% (p=%.2g)",
    .n_known, nrow(M), 100 * .n_known / nrow(M),
    TEMPORAL$pct_pre2018[1], TEMPORAL$pct_2018plus[1], TEMPORAL$fisher_p[1],
    TEMPORAL$pct_pre2018[2], TEMPORAL$pct_2018plus[2], TEMPORAL$fisher_p[2])

## ---- the supporting panel: this must not stay as prose statistics -----------
.plotdat <- TEMPORAL %>%
  select(outcome, pct_pre2018, pct_2018plus, n_pre2018, n_2018plus) %>%
  tidyr::pivot_longer(c(pct_pre2018, pct_2018plus),
                      names_to = "period", values_to = "pct") %>%
  mutate(period = ifelse(period == "pct_pre2018", "Launched before 2018", "Launched 2018 or later"),
         period = factor(period, levels = c("Launched before 2018", "Launched 2018 or later")),
         n = ifelse(period == "Launched before 2018", n_pre2018, n_2018plus))

p_temporal <- ggplot(.plotdat, aes(period, pct / 100, fill = period)) +
  geom_col(width = .62) +
  geom_text(aes(label = sprintf("%.1f%%\n(n=%d)", pct, n)), vjust = -0.25, size = 2.7) +
  facet_wrap(~ outcome, nrow = 1) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.48),
                     expand = expansion(mult = c(0, .12))) +
  scale_fill_manual(values = c("Launched before 2018" = PAL$softblue,
                               "Launched 2018 or later" = PAL$purple), guide = "none") +
  labs(title = "Figure 11. Newer programmes document less, and only one half of that is a maturity effect",
       subtitle = WRAP(sprintf("Launch year is documented for %d of %d programmes (%.0f%%). Programmes launched from 2018 onward document a route without pre-financing %.1f%% of the time against %.1f%% before 2018 (Fisher p = %.2g), and document monitoring %.1f%% against %.1f%% (p = %.2g). THE TWO ARE NOT EQUIVALENT. D4b carries a maturity confound -- a programme launched recently cannot yet have published a periodic review -- so part of the monitoring gap is an artefact of age. D3b does not: payment design is fixed at launch. The cost-barrier trend is therefore the load-bearing half. This is not a documentation artefact either: year-known records carry a HIGHER median URL count (%d) than year-unknown ones (%d).",
                 .n_known, nrow(M), 100 * .n_known / nrow(M),
                 TEMPORAL$pct_2018plus[1], TEMPORAL$pct_pre2018[1], TEMPORAL$fisher_p[1],
                 TEMPORAL$pct_2018plus[2], TEMPORAL$pct_pre2018[2], TEMPORAL$fisher_p[2],
                 .doc_test$median_urls[1], .doc_test$median_urls[2])),
       x = NULL, y = "Share of programmes documenting the provision") +
  theme_nbs(8.4)

save_fig(p_temporal, "fig11_temporal_provision", w = 9.2, h = 5.2)
