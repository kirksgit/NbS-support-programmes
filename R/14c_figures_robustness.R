# =============================================================================
# 14c_figures_robustness.R  |  PART C -- DATA, METHODS AND ROBUSTNESS
#
#   C1  what can be judged at all: evaluability, block documentation, provenance
#   C2  how much rests on a rule: threshold sensitivity WITH direction,
#       programme-family de-duplication, leave-one-country-out
#   C3  typology diagnostics: silhouette, Benzecri inertia, HCPC vs PAM
# =============================================================================

suppressPackageStartupMessages({
  library(ggplot2); library(patchwork); library(scales); library(forcats)
})
FGC <- M

# =============================================================================
# C1 | EVALUABILITY, BLOCK DOCUMENTATION, PROVENANCE
# =============================================================================
p_c1a <- FGC %>%
  summarise(`D1 Ecological-functional` = mean(ev_eco == 1, na.rm = TRUE),
            `D2 Tenure & authority access` = mean(ev_access == 1, na.rm = TRUE),
            `D3a Technical enablement` = mean(ev_tech == 1, na.rm = TRUE),
            `D3c Administrative access` = mean(ev_admin == 1, na.rm = TRUE),
            `D4a Maintenance support` = mean(ev_steward == 1, na.rm = TRUE),
            `D4b Monitoring` = mean(ev_monitor == 1, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "d", values_to = "v") %>%
  mutate(d = fct_reorder(d, v)) %>%
  ggplot(aes(v, d)) +
  geom_segment(aes(x = 0, xend = v, yend = d), colour = "grey78", linewidth = .9) +
  geom_point(size = 3, colour = PAL$purple) +
  geom_text(aes(label = percent(v, .1)), hjust = -.3, size = 2.3, colour = "grey30") +
  scale_x_continuous(labels = percent_format(), limits = c(0, 1.13)) +
  labs(title = "a. Evaluability by domain",
       subtitle = WRAP("Share of records documented well enough for the domain to be judged at all. Post-installation reaches 100% BY CONSTRUCTION (its fields are always codable as one of three states; verdicts can still be Indeterminate) -- every other domain stays below 100% because silence stays silence."),
       x = "Share evaluable", y = NULL) +
  theme_nbs(8.5)

## block-specific documentation coverage: share of a block's coded variables
## carrying substantive content (not 'not stated'), per record -- the honest
## replacement for counting URLs
blocks <- list(
  "Access (C-8..C-16, D-7)" = c("C8","C9","C10","C11","C12","C13","C14","C15","C16","D7"),
  "Technical (C-5..C-7, D-8, E-13, E-21)" = c("C5","C6","C7","D8","E13","E21"),
  "Financial (E-19..E-25)" = c("E19","E20","E21","E22","E23","E24","E25"),
  "Administrative (D-1..D-6)" = c("D1","D2","D3","D4","D5","D6"),
  "Post-installation (E-15..E-18, F-5)" = c("E15","E16","E17","E18","F5"),
  "Monitoring (F-1..F-4)" = c("F1","F2","F3","F4"))
cov_tab <- purrr::imap_dfr(blocks, function(codes, lab) {
  cols <- paste0("v_", codes); cols <- intersect(cols, names(raw))
  stated <- sapply(cols, function(cc)
    !str_detect(replace_na(strip_qual(raw[[cc]]), "not stated"),
                regex("^(not stated|cannot verify)", ignore_case = TRUE)))
  tibble(block = lab, coverage = rowMeans(stated))
})
save_tab(cov_tab %>% group_by(block) %>%
           summarise(mean = mean(coverage), median = median(coverage), .groups = "drop"),
         "14_c1_block_documentation")

p_c1b <- cov_tab %>%
  mutate(block = fct_reorder(str_wrap(block, 26), coverage, median)) %>%
  ggplot(aes(coverage, block, fill = block)) +
  ggplot2::stat_summary(geom = "col", fun = mean, width = .7, alpha = .85) +
  ggplot2::stat_summary(geom = "errorbarh",
                        fun.min = function(z) quantile(z, .25),
                        fun.max = function(z) quantile(z, .75),
                        fun = median, height = .22, colour = "grey25", linewidth = .4) +
  scale_fill_viridis_d(option = "cividis", guide = "none") +
  scale_x_continuous(labels = percent_format(), limits = c(0, 1)) +
  labs(title = "b. How well each framework block is documented",
       subtitle = WRAP("Per-record share of the block's coded variables carrying substantive content (bar = mean, whiskers = interquartile range). This block-level measure replaces URL counting, which conflates page count with completeness."),
       x = "Share of the block's variables documented", y = NULL) +
  theme_nbs(8)

prov <- try(utils::read.csv(file.path(CFG$out_tab, "13_parser_provenance_agreement.csv")), silent = TRUE)
if (!inherits(prov, "try-error") && nrow(prov)) {
  code_lab <- c(C5 = "Technical standards cited (C-5)", C6 = "Permit requirement stated (C-6)",
    C7 = "Permit fee waiver (C-7)", C8 = "Homeowner eligibility (C-8)",
    C9 = "Landlord eligibility (C-9)", C10 = "Tenant eligibility (C-10)",
    C11 = "Co-op/HOA eligibility (C-11)", C12 = "Business eligibility (C-12)",
    C13 = "Institutional eligibility (C-13)", C14 = "Faith/community eligibility (C-14)",
    C15 = "Social-housing eligibility (C-15)", C16 = "Informal-group eligibility (C-16)",
    D1 = "Applicant guide (D-1)", D2 = "Decision timeline (D-2)", D3 = "Form accessibility (D-3)",
    D4 = "Application channels (D-4)", D5 = "Multilingual materials (D-5)",
    D6 = "Co-design (D-6)", D7 = "Tenant pathway (D-7)", D8 = "Housing-type guidance (D-8)",
    E1 = "Green roofs supported (E-1)", E2 = "Green facades supported (E-2)",
    E3 = "Rain gardens supported (E-3)", E4 = "Depaving supported (E-4)",
    E5 = "Infiltration supported (E-5)", E6 = "Trees supported (E-6)",
    E13 = "Technical advice (E-13)", E15 = "Maintenance support (E-15)",
    E16 = "Maintenance duty (E-16)", E17 = "Binding condition (E-17)",
    E21 = "Funded design (E-21)", E22 = "Pre-financing requirement (E-22)",
    F1 = "Indicators published (F-1)", F2 = "Results reporting (F-2)",
    F3 = "Periodic review (F-3)", F4 = "Feedback channel (F-4)",
    G1 = "Rental context noted (G-1)", G3 = "Equity statement (G-3)",
    G4 = "Equity targeting (G-4)", G5 = "Equity indicators (G-5)", G6 = "Co-benefits stated (G-6)")
  p_c1c <- prov %>%
    mutate(block = dplyr::case_when(grepl("^C", variable) ~ "C  Eligibility and rules",
                                    grepl("^D", variable) ~ "D  Procedure",
                                    grepl("^E", variable) ~ "E  Measures and support",
                                    grepl("^F", variable) ~ "F  Monitoring",
                                    TRUE                  ~ "G  Justice and context"),
           vlab = dplyr::coalesce(code_lab[as.character(variable)], as.character(variable)),
           vlab = reorder_within(vlab, pct_concordant, block)) %>%
    ggplot(aes(pct_concordant, vlab)) +
    facet_grid(block ~ ., scales = "free_y", space = "free_y",
               labeller = labeller(block = label_wrap_gen(14))) +
    scale_y_reordered() +
    geom_segment(aes(x = 0, xend = pct_concordant, yend = vlab), colour = "grey80", linewidth = .5) +
    geom_point(aes(colour = pct_concordant < 90), size = 1.8) +
    scale_colour_manual(values = c(`TRUE` = PAL$misfit, `FALSE` = PAL$fit),
                        labels = c(`TRUE` = "below 90%: flagged for human review",
                                   `FALSE` = "90% or above"), name = NULL) +
    labs(title = "c. The parser against the coders' own confidence grades",
         ## "the permit pair sits lowest" WAS FALSE and is now derived from the
         ## table. Measured concordance ranks the equity-targeting pair lowest (G-5 38.5%,
         ## G-6 46.2%); the permit variable is third at 52.7%. The mechanism the sentence
         ## described is real for permit language, so it is kept as an EXAMPLE rather than
         ## a superlative, and the actual lowest variables are named from the data.
         subtitle = WRAP(sprintf("88 cases carry cell-level provenance; concordance between this pipeline's reading and the coders' grades, per variable in plain language (code in parentheses), grouped by coding-sheet block. Median %.0f%%. Concordance is lowest on %s (%.1f%%) and %s (%.1f%%); permit language ranks close behind because it is frequently conditional ('may require review'), which the parser reads as present and the coders graded as unclear. All are flagged for human reconciliation, not silently overridden.",
                 median(prov$pct_concordant, na.rm = TRUE),
                 as.character(prov$variable[order(prov$pct_concordant)][1]), sort(prov$pct_concordant)[1],
                 as.character(prov$variable[order(prov$pct_concordant)][2]), sort(prov$pct_concordant)[2])),
         x = "Concordance (%)", y = NULL) +
    theme_nbs(6.6) + theme(legend.position = "bottom", strip.text.y = element_text(angle = 0))
} else p_c1c <- plot_spacer()

## The applicability strip ("Can it be assessed?") lives here, moved out of the
## main-text verdict figure: evaluability is a property of the record, so it
## belongs with the evidence panels. `appl` is built in 14b (same session).
p_c1d <- ggplot(appl %>% mutate(dlab = factor(lab_of[as.character(domain)], levels = rev(ord_lab))),
                aes(p, dlab, fill = state)) +
  geom_col(width = .62, colour = "white", linewidth = .25) +
  geom_text(aes(label = ifelse(p > .06, n, "")),
            position = position_stack(vjust = .5), size = 2, colour = "grey15") +
  scale_fill_manual(values = STATE3, name = NULL) +
  scale_x_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
  labs(title = "d. Can it be assessed? Applicability and evaluability of the seven components",
       x = "Share of records", y = NULL) +
  theme_nbs(7.2) + theme(legend.position = "bottom")

fig_c1 <- ((p_c1a / p_c1b) | p_c1c) / p_c1d
fig_c1 <- fig_c1 + plot_layout(heights = c(1.6, .55)) +
  plot_annotation(title = "Figure S1. What the record can support: data, evidence, and where each test applies",
                  theme = TITLE())
save_fig(fig_c1, "figS01_evidence_applicability", 15, 13)

# =============================================================================
# C2 | HOW MUCH RESTS ON A RULE
# =============================================================================
sens <- as_tibble(utils::read.csv(file.path(CFG$out_tab, "12_threshold_sensitivity.csv"))) %>%
  mutate(kind = ifelse(str_detect(alternative_rule, regex("enabler|threshold", TRUE)),
                       "Numerical threshold", "Conceptual rule"),
         direction = case_when(
           pct_fit_alternative > pct_fit_baseline ~ "more lenient (fit share rises)",
           pct_fit_alternative < pct_fit_baseline ~ "stricter (fit share falls)",
           TRUE ~ "neutral"))
p_c2a <- sens %>%
  mutate(rule = fct_reorder(str_wrap(alternative_rule, 34), abs(pct_fit_alternative - pct_fit_baseline))) %>%
  ggplot(aes(y = rule)) +
  geom_segment(aes(x = pct_fit_baseline / 100, xend = pct_fit_alternative / 100, yend = rule,
                   colour = direction),
               linewidth = 1.1, arrow = arrow(length = unit(4.5, "pt"), type = "closed")) +
  geom_point(aes(x = pct_fit_baseline / 100, shape = kind), size = 2.4, colour = "grey35") +
  geom_text(aes(x = pmax(pct_fit_baseline, pct_fit_alternative) / 100,
                label = sprintf("  %.0f%% of labels move", pct_changed)),
            hjust = 0, size = 2.1, colour = "grey30") +
  facet_grid(kind ~ ., scales = "free_y", space = "free_y",
             labeller = labeller(kind = label_wrap_gen(12))) +
  scale_colour_manual(values = c("stricter (fit share falls)" = PAL$misfit,
                                 "more lenient (fit share rises)" = PAL$fit,
                                 "neutral" = "grey55"), name = "Direction of the alternative") +
  scale_shape_manual(values = c("Numerical threshold" = 16, "Conceptual rule" = 17), name = NULL) +
  scale_x_continuous(labels = percent_format(), limits = c(0, 1.2), breaks = seq(0, 1, .25)) +
  labs(title = "a. Threshold sensitivity as transitions",
       subtitle = WRAP("Each arrow runs from the baseline documented-fit share (dot) to the share under the alternative rule; the text is how many labels move. Conceptual rules and numerical thresholds are kept apart. A rule can move many labels while the aggregate share barely changes -- both facts are visible."),
       x = "Documented-fit share of comparable records", y = NULL) +
  theme_nbs(7.6) + theme(strip.text.y = element_text(angle = 0), legend.position = "bottom")

## programme-family de-duplication of the headline shares
fam_sens <- purrr::map_dfr(list(
  c(k = "open", lab = "Open access route (D2)"),
  c(k = "req",  lab = "Technical requirements met (D3a)"),
  c(k = "mon",  lab = "Indicators or review (D4b)")), function(z) {
    d <- FGC %>% mutate(
      open = ifelse(access_fit == "Indeterminate", NA,
                    as.integer(access_route %in% c("Direct tenant/broad pathway",
                                                   "Controller/collective pathway"))),
      req = as.integer(requirement_match >= .75),
      mon = as.integer(mon_indicators == 1 | f_review == 1)) %>%
      filter(!is.na(.data[[z[["k"]]]]))
    fam1 <- d %>% group_by(prog_family) %>% slice_head(n = 1) %>% ungroup()
    tibble(outcome = z[["lab"]],
           `All records` = mean(d[[z[["k"]]]]),
           `One record per programme family` = mean(fam1[[z[["k"]]]]))
  }) %>% pivot_longer(-outcome, names_to = "unit", values_to = "p")
save_tab(fam_sens, "14_c2_family_sensitivity")

p_c2b <- ggplot(fam_sens, aes(p, outcome, colour = unit)) +
  geom_line(aes(group = outcome), colour = "grey70", linewidth = .7) +
  geom_point(size = 3) +
  geom_text(aes(label = percent(p, 1)), vjust = -1, size = 2.2, show.legend = FALSE) +
  scale_colour_manual(values = c("All records" = PAL$blue,
                                 "One record per programme family" = PAL$teal), name = NULL) +
  scale_x_continuous(labels = percent_format()) +
  labs(title = "b. De-duplicating the regional schemes barely moves the headline shares",
       subtitle = sprintf("%d distinct programme families sit behind the %d records; %d families are coded once per municipality served.",
                          dplyr::n_distinct(A$prog_family), nrow(A), sum(table(A$prog_family) > 1)),
       x = "Share of evaluable records", y = NULL) +
  theme_nbs(8) + theme(legend.position = "bottom")

## leave-one-country-out for the concentration-sensitive shares
loco <- purrr::map_dfr(c("United States", "Japan", "Germany"), function(cc) {
  d <- FGC %>% mutate(open = ifelse(access_fit == "Indeterminate", NA,
                as.integer(access_route %in% c("Direct tenant/broad pathway",
                                               "Controller/collective pathway")))) %>%
    filter(!is.na(open))
  tibble(dropped = sprintf("without %s (n=%d)", cc, sum(d$country_mod != cc)),
         p = mean(d$open[d$country_mod != cc]))
}) %>% bind_rows(tibble(dropped = "full corpus", p = {
  d <- FGC %>% mutate(open = ifelse(access_fit == "Indeterminate", NA,
                as.integer(access_route %in% c("Direct tenant/broad pathway",
                                               "Controller/collective pathway"))))
  mean(d$open, na.rm = TRUE)}))
p_c2c <- loco %>%
  mutate(dropped = fct_reorder(dropped, p)) %>%
  ggplot(aes(p, dropped)) +
  geom_segment(aes(x = 0, xend = p, yend = dropped), colour = "grey80", linewidth = .8) +
  geom_point(size = 3, colour = PAL$blue) +
  geom_text(aes(label = percent(p, 1)), hjust = -.3, size = 2.3) +
  scale_x_continuous(labels = percent_format(), limits = c(0, .8)) +
  labs(title = "c. Leave-one-country-out on the open-route share",
       subtitle = "Each of the three dominant countries removed in turn. The result is compositional, not carried by any single national system.",
       x = "Open access route, share of evaluable records", y = NULL) +
  theme_nbs(8)

fig_c2 <- p_c2a / (p_c2b | p_c2c) + plot_layout(heights = c(1.15, .85)) +
  plot_annotation(title = "Figure S2. How much rests on a rule, a duplicate, or one country (robustness)",
                  theme = TITLE())
save_fig(fig_c2, "figS02_sensitivity", 15, 12)

# =============================================================================
# C3 | TYPOLOGY DIAGNOSTICS
# =============================================================================
diag_k <- try(utils::read.csv(file.path(CFG$out_tab, "08_cluster_diagnostics.csv")), silent = TRUE)
inertia <- try(utils::read.csv(file.path(CFG$out_tab, "08_mca_inertia.csv")), silent = TRUE)
robust  <- try(utils::read.csv(file.path(CFG$out_tab, "08_typology_robustness.csv")), silent = TRUE)

## the S11 subtitle's numbers, computed from the same tables the panels plot.
## The bootstrap k is taken as the k with the highest MEAN Jaccard across its classes,
## which is the quantity the panel shows; the macro figures come from the typology
## column the rest of the paper uses, not from a remembered run.
if (!inherits(diag_k, "try-error")) {
  .sil_k <- diag_k$k[which.max(diag_k$avg_silhouette)]
  .ch_k  <- diag_k$k[which.max(diag_k$calinski_harabasz)]
  .boot_k <- if (!inherits(robust, "try-error") && "jaccard" %in% names(robust)) {
    .bk <- robust %>% group_by(k) %>% summarise(m = mean(jaccard, na.rm = TRUE), .groups = "drop")
    .bk$k[which.max(.bk$m)]
  } else {
    .bj <- try(utils::read.csv(file.path(CFG$out_tab, "08_bootstrap_jaccard.csv")), silent = TRUE)
    if (inherits(.bj, "try-error")) NA_integer_ else {
      .bk <- .bj %>% group_by(k) %>% summarise(m = mean(jaccard, na.rm = TRUE), .groups = "drop")
      .bk$k[which.max(.bk$m)]
    }
  }
  .mac <- FGB %>% filter(!is.na(typology_macro),
                         as.character(typology_macro) != "Opacity group") %>%
          count(typology_macro)
  .macro_k   <- nrow(.mac)
  .macro_max <- max(.mac$n)
  .macro_n   <- sum(.mac$n)
  stopifnot(is.finite(.sil_k), is.finite(.ch_k), .macro_k > 0)

  p_c3a <- diag_k %>%
    pivot_longer(c(avg_silhouette, calinski_harabasz), names_to = "crit", values_to = "v") %>%
    mutate(crit = recode(crit, avg_silhouette = "Average silhouette",
                         calinski_harabasz = "Calinski-Harabasz")) %>%
    ggplot(aes(k, v)) +
    geom_line(colour = "grey60") +
    geom_point(aes(colour = selected), size = 2.6) +
    facet_wrap(~ crit, scales = "free_y") +
    scale_colour_manual(values = c(`TRUE` = PAL$misfit, `FALSE` = "grey35"),
                        labels = c(`TRUE` = "selected k", `FALSE` = ""), name = NULL) +
    labs(title = "a. Cluster count selection",
         subtitle = "Selection maximises Calinski-Harabasz among partitions whose smallest class holds at least 5% of records.",
         x = "k", y = NULL) +
    theme_nbs(8)
} else p_c3a <- plot_spacer()

if (!inherits(inertia, "try-error")) {
  p_c3b <- inertia %>% filter(dim <= 10) %>%
    ggplot(aes(dim, pct_benzecri)) +
    geom_col(fill = PAL$blue, width = .7) +
    geom_line(aes(y = cum_pct_benzecri / 3), colour = PAL$misfit, linewidth = .7) +
    geom_point(aes(y = cum_pct_benzecri / 3), colour = PAL$misfit, size = 1.6) +
    scale_y_continuous(name = "Benzecri-corrected inertia (%)",
                       sec.axis = sec_axis(~ . * 3, name = "Cumulative (%)")) +
    scale_x_continuous(breaks = 1:10) +
    labs(title = "b. MCA dimensionality",
         subtitle = "Benzecri correction, the appropriate scale for MCA: raw percentages understate structure by construction.",
         x = "Dimension") +
    theme_nbs(8)
} else p_c3b <- plot_spacer()

if (!inherits(robust, "try-error")) {
  p_c3c <- ggplot() +
    annotate("text", x = 0, y = .72, hjust = 0, size = 3.1, colour = PAL$ink, fontface = "bold",
             label = "c. Partition stability") +
    annotate("text", x = 0, y = .42, hjust = 0, size = 2.6, colour = "grey25",
             label = sprintf("HCPC vs PAM on Gower distances, same k:\nadjusted Rand index = %.2f",
                             robust$adjusted_rand_index[1])) +
    annotate("text", x = 0, y = .14, hjust = 0, size = 2.6, colour = "grey25",
             label = sprintf("Average silhouette: HCPC %.2f, PAM %.2f",
                             robust$hcpc_avg_silhouette[1], robust$pam_avg_silhouette[1])) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) + theme_void()
} else p_c3c <- plot_spacer()

## Bootstrap Jaccard stability (fixed-MCA-space approximation, from 08)
jac <- as_tibble(utils::read.csv(file.path(CFG$out_tab, "08_bootstrap_jaccard.csv")))
p_c3d <- jac %>%
  mutate(klab = sprintf("k = %d", k)) %>%
  ggplot(aes(factor(class), jaccard, fill = klab)) +
  geom_col(position = position_dodge2(preserve = "single"), width = .7) +
  geom_hline(yintercept = .75, linetype = "22", colour = "grey40") +
  facet_grid(. ~ klab, scales = "free_x", space = "free_x") +
  ## the third partition is whichever k the diagnostics select, not k = 6.
  scale_fill_manual(values = setNames(c(PAL$softblue, PAL$purple, "grey60"),
                                      sprintf("k = %d", sort(unique(jac$k)))),
                    guide = "none") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "d. Bootstrap class stability",
       ## THIS COMPARISON WAS BACKWARDS and is now read from the table. Measured on
       ## the shipped workbook, 4 of 6 k = 6 classes clear the 0.75 convention against 1
       ## of 3 at k = 3, and only one k = 6 class sits below k = 3's weakest. The old
       ## sentence supported a "macro choice" the paper no longer makes -- the six-class
       ## partition is primary. Neither partition is fully stable and the caption says so.
       ## "k = 6" WAS LITERAL TEXT while the counts around it were computed.
       ## The measure corrections moved the selected refinement from k = 6 to
       ## k = 4, so the caption named a partition the run no longer produces.
       ## The label is now read from the data like everything else in it.
       subtitle = WRAP(sprintf("Mean bootstrap Jaccard per class (200 resamples in the fixed MCA space; 0.75 dashed = conventional stability). %d of %d classes clear the convention at k = %d (range %.2f-%.2f) against %d of %d at k = 3 (range %.2f-%.2f). The %d-class partition is primary because it separates on more design axes.",
                 sum(jac$jaccard[jac$k == max(jac$k)] >= 0.75), sum(jac$k == max(jac$k)),
                 max(jac$k),
                 min(jac$jaccard[jac$k == max(jac$k)]), max(jac$jaccard[jac$k == max(jac$k)]),
                 sum(jac$jaccard[jac$k == 3] >= 0.75), sum(jac$k == 3),
                 min(jac$jaccard[jac$k == 3]), max(jac$jaccard[jac$k == 3]),
                 max(jac$k))),
       x = "Class", y = "Mean Jaccard") +
  theme_nbs(7.6)

fig_c3 <- (p_c3a | p_c3d) / (p_c3b | p_c3c) + plot_layout(heights = c(1, 1)) +
  plot_annotation(
    title = "Figure S11. Typology diagnostics: a pragmatic macro-partition under modest separation",
    ## EVERY NUMBER IN THIS SUBTITLE IS NOW READ OFF THE DIAGNOSTIC TABLE
    ## PLOTTED DIRECTLY ABOVE IT. As written it said silhouette peaks at k = 2, "CH
    ## keeps rising to k = 6", and "Type 2 holds 517 of the 682 classified records".
    ## The first was right; the second and third were not -- the table shows CH
    ## peaking at k = 4 and falling after, and the largest class held 472 of 707. A
    ## caption that a reader can check against the panel beside it must be computed
    ## from the same object, or it becomes a claim the figure itself contradicts.
    subtitle = WRAP(sprintf(
      paste("The criteria genuinely conflict: silhouette peaks at k = %d, Calinski-Harabasz at k = %d,",
            "and bootstrap Jaccard is highest at k = %d. The k = %d macro-partition is pragmatic --",
            "stable and interpretable -- not a resolved typology, and its largest type holds %d of",
            "the %d classified records."),
      .sil_k, .ch_k, .boot_k, .macro_k, .macro_max, .macro_n)),
    theme = TITLE())
save_fig(fig_c3, "figS11_typology_diagnostics", 14.5, 10)

# =============================================================================
# C4 | THE FINER SIX-CLASS PARTITION (exploratory refinement; supplement)
# =============================================================================
## the stability figures the subtitle quotes are READ from the diagnostic table
## the claim rests on, never typed. If a future run's stability changes, the caption
## changes with it -- and the assertion below fails rather than the caption drifting.
.jall <- readr::read_csv(file.path(CFG$out_tab, "08_bootstrap_jaccard.csv"), show_col_types = FALSE)
.j6 <- .jall[.jall$k == max(.jall$k), , drop = FALSE]
.j3 <- .jall[.jall$k == 3, , drop = FALSE]
## The caption STATES how many classes clear the convention rather than asserting that
## all do -- because on the corrected workbook they do not (k=6 runs to 0.70, k=3 to
## 0.72). Assert only what must be true for the caption to be readable: both partitions
## present, and the counts the caption quotes are derivable.
stopifnot(nrow(.j6) >= 3, nrow(.j3) == 3, all(is.finite(.j6$jaccard)), all(is.finite(.j3$jaccard)))

prof6 <- purrr::map_dfr(seq_len(nrow(arch_items)), function(i) {
  FGB %>% filter(typology_k != "Opacity group") %>%
    group_by(typology_k) %>%
    summarise(p = mean(eval(arch_items$val[[i]], envir = pick(everything())), na.rm = TRUE),
              .groups = "drop") %>%
    mutate(item = arch_items$item[i], block = arch_items$block[i])
})
n6 <- FGB %>% filter(typology_k != "Opacity group") %>% count(typology_k)
## THE FINE-PARTITION PROFILE IS NOW SAVED.
## The manuscript quotes thirteen percentages off this object, and it was computed
## inside the figure and written nowhere -- so not one of those thirteen could be
## checked against the bundle. Any number a caption or the manuscript quotes must
## exist in output/tables/.
## THE COMPOSITE IS NOT WHAT THE TEXT QUOTES.
## prof6's technical row is "Standards or advice (C-5/E-13)", i.e.
## r_standards | i_advice, which runs near 100% in most classes and cannot be
## the source of the manuscript's per-class standards figures. The BARE
## variables the text actually quotes -- r_standards, el_tenant_direct, i_grant,
## st_support -- appeared by k=3 macro type in 08_macro_architecture_profiles.csv
## and by k=6 class in NO shipped table. They are computed here with exactly the
## same definitions as prof3 in 08_typology.R, so the two are comparable.
.bare6 <- FGB %>%
  filter(typology_k != "Opacity group") %>%
  group_by(typology_k) %>%
  summarise(`Tenant-direct eligibility (el_tenant_direct)` = mean(el_tenant_direct == 1, na.rm = TRUE),
            `Grant instrument (i_grant)`                   = mean(i_grant == 1, na.rm = TRUE),
            `Technical standards (r_standards, C-5)`       = mean(r_standards == 1, na.rm = TRUE),
            `Post-installation support (st_support)`       = mean(st_support == 1, na.rm = TRUE),
            .groups = "drop") %>%
  tidyr::pivot_longer(-typology_k, names_to = "item", values_to = "p") %>%
  mutate(block = "Bare variables quoted in section 3.4")

save_tab(bind_rows(
           prof6 %>% left_join(n6, by = "typology_k") %>%
             select(typology_k, n, block, item, p),
           .bare6 %>% left_join(n6, by = "typology_k") %>%
             select(typology_k, n, block, item, p)) %>%
           arrange(typology_k, block, item),
         "14_c4_fine_partition_profiles")
p_c4 <- prof6 %>% left_join(n6, by = "typology_k") %>%
  mutate(cls = sprintf("%s\n(n=%d)", typology_k, n),
         block = factor(block, levels = unique(arch_items$block))) %>%
  ggplot(aes(cls, fct_rev(item), fill = p)) +
  geom_tile(colour = "white", linewidth = .5) +
  geom_text(aes(label = percent(p, 1)), size = 2) +
  facet_grid(block ~ ., scales = "free_y", space = "free_y",
             labeller = labeller(block = label_wrap_gen(14))) +
  scale_fill_seq("Share in class", labels = percent_format(), limits = c(0, 1)) +
  ## THE CLASS COUNT IS DERIVED, NOT WRITTEN. The title read "six-class"
  ## while the run selected k on the documented subset -- it drew FIVE classes at
  ## 1 and FOUR here. The figure had been mislabelled for as long as k differed
  ## from six, and it was invisible because the number sat in prose while the panel
  ## drew whatever the selection returned. This is the same defect class as every
  ## other hardcoded literal in the figure code: a number a reader can check against
  ## the panel beside it must come from the same object the panel is drawn from.
  labs(title = sprintf("Figure 13. The finer %d-class partition, shown with its measured stability",
                       nrow(n6)),
       subtitle = WRAP(sprintf(
         paste("The PRIMARY typology, promoted from the supplement. Bootstrap-Jaccard stability runs %.2f-%.2f",
               "across the %d classes, with %d of %d clearing the study's 0.75 convention; the k=3 macro-partition",
               "runs %.2f-%.2f with %d of 3 clearing it. NEITHER partition is fully stable, and this is reported",
               "rather than resolved: the finer partition is primary because it is separated on more design axes",
               "at once, not because it is stable where the coarser one is not. Classes are numbered, not named --",
               "the profile below IS the description. Each cell is the share of that class documenting the row."),
         min(.j6$jaccard), max(.j6$jaccard), nrow(n6),
         sum(.j6$jaccard >= 0.75), nrow(.j6),
         min(.j3$jaccard), max(.j3$jaccard), sum(.j3$jaccard >= 0.75))),
       x = NULL, y = NULL) +
  theme_nbs(7.2) + theme(strip.text.y = element_text(angle = 0))
## PROMOTED TO THE MAIN TEXT, on the author's decision. The k=3 macro-partition
## fails the project's own 0.75 bootstrap-Jaccard convention on all three classes
## (0.661 / 0.631 / 0.745, table 08_bootstrap_jaccard); the finer partition is the ONLY
## one where every class clears it, so it becomes the primary typology and k=3 becomes
## the coarse summary. The filename changes and the subtitle now states the stability
## that supports it, computed from the same table the claim rests on -- nothing about
## how the partition is BUILT changes, so no verdict, table or other figure moves.
save_fig(p_c4, "fig13_design_classes", 13.5, 9)

msg("Part C (data, methods, robustness): 4 figures written.")
