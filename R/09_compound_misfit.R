# =============================================================================
# 09_compound_misfit.R  |  DO WEAKNESSES COMPOUND WITHIN THE SAME PROGRAMME?
# Two indicator sets, reported side by side:
#   STRICT  mf_*  : confirmed misfits only (explicit exclusion / documented
#                   mismatch). Conservative, and by construction rare.
#   BROAD   wk_*  : documented weaknesses (owner-restricted access, <=1
#                   technical enabler, no burden-reducing provision, no
#                   post-installation architecture, no monitoring). Substantive
#                   but weaker evidentiary status -- sensitivity only.
# Denominators are ALWAYS pairwise-evaluable, never the full corpus.
# =============================================================================

## ALL SEVEN COMPONENTS, not four. D3c, D4a and D4b were
## excluded entirely -- D4a carries the second-largest misfit count and D4b the
## post-installation argument, so the strict compounding test was run on a
## subset that omitted exactly what the discussion leans on.
strict <- c(mf_eco = "Ecological-functional", mf_access = "Tenure & authority access",
            mf_tech = "Technical enablement", mf_fin = "Financial accessibility",
            mf_admin = "Administrative burden", mf_steward = "Stewardship support",
            mf_monitor = "Monitoring and review")
broad  <- c(wk_access = "Access restricted to owners", wk_tech = "Thin technical enablement",
            wk_admin = "Thin administrative provision", wk_steward = "Weak stewardship closure",
            wk_monitor = "No monitoring or feedback")

pairwise_assoc <- function(df, vars, label) {
  combos <- t(combn(names(vars), 2))
  map_dfr(seq_len(nrow(combos)), function(i) {
    a <- combos[i, 1]; b <- combos[i, 2]
    d <- df %>% filter(!is.na(.data[[a]]), !is.na(.data[[b]]))
    if (nrow(d) < 30) return(NULL)
    tb <- table(d[[a]], d[[b]])
    if (any(dim(tb) < 2)) return(NULL)
    ft  <- fisher.test(tb)
    ## Signed phi. sqrt(chi2/n) is Cramer's V and is always non-negative, so it
    ## cannot distinguish weaknesses that travel together from weaknesses that
    ## substitute for one another.
    aa <- sum(d[[a]] == 1 & d[[b]] == 1); bb <- sum(d[[a]] == 1 & d[[b]] == 0)
    cc <- sum(d[[a]] == 0 & d[[b]] == 1); dd <- sum(d[[a]] == 0 & d[[b]] == 0)
    den <- sqrt(as.numeric(aa + bb) * (cc + dd) * (aa + cc) * (bb + dd))
    phi <- if (den > 0) ((aa * dd) - (bb * cc)) / den else NA_real_
    tibble(set = label, var_a = vars[[a]], var_b = vars[[b]],
           n_pairwise_evaluable = nrow(d),
           n_both = sum(d[[a]] == 1 & d[[b]] == 1),
           expected_both = sum(d[[a]] == 1) * sum(d[[b]] == 1) / nrow(d),
           phi = as.numeric(phi), odds_ratio = unname(ft$estimate), p.value = ft$p.value)
  })
}

pw <- bind_rows(pairwise_assoc(M, strict, "Strict (confirmed misfit)"),
                pairwise_assoc(M, broad,  "Broad (documented weakness)")) %>%
  group_by(set) %>% mutate(p_bh = bh(p.value)) %>% ungroup()
save_tab(pw, "09_pairwise_compound_misfit")

## ---- Complete-core analysis ------------------------------------------------
## The financial test applies to a small, explicitly targeted subset, so the
## strict core is defined over the three broadly applicable domains; the
## financial misfit is reported alongside rather than inside the core.
core_domains <- c("mf_eco", "mf_access", "mf_tech")
core_strict <- M %>% filter(if_all(all_of(core_domains), ~ !is.na(.x))) %>%
  mutate(n_misfit = rowSums(across(all_of(core_domains)), na.rm = TRUE))
core_broad <- M %>% filter(if_all(all_of(names(broad)), ~ !is.na(.x))) %>%
  mutate(n_weak = rowSums(across(all_of(names(broad))), na.rm = TRUE))
## v58 / finding 43+44: a COMPLETE-CASE core over ALL SEVEN components, so the
## strict dispersion comparison is computed on the same footing as the broad one
## (every component observed, NA never silently read as 0). core_strict above is
## retained unchanged for the three-domain series the manuscript already quotes;
## this is the seven-component counterpart, reported alongside it.
core_strict7 <- M %>% filter(if_all(all_of(names(strict)), ~ !is.na(.x))) %>%
  mutate(n_misfit7 = rowSums(across(all_of(names(strict))), na.rm = TRUE))
msg("Strict complete core: 3-domain n = %d; 7-component n = %d",
    nrow(core_strict), nrow(core_strict7))

core_tab <- bind_rows(
  core_strict %>% count(n_misfit) %>% mutate(set = "Strict", p = n / sum(n)) %>%
    rename(n_problems = n_misfit),
  core_broad %>% count(n_weak) %>% mutate(set = "Broad", p = n / sum(n)) %>%
    rename(n_problems = n_weak))
save_tab(core_tab, "09_complete_core_counts")
msg("Complete core: strict n = %d (%.0f%% with >=2 confirmed misfits); broad n = %d (%.0f%% with >=3 weaknesses)",
    nrow(core_strict), 100 * mean(core_strict$n_misfit >= 2),
    nrow(core_broad),  100 * mean(core_broad$n_weak >= 3))

## Is compounding more than chance? Compare observed distribution of the number
## of weaknesses with the distribution expected under independence (Poisson-
## binomial via simulation using the marginal rates).
set.seed(CFG$seed)
p_marg <- colMeans(core_broad[, names(broad)], na.rm = TRUE)
sim <- replicate(5000, sum(rbinom(length(p_marg), 1, p_marg)))
obs <- core_broad$n_weak
## BOTH SETS ARE EMITTED, EACH LABELLED.
## This table was computed on the FIVE BROAD weakness flags, while the
## surrounding text quoted the STRICT component count. A reader comparing the
## two was comparing different things, and the strict comparison is the weaker
## of the two -- which is exactly the number the text should have carried.
.disp_of <- function(d, flags, lab) {
  pm  <- colMeans(d[, flags], na.rm = TRUE)
  sm  <- replicate(5000, sum(rbinom(length(pm), 1, pm)))
  ob  <- rowSums(d[, flags], na.rm = TRUE)
  tibble(set = lab,
         statistic = c("variance observed", "variance under independence",
                       "share with >=3 observed", "share with >=3 independent",
                       "n components", "n records"),
         value = c(var(ob), var(sm), mean(ob >= 3), mean(sm >= 3),
                   length(flags), nrow(d)))
}
disp <- bind_rows(
  .disp_of(core_broad,  names(broad),
           sprintf("Broad (%d documented weakness flags)", length(broad))),
  .disp_of(core_strict7, names(strict),
           sprintf("Strict (%d confirmed-misfit components, complete cases)",
                   length(strict))))
save_tab(disp, "09_compounding_vs_independence")
msg("Compounding vs independence emitted for BOTH sets (finding 44)")

## ---- Compound weakness by typology, region and context --------------------
save_tab(core_broad %>% group_by(typology_k) %>%
           summarise(n = n(), mean_weaknesses = mean(n_weak),
                     pct_3plus = 100 * mean(n_weak >= 3), .groups = "drop"),
         "09_compound_by_typology")
save_tab(core_broad %>% group_by(region_mod) %>%
           summarise(n = n(), mean_weaknesses = mean(n_weak),
                     pct_3plus = 100 * mean(n_weak >= 3), .groups = "drop"),
         "09_compound_by_region")

## ---- Documentation-coverage sensitivity -----------------------------------
sens <- core_broad %>%
  mutate(doc_tertile = ntile(n_url_unique, 3)) %>%
  group_by(doc_tertile) %>%
  summarise(n = n(), mean_weaknesses = mean(n_weak),
            pct_3plus = 100 * mean(n_weak >= 3), .groups = "drop")
save_tab(sens, "09_compound_by_documentation_depth")

M <- M %>%
  left_join(core_broad %>% select(case_id, n_weak), by = "case_id") %>%
  left_join(core_strict %>% select(case_id, n_misfit), by = "case_id")
