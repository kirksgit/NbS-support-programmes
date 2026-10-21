# =============================================================================
# 11_reporting.R  |  RESULTS ASSEMBLY
# Produces the tables in the order of the Results section, a small number of
# figures, an analysis-ready case-level dataset, and a single workbook.
# Every corpus-level statistic is reported twice: programme-weighted and
# country-standardised.
# =============================================================================

suppressPackageStartupMessages({ library(ggplot2); library(writexl) })

theme_set(theme_minimal(base_size = 11) +
            theme(panel.grid.minor = element_blank(),
                  plot.title = element_text(face = "bold", size = 11)))

# -----------------------------------------------------------------------------
# 3.1 | The global programme landscape (RQ1)
# -----------------------------------------------------------------------------
t31_geo <- M %>% count(region_mod, country) %>% arrange(region_mod, desc(n))
t31_obj <- dist_table(M %>% mutate(v = replace_na(obj_class, "Not documented")), "v")
t31_prt <- dist_table(M %>% mutate(v = replace_na(portfolio, "Not documented")), "v")
t31_ins <- dist_table(M %>% mutate(v = replace_na(instrument_pkg, "Not documented")), "v")
t31_lead<- dist_table(M %>% mutate(v = replace_na(as.character(lead_body), "Not documented")), "v")
t31_meas <- tibble(
  measure = c("Green roofs","Green facades","Rain gardens","Depaving/permeable",
              "Trees","Rain barrels/cisterns","Infiltration","Downspout disconnection"),
  n_yes = c(sum(M$m_greenroof == 1, na.rm = TRUE), sum(M$m_facade == 1, na.rm = TRUE),
            sum(M$m_raingard == 1, na.rm = TRUE), sum(M$m_depave == 1, na.rm = TRUE),
            sum(M$m_trees == 1, na.rm = TRUE), sum(M$m_barrel == 1, na.rm = TRUE),
            sum(M$m_infiltr == 1, na.rm = TRUE), sum(M$m_disconn == 1, na.rm = TRUE))) %>%
  mutate(pct = 100 * n_yes / nrow(M))

## Corpus-composition warning, printed with every global claim
concentration <- M %>% count(country) %>% arrange(desc(n)) %>%
  mutate(share = n / sum(n), cum_share = cumsum(share))
msg("Corpus concentration: top 3 countries = %.1f%% of programmes",
    100 * concentration$cum_share[3])

# -----------------------------------------------------------------------------
# 3.2 - 3.5 | Domain results (already computed; assembled here)
# -----------------------------------------------------------------------------
domain_summary <- bind_rows(
  dist_table(M %>% mutate(v = eco_fit), "v") %>% mutate(domain = "3.2 Ecological-functional"),
  ## the "documented objectives only" row is gone. It excluded cases whose
  ## B-5 carried an [inferred] qualifier; those are evidence-backed codes and are
  ## now treated as equal to any other. See the note at eco_fit_strict_RETIRED.
  dist_table(M %>% mutate(v = spatial_response), "v") %>% mutate(domain = "3.2 Spatial responsiveness"),
  dist_table(M %>% mutate(v = access_route), "v") %>% mutate(domain = "3.3 Tenure & authority access"),
  dist_table(M %>% mutate(v = participation_scope), "v") %>% mutate(domain = "3.3 Participation scope"),
  dist_table(M %>% mutate(v = as.character(tech_grade)), "v") %>% mutate(domain = "3.4.1 Technical enablement"),
  dist_table(M %>% mutate(v = payment_config), "v") %>% mutate(domain = "3.4.2 Payment configuration"),
  dist_table(M %>% mutate(v = replace_na(coverage_scope, "Not stated")), "v") %>% mutate(domain = "3.4.2 Cost coverage"),
  ## admin_profile is the RETIRED counting construct, kept only as a
  ## descriptive breakdown of provisions. The reported administrative-access
  ## result is the canonical D3c verdict.
  dist_table(M %>% mutate(v = as.character(fit_D3c)), "v") %>% mutate(domain = "3.4.3 Administrative access"),
  dist_table(M %>% mutate(v = as.character(admin_profile)), "v") %>%
    mutate(domain = "3.4.3b Administrative provisions (descriptive, not a verdict)"),
  dist_table(M %>% mutate(v = replace_na(as.character(delivery_arch), "Not documented")), "v") %>%
    mutate(domain = "3.4.4 Delivery architecture"),
  dist_table(M %>% mutate(v = steward_config), "v") %>% mutate(domain = "3.5.1 Post-installation responsibility and support"),
  dist_table(M %>% mutate(v = as.character(monitor_config)), "v") %>% mutate(domain = "3.5.2 Monitoring")
) %>% select(domain, level = v, n, p_prog, p_country_std)
save_tab(domain_summary, "11_domain_summary_dual_weighted")

# -----------------------------------------------------------------------------
# 3.6 | Equity-related design provisions (distribution / recognition / procedure)
# -----------------------------------------------------------------------------
equity_tab <- tibble(
  dimension = c(rep("Distribution", 3), rep("Recognition", 4), rep("Procedure", 4)),
  provision = c("Equity or vulnerability targeting", "Hazard-zone targeting",
                "Pre-financing required of applicant",
                "Landlords eligible", "Co-ops/HOAs eligible", "Social housing eligible",
                "Informal groups eligible or acknowledged",
                "Applicant guide (full or partial)", "Forms accessible (full or partial)",
                "Documented feedback channel", "Co-design or civil society in design"),
  n_documented = c(
    sum(M$equity_targeted == 1, na.rm = TRUE),
    sum(M$equity_target == "Hazard-exposure zones", na.rm = TRUE),
    sum(M$prefinancing_required == 1, na.rm = TRUE),
    sum(M$el_land == 1, na.rm = TRUE), sum(M$el_coop == 1, na.rm = TRUE),
    sum(M$el_socho == 1, na.rm = TRUE),
    sum(M$el_inform == 1 | M$q_informal_ack == 1, na.rm = TRUE),
    sum(M$p_guide >= 1, na.rm = TRUE), sum(M$p_forms >= 1, na.rm = TRUE),
    sum(M$f_feedback == 1, na.rm = TRUE),
    sum(M$p_codesign == 1 | M$q_civsoc == 1, na.rm = TRUE))) %>%
  mutate(pct = 100 * n_documented / nrow(M))
save_tab(equity_tab, "11_equity_design_provisions")

## WHERE THE RARE RECOGNITION PROVISIONS ACTUALLY SIT.
## The corpus-level rates above say a provision is rare; they do not say whether
## it is thinly spread or concentrated in a few national systems. Those are
## different findings with different implications for transfer, and only the
## second is visible per country. Rate is within-country: n documenting the
## provision over that country's corpus size, so a small national corpus with a
## high rate is legible as such rather than hidden by its absolute count.
## DEFINITIONS MUST MATCH equity_tab ABOVE, or the Results sentence and the
## clause extending it would count different things one after the other. The
## informal row there is el_inform OR q_informal_ack (53 programmes, 7.5%), not
## el_inform alone (39) -- so this table carries a flag PAIR per provision and
## takes their union, exactly as equity_tab does.
.rare_defs <- list(
  ## Names are the equity table's OWN row labels, character for character -- the
  ## consistency check joins on them, and a prettier synonym here would break the
  ## join and, worse, let the two tables drift apart unnoticed.
  "Social housing eligible"                  = c("el_socho"),
  "Informal groups eligible or acknowledged" = c("el_inform", "q_informal_ack"),
  "Landlords eligible"                       = c("el_land"))
.rare_conc <- lapply(
  .rare_defs,
  function(vs) {
    .hit <- Reduce(`|`, lapply(vs, function(v) coalesce(as.integer(M[[v]]), 0L) == 1))
    M %>%
      dplyr::filter(.hit) %>%
      dplyr::count(country, name = "n_documenting") %>%
      dplyr::left_join(dplyr::count(M, country, name = "n_country_corpus"), by = "country") %>%
      dplyr::mutate(pct_of_country = 100 * n_documenting / n_country_corpus) %>%
      dplyr::arrange(dplyr::desc(n_documenting))
  })
.rare_conc <- dplyr::bind_rows(.rare_conc, .id = "provision") %>%
  dplyr::group_by(provision) %>%
  dplyr::mutate(n_corpus_total = sum(n_documenting),
                pct_of_all_documenting = 100 * n_documenting / n_corpus_total) %>%
  dplyr::ungroup()
save_tab(.rare_conc, "11_rare_provision_concentration")

## Corpus-level absence statement for G-5
msg("Equity-indicator tracking (G-5) documented in %d of %d programmes",
    sum(M$q_equity_ind == 1, na.rm = TRUE), nrow(M))

# -----------------------------------------------------------------------------
# 3.9 | Typology-specific upgrading priorities
# Derived mechanically from each typology's weakest requirements, not asserted.
# -----------------------------------------------------------------------------
upgrade <- read.csv(file.path(CFG$out_tab, "10_readiness_gaps_by_typology.csv")) %>%
  as_tibble() %>%
  group_by(typology_k) %>%
  arrange(pct_documented, .by_group = TRUE) %>%
  slice_head(n = 3) %>%
  summarise(priority_upgrades = paste(requirement, collapse = " | "),
            weakest_pct_documented = paste(round(pct_documented, 1), collapse = " | "),
            .groups = "drop")
save_tab(upgrade, "11_typology_upgrading_priorities")

# -----------------------------------------------------------------------------
# 3.10 | CITY PROGRAMME PORTFOLIOS (named cases, not a modelled level)
# Grouping on the parent city recorded in A-1 recovers genuine multi-programme
# portfolios. These are reported as named cases: the corpus supports portfolio
# COVERAGE analysis for a handful of cities, not a city-level model.
# -----------------------------------------------------------------------------
# EXPORTS
# -----------------------------------------------------------------------------
analysis_ready <- M %>%
  select(case_id, city, country, region = region_mod, typology_k, context_type,
         obj_class, portfolio, tech_demand, maint_demand, spatial_response, equity_target,
         access_route, access_fit, participation_scope,
         tech_grade, n_enablers, tech_fit,
         payment_config, coverage_scope, incentive_basis, fit_D3b,
         admin_profile, learn_score, comply_score,
         steward_config, steward_fit, monitor_config,
         eco_fit,
         starts_with("mf_"), starts_with("wk_"), n_weak, n_misfit,
         n_domains_evaluable, n_url_unique, notstated_share, inferred_share,
         city_group, n_prog_in_city,
         pop, density, renter_pct, climate_grp, hx_flood, hx_heat, hx_drought,
         lead_body, delivery_arch, funding_source, funding_horizon, year_launched)
utils::write.csv(analysis_ready, file.path(CFG$out_der, "analysis_ready_cases.csv"), row.names = FALSE)

tabs <- list.files(CFG$out_tab, pattern = "\\.csv$", full.names = TRUE)
wb <- setNames(lapply(tabs, function(f) utils::read.csv(f, check.names = FALSE)),
               substr(tools::file_path_sans_ext(basename(tabs)), 1, 31))
writexl::write_xlsx(wb, file.path(CFG$out_der, "NbS_Programme_Fitness_Results.xlsx"))

msg("Reporting complete: %d tables, 4 figures, analysis-ready dataset (%d cases, %d variables)",
    length(tabs), nrow(analysis_ready), ncol(analysis_ready))
