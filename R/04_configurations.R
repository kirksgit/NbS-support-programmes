# =============================================================================
# 04_configurations.R  |  OBSERVED CONFIGURATIONS -- REPORTED BEFORE GROUPING
#
# Standing rule for this project: no composite is presented until its components
# have been shown. "Two enablers" is meaningless until the reader knows WHICH
# two and how often that specific pair occurs. This module therefore produces,
# for every block that is later summarised:
#   (a) item-level prevalence, keeping Yes / No / Not stated distinct;
#   (b) the exact combinations observed, named in full, with counts;
#   (c) inventories of the free-text fields the composites otherwise compress.
# Nothing is judged here. Classification happens only in 05_fitness.R, and
# 12_classification_audit.R traces every label back to the rows below.
# =============================================================================

DET <- arch %>% left_join(ctx %>% select(case_id, country, city, region), by = "case_id")

# -----------------------------------------------------------------------------
# (a) ITEM-LEVEL PREVALENCE, THREE-WAY
# The distinction between an explicit "No" and silence carries the whole weight
# of the confirmed-misfit rules, so it is never collapsed.
# -----------------------------------------------------------------------------
three_way <- function(code, label, block) {
  col <- paste0("v_", code)
  if (!col %in% names(raw)) return(NULL)
  t <- ternary(raw[[col]])
  tibble(block = block, variable = code, item = label,
         yes = sum(t == "yes", na.rm = TRUE),
         partial = sum(t == "partial", na.rm = TRUE),
         no = sum(t == "no", na.rm = TRUE),
         not_stated = sum(t == "not_stated", na.rm = TRUE),
         not_applicable_or_unparsed = sum(is.na(t))) %>%
    mutate(pct_yes = round(100 * yes / nrow(raw), 1))
}

item_index <- tribble(
  ~code, ~label,                              ~block,
  "E1",  "Green roofs",                       "Measures",
  "E2",  "Green facades",                     "Measures",
  "E3",  "Rain gardens / bioretention",       "Measures",
  "E4",  "Depaving / permeable paving",       "Measures",
  "E5",  "Trees / tree pits",                 "Measures",
  "E6",  "Rain barrels / cisterns",           "Measures",
  "C5",  "Technical standards cited",         "Technical enablers",
  "C6",  "Permit fast-track",                 "Technical enablers",
  "C7",  "Permit fee waiver",                 "Technical enablers",
  "D8",  "Housing-type guidance",             "Technical enablers",
  "C8",  "Homeowners eligible",               "Actor eligibility",
  "C9",  "Landlords eligible",                "Actor eligibility",
  "C10", "Tenants eligible",                  "Actor eligibility",
  "C11", "Co-ops / HOAs eligible",            "Actor eligibility",
  "C12", "Schools eligible",                  "Actor eligibility",
  "C13", "NGOs eligible",                     "Actor eligibility",
  "C14", "SMEs eligible",                     "Actor eligibility",
  "C15", "Social housing eligible",           "Actor eligibility",
  "C16", "Informal groups eligible",          "Actor eligibility",
  "D7",  "Tenant pathway documented",         "Procedural",
  "D1",  "Step-by-step applicant guide",      "Procedural",
  "D2",  "Timelines stated",                  "Procedural",
  "D3",  "Forms accessible",                  "Procedural",
  "D6",  "Co-design documented",              "Procedural",
  "C1",  "Lead department named",             "Delivery",
  "C2",  "Multi-department involvement",      "Delivery",
  "C3",  "Coordination office",               "Delivery",
  "C4",  "Delivery partners named",           "Delivery",
  "E16", "Ongoing maintenance covered",       "Post-installation",
  "E17", "Maintenance as a condition",        "Post-installation",
  "F1",  "M&E indicators published",          "Monitoring",
  "F3",  "Periodic review stated",            "Monitoring",
  "G1",  "Informal groups acknowledged",      "Recognition",
  "G3",  "Civil society in design",           "Recognition",
  "G5",  "Equity indicators tracked",         "Recognition",
  "G6",  "Informal action acknowledged",      "Recognition"
)
item_prev <- pmap_dfr(list(item_index$code, item_index$label, item_index$block), three_way)
save_tab(item_prev, "04_item_prevalence_three_way")

# -----------------------------------------------------------------------------
# (b) OBSERVED COMBINATIONS, NAMED IN FULL
# Helper: build a readable signature from a set of 0/1 indicators.
# -----------------------------------------------------------------------------
signature <- function(df, cols, labels, none = "none documented") {
  m <- as.matrix(df[, cols, drop = FALSE])
  apply(m, 1, function(r) {
    hit <- labels[which(r == 1)]
    if (!length(hit)) none else paste(hit, collapse = " + ")
  })
}

combo_table <- function(sig, block, group = NULL) {
  d <- tibble(block = block, configuration = sig)
  if (!is.null(group)) d$grouping_later_applied <- group
  d %>% count(across(everything()), name = "n") %>%
    mutate(pct = round(100 * n / length(sig), 1)) %>%
    arrange(desc(n))
}

## --- Measure portfolios: every observed combination of supported measures ----
## THIS MODULE ENUMERATED ITS OWN MEASURE SET.
## It listed ten columns including m_greywater (labelled "greywater/harvesting",
## a category the taxonomy audit explicitly excludes) while OMITTING m_turf and
## m_pond. So the configuration signatures -- which feed the MCA and therefore
## the typology -- were built on a set that is neither the canonical eleven nor
## the fifteen coded. Three of R5's four named turf-only records showed as
## "native/drought planting" and 655 Santa Monica showed as "none documented".
## R5 requires ONE set, read from one place, by every consumer.
meas_cols <- MEASURES$var
meas_lab  <- tolower(MEASURES$measure)
DET$sig_measures <- signature(DET, meas_cols, meas_lab)
save_tab(combo_table(DET$sig_measures, "Measure portfolio", DET$portfolio),
         "04_config_measure_portfolios")

## --- Technical enablers: WHICH enablers, not how many ------------------------
enab_cols <- c("r_standards","r_fasttrack","r_feewaiver","p_housetype","i_advice","cost_design")
enab_lab  <- c("technical standards (C-5)","permit fast-track (C-6)","permit fee waiver (C-7)",
               "housing-type guidance (D-8)","technical advice (E-13)","design costs eligible (E-21)")
DET$sig_enablers <- signature(DET, enab_cols, enab_lab)
enab_tab <- combo_table(DET$sig_enablers, "Technical enablement",
                        paste0(rowSums(DET[, enab_cols] == 1, na.rm = TRUE), " enabler(s)"))
save_tab(enab_tab, "04_config_technical_enablers")
msg("Technical enablement: %d distinct enabler combinations observed; the most common is '%s' (n = %d)",
    nrow(enab_tab), enab_tab$configuration[1], enab_tab$n[1])

## --- Technical advice: which specific types ---------------------------------
save_tab(combo_table(signature(DET, c("adv_guides","adv_helpdesk","adv_sitevisits"),
                               c("guides","helpdesk","site visits")), "Technical advice types"),
         "04_config_advice_types")

## --- Actor eligibility: exact eligible-actor sets ---------------------------
act_cols <- c("el_home","el_land","el_tenant_direct","el_tenant_cond","el_coop","el_school",
              "el_ngo","el_sme","el_socho","el_inform")
act_lab  <- c("homeowners","landlords","tenants (direct)","tenants (consent-dependent)",
              "co-ops/HOAs","schools","NGOs","SMEs","social housing","informal groups")
DET$sig_actors <- signature(DET, act_cols, act_lab, none = "no actor documented")
save_tab(combo_table(DET$sig_actors, "Actor eligibility"), "04_config_actor_sets")

## Explicit exclusions, reported separately from silence
excl_cols <- c("ex_home","ex_land","ex_tenant","ex_coop","ex_socho")
excl_lab  <- c("homeowners excluded","landlords excluded","tenants excluded",
               "co-ops/HOAs excluded","social housing excluded")
save_tab(combo_table(signature(DET, excl_cols, excl_lab, none = "no explicit exclusion"),
                     "Explicit actor exclusions"), "04_config_explicit_exclusions")

## --- Instruments: exact bundles ---------------------------------------------
inst_cols <- c("i_grant","i_taxreb","i_loan","i_discount","i_inkind","i_advice","i_reward")
inst_lab  <- c("capital grant","tax/fee rebate","loan","discounted purchase","in-kind provision",
               "technical advice","behavioural reward")
DET$sig_instruments <- signature(DET, inst_cols, inst_lab)
save_tab(combo_table(DET$sig_instruments, "Instrument bundle", DET$instrument_pkg),
         "04_config_instrument_bundles")

## --- Financial calibration: timing x basis x coverage x eligible costs -------
save_tab(DET %>%
           transmute(disbursement = replace_na(disbursement, "unparsed"),
                     incentive_basis = replace_na(incentive_basis, "unparsed"),
                     coverage_scope = replace_na(coverage_scope, "not stated"),
                     eligible_costs = signature(DET,
                       c("cost_design","cost_install","cost_permit","cost_maint","cost_monitor"),
                       c("design","installation","permitting","maintenance","monitoring"))) %>%
           count(disbursement, incentive_basis, coverage_scope, eligible_costs, name = "n") %>%
           arrange(desc(n)) %>% mutate(pct = round(100 * n / nrow(DET), 1)),
         "04_config_financial_calibration")

## --- Maximum public contribution: actual documented amounts -----------------
save_tab(DET %>% filter(!is.na(cap_amount)) %>%
           count(cap_currency, cap_band, name = "n") %>% arrange(cap_currency, cap_band),
         "04_config_contribution_caps")
save_tab(DET %>% filter(!is.na(cap_amount)) %>%
           group_by(cap_currency) %>%
           summarise(n = n(), min = min(cap_amount), q25 = quantile(cap_amount, .25),
                     median = median(cap_amount), q75 = quantile(cap_amount, .75),
                     max = max(cap_amount), .groups = "drop"),
         "04_config_contribution_cap_distribution")

## --- Administrative provisions: which provisions, not a score ---------------
adm_cols <- c("p_timeline","p_multiling","p_housetype","r_fasttrack","r_feewaiver","tenant_pathway")
adm_lab  <- c("timelines stated","multilingual materials","housing-type guidance",
              "permit fast-track","permit fee waiver","tenant pathway")
DET$sig_admin <- signature(DET, adm_cols, adm_lab)
save_tab(DET %>% transmute(guide = replace_na(d1_guide, "unparsed"),
                           forms = replace_na(d3_forms, "unparsed"),
                           other_provisions = DET$sig_admin) %>%
           count(guide, forms, other_provisions, name = "n") %>% arrange(desc(n)) %>%
           mutate(pct = round(100 * n / nrow(DET), 1)),
         "04_config_administrative_provisions")

## --- Application channels and languages -------------------------------------
save_tab(combo_table(signature(DET,
           c("ch_online","ch_email","ch_paper","ch_thirdparty","ch_inperson","ch_other"),
           c("online form","email","paper/mail","third-party portal","in-person","other")),
           "Application channels"), "04_config_application_channels")
save_tab(DET %>% count(n_languages, language_profile, name = "n") %>% arrange(desc(n)),
         "04_config_languages")

## --- Post-installation: exact element combinations --------------------------------
DET$sig_steward <- signature(DET,
  c("st_resp_named","s_condition","s_maintgrant","s_maintcost","s_guides","s_helpdesk",
    "s_sitevisit","s_reminders"),
  c("responsibility named","binding condition","maintenance grant","maintenance costs covered",
    "guides","helpdesk","site visits","inspection reminders"))
save_tab(DET %>% transmute(responsibility = replace_na(s_respons, "not stated"),
                           elements = DET$sig_steward,
                           condition_years = replace_na(as.character(s_duration), "not stated")) %>%
           count(responsibility, elements, condition_years, name = "n") %>% arrange(desc(n)),
         "04_config_stewardship")

## --- Monitoring: exact element combinations ---------------------------------
DET$sig_monitor <- signature(DET, c("f_me_published","f_review","f_feedback"),
                             c("published indicators","periodic review","feedback channel"))
save_tab(DET %>% transmute(reporting_type = replace_na(f_report_type, "unparsed"),
                           elements = DET$sig_monitor,
                           feedback_types = replace_na(feedback_types, "none")) %>%
           count(reporting_type, elements, feedback_types, name = "n") %>% arrange(desc(n)),
         "04_config_monitoring")

## --- Spatial and equity targeting: raw cross-tabulation ---------------------
save_tab(DET %>% count(spatial_target = replace_na(spatial_target, "unparsed"),
                       equity_target  = replace_na(equity_target, "unparsed"), name = "n") %>%
           arrange(desc(n)), "04_config_targeting_crosstab")

## --- Governance and funding -------------------------------------------------
save_tab(DET %>% count(lead_body = replace_na(as.character(lead_body), "unparsed"),
                       delivery = replace_na(as.character(delivery_arch), "unparsed"),
                       funding_source = replace_na(funding_source, "unparsed"),
                       funding_horizon = replace_na(funding_horizon, "unparsed"), name = "n") %>%
           arrange(desc(n)), "04_config_governance_funding")

# -----------------------------------------------------------------------------
# (c) FREE-TEXT INVENTORIES
# E-7, E-14, E-12 and A-7i carry detail that no coded variable captures. They
# are inventoried verbatim so that nothing is lost behind the keyword flags.
# -----------------------------------------------------------------------------
freetext_inventory <- function(code, label) {
  col <- paste0("v_", code)
  s <- strip_qual(raw[[col]])
  keep <- !is.na(s) & s != "" &
    !str_detect(s, regex("^(not stated|not applicable|none|cannot verify)", ignore_case = TRUE))
  tibble(field = label, case_id = raw$case_id[keep], city = raw$city[keep],
         country = raw$country[keep], text = s[keep])
}
save_tab(bind_rows(freetext_inventory("E7",  "E-7 other supported measures"),
                   freetext_inventory("E14", "E-14 other instruments"),
                   freetext_inventory("E12", "E-12 behavioural reward"),
                   freetext_inventory("E25", "E-25 cost coverage scope"),
                   freetext_inventory("A7i", "A-7i topography")),
         "04_freetext_inventory")

## Coded taxonomy derived from the free text, with counts (the flags used later)
save_tab(tibble(
  field = c(rep("E-7 other measures", 8), rep("E-14 other instruments", 5),
            rep("A-7i topography", 6)),
  category = c("infiltration structures","downspout disconnection","native/drought planting",
               "turf replacement","greywater/harvesting","composting","ponds/wetlands","soil/mulch",
               "free materials","workshops/education","contractor or vendor list",
               "vouchers/coupons","bill credit",
               "coastal","river/valley","lowland/flat","hilly/sloping","elevated/plateau","karst/other"),
  n = c(sum(DET$m_infiltr == 1, na.rm = TRUE), sum(DET$m_disconn == 1, na.rm = TRUE),
        sum(DET$m_planting == 1, na.rm = TRUE), sum(DET$m_turf == 1, na.rm = TRUE),
        sum(DET$m_greywater == 1, na.rm = TRUE), sum(DET$m_compost == 1, na.rm = TRUE),
        sum(DET$m_pond == 1, na.rm = TRUE), sum(DET$m_soil == 1, na.rm = TRUE),
        sum(DET$x_freematerials == 1, na.rm = TRUE), sum(DET$x_workshops == 1, na.rm = TRUE),
        sum(DET$x_contractorlist == 1, na.rm = TRUE), sum(DET$x_voucher == 1, na.rm = TRUE),
        sum(DET$x_billcredit == 1, na.rm = TRUE),
        sum(DET$topo_coastal == 1, na.rm = TRUE), sum(DET$topo_river == 1, na.rm = TRUE),
        sum(DET$topo_flat == 1, na.rm = TRUE), sum(DET$topo_hilly == 1, na.rm = TRUE),
        sum(DET$topo_elevated == 1, na.rm = TRUE), sum(DET$topo_other == 1, na.rm = TRUE))) %>%
  mutate(pct = round(100 * n / nrow(DET), 1)),
  "04_freetext_derived_categories")

## Signatures are carried forward so the audit in 12 can trace every label
config_signatures <- DET %>%
  select(case_id, sig_measures, sig_enablers, sig_actors, sig_instruments,
         sig_admin, sig_steward, sig_monitor)
save_tab(config_signatures, "04_case_configuration_signatures")

msg("Configuration inventories written: %d measure combinations, %d actor sets, %d instrument bundles",
    n_distinct(DET$sig_measures), n_distinct(DET$sig_actors), n_distinct(DET$sig_instruments))
