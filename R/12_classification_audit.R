# =============================================================================
# 12_classification_audit.R  |  ARE THE CONCLUSIONS RULE-DRIVEN?
#
# Every fit label rests on a threshold set by judgement. This module makes
# that judgement inspectable in two ways:
#   (a) a crosswalk showing WHICH raw configurations produced each label, so a
#       reader can see the evidence behind "documented fit" without trusting it;
#   (b) alternative defensible thresholds, re-run, so the reader can see how
#       much of each conclusion survives a different rule.
# If a finding only holds under one threshold, it is a rule artefact and must be
# reported as such.
# =============================================================================

AUD <- M %>% left_join(config_signatures, by = "case_id")

# -----------------------------------------------------------------------------
# (a) LABEL -> RAW CONFIGURATION CROSSWALK
# -----------------------------------------------------------------------------
crosswalk <- function(label_var, sig_var, block) {
  AUD %>%
    transmute(block = block,
              assigned_label = as.character(.data[[label_var]]),
              raw_configuration = .data[[sig_var]]) %>%
    count(block, assigned_label, raw_configuration, name = "n") %>%
    group_by(assigned_label) %>%
    mutate(pct_of_label = round(100 * n / sum(n), 1)) %>%
    ungroup() %>% arrange(assigned_label, desc(n))
}

save_tab(bind_rows(
  crosswalk("tech_grade",     "sig_enablers",    "Technical enablement"),
  crosswalk("access_route",   "sig_actors",      "Tenure & authority access"),
  crosswalk("steward_config", "sig_steward",     "Post-installation"),
  crosswalk("monitor_config", "sig_monitor",     "Monitoring"),
  crosswalk("instrument_pkg", "sig_instruments", "Instrument package"),
  crosswalk("portfolio",      "sig_measures",    "Measure portfolio")
), "12_label_to_configuration_crosswalk")

# -----------------------------------------------------------------------------
# (b) THRESHOLD SENSITIVITY
# Each rule is re-run under alternatives that a reasonable reviewer might
# prefer. Reported as the share of cases whose label changes.
# -----------------------------------------------------------------------------
## v19 REWRITTEN AGAINST THE CURRENT CONSTRUCTS. Four of the six alternatives
## below tested rules the framework NO LONGER RUNS: n_enablers cuts for D3a and
## learn_score/comply_score cuts for D3c were both replaced at v13 when those
## components moved from provision COUNTING to requirement MATCHING. An audit of
## a retired rule certifies nothing, and the §0.8 spread table in the framework
## document was quoting those retired figures. Each alternative is now a rule a
## reviewer could actually prefer over the one in force, and each names the
## literature position it represents.
alt <- AUD %>% mutate(

  ## D3A. Baseline: the demand class decides which technical element is required.
  ## ALTERNATIVE (Nawrath et al. 2026; Gunawardena & Steemers 2020): treat design
  ## advice as necessary for EVERY demand class, not only the ground-modifying
  ## one -- the maintenance-and-installation literature holds that site-specific
  ## advice is what carries an installation, whatever the measure.
  tech_fit_alt_advice = ifelse(tech_applicable == 1,
                               ifelse(coalesce(adv_element, 0L) == 1, "Documented fit",
                               ifelse(coalesce(spec_element, 0L) == 1 |
                                      coalesce(guid_element, 0L) == 1 |
                                      coalesce(est_element, 0L) == 1,
                                      "Partial fit", "Indeterminate")), NA),

  ## D2. Baseline: reach >= requirement, three ordered bands.
  ## ALTERNATIVE A (Ástmarsson et al. 2013; Snep et al. 2023 -- the landlord/
  ## tenant dilemma): consent-dependence is a real route, so count it as fit.
  access_fit_alt = case_when(
    access_route == "Permission-dependent pathway" ~ "Documented fit",
    TRUE ~ access_fit),
  ## ALTERNATIVE B (Pellerey & Giezen 2026; Matsler et al. 2023): owner-only
  ## design in a renter-majority city is not a partial provision, it is a
  ## documented exclusion of the majority of residents.
  access_fit_alt_strict = case_when(
    access_route == "Owner-restricted pathway" & renter_pct >= 50 ~ "Confirmed misfit",
    TRUE ~ access_fit),

  ## D3C. Baseline: KNOW core (D-1) AND LODGE core (D-3), each satisfiable once.
  ## ALTERNATIVE (Matsler et al. 2023, equity scan of GSI incentive programmes):
  ## a procedure is only accessible if it also reaches beyond the default
  ## applicant, so require a widening provision on top of both cores.
  admin_alt_strict = case_when(
    is.na(d3c_know) & is.na(d3c_lodge)                              ~ "Indeterminate",
    d3c_know == 1 & d3c_lodge == 1 & coalesce(d3c_widen, 0L) >= 1   ~ "Documented fit",
    d3c_know == 1 & d3c_lodge == 1                                  ~ "Partial fit",
    d3c_know == 1 | d3c_lodge == 1                                  ~ "Partial fit",
    TRUE                                                            ~ "Confirmed misfit"),

  ## D4A. Baseline: responsibility plus any support form (financial or not).
  ## SENSITIVITY, NOT A PREFERRED ALTERNATIVE. The framework text settles this:
  ## "Assigning a maintenance responsibility without a public grant is not itself
  ## a misfit; the test is whether responsibilities, obligations and any support
  ## are documented and internally workable." A funded-only rule is also
  ## impractical -- few municipalities can commit recurrent maintenance budgets to
  ## private parcels, so it would score the delivery model itself as a misfit
  ## rather than measuring design. It is computed because Toxopeus & Polzin (2021)
  ## and Nawrath et al. (2026) show funding is the binding constraint, and the gap
  ## between the two readings is the finding: support here is advisory, not
  ## funded. Report the pair; never substitute the strict rule for the base.
  ##
  ## "FUNDED ONLY" WAS THE WRONG STRICT READING, AND TESTING THE RIGHT ONE
  ## SHOWED THE BASELINE ALREADY IMPLEMENTS IT.
  ##
  ## Cash is too narrow a test of whether a citizen is left alone with a
  ## maintenance duty: a delivered replacement plant, a maintenance site visit or
  ## a named advice line answers that as genuinely as a grant. The defensible
  ## strict rule is SUSTAINED SUPPORT IN ANY SUBSTANTIVE FORM -- cash, cost
  ## coverage, materials in kind, or a technical service. Computing it returns
  ## EXACTLY the baseline (393 = 393, 0 records moved), because the baseline
  ## already accepts non-financial support. The reviewer's preferred reading is
  ## therefore the rule in force, not an alternative to it, and no "report both
  ## figures" instruction is needed for D4a.
  ##
  ## What remains reportable is the DECOMPOSITION, not a rival rule: of the 393
  ## records with responsibility plus support, only 22 carry cash (a maintenance
  ## grant or cost coverage) and 371 rest on non-financial support alone. That is
  ## the substantive finding -- maintenance backing in this corpus is
  ## overwhelmingly advisory or in-kind -- and it is a descriptive split of the
  ## baseline, not a competing verdict. The variable below computes it so the
  ## audit table keeps a discriminating row; read it as a decomposition.
  ##
  ## The 5-year horizon is the right question and cannot be asked of this corpus:
  ## no variable records support DURATION, so a "first five years" test would be
  ## invented rather than read. Stated as a limitation in the framework, never
  ## approximated by the cash split.
  steward_fit_alt = case_when(
    steward_config == "No maintenance architecture documented" ~ "Indeterminate",
    grepl("^Responsibility", as.character(steward_config)) &
      (coalesce(s_maintgrant, 0L) == 1 | coalesce(s_maintcost, 0L) == 1) ~ "Documented fit",
    grepl("^Responsibility", as.character(steward_config))     ~ "Partial fit",
    TRUE                                                       ~ "Indeterminate"),

  ## D4B. Baseline: a webpage summary counts as a met element at partial strength.
  ## ALTERNATIVE (Hölscher et al. 2023, on mainstreaming and learning routines):
  ## only a formal published indicator or periodic review is a persistence check.
  monitor_fit_alt = case_when(
    coalesce(d4b_looks, 0L) >= 1 & coalesce(d4b_web, 0L) == 0 ~ "Documented fit",
    coalesce(d4b_looks, 0L) >= 1 | coalesce(f_review, 0L) == 1 ~ "Partial fit",
    coalesce(d4b_absent, 0L) >= 1                              ~ "Confirmed misfit",
    TRUE                                                       ~ "Indeterminate"),

  ## D1. Baseline: every stated objective matched by a functionally plausible
  ## measure. ALTERNATIVE (Zheng et al. 2021; Kumar et al. 2024 -- performance
  ## varies by measure and climate): require the portfolio to carry at least two
  ## distinct measures, so a single-measure programme cannot claim full match.
  ## THE PORTFOLIO TEST NOW APPLIES ONLY WHERE THE PROGRAMME'S OWN OBJECTIVES
  ## DEMAND IT. A flat "2+ measures or not a full fit" penalises the single-measure
  ## programme that is correctly scoped -- a drought city running one turf-conversion
  ## rebate has matched its stated objective with the measure that delivers it, and
  ## nothing in Zheng (2021) or Kumar (2024) says otherwise. What those sources do
  ## support is narrower: cooling, retention and habitat are delivered by DIFFERENT
  ## measures, so a programme claiming several distinct objectives cannot deliver
  ## them all through one. The requirement is therefore conditional on obj_n: a
  ## single-objective programme may fully fit on one measure; a multi-objective one
  ## needs a portfolio at least as wide as the number of objectives it claims,
  ## capped at 2 so the test stays a floor rather than a sliding scale.
  eco_fit_alt = case_when(
    is.na(eco_applicable) | eco_applicable == 0        ~ "Indeterminate",
    n_obj_matched == obj_n &
      coalesce(n_measures, 0L) >= pmin(coalesce(obj_n, 1L), 2L) ~ "Documented fit",
    n_obj_matched > 0                                  ~ "Partial fit",
    TRUE                                               ~ "Confirmed misfit")
)

shift <- function(base, alt_var, rule) {
  b <- as.character(alt[[base]]); a <- as.character(alt[[alt_var]])
  keep <- !is.na(b) & !is.na(a)
  tibble(baseline_variable = base, alternative_rule = rule,
         n_compared = sum(keep),
         n_changed = sum(b[keep] != a[keep]),
         pct_changed = round(100 * mean(b[keep] != a[keep]), 1),
         ## the second disjunct was "Two to three enablers", a level from the
         ## retired counting design. It inflated the baseline fit share for D3a and
         ## matched nothing for the other six.
         pct_fit_baseline = round(100 * mean(b[keep] == "Documented fit"), 1),
         pct_fit_alternative = round(100 * mean(a[keep] == "Documented fit"), 1))
}

sens <- bind_rows(
  shift("eco_fit",     "eco_fit_alt",           "D1: multi-objective programmes need a portfolio as wide as their objective count (Zheng 2021; Kumar 2024)"),
  shift("access_fit",  "access_fit_alt",        "D2: consent-dependent tenant route counts as fit (Astmarsson 2013; Snep 2023)"),
  shift("access_fit",  "access_fit_alt_strict", "D2: owner-only design in renter-majority cities = misfit (Pellerey 2026; Matsler 2023)"),
  shift("tech_fit",    "tech_fit_alt_advice",   "D3a: design advice required for every demand class (Nawrath 2026; Gunawardena 2020)"),
  shift("fit_D3c",     "admin_alt_strict",      "D3c: both cores AND a widening provision (Matsler 2023 equity scan)"),
  ## NOT a rival rule -- a decomposition of the baseline. The substantive-support
  ## reading (cash OR in-kind OR technical service) returns the baseline exactly,
  ## so this row isolates how much of that baseline rests on cash: 22 of 393.
  shift("steward_fit", "steward_fit_alt",       "D4a DECOMPOSITION (not a rival rule): how much of the baseline rests on CASH rather than in-kind or technical support (Toxopeus 2021; Nawrath 2026)"),
  shift("fit_D4b",     "monitor_fit_alt",       "D4b: webpage summaries do not count as a persistence check (Holscher 2023)")
)
save_tab(sens, "12_threshold_sensitivity")

## Do the headline conclusions survive? Re-run the compound-weakness count under
## the stricter access rule, which is the threshold most likely to be contested.
alt_weak <- alt %>%
  mutate(wk_access_alt = ifelse(access_fit_alt_strict == "Indeterminate", NA_integer_,
                                as.integer(access_fit_alt_strict == "Confirmed misfit" |
                                           access_route == "Owner-restricted pathway"))) %>%
  filter(!is.na(wk_access_alt), !is.na(wk_tech), !is.na(wk_admin),
         !is.na(wk_steward), !is.na(wk_monitor)) %>%
  mutate(n_weak_alt = wk_access_alt + wk_tech + wk_admin + wk_steward + wk_monitor)
save_tab(tibble(rule = c("baseline", "stricter access rule"),
                n = c(sum(!is.na(M$n_weak)), nrow(alt_weak)),
                pct_3plus = c(round(100 * mean(M$n_weak >= 3, na.rm = TRUE), 1),
                              round(100 * mean(alt_weak$n_weak_alt >= 3), 1))),
         "12_compound_weakness_under_alternative_rules")

## -----------------------------------------------------------------------------
## DOWNSPOUT DISCONNECTION: does it qualify as a citizen-led NbS measure?
## Disconnection is a plumbing change. It is nature-based only where the flow is
## sent to a vegetated or infiltrating receiver. The test is about the STATUS OF
## THE MEASURE, not the eligibility of the programme: a programme that supports
## other NbS measures stays in the corpus regardless, because its NbS validity
## rests on those measures. Three verdicts:
##   NBS (receiver documented)   - a vegetated/infiltrating receiver is named
##   NBS-BY-PORTFOLIO            - no receiver named, but the programme funds a
##                                 vegetated/infiltrating measure, so the
##                                 disconnection sits inside an NbS package
##   GREY (adjunct only)         - no receiver named and no vegetated/infiltrating
##                                 measure. The DISCONNECTION is counted as a grey
##                                 hybrid adjunct, not as an NbS measure. Removal
##                                 from the corpus applies ONLY where the
##                                 programme also supports no other NbS measure
##                                 at all (column programme_supports_other_nbs).
## -----------------------------------------------------------------------------
RECEIVER_RX <- regex(paste0(
  "garden|rain ?garden|bioreten|planted|planting|bed\\b|lawn|vegetat|soakaway|",
  "soak ?away|infiltrat|dry ?well|swale|tree|pervious|permeable|splash ?block|",
  "grass|soil|dispers|Versickerung|Garten|Beet|Mulde|Rasen"), ignore_case = TRUE)

VEG_INFIL_VARS <- c("m_greenroof", "m_facade", "m_raingard", "m_pond",
                    "m_infiltr", "m_depave", "m_trees", "m_planting", "m_turf")

ANY_NBS_VARS <- c(VEG_INFIL_VARS, "m_barrel")   # barrels/cisterns are the hybrid
                                                # capture family, still an NbS-
                                                # portfolio member

disc_audit <- M %>%
  filter(m_disconn == 1) %>%
  mutate(e7_text = strip_qual(raw$v_E7[match(case_id, raw$case_id)]),
         receiver_documented = str_detect(replace_na(e7_text, ""), RECEIVER_RX),
         other_veg_infil = rowSums(across(all_of(
           intersect(VEG_INFIL_VARS, names(M)))) == 1, na.rm = TRUE) > 0,
         programme_supports_other_nbs = rowSums(across(all_of(
           intersect(ANY_NBS_VARS, names(M)))) == 1, na.rm = TRUE) > 0) %>%
  select(case_id, city, country, e7_text, receiver_documented, other_veg_infil,
         programme_supports_other_nbs) %>%
  mutate(measure_status = case_when(
    receiver_documented ~ "NbS (vegetated/infiltrating receiver documented)",
    other_veg_infil     ~ "NbS by portfolio (no receiver named; programme funds a vegetated/infiltrating measure)",
    TRUE                ~ "Grey hybrid adjunct (no receiver named, no vegetated/infiltrating measure)"),
    corpus_action = if_else(programme_supports_other_nbs,
      "RETAIN programme (its NbS validity rests on its other supported measures)",
      "REVIEW for removal (programme supports no other NbS measure)"))
save_tab(disc_audit, "12_downspout_nbs_qualification_audit")

## records where the DISCONNECTION itself is grey, and (separately) the much
## narrower set where the PROGRAMME would need review
disc_grey <- disc_audit %>% filter(startsWith(measure_status, "Grey hybrid"))
save_tab(disc_grey, "12_downspout_grey_adjunct_records")
disc_remove <- disc_audit %>% filter(startsWith(corpus_action, "REVIEW"))
save_tab(disc_remove, "12_downspout_programmes_for_removal_review")

msg(paste("Downspout audit: %d records flag disconnection -- %d NbS (receiver named),",
          "%d NbS by portfolio, %d grey adjunct. Programmes needing removal review: %d",
          "(all others retain NbS validity through other supported measures)."),
    nrow(disc_audit), sum(disc_audit$receiver_documented),
    sum(!disc_audit$receiver_documented & disc_audit$other_veg_infil),
    nrow(disc_grey), nrow(disc_remove))

msg("Classification audit: %d threshold variants tested; largest label shift %.1f%% (%s)",
    nrow(sens), max(sens$pct_changed),
    sens$alternative_rule[which.max(sens$pct_changed)])
