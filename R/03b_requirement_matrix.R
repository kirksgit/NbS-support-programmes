# =============================================================================
# 03b_requirement_matrix.R  |  WHAT EACH MEASURE ACTUALLY NEEDS
#
# The earlier technical test counted enabling provisions. Counting treats a
# permit-fee waiver plus housing guidance as equivalent to structural standards
# plus professional site advice, which they are not. This module replaces the
# count with a REQUIREMENT MATCH: each supported measure carries a set of
# provisions it needs, the programme's requirement set is the union across the
# measures it supports, and fit asks how much of that set is documented.
#
# The five provision types follow the implementation literature:
#
#   P1 Specification    technical standards, sizing rules, material or
#                       structural specifications. Required wherever the
#                       intervention bears load, holds water or must be sized to
#                       a contributing area.
#   P2 Site assessment  structural, soil or infiltration assessment, or funded
#                       professional design. Required wherever performance
#                       depends on site conditions that the applicant cannot
#                       establish unaided (roof load capacity, infiltration
#                       rate, groundwater depth, utility conflicts).
#   P3 Regulatory       permit fast-track or fee waiver. Required where the work
#                       is typically permit-dependent: building envelope
#                       alterations, surface removal, drainage connections.
#   P4 Guidance         applicant guides or housing-type-specific guidance.
#                       Required for every measure: administrative-burden
#                       theory identifies learning cost as the first barrier to
#                       participation, and it applies even to simple measures.
#   P5 Establishment    documented maintenance or establishment support.
#                       Required where the intervention's function depends on
#                       vegetation surviving an establishment period, or on
#                       periodic clearing to keep working.
#
# The tiering follows the technical demand classes already in the architecture
# layer: high (building-integrated), medium (ground-modifying), low
# (standardised distribution).
#
# This matrix is a judgement and is exported so it can be contested. Change a
# cell here and the technical results change accordingly; 12_classification_audit
# reports how far the labels move.
# =============================================================================

REQUIREMENT_MATRIX <- tribble(
  ~measure_var,   ~measure,                    ~demand_class, ~P1_specification, ~P2_site_assessment, ~P3_regulatory, ~P4_guidance, ~P5_establishment, ~rationale,

  "m_greenroof",  "Green roofs",               "High",
  "required", "required", "required", "required", "required",
  "Load-bearing capacity, waterproofing and root barrier, drainage layer and substrate depth are all specification-dependent; retrofit almost always requires structural verification and, in most jurisdictions, a permit. REASSESSED, cells unchanged and confirmed: programme guidance in this corpus and in practice commonly requires a load analysis from a licensed structural engineer and installation by a certified contractor, i.e. green-roof retrofit is explicitly not a self-install measure. Vegetation must survive establishment for any of the claimed functions to occur.",

  "m_facade",     "Green facades",             "High",
  "required", "required", "recommended", "required", "required",
  "Attachment loads, wall moisture management and irrigation are specification-dependent, and access for maintenance must be designed in. Permit requirements vary more than for roofs, so regulatory support is recommended rather than required.",

  "m_raingard",   "Rain gardens",              "Medium",
  "required", "required", "recommended", "required", "required",
  "Performance depends on sizing to the contributing area, on infiltration rate, on soil-media specification and on setbacks from foundations and utilities, none of which a household can establish unaided. REASSESSED, cells unchanged and now better evidenced: incorrect soil media is reported as the leading cause of bioretention failure, and a controlled study of established homeowner-maintained rain gardens found they stored and infiltrated only about 40% of the design storm on average -- limited by construction and berm grading rather than by infiltration rate. Specification and site assessment are therefore genuinely required, not advisory.",

  "m_pond",       "Garden ponds and swales",  "Medium",
  "required", "required", "recommended", "required", "required",
  "Sizing to the contributing area, liner or infiltration design, edge safety and setbacks are site-dependent, and vegetation must establish before the retention and habitat functions are reliable -- the same ground-modifying profile as rain gardens.",

  "m_infiltr",    "Infiltration systems",      "Medium",
  "required", "required", "recommended", "required", "required",
  "Soakaways, dry wells, trenches and chambers depend on percolation rate, groundwater depth and setback distance. REASSESSED: establishment/maintenance support raised from recommended to required. Subsurface systems fail SILENTLY -- there is no visible signal of lost function -- and sediment clogging is their documented dominant failure mode, so periodic clearing is a condition of continued function rather than an optional extra.",

  "m_depave",     "Depaving / permeable",      "Medium",
  "required", "recommended", "required", "required", "required",
  "Subgrade infiltration capacity and drainage connection govern performance, and surface removal frequently triggers permit and utility-locate requirements. Site assessment stays recommended because small courtyard conversions are often exempt. REASSESSED: maintenance support raised from recommended to required -- surface clogging by sediment and organic debris is the single most frequently cited failure mode for permeable surfaces, unmaintained permeable mixes have been observed to clog completely within about a decade, and remediation once clogged needs specialist equipment. Regular clearing is therefore constitutive of the measure working at all.",

  "m_trees",      "Trees (pits)",              "Medium",
  "recommended", "not required", "recommended", "required", "required",
  "Species selection, siting relative to overhead and buried utilities, and pit dimensions/soil volume benefit from specification, but the dominant failure mode is establishment mortality rather than design error: annual mortality peaks in the first five years after planting and stewardship/maintenance is among the most frequently cited human factors in the urban-tree mortality literature. Watering through establishment is the decisive provision. This Medium class is now also carried in the figure taxonomy, which previously showed trees as Low.",

  "m_barrel",     "Rain barrels / cisterns",   "Low",
  "not required", "not required", "not required", "required", "recommended",
  "A standardised distribution. The provisions that matter are instructions for overflow routing, winterisation and mosquito control, which are guidance rather than specification.",

  "m_disconn",    "Downspout disconnection",   "Low",
  "recommended", "not required", "recommended", "required", "not required",
  "Simple to perform but consequential if discharge is routed against a foundation or into a neighbouring property; some jurisdictions regulate disconnection from combined sewers.",

  "m_planting",   "Native / drought planting", "Low",
  "not required", "not required", "not required", "required", "required",
  "Low technical demand, but plant palettes and establishment watering determine whether the planting survives its first two seasons.",

  "m_turf",       "Turf replacement",          "Low",
  "recommended", "not required", "not required", "required", "required",
  "Conversion depends on soil preparation and irrigation adjustment; without establishment support, replacement plantings commonly fail and are reinstated as lawn."
)
save_tab(REQUIREMENT_MATRIX, "03b_technical_requirement_matrix")

## -----------------------------------------------------------------------------
## CONSISTENCY GUARD. The demand class lives in two places: MEASURES$tier (used
## by the figure axes) and REQUIREMENT_MATRIX$demand_class (used by the D3a
## technical test). They diverged silently once -- trees were Low in the taxonomy
## and Medium here -- so the figure axis contradicted the test. Fail loudly now.
## -----------------------------------------------------------------------------
.tier_check <- MEASURES %>%
  transmute(measure_var = var, taxonomy = sub(" .*", "", tier)) %>%
  left_join(REQUIREMENT_MATRIX %>% select(measure_var, matrix = demand_class),
            by = "measure_var") %>%
  filter(taxonomy != matrix)
if (nrow(.tier_check)) {
  stop(sprintf(paste("Demand class mismatch between MEASURES$tier and",
                     "REQUIREMENT_MATRIX$demand_class for: %s.",
                     "Fix both or the D3a test and the figures will disagree."),
               paste(sprintf("%s (taxonomy=%s, matrix=%s)", .tier_check$measure_var,
                             .tier_check$taxonomy, .tier_check$matrix), collapse = "; ")))
}
msg("Demand-class consistency: taxonomy and requirement matrix agree on all %d measures",
    nrow(MEASURES))

# -----------------------------------------------------------------------------
# OBSERVED PROVISIONS
# Each provision type is scored three ways so that a documented absence is never
# confused with a silence.
# -----------------------------------------------------------------------------
prov <- raw %>% transmute(
  case_id,
  ## P1 specification
  P1 = ternary(v_C5),
  ## P2 site assessment: funded professional design, or site visits as advice
  P2 = case_when(arch$cost_design == 1 | arch$adv_sitevisits == 1 ~ "yes",
                 ternary(v_E21) == "no" & ternary(v_E13) == "no"  ~ "no",
                 TRUE ~ "not_stated"),
  ## P3 regulatory support
  P3 = case_when(arch$r_fasttrack == 1 | arch$r_feewaiver == 1 ~ "yes",
                 ternary(v_C6) == "no" & ternary(v_C7) == "no"  ~ "no",
                 TRUE ~ "not_stated"),
  ## P4 guidance
  P4 = case_when(arch$p_housetype == 1 | arch$adv_guides == 1 |
                   arch$p_guide >= 1                            ~ "yes",
                 ternary(v_D1) == "no" & ternary(v_D8) == "no"  ~ "no",
                 TRUE ~ "not_stated"),
  ## P5 establishment and maintenance support
  P5 = case_when(arch$s_nonfin_n > 0 | arch$s_maintgrant == 1 |
                   arch$s_maintcost == 1                        ~ "yes",
                 ternary(v_E18) == "no"                         ~ "no",
                 TRUE ~ "not_stated")
)

# -----------------------------------------------------------------------------
# REQUIREMENT SET PER PROGRAMME = union across the measures it supports
# -----------------------------------------------------------------------------
PROV <- c("P1", "P2", "P3", "P4", "P5")
req_cols <- c("P1_specification", "P2_site_assessment", "P3_regulatory",
              "P4_guidance", "P5_establishment")

req_level <- function(i, p) {
  ## strongest requirement across the measures this programme supports
  supported <- REQUIREMENT_MATRIX$measure_var[
    vapply(REQUIREMENT_MATRIX$measure_var, function(v)
      isTRUE(arch[[v]][i] == 1), logical(1))]
  if (!length(supported)) return(NA_character_)
  lv <- REQUIREMENT_MATRIX[[req_cols[match(p, PROV)]]][
    REQUIREMENT_MATRIX$measure_var %in% supported]
  if (any(lv == "required")) "required"
  else if (any(lv == "recommended")) "recommended"
  else "not required"
}

n_case <- nrow(arch)
req_mat <- matrix(NA_character_, n_case, length(PROV), dimnames = list(NULL, PROV))
for (p in PROV) req_mat[, p] <- vapply(seq_len(n_case), req_level, character(1), p = p)

tech_req <- tibble(case_id = arch$case_id) %>%
  mutate(
    n_required      = rowSums(req_mat == "required", na.rm = TRUE),
    n_recommended   = rowSums(req_mat == "recommended", na.rm = TRUE),
    n_req_met       = rowSums(req_mat == "required" &
                                as.matrix(prov[, PROV]) == "yes", na.rm = TRUE),
    n_req_absent    = rowSums(req_mat == "required" &
                                as.matrix(prov[, PROV]) == "no", na.rm = TRUE),
    n_req_silent    = rowSums(req_mat == "required" &
                                as.matrix(prov[, PROV]) == "not_stated", na.rm = TRUE),
    n_rec_met       = rowSums(req_mat == "recommended" &
                                as.matrix(prov[, PROV]) == "yes", na.rm = TRUE),
    requirement_match = ifelse(n_required > 0, n_req_met / n_required, NA_real_),
    ## which specific requirements are unmet, named
    unmet_requirements = vapply(seq_len(n_case), function(i) {
      u <- PROV[which(req_mat[i, ] == "required" & prov[i, PROV] != "yes")]
      if (!length(u)) "none" else paste(u, collapse = " + ")
    }, character(1))
  )

## Requirement-based technical fit. Applicability follows the demand class:
## a programme distributing rain barrels is not held to the roof requirement set.
tech_req <- tech_req %>%
  mutate(
    tech_fit_req = case_when(
      is.na(n_required) | n_required == 0        ~ NA_character_,      # not applicable
      requirement_match == 1                     ~ "Documented fit",
      requirement_match >= 0.5                   ~ "Partial fit",
      n_req_met == 0 & n_req_absent > 0          ~ "Confirmed misfit",
      n_req_silent == n_required                 ~ "Indeterminate",
      TRUE                                       ~ "Partial fit"),
    tech_fit_req = factor(tech_fit_req,
      levels = c("Documented fit", "Partial fit", "Confirmed misfit", "Indeterminate")))

arch <- arch %>% left_join(tech_req, by = "case_id") %>%
  bind_cols(prov %>% select(all_of(PROV)) %>%
              setNames(paste0("prov_", PROV)))

save_tab(tech_req %>% count(tech_fit_req, name = "n") %>% mutate(pct = 100 * n / sum(n)),
         "03b_requirement_match_distribution")
save_tab(tech_req %>% count(unmet_requirements, name = "n") %>% arrange(desc(n)),
         "03b_unmet_requirements")
save_tab(as_tibble(req_mat) %>% mutate(case_id = arch$case_id) %>%
           count(P1, P2, P3, P4, P5, name = "n") %>% arrange(desc(n)),
         "03b_requirement_sets_observed")

msg("Requirement matrix: %d measures tagged across 5 provision types; %d records have a requirement set",
    nrow(REQUIREMENT_MATRIX), sum(tech_req$n_required > 0, na.rm = TRUE))
msg("  median requirement match %.2f; most common unmet set '%s'",
    stats::median(tech_req$requirement_match, na.rm = TRUE),
    names(sort(table(tech_req$unmet_requirements), decreasing = TRUE))[1])
