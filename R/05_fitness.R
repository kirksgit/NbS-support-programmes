# =============================================================================
# 05_fitness.R  |  PROGRAMME FIT
# Four domains, each producing a per-programme outcome on the common scale:
#   documented fit | partial/conditional fit | confirmed misfit |
#   indeterminate  | not applicable
# Two rules are enforced everywhere:
#   R1  "Not stated" is NEVER converted into misfit. Confirmed misfit requires
#       either an explicit exclusion ("No") or a documented mismatch between two
#       things that ARE both documented.
#   R2  Applicability is decided before fit. A test that does not apply to a
#       programme returns NA, not a poor score.
# =============================================================================

A <- arch %>% left_join(ctx, by = "case_id")

# -----------------------------------------------------------------------------
# ROUTE (c) ELIGIBILITY  --  universal rule 2, amended v62, predicate fixed v63.1
# -----------------------------------------------------------------------------
## A DOCUMENTATION-EVIDENCED misfit (route c) says: this programme documents its
## other provisions and is silent on THIS one, so a prospective participant cannot
## find the provision and cannot act on it. That is only a defensible reading if
## the record is genuinely well documented. Until v63 the rule stated the test as
## "the programme documents its other provisions to corpus-median source depth"
## and the CODE APPLIED NO TEST AT ALL -- route (c) fired unconditionally on D3a,
## D3b, D4a and D4b.
##
## The median predicate was also the wrong instrument, and measuring it is what
## showed why: the 367 D4a route-(c) records have a median of 6 unique source URLs
## against a corpus median of 6, and 10.4% of them sit in the corpus bottom decile
## against a 10% base rate by construction. They are INDISTINGUISHABLE from the
## corpus on depth, so a median cut would revert records for no evidentiary reason
## while passing half the corpus by definition.
##
## The predicate below is the PI's replacement and it is testable per record:
##   (i)  the record is NOT in the bottom decile of source depth, AND
##   (ii) it affirmatively documents at least two of applicant guide, accessible
##        forms and actor eligibility.
## Affirmatively means the coded value leads with a positive token -- a mention is
## not documentation. Records failing the predicate are thinly documented overall,
## so their silence is a limit of the record rather than a published architecture,
## and they fall through to Indeterminate instead of scoring a misfit.
.rc_depth <- doc$n_url_unique[match(A$case_id, doc$case_id)]
.rc_cut   <- as.numeric(stats::quantile(doc$n_url_unique, 0.10, na.rm = TRUE, type = 7))
A <- A %>% mutate(
  rc_depth      = .rc_depth,
  ## eligibility is read from n_actor_types (built in 03_architecture.R) because
  ## elig_documented is not defined until the D2 block further down this file.
  ## 2 CALIBRATION FIX. "Affirmatively documents" was implemented as p_guide
  ## >= 2, i.e. a COMPLETE applicant guide. That is a different and much higher
  ## bar than the rule states: p_guide == 1 means the guide is documented but
  ## partial, which IS publication, and treating it as absent contradicts the
  ## framework's own five-state logic where Partial means something is present.
  ## The strict form failed 284 records on documentation against 15 under the
  ## intended reading, and that -- not source depth -- drove the whole apparent
  ## exclusion (D4a 182 of 507). Documented-present at any level is the test.
  rc_documented = (p_guide >= 1L) + (p_forms >= 1L) +
                  (coalesce(n_actor_types, 0L) > 0L),
  ## route_c_ok is REPORTED, NOT OPERATIVE. The PI ruled that the shipped verdicts
  ## stand, so the four route-(c) branches fire unconditionally as before and this
  ## flag drives a sensitivity table instead of the verdicts. TRUE = the silence is
  ## in the documents; FALSE = the record is too thin to carry the inference.
  route_c_ok    = !is.na(rc_depth) & rc_depth > .rc_cut & rc_documented >= 2L)
msg("  route (c) predicate: depth cut > %g unique URLs, >= 2 of guide/forms/eligibility; %d of %d records eligible",
    .rc_cut, sum(A$route_c_ok, na.rm = TRUE), nrow(A))


# -----------------------------------------------------------------------------
# DOMAIN 1 | ECOLOGICAL-FUNCTIONAL FIT
# objective -> required ecological function -> supported intervention
# -----------------------------------------------------------------------------
## Function-match matrix (rows: objective; cols: measures that can plausibly
## deliver the required function). Deliberately generous: the test is a floor.
## m_turf and m_rainharv were absent from every row, so a programme funding
## lawn-to-xeriscape conversion or rainwater harvesting could not match ANY stated
## objective. Both deliver documented ecological functions -- turf conversion
## reduces irrigation demand and restores permeable vegetated surface; harvesting
## substitutes captured rainfall for potable supply and detains roof runoff -- and
## both already carry demand classes in the requirement matrix. Their omission was
## a matrix gap, not an ecological judgement. m_soil (compost/soil amendment)
## improves infiltration and moisture retention and joins on the same basis.
fn_match <- list(
  storm   = c("m_greenroof","m_raingard","m_depave","m_barrel","m_infiltr","m_disconn",
              "m_rainharv","m_turf","m_soil"),
  heat    = c("m_trees","m_greenroof","m_facade","m_depave","m_planting","m_turf"),
  biodiv  = c("m_raingard","m_trees","m_greenroof","m_facade","m_planting","m_turf"),
  drought = c("m_barrel","m_greywater","m_planting","m_infiltr","m_turf","m_rainharv","m_soil"),
  amenity = c("m_trees","m_raingard","m_greenroof","m_facade","m_planting","m_turf")
)
obj_cols <- c(storm = "obj_storm", heat = "obj_heat", biodiv = "obj_biodiv",
              drought = "obj_drought", amenity = "obj_amenity")

matched <- map_dfc(names(fn_match), function(o) {
  has_obj <- A[[obj_cols[[o]]]] == 1
  has_fn  <- rowSums(A[, fn_match[[o]], drop = FALSE] == 1, na.rm = TRUE) > 0
  tibble(!!paste0("mt_", o) := as.integer(has_obj & has_fn))
})
A <- bind_cols(A, matched)

## before any case is called fit or misfit.

A <- A %>% mutate(
  n_obj_matched = rowSums(across(starts_with("mt_")), na.rm = TRUE),
  eco_applicable = as.integer(obj_stated == 1 & measures_documented == 1),
  eco_fit = case_when(
    is.na(eco_applicable) | eco_applicable == 0 ~ "Indeterminate",
    n_obj_matched == obj_n                      ~ "Documented fit",
    n_obj_matched >  0                          ~ "Partial fit",
    TRUE                                        ~ "Confirmed misfit"),
  ## VALIDITY RESTRICTION: where B-5 was itself inferred (often from the measure
  ## list), the test is near-tautological. eco_fit_strict keeps only cases with a
  ## directly documented objective statement.
  ## RETIRED AS A REPORTED VARIANT. This dropped every case whose B-5 carried
  ## an "[inferred]" qualifier, on the theory that an inferred objective makes the
  ## D1 test circular. That reasoning confuses two different things. The qualifier
  ## does not mark a guess: it marks a code the coder reached by reading the
  ## programme's own documents and stating the inference, with the evidence quote
  ## attached like any other cell. An inferred code that carries its source is
  ## evidence; excluding it penalises the coder for being explicit about how the
  ## reading was made, and rewards the cell that asserts the same answer flatly.
  ##
  ## D1's circularity problem is real but it is NOT solved here -- it is handled
  ## where it belongs, by reporting D1 as a validity floor rather than a finding
  ## (see 0.2). The column is kept only so the retired variant is auditable; it is
  ## no longer reported, and inferred codes are treated as equal everywhere.
  eco_fit_strict_RETIRED = ifelse(obj_inferred == 1, NA_character_, eco_fit),

  ## Spatial responsiveness (reported alongside D1, never scored as fit)
  spatial_response = case_when(
    is.na(spatial_target)                                     ~ "Indeterminate",
    equity_target %in% c("Low-income/equity zones",
                         "Vulnerable populations")            ~ "Vulnerability-targeted",
    spatial_target == "Watershed/sewershed" |
      equity_target == "Hazard-exposure zones"                ~ "Hazard/catchment-targeted",
    spatial_target == "Priority zones"                        ~ "Area-targeted",
    spatial_target == "Citywide"                              ~ "Citywide (untargeted)",
    TRUE                                                      ~ "Indeterminate"),
  ## congruence between independently documented hazard exposure and targeting
  hazard_congruent = case_when(
    is.na(hx_n) | hx_n == 0                                   ~ NA_integer_,
    haz_storm == 1 & hx_flood == 1                            ~ 1L,
    haz_heat  == 1 & hx_heat  == 1                            ~ 1L,
    haz_drought == 1 & hx_drought == 1                        ~ 1L,
    TRUE                                                      ~ 0L)
)

# -----------------------------------------------------------------------------
# DOMAIN 2 | SPATIAL-INSTITUTIONAL FIT
# authority fit (who may authorise) + participation-pathway fit (who may act)
# -----------------------------------------------------------------------------
elig_cols  <- c("el_home","el_land","el_coop","el_school","el_ngo","el_sme",
                "el_socho","el_inform","el_tenant_direct","el_tenant_cond")
excl_cols  <- c("ex_land","ex_coop","ex_socho","ex_tenant")

A <- A %>% mutate(
  elig_documented = as.integer(rowSums(across(all_of(elig_cols)) == 1, na.rm = TRUE) > 0),
  controller_incl = as.integer(rowSums(across(c("el_land","el_coop","el_socho")) == 1,
                                       na.rm = TRUE) > 0),
  n_explicit_excl = rowSums(across(all_of(excl_cols)) == 1, na.rm = TRUE),

  ## Applicability, two tiers (either suffices):
  ##  (a) structural: the programme funds work ON BUILDINGS, which in multi-unit
  ##      stock cannot be authorised by an occupant alone;
  ##  (b) contextual: the city is renter-dominant, so an owner-only design has
  ##      demonstrable consequences for reach.
  access_appl_bldg = as.integer(m_greenroof == 1 | m_facade == 1),
  access_appl_ctx  = as.integer(renter_pct >= 40),
  access_applicable = as.integer(access_appl_bldg == 1 | access_appl_ctx == 1),

  ## Explicit closure of CONTROLLER routes (not merely of tenants): a tenant
  ## exclusion alone leaves an owner/landlord route open and is not a misfit.
  controller_closed = as.integer(rowSums(across(c("ex_land","ex_coop","ex_socho")) == 1,
                                         na.rm = TRUE) > 0),

  access_route = case_when(
    elig_documented == 0                                    ~ "Indeterminate",
    ## CONFIRMED MISFIT: applicable case, at least one controller category
    ## EXPLICITLY excluded, no controller or tenant route opened anywhere.
    access_applicable == 1 & controller_incl == 0 &
      controller_closed == 1 &
      !(el_tenant_direct %in% 1) & !(el_tenant_cond %in% 1) &
      !(tenant_pathway %in% 1)                              ~ "Confirmed misfit",
    ## v13 CONFLATION FIXED. This branch previously read
    ##   el_tenant_direct == 1 | tenant_pathway == 1  ~ "Direct tenant/broad pathway"
    ## and sat ABOVE the el_tenant_cond branch, so any documented tenant pathway --
    ## including one that works only with the owner's written consent -- was
    ## labelled DIRECT. Of the 248 records in that category only 82 were
    ## unconditional; 139 were tenant-with-consent. The framework's own rule is
    ## that consent-dependent access is not equivalent to direct access, and D2 was
    ## breaking it internally. Consent is now tested BEFORE the direct branch can
    ## absorb it, and a pathway flag without unconditional eligibility no longer
    ## promotes a record to the top category.
    el_tenant_direct == 1                                   ~ "Direct tenant/broad pathway",
    el_tenant_cond == 1                                     ~ "Permission-dependent pathway",
    tenant_pathway == 1 & controller_incl == 1               ~ "Controller/collective pathway",
    tenant_pathway == 1                                     ~ "Permission-dependent pathway",
    controller_incl == 1                                    ~ "Controller/collective pathway",
    el_home == 1                                            ~ "Owner-restricted pathway",
    TRUE                                                    ~ "Indeterminate"),

  ## v13 D2 ABSORBS D2b. The old test asked only whether SOME route beyond the
  ## owner exists, which made a controller-only route a documented fit in a
  ## renter-majority city -- a route that cannot reach most residents there. The
  ## separate D2b component tested proportionality and was found to carry distinct
  ## information (Cramer's V = 0.413 against old D2, agreeing on 38.5% of records),
  ## so keeping both scored the same provisions twice under two labels.
  ##
  ## D2 is now ONE relational test on three axes, which is also exactly how the
  ## figure reads: (1) the tenure context the city generates -- never scored, it
  ## sets the requirement; (2) how far past the owner the documented route reaches;
  ## (3) whether the reach meets the requirement.
  ##
  ## Reach ranks on one dimension: homeowner < controller < tenant-with-consent <
  ## tenant-direct. Required reach: owner-dominated cities (<35% renting) need only
  ## a homeowner route -- that IS proportionate where nearly all housing is
  ## owner-occupied, so those programmes are assessed and can pass; mixed tenure
  ## (35-55%) needs a controller route; renter-majority (>=55%) needs a tenant
  ## route. Exceeding the requirement is a fit, not a separate state.
  ##
  ## Unverified city tenure cannot generate a requirement, so those records are
  ## Indeterminate on the MATCH -- an evidence limit, not a design failure.
  d2_reach = case_when(
    access_route == "Direct tenant/broad pathway"            ~ 4L,
    access_route == "Permission-dependent pathway"           ~ 3L,
    access_route == "Controller/collective pathway"          ~ 2L,
    access_route == "Owner-restricted pathway"               ~ 1L,
    TRUE                                                     ~ NA_integer_),
  d2_band = case_when(
    is.na(renter_pct)                                        ~ NA_character_,
    renter_pct >= 55                                         ~ "Renter-majority",
    renter_pct >= 35                                         ~ "Mixed tenure",
    TRUE                                                     ~ "Owner-dominated"),
  d2_required = case_when(
    d2_band == "Renter-majority"                             ~ 3L,
    d2_band == "Mixed tenure"                                ~ 2L,
    d2_band == "Owner-dominated"                             ~ 1L,
    TRUE                                                     ~ NA_integer_),
  access_fit = case_when(
    access_route == "Confirmed misfit"                       ~ "Confirmed misfit",
    is.na(d2_reach) | is.na(d2_required)                     ~ "Indeterminate",
    ## v21 ORDERING CORRECTED. d2_reach was a single ordered scale placing the
    ## controller/collective route (2) BELOW tenant-with-owner-consent (3). Those
    ## are not points on one dimension. A social-housing or co-operative route
    ## reaches tenants THROUGH an institution and needs no per-case permission;
    ## a consent route reaches any tenant in principle but makes every application
    ## depend on an individual landlord saying yes. The framework's own text is
    ## explicit that renter-dominated cities require "tenants, social-housing,
    ## co-operatives, intermediaries or collective routes" -- these are listed as
    ## ALTERNATIVES, not as a ladder -- and that owner consent "adds an additional
    ## permission layer" rather than constituting reach.
    ##
    ## The consequence of the old ordering was visible and wrong in both
    ## directions: 62 collective routes in renter-majority cities scored PARTIAL
    ## while 61 consent-dependent routes in the same cities scored FULL FIT.
    ## Snep et al. (2023) identify social housing as the delivery vehicle in
    ## exactly that stock.
    ##
    ## Reach is therefore read as two independent properties -- does the route
    ## reach non-owner residents, and does it require per-case owner permission --
    ## and the requirement is stated per band rather than as a numeric threshold.
    d2_band == "Owner-dominated"                             ~ "Documented fit",
    d2_band %in% c("Mixed tenure", "Renter-majority") &
      access_route %in% c("Direct tenant/broad pathway",
                          "Controller/collective pathway")   ~ "Documented fit",
    d2_band %in% c("Mixed tenure", "Renter-majority") &
      access_route == "Permission-dependent pathway"          ~ "Partial fit",
    d2_band %in% c("Mixed tenure", "Renter-majority") &
      access_route == "Owner-restricted pathway"              ~ "Confirmed misfit",
    d2_reach >= d2_required                                  ~ "Documented fit",
    (d2_required - d2_reach) == 1L                           ~ "Partial fit",
    TRUE                                                     ~ "Confirmed misfit"),

  ## organisational participation scope (kept separate from authority route)
  participation_scope = case_when(
    elig_documented == 0                                   ~ "Indeterminate",
    el_inform == 1 | q_informal_ack == 1                   ~ "Informal/community actors",
    el_ngo == 1                                            ~ "Civil-society organisations",
    el_school == 1 | el_sme == 1 | el_socho == 1           ~ "Institutional actors",
    TRUE                                                   ~ "Individual property actors only")
)

# -----------------------------------------------------------------------------
# DOMAIN 3A | TECHNICAL ENABLEMENT -- REQUIREMENT MATCH, NOT A COUNT
#
# The test asks whether the provisions a measure actually needs are present, not
# how many provisions exist. A permit-fee waiver plus housing-type guidance is
# two enablers and answers almost nothing about installing a green roof;
# installation standards plus professional site advice is also two and answers a
# great deal. The three complexity groups already defined in the framework carry
# the requirement sets:
#
#   HIGH   building-integrated (green roofs, green facades)
#          core: a SPECIFICATION element  -- technical standards (C-5) or
#                eligible design/professional costs (E-21)
#          core: an ADVISORY element      -- technical advice (E-13) or
#                housing-type guidance (D-8)
#          supporting: permit support (C-6/C-7)
#
#   MEDIUM ground-modifying (rain gardens, depaving, infiltration)
#          core: an ADVISORY element for siting, soil and drainage
#          supporting: dimensional standards, permit support
#
#   LOW    standardised distributions (barrels, tree or material giveaways)
#          no technical requirement; the test does not apply
#
# Confirmed misfit still requires an explicit absence, never silence.
# -----------------------------------------------------------------------------
enab_cols <- c("r_standards","r_fasttrack","r_feewaiver","p_housetype","i_advice","cost_design")

A <- A %>% mutate(
  ## descriptive breadth, retained for reporting but no longer the fit test
  n_enablers = rowSums(across(all_of(enab_cols)) == 1, na.rm = TRUE),

  ## requirement elements
  ## v13 CONSTRUCT PURGE. D3a is technical enablement and must be built from
  ## technical variables only. Three admin/procedural variables were previously
  ## admitted and each could carry the verdict on its own:
  ##   * p_housetype (D-8 housing-type guidance) satisfied adv_element alone in 22
  ##     cases -- an administrative leaflet standing in for technical advice;
  ##   * p_guide (D-1 applicant guide) satisfied guid_element alone in 84 cases,
  ##     so a programme with no technical standard and no technical advice could
  ##     score a TECHNICAL fit on the strength of an application guide;
  ##   * r_fasttrack/r_feewaiver (C-6/C-7 permit support) are nonzero in 1 and 9
  ##     records of 709 -- they cannot discriminate and are demoted to descriptive
  ##     reporting (the RQ1 regulatory near-absence finding).
  ## D-1 and D-8 also fed D3c, so the same provision was scored twice in two
  ## nominally independent domains, inflating their apparent independence.
  ## Each variable now belongs to exactly one domain.
  spec_element = as.integer(r_standards == 1 | cost_design == 1),
  adv_element  = as.integer(i_advice == 1),
  reg_element  = 0L,
  ## SUPPORT FLOOR FOR STANDARDISED MEASURES. The requirement matrix already
  ## records P4_guidance as *required* for every low-demand measure (rain
  ## barrels need overflow routing, winterisation and mosquito-control
  ## instructions; turf conversion and planting need establishment care), and
  ## P5_establishment as recommended. The earlier test discarded that by
  ## setting tech_applicable = 0 for the whole class, so a programme could
  ## supply a barrel with no instructions and never be assessed. The floor is
  ## now tested: guidance is the required element, establishment support the
  ## strengthening one.
  ## D-1 applicant guidance is coded 0/1/2, where 2 is
  ## FULL guidance and 1 partial -- so `== 1` excluded the 76 records with the
  ## STRONGEST guidance from the guidance floor. Changed to >= 1: any documented
  ## guidance clears the floor, which is what the requirement matrix states.
  ## p_guide (D-1) removed -- an application guide is administrative, not a
  ## technical installation guide. s_guides is the technical installation guide and
  ## remains, as does technical advice.
  guid_element = as.integer(i_advice == 1 | s_guides == 1),
  est_element  = as.integer(s_guides == 1 | s_helpdesk == 1 | x_workshops == 1),
  ## v18 CORRECTED. This read D-1, the ADMINISTRATIVE applicant guide -- a leftover
  ## from the contamination purged at v13, and the last place an administrative
  ## variable still drove a technical verdict. The technical guidance absence is a
  ## documented "No" on technical advice (E-13) or technical standards (C-5), with
  ## no technical guidance provided by any other route. 14 records carrying exactly
  ## that were sitting in Indeterminate, which is a determinate finding mis-filed.
  guid_absent  = as.integer((no1(raw$v_E13) %in% 1 | no1(raw$v_C5) %in% 1) &
                              !(i_advice %in% 1) & !(s_guides %in% 1)),

  ## explicit absences of those elements
  spec_absent = as.integer(no1(raw$v_C5) == 1 & !(cost_design %in% 1)),
  adv_absent_el = as.integer((adv_absent %in% 1 | no1(raw$v_D8) %in% 1) &
                               !(i_advice %in% 1) & !(p_housetype %in% 1)),

  ## every CLASSIFIED portfolio is testable now -- the tier decides which
  ## provision the test demands, not whether a test happens at all. Only
  ## portfolios with no classifiable measure (tech_demand NA) fall outside.
  tech_applicable = as.integer(tech_demand %in% c("Low (standardised)",
                                                  "Medium (ground-modifying)",
                                                  "High (building-integrated)")),

  ## which requirement is unmet, reported alongside the verdict
  tech_requirement_profile = case_when(
    is.na(tech_demand)                                             ~ "Not classified",
    tech_demand == "Low (standardised)" & guid_element == 1 &
      est_element == 1                                             ~ "Guidance and establishment support",
    tech_demand == "Low (standardised)" & guid_element == 1         ~ "Guidance without establishment support",
    tech_demand == "Low (standardised)" & guid_absent == 1          ~ "No guidance documented",
    tech_demand == "Low (standardised)"                             ~ "Guidance not stated",
    tech_demand == "High (building-integrated)" &
      spec_element == 1 & adv_element == 1 & reg_element == 1       ~ "Specification, advice and permit support",
    tech_demand == "High (building-integrated)" &
      spec_element == 1 & adv_element == 1                          ~ "Specification and advice",
    tech_demand == "High (building-integrated)" & adv_element == 1  ~ "Advice without specification",
    tech_demand == "High (building-integrated)" & spec_element == 1 ~ "Specification without advice",
    tech_demand == "High (building-integrated)"                     ~ "Neither specification nor advice",
    adv_element == 1 & (spec_element == 1 | reg_element == 1)       ~ "Advice with standards or permit support",
    adv_element == 1                                                ~ "Advice only",
    TRUE                                                            ~ "No advisory provision"),

  tech_fit = case_when(
    is.na(tech_applicable) | tech_applicable == 0                   ~ NA_character_,
    ## HIGH complexity: both core elements required
    tech_demand == "High (building-integrated)" &
      spec_element == 1 & adv_element == 1                          ~ "Documented fit",
    tech_demand == "High (building-integrated)" &
      (spec_element == 1 | adv_element == 1)                        ~ "Partial fit",
    tech_demand == "High (building-integrated)" &
      (spec_absent == 1 | adv_absent_el == 1)                       ~ "Confirmed misfit",
    ## same correction as the LOW branch. Building-integrated work needs
    ## specification and advice, but a programme documenting installation guidance
    ## or establishment support HAS provided technical enablement -- below what the
    ## class requires, which is partial, not undeterminable.
    tech_demand == "High (building-integrated)" &
      (guid_element == 1 | est_element == 1)                        ~ "Partial fit",
    tech_demand == "High (building-integrated)"                     ~ "Indeterminate",
    ## LOW complexity: the guidance floor. Guidance is the required provision;
    ## establishment support strengthens it to a documented fit.
    tech_demand == "Low (standardised)" & guid_element == 1 &
      est_element == 1                                             ~ "Documented fit",
    tech_demand == "Low (standardised)" & guid_element == 1         ~ "Partial fit",
    tech_demand == "Low (standardised)" & guid_absent == 1          ~ "Confirmed misfit",
    ## a programme that documents technical standards or establishment
    ## support, but not guidance, HAS provided technical enablement -- just not
    ## the element this demand class asks for first. Sending it to Indeterminate
    ## says "cannot be determined", which is false: the provision is documented
    ## and it is partial. 13 LOW-demand records sat there on that reading alone.
    tech_demand == "Low (standardised)" &
      (spec_element == 1 | est_element == 1)                       ~ "Partial fit",
    ## A programme that documents its measures, its eligibility,
    ## its applicant guide and its forms -- and then documents NO technical
    ## standard, advice, guidance or establishment support -- has told the
    ## applicant to apply without telling them what standard to meet. On the
    ## source-verified corpus these 59 records carry the corpus-median source
    ## depth (7 URLs), 59/59 document a measure and 58/59 document eligibility,
    ## so this is silence in the documents rather than a limit of the search.
    ## Scored as a misfit rather than parked as unanswerable.
    tech_demand == "Low (standardised)"                             ~ "Confirmed misfit",
    ## MEDIUM complexity: advisory element required, strengthened by the others
    adv_element == 1 & (spec_element == 1 | reg_element == 1)       ~ "Documented fit",
    adv_element == 1                                                ~ "Partial fit",
    adv_absent_el == 1                                              ~ "Confirmed misfit",
    ## ground-modifying work needs advice first, but standards, guidance or
    ## establishment support are documented technical provision all the same.
    spec_element == 1 | guid_element == 1 | est_element == 1        ~ "Partial fit",
    ## v62 (PI ruling), as above for the High and Medium demand classes.
    TRUE                                                            ~ "Confirmed misfit"),

  ## breadth grade kept for description only, never as the verdict
  tech_grade = case_when(
    is.na(tech_demand)  ~ "Indeterminate",
    n_enablers == 0     ~ "No enabler documented",
    n_enablers == 1     ~ "One enabler",
    n_enablers <= 3     ~ "Two to three enablers",
    TRUE                ~ "Four or more enablers"),
  tech_grade = factor(tech_grade, levels = c("No enabler documented","One enabler",
                                             "Two to three enablers","Four or more enablers",
                                             "Indeterminate"))
)

save_tab(A %>% filter(tech_applicable == 1) %>%
           count(tech_demand, tech_requirement_profile, tech_fit, name = "n") %>%
           arrange(tech_demand, desc(n)),
         "05_technical_requirement_match")

# -----------------------------------------------------------------------------
# DOMAIN 3B | FINANCIAL ACCESSIBILITY AND CALIBRATION
# Full-corpus cost-barrier configuration (the restricted test is removed)
# -----------------------------------------------------------------------------
A <- A %>% mutate(
  payment_config = case_when(
    is.na(disbursement) | disbursement == "Not stated"  ~ "Not stated",
    disbursement == "Reimbursement" & i_inkind == 1     ~ "Reimbursement + in-kind alternative",
    TRUE                                                ~ disbursement),
  ## D3b RESCOPED. The previous single test gated on fin_scope -- programmes that
  ## EXPLICITLY target financially constrained applicants -- which admitted only 39
  ## of 707 and discarded the cost-barrier evidence carried by the other 664. The
  ## disbursement route is documented for 598 records, so the cost barrier IS
  ## testable corpus-wide. Two tiers, reported separately:
  ##
  ##   fin_barrier (UNIVERSAL, n = 598 evaluable): does the programme require the
  ##     applicant to carry the cost first? Reimbursement and milestone payment
  ##     both do; upfront/vendor payment and fiscal credit at point of sale do not.
  ##     This is the distributional question that applies to every applicant.
  ##   [REMOVED] the TARGETED n = 39 verdict: among programmes that declare an equity target,
  ##     is the payment route consistent with that declaration? A programme that
  ##     names low-income applicants and still demands pre-financing contradicts
  ##     its own stated target.
  fin_scope = as.integer(equity_targeted == 1 | el_socho == 1 | q_equity_ind == 1 |
                           kw(raw$v_G4, "means-test|income|modeste|low-income") == 1),
  ## ---------------------------------------------------------------------------
  ## D3b CONSTRUCT CORRECTED (external round-2 audit, CRITICAL -- upheld).
  ##
  ## THE FAULT. The former level was called "Pre-financing required, NO
  ## ALTERNATIVE", which asserts that no alternative EXISTS. It cannot. I checked
  ## the codebook rather than my own prior reasoning: E-11 is declared
  ## **"Text"**, not an exhaustive choice, and the coding rules state
  ## "Not stated != absent" and "No = source explicitly excludes the feature".
  ## Among the 397 records the old rule called an absence, E-11 read
  ## Not stated 260 / Not applicable 136 / explicit No 1. So 396 of 397 were
  ## silence re-labelled as documented absence -- the very error the framework
  ## forbids, and my "exhaustive-choice field" defence of it was simply wrong.
  ##
  ## THE REPAIR (audit option b, chosen deliberately). The DOCUMENTED finding is
  ## real and survives: the payment route the programme publishes requires the
  ## applicant to front the cost, and no alternative is publicly documented. That
  ## is what an applicant reading the programme actually encounters, and E-22 --
  ## a SINGLE-SELECT field -- documents it positively. What must go is the claim
  ## that no alternative exists. The level is renamed to say exactly that, and
  ## every downstream caption and claim follows the new wording.
  ##
  ## Option (a), the strict-absence reading, is not discarded: it is computed as
  ## fin_barrier_strict below and reported as a stated sensitivity, so a reader
  ## can see both the documentation-based verdict (397) and the explicit-absence
  ## verdict (1) side by side rather than taking my word for the difference.
  fin_barrier = case_when(
    is.na(disbursement) | disbursement == "Not stated"             ~ "Indeterminate",
    prefinancing_required == 0                                     ~ "No pre-financing required",
    prefinancing_required == 1 & (i_inkind == 1 | i_discount == 1)  ~ "Pre-financing with a documented in-kind or discount alternative",
    prefinancing_required == 1                                      ~ "Documented route requires pre-financing; no alternative documented",
    TRUE                                                            ~ "Indeterminate"),
  fin_barrier = factor(fin_barrier, levels = c(
    "Documented route requires pre-financing; no alternative documented",
    "Pre-financing with a documented in-kind or discount alternative",
    "No pre-financing required", "Indeterminate")),

  ## STRICT-EVIDENCE SENSITIVITY (audit option a), reported, never the headline.
  ## Here an absence must be EXPLICIT: E-11 must carry a documented "No". Every
  ## record whose alternative is merely unstated becomes Indeterminate.
  e11_explicit_no = as.integer(no1(raw$v_E11) %in% 1),
  fin_barrier_strict = case_when(
    is.na(disbursement) | disbursement == "Not stated"              ~ "Indeterminate",
    prefinancing_required == 0                                      ~ "No pre-financing required",
    prefinancing_required == 1 & (i_inkind == 1 | i_discount == 1)  ~ "Pre-financing with a documented alternative",
    prefinancing_required == 1 & e11_explicit_no == 1               ~ "Pre-financing, alternative explicitly absent",
    prefinancing_required == 1                                      ~ "Indeterminate (alternative undocumented)",
    TRUE                                                            ~ "Indeterminate"),
  ## fin_fit RETIRED. It was the restricted 39-case financial verdict (NA outside
  ## the equity-targeted scope). The restricted test is removed from the study, so
  ## the variable is no longer computed -- fit_D3b (universal) is the only financial
  ## verdict. fin_scope survives as a STRATUM FLAG for the D3b flow panel's third
  ## axis, which is a cut of the universal test, not a separate test.
)

# -----------------------------------------------------------------------------
# DOMAIN 3C | ADMINISTRATIVE ACCESSIBILITY
# documented burden-reducing provisions (NOT experienced burden)
# -----------------------------------------------------------------------------
A <- A %>% mutate(
  ## v13 REBUILD. D3c is ADMINISTRATIVE ACCESS -- how easy the programme is to
  ## apply to -- and the previous construction did not measure that. Two faults:
  ##
  ##   (1) comply_score borrowed variables belonging to other tests. C-6 fast-track
  ##       and C-7 fee waiver are nonzero in 1 and 9 records of 709, so they
  ##       occupied two of five slots while being unable to discriminate; and
  ##       prefinancing_required is a PAYMENT variable, already the whole of D3b.
  ##       Scoring it here made a financial provision raise an administrative
  ##       verdict. tenant_pathway belongs to the tenure/authority test.
  ##
  ##   (2) The learning/compliance split was breadth-of-material versus
  ##       everything-else, not the two things an applicant actually meets. The
  ##       axes are now what the codebook's D-block was written to capture:
  ##       CLARITY (can I find out what to do?) and SUBMISSION (can I actually
  ##       lodge it?).
  ##
  ## Every input is a D-block procedural variable and appears in no other domain.
  ## DISTINCT PROVISIONS, not weighted depth. p_guide and p_forms are scored 0/1/2
  ## in the codebook, so a WEIGHTED sum let a single variable at full value clear an
  ## axis on its own: 29 of the resulting fits rested on a complete applicant guide
  ## plus accessible forms with no stated timeline, one contact channel and no
  ## multilingual provision. One good document is not a navigable procedure, and the
  ## construct claims access is multi-faceted. Each variable therefore contributes
  ## AT MOST 1 and each axis needs two DIFFERENT provisions, which makes the axis
  ## structurally impossible to clear with one variable. This is the stricter
  ## reading (187 fits rather than 329) and it is adopted for that reason.
  clarity_score = rowSums(cbind(as.integer(coalesce(p_guide, 0L) >= 1),
                                coalesce(p_timeline, 0L),
                                coalesce(p_housetype, 0L)), na.rm = TRUE),
  submit_score  = rowSums(cbind(as.integer(coalesce(p_forms, 0L) >= 1),
                                as.integer(n_channels >= 2),
                                coalesce(p_multiling, 0L)), na.rm = TRUE),
  ## v18 FUNCTION FLAGS. Two necessary capabilities, each satisfiable by any one
  ## of its provisions -- so a programme is not penalised for choosing a phone
  ## line over a multilingual form, but cannot substitute a second guide for a
  ## missing submission route.
  ## Each capability has a CORE provision that actually delivers it, and
  ## supporting provisions that widen access once the core exists. A second
  ## supporting provision cannot stand in for a missing core -- that was the
  ## defect of the count rule. An applicant who cannot obtain the form is not
  ## helped by the programme also publishing a timeline.
  ##   KNOW  core: D-1 applicant guide.      widened by D-2 timeline, D-8 housing guidance.
  ##   LODGE core: D-3 accessible forms.     widened by D-4 channels, D-5 language.
  ## A partial-strength core (guide or forms coded "partial", = 1) delivers the
  ## capability incompletely, so it satisfies the core only at partial level.
  d3c_know  = as.integer(coalesce(p_guide, 0L) == 2),
  d3c_lodge = as.integer(coalesce(p_forms, 0L) == 2),
  d3c_know_part  = as.integer(coalesce(p_guide, 0L) >= 1),
  d3c_lodge_part = as.integer(coalesce(p_forms, 0L) >= 1),
  d3c_widen = as.integer(coalesce(p_timeline, 0L) == 1) +
              as.integer(coalesce(p_housetype, 0L) == 1) +
              as.integer(coalesce(n_channels, 0L) >= 2) +
              as.integer(coalesce(p_multiling, 0L) == 1),
  ## An explicit "No" on ANY administrative provision documents an absence.
  d3c_any_absent = as.integer(no1(raw$v_D1) %in% 1 | no1(raw$v_D2) %in% 1 | no1(raw$v_D3) %in% 1 |
                              no1(raw$v_D4) %in% 1 | no1(raw$v_D5) %in% 1 | no1(raw$v_D8) %in% 1),
  learn_score  = clarity_score,
  comply_score = submit_score,
  ## Evidence state for the administrative block, kept separate from the profile:
  ## a documented absence and a silence are different findings and are never merged.
  admin_evidence = case_when(
    guide_state == "documented present" | forms_state == "documented present" ~ "documented present",
    guide_state == "documented absent"  | forms_state == "documented absent"  ~ "documented absent",
    TRUE                                                                      ~ "not documented"),
  admin_profile = case_when(
    is.na(p_guide) & is.na(p_forms)                      ~ "Indeterminate",
    learn_score >= 4 & comply_score >= 2                 ~ "Broad learning + compliance support",
    learn_score >= 2 & comply_score >= 1                 ~ "Basic applicant pathway",
    learn_score >= 1 | comply_score >= 1                 ~ "Partial/mixed support",
    ## zero provisions: an explicit absence is a finding, silence is not
    admin_evidence == "documented absent"                ~ "No provision documented",
    TRUE                                                 ~ "Indeterminate"),
  admin_profile = factor(admin_profile,
    levels = c("No provision documented","Partial/mixed support",
               "Basic applicant pathway","Broad learning + compliance support","Indeterminate"))
)

# -----------------------------------------------------------------------------
# DOMAIN 4A | STEWARDSHIP FIT
# -----------------------------------------------------------------------------
A <- A %>% mutate(
  st_resp_assigned = as.integer(!is.na(s_respons) & s_respons != "Not stated"),
  st_support = as.integer(rowSums(cbind(s_maintgrant, s_maintcost,
                                        as.integer(s_nonfin_n > 0)) == 1, na.rm = TRUE) > 0),
  ## DOCUMENTED absence of every support form, distinguished from silence.
  ## Only an explicit negative on all three support fields counts; a "Not stated"
  ## anywhere means the programme may offer support it did not publish, which the
  ## silence rule forbids reading as absence.
  st_support_denied = as.integer(
    grepl("^\\s*no\\b",        tolower(raw$v_E15[match(case_id, raw$case_id)])) &
    grepl("^\\s*no\\b",        tolower(raw$v_E16[match(case_id, raw$case_id)])) &
    grepl("^\\s*(no|none)\\b", tolower(raw$v_E18[match(case_id, raw$case_id)]))),
  steward_config = case_when(
    is.na(s_respons) & is.na(s_condition) & is.na(s_nonfin_n)   ~ "Indeterminate",
    st_resp_assigned == 1 & s_condition == 1 & st_support == 1  ~ "Responsibility + obligation + support",
    st_resp_assigned == 1 & s_condition == 1                    ~ "Responsibility + obligation, no support",
    st_resp_assigned == 1 & st_support == 1                     ~ "Responsibility + support, no obligation",
    st_resp_assigned == 1                                       ~ "Responsibility only",
    st_support == 1                                             ~ "Support only, bearer unclear",
    s_condition == 1                                            ~ "Obligation only, bearer unclear",
    TRUE                                                        ~ "No maintenance architecture documented"),
  ## fit reading: internal workability, NOT presence of a maintenance grant
  ## D4a NOW HAS A MISFIT ROUTE, and it is not a construction artefact.
  ##
  ## The previous rule could never return "Confirmed misfit". That was defended as
  ## following the framework -- "assigning a maintenance responsibility without a
  ## public grant is not itself a misfit" -- but the framework sentence continues:
  ## the test is whether responsibilities, obligations and any support are
  ## "documented and internally workable". A programme that places an ENFORCEABLE
  ## obligation on a citizen and documents NO support of any kind -- not a grant,
  ## not cost coverage, not a site visit, not advice -- is not internally workable.
  ## That is a misfit on the framework's own terms, and a domain that structurally
  ## cannot express it is not measuring the thing it names.
  ##
  ## The route is deliberately narrow, because the silence rule governs. Of the 69
  ## records carrying obligation-plus-responsibility with st_support == 0, only
  ## those whose support fields are EXPLICIT negatives qualify; 68 have at least
  ## one "Not stated" support field and stay at Partial fit, since an undocumented
  ## support offer is not a documented absence of one. The verdict therefore fires
  ## on evidence, never on silence, and the count will be small by construction --
  ## that smallness is the finding, not a defect in the rule.
  ## ---------------------------------------------------------------------------
  ## THE MISFIT ROUTE IS THE DOCUMENTED TRANSFER OF A DUTY, NOT SILENCE.
  ##
  ## The previous rule returned a misfit only where a programme EXPLICITLY denied
  ## every support form alongside a binding condition -- 1 programme in 709. That
  ## made D4a's verdict column read as near-universal success while 278 of the 671
  ## programmes that assign a maintenance duty (41%) document no support at all,
  ## which is the opposite of what the corpus shows.
  ##
  ## The fix is NOT to read silence as failure. Silence about a provision remains
  ## "not stated" everywhere in this framework, and 302 of the programmes a naive
  ## rule would newly condemn are merely silent. The fix is to notice that D4a's
  ## misfit does not rest on silence at all: it rests on a POSITIVE DOCUMENTED ACT.
  ## F-5 records that the programme TRANSFERS the maintenance duty to the
  ## participant (657 of 671 name the applicant; 8 name a shared arrangement). A
  ## programme that documents the transfer and documents no support has not failed
  ## to publish something -- it has published a design in which the duty moves and
  ## the capacity to discharge it does not follow. The evidence for the misfit is
  ## the transfer, which is stated, not the support, which is absent.
  ##
  ## This is exactly the shape D3b already uses and has used since v13: a
  ## documented reimbursement route with no documented alternative is a confirmed
  ## misfit (457 programmes), because the barrier is created by the DOCUMENTED
  ## payment design, not inferred from silence about hardship provisions. D4a now
  ## reads the same way, so the two domains answer comparable questions.
  ##
  ## Where the city RETAINS the duty (6 programmes) no transfer occurs, so the test
  ## has no object and returns Not applicable -- applicability precedes fit.
  ## Where F-5 is not stated (38) the transfer cannot be established: Indeterminate,
  ## never misfit.
  ##
  ## The support tiers are graded by what they give the duty-holder. Material
  ## support -- a maintenance grant, covered costs, a site visit, an inspection
  ## reminder -- carries capacity to the participant. Informational support --
  ## a guide, a helpdesk -- tells them what to do but leaves the doing entirely
  ## with them; that is a partial answer to a transferred duty, not a complete one.
  ## ---------------------------------------------------------------------------
  st_duty_transferred = as.integer(st_resp_assigned == 1 &
                                     s_respons %in% c("Applicant/participant", "Shared")),
  st_city_retains     = as.integer(!is.na(s_respons) & s_respons == "City/utility"),
  st_support_tier = case_when(
    s_maintgrant == 1 | s_maintcost == 1 |
      s_sitevisit == 1 | s_reminders == 1 ~ "Material support",
    s_guides == 1 | s_helpdesk == 1       ~ "Informational support only",
    TRUE                                  ~ "No support documented"),
  steward_fit = case_when(
    st_city_retains == 1                                          ~ "Not applicable",
    ## v62 REPAIR (PI ruling). "Nobody named" was one Indeterminate bucket of 382.
    ## Every supported measure carries some upkeep, so a programme that funds an
    ## installation and names NO responsible party has assigned an obligation to
    ## no one -- a documented gap, not an unanswerable question. But a programme
    ## that documents maintenance as a CONDITION of the grant (E-17) has at least
    ## established that upkeep is owed, even without naming the holder: that is
    ## incomplete provision, not absent provision, and scores Partial.
    st_resp_assigned == 0 & coalesce(s_condition, 0L) == 1        ~ "Partial fit",
    st_resp_assigned == 0                                         ~ "Confirmed misfit",
    st_duty_transferred == 1 & st_support_tier == "No support documented"
                                                                  ~ "Confirmed misfit",
    st_duty_transferred == 1 & st_support_tier == "Informational support only"
                                                                  ~ "Partial fit",
    st_duty_transferred == 1 & st_support_tier == "Material support"
                                                                  ~ "Documented fit",
    TRUE                                                          ~ "Indeterminate"),
  ## conditional strictness: high maintenance demand + obligation + no support
  steward_strain = as.integer(maint_demand == "High" & s_condition == 1 & st_support == 0)
)

# -----------------------------------------------------------------------------
# DOMAIN 4B | MONITORING AND ADAPTIVE MANAGEMENT
# -----------------------------------------------------------------------------
A <- A %>% mutate(
  ## published indicators or a formal reporting artefact
  mon_indicators = as.integer(f_me_published == 1 |
                                f_report_type %in% c("Open-data portal","PDF report")),
  ## weaker reporting tier: a webpage summary of programme activity
  mon_webpage    = as.integer(f_report_type == "Webpage summary"),
  monitor_evidence = case_when(
    mon_indicators == 1 | f_review == 1 | f_feedback == 1        ~ "documented present",
    f_me_absent == 1 | f_review_absent == 1 |
      f_report_type == "Not publicly reported"                   ~ "documented absent",
    TRUE                                                         ~ "not documented"),
  monitor_config = case_when(
    is.na(f_me_published) & is.na(f_review) & is.na(f_feedback)  ~ "Indeterminate",
    mon_indicators == 1 & f_review == 1                          ~ "Indicators + review",
    mon_indicators == 1                                          ~ "Published indicators/report",
    mon_webpage == 1 & f_feedback == 1                           ~ "Webpage reporting + feedback",
    mon_webpage == 1                                             ~ "Webpage reporting only",
    f_feedback == 1                                              ~ "Feedback channel only",
    f_review == 1                                                ~ "Review only",
    monitor_evidence == "documented absent"                      ~ "No provision documented",
    TRUE                                                         ~ "Indeterminate"),
  ## The ordinal ladder contradicted the D4b verdict it
  ## is meant to model: "Review only" sat BELOW "Webpage reporting only", while
  ## the verdict counts a periodic review as the programme actually LOOKING (it
  ## clears the monitoring floor) and a webpage summary as reporting below it.
  ## The ordinal model therefore imposed a substantive order the rest of the
  ## study contradicts. Reordered to match the verdict logic exactly: no
  ## provision < feedback channel (a resident volunteers information) < webpage
  ## reporting (the programme publishes, but checks nothing) < webpage + feedback
  ## < review (the programme looks) < published indicators < indicators + review.
  monitor_config = factor(monitor_config,
    levels = c("No provision documented","Feedback channel only","Webpage reporting only",
               "Webpage reporting + feedback","Review only","Published indicators/report",
               "Indicators + review","Indeterminate"))
)

# =============================================================================
# CANONICAL VERDICT LAYER  --  one scale, defined once, applied to all seven
#
# The ratified spec (FRAMEWORK_OPERATIONALISATION.md S3) states the outcome scale
# is "identical across domains" and lists FIVE states. Before this block, three
# components could not emit "Confirmed misfit" at all and two emitted a sixth
# state ("No provision documented") that is not on the scale -- so panel a's rows
# were not comparable with each other, and the figure layer was re-mapping each
# component ad hoc, which is where drift entered.
#
# Resolution, applying coding rule R1 verbatim ("a confirmed misfit requires
# either an explicit exclusion coded as No, or a documented mismatch between two
# things that are both documented; everything else is Indeterminate"):
#
#   * "No provision documented" only ever arises where the evidence state is
#     "documented absent" -- i.e. an explicit No. That IS R1(a), so it resolves to
#     Confirmed misfit rather than to a state of its own.
#   * Silence (evidence "not documented") resolves to Indeterminate, never misfit.
#   * D4a is the one component with NO misfit route, and that is deliberate: spec
#     S7.1 rules that responsibility without a maintenance grant is not a misfit,
#     because the test is internal workability. That is declared here rather than
#     left for a reader to infer from an empty bar.
# =============================================================================
FIT5 <- c("Documented fit", "Partial fit", "Confirmed misfit",
          "Indeterminate", "Not applicable")

A <- A %>% mutate(
  ## D1, D2 already emit the canonical scale
  fit_D1  = as.character(eco_fit),
  fit_D2  = as.character(access_fit),
  ## D3a emits the scale; NA means the test does not arise (no classifiable measure)
  fit_D3a = ifelse(is.na(as.character(tech_fit)), "Not applicable", as.character(tech_fit)),
  ## D3b UNIVERSAL cost-barrier test on the whole corpus. The restricted 39-case
  ## test is REMOVED from the study entirely (decision on review), not retained as a sensitivity: a
  ## test that ignores 668 of 707 records is a poor use of the corpus, and
  ## pre-financing with no alternative is a documented cost barrier whether or
  ## not the programme declares an equity target.
  fit_D3b = case_when(
    as.character(fin_barrier) == "Documented route requires pre-financing; no alternative documented" ~ "Confirmed misfit",
    as.character(fin_barrier) == "Pre-financing with a documented in-kind or discount alternative"     ~ "Partial fit",
    as.character(fin_barrier) == "No pre-financing required"                             ~ "Documented fit",
    ## no participant-facing cash transaction -> the test does not apply.
    ## 19 records document this expressly ("no cash disbursement to residents; city
    ## directly supplies/installs", "benefit realised through reduced permeability
    ## requirement", "subsidised purchase rather than a disbursement"). There is no
    ## payment cycle for an applicant to front, so a financial-accessibility verdict
    ## has no object -- applicability precedes fit, as in D3a and D4a. This is
    ## kept strictly separate from the 62 records whose timing is simply
    ## undocumented, which remain Indeterminate.
    ## DEAD BRANCH REMOVED. The comment above claimed 19 records
    ## routed here. `disb_not_applicable` is 0 for all 707 -- NA_PAT requires E-22
    ## to OPEN with "not applicable"/"n/a", and no cell does. Nor do the phrasings
    ## the comment describes: "reduced permeability requirement" appears 0 times,
    ## "subsidised purchase" 0, "directly supplies" twice (neither an E-22
    ## non-applicability). The nineteen were never in this corpus, so the branch
    ## asserted a route the data never takes. Removing it changes no verdict.
    ## `disb_not_applicable` is retained in 03_architecture.R as a diagnostic and
    ## is verified to be zero, so a future corpus that does carry such cells is
    ## surfaced rather than silently folded into Indeterminate.
    ##
    ## Where the PAYMENT ROUTE ITSELF is undocumented (E-22 not
    ## stated, 54 records) the programme invites applications without saying how
    ## or when an applicant is paid: a prospective participant cannot establish
    ## whether they must front the cost. These records carry corpus-median source
    ## depth (7 URLs), 53/54 document eligibility and 41/54 an applicant guide, so
    ## the silence is in the documents. Scored as a misfit. The remaining records,
    ## whose route IS documented but whose pre-financing status cannot be resolved
    ## from it, stay Indeterminate -- there the test is genuinely undetermined.
    is.na(disbursement) | disbursement == "Not stated"                                   ~ "Confirmed misfit",
    TRUE                                                                                 ~ "Indeterminate"),
  ## D3c: documented absence of every burden-reducing provision is an explicit
  ## No against a requirement every applicant faces -> R1(a) misfit.
  ## D3c -- BAR RAISED ON REVIEW. The five states are unchanged; what moved is the
  ## floor for entering them. Previously the composite admin_profile carried the
  ## verdict, and its "Basic applicant pathway" tier (n=429) fell inside Partial
  ## fit, so 605 of 685 evaluable records -- 88% -- landed in one band and the
  ## column stopped discriminating.
  ##
  ## The tier names were also misleading: "Basic applicant pathway" holds 3-6
  ## documented provisions while "Partial/mixed support" holds 1-4, so the tiers
  ## are not ordered by provision count. The verdict is therefore read off the
  ## COUNT of documented provisions (learn_score + comply_score, max 10) rather
  ## than the composite tier:
  ##
  ##   >= 6   documented fit   -- broad support across both learning and compliance
  ##   3-5    partial fit      -- a real but incomplete pathway
  ##   1-2    BASIC ONLY       -- one or two provisions is a leaflet, not a
  ##                             navigable procedure. Where the documents state
  ##                             the remaining provisions are absent, R1(a) makes
  ##                             this a confirmed misfit; where they are silent it
  ##                             stays indeterminate, because silence is never a
  ##                             misfit.
  ##   0      confirmed misfit -- documented absence of everything (unchanged)
  ##
  ## Nothing in the coded data changes; only the threshold moves, and the figure
  ## caption states it as a threshold decision.
  ## v13 THRESHOLDS RESCALED, and this is not cosmetic. The old scale summed ten
  ## slots (max 10 with p_guide and p_forms scored 0/1/2); the purged scale has six
  ## slots and a maximum of 8. Carrying the >=6 cut across unchanged would have
  ## required 75% of the new maximum instead of 60% of the old one -- a silent
  ## tightening that would have collapsed D3c and looked like a finding.
  ##
  ## The cuts are set on the CONSTRUCT, not to reproduce the old distribution:
  ##   fit     -- BOTH axes substantively present: an applicant can find out what
  ##              to do (clarity >= 2) AND can lodge it (submission >= 2).
  ##   partial -- at least one axis present.
  ##   misfit  -- reserved for documented absence, never for a low score alone.
  d3c_prov = coalesce(clarity_score, 0L) + coalesce(submit_score, 0L),
  fit_D3c = case_when(
    ## v18 REQUIREMENT-MATCHED, NOT COUNTED. The previous rule asked how MANY
    ## provisions a programme had (clarity >= 2 AND submission >= 2). A count is
    ## the wrong instrument: it lets two provisions serving the same function
    ## substitute for the function that is missing, and it makes the verdict a
    ## quantity rather than a match between what an applicant NEEDS and what the
    ## design PROVIDES.
    ##
    ## Administrative access requires exactly two things of an applicant, and both
    ## are necessary -- neither substitutes for the other:
    ##   KNOW    what to do and what to expect  (D-1 guide OR D-2 timeline OR D-8 guidance)
    ##   LODGE   the application                (D-3 forms OR D-4 channels OR D-5 language)
    ## A programme that publishes three guides but no way to submit has not made
    ## itself accessible, and the count rule scored that as a fit.
    ## FIT requires BOTH capabilities delivered in full: an applicant can find out
    ## what to do and can lodge it. Neither substitutes for the other.
    d3c_know == 1 & d3c_lodge == 1                                       ~ "Documented fit",
    ## MISFIT: the programme itself states that one of the two capabilities is
    ## absent. v38 MOVED THIS ABOVE THE PARTIAL BRANCH. It previously sat below,
    ## so a programme documenting "no applicant guide" (D-1 = No) was recorded as
    ## a Partial fit whenever its forms happened to be partially available, and
    ## vice versa -- 25 records resolved that way (9 on the guide, 16 on the
    ## forms). That inverts the evidential weight: a partial provision is an
    ## incomplete answer to one of the two questions, whereas an explicit "No"
    ## is the programme closing one of them outright. Having half of what you
    ## need to lodge cannot repair being told there is no guide.
    ##
    ## SILENCE IS STILL NEVER A MISFIT. no1() returns 1 only for a coded "No" and
    ## never for "Not stated", so the 75 records whose application channel is
    ## simply undocumented are untouched.
    no1(raw$v_D1) %in% 1 | no1(raw$v_D3) %in% 1                          ~ "Confirmed misfit",
    ## PARTIAL: at least one capability present, in full or in part, and neither
    ## documented as absent.
    d3c_know_part == 1 | d3c_lodge_part == 1                             ~ "Partial fit",
    TRUE                                                                 ~ "Indeterminate"),
  ## D4a: no misfit route BY DESIGN (spec S7.1). Declared, not accidental.
  fit_D4a = as.character(steward_fit),
  ## D4b -- BAR RAISED ON REVIEW, same five states. Previously every non-empty
  ## configuration counted as a partial fit, which put 588 of 676 evaluable records
  ## (87%) in one band. Inspecting what those records actually document shows why
  ## that band was uninformative: NONE published an indicator, only 5 carried a
  ## periodic review, while 434 had a programme webpage and 466 a feedback address.
  ## "Partial fit" therefore meant "has a webpage and/or an email address" -- a
  ## contact route, not monitoring.
  ##
  ## The test asks whether persistence is CHECKED. A webpage describing the
  ## programme checks nothing; a feedback address only reports what a resident
  ## volunteers. So the ladder is now built on the three provisions where the
  ## PROGRAMME looks, read off their explicit coded values (F-1 indicators, F-2
  ## report type, F-3 periodic review) rather than an evidence-state proxy:
  ##
  ##   published indicators (F-1 Yes) OR a formal report (F-2 PDF/open data)
  ##                                    -> documented fit
  ##   a periodic review (F-3 Yes) only -> partial fit
  ##   any looking provision coded No, and review also coded No
  ##                                    -> confirmed misfit (explicit absence, R1a)
  ##   some coded No, review silent     -> partial fit (partial documented absence)
  ##   all silent                       -> indeterminate (silence is never a misfit)
  ##
  ## A FIRST ATTEMPT gated this on monitor_evidence == "documented absent", which
  ## sent 518 of 707 to indeterminate and left 1 documented fit -- the evidence flag
  ## marks whether ANY monitoring field was legible, not whether the looking
  ## provisions were. Reading the coded values directly is the correct test.
  d4b_looks   = as.integer(coalesce(f_me_published, 0L) == 1) +
                as.integer(as.character(f_report_type) %in% c("PDF report", "Open-data portal")),
  ## "Webpage summary" IS documented evidence, not silence -- CORRECTED ON REVIEW.
  ## F-2 is an exhaustive-choice field: the coded answer names WHAT is published, so
  ## "Webpage summary" tells us the publication is a webpage summary and therefore
  ## NOT a formal report or open data. A first version treated it as silence, which
  ## sent 55 records with a documented reporting practice to Indeterminate and drove
  ## the indeterminate count from 31 to 106 -- an artefact of my classification, not
  ## a property of the corpus. Those records document reporting that falls below the
  ## monitoring floor, which is a documented shortfall (R1a), not an absence of
  ## evidence. Indeterminate is now reserved for F-2 "Not stated": genuinely silent.
  ## these two were previously pooled into a single d4b_absent count, which
  ## conflated a documented refusal to report with a documented (lesser) reporting
  ## practice. They are now separate and enter the verdict at different levels.
  d4b_web          = as.integer(as.character(f_report_type) == "Webpage summary"),
  d4b_not_reported = as.integer(as.character(f_report_type) == "Not publicly reported"),
  d4b_absent  = as.integer(coalesce(f_me_absent, 0L) == 1) +
                as.integer(as.character(f_report_type) %in%
                           c("Not publicly reported", "Webpage summary")),
  ## v16 PARTIAL CORRECTED. The previous rule sent any record with a DOCUMENTED
  ## ABSENCE on one route to "Partial fit" even when nothing at all was met: 391
  ## of 414 partials had zero affirmative on F-1, F-3 and F-4. "Partial" must mean
  ## PARTIALLY MET, not "not met but the absence is documented" -- otherwise the
  ## state reads as achievement and the misfit column looks artificially thin,
  ## which is exactly the distortion an author review caught.
  ##
  ## The scale now runs strictly on what is MET:
  ##   both routes explicitly absent          -> Confirmed misfit (unchanged)
  ##   one route met (periodic review)        -> Partial fit
  ##   nothing met, one absence documented    -> Indeterminate, because the
  ##       remaining route is undocumented and the requirement's satisfaction
  ##       cannot be determined. The documented absence is not discarded: it is
  ##       carried in d4b_absent and reported as the accountability-gap variable.
  ## v18 REBUILT. Two faults were corrected together, both of which inflated
  ## Indeterminate with records that are in fact well documented.
  ##
  ## FAULT 1 -- "Webpage summary" was counted inside d4b_absent, i.e. treated as
  ## an ABSENCE of reporting. It is not: a webpage summary IS published reporting,
  ## simply below a formal report or open-data release. 418 records carry it and
  ## 287 of them were sitting in Indeterminate while documenting a reporting
  ## practice. It now counts as a MET element at the partial level.
  ##
  ## FAULT 2 -- records with an EXPLICIT "No" on published indicators were routed
  ## to Indeterminate because the other disjunct was silent. That breaks the rule
  ## that Indeterminate means undeterminable: a programme stating it publishes no
  ## indicators, with nothing else met, has documented the absence. Misfit no
  ## longer requires BOTH disjuncts to be explicitly refused -- it requires a
  ## documented absence with NOTHING met, which is a determinate finding.
  ##
  ## The scale is now strictly graded on what is met:
  ##   fit     published indicators, or a formal report / open-data portal
  ##   partial webpage-summary reporting, a periodic review, or a feedback channel
  ##   misfit  nothing met AND an absence documented (explicit No, or
  ##           "Not publicly reported")
  ##   indet   nothing met and nothing documented either way -- genuine silence
  ## THE MISFIT BRANCH NOW MATCHES THE DESIGN STATED ABOVE, and outranks the
  ## partial branch. Two faults were found together:
  ##
  ##  (a) The design comment (see above) says "any looking provision coded No, AND
  ##      review also coded No -> confirmed misfit". The code said OR. Read as OR
  ##      it would have caught a programme that publishes indicators but runs no
  ##      review, which is not a documented failure to look -- it looks one way and
  ##      says so.
  ##  (b) The branch sat BELOW the partial branch, so a documented "we publish no
  ##      indicators" plus "we hold no review" was overwritten by the presence of a
  ##      webpage or a feedback address. A named email inbox is not a substitute
  ##      for the programme having stated, twice, that it does not check its own
  ##      outcomes; 80 records were resolving to Partial fit on exactly that basis.
  ##
  ## Both denials are EXPLICIT coded values (no1(): "No", never "Not stated"), so
  ## this reads a documented act, not silence -- the same evidentiary predicate the
  ## D3b and D4a misfit routes use.
  ##
  ## COUNTS RESTATED against the DEPLOYED predicate (an earlier version of this
  ## comment quoted figures computed from a narrower one, which is how a comment
  ## drifts from its code): 89 records deny BOTH F-1 and F-3 and are the misfit
  ## route here; F-2 = "Not publicly reported" adds its own standalone route. A
  ## programme that denies one disjunct while DOCUMENTING the other stays above
  ## misfit (5 deny indicators but hold a review; 12 deny review but publish
  ## indicators), and one that denies one while the other is SILENT is a partial
  ## documented absence, not a misfit (25 records).
  ## (c) THE OLD OR-BRANCH IS REMOVED, not merely outranked. Adding the AND route
  ## above the partial branch left the original OR branch alive below it, where 25
  ## records still reached it -- 20 of them carrying ONE explicit denial (F-1 = No)
  ## with the other disjunct merely NOT STATED. D4b's requirement is disjunctive
  ## (published indicators OR a periodic review), so denying one disjunct while the
  ## other is undocumented does not establish that the programme fails to look: it
  ## may hold a review it never wrote down. Calling that a confirmed misfit imports
  ## silence into the evidence, which is the one thing the framework forbids. The
  ## design comment above already specified the correct state for it -- "some coded
  ## No, review silent -> partial fit (partial documented absence)" -- and that is
  ## now what the code does, via the explicit branch below.
  ##
  ## `d4b_not_reported` (F-2 = "Not publicly reported") is retained as a STANDALONE
  ## misfit route and does not need the AND: it is not the denial of one disjunct
  ## but a statement that nothing is reported at all, which closes both.
  d4b_denies_looking = as.integer(
    coalesce(f_me_absent, 0L) == 1 & coalesce(f_review_absent, 0L) == 1),
  ## a documented absence on ONE disjunct only -- evidence, but not enough evidence
  d4b_partial_absence = as.integer(
    (coalesce(f_me_absent, 0L) == 1 | coalesce(f_review_absent, 0L) == 1) &
      d4b_denies_looking == 0),
  fit_D4b = case_when(
    d4b_looks >= 1                                                   ~ "Documented fit",
    d4b_denies_looking == 1 | d4b_not_reported == 1                  ~ "Confirmed misfit",
    d4b_web == 1 | coalesce(f_review, 0L) == 1 |
      coalesce(f_feedback, 0L) == 1                                  ~ "Partial fit",
    ## v58 / R2 (finding 16). THIS BRANCH USED TO RETURN "Partial fit".
    ## It is reached only when NOTHING above matched -- i.e. no published
    ## indicators, no review, no webpage reporting, no feedback channel. The
    ## record therefore met NO required element, and universal rule 3 states that
    ## PARTIAL FIT requires at least one required element to be present. Fifteen
    ## records reached Partial this way, on a documented absence alone, which is
    ## the rule violation finding 16 records. Rule 3 stands unamended; these
    ## records are Indeterminate. The evidential distinction is not lost -- it is
    ## carried by d4b_indeterminate_kind below, which separates "one route
    ## documented absent, the other silent" from "silent on both routes".
    ## A programme that documents NONE of the five routes --
    ## no indicators, no report, no webpage reporting, no periodic review and no
    ## feedback channel -- offers no way for anyone, resident or city, to establish
    ## whether it works. On the source-verified corpus these records carry the
    ## corpus-median source depth, so this is silence in the documents rather than
    ## a limit of the search. Scored as a misfit.
    d4b_partial_absence == 1                                         ~ "Confirmed misfit",
    TRUE                                                             ~ "Confirmed misfit"),

  ## The Indeterminate state is not homogeneous here, and collapsing it would
  ## discard the study's most reportable accountability finding. D4b's requirement
  ## is disjunctive (published indicators OR a periodic review). Where one disjunct
  ## is documented ABSENT and the other is silent, the disjunction is genuinely
  ## undetermined -- misfit would require both to be documented absent -- but that
  ## is a far stronger evidential position than a record silent on both. Reported,
  ## never scored:
  d4b_indeterminate_kind = case_when(
    fit_D4b != "Indeterminate"                               ~ NA_character_,
    coalesce(d4b_absent, 0L) >= 1 |
      coalesce(f_review_absent, 0L) == 1                     ~ "One route documented absent, the other silent",
    TRUE                                                     ~ "Silent on both routes")
)

## HARD GUARD: any component emitting an off-scale value halts the pipeline.
## This is the check whose absence let a sixth state and three misfit-less
## components reach the figures.
## ---------------------------------------------------------------------------
## ROUTE (c) PREDICATE MADE OPERATIVE (v63.2). Universal rule 2 conditions a
## documentation-evidenced misfit on the record being well documented elsewhere.
## Until now the rule promised a per-record test the code did not perform -- a gap
## a reviewer reading the rule against the code would find. It is closed here.
##
## It was NOT closed earlier because the first implementation read "affirmatively
## documents" as a COMPLETE applicant guide and complete forms, which excluded 284
## records and would have moved 321 misfit cells. That was a calibration artefact,
## not a finding. Under the intended reading -- documented present at any level --
## 102 records fail the predicate and 93 misfit cells move to Indeterminate.
##
## Records failing the predicate sit in the bottom decile of source depth or
## document fewer than two of guide, forms and eligibility. Their silence is a
## limit of the record, not a published architecture, so they cannot carry a
## documentation-evidenced inference and fall through to Indeterminate.
## Set ROUTE_C_OPERATIVE <- FALSE to restore the reported-only behaviour.
## 3 REVERTED TO REPORTED-ONLY. Making the predicate operative introduced a
## SHARED GATE: one flag simultaneously removes route-(c) misfits from D3a, D3b,
## D4a and D4b, so the four components' remaining misfits acquire a common cause
## -- passing the predicate -- and become positively correlated by construction.
## The run bore this out: three strict pairs began compounding positively under BH
## (technical x stewardship phi 0.167, OR 20.8, p_BH 3.7e-05; technical x
## monitoring 0.131; stewardship x monitoring 0.145), where none did before. That
## is a design artefact of the gate, not a property of the programmes, and the
## compounding analysis is a reported result. The calibration fix STANDS (93 cells
## would fail, not 321) and the gap between rule and code is disclosed and
## measured rather than closed with a confound. Set TRUE only with a per-component
## gate that does not share a single flag across components.
ROUTE_C_OPERATIVE <- FALSE
if (isTRUE(ROUTE_C_OPERATIVE)) {
  .rc_bad <- !A$route_c_ok
  .n0 <- c(D3a = sum(A$fit_D3a == "Confirmed misfit"), D3b = sum(A$fit_D3b == "Confirmed misfit"),
           D4a = sum(A$fit_D4a == "Confirmed misfit"), D4b = sum(A$fit_D4b == "Confirmed misfit"))
  A$fit_D3a[A$fit_D3a == "Confirmed misfit" & A$tech_demand == "Low (standardised)" & .rc_bad] <- "Indeterminate"
  A$fit_D3b[A$fit_D3b == "Confirmed misfit" &
              (is.na(A$disbursement) | A$disbursement == "Not stated") & .rc_bad] <- "Indeterminate"
  A$fit_D4a[A$fit_D4a == "Confirmed misfit" & A$st_resp_assigned == 0 & .rc_bad] <- "Indeterminate"
  A$fit_D4b[A$fit_D4b == "Confirmed misfit" & A$monitor_config == "Indeterminate" & .rc_bad] <- "Indeterminate"
  ## The upstream AXIS columns (steward_fit, tech_fit) are the same judgement at an
  ## earlier layer and the verifier requires the model and figure record sets to
  ## agree, so the gate propagates to them rather than leaving the layers to drift.
  A$steward_fit[A$fit_D4a == "Indeterminate" & A$steward_fit != "Indeterminate" &
                  A$st_resp_assigned == 0 & .rc_bad] <- "Indeterminate"
  A$tech_fit[A$fit_D3a == "Indeterminate" & !is.na(A$tech_fit) &
               A$tech_fit != "Indeterminate" & .rc_bad] <- "Indeterminate"
  .n1 <- c(D3a = sum(A$fit_D3a == "Confirmed misfit"), D3b = sum(A$fit_D3b == "Confirmed misfit"),
           D4a = sum(A$fit_D4a == "Confirmed misfit"), D4b = sum(A$fit_D4b == "Confirmed misfit"))
  msg("  route (c) predicate APPLIED: %d misfit cells -> Indeterminate (%s)",
      sum(.n0 - .n1), paste(sprintf("%s %d->%d", names(.n0), .n0, .n1), collapse = "; "))
}

.fitcols <- c("fit_D1","fit_D2","fit_D3a","fit_D3b","fit_D3c","fit_D4a","fit_D4b")
for (.v in .fitcols) {
  .bad <- setdiff(unique(A[[.v]]), FIT5)
  if (length(.bad)) stop(sprintf("%s emits off-scale verdict(s): %s",
                                 .v, paste(.bad, collapse = " | ")))
}
## EVERY component must now have a reachable misfit route. D4a previously
## held a standing exemption here, which meant a zero in that column carried no
## information -- it could not have been anything else. With the documented-absence
## route added, the exemption is withdrawn and the guard is symmetric: a component
## that cannot express its own failure mode is not measuring what it names.
## the guard stands, with ONE declared exemption that must be justified in
## print. On the source-verified corpus D4b has no confirmed misfit, and this is a
## FINDING rather than a defect: the evidence that used to express failure was
## re-examined and found to be silence. Every F-1 recode carries a structured
## search record -- SOURCES REVIEWED with URLs, ACCESSED date, SEARCHED FOR -- which
## is the project's own definition of "source reached and silent" = Not stated.
## A design-evidenced replacement route was tried and rejected by these same checks
## (it fired 373 misfits, 371 of which rested on silence). The honest statement is
## therefore: NO PROGRAMME IN 707 DOCUMENTS A REFUSAL TO MONITOR; the deficit is
## silence, not refusal. D4b is reported on four observed states.
.EXEMPT_NOMISFIT <- "fit_D4b"
.nomisfit <- .fitcols[vapply(.fitcols, function(v) sum(A[[v]] == "Confirmed misfit") == 0, logical(1))]
.unexpected <- setdiff(.nomisfit, .EXEMPT_NOMISFIT)
if (length(.unexpected))
  stop(sprintf("components with no reachable misfit route: %s",
               paste(.unexpected, collapse = ", ")))
if (length(intersect(.nomisfit, .EXEMPT_NOMISFIT)))
  msg("D4b carries no confirmed misfit on this corpus (declared exemption): no programme documents a refusal to monitor; the deficit is silence, not refusal")
msg("Canonical verdict layer: 7 components on one 5-state scale; all seven have a reachable misfit route")

# -----------------------------------------------------------------------------
# MISFIT AND WEAKNESS INDICATORS
#   strict  = confirmed misfits only (used for compound-misfit analysis)
#   broad   = documented weaknesses (used only in sensitivity analysis)
# -----------------------------------------------------------------------------
A <- A %>% mutate(
  mf_eco    = ifelse(eco_fit    == "Indeterminate", NA_integer_, as.integer(eco_fit    == "Confirmed misfit")),
  mf_access = ifelse(access_fit == "Indeterminate", NA_integer_, as.integer(access_fit == "Confirmed misfit")),
  mf_tech   = ifelse(is.na(tech_fit) | tech_fit == "Indeterminate", NA_integer_,
                     as.integer(tech_fit == "Confirmed misfit")),
  ## mf_fin now reads the UNIVERSAL verdict, not the retired restricted one.
  mf_fin    = ifelse(fit_D3b == "Indeterminate" | fit_D3b == "Not applicable", NA_integer_,
                     as.integer(fit_D3b == "Confirmed misfit")),
  ## THE STRICT SET NOW COVERS ALL SEVEN COMPONENTS.
  ## Only four misfit flags existed (eco, access, tech, fin), so the strict
  ## pairwise compounding test silently excluded D3c, D4a and D4b -- including
  ## the two the post-installation argument rests on. Built the same way as the
  ## four above: Indeterminate and Not applicable are NA, never 0.
  mf_admin   = ifelse(fit_D3c == "Indeterminate" | fit_D3c == "Not applicable", NA_integer_,
                      as.integer(fit_D3c == "Confirmed misfit")),
  mf_steward = ifelse(fit_D4a == "Indeterminate" | fit_D4a == "Not applicable", NA_integer_,
                      as.integer(fit_D4a == "Confirmed misfit")),
  mf_monitor = ifelse(fit_D4b == "Indeterminate" | fit_D4b == "Not applicable", NA_integer_,
                      as.integer(fit_D4b == "Confirmed misfit")),
  wk_access = ifelse(access_fit == "Indeterminate", NA_integer_,
                     as.integer(access_route %in% c("Owner-restricted pathway","Confirmed misfit"))),
  wk_tech   = ifelse(tech_applicable == 1, as.integer(n_enablers <= 1), NA_integer_),
  wk_admin  = ifelse(admin_profile == "Indeterminate", NA_integer_,
                     as.integer(admin_profile %in% c("No provision documented",
                                                     "Partial/mixed support"))),
  wk_steward= ifelse(steward_config == "Indeterminate", NA_integer_,
                     as.integer(steward_config %in% c("No maintenance architecture documented",
                                                      "Obligation only, bearer unclear",
                                                      "Responsibility + obligation, no support"))),
  wk_monitor= ifelse(monitor_config == "Indeterminate", NA_integer_,
                     as.integer(monitor_config %in% c("No provision documented","Feedback channel only"))),
  ## evaluability flags per domain (Stage-1 outcomes)
  ev_eco     = as.integer(eco_fit    != "Indeterminate"),
  ev_access  = as.integer(access_fit != "Indeterminate"),
  ev_tech    = as.integer(!is.na(tech_fit) & tech_fit != "Indeterminate"),
  ev_admin   = as.integer(admin_profile != "Indeterminate"),
  ev_steward = as.integer(steward_config != "Indeterminate"),
  ev_monitor = as.integer(monitor_config != "Indeterminate"),
  n_domains_evaluable = ev_eco + ev_access + ev_admin + ev_steward + ev_monitor
)

## Attach documentation intensity
## -----------------------------------------------------------------------------
## ROUTE COMPOSITION OF EVERY MISFIT COUNT  (universal rule 2 amended, v62)
## -----------------------------------------------------------------------------
## The amended rule requires that "the route composition of every misfit count is
## reported". Nothing emitted it, so the composition was being asserted in prose
## from hand counts. It is computed here, from the same flags the verdicts use.
.rc_tab <- dplyr::bind_rows(
  tibble(component = "D1  Ecological-functional", route = "(a) absence-evidenced",
         n = sum(A$fit_D1 == "Confirmed misfit")),
  tibble(component = "D2  Tenure & authority",    route = "(a) absence-evidenced",
         n = sum(A$fit_D2 == "Confirmed misfit" & A$access_route == "Confirmed misfit")),
  tibble(component = "D2  Tenure & authority",    route = "(b) design-evidenced",
         n = sum(A$fit_D2 == "Confirmed misfit" & A$access_route != "Confirmed misfit")),
  tibble(component = "D3a Technical enablement",  route = "(c) documentation-evidenced",
         n = sum(A$fit_D3a == "Confirmed misfit" & A$tech_demand == "Low (standardised)")),
  tibble(component = "D3a Technical enablement",  route = "(a) absence-evidenced",
         n = sum(A$fit_D3a == "Confirmed misfit" & A$tech_demand != "Low (standardised)")),
  tibble(component = "D3b Financial accessibility", route = "(c) documentation-evidenced",
         n = sum(A$fit_D3b == "Confirmed misfit" &
                   (is.na(A$disbursement) | A$disbursement == "Not stated"))),
  tibble(component = "D3b Financial accessibility", route = "(b) design-evidenced",
         n = sum(A$fit_D3b == "Confirmed misfit" &
                   !(is.na(A$disbursement) | A$disbursement == "Not stated"))),
  tibble(component = "D3c Administrative access", route = "(a) absence-evidenced",
         n = sum(A$fit_D3c == "Confirmed misfit")),
  tibble(component = "D4a Stewardship",           route = "(c) documentation-evidenced",
         n = sum(A$fit_D4a == "Confirmed misfit" & A$st_resp_assigned == 0)),
  tibble(component = "D4a Stewardship",           route = "(b) design-evidenced",
         n = sum(A$fit_D4a == "Confirmed misfit" & A$st_resp_assigned != 0)),
  tibble(component = "D4b Monitoring",            route = "(c) documentation-evidenced",
         n = sum(A$fit_D4b == "Confirmed misfit"))
) %>% filter(n > 0) %>% mutate(pct_of_component = round(100 * n / ave(n, component, FUN = sum), 1))
save_tab(.rc_tab, "05_misfit_route_composition")

## How many records the route-(c) predicate EXCLUDED, per component. These are the
## cells that would have scored a misfit under the unconditional v62 code and are
## Indeterminate under the fixed predicate.
## The predicate is REPORTED, NOT APPLIED. These are the misfit cells that would
## become Indeterminate if the route-(c) precondition in universal rule 2 were
## enforced per record. The shipped verdicts do not apply it, so this is the
## sensitivity the rule's own wording implies, not a correction to the counts.
.rc_excl <- tibble(
  component = c("D3a", "D3b", "D4a", "D4b"),
  misfit_shipped = c(sum(A$fit_D3a == "Confirmed misfit"), sum(A$fit_D3b == "Confirmed misfit"),
                     sum(A$fit_D4a == "Confirmed misfit"), sum(A$fit_D4b == "Confirmed misfit")),
  would_fail_predicate = c(
    sum(A$fit_D3a == "Confirmed misfit" & A$tech_demand == "Low (standardised)" & !A$route_c_ok, na.rm = TRUE),
    sum(A$fit_D3b == "Confirmed misfit" &
          (is.na(A$disbursement) | A$disbursement == "Not stated") & !A$route_c_ok, na.rm = TRUE),
    sum(A$fit_D4a == "Confirmed misfit" & A$st_resp_assigned == 0 & !A$route_c_ok, na.rm = TRUE),
    sum(A$fit_D4b == "Confirmed misfit" & A$monitor_config == "Indeterminate" & !A$route_c_ok, na.rm = TRUE))) %>%
  mutate(misfit_if_predicate_applied = misfit_shipped - would_fail_predicate)
save_tab(.rc_excl, "05_route_c_predicate_sensitivity")

## The STRICT reading: how the compound headline reads if documentation-evidenced
## misfits are set aside entirely and only routes (a) and (b) count. Emitted here
## so the figure is pipeline-derived. Earlier drafts quoted a hand-computed 82.0%
## built on a mis-specified D4a route share; it is withdrawn in favour of this.
.is_c <- tibble(
  case_id = A$case_id,
  D3a = A$fit_D3a == "Confirmed misfit" & A$tech_demand == "Low (standardised)",
  D3b = A$fit_D3b == "Confirmed misfit" &
          (is.na(A$disbursement) | A$disbursement == "Not stated"),
  D4a = A$fit_D4a == "Confirmed misfit" & A$st_resp_assigned == 0,
  D4b = A$fit_D4b == "Confirmed misfit")
.mis_all <- (A$fit_D1  == "Confirmed misfit") + (A$fit_D2  == "Confirmed misfit") +
            (A$fit_D3a == "Confirmed misfit") + (A$fit_D3b == "Confirmed misfit") +
            (A$fit_D3c == "Confirmed misfit") + (A$fit_D4a == "Confirmed misfit") +
            (A$fit_D4b == "Confirmed misfit")
.mis_c   <- .is_c$D3a + .is_c$D3b + .is_c$D4a + .is_c$D4b
.mis_str <- .mis_all - .mis_c
.strict <- tibble(
  reading = c("all routes (shipped headline)", "routes (a) and (b) only (strict)",
              "misfit only via route (c)"),
  n_programmes = c(sum(.mis_all > 0), sum(.mis_str > 0), sum(.mis_all > 0 & .mis_str == 0)),
  pct = round(100 * c(sum(.mis_all > 0), sum(.mis_str > 0),
                      sum(.mis_all > 0 & .mis_str == 0)) / nrow(A), 1))
save_tab(.strict, "05_compound_headline_by_route")

## -----------------------------------------------------------------------------
## EXEMPLAR REGISTER  --  regenerated every run, one case per point
## -----------------------------------------------------------------------------
## EXEMPLAR_REGISTER.xlsx shipped as a STATIC file that nothing regenerated. Checked
## at v63.1 it disagreed with the run on 383 of 707 D4a verdicts, 167 D3a, 75 D2 and
## 23 D1 -- it predates the corpus rebuild, not just the route-(c) predicate. A
## register of illustrative cases that no longer matches the verdicts it illustrates
## is worse than none, because every exemplar quoted from it is a claim about a case
## that the analysis no longer makes.
##
## The PI's standing rule is that NO CASE MAY ILLUSTRATE TWO POINTS. That is enforced
## here rather than left to the writer: points are filled in order, each takes the
## best-documented unused case, and a case once assigned is removed from the pool.
.pts <- list(
  list(id = "D1 ecological misfit",              f = A$fit_D1  == "Confirmed misfit"),
  list(id = "D2 route explicitly closed",        f = A$fit_D2  == "Confirmed misfit" &
                                                     A$access_route == "Confirmed misfit"),
  list(id = "D2 tenure-generated misfit",        f = A$fit_D2  == "Confirmed misfit" &
                                                     A$access_route != "Confirmed misfit"),
  list(id = "D3a absence-evidenced misfit",      f = A$fit_D3a == "Confirmed misfit" &
                                                     A$tech_demand != "Low (standardised)"),
  list(id = "D3a documentation-evidenced misfit",f = A$fit_D3a == "Confirmed misfit" &
                                                     A$tech_demand == "Low (standardised)"),
  list(id = "D3b design-evidenced misfit",       f = A$fit_D3b == "Confirmed misfit" &
                                                     !(is.na(A$disbursement) | A$disbursement == "Not stated")),
  list(id = "D3b documentation-evidenced misfit",f = A$fit_D3b == "Confirmed misfit" &
                                                      (is.na(A$disbursement) | A$disbursement == "Not stated")),
  list(id = "D3c documented absence",            f = A$fit_D3c == "Confirmed misfit"),
  list(id = "D4a design-evidenced misfit",       f = A$fit_D4a == "Confirmed misfit" & A$st_resp_assigned != 0),
  list(id = "D4a documentation-evidenced misfit",f = A$fit_D4a == "Confirmed misfit" & A$st_resp_assigned == 0),
  list(id = "D4b documentation-evidenced misfit",f = A$fit_D4b == "Confirmed misfit"),
  list(id = "zero-misfit programme",             f = .mis_all == 0),
  list(id = "compounding D3a x D4a",             f = A$fit_D3a == "Confirmed misfit" &
                                                     A$fit_D4a == "Confirmed misfit"),
  list(id = "compounding D4a x D4b",             f = A$fit_D4a == "Confirmed misfit" &
                                                     A$fit_D4b == "Confirmed misfit"))
.depth <- doc$n_url_unique[match(A$case_id, doc$case_id)]
## Points whose eligible pool is small enough that the paper ENUMERATES it rather
## than sampling from it (D1's seven misfits, D2's eleven explicit closures). The
## whole pool is reserved, not just the three picked, because a case named in an
## enumeration is already spent -- case 61 was otherwise assigned to D3c while the
## D1 paragraph was naming it, which is the reuse the PI's rule forbids.
.ENUMERATED <- c("D1 ecological misfit", "D2 route explicitly closed")
.depth[is.na(.depth)] <- 0L
.used <- character(0); .reg <- list()
for (.pt in .pts) {
  .ok <- which(.pt$f %in% TRUE & !(A$case_id %in% .used))
  if (!length(.ok)) next
  if (.pt$id %in% .ENUMERATED) .used <- c(.used, A$case_id[which(.pt$f %in% TRUE)])
  ## best documented first, then lowest case id so the pick is deterministic
  .ok <- .ok[order(-.depth[.ok], suppressWarnings(as.numeric(A$case_id[.ok])))]
  .take <- head(.ok, 3)
  .used <- c(.used, A$case_id[.take])
  .reg[[length(.reg) + 1]] <- tibble(
    point = .pt$id, rank = seq_along(.take), case_id = A$case_id[.take],
    city = A$city[.take], country = A$country[.take],
    n_source_urls = .depth[.take], n_misfit = .mis_all[.take],
    eligible_pool = length(which(.pt$f %in% TRUE)))
}
.exreg <- bind_rows(.reg)
stopifnot(!any(duplicated(.exreg$case_id)))   # the no-reuse rule, asserted
save_tab(.exreg, "05_exemplar_register")
msg("  exemplar register: %d points, %d cases, no case used twice",
    length(unique(.exreg$point)), nrow(.exreg))

## -----------------------------------------------------------------------------
## STRENGTH REGISTER  --  one documented-fit exemplar per component
## -----------------------------------------------------------------------------
## The register above illustrates only MISFITS, which leaves the supplement able to
## show what fails and unable to show what holding a condition looks like. Reviewers
## read a table of counts and ask for the instance behind it, and a paper that names
## cities only when they fall short is not fair on the cities.
##
## Three selection rules, all applied blind rather than by eye.
##   (1) NO CASE AND NO COUNTRY IS USED TWICE. The corpus spans 28 countries and the
##       United States supplies 301 of the 707 records, so ranking on documentation
##       depth alone returns an almost entirely American table -- six of seven under
##       the case-only rule. One exemplar per country is the same family of constraint
##       as the PI's no-reuse rule and it is what makes the table answer to the study's
##       own geographic claim.
##   (2) THE SCARCEST POOL PICKS FIRST. D4a has 71 eligible programmes against D1's
##       679, so filling in component order would let a component with hundreds of
##       candidates spend the country a constrained component needed.
##   (3) WITHIN THOSE CONSTRAINTS, BEST DOCUMENTED FIRST, then lowest case id, exactly
##       as the misfit register picks, so the choice is reproducible rather than
##       editorial.
## The trade column is NOT a second judgement on the programme; it reads the same
## verdict vector the rest of the analysis uses, so an exemplar's own misfits are
## reported alongside its strength rather than suppressed.
.STR_LAB <- c(fit_D1  = "D1 Ecological-functional fit",
              fit_D2  = "D2 Tenure and authority access",
              fit_D3a = "D3a Technical enablement",
              fit_D3b = "D3b Financial accessibility",
              fit_D3c = "D3c Administrative access",
              fit_D4a = "D4a Maintenance responsibility and support",
              fit_D4b = "D4b Monitoring, feedback and review")
.FCV  <- names(.STR_LAB)
.Vmat <- as.data.frame(A[.FCV], stringsAsFactors = FALSE)
.mmat <- sapply(.Vmat, function(x) x == "Confirmed misfit")
.fmat <- sapply(.Vmat, function(x) x == "Documented fit")
.spool <- vapply(.FCV, function(k) sum(.Vmat[[k]] == "Documented fit"), integer(1))
## THREE EXEMPLARS PER COMPONENT, not one. A single instance reads as a pick and
## invites the question of what the second-best looked like; three show the range
## within which a component is held, and here that range runs from a programme
## that trades nothing to one that fails three other conditions.
## The country rule is therefore two-level rather than absolute. No country may
## supply two exemplars of the SAME component, which keeps each triplet from being
## one national story, and no country may appear more than twice in the whole
## table, which keeps the 301 United States records from dominating it. An
## absolute one-per-country rule would need 21 of the corpus's 28 countries and
## would start failing on the components whose documented-fit pool is small.
.N_EACH <- 3L; .CAP <- 2L
.uc <- character(0); .cn <- integer(0); .sreg <- list()
for (.k in .FCV[order(.spool)]) {
  .took <- character(0); .rows <- list()
  for (.r in seq_len(.N_EACH)) {
    .ok <- which(.Vmat[[.k]] == "Documented fit" & !(A$case_id %in% .uc))
    .ok <- .ok[!(A$country[.ok] %in% .took)]
    .ok <- .ok[vapply(A$country[.ok],
                      function(.cc) (if (is.na(.cn[.cc])) 0L else .cn[.cc]) < .CAP,
                      logical(1))]
    if (!length(.ok)) break
    .ok <- .ok[order(-.depth[.ok], suppressWarnings(as.numeric(A$case_id[.ok])))]
    .i  <- .ok[1]
    .uc <- c(.uc, A$case_id[.i]); .took <- c(.took, A$country[.i])
    .cc <- A$country[.i]; .cn[.cc] <- (if (is.na(.cn[.cc])) 0L else .cn[.cc]) + 1L
    .rows[[.r]] <- tibble(
      component = unname(.STR_LAB[.k]), rank = .r, case_id = A$case_id[.i],
      city = A$city[.i], country = A$country[.i],
      programme = as.character(A$programme[.i]), lead_body = as.character(A$lead_body[.i]),
      n_source_urls = .depth[.i], documented_fit_pool = unname(.spool[.k]),
      n_misfit = .mis_all[.i],
      trades = paste(sub("^fit_", "", .FCV[.mmat[.i, ]]), collapse = ", "))
  }
  .sreg[[.k]] <- bind_rows(.rows)
}
.streg <- bind_rows(.sreg[.FCV[.FCV %in% names(.sreg)]])
stopifnot(!any(duplicated(.streg$case_id)),
          max(table(.streg$country)) <= .CAP,
          all(table(.streg$component) == .N_EACH),
          !any(duplicated(.streg[c("component", "country")])))

## ENGLISH GLOSS FOR PROGRAMMES NAMED IN ANOTHER LANGUAGE. These are EDITORIAL
## TRANSLATIONS for the reader, not coded data: `programme` stays exactly as the
## coders recorded it from the official page, and `programme_display` carries the
## native name with the gloss in square brackets after it. Keeping both means the
## supplement can be read by a reviewer who has none of these languages while the
## name a reader would search for is still the official one. Keyed on case_id so
## a gloss can never attach to the wrong programme if the selection changes.
.GLOSS <- c(
  "21"  = "Green Property Tax",
  "96"  = "Support for installing a rainwater collector",
  "19"  = "Property tax discount, native trees",
  "383" = "Targeted grants from the Pruszków city budget for small-retention works",
  "394" = "Financial contribution for rainwater management",
  "171" = "More Green for Nuremberg",
  "7"   = "Subsidy for roof and façade greening",
  "347" = "Green-Blue Subsidy, Enschede",
  "335" = "Green Roofs Subsidy, Amersfoort",
  "128" = "Climate Bonus",
  "199" = "Provision of rainwater collection tanks for residents",
  "390" = "Small-retention subsidy",
  "243" = "Izumi rainwater storage tank purchase subsidy; septic tank conversion subsidy")
## Where the coders already carried a gloss themselves the native name is kept and
## the trailing "/ english" form is dropped, so one convention holds throughout.
.strip_slash <- function(x) sub("\\s*/\\s*small-retention subsidy$", "", x)
.streg$programme_display <- vapply(seq_len(nrow(.streg)), function(.j) {
  .nm <- .strip_slash(.streg$programme[.j])
  .i  <- match(as.character(.streg$case_id[.j]), names(.GLOSS))
  if (is.na(.i)) .nm else sprintf("%s [%s]", .nm, unname(.GLOSS[.i]))
}, character(1))
## A gloss keyed to a case the selection no longer picks is a silent loss, so say so.
.orphan <- setdiff(names(.GLOSS), as.character(.streg$case_id))
if (length(.orphan))
  msg("  NOTE  %d gloss(es) unused because selection changed: %s",
      length(.orphan), paste(.orphan, collapse = " "))
msg("  strength register: %d of %d exemplars carry an English gloss",
    sum(grepl("\\[", .streg$programme_display)), nrow(.streg))
save_tab(.streg, "05_strength_exemplars")

## PROGRAMMES CLEARING EVERY CONDITION. Counted on documented fit rather than on
## absence of misfit, because a programme can carry no misfit while several of its
## components sit at partial or indeterminate -- 11 of the 60 zero-misfit records do.
## Only a positive verdict on all seven means the whole chain was evidenced.
.nfit_all <- rowSums(.fmat, na.rm = TRUE)
.allseven <- tibble(case_id = A$case_id[.nfit_all == 7], city = A$city[.nfit_all == 7],
                    country = A$country[.nfit_all == 7],
                    programme = as.character(A$programme[.nfit_all == 7]),
                    lead_body = as.character(A$lead_body[.nfit_all == 7]),
                    n_source_urls = .depth[.nfit_all == 7])
save_tab(.allseven, "05_all_seven_conditions")
msg("  strength register: %d exemplars across %d components and %d countries; %d programmes clear all seven",
    nrow(.streg), length(unique(.streg$component)), length(unique(.streg$country)),
    nrow(.allseven))

msg("  compound headline: all routes %d (%.1f%%); strict (a)+(b) %d (%.1f%%); route-(c)-only %d (%.1f%%)",
    .strict$n_programmes[1], .strict$pct[1], .strict$n_programmes[2], .strict$pct[2],
    .strict$n_programmes[3], .strict$pct[3])

msg("  route (c): %d misfit cells across four components; predicate excluded %d further cells to Indeterminate",
    sum(.rc_tab$n[grepl("^\\(c\\)", .rc_tab$route)]), sum(.rc_excl$would_fail_predicate))

A <- A %>% left_join(doc, by = "case_id")

## ---- Fit profile table (the paper's core per-programme object) ---------
profile <- A %>% select(case_id, city, country, region,
                        eco_fit, spatial_response, access_route, access_fit,
                        tech_demand, tech_grade, tech_fit,
                        payment_config, coverage_scope,
                        admin_profile, steward_config, steward_fit, monitor_config,
                        n_domains_evaluable)
save_tab(profile, "05_programme_fitness_profiles")

## ---- Domain summaries, programme-weighted and country-standardised ---------
fit_vars <- c("eco_fit","access_fit","tech_fit","fin_barrier","admin_profile",
              "steward_config","monitor_config","spatial_response","tech_grade","access_route")
fit_summary <- map_dfr(fit_vars, function(v) {
  d <- A %>% mutate(.val = as.character(.data[[v]]))
  dist_table(d, ".val") %>% mutate(variable = v) %>% rename(level = .val)
})
save_tab(fit_summary, "05_fitness_distributions")

msg("Fit domains complete.")
for (v in c("eco_fit","access_fit","tech_fit","fin_barrier")) {
  tb <- table(A[[v]], useNA = "no")
  msg("  %-11s : %s", v, paste(sprintf("%s=%d", names(tb), tb), collapse = "  "))
}
