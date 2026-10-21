# =============================================================================
# 03_architecture.R  |  PROGRAMME ARCHITECTURE
# The programme's component parts: what it seeks to do, for whom, through which
# means. Architecture is descriptive; it is NOT fit. It supplies (a) RQ1,
# (b) the active variables for the typology, (c) predictors in Stage-2 models.
#
# Every variable is defined exactly once. Where a coded field carries more
# detail than a single indicator can hold (E-7, E-13, E-14, E-19, D-4, D-5,
# E-18, F-4, A-7i), the detail is preserved as separate named items and only
# summarised later, in 04_configurations.R.
# =============================================================================

arch <- raw %>% transmute(
  case_id,

  # ===========================================================================
  # B | ENVIRONMENTAL ORIENTATION
  # ===========================================================================
  ## VOCABULARY WIDTH: the original patterns matched only the canonical label for
  ## each family, so a B-5 recorded in the programme's own words ("runoff
  ## reduction", "potable water savings", "shade and cooling", "pollinator
  ## habitat") scored zero objectives and sent the record to D1 Indeterminate on
  ## the applicability gate rather than on any absence of evidence. The families
  ## themselves are unchanged -- these are synonyms for the SAME five objectives,
  ## drawn from the values actually present in the coded field, not new categories.
  ## The gate itself is untouched: a record with no objective at all is still
  ## Indeterminate, because that is a real limit of the documentation.
  obj_storm    = multi_has(v_B5, "stormwater|storm water|flood|runoff|run-off|drainage|sewer overflow|water retention|retention|infiltration|permeab"),
  obj_heat     = multi_has(v_B5, "urban heat|heat|cooling|shade|shading|temperature|heat island"),
  obj_biodiv   = multi_has(v_B5, "biodiversity|habitat|pollinator|ecolog|native (planting|species|vegetation)|greening|green space|wildlife"),
  obj_drought  = multi_has(v_B5, "drought|water conservation|water conserv|water saving|water efficien|potable water|xeriscap|irrigation"),
  obj_amenity  = multi_has(v_B5, "amenity|aesthetic|liveab|livab|quality of life|recreation|streetscape"),
  obj_n        = obj_storm + obj_heat + obj_biodiv + obj_drought + obj_amenity,
  obj_stated   = as.integer(obj_n > 0),
  obj_inferred = as.integer(is_inferred(v_B5)),

  haz_storm    = multi_has(v_B6, "stormwater|flood"),
  haz_heat     = multi_has(v_B6, "urban heat"),
  haz_air      = multi_has(v_B6, "air quality"),
  haz_drought  = multi_has(v_B6, "drought"),
  haz_erosion  = multi_has(v_B6, "erosion"),
  haz_coastal  = multi_has(v_B6, "coastal"),
  haz_biodiv   = multi_has(v_B6, "biodiversity loss"),
  haz_n        = haz_storm + haz_heat + haz_air + haz_drought + haz_erosion +
                 haz_coastal + haz_biodiv,

  obj_class = case_when(
    obj_n == 0 | is.na(obj_n)         ~ NA_character_,
    obj_n >= 3                        ~ "Multi-objective (3+)",
    obj_storm == 1 & obj_drought == 1 ~ "Stormwater + water conservation",
    obj_storm == 1                    ~ "Stormwater-led",
    obj_drought == 1                  ~ "Water-conservation-led",
    obj_heat == 1                     ~ "Heat-led",
    obj_biodiv == 1                   ~ "Biodiversity-led",
    TRUE                              ~ "Amenity/other"),

  # ===========================================================================
  # B-7 / G-4 | SPATIAL ORIENTATION (spatial responsiveness, not site fit)
  # ===========================================================================
  spatial_target = pick_option(v_B7, c(
    "Equity/vulnerability zones" = "equity|vulnerab|low-income",
    "Watershed/sewershed"        = "watershed|sewershed|catchment",
    "Priority zones"             = "priority zone|priority area|f\u00f6rdergebiet|programmgebiet",
    "Citywide"                   = "citywide|city-wide|service-area",
    "Not stated"                 = "not stated")),
  equity_target = pick_option(v_G4, c(
    "Vulnerable populations"  = "vulnerable population",
    "Low-income/equity zones" = "low-income|equity zone|income-condition|means-test|qpv",
    "Hazard-exposure zones"   = "hazard-exposure|hazard exposure",
    "Citywide"                = "citywide|city-wide|service-area",
    "Not stated"              = "not stated")),
  targeted_prog = as.integer(
    spatial_target %in% c("Priority zones", "Watershed/sewershed", "Equity/vulnerability zones") |
    equity_target %in% c("Hazard-exposure zones", "Low-income/equity zones", "Vulnerable populations")),
  equity_targeted = as.integer(equity_target %in% c("Low-income/equity zones",
                                                    "Vulnerable populations")),

  # ===========================================================================
  # E-1..E-7 | INTERVENTION PORTFOLIO
  # E-1 to E-6 are coded fields; the remainder are recovered from the E-7 free
  # text, which is non-empty for 648 of 707 cases and would otherwise be lost.
  # ===========================================================================
  m_greenroof = yes1(v_E1),
  m_facade    = yes1(v_E2),
  m_raingard  = yes1(v_E3),
  ## "Entsiegelung" is German for unsealing/depaving and belongs here, not with planting
  m_depave    = pmax(yes1(v_E4), kw_pos(v_E7, "entsiegel|unsealing|desealing"), na.rm = TRUE),
  m_trees     = yes1(v_E5),
  ## v58 / R5 as RULED at finding 47. Measure 1 covers capture, storage AND
  ## REUSE: rainwater harvesting is absorbed into it, greywater is NOT.
  ##   ABSORB harvesting -- 23 records, 21 already carry E-6. The two that do not
  ##   (87 Lille, 100 Paris) document cistern-type capture against an E-6 that is
  ##   SILENT, so coding barrels 0 there loses a documented measure, and the
  ##   taxonomy audit already treats harvesting as a duplicate of E-6.
  ##   REJECT greywater -- 15 records, 13 already carry E-6. The two that do not
  ##   (72 Frederiksberg, 571 Minneapolis) both code E-6 as an explicit "No", and
  ##   571 says "rain barrels not eligible" in the same cell. Absorbing it would
  ##   set the measure to 1 on programmes that explicitly deny it -- the exact
  ##   silence-versus-denial error this review exists to remove. Greywater reuse
  ##   also has no vegetated or soil-based mechanism, so it is not nature-based.
  ## Net effect: rain barrels 490 -> 492.
  m_barrel    = pmax(yes1(v_E6),
                     kw_pos(v_E7, "rainwater harvest|rainwater collect"), na.rm = TRUE),
  m_infiltr   = pmax(kw_pos(v_E7, "infiltrat|soakaway|dry ?well|versickerung|\u6d78\u900f|drainage pit"), 0, na.rm = TRUE),
  m_disconn   = pmax(kw_pos(v_E7, "downspout|disconnect|abkopp"), 0, na.rm = TRUE),
  ## v58 / R1 (finding T3a). The previous patterns were written from a handful of
  ## remembered phrasings and missed the ordinary vocabulary of turf-replacement
  ## and low-water-planting programmes -- "turf removal", "grass removal",
  ## "lawn-to-X conversion", "low-water plant", "water-wise", "desertscape".
  ## They are now generated from a CLASSIFICATION of all 707 E-7 strings against
  ## written operational definitions (data/E7_MEASURE_CLASSIFICATION.csv), and
  ## 16_verify.R asserts the patterns still reproduce that classification, so they
  ## cannot silently narrow again.
  ##
  ## OPERATIONAL DEFINITION -- turf replacement: the programme supports REMOVING or
  ## CONVERTING existing turf/lawn/grass to another surface or planting. Incidental
  ## lawn mentions (mowers, clippings, lawn-care education, grass swales, grass
  ## pavers, lawn watering) are stripped before matching, not counted.
  ##
  ## OPERATIONAL DEFINITION -- native/drought planting: the programme supports
  ## installing native, drought-tolerant, low-water, water-wise, pollinator,
  ## meadow/wildflower or climate-adapted vegetation.
  ##
  ## "xeriscape"/"desertscape" count for BOTH: a xeriscape rebate documents the
  ## removal of turf and the installation of drought-tolerant planting, which is
  ## how the programmes themselves describe it. This also preserves the four
  ## records that carry turf replacement and nothing else (442, 609, 655, 676).
  m_planting  = pmax(kw_pos(v_E7, M_PAT$planting), 0, na.rm = TRUE),
  m_turf      = pmax(kw_pos(gsub(M_PAT$turf_neg, " ", tolower(v_E7)), M_PAT$turf), 0, na.rm = TRUE),
  ## Rainwater harvesting overlaps rain barrels (E-6) and is kept separate from greywater
  m_greywater = pmax(kw_pos(v_E7, "greywater|grey water|water reuse"), 0, na.rm = TRUE),
  m_rainharv  = pmax(kw_pos(v_E7, "rainwater harvest|rainwater collect"), 0, na.rm = TRUE),
  m_compost   = pmax(kw_pos(v_E7, "compost|mulch bin|leaf mould|leaf mold|soil improver"), 0, na.rm = TRUE),
  ## PONDS / WETLANDS / BIOSWALES -- promoted into the analysis taxonomy this
  ## round, but ONLY where the mention is citizen-scale. Rule: the pond term
  ## must appear together with citizen-measure context (rain garden, cistern,
  ## permeable, native planting, downspout, resident/homeowner) AND without a
  ## municipal-asset or negation marker (municipal, city-owned, regional,
  ## street/right-of-way, park, "none documented").
  ## THESE COUNTS WERE STALE. The comment claimed "41 raw term
  ## hits -> 34 retained; 7 excluded". Measured on this run under the negation
  ## guard: 43 negation-clean term hits -> 32 retained, 11 excluded as
  ## municipal-context or non-offer mentions. m_pond moved 35 -> 32 between v57
  ## and v58 and that movement was not reported; it is now in
  ## V58_WHAT_MOVED.md and the excluded set is emitted below.
  m_pond      = pmax(kw_pos(v_E7, "pond|wetland|swale") *
                       kw(v_E7, "rain ?garden|cistern|permeable|native plant|downspout|rain barrel|resident|homeowner|property owner|garden") *
                       (1 - kw(v_E7, "municipal|city-owned|regional |public park|street |right-of-way|none documented")),
                     0, na.rm = TRUE),
  ## the raw side uses kw_pos() too, so "excluded" means "named, not in a
  ## negation, but failing the citizen-scale test" -- not "named inside a list of
  ## things the programme does not offer", which is a different thing entirely.
  m_pond_excluded = pmax(kw_pos(v_E7, "pond|wetland|swale") - m_pond, 0, na.rm = TRUE),
  m_soil      = pmax(kw_pos(v_E7, "soil amend|soil improv|topsoil|bodenverbesser"), 0, na.rm = TRUE),

  ## Ordinary addition propagates a single NA
  ## to the whole count: a record with a documented green roof but E-5 coded
  ## "Not applicable" scored n_measures = NA, which made it look like a record
  ## naming NO measure, so it dropped out of the D3a applicability test and out of
  ## the D1 objective match. 24 records were NA and 21 of them had at least one
  ## measure explicitly coded 1; ZERO had all ten fields missing.
  ##
  ## Row sums are now NA-safe, and NA is preserved ONLY where every component is
  ## missing -- which is the one case where the count is genuinely unknown rather
  ## than merely partly documented.
  ## THE CANONICAL SET IS ELEVEN AND IS READ FROM ONE PLACE.
  ## This list was hand-typed and held TEN -- m_turf was missing, while figure 4
  ## and 14_measure_prevalence displayed eleven (finding 26). Four records carry
  ## turf replacement and nothing else (442 Buckeye, 609 Phoenix, 655 Santa
  ## Monica, 676 Surprise) and therefore documented no measure at all. The set
  ## now comes from MEASURES$var in 00b_design.R; 16_verify.R asserts that no
  ## module enumerates measures independently.
  n_measures  = ifelse(rowSums(!is.na(across(all_of(MEASURES$var)))) == 0,
                       NA_integer_,
                       rowSums(across(all_of(MEASURES$var)), na.rm = TRUE)),
  ## n_measures counts the primary taxonomy used for portfolio and demand classes.
  ## n_measures_all additionally counts the secondary categories recovered from E-7.
  ## this inherited NA from n_measures, so a portfolio consisting ENTIRELY of
  ## secondary measures (turf conversion, greywater, rainwater harvesting) counted
  ## as unknown rather than as the measures it documents. The secondary sum is now
  ## computed independently and NA survives only when BOTH sets are empty.
  ## m_turf is promoted into the canonical set above and is no longer
  ## secondary. Compost (43) and soil amendment (9) remain descriptive-only.
  ## m_rainharv is now ABSORBED into m_barrel (finding 47), so counting it here
  ## too would double-count it. It is retained as a named descriptive flag.
  n_measures_sec = rowSums(cbind(m_greywater, m_compost, m_soil),
                           na.rm = TRUE),
  n_measures_all = ifelse(is.na(n_measures) & n_measures_sec == 0, NA_integer_,
                          coalesce(n_measures, 0L) + n_measures_sec),
  ## v19 APPLICABILITY CORRECTED. measures_documented gated D1 on n_measures, the
  ## PRIMARY taxonomy only -- so a programme whose whole portfolio is turf
  ## replacement, xeriscape conversion, greywater reuse or rainwater harvesting
  ## read as supporting NO measure and fell out of the ecological test entirely.
  ## 21 records sat in Indeterminate on that basis, almost all US/Southwest
  ## drought programmes funding lawn-to-desert-landscape conversion. Those are
  ## documented NbS measures with a documented ecological function -- m_turf and
  ## m_planting already carry demand classes and sit in the requirement matrix --
  ## and their absence from the count was a taxonomy gap, not a documentation gap.
  ##
  ## Applicability now uses the FULL measure set. n_measures itself is unchanged,
  ## so the demand classes, portfolio labels and D3a tiers keep their primary
  ## taxonomy and no downstream count shifts.
  measures_documented = as.integer(coalesce(n_measures_all, n_measures) > 0),

  portfolio = case_when(
    is.na(n_measures) | n_measures == 0                     ~ NA_character_,
    (m_greenroof + m_facade) > 0 & n_measures >= 3          ~ "Mixed incl. building-integrated",
    (m_greenroof + m_facade) > 0                            ~ "Building-integrated",
    (m_raingard + m_depave + m_infiltr) > 0 & m_barrel == 1 ~ "Ground + storage",
    (m_raingard + m_depave + m_infiltr) > 0                 ~ "Ground/infiltration",
    m_barrel == 1                                           ~ "Storage only",
    m_trees == 1                                            ~ "Trees/planting only",
    TRUE                                                    ~ "Other/single measure"),

  tech_demand = case_when(
    is.na(n_measures) | n_measures == 0        ~ NA_character_,
    (m_greenroof + m_facade) > 0               ~ "High (building-integrated)",
    (m_raingard + m_depave + m_infiltr + m_pond) > 0 ~ "Medium (ground-modifying)",
    TRUE                                       ~ "Low (standardised)"),
  tech_demand = factor(tech_demand, levels = c("Low (standardised)",
                                               "Medium (ground-modifying)",
                                               "High (building-integrated)")),
  maint_demand = case_when(
    is.na(n_measures) | n_measures == 0            ~ NA_character_,
    (m_greenroof + m_facade + m_raingard) > 0      ~ "High",
    (m_trees + m_depave + m_infiltr) > 0           ~ "Medium",
    TRUE                                           ~ "Low"),
  maint_demand = factor(maint_demand, levels = c("Low", "Medium", "High")),

  # ===========================================================================
  # C-8..C-16 | ACTOR ELIGIBILITY (documented Yes and explicit No kept apart)
  # ===========================================================================
  el_home   = yes1(v_C8),   ex_home   = no1(v_C8),
  el_land   = yes1(v_C9),   ex_land   = no1(v_C9),
  el_coop   = yes1(v_C11),  ex_coop   = no1(v_C11),
  el_school = yes1(v_C12),  ex_school = no1(v_C12),
  el_ngo    = yes1(v_C13),  ex_ngo    = no1(v_C13),
  el_sme    = yes1(v_C14),  ex_sme    = no1(v_C14),
  el_socho  = yes1(v_C15),  ex_socho  = no1(v_C15),
  el_inform = yes1(v_C16),  ex_inform = no1(v_C16),

  ten_raw = strip_qual(v_C10),
  ten_status = case_when(
    str_detect(ten_raw, regex("^not stated|^not applicable|^cannot verify", TRUE)) ~ "not_stated",
    str_detect(ten_raw, regex("^yes.*conditional", TRUE)) |
      (str_detect(ten_raw, regex("^yes", TRUE)) &
       str_detect(ten_raw, regex("consent|approval|permission", TRUE)))            ~ "conditional",
    str_detect(ten_raw, regex("^yes", TRUE))                                       ~ "direct",
    str_detect(ten_raw, regex("^no\\b|^no[-,;.]", TRUE))                           ~ "excluded",
    TRUE                                                                           ~ NA_character_),
  el_tenant_direct = as.integer(ten_status == "direct"),
  el_tenant_cond   = as.integer(ten_status == "conditional"),
  ex_tenant        = as.integer(ten_status == "excluded"),

  d7_tenant_path = yes1(v_D7),
  g2_tenant_path = yes1(v_G2),
  tenant_pathway = pmax(d7_tenant_path, g2_tenant_path, na.rm = TRUE),

  n_actor_types = el_home + el_land + el_coop + el_school + el_ngo + el_sme +
                  el_socho + el_inform + pmax(el_tenant_direct, el_tenant_cond, na.rm = TRUE),
  actor_breadth = case_when(
    is.na(n_actor_types)                         ~ NA_character_,
    el_inform == 1 | el_ngo == 1                 ~ "Civil society / informal included",
    el_school == 1 | el_sme == 1 | el_socho == 1 ~ "Institutional actors included",
    n_actor_types >= 1                           ~ "Individual property actors only",
    TRUE                                         ~ "None documented"),

  # ===========================================================================
  # E-8..E-14 | INSTRUMENT PACKAGE
  # ===========================================================================
  i_grant    = yes1(v_E8),
  i_taxreb   = yes1(v_E9),
  i_loan     = yes1(v_E10),
  i_discount = yes1(v_E11),
  i_reward   = pmax(yes1(v_E12),
                    kw(v_E12, "recognition|award|label|certif|competition|plaque"), na.rm = TRUE),
  i_inkind   = pmax(kw(v_E11, "free of charge|donated|in-kind|no cost|city (supplies|installs)"),
                    kw(v_E8,  "in-kind|city installs|free of charge|no cost to"),
                    kw(v_E25, "in-kind|city installs|free of charge"), na.rm = TRUE),

  ## E-13, kept as specific types rather than only as a collapsed category
  adv_multiple   = kw(v_E13, "multiple"),
  adv_guides     = pmax(kw(v_E13, "guide|manual|brochure|factsheet|handbook"), adv_multiple, na.rm = TRUE),
  adv_helpdesk   = pmax(kw(v_E13, "helpdesk|hotline|phone|contact point|advice line"), adv_multiple, na.rm = TRUE),
  adv_sitevisits = pmax(kw(v_E13, "site visit|on-?site|consultation visit"), adv_multiple, na.rm = TRUE),
  adv_type = pick_option(v_E13, c(
    "Multiple types" = "multiple",
    "Site visits"    = "site visit",
    "Helpdesk"       = "helpdesk|hotline|phone:|contact point",
    ## widened for the harmonised vocabulary (8 cells read "Advice/contact support stated")
    "Guides only"    = "guide|advice/contact support",
    "None"           = "none|not stated")),
  i_advice = as.integer(adv_type %in% c("Guides only", "Helpdesk", "Site visits", "Multiple types")),
  ## E-13 offers "None" as a controlled option, which is a documented absence,
  ## and "Not stated", which is silence. The technical requirement test needs
  ## the difference, so it is captured here rather than inferred later.
  ## SILENCE-CONFLATION FIX (found in cross-check against the external branch).
  ## E-13's absence value is coded as the COMPOUND "None/Not stated" -- all 81
  ## occurrences take that exact form, and NOT ONE is an unambiguous "None". The
  ## coder could not separate "we looked and there is no advisory provision" from
  ## "the documents do not say", so the cell cannot establish a documented absence.
  ## Matching "^none" on it made silence into an explicit No, which is precisely
  ## what coding rule R1 forbids, and it was carrying 8 D3a confirmed misfits.
  ## adv_absent is now set ONLY by a value that is unambiguously an absence; the
  ## compound falls through to NA and the verdict lands on Indeterminate, which is
  ## the honest reading. D-8 ("No") remains a valid explicit absence and is
  ## unaffected -- see adv_absent_el in 05_fitness.R.
  adv_absent = as.integer(str_detect(strip_qual(v_E13), regex("^none", ignore_case = TRUE)) &
                          !str_detect(strip_qual(v_E13), regex("^none\\s*/\\s*not stated", ignore_case = TRUE))),

  ## E-14 free text: support no coded instrument variable captures (598 cases)
  x_freematerials  = pmax(kw(v_E14, "free (material|plant|tree|barrel|seed)|materials provided|kostenlos"),
                          kw(v_E11, "free of charge|donated|no cost"), na.rm = TRUE),
  x_workshops      = kw(v_E14, "workshop|training|course|webinar|demonstration|schulung"),
  x_contractorlist = kw(v_E14, "contractor list|approved installer|vendor list|preferred supplier"),
  x_voucher        = kw(v_E14, "voucher|coupon|gutschein"),
  x_billcredit     = pmax(kw(v_E14, "bill credit|fee credit|utility credit"),
                          kw(v_E9,  "credit|fee reduction"), na.rm = TRUE),

  instrument_pkg = case_when(
    is.na(i_grant) & is.na(i_taxreb) & is.na(i_discount) & is.na(i_inkind) ~ NA_character_,
    (i_inkind == 1 | i_discount == 1) & i_advice == 1                      ~ "Direct provision + advice",
    (i_inkind == 1 | i_discount == 1)                                      ~ "Direct provision",
    (i_grant == 1 | i_taxreb == 1) & i_advice == 1                         ~ "Financial + technical",
    (i_grant == 1 | i_taxreb == 1)                                         ~ "Financial only",
    i_advice == 1                                                          ~ "Technical only",
    TRUE                                                                   ~ "Other/unclear"),

  # ===========================================================================
  # E-19..E-25 | FINANCIAL CALIBRATION
  # Currencies are NOT converted: exchange-rate and purchasing-power conversion
  # would add error the dataset cannot support. Amounts are analysed within
  # currency and banded by within-currency tertiles (computed after transmute).
  # ===========================================================================
  cap_text       = strip_qual(v_E19),
  cap_documented = as.integer(!str_detect(cap_text,
                     regex("^(not stated|not applicable|cannot verify)", ignore_case = TRUE))),
  cap_currency = case_when(
    cap_documented != 1                                       ~ NA_character_,
    str_detect(cap_text, "\u20ac|EUR|euro")                   ~ "EUR",
    ## A bare "$" was pooled into one USD/CAD/AUD band and
    ## then tertiled, so a US and a Canadian cap competed for the same cut points
    ## as if the units were interchangeable. The country field disambiguates the
    ## symbol exactly (212 US, 25 CA, 2 AU, plus SGD/NZD/TWD singletons), so the
    ## currency is resolved from it rather than pooled.
    str_detect(cap_text, "USD")                               ~ "USD",
    str_detect(cap_text, regex("CAD|C\\$", TRUE))              ~ "CAD",
    str_detect(cap_text, regex("AUD|A\\$", TRUE))              ~ "AUD",
    str_detect(cap_text, "\\$") & country == "United States"   ~ "USD",
    str_detect(cap_text, "\\$") & country == "Canada"          ~ "CAD",
    str_detect(cap_text, "\\$") & country == "Australia"       ~ "AUD",
    str_detect(cap_text, "\\$") & country == "New Zealand"     ~ "NZD",
    str_detect(cap_text, "\\$") & country == "Singapore"       ~ "SGD",
    str_detect(cap_text, "\\$")                                ~ "Other dollar",
    str_detect(cap_text, "\u00a5|JPY|\u5186")                 ~ "JPY",
    str_detect(cap_text, regex("PLN|z\u0142", TRUE))          ~ "PLN",
    str_detect(cap_text, "\u00a3|GBP")                        ~ "GBP",
    str_detect(cap_text, regex("R\\$|BRL", TRUE))             ~ "BRL",
    ## Named minor currencies are kept SEPARATE rather than pooled into one band:
    ## they are 1-2 records each, so they fall below the 12-record banding floor
    ## and are correctly left unbanded instead of banded against each other.
    str_detect(cap_text, regex("CHF", TRUE))                  ~ "CHF",
    str_detect(cap_text, regex("DKK", TRUE))                  ~ "DKK",
    str_detect(cap_text, regex("HUF", TRUE))                  ~ "HUF",
    str_detect(cap_text, regex("MYR", TRUE))                  ~ "MYR",
    str_detect(cap_text, regex("NOK", TRUE))                  ~ "NOK",
    TRUE                                                      ~ NA_character_),
  cap_amount = ifelse(cap_documented == 1,
                      first_num(str_extract(str_remove_all(cap_text, "[,\u00a0]"),
                                            "[0-9]{2,9}(\\.[0-9]+)?")), NA_real_),
  cap_pct    = first_num(str_extract(cap_text, "[0-9]{1,3}(?= ?%)")),

  incentive_basis = pick_option(v_E20, c(
    "% of eligible costs"     = "% of eligible|percentage of",
    "Per m2/unit"             = "per m|per unit|/unit",
    "Flat per intervention"   = "flat",
    "Custom/project-specific" = "custom|project-specific",
    "Not stated"              = "not stated")),
  ## "Not applicable" is now captured as its own level. 19 records read
  ## "Not applicable -- no cash disbursement to residents" or "...discounted
  ## purchase rather than a disbursement". Those describe a programme with NO
  ## participant-facing cash transaction, so D3b has nothing to assess: the correct
  ## verdict is Not applicable, and calling them Indeterminate asserted an
  ## evidence gap where the evidence is in fact clear. Applicability precedes
  ## fit, so this is the same rule already governing D3a and D4a.
  ## The 62 records whose E-22 opens with a silence token stay Indeterminate, and
  ## the 11 milestone records with undocumented first-tranche timing stay
  ## Indeterminate under the corrected milestone rule below.
  ## The 19 "Not applicable" records are already mapped to NA upstream by
  ## norm_txt/NA_PAT, which treats structural non-codes as absent -- so a
  ## "Not applicable" LEVEL inside pick_option can never fire, and adding one was
  ## the wrong repair. What was missing is that D3b could not TELL those 19 apart
  ## from the 62 records whose timing is genuinely undocumented: both arrived as NA
  ## and both became Indeterminate. This flag preserves the distinction, so the
  ## verdict can route a documented non-applicability to Not applicable and leave
  ## real silence Indeterminate.
  disb_not_applicable = as.integer(str_detect(norm_txt(v_E22), NA_PAT) %in% TRUE),
  disbursement = pick_option(v_E22, c(
    ## widened (4 cells read "Payment after completion/document review as stated",
    ## "Payment by mailed check after final approval") -- all are reimbursement.
    "Reimbursement"  = "reimburse|payment after (completion|final approval)|paid after (completion|install)|mailed check after",
    "Upfront/vendor" = "upfront|before install|point of sale|voucher|city pays|direct-vendor",
    "Milestone"      = "milestone",
    "Fiscal credit"  = "fiscal credit|bill reduction",
    "Not stated"     = "not stated")),
  ## MILESTONE RULE CORRECTED (external round-2 audit, HIGH -- upheld).
  ##
  ## THE FAULT. Every milestone programme was mapped to prefinancing_required = 0,
  ## i.e. treated as certainly NOT requiring the applicant to front the cost. That
  ## is a blanket claim the coded data does not support: a milestone schedule says
  ## payment is staged, not WHEN the first tranche lands. Staged-after-completion
  ## (applicant fronts) and staged-in-advance (city fronts) are both milestone
  ## designs and they sit on opposite sides of this test.
  ##
  ## WHAT THE CODED DATA ACTUALLY CONTAINS. I read all 8 milestone records rather
  ## than adopt the audit's per-case timings, which come from source documents and
  ## not from the workbook. E-22 holds the bare string "Milestone-based" in all 8.
  ## Searching every coded field of those rows for timing language returns ONE
  ## determinate case: Zilina (395), E-25 "paid in two tranches: 70% ... " -- an
  ## advance tranche, so no applicant pre-financing. For the other 7 the coded
  ## record does not say, and inventing a value from outside the sheet would be
  ## exactly the fabrication this audit exists to catch.
  ##
  ## So milestone now yields NA (indeterminate) rather than a false 0, with the one
  ## documented case resolved on its own evidence. These records leave the D3b test
  ## as Indeterminate, which is the honest reading, and they are listed in the
  ## milestone case table for reviewer follow-up.
  ## the advance was only ever read out of E-25, but 3 of the 15 milestone
  ## records state it in E-22 ITSELF -- Amersfoort ("90% of the subsidy in advance"),
  ## Tourcoing ("in-kind provision during installation", corroborated by E-11
  ## "support is in kind, not a lump-sum payment"), Laguna Niguel ("direct-install
  ## cost sharing"). Reading only E-25 sent all three to Indeterminate on a fact
  ## their own disbursement cell records. Both cells are now read, and an in-kind
  ## or direct-install milestone counts as no pre-financing: there is no cost for
  ## the resident to front.
  milestone_advance_documented = as.integer(
    disbursement == "Milestone" &
      (str_detect(strip_qual(v_E25), regex("tranche|advance|in advance|paid up front",
                                           ignore_case = TRUE)) %in% TRUE |
       str_detect(strip_qual(v_E22), regex("tranche|advance|in advance|paid up front|in-kind|direct.install|city (supplies|installs)",
                                           ignore_case = TRUE)) %in% TRUE)),
  prefinancing_required = case_when(
    disbursement == "Reimbursement"                          ~ 1L,
    disbursement %in% c("Upfront/vendor", "Fiscal credit")   ~ 0L,
    disbursement == "Milestone" & milestone_advance_documented == 1 ~ 0L,
    disbursement == "Milestone"                              ~ NA_integer_,
    TRUE ~ NA_integer_),

  cost_design  = multi_has(v_E21, "design|planning"),
  cost_install = multi_has(v_E21, "installation"),
  cost_permit  = multi_has(v_E21, "permitting"),
  cost_maint   = multi_has(v_E21, "maintenance"),
  cost_monitor = multi_has(v_E21, "monitoring"),
  n_cost_types = multi_n(v_E21),
  coverage_scope = case_when(
    is.na(v_E25) | str_detect(strip_qual(v_E25),
        regex("^(not stated|not applicable|cannot verify)", ignore_case = TRUE)) ~ NA_character_,
    kw(v_E25, "100%|full cost|entire cost|all costs|covers all|free of charge") == 1 &
      kw(v_E25, "exclud|not covered") == 0                             ~ "Full",
    kw(v_E25, "exclud|not covered|only materials|up to|cap|max") == 1   ~ "Partial/capped",
    TRUE                                                               ~ "Scope unclear"),

  funding_source = pick_option(v_E23, c(
    "Mixed"                  = "^mixed",
    "Utility/stormwater fee" = "utility|stormwater fee|wasser",
    "State/national grant"   = "state/national|national grant",
    "Donor/foundation"       = "donor|foundation",
    "Municipal budget"       = "municipal",
    "Not stated"             = "not stated")),
  funding_horizon = pick_option(v_E24, c(
    "Recurring annual" = "recurring",
    "Fixed-term"       = "fixed-term",
    "Pilot"            = "pilot",
    "Not stated"       = "not stated")),

  # ===========================================================================
  # D-1..D-8 | PROCEDURAL / ADMINISTRATIVE PROVISIONS
  # ===========================================================================
  d1_guide   = ternary(v_D1),
  ## Three evidence states are preserved. p_guide scores documented provision;
  ## guide_state records whether a zero is a documented absence or a silence.
  p_guide    = case_when(d1_guide == "yes" ~ 2L, d1_guide == "partial" ~ 1L,
                         d1_guide %in% c("no", "not_stated") ~ 0L, TRUE ~ NA_integer_),
  guide_state = case_when(d1_guide %in% c("yes", "partial") ~ "documented present",
                          d1_guide == "no"                  ~ "documented absent",
                          d1_guide == "not_stated"          ~ "not documented",
                          TRUE ~ NA_character_),
  p_timeline = yes1(v_D2),
  d3_forms   = ternary(v_D3),
  p_forms    = case_when(d3_forms == "yes" ~ 2L, d3_forms == "partial" ~ 1L,
                         d3_forms %in% c("no", "not_stated") ~ 0L, TRUE ~ NA_integer_),
  forms_state = case_when(d3_forms %in% c("yes", "partial") ~ "documented present",
                          d3_forms == "no"                  ~ "documented absent",
                          d3_forms == "not_stated"          ~ "not documented",
                          TRUE ~ NA_character_),

  ## D-4 channels, itemised
  ch_online     = multi_has(v_D4, "online form"),
  ch_email      = multi_has(v_D4, "email"),
  ## v39b: "mail" IS A SUBSTRING OF "Email". The pattern was "paper|mail", so every
  ## record whose only postal-looking token was the word "Email" was flagged as a
  ## PAPER channel: 25 records of the 325, with D-4 values like "Online form; Email"
  ## and "Email" alone. The figure-7 channel stratum therefore showed a paper route
  ## for programmes documenting no such thing. Anchored on a word boundary and with
  ## the e-mail form excluded explicitly, so the two channels cannot alias again.
  ## This is a LABEL-side defect only: fit_D3c reads n_channels (a token count) and
  ## p_guide/p_forms, never these itemised flags -- 0 references in 05_fitness.R.
  ## Pattern tested against the real D-4 vocabulary before adoption: "Email",
  ## "E-mail", "e-mail" -> 0; "Paper or mail", "Paper/mail and in-person",
  ## "by mail", "Mail-in", "Postal" -> 1. A first attempt used two separate
  ## lookbehinds and silently rejected "by mail" as well, which is why the pattern
  ## is asserted in 16_verify.R rather than trusted.
  ch_paper      = multi_has(v_D4, "paper|postal|(?<![e-])\\bmail"),
  ch_thirdparty = multi_has(v_D4, "third-party portal"),
  ch_inperson   = multi_has(v_D4, "in-person"),
  ch_other      = multi_has(v_D4, "other"),
  n_channels    = multi_n(v_D4),

  ## D-5 languages: the field uses commas, not semicolons
  lang_text   = strip_qual(v_D5),
  n_languages = ifelse(str_detect(lang_text, regex("^(not stated|not applicable)", TRUE)), 0L,
                       str_count(lang_text, ",") + 1L),
  lang_en     = kw(v_D5, "\\bEN\\b|english"),
  language_profile = case_when(
    is.na(n_languages) | n_languages == 0 ~ "not stated",
    n_languages == 1 & lang_en == 1       ~ "English only",
    n_languages == 1                      ~ "single national language",
    n_languages == 2                      ~ "bilingual",
    TRUE                                  ~ "three or more languages"),
  p_multiling = as.integer(n_languages >= 2),
  p_codesign  = yes1(v_D6),
  p_housetype = yes1(v_D8),

  # ===========================================================================
  # B-4, C-1..C-7 | DELIVERY ARCHITECTURE AND REGULATORY ENABLERS
  # ===========================================================================
  lead_tag      = norm_txt(str_remove_all(str_extract(v_B4, "\\{[^}]+\\}"), "[{}]")),
  lead_body_txt = norm_txt(v_B4),
  lead_body = case_when(
    !is.na(lead_tag) & str_detect(lead_tag, regex("municipal", TRUE)) &
      str_detect(lead_tag, regex("utility|ngo", TRUE))            ~ "Mixed/partnership",
    !is.na(lead_tag) & str_detect(lead_tag, regex("utility", TRUE)) &
      str_detect(lead_tag, regex("ngo", TRUE))                    ~ "Mixed/partnership",
    !is.na(lead_tag) & str_detect(lead_tag, regex("municipal", TRUE)) ~ "Municipality",
    !is.na(lead_tag) & str_detect(lead_tag, regex("utility", TRUE))   ~ "Utility",
    !is.na(lead_tag) & str_detect(lead_tag, regex("ngo|partnership", TRUE)) ~ "NGO/partnership",
    !is.na(lead_tag)                                              ~ "Other",
    ## fallback where the {tag} is missing: classify from the body name
    str_detect(lead_body_txt, regex("water district|utility|waterschap|sewer|stadtwerke|water authority", TRUE)) ~ "Utility",
    str_detect(lead_body_txt, regex("city of|stadt|ville de|municipal|county|gemeente|department of", TRUE)) ~ "Municipality",
    TRUE                                                          ~ NA_character_),
  g_dept     = yes1(v_C1),
  g_multidep = yes1(v_C2),
  g_coord    = yes1(v_C3),
  g_partners = yes1(v_C4),
  delivery_arch = case_when(
    is.na(g_dept) & is.na(g_multidep) & is.na(g_coord) & is.na(g_partners) ~ NA_character_,
    g_coord == 1                      ~ "Coordinated (named office)",
    g_multidep == 1 & g_partners == 1 ~ "Multi-department + partners",
    g_partners == 1                   ~ "Partnered delivery",
    g_multidep == 1                   ~ "Multi-department",
    TRUE                              ~ "Single body"),
  r_standards = yes1(v_C5),
  r_fasttrack = yes1(v_C6),
  r_feewaiver = yes1(v_C7),

  # ===========================================================================
  # E-15..E-18, F | LIFECYCLE PROVISIONS
  # ===========================================================================
  s_maintgrant = yes1(v_E15),
  s_maintcost  = yes1(v_E16),
  s_condition  = yes1(v_E17),
  s_duration   = first_num(str_extract(strip_qual(v_E17),
                   regex("[0-9]+ ?(year|jahr|yr|\u5e74)", ignore_case = TRUE))),
  s_duration_band = cut(s_duration, c(-Inf, 2, 5, 10, Inf),
                        labels = c("1-2 years", "3-5 years", "6-10 years", ">10 years")),
  ## E-18 codes exactly four non-financial maintenance support types, so each is
  ## flagged individually and no residual "other" category is needed.
  s_guides    = multi_has(v_E18, "guide"),
  s_sitevisit = multi_has(v_E18, "site visit"),
  s_helpdesk  = multi_has(v_E18, "helpdesk"),
  s_reminders = multi_has(v_E18, "inspection reminder"),
  s_helpdesk  = multi_has(v_E18, "helpdesk"),
  s_sitevisit = multi_has(v_E18, "site visit"),
  s_reminders = multi_has(v_E18, "inspection reminder"),
  s_nonfin_n  = multi_n(v_E18),
  st_resp_named = as.integer(!str_detect(strip_qual(v_F5),
                    regex("^(not stated|not applicable|cannot verify)", ignore_case = TRUE))),
  s_respons = pick_option(v_F5, c(
    "Applicant/participant" = "applicant|participant|owner",
    "City/utility"          = "city|utility|municipal",
    "Shared"                = "shared",
    "Contractor"            = "contractor",
    "Not stated"            = "not stated")),

  f_me_published = yes1(v_F1),
  f_me_absent    = no1(v_F1),
  f_report_type  = pick_option(v_F2, c(
    "Open-data portal"      = "open-data|open data",
    ## widened (3 cells read "PDF municipal MS4 annual report", "Webpage map")
    "PDF report"            = "pdf report|pdf [a-z ]*report|annual report",
    "Webpage summary"       = "webpage summary|summary on programme|webpage map",
    "Not publicly reported" = "not publicly reported",
    "Not stated"            = "not stated")),
  f_review = yes1(v_F3),
  f_review_absent = no1(v_F3),

  ## F-4 feedback: the specific channel types, itemised
  fb_phone   = kw(v_F4, "phone|tel[:.]|telefon|\u96fb\u8a71"),
  fb_email   = kw(v_F4, "e-?mail|@"),
  fb_webform = kw(v_F4, "web ?form|online form|portal|contact form|formular"),
  fb_app     = kw(v_F4, "\\bapp\\b|mobile"),
  f_feedback = as.integer(!str_detect(strip_qual(v_F4),
                 regex("^(not stated|none|not applicable|cannot verify)", ignore_case = TRUE)) &
                 str_detect(strip_qual(v_F4),
                 regex("phone|email|web form|app\\b|portal|hotline|@", ignore_case = TRUE))),

  # ===========================================================================
  # G | RECOGNITION PROVISIONS
  # ===========================================================================
  q_informal_ack = yes1(v_G1),
  q_civsoc       = yes1(v_G3),
  q_equity_ind   = yes1(v_G5),
  q_informal_act = yes1(v_G6),

  # ===========================================================================
  # A-7i | TOPOGRAPHY (context detail, never a fit input)
  # ===========================================================================
  topo_coastal  = kw(v_A7i, "coastal|sea\\b|estuar|tidal|bay\\b|harbou?r"),
  topo_river    = kw(v_A7i, "river|creek|stream|canal|delta"),
  topo_flat     = kw(v_A7i, "flat|lowland|plain|low-lying|polder"),
  topo_hilly    = kw(v_A7i, "hill|slope|steep|valley|undulating|escarp"),
  topo_elevated = kw(v_A7i, "plateau|highland|elevation|upland|mountain|alpine"),
  topo_other    = kw(v_A7i, "karst|volcanic|desert|island"),

  year_launched = first_year(v_B3),
  prog_age      = 2026 - year_launched
) %>%
  select(-ten_raw, -lead_tag, -lead_body_txt, -cap_text, -lang_text)

## Within-currency banding of documented contribution caps (computed after the
## transmute so that tertiles are defined on the observed distribution)
## H3 FIX, second half. An amount whose currency could not be identified was
## still banded -- against a mixed pool -- because group_by() treats NA as its own
## group. 33 records were banded this way. An unbanded amount is honest; a band
## computed over unknown units is not, so those are now left NA.
arch <- arch %>%
  group_by(cap_currency) %>%
  mutate(cap_band = if (!is.na(dplyr::first(cap_currency)) && sum(!is.na(cap_amount)) >= 12)
                      as.character(cut(cap_amount,
                        breaks = unique(quantile(cap_amount, c(0, 1/3, 2/3, 1), na.rm = TRUE)),
                        labels = c("lower third", "middle third", "upper third"),
                        include.lowest = TRUE))
                    else NA_character_) %>%
  ungroup()

## Feedback-channel signature, needed by 04_configurations.R
arch <- arch %>%
  mutate(feedback_types = {
    tp  <- cbind(fb_phone, fb_email, fb_webform, fb_app)
    lbl <- c("phone", "email", "web form", "app")
    apply(tp, 1, function(r) {
      h <- lbl[which(r == 1)]
      if (!length(h)) NA_character_ else paste(h, collapse = " + ")
    })
  })

msg("Architecture derived: %d variables (%d documented contribution caps parsed)",
    ncol(arch) - 1, sum(!is.na(arch$cap_amount)))

## ---- Delivery body distribution --------------------------------------------
## SAVED AS A TABLE because the Results quote it. It had no table, so the
## share was computed live from table(lead_body) -- which DROPS NA and therefore
## divided by the 624 classified cases while the sentence read as a share of 709.
## The unclassified cases are a category here, not a silent omission: B-4 gives no
## {tag} and the body name matches no pattern, so the lead body is not determinable
## from the documentation. Any percentage the manuscript quotes must name which
## denominator it uses, and the verifier can only check a number that exists in a
## table -- which is how the drop was caught.
lead_dist <- arch %>%
  count(lead_body = replace_na(as.character(lead_body), "Not classifiable from documentation")) %>%
  mutate(pct_all      = round(100 * n / sum(n), 1),
         pct_classified = ifelse(lead_body == "Not classifiable from documentation", NA_real_,
                                 round(100 * n / sum(n[lead_body != "Not classifiable from documentation"]), 1))) %>%
  arrange(desc(n))
save_tab(lead_dist, "03_lead_body_distribution")
msg("  delivery body: %s", paste(sprintf("%s %d (%.1f%% of all)", lead_dist$lead_body,
                                         lead_dist$n, lead_dist$pct_all), collapse = "; "))

## ---- Prevalence screen (pre-registered) ------------------------------------
bin_vars <- names(arch)[map_lgl(arch, ~ is.integer(.x) && all(.x %in% c(0L, 1L, NA_integer_)))]
prev <- tibble(variable = bin_vars,
               n_valid = map_int(bin_vars, ~ sum(!is.na(arch[[.x]]))),
               n_yes   = map_int(bin_vars, ~ sum(arch[[.x]] == 1, na.rm = TRUE))) %>%
  mutate(prevalence = n_yes / n_valid,
         modellable = n_yes >= CFG$min_events & prevalence >= CFG$min_prevalence &
                      (n_valid - n_yes) >= CFG$min_events) %>%
  arrange(prevalence)
save_tab(prev, "03_prevalence_screen")
msg("  %d of %d binary architecture variables pass the prevalence screen",
    sum(prev$modellable), nrow(prev))
