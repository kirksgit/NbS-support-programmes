# =============================================================================
# 15_glossary.R  |  VARIABLE DICTIONARY
#
# Every term used in a figure, table or model is defined here and assigned to
# its place in the framework, so a reader never meets a name like "renter share"
# or "building-integrated" without knowing what it measures and which part of
# the analysis it belongs to.
#
# LEVEL is the framework tier:
#   Context      - implementation context. Describes the city. Never scored.
#   Architecture - programme design. Descriptive (RQ1) and the input to typology.
#   Fit      - the evaluative judgement (RQ2), one of four domains.
#   Justice      - the cross-cutting lens: distribution, recognition, procedure.
#   Evidence     - documentation and evaluability measures.
#
# DOMAIN is, for fit variables, one of the four domains; for other levels it
# names the block the variable sits in.
# =============================================================================

GLOSSARY <- tribble(
  ~variable, ~label, ~level, ~domain, ~block, ~definition, ~source_field,

  # --------------------------------------------------------------- CONTEXT ---
  "pop", "City population", "Context", "-", "Settlement",
  "Population of the city the programme serves.", "A-3",
  "density", "Population density", "Context", "-", "Urban form",
  "Inhabitants per square kilometre. Values below 50 or above 60,000 are treated as unparseable.", "A-6",
  "density_band", "Density band", "Context", "-", "Urban form",
  "Density grouped as <1,000, 1,000-2,499, 2,500-4,999, 5,000+ inhabitants/km2. 'Dense' in the context modules means 2,500+.", "A-6",
  "climate_grp", "Climate group", "Context", "-", "Climate",
  "Koppen main group collapsed to arid/semi-arid, Mediterranean, temperate humid, continental, tropical.", "A-4",
  "renter_pct", "Renter share", "Context", "-", "Tenure",
  "Percentage of households renting rather than owning. City-level for 517 records; prefecture or parent-city level for 190, which is a known measurement limitation. 'Renter-dominant' means 50% or above; the access test also uses a 40% threshold for applicability.", "Tenure fields",
  "hx_flood", "Documented flood exposure", "Context", "-", "Hazard exposure",
  "Flood, pluvial, stormwater or sewer-surcharge exposure documented for the city independently of what the programme claims.", "A-7ii",
  "hx_heat", "Documented heat exposure", "Context", "-", "Hazard exposure",
  "Urban heat island or extreme heat exposure documented independently of the programme.", "A-7ii",
  "hx_drought", "Documented drought exposure", "Context", "-", "Hazard exposure",
  "Drought or water-stress exposure documented independently of the programme.", "A-7ii",
  "topo_coastal", "Coastal setting", "Context", "-", "Topography",
  "Coastal, estuarine or tidal setting named in the topography field.", "A-7i",

  # ---------------------------------------------------------- ARCHITECTURE ---
  "obj_class", "Environmental objective class", "Architecture", "-", "Environmental orientation",
  "The programme's stated objective, collapsed to stormwater-led, water-conservation-led, heat-led, biodiversity-led, multi-objective or amenity/other.", "B-5",
  "haz_storm", "Stormwater/flood objective", "Architecture", "-", "Environmental orientation",
  "The programme names stormwater or flooding as a problem it addresses. Distinct from documented flood exposure, which is a context variable.", "B-6",
  "m_greenroof", "Green roofs", "Architecture", "-", "Intervention portfolio",
  "The programme supports vegetated roof systems.", "E-1",
  "m_facade", "Green facades", "Architecture", "-", "Intervention portfolio",
  "The programme supports vegetated walls or facade greening.", "E-2",
  "bldg_integrated", "Building-integrated measures", "Architecture", "-", "Intervention portfolio",
  "The programme supports green roofs or green facades: work on the building envelope, which in multi-unit stock an occupant cannot authorise alone. This is the term used in the renter-share interaction.", "E-1, E-2",
  "m_raingard", "Rain gardens", "Architecture", "-", "Intervention portfolio",
  "Bioretention planting designed to receive runoff.", "E-3",
  "m_depave", "Depaving and permeable surfacing", "Architecture", "-", "Intervention portfolio",
  "Removal of sealed surface or replacement with permeable surfacing. Includes German 'Entsiegelung' recovered from the free-text field.", "E-4, E-7",
  "m_trees", "Trees and tree pits", "Architecture", "-", "Intervention portfolio",
  "Street trees, garden trees or tree pits.", "E-5",
  "m_barrel", "Rain barrels and cisterns", "Architecture", "-", "Intervention portfolio",
  "Rainwater storage vessels connected to roof drainage.", "E-6",
  "m_infiltr", "Infiltration systems", "Architecture", "-", "Intervention portfolio",
  "Soakaways, dry wells, infiltration trenches and equivalent subsurface systems.", "E-7",
  "portfolio", "Intervention portfolio class", "Architecture", "-", "Intervention portfolio",
  "Mutually exclusive class: building-integrated, mixed including building-integrated, ground/infiltration, ground plus storage, storage only, trees/planting only.", "E-1 to E-7",
  "tech_demand", "Technical demand class", "Architecture", "-", "Intervention portfolio",
  "How structurally demanding the supported measures are: high for building-integrated, medium for ground-modifying, low for standardised distributions such as barrels or tree giveaways. Governs whether the technical test applies.", "E-1 to E-7",
  "maint_demand", "Maintenance demand class", "Architecture", "-", "Intervention portfolio",
  "How much continuing care the supported measures require: high for roofs, facades and rain gardens; medium for trees, depaving and infiltration; low otherwise.", "E-1 to E-7",
  "el_home", "Homeowner eligibility", "Architecture", "-", "Eligible actors",
  "Owner-occupiers may apply.", "C-8",
  "el_land", "Landlord eligibility", "Architecture", "-", "Eligible actors",
  "Landlords or property owners letting to others may apply.", "C-9",
  "el_tenant_direct", "Direct tenant eligibility", "Architecture", "-", "Eligible actors",
  "Tenants may apply in their own right, without documented owner consent.", "C-10",
  "el_tenant_cond", "Consent-dependent tenant eligibility", "Architecture", "-", "Eligible actors",
  "Tenants may apply only with owner or landlord consent. Kept separate from direct eligibility throughout.", "C-10",
  "el_coop", "Co-operative and HOA eligibility", "Architecture", "-", "Eligible actors",
  "Housing co-operatives or homeowner associations may apply.", "C-11",
  "el_socho", "Social housing eligibility", "Architecture", "-", "Eligible actors",
  "Social or public housing providers may apply.", "C-15",
  "controller_incl", "Property controllers included", "Architecture", "-", "Eligible actors",
  "At least one of landlords, co-operatives/HOAs or social housing providers is eligible: an actor with authority over multi-unit or shared property.", "C-9, C-11, C-15",
  "i_grant", "Capital grant", "Architecture", "-", "Instruments",
  "A cash grant towards installation cost.", "E-8",
  "i_taxreb", "Tax or fee rebate", "Architecture", "-", "Instruments",
  "A reduction in a tax, stormwater fee or utility charge.", "E-9",
  "i_inkind", "In-kind provision", "Architecture", "-", "Instruments",
  "Materials, plants or installation supplied directly at no or reduced cost.", "E-8, E-11, E-25",
  "i_advice", "Technical advice", "Architecture", "-", "Instruments",
  "Guides, helpdesk, site visits or several of these.", "E-13",
  "instrument_pkg", "Instrument package", "Architecture", "-", "Instruments",
  "The bundle class: financial only, financial plus technical, direct provision, direct provision plus advice, technical only.", "E-8 to E-13",
  "cap_amount", "Maximum public contribution", "Architecture", "-", "Financial calibration",
  "The documented cap on public contribution, in the currency stated. Currencies are never converted; comparison is within currency only.", "E-19",
  "disbursement", "Payment configuration", "Architecture", "-", "Financial calibration",
  "When money reaches the applicant: reimbursement after installation, upfront or direct-to-vendor, milestone, or fiscal credit.", "E-22",
  "prefinancing_required", "Pre-financing requirement", "Architecture", "-", "Financial calibration",
  "The applicant must fund the work first and claim afterwards. Derived from reimbursement-only payment.", "E-22",
  "coverage_scope", "Cost coverage scope", "Architecture", "-", "Financial calibration",
  "Whether the stated coverage is full or partial/capped. 680 of 707 records are partial or capped, so this is reported as a corpus-level finding, never modelled.", "E-25",
  "n_channels", "Application channels", "Architecture", "-", "Applicant-facing procedure",
  "Count of documented ways to apply: online form, email, paper, third-party portal, in person.", "D-4",
  "n_languages", "Application languages", "Architecture", "-", "Applicant-facing procedure",
  "Number of languages in which programme materials are documented.", "D-5",
  "delivery_arch", "Delivery architecture", "Architecture", "-", "Delivery and funding",
  "Single body, partnered delivery, multi-department, multi-department with partners, or a named coordination office.", "C-1 to C-4",
  "lead_body", "Lead body", "Architecture", "-", "Delivery and funding",
  "Municipality, utility, NGO/partnership or mixed.", "B-4",
  "funding_horizon", "Funding horizon", "Architecture", "-", "Delivery and funding",
  "Recurring annual, fixed-term, pilot, or not stated.", "E-24",

  # --------------------------------------------------------------- FIT ---
  "eco_fit", "Ecological-functional fit", "Fit", "Domain 1. Ecological-functional",
  "Objective to function to intervention",
  "Whether every stated objective is matched by at least one intervention that can plausibly deliver the required ecological function. A validity floor rather than a finding, because objectives and measures are often documented in the same passage.", "B-5, B-6, E-1 to E-7",
  "spatial_response", "Spatial targeting", "Fit", "Domain 1. Ecological-functional",
  "Spatial responsiveness",
  "Whether the programme is citywide, area-targeted, hazard or catchment-targeted, or vulnerability-targeted. Reported alongside Domain 1 but never scored as fit.", "B-7, G-4",
  "access_route", "Access route", "Fit", "Domain 2. Tenure & authority access",
  "Authority and participation pathway",
  "Which route into the programme exists: direct tenant or broad, controller/collective, permission-dependent, owner-restricted, or a confirmed misfit where controller routes are explicitly closed.", "C-8 to C-16, D-7, G-2",
  "access_fit", "Tenure & authority access fit", "Fit", "Domain 2. Tenure & authority access",
  "Authority and participation pathway",
  "Whether eligibility corresponds to authority over the space where the intervention must occur. Applies where the programme funds work on buildings, or where the renter share is 40% or above.", "C-8 to C-16, D-7, tenure",
  "n_enablers", "Technical enabler count", "Fit", "Domain 3. Implementation-support",
  "Technical enablement",
  "How many of six enabling provisions are documented: technical standards, permit fast-track, permit fee waiver, housing-type guidance, technical advice, design costs eligible. Reported with the specific combination, never as a bare count.", "C-5 to C-7, D-8, E-13, E-21",
  "tech_fit", "Technical enablement fit", "Fit", "Domain 3. Implementation-support",
  "Technical enablement",
  "Whether technically demanding measures receive corresponding support. Applies only to medium and high technical demand classes.", "C-5 to C-7, D-8, E-13, E-21",
  "fit_D3b", "Financial accessibility fit (universal cost-barrier test)", "Fit", "Domain 3. Implementation-support",
  "Financial accessibility",
  "Restricted test: among programmes explicitly targeting financially constrained actors, whether payment timing avoids requiring the applicant to fund work first. Scope is 39 records, so results are descriptive only.", "E-22, G-4, C-15",
  "admin_profile", "Administrative provision", "Fit", "Domain 3. Implementation-support",
  "Administrative accessibility",
  "Documented burden-reducing provisions, graded from none through partial and basic to broad. Never described as observed administrative burden, which programme documents cannot show.", "D-1 to D-8, C-6, C-7",
  "steward_config", "Post-installation support configuration", "Fit", "Domain 4. Post-installation",
  "Post-installation responsibility and support",
  "Which combination of responsibility, binding condition and support is documented after installation. Responsibility without a maintenance grant is not automatically a misfit.", "E-15 to E-18, F-5",
  "monitor_config", "Monitoring configuration", "Fit", "Domain 4. Lifecycle",
  "Monitoring and adaptive management",
  "Which combination of published indicators, webpage reporting, periodic review and feedback channel is documented.", "F-1 to F-4",

  # --------------------------------------------------------------- JUSTICE ---
  "equity_targeted", "Equity or vulnerability targeting", "Justice", "Cross-cutting",
  "Distribution",
  "The programme prioritises low-income areas, equity zones or vulnerable populations.", "G-4",
  "q_equity_ind", "Equity-indicator tracking", "Justice", "Cross-cutting", "Distribution",
  ## THIS NUMBER WAS TYPED AND IT WAS WRONG. The glossary said
  ## "1 of 707"; G-5 is documented for 3. Figure 9's caption already computed 3,
  ## so the bundle contradicted itself. Computed from the run, never typed.
  sprintf("The programme tracks equity indicators. Documented for %d of %d records, reported as a corpus-level absence.",
          sum(M$q_equity_ind == 1, na.rm = TRUE), nrow(M)), "G-5",
  "el_inform", "Informal group eligibility", "Justice", "Cross-cutting", "Recognition",
  "Informal or unconstituted community groups may apply.", "C-16",
  "p_codesign", "Co-design", "Justice", "Cross-cutting", "Procedure",
  "Residents or civil society participated in designing the programme.", "D-6",
  "f_feedback", "Feedback channel", "Justice", "Cross-cutting", "Procedure",
  "A named channel through which applicants can contact the programme. A generic contact page does not qualify.", "F-4",

  # -------------------------------------------------------------- EVIDENCE ---
  "n_url_unique", "Documentation depth", "Evidence", "-", "Documentation",
  "Count of distinct source URLs behind a record's evidence strings.", "Evidence columns",
  "inferred_share", "Inferred-code share", "Evidence", "-", "Documentation",
  "Share of a record's 76 coded cells carrying an [inferred] qualifier rather than a direct statement. Corpus mean 8.5%.", "All coded cells",
  "ev_access", "Evaluability, Domain 2", "Evidence", "-", "Evaluability",
  "Whether the record carries enough documentation for spatial-institutional fit to be judged at all.", "Derived",
  "n_weak", "Documented weakness count", "Evidence", "-", "Compound analysis",
  "How many of five documented weaknesses a record carries: owner-restricted access, one or no technical enabler, thin administrative provision, weak stewardship closure, absent monitoring.", "Derived",
  "prog_family", "Programme family", "Evidence", "-", "Unit of analysis",
  "Records sharing a primary source URL, or a lead body and programme name within a country, are one programme coded once per municipality served. 707 records cover 663 families.", "B-2, B-4, primary URL"
)

save_tab(GLOSSARY, "15_variable_glossary")

## Compact label lookup used by the figure captions:
## lbl("renter_pct") returns "Renter share (Context: tenure)"
lbl <- function(v) {
  i <- match(v, GLOSSARY$variable)
  if (is.na(i)) return(v)
  lv <- GLOSSARY$level[i]
  bk <- if (lv == "Fit") GLOSSARY$domain[i] else GLOSSARY$block[i]
  sprintf("%s (%s: %s)", GLOSSARY$label[i], lv, bk)
}

## Interaction terms named in the models, spelled out in full
INTERACTIONS <- tribble(
  ~short, ~spelled_out, ~tests,
  "renter share x building-integrated",
  "Renter share (Context: tenure) x building-integrated measures (Architecture: intervention portfolio) -> access route (Fit: Domain 2)",
  "Where a programme funds work on the building envelope in a city where most households rent, does it open a controller or tenant route?",
  "technical demand x delivery architecture",
  "Technical demand class (Architecture: intervention portfolio) x delivery architecture (Architecture: delivery and funding) -> technical enabler count (Fit: Domain 3)",
  "Do structurally demanding portfolios receive correspondingly organised implementation support?",
  "priority targeting x intervention class",
  "Equity or vulnerability targeting (Justice: distribution) x technical demand class (Architecture: intervention portfolio) -> pre-financing requirement (Architecture: financial calibration)",
  "Do programmes that target financially constrained applicants avoid asking them to fund the work first?",
  "maintenance demand x funding horizon",
  "Maintenance demand class (Architecture: intervention portfolio) x funding horizon (Architecture: delivery and funding) -> maintenance support (Fit: Domain 4)",
  "Are long-term care requirements matched by programme continuity?",
  "hazard exposure x density",
  "Documented hazard exposure (Context: hazard exposure) x population density (Context: urban form) -> spatial targeting (Fit: Domain 1)",
  "Do programmes target the areas where the hazard is documented, especially where density concentrates exposure?"
)
save_tab(INTERACTIONS, "15_interaction_terms_spelled_out")

msg("Glossary: %d variables defined across %d levels; %d interaction terms spelled out",
    nrow(GLOSSARY), n_distinct(GLOSSARY$level), nrow(INTERACTIONS))
