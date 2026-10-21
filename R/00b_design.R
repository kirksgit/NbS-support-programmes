# =============================================================================
# 00b_design.R  |  SHARED DESIGN SYSTEM
#
# One palette, one theme, and the two structural typologies (city context and
# measure grouping) that every figure draws from, so nothing is defined twice
# and the colour of a domain or an outcome is identical everywhere it appears.
#
# Loaded once, early. Figures never redefine a colour.
# =============================================================================

suppressPackageStartupMessages({ library(ggplot2); library(scales) })

# -----------------------------------------------------------------------------
# PALETTE -- ONE source palette across every figure (Kirk's instruction):
# ColorBrewer RdYlBu (11-class), colour-blind safe. All hues in every figure
# are drawn from these steps; achromatic greys are reserved for evidence
# states that carry no judgement (not documented / indeterminate / N-A).
# Steps: #a50026 #d73027 #f46d43 #fdae61 #fee090 #ffffbf(unused)
#        #e0f3f8 #abd9e9 #74add1 #4575b4 #313695
# Semantics: warm arm = misfit/restriction/hazard-warm; cool arm = fit/
# provision/water; #fee090 = documented absence (pale, not alarming).
# -----------------------------------------------------------------------------
PAL <- list(
  fit = "#4575b4", partial = "#fdae61", misfit = "#d73027",       # judgement
  noprov = "#fee090",                                             # doc. absence
  indet = "#777777", notdoc = "#D5D5D5", na = "#F2F2F2",          # greys
  deep = "#313695", blue = "#4575b4", softblue = "#74add1",
  ice = "#abd9e9", paleblue = "#e0f3f8",
  darkred = "#a50026", orange = "#f46d43",
  ## legacy key names kept so downstream code needs no rename:
  purple = "#313695", magenta = "#a50026", green2 = "#74add1",
  brown = "#f46d43", teal = "#abd9e9", grey = "#666666",
  ink = "#22262A")

PAL_FIT <- c("Documented fit" = PAL$fit, "Partial fit" = PAL$partial,
             "Confirmed misfit" = PAL$misfit, "No provision documented" = PAL$noprov,
             "Indeterminate" = PAL$indet, "Not applicable" = PAL$na)

PAL_DOMAIN <- c("D1 Ecological-functional" = "#74add1",
                "D2 Tenure & authority access" = "#313695",
                "D3 Implementation-support" = "#fdae61",
                "D4 Maintenance support" = "#f46d43",
                "Justice (cross-cutting)" = "#a50026")

PAL_LEAD <- c("Municipality" = "#313695", "Utility" = "#74add1",
              "NGO/partnership" = "#f46d43", "Mixed/partnership" = "#a50026",
              "Other" = PAL$grey)

PAL_INSTR <- c("Financial" = "#a50026", "Fiscal" = "#d73027",
               "Material/in-kind" = "#f46d43", "Technical" = "#fdae61",
               "Regulatory" = "#74add1", "Behavioural" = "#4575b4")

## Core NbS families cool (vegetated), hybrid adjuncts warm -- an internal
## logic the single palette makes possible
PAL_ECO <- c("Vegetated building systems" = "#313695",
             "Vegetated ground and canopy" = "#4575b4",
             "Permeability and soil-water" = "#74add1",
             "Hybrid capture and conveyance" = "#fdae61")

TIER_BADGE <- c("High demand (building-integrated)" = "H",
                "Medium demand (ground-modifying)" = "M",
                "Low demand (standardised)" = "L")
PAL_TIER <- c("High demand (building-integrated)" = "#313695",
              "Medium demand (ground-modifying)" = "#74add1",
              "Low demand (standardised)" = "#abd9e9")

PAL_HAZFAM <- c("Floods/stormwater (incl. coastal)" = "#4575b4",
                "Heat" = "#d73027",
                "Drought / water scarcity" = "#fdae61",
                "Multiple families" = "#a50026",
                "Other documented (erosion/slope)" = "#f46d43",
                "None documented" = PAL$notdoc)

PAL_TYPE <- c("Type 1" = "#313695", "Type 2" = "#74add1", "Type 3" = "#f46d43",
              "Opacity group" = "#969696")

## CANONICAL REGION ORDER -- used by every figure that splits by region, so the
## reading order never changes between panels. Western Europe precedes East Asia.
REGION_ORDER <- c("North America", "Western Europe", "Central/Eastern Europe",
                  "East Asia", "Other regions")
PAL_REGION <- c("North America" = "#a50026", "Western Europe" = "#f46d43",
                "Central/Eastern Europe" = "#74add1", "East Asia" = "#4575b4",
                "Other regions" = "#fdae61")

## CANONICAL HAZARD VOCABULARY -- one wording everywhere: Floods, Heat, Drought,
## Biodiversity loss. No "flood/stormwater", no "drought/water scarcity".
HAZ_LABELS <- c("Floods", "Heat", "Drought", "Biodiversity loss")
HAZ_FILL   <- c("Floods"            = "#4575b4",
                "Heat"              = "#d73027",
                "Drought"           = "#fdae61",
                "Biodiversity loss" = "#74add1",
                "None documented"   = "#d9d9d9")

PAL_POS <- PAL_ECO  # positions retired as a colour system; house uses PAL_ECO

## Ramps, all from the same palette
scale_fill_seq <- function(name, ...)
  scale_fill_gradient(low = "#e0f3f8", high = "#313695", name = name, ...)
scale_fill_orange <- function(name, ...)
  scale_fill_gradient(low = "#fee090", high = "#a50026", name = name, ...)
scale_fill_div <- function(name, mid = 0, ...)
  scale_fill_gradient2(low = "#d73027", mid = "white", high = "#4575b4",
                       midpoint = mid, name = name, ...)

# -----------------------------------------------------------------------------
# THEME
# -----------------------------------------------------------------------------
## TYPE SCALE. Two rules, applied everywhere:
##  (1) a FLOOR: nothing below 6pt renders legibly at journal column width, so
##      every derived size is clamped there rather than allowed to shrink with
##      the base;
##  (2) HIERARCHY BY WEIGHT, not by size alone -- the panel title is the only
##      bold element at its size, subtitles are grey and regular, so the eye
##      finds the title without the subtitle having to be tiny.
## Subtitles wrap rather than clip: long ones previously ran past the panel and
## overlaid the next element.
.fl <- function(x, floor = 6) pmax(x, floor)
theme_nbs <- function(base = 9) {
  theme_minimal(base_size = base, base_family = "") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "grey93", linewidth = .25),
          plot.title = element_text(face = "bold", size = .fl(base + 1, 8),
                                    colour = PAL$ink, margin = margin(b = 3)),
          plot.subtitle = element_text(colour = "grey38", size = .fl(base - 1, 6.6),
                                       margin = margin(b = 6), lineheight = 1.12),
          plot.caption = element_text(colour = "grey45", size = .fl(base - 2.5),
                                      hjust = 0, lineheight = 1.1,
                                      margin = margin(t = 5)),
          strip.text = element_text(face = "bold", size = .fl(base - .5, 6.6),
                                    colour = PAL$ink),
          axis.title = element_text(size = .fl(base - 1, 6.4), colour = "grey30"),
          axis.text = element_text(size = .fl(base - 1.5, 6.2), colour = "grey30"),
          legend.position = "bottom",
          legend.title = element_text(size = .fl(base - 1.5, 6.4), face = "bold"),
          legend.text = element_text(size = .fl(base - 2, 6.2)),
          legend.key.size = unit(3.5, "mm"),
          legend.margin = margin(t = 2, b = 0),
          plot.title.position = "plot",
          plot.background = element_rect(fill = "white", colour = NA),
          plot.margin = margin(7, 10, 7, 7),
          panel.spacing = unit(3.5, "mm"))
}
theme_set(theme_nbs())

## FIGURE-LEVEL TITLE. Wrapping is the fix for the long subtitles that used to
## run off the right edge of the canvas: patchwork does not wrap plot_annotation
## text, so it is wrapped explicitly at the call site via WRAP() below. Sizes
## sit one step above the panel scale so the figure title outranks every panel
## title, and the title block is positioned against the whole plot, not the
## first panel, so it starts at the left margin.
TITLE   <- function(...) theme(plot.title = element_text(face = "bold", size = 11.5,
                                                         colour = PAL$ink,
                                                         margin = margin(b = 4)),
                               plot.subtitle = element_text(size = 8, colour = "grey38",
                                                            lineheight = 1.15,
                                                            margin = margin(b = 6)),
                               plot.title.position = "plot",
                               plot.margin = margin(9, 11, 8, 9))
## Wrap a figure subtitle to a sane measure. Called on every long subtitle so no
## text is clipped by the canvas edge.
WRAP <- function(x, width = 200) paste(strwrap(x, width = width), collapse = "\n")
## RENDERING IS OPTIONAL; BUILDING THE FIGURE IS NOT. The figure modules are not
## only drawing code: they compute the flow frames, reconcile every panel against
## the verdict layer, and run the tautology guard, and sixteen checks in
## 16_verify.R read those objects. Deleting them would remove sixteen correctness
## checks to save some image files, so the plot is always built and only the
## writing to disk is switchable. The figures in the paper are produced
## separately, so NBS_FIGURES=off is the normal setting for a reader who wants
## the tables and the audit; set it to "on" to get the PNG and PDF files too.
.FIG_WRITE <- !identical(tolower(Sys.getenv("NBS_FIGURES", "off")), "off")
save_fig <- function(p, name, w = 14, h = 9, dpi = 300) {
  ## force() the plot even when nothing is written, so a figure that would have
  ## failed to build still fails here rather than passing silently unrendered.
  force(p)
  if (.FIG_WRITE) {
    ## created here rather than in 00_setup.R, so a default run leaves no empty
    ## output/figures folder behind
    if (!dir.exists(CFG$out_fig)) dir.create(CFG$out_fig, recursive = TRUE, showWarnings = FALSE)
    ggsave(file.path(CFG$out_fig, paste0(name, ".png")), p, width = w, height = h, dpi = dpi, bg = "white")
    ggsave(file.path(CFG$out_fig, paste0(name, ".pdf")), p, width = w, height = h, bg = "white")
  }
  ## C4: manifest, same reason as save_tab -- a renamed figure used to leave its
  ## previous file on disk, which is how a stale figure reached a delivered archive.
  RUN_MANIFEST$figures <- union(RUN_MANIFEST$figures, paste0(name, ".png"))
  invisible(msg("  %s %s", if (.FIG_WRITE) "saved" else "built", name))
}

# =============================================================================
# CITY CONTEXT TYPOLOGY  (carried throughout, introduced in the descriptive part)
#
# The context variables are only weakly redundant (pairwise |r| <= 0.28) and each
# discriminates a DIFFERENT fit outcome: tenure drives spatial-institutional
# access; density and city size drive technical demand; hazard exposure drives
# targeting. Collapsing them into one composite type destroys that signal.
#
# The paper therefore uses the two dimensions that the urban-form literature
# already identifies as governing what can be installed and who can authorise
# it, as a 3x3 frame every city can locate itself in:
#
#   BUILT FORM (how interventions physically fit): compact/vertical,
#              intermediate, dispersed
#   TENURE     (who can authorise them): owner-dominated, mixed, renter-majority
#
# Hazard exposure is the PROBLEM addressed, so it is carried as an overlay on
# this frame rather than as a third defining axis. Population, climate and
# land-use remain available and are shown in the descriptive context figure.
# =============================================================================
add_city_typology <- function(df) {
  df %>% mutate(
    ## Built form comes from CONTEXT ONLY (density). An earlier draft also used
    ## the programme's supported measures (green roofs/facades) as a signal of
    ## vertical form; that contaminates context with architecture and would make
    ## the frame partly "discover" what was built into it. Thresholds follow the
    ## common urban-form splits: dispersed < 1,500 inh/km2, compact >= 4,000.
    built_form = case_when(
      is.na(density)     ~ "Unknown",
      density >= 4000    ~ "Compact / vertical",
      density >= 1500    ~ "Intermediate",
      TRUE               ~ "Dispersed"),
    built_form = factor(built_form,
      levels = c("Dispersed", "Intermediate", "Compact / vertical", "Unknown")),
    tenure_class = case_when(
      is.na(renter_pct)      ~ "Unknown",
      renter_pct >= 55       ~ "Renter-majority",
      renter_pct >= 35       ~ "Mixed tenure",
      TRUE                   ~ "Owner-dominated"),
    tenure_class = factor(tenure_class,
      levels = c("Owner-dominated", "Mixed tenure", "Renter-majority", "Unknown")),
    ## Hazard families. Water-related combines flood/stormwater with coastal,
    ## per the harmonised hazard taxonomy. A city documented for MORE than one
    ## family is shown as such rather than being assigned by priority order,
    ## which would systematically hide heat behind flood.
    haz_water = as.integer(hx_flood == 1 | hx_coastal == 1),
    hazard_family = case_when(
      (haz_water + (hx_heat == 1) + (hx_drought == 1)) >= 2 ~ "Multiple families",
      haz_water == 1                                        ~ "Floods/stormwater (incl. coastal)",
      hx_heat == 1                                          ~ "Heat",
      hx_drought == 1                                       ~ "Drought / water scarcity",
      hx_erosion == 1                                       ~ "Other documented (erosion/slope)",
      TRUE                                                  ~ "None documented"),
    hazard_family = factor(hazard_family, levels = names(PAL_HAZFAM)),
    city_type = ifelse(built_form == "Unknown" | tenure_class == "Unknown",
                       "Unclassified",
                       paste0(built_form, " / ", tenure_class)))
}

# =============================================================================
# MEASURE GROUPING  (technical-demand tiers, specifics named under each)
# Definitions are attached so no measure appears unexplained.
# =============================================================================
MEASURES <- tibble::tribble(
  ~var, ~measure, ~eco_group, ~tier, ~position, ~definition,
  "m_greenroof", "Green roofs", "Vegetated building systems", "High demand (building-integrated)", "Roof",
  "Vegetated roof systems: waterproofing, drainage layer, substrate and planting on a roof.",
  "m_facade", "Green facades", "Vegetated building systems", "High demand (building-integrated)", "Facade",
  "Vegetated walls or facade-mounted planting systems on a building's vertical envelope.",
  ## One collective phrase, with the contents stated in the definition/annotation.
  ## "Garden ponds and swales" is specific about BOTH scale (the parcel, not the
  ## catchment) and type (standing or conveying water held in vegetation).
  "m_pond", "Garden ponds and swales", "Vegetated ground and canopy",
  "Medium demand (ground-modifying)", "Ground / surface",
  "Garden ponds, constructed wetland cells and vegetated bioswales on the parcel: standing water and vegetated conveyance that retain, convey and treat runoff.",
  "m_raingard", "Rain gardens", "Vegetated ground and canopy", "Medium demand (ground-modifying)", "Ground / surface",
  "Shallow planted depressions (bioretention) that receive and soak up runoff.",
  "m_depave", "Depaving / permeable", "Permeability and soil-water", "Medium demand (ground-modifying)", "Ground / surface",
  "Removing sealed surface or replacing it with permeable paving so water infiltrates.",
  ## Kept as the generic class because the corpus genuinely spans several buried
  ## structure types; the definition (carried as the figure annotation) states
  ## exactly what it contains. Corpus wording: infiltration pits (51 records),
  ## infiltration / rainwater facilities, trenches, soakaway pits, crates,
  ## chambers, pipes and wells.
  "m_infiltr", "Infiltration systems", "Permeability and soil-water", "Medium demand (ground-modifying)", "Subsurface / infiltration",
  "Buried infiltration structures: soakaway pits, dry wells, infiltration trenches, chambers, crates and pipes that let roof or surface water seep into the soil.",
  ## Downspout disconnection is NOT nature-based on its own -- it is a plumbing
  ## change. It qualifies here only where the programme directs the flow to a
  ## vegetated or infiltrating receiving surface (garden, bed, soakaway, rain
  ## garden); that conditional status is audited in 12_classification_audit.R and
  ## it is reported as a hybrid adjunct, never as a standalone NbS measure.
  "m_disconn", "Downspout disconnection (to a vegetated or infiltrating receiver)", "Hybrid capture and conveyance", "Low demand (standardised)", "Capture / storage",
  "Redirecting a roof downpipe out of the sewer onto a garden, planted bed or soakaway; nature-based only through the receiving surface.",
  "m_barrel", "Rain barrels / cisterns", "Hybrid capture and conveyance", "Low demand (standardised)", "Capture / storage",
  "Vessels that store roof runoff for later use; a standardised, self-install product.",
  ## TIER REASSESSED (Low -> Medium). Two reasons. (1) The taxonomy tier and the
## requirement matrix in 03b disagreed: 03b already classed trees "Medium" and it
## is 03b that drives the D3a technical test, so the figure axis was contradicting
## the test. (2) Substantively, Medium is the defensible class: species selection,
## siting relative to overhead and buried utilities, and pit dimensions/soil volume
## are specification questions, and establishment mortality is the dominant failure
## mode -- annual mortality peaks in the first five years after planting and
## stewardship/maintenance is among the most frequently cited human factors in the
## mortality literature. A tree therefore demands more programme support than a
## standardised self-install product, though less hydraulic design than a rain garden.
  "m_trees", "Trees (pits)", "Vegetated ground and canopy", "Medium demand (ground-modifying)", "Tree / canopy",
  "Street or garden trees and the prepared pits they are planted in.",
  "m_planting", "Native / drought planting", "Vegetated ground and canopy", "Low demand (standardised)", "Ground / surface",
  "Replacing lawn or bare ground with native or drought-tolerant plants.",
  "m_turf", "Turf replacement", "Vegetated ground and canopy", "Low demand (standardised)", "Ground / surface",
  "Converting mown lawn to planted, permeable or water-wise ground cover.")

# =============================================================================
# INSTRUMENT GROUPING  (families, specifics under each; replaces the "+" bundles)
# =============================================================================
## Labels carry two or three corpus examples in brackets where the instrument
## name alone is abstract, so a reader knows what the category contains without
## consulting the codebook. Examples are drawn from the coded free text.
INSTRUMENTS <- tibble::tribble(
  ~var, ~instrument, ~family,
  "i_grant",    "Capital grant",        "Financial",
  "i_loan",     "Loan",                 "Financial",
  "i_taxreb",   "Tax or fee rebate",    "Fiscal",
  "x_billcredit","Bill or fee credit",  "Fiscal",
  "i_inkind",   "In-kind provision (subsidised barrel sale, delivery)", "Material/in-kind",
  "x_freematerials","Free materials (barrels, trees, compost)",  "Material/in-kind",
  "i_advice",   "Technical advice",     "Technical",
  "r_standards","Technical standards",  "Technical",
  "x_workshops","Workshops / training", "Technical",
  "r_fasttrack","Permit fast-track",    "Regulatory",
  "r_feewaiver","Permit fee waiver",    "Regulatory",
  "i_reward",   "Recognition (awards, competitions, publicity)", "Behavioural")

## reorder-within-facet helpers (shared; avoids a tidytext dependency)
reorder_within <- function(x, by, within, fun = mean, sep = "___") {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}
scale_y_reordered <- function(...) {
  ggplot2::scale_y_discrete(labels = function(z) gsub("___.+$", "", z), ...)
}


## =============================================================================
## MEASURE KEYWORD PATTERNS, DEFINED ONCE.
## Generated from a classification of all 707 E-7 strings against the written
## operational definitions above (see data/E7_MEASURE_CLASSIFICATION.csv).
## 16_verify.R asserts that these patterns still reproduce that classification.
## No module may write its own measure regex.
## =============================================================================
M_PAT <- list(
  turf = paste0(
    "(turf|lawn|grass|sod)[ /-]*(grass)?[ /-]*(replace|removal|remov|convers|convert|",
    "buy-?back|to[ -]|trade-?in|reduction|reduc)",
    "|(remov[a-z]*|replac[a-z]*|convert[a-z]*|conversion)[^.;|]{0,40}\\b(turf|lawn|grass|sod)\\b",
    "|xeriscap|desertscape|desert[ -]?landscap",
    "|(turf|lawn)[ -]?to[ -]",
    "|sheet[ -]?mulch[a-z]*[^.;|]{0,40}(lawn|turf|grass)",
    "|(lawn|turf|grass)[^.;|]{0,30}sheet[ -]?mulch",
    "|grandscape|lawns? to legumes"),
  ## incidental lawn/grass mentions that are NOT the measure; stripped before matching
  turf_neg = paste0(
    "lawn ?mower|grass[ -]?clipping|lawn care (education|agreement|messaging)",
    "|lawn chemical|grass(ed)? (swale|waterway|channel)|grass[ -]?paver",
    "|lawn[ -]?maintenance bmp|lawn watering|lawn soil restoration",
    ## v58 audit of every added record. Three further incidental forms:
    ## (a) turf/lawn as an ELIGIBLE-AREA metric counted toward a green ratio
    ##     (231 Hiroshima) -- not a removal programme;
    ## (b) replacing PAVING with grass (365 The Hague) -- that is depaving, and
    ##     runs in the opposite direction to turf replacement;
    ## (c) turf-replacement CLASSES / guidance (473 Daly City) -- education,
    ##     not a supported practice.
    "|turf/lawn[ -]?covered|turf/lawn-covered area",
    "|replac\\w* pav\\w*[^.;|]{0,30}with (grass|plants)",
    "|(class|classes|workshop|seminar|course)[^.;|]{0,25}turf",
    "|turf[ -]?replacement (class|classes|workshop|course)"),
  planting = paste0(
    "native[ /-]([a-z-]+ )?[a-z-]*(plant|planting|landscap|garden|species|vegetat|tree|shrub|",
    "grass|perennial|bed|habitat)",
    "|(plant|landscap|garden|vegetat|species)[^.;|]{0,25}\\bnative\\b",
    "|low[ -]?water[ -]?(use[ -]?)?(plant|landscap|garden|vegetat)",
    "|water[ -]?wise|waterwise",
    "|water[ -]?efficient[ -]?(plant|landscap|garden|vegetat)",
    "|drought[ -]?(tolerant|resistant|adapted|proof)",
        ## "wiesen" alone matched Streuobstwiesen (orchard meadow), which is a
    ## location, not a planting measure. Narrowed to flowering-meadow compounds
    ## and extended with the German native-seed vocabulary the corpus uses.
    "|pollinator|wildflower|meadow|blühwiese|blumenwiese|wildblumenwiese",
    ## the harmonised corpus renders Japanese flowerbed-greening programmes
    ## in English ("Citizen flowerbed greening through in-kind flower seedlings").
    ## Measured before adding: matches 3 records, none inside a negation clause.
    "|flower ?bed|flower seedling|flowerbed greening",
    "|regional\\w* saatgut|gebietseigen\\w*|heimisch\\w* (pflanz|saat|geh)",
    "|climate[ -]?(appropriate|adapted|adaptive)[^.;|]{0,20}(plant|landscap|vegetat)",
    "|desert[ -]?adapted|california[ -]?friendly|indigenous plant|prairie",
    "|xeriscap|desertscape")
)

MEASURES$eco_group <- factor(MEASURES$eco_group, levels = names(PAL_ECO))
msg("Design system loaded: ColorBrewer palette, implementation-context frame, %d measures, %d instruments",
    nrow(MEASURES), nrow(INSTRUMENTS))


## -----------------------------------------------------------------------------
## coord_radar(): a polar coordinate system that closes the polygon.
## ggplot2 has no radar coord; coord_polar() draws each polygon side as an arc and
## leaves the last vertex unjoined, which makes a radar look like a spiral. This
## subclass renders straight edges between vertices and closes the loop, which is
## what a radar chart requires.
## -----------------------------------------------------------------------------
coord_radar <- function(theta = "x", start = -pi / 7, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto("CordRadar", ggplot2::CoordPolar,
          theta = theta, r = r, start = start,
          direction = sign(direction), clip = "off",
          is_linear = function(coord) TRUE)
}
