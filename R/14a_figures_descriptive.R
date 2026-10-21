# =============================================================================
# 14a_figures_descriptive.R  |  PART A -- DESCRIPTIVE OVERVIEW
#
# What the corpus contains, before any fit judgement. Four figures:
#   A1  geography and who delivers          (map, all countries)
#   A2  the NbS measures                     (tiers + house schematic + definitions)
#   A3  the support instruments              (families, no "+" bundles)
#   A4  the cities                           (built-form x tenure typology + context)
# =============================================================================

suppressPackageStartupMessages({ library(ggplot2); library(patchwork); library(scales); library(forcats) })
HAS <- function(p) requireNamespace(p, quietly = TRUE)


FG <- M %>% left_join(config_signatures, by = "case_id") %>% add_city_typology()


# =============================================================================
# FIGURE 1 | THE STUDY PIPELINE (from the ratified schematic)
# Three numbered premises feed three analysis stages, draining into scaling
# pathways. Minimal text; the reader follows the arrows.
# =============================================================================
sp_box <- function(x0, x1, y0, y1, fill = "#ffffff", col = "grey45", lw = .4)
  annotate("rect", xmin = x0, xmax = x1, ymin = y0, ymax = y1, fill = fill, colour = col, linewidth = lw)
sp_txt <- function(x, y, lab, size = 2.5, face = "plain", col = PAL$ink, hj = 0, lh = 1.1)
  annotate("text", x = x, y = y, label = lab, size = size, fontface = face, colour = col, hjust = hj, lineheight = lh)
sp_num <- function(x, y, n) list(
  annotate("point", x = x, y = y, size = 7.5, colour = "#1b7a72"),
  annotate("text", x = x, y = y, label = n, size = 3.1, fontface = "bold", colour = "white"))
sp_arr <- function(x0, x1, y) annotate("segment", x = x0, xend = x1, y = y, yend = y,
  linewidth = .65, colour = "grey25", arrow = arrow(length = unit(6, "pt"), type = "closed"))
TEAL <- "#1b7a72"

p_pipe <- ggplot() +
  ## premise row
  sp_num(.55, 9.3, "1") + sp_box(.95, 4.35, 8.55, 10.05) +
  sp_txt(1.15, 9.6, "City mandate", 2.7, "bold") +
  sp_txt(1.15, 9.05, "NbS goals whose reach ends at the\nparcel boundary", 2.25, col = "grey30") +
  sp_num(5.05, 9.3, "2") + sp_box(5.45, 8.85, 8.55, 10.05) +
  sp_txt(5.65, 9.6, "Programme mechanism", 2.7, "bold") +
  sp_txt(5.65, 9.05, "support programmes carrying\ndifferent instrument families", 2.25, col = "grey30") +
  sp_num(9.55, 9.3, "3") + sp_box(9.95, 13.35, 8.55, 10.05) +
  sp_txt(10.15, 9.6, "Citizen-led NbS", 2.7, "bold") +
  sp_txt(10.15, 9.05, "micro-scale measures on\n(semi-)private land", 2.25, col = "grey30") +
  ## stage row
  sp_box(.95, 4.35, 4.7, 7.9) +
  sp_txt(1.15, 7.45, "Programme landscape\nmapping", 2.6, "bold") +
  sp_txt(1.15, 6.2, sprintf("hazards, measures and support\ntypes documented across %d\nprogrammes in %d countries\n(RQ1 \u00b7 Figures 3\u20136)",
                           nrow(A), dplyr::n_distinct(A$country)), 2.2, col = "grey30") +
  sp_arr(4.45, 5.35, 6.3) +
  sp_box(5.45, 8.85, 4.7, 7.9) +
  sp_txt(5.65, 7.45, "Programme fit\nassessment", 2.6, "bold") +
  sp_txt(5.65, 6.2, "seven relational fit tests across\nthree domains assess design\nquality and equity alignment\n(RQ2 \u00b7 Figures 7\u20138)", 2.2, col = "grey30") +
  sp_arr(8.95, 9.85, 6.3) +
  sp_box(9.95, 13.35, 4.7, 7.9) +
  sp_txt(10.15, 7.45, "Programme\ntypologies", 2.6, "bold") +
  sp_txt(10.15, 6.2, "design typologies from MCA and\nhierarchical clustering on the\nprogramme-design constructs\n(RQ3 \u00b7 Figure 8)", 2.2, col = "grey30") +
  ## vertical premise -> stage connectors
  annotate("segment", x = c(2.65, 7.15, 11.65), xend = c(2.65, 7.15, 11.65),
           y = 8.5, yend = 7.95, linewidth = .5, colour = "grey25",
           arrow = arrow(length = unit(5, "pt"), type = "closed")) +
  ## outcome
  sp_arr(13.45, 14.25, 6.3) +
  annotate("point", x = 16.1, y = 6.3, size = 62, colour = TEAL) +
  sp_txt(16.1, 6.95, "Scaling pathways", 2.75, "bold", "white", hj = .5) +
  sp_txt(16.1, 6.0, "context-sensitive guidance\nby hazard orientation, urban\ndensity, tenure and typology", 2.1, col = "white", hj = .5) +
  coord_cartesian(xlim = c(0, 18.3), ylim = c(3.9, 10.55), expand = FALSE) +
  theme_void() +
  plot_annotation(
    title = "Figure 1. Study overview: from the city mandate to context-sensitive scaling pathways",
    theme = TITLE())
save_fig(p_pipe, "fig01_study_pipeline", 13, 5)


# =============================================================================
# FIGURE 01 | THE FRAMEWORK AS A WIRING DIAGRAM
#
# Show, don't tell: the framework IS a set of specific comparisons, so the figure
# draws each one as a pair of edges. Every fit test (centre) receives exactly
# the context element(s) that generate its requirement (blue edges, from the left)
# and the architecture element(s) that answer it (amber edges, from the right).
# A reader can trace any single wire: tenure runs into D2 and nowhere else;
# post-installation arrangements run into D4 and nowhere else; D3's requirement
# side comes from the programme's own measure portfolio -- an amber edge from
# Measures -- which is assumption A3 made visible rather than stated. Tests drain
# into one five-state profile, and the profile into the two outputs. Justice is
# three red taps on the tests it is read from.
# =============================================================================
fw_box <- function(x0, x1, y0, y1, fill, col = NA, lw = .35) annotate("rect",
  xmin = x0, xmax = x1, ymin = y0, ymax = y1, fill = fill, colour = col, linewidth = lw)
fw_txt <- function(x, y, lab, size = 2.3, face = "plain", col = PAL$ink, hj = .5, lh = 1.08, a = 0)
  annotate("text", x = x, y = y, label = lab, size = size, fontface = face,
           colour = col, hjust = hj, lineheight = lh, angle = a)
fw_edge <- function(x0, y0, x1, y1, col, lw = .55, curv = .12) annotate("curve",
  x = x0, y = y0, xend = x1, yend = y1, colour = col, linewidth = lw,
  curvature = curv, alpha = .85)

C_CTX <- "#2b6cb0"; C_ARC <- "#c05621"
F_CTX <- "#deebf7"; F_ARC <- "#fdefd2"
S_FIT <- "#2b6cb0"; S_PART <- "#fdae61"; S_MIS <- "#d73027"; S_IND <- "#8a8a8a"; S_NA <- "#e8e8e8"

## ---- node positions --------------------------------------------------------
## tests, centre column
tst <- tibble::tribble(
  ~y,    ~id,    ~lab,
  10.9, "D1",  "D1   hazard is answered by a delivering measure",
   9.6, "D2",  "D2   authority over the space matches who is admitted",
   8.3, "D3a", "D3a  portfolio's technical demand is enabled",
   7.0, "D3b", "D3b  cost falls within the applicant's reach",
   5.7, "D3c", "D3c  procedure is navigable",
   4.4, "D4a", "D4a  maintenance duty carries support",
   3.1, "D4b", "D4b  persistence is checked")
TX0 <- 5.05; TX1 <- 11.6                     # test box extent
## context nodes, left
ctx <- tibble::tribble(
  ~y,    ~lab,
  10.9, "hazard exposure",
   9.6, "tenure and authority",
   8.45, "built form",
   7.0, "applicant financial capacity",
   5.5, "density \u00b7 population")
CX0 <- .3; CX1 <- 3.3
## architecture nodes, right
arc <- tibble::tribble(
  ~y,     ~lab,
  10.9, "measures (NbS taxonomy)",
   9.3, "eligible actors and routes",
   7.75, "support instruments",
   6.2, "procedure and payment design",
   4.4, "maintenance support")
AX0 <- 13.35; AX1 <- 16.75

## ---- edge list: requirement (blue) and provision (amber), per test ---------
## anchors are STAGGERED down each box edge (from_y offsets) so wires leaving
## the same box separate immediately instead of crossing at the box edge.
edges <- tibble::tribble(
  ~side,  ~from_y, ~to_y,
  "ctx",  10.9, 10.9,   # hazard -> D1
  "ctx",   9.6,  9.6,   # tenure -> D2
  "ctx",   8.45, 9.35,  # built form -> D2 (the space the measure occupies)
  "ctx",   7.0,  7.0,   # applicant financial capacity -> D3b
  "arc",  11.15, 10.9,  # measures -> D1 (top anchor)
  "arc",  10.9,   8.3,  # measures -> D3a (middle anchor)
  "arc",  10.65,  4.55, # measures -> D4a (bottom anchor)
  "arc",   9.3,   9.5,  # actors/routes -> D2
  "arc",   7.75,  8.15, # instruments -> D3a
  "arc",   6.35,  7.0,  # procedure/payment -> D3b (upper anchor)
  "arc",   6.05,  5.7,  # procedure/payment -> D3c (lower anchor)
  "arc",   4.55,  4.3,  # maintenance support -> D4a (upper anchor)
  "arc",   4.25,  3.1)  # maintenance support -> D4b (lower anchor)

p_fw <- ggplot() +

  ## column headers -----------------------------------------------------------
  fw_txt((CX0+CX1)/2, 12.15, "URBAN CONTEXT", 2.9, "bold", C_CTX) +
  fw_txt((CX0+CX1)/2, 11.72, "generates the requirement \u00b7 never scored", 2.0, "italic", "grey40") +
  fw_txt((TX0+TX1)/2, 12.15, "SEVEN RELATIONAL FIT TESTS", 2.9, "bold") +
  fw_txt((TX0+TX1)/2, 11.72, "does the provision answer the requirement?", 2.0, "italic", "grey40") +
  fw_txt((AX0+AX1)/2, 12.15, "PROGRAMME ARCHITECTURE", 2.9, "bold", "#9c4221") +
  fw_txt((AX0+AX1)/2, 11.72, "offers the provision \u00b7 documented design only", 2.0, "italic", "grey40") +

  ## edges first, so nodes sit on top -----------------------------------------
  purrr::pmap(edges, function(side, from_y, to_y) {
    if (side == "ctx") fw_edge(CX1 + .05, from_y, TX0 - .05, to_y, C_CTX,
                               curv = ifelse(abs(from_y - to_y) < .5, 0, .10))
    else               fw_edge(AX0 - .05, from_y, TX1 + .05, to_y, C_ARC,
                               curv = ifelse(abs(from_y - to_y) < .5, 0, -.10))
  }) +

  ## context nodes -------------------------------------------------------------
  purrr::pmap(ctx, function(y, lab) list(
    fw_box(CX0, CX1, y - .38, y + .38, F_CTX, C_CTX, .4),
    fw_txt((CX0+CX1)/2, y, lab, 2.3))) +
  ## density/population: enters the models directly, not a test -- shown as a
  ## grey wire bypassing the tests into PATTERNS, which is rule R2 drawn.
  annotate("curve", x = CX1 + .05, y = 5.5, xend = 2.2, yend = 1.05,
           colour = "grey60", linewidth = .5, curvature = .34, linetype = "22") +

  ## test nodes ----------------------------------------------------------------
  purrr::pmap(tst, function(y, id, lab) list(
    fw_box(TX0, TX1, y - .43, y + .43, "#fbfbfb", "grey45", .45),
    fw_txt(TX0 + .18, y, lab, 2.3, hj = 0))) +

  ## architecture nodes ---------------------------------------------------------
  purrr::pmap(arc, function(y, lab) list(
    fw_box(AX0, AX1, y - .38, y + .38, F_ARC, C_ARC, .4),
    fw_txt((AX0+AX1)/2, y, lab, 2.3))) +

  ## JUSTICE: a translucent band running the full height of the test column,
  ## with its three dimensions anchored at the tests each one is read from --
  ## recognitional at D2, procedural at D3b/D3c, distributive at D4a/D4b.
  fw_box(10.85, 11.5, 2.67, 11.33, "#fdf2f2", NA) +
  annotate("rect", xmin = 10.85, xmax = 11.5, ymin = 7.87, ymax = 10.03,
           fill = "#f3c3c3", colour = "#a50026", linewidth = .35) +
  annotate("rect", xmin = 10.85, xmax = 11.5, ymin = 5.27, ymax = 7.43,
           fill = "#f3c3c3", colour = "#a50026", linewidth = .35) +
  annotate("rect", xmin = 10.85, xmax = 11.5, ymin = 2.67, ymax = 4.83,
           fill = "#f3c3c3", colour = "#a50026", linewidth = .35) +
  fw_txt(11.17, 11.6, "JUSTICE", 2.3, "bold", "#a50026") +
  fw_txt(11.17, 8.95, "recognitional", 1.95, "bold", "#a50026", a = 90) +
  fw_txt(11.17, 6.35, "procedural", 1.95, "bold", "#a50026", a = 90) +
  fw_txt(11.17, 3.75, "distributive", 1.95, "bold", "#a50026", a = 90) +

  ## tests drain into ONE profile ----------------------------------------------
  purrr::map(tst$y, function(y) annotate("curve",
    x = (TX0+TX1)/2, y = y - .43, xend = (TX0+TX1)/2, yend = 2.28,
    colour = "grey55", linewidth = .35, curvature = 0, alpha = .5)) +
  fw_box(4.6, 12.05, 1.42, 2.28, "#ffffff", "grey40", .5) +
  fw_txt(4.85, 2.02, "the programme's PROFILE \u2014 seven verdicts, no score", 2.25, "bold", hj = 0) +
  fw_box(4.85, 6.15, 1.56, 1.84, S_FIT, NA)  + fw_txt(5.5, 1.7, "fit", 1.95, col = "white") +
  fw_box(6.3, 7.6, 1.56, 1.84, S_PART, NA)   + fw_txt(6.95, 1.7, "partial", 1.95) +
  fw_box(7.75, 9.05, 1.56, 1.84, S_MIS, NA)  + fw_txt(8.4, 1.7, "misfit", 1.95, col = "white") +
  fw_box(9.2, 10.5, 1.56, 1.84, S_IND, NA)   + fw_txt(9.85, 1.7, "indeterminate", 1.85, col = "white") +
  fw_box(10.65, 11.85, 1.56, 1.84, S_NA, "grey60") + fw_txt(11.25, 1.7, "not applicable", 1.85) +
  fw_txt(12.25, 1.85, "silence \u2260 misfit\napplicability first", 1.85, "italic", "#a50026", hj = 0) +

  ## profile drains into the two outputs ----------------------------------------
  fw_edge(5.6, 1.42, 3.4, .95, "grey45", .5, .18) +
  fw_edge(11.0, 1.42, 13.3, .95, "grey45", .5, -.18) +
  fw_box(.3, 6.0, .1, 1.0, F_CTX, C_CTX, .45) +
  fw_txt(.55, .72, "TYPOLOGIES", 2.35, "bold", C_CTX, hj = 0) +
  fw_txt(.55, .38, "which architecture types recur, where, and what travels together", 1.95, hj = 0) +
  fw_box(11.1, 16.75, .1, 1.0, F_ARC, C_ARC, .45) +
  fw_txt(11.35, .72, "SCALING PATHWAYS", 2.35, "bold", "#9c4221", hj = 0) +
  fw_txt(11.35, .38, "what each context requires for micro-NbS support to scale out", 1.95, hj = 0) +

  coord_cartesian(xlim = c(0, 16.95), ylim = c(-.1, 12.5), expand = FALSE) +
  theme_void() +
  plot_annotation(
    title = "Figure 2. Programme fit as a relational assessment: each test compares one requirement with one provision",
    subtitle = WRAP("Blue wires carry what the context requires; amber wires carry what the documented design offers. Five tests pair a context requirement with a design provision; D3c and D4b have no blue wire because their requirement is universal \u2014 every applicant must navigate a procedure and every installation must persist \u2014 so no contextual condition has to arise for them to apply. The dashed wire is context entering the models directly \u2014 never scored. Verdicts pool into a profile, never an index."),
    theme = TITLE())
save_fig(p_fw, "fig02_framework", 13.5, 10)



# =============================================================================
# FIGURE A1 | Corpus geography and delivery
# =============================================================================
if (HAS("maps")) {
  cmap <- c("United States"="USA","Japan"="Japan","Germany"="Germany","Canada"="Canada",
            "Netherlands"="Netherlands","France"="France","Poland"="Poland","Brazil"="Brazil",
            "Belgium"="Belgium","Hungary"="Hungary","Denmark"="Denmark","Switzerland"="Switzerland",
            "Austria"="Austria","Australia"="Australia","Spain"="Spain","Slovak Republic"="Slovakia",
            "Czech Republic"="Czech Republic","United Kingdom"="UK","Ireland"="Ireland",
            "Korea, Rep."="South Korea","India"="India","New Zealand"="New Zealand",
            "Malaysia"="Malaysia","Mexico"="Mexico","Taiwan, Province of China"="Taiwan",
            "Norway"="Norway","Argentina"="Argentina","Singapore"="Singapore")
  ## v64 REPRODUCIBILITY FIX. `maps::world.cities` is a PACKAGE DATA TABLE whose
  ## contents differ between maps releases, and `iconv(..., ASCII//TRANSLIT)` is
  ## additionally locale- and platform-dependent. Between two machines this moved
  ## lat/long on 60 city rows and geo_type on 23, which then moved every derived
  ## zoom box -- 16 of the 16 differences a cross-machine comparison reported,
  ## none of them touching a verdict, a fit table or the typology.
  ## Coordinates are INPUT DATA, not a result, so they are frozen: the first run
  ## writes data/CITY_GEOCODES.csv and every later run reads it. Delete the file
  ## to re-derive from whatever maps version is installed.
  .geo_frozen <- file.path("data", "CITY_GEOCODES.csv")
  if (file.exists(.geo_frozen)) {
    wc <- as_tibble(utils::read.csv(.geo_frozen, stringsAsFactors = FALSE))
    msg("  city geocodes: read %d frozen rows from %s", nrow(wc), .geo_frozen)
  } else {
    wc <- as_tibble(maps::world.cities) %>%
      ## stringi, not iconv -- see the note on the corpus side below.
      mutate(ckey = tolower(stringi::stri_trans_general(name, "Latin-ASCII"))) %>%
      select(ckey, country.etc, lat, long) %>% distinct(ckey, country.etc, .keep_all = TRUE)
    utils::write.csv(wc, .geo_frozen, row.names = FALSE)
    msg("  city geocodes: froze %d rows to %s (delete to re-derive)", nrow(wc), .geo_frozen)
  }
  geo <- FG %>%
    ## 1 THE OTHER HALF OF THE JOIN. Freezing the lookup table alone did not
    ## make the map deterministic: the CORPUS-side key was still built with
    ## iconv(..., "ASCII//TRANSLIT"), which mangles accented city names
    ## differently on macOS and glibc. The frozen keys were normalised on one
    ## platform and the corpus keys on the other, so accented cities missed the
    ## join and fell back to a country centroid -- the 60 lat/long and 23
    ## geo_type rows that still differed. stringi::stri_trans_general is
    ## identical under every locale and platform tested and is already used for
    ## the same purpose in norm_city().
    mutate(ckey = tolower(stringi::stri_trans_general(city, "Latin-ASCII")),
           cmapped = unname(cmap[country])) %>%
    left_join(wc, by = c("ckey" = "ckey", "cmapped" = "country.etc"), relationship = "many-to-one") %>%
    mutate(geo_type = ifelse(is.na(lat), "country centroid", "city match"))
  cent <- geo %>% filter(!is.na(lat)) %>% group_by(country) %>%
    summarise(clat = mean(lat), clon = mean(long), .groups = "drop")
  geo <- geo %>% left_join(cent, by = "country") %>%
    mutate(lat = coalesce(lat, clat), long = coalesce(long, clon))
  ## One point per city. Nearly every city holds exactly one programme, so
  ## sizing by programme count would render 701 identical dots; size therefore
  ## encodes CITY POPULATION (Context) and colour the lead body. Sizes are kept
  ## in a narrow, always-visible band and dense regions get zoom insets instead
  ## of larger dots.
  city_pts <- geo %>% filter(!is.na(lat)) %>%
    group_by(city, country, lat, long, geo_type) %>%
    ## the map sizes by POPULATION (the author's instruction was about panel
    ## b, not the map). The land-use mix is carried onto each city point for the
    ## audit trail only; `names(sort(table(...)))[1]` returns character(0) rather
    ## than NA when a city's records are all unclassified, which summarise()
    ## rejects as an incompatible vector -- hence the explicit NA fallback.
    summarise(n = n(), pop = suppressWarnings(max(pop, na.rm = TRUE)),
              landuse_mix = { .t <- sort(table(landuse_mix), decreasing = TRUE)
                              if (length(.t)) names(.t)[1] else NA_character_ },

              lead = names(sort(table(lead_body), decreasing = TRUE))[1], .groups = "drop") %>%
    mutate(pop = ifelse(is.finite(pop), pop, NA_real_)) %>%
    left_join(FG %>% distinct(city, country, region_mod), by = c("city", "country"),
              relationship = "many-to-many") %>%
    distinct(city, country, lat, long, geo_type, .keep_all = TRUE)
  world <- ggplot2::map_data("world")

  base_map <- function(xlim, ylim, pts, psize = c(1.4, 4.6), show_centroid = FALSE) {
    p <- ggplot() +
      ## Land in a warm neutral with faint borders on a near-white ocean, so the
      ## programme points are the only saturated ink on the panel; no graticule.
      geom_polygon(data = world, aes(long, lat, group = group),
                   fill = "#EDE9E3", colour = "#FFFFFF", linewidth = .18) +
      geom_point(data = pts %>% filter(geo_type == "city match"),
                 ## SIZE IS POPULATION AGAIN. v31 read the instruction backwards.
                 ## The author wrote that the MAP already shows population by dot
                 ## size, and that PANEL B's population column should therefore be
                 ## replaced by land use -- so that panel b carries density, land
                 ## use and tenure. The map keeps population; panel b changes.
                 aes(long, lat, fill = lead, size = pop), shape = 21,
                 alpha = .78, colour = "white", stroke = .25) +
      geom_point(data = pts %>% filter(geo_type == "city match", n >= 2),
                 aes(long, lat, size = pop), shape = 21, fill = NA,
                 colour = PAL$ink, stroke = .55)
    if (show_centroid)
      p <- p + geom_point(data = pts %>% filter(geo_type == "country centroid"),
                          aes(long, lat), shape = 21, size = 1.9,
                          colour = "grey35", fill = NA, stroke = .45)
    p + scale_fill_manual(values = PAL_LEAD, na.value = PAL$grey, name = "Lead body") +
      scale_size(range = psize, breaks = c(1e5, 5e5, 2e6, 8e6),
                 labels = c("100k", "500k", "2M", "8M"), name = "City population (Context)") +
      coord_fixed(1.32, xlim = xlim, ylim = ylim, expand = FALSE) +
      theme_nbs(9) +
      theme(axis.text = element_blank(), axis.title = element_blank(),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "#FBFCFD", colour = NA),
            panel.border = element_rect(fill = NA, colour = "grey86", linewidth = .3))
  }

  ## Every region gets its own zoom, computed from the data and padded, so no
  ## region is silently left at global scale. Numbered boxes on the world map
  ## correspond one-to-one with the numbered panels beneath it.
  ## Zoom only the four regions where the corpus actually concentrates. "Other
  ## regions" is deliberately NOT zoomed: its members are scattered across four
  ## continents, so its bounding box degenerates to world scale and the inset
  ## reproduces the world map. Those cities stay visible on the world map above
  ## and are reported in the country panel (A4c) instead.
  ## Trim to the central 98% of each region's points so one outlying city cannot
  ## inflate a box (e.g. Hawaii or Alaska stretching North America).
  REG_SEQ <- c("North America", "Western Europe", "Central/Eastern Europe", "East Asia")
  reg_box <- city_pts %>%
    filter(geo_type == "city match", region_mod %in% REG_SEQ) %>%
    group_by(region_mod) %>%
    summarise(x0 = stats::quantile(long, .01, names = FALSE),
              x1 = stats::quantile(long, .99, names = FALSE),
              y0 = stats::quantile(lat,  .01, names = FALSE),
              y1 = stats::quantile(lat,  .99, names = FALSE),
              n = n(), .groups = "drop") %>%
    mutate(region_mod = factor(region_mod, levels = REG_SEQ)) %>%
    filter(!is.na(region_mod)) %>% arrange(region_mod) %>%
    mutate(padx = pmax((x1 - x0) * .10, 2), pady = pmax((y1 - y0) * .10, 2),
           x0 = x0 - padx, x1 = x1 + padx, y0 = y0 - pady, y1 = y1 + pady,
           idx = row_number()) %>%
    ## EQUAL SQUARE INSETS. The maps draw under coord_fixed(1.32), so a panel's
    ## rendered aspect is 1.32 * (dlat/dlon); it renders as a SQUARE only when
    ## dlon = 1.32 * dlat exactly. Give each region the lat extent it needs
    ## (or lon/1.32 if that binds), centre on the midpoint, and set
    ## dlon = 1.32 * dlat -- every panel is then an identical square on the
    ## page while keeping its own zoom.
    mutate(cx = (x0 + x1) / 2, cy = (y0 + y1) / 2,
           dlat = pmax(y1 - y0, (x1 - x0) / 1.32),
           dlon = 1.32 * dlat,
           x0 = cx - dlon / 2, x1 = cx + dlon / 2,
           y0 = cy - dlat / 2, y1 = cy + dlat / 2)
  save_tab(reg_box, "14_a1_region_zoom_boxes")

  ## NO DASHED LOCATOR BOXES. They added four rectangles of ink across the
  ## densest part of the map for information a numbered chip already carries.
  ## The chip sits at the region's centroid; the inset below repeats the number
  ## and carries a short rule in the same ink, which is what ties the two together.
  p_world <- base_map(c(-165, 178), c(-56, 76), city_pts, show_centroid = TRUE) +
    geom_label(data = reg_box, aes(x = cx, y = y1 + 3, label = idx), inherit.aes = FALSE,
               size = 2.5, fontface = "bold", colour = "white", fill = PAL$ink,
               label.padding = unit(1.5, "pt"), label.size = 0, label.r = unit(3, "pt")) +
    labs(title = "a. Cities represented in the global-search programme corpus, and who delivers there",
         subtitle = WRAP("One point per city, sized by city population and coloured by lead body; a dark ring marks cities holding two or more programmes. Hollow points sit at a country centroid where the city could not be geocoded; unmarked countries reflect the search corpus, not an absence of programmes. Numbered chips mark the four regions enlarged below; cities in other regions appear on the world map only, as their combined extent spans four continents and would not enlarge meaningfully."))

  ## Each inset colours ONLY its own region's cities; cities of other regions
  ## that fall inside the shared square footprint are faint grey context --
  ## otherwise the Western Europe and Central/Eastern Europe squares, which
  ## overlap geographically, would read as duplicates.
  reg_panel <- function(i) {
    b <- reg_box[i, ]
    own   <- city_pts %>% filter(region_mod == as.character(b$region_mod))
    other <- city_pts %>% filter(region_mod != as.character(b$region_mod),
                                 geo_type == "city match")
    base_map(c(b$x0, b$x1), c(b$y0, b$y1), own, psize = c(1.7, 5.6)) +
      geom_point(data = other, aes(long, lat, size = pop), shape = 21,
                 fill = "grey82", colour = "white", alpha = .55, stroke = .2) +
      guides(fill = "none", size = "none") +
      labs(title = sprintf("%d \u2014 %s   (%d cities)", b$idx, b$region_mod, b$n)) +
      theme(plot.title = element_text(size = 7.6, face = "bold", colour = PAL$ink,
                                      margin = margin(b = 3)),
            plot.margin = margin(3, 4, 3, 4))
  }
  reg_panels <- lapply(seq_len(nrow(reg_box)), reg_panel)
  ## All four insets share one square footprint (equal geographic side length,
  ## equal panel size).
  p_zooms <- patchwork::wrap_plots(reg_panels, nrow = 1, widths = rep(1, 4))
  p_a1 <- p_world / p_zooms + plot_layout(heights = c(1.5, 1))
  ## Figure 3 is completed after the country-context panel (A4c) is built:
  ## it becomes panel b, with the renter-share distribution as its 4th column.

  save_tab(city_pts, "14_city_points_map")
}

# =============================================================================
# FIGURE A2 | The NbS measures: tiers, cross-section schematic, definitions
# =============================================================================
mprev <- MEASURES %>%
  mutate(n = map_int(var, ~ sum(FG[[.x]] == 1, na.rm = TRUE)), pct = n / nrow(FG)) %>%
  arrange(desc(n))

## PANEL A (analytical, not descriptive): what the hazard actually demands.
## Each flow runs hazard -> the ecological function that answers it -> the
## specific measure delivering that function -> the technical support tier the
## measure needs. Reading right to left shows which support a city must supply
## once it names a hazard; multi-response throughout, so a record contributes a
## flow for every hazard-measure pair it documents.
## ONE ecological function per measure -- single-term labels, no paired
## compounds. The only permitted compound is "Capture and storage", which is a
## single indivisible function (a vessel captures in order to store).
## Pollination is folded into Habitat: habitat provision already carries it and
## the case base does not support it as an independent function.
ECO_FUN <- c(m_greenroof = "Retention",
             m_facade    = "Cooling",
             m_trees     = "Shading",
             m_raingard  = "Retention",
             m_pond      = "Retention",
             m_infiltr   = "Infiltration",
             m_depave    = "Infiltration",
             m_disconn   = "Infiltration",
             m_barrel    = "Capture and storage",
             m_planting  = "Habitat",
             m_turf      = "Habitat")
## Hazard vocabulary fixed across every figure: Floods, Heat, Drought,
## Biodiversity loss (see HAZ_LABELS in 00b_design.R).
HAZ4 <- list(Floods               = quote(hx_flood == 1 | hx_coastal == 1),
             Heat                 = quote(hx_heat == 1),
             Drought              = quote(hx_drought == 1),
             `Biodiversity loss`  = quote(obj_biodiv == 1))
flows <- purrr::map_dfr(names(HAZ4), function(h) {
  keep <- which(sapply(seq_len(nrow(FG)), function(r)
    isTRUE(eval(HAZ4[[h]], envir = as.list(FG[r, ])))))
  if (!length(keep)) return(NULL)
  purrr::map_dfr(names(ECO_FUN), function(v) {
    nn <- sum(FG[[v]][keep] == 1, na.rm = TRUE)
    if (!nn) return(NULL)
    tibble(hazard = h, func = unname(ECO_FUN[v]),
           measure = MEASURES$measure[MEASURES$var == v],
           tier = MEASURES$tier[MEASURES$var == v], n = nn)
  })
})
save_tab(flows, "14_a2_hazard_function_measure_flows")
TIER_SHORT <- c("High demand (building-integrated)" = "High demand\n(building-integrated)",
                "Medium demand (ground-modifying)" = "Medium demand\n(ground-modifying)",
                "Low demand (standardised)" = "Low demand\n(standardised)")
## Measure axis ordered ALPHABETICALLY (not in taxonomy order) so the ribbons
## cross rather than running as parallel straight lines -- the crossings are
## what make the hazard -> function -> measure routing legible.
MEASURE_ALPHA <- sort(unique(as.character(flows$measure)))
FUNC_ALPHA    <- sort(unique(as.character(flows$func)))
flows2 <- flows %>%
  mutate(hazard = factor(hazard, levels = names(HAZ4)),
         func = factor(func, levels = FUNC_ALPHA),
         measure = factor(measure, levels = MEASURE_ALPHA),
         tier = factor(unname(TIER_SHORT[as.character(tier)]), levels = unname(TIER_SHORT)))
## THREE axes, not four. Ecological function has moved OUT of this panel and into
## figure B1c, where it does analytical work: B1c asks whether the portfolio
## contains a measure that delivers the function the hazard requires, which is the
## D1 fit test. Keeping it here as well made A2a a partial preview of that test
## and left A2a itself ambiguous about whether it was describing or assessing.
## A2a is now unambiguously DESCRIPTIVE: what hazard is named, what measure is
## supported, what technical support that measure demands.
p_a2_sankey <- ggplot(flows2, aes(y = n, axis1 = hazard,
                                  axis2 = measure, axis3 = tier)) +
  ggalluvial::geom_alluvium(aes(fill = hazard), alpha = .62, width = .22,
                            curve_type = "sigmoid") +
  ggalluvial::geom_stratum(width = .22, fill = "grey97", colour = "grey55", linewidth = .3) +
  geom_text(stat = ggalluvial::StatStratum, aes(label = stringr::str_wrap(as.character(after_stat(stratum)), 24)), size = 2.1, lineheight = .9) +
  scale_fill_manual(values = HAZ_FILL, name = "Documented hazard") +
  scale_x_continuous(breaks = 1:3, labels = c("Hazard / objective\n(Context)",
                                              "Measure supported\n(E-1..E-7)",
                                              "Support the measure needs\n(technical and administrative; D3a)"),
                     expand = c(.06, .06)) +
  labs(title = "a. From the hazard a city names to the measures it supports and the technical support those measures demand",
       subtitle = WRAP("DESCRIPTIVE: what is named, what is supported, and what that support demands. Multi-response flows -- every hazard-measure pair a record documents contributes one flow, so widths are programme-measure combinations rather than programmes. The right-hand axis is the demand class that drives the technical fit test. Whether a supported measure actually delivers the ecological function its hazard requires is the D1 fit test, assessed in figure B1c, not here."),
       x = NULL, y = "Programme-measure combinations") +
  theme_nbs(7.6) + theme(legend.position = "bottom",
                         panel.grid.major.x = element_blank())

## Panel C: prevalence grouped by NATURE-BASED function; technical demand is a
## secondary badge (H/M/L), not the primary taxonomy
grp_tot <- purrr::map_dfr(names(PAL_ECO), function(g) {
  vars <- MEASURES$var[MEASURES$eco_group == g]
  tibble(eco_group = g, measure = sprintf("ANY %s measure", toupper(g)),
         mlab_raw = sprintf("All %s", tolower(g)),
         n = sum(rowSums(FG[, vars, drop = FALSE] == 1, na.rm = TRUE) > 0),
         is_total = TRUE)
}) %>% mutate(pct = n / nrow(FG))
mprev2 <- bind_rows(
  mprev %>% mutate(mlab_raw = sprintf("%s  [%s]", measure, TIER_BADGE[tier]), is_total = FALSE),
  grp_tot) %>%
  mutate(eco_group = factor(eco_group, levels = names(PAL_ECO))) %>%
  group_by(eco_group) %>%
  arrange(desc(is_total), n, .by_group = TRUE) %>%
  mutate(mlab = factor(mlab_raw, levels = mlab_raw)) %>% ungroup()
## TAXONOMY COMPLETENESS AUDIT (resolved). Five candidate measures were
## recovered from the E-7 free text and each was read back against the source
## wording before any decision. None qualifies as a citizen-led micro-NbS
## measure, so none is promoted into the analysis taxonomy; the verdicts and
## counts are exported so the exclusion is auditable rather than silent.
## ---- TAXONOMY AUDIT, RESOLVED --------------------------------------------
## Re-read against the source wording, with the citizen-led test applied:
##  - ponds / wetlands / BIOSWALES: PROMOTED. 41 free-text term hits, of which
##    34 sit inside citizen-measure lists (swales beside rain gardens and
##    cisterns; "infiltration wells and swales -- 35 built since 2019"); those
##    34 now enter the analysis taxonomy as a vegetated ground-and-canopy
##    measure with the ground-modifying demand class, the same requirement
##    profile as rain gardens. The 7 excluded hits are municipal-asset context
##    or explicit non-offers and are listed for the validation pass.
##  - compost / mulch and soil amendments: EXCLUDED. Support inputs to another
##    measure (topsoil within a facade scheme, subsidised backyard composters),
##    not nature-based interventions in their own right.
##  - greywater reuse: EXCLUDED. Building water-services retrofit, listed beside
##    solar hot water and PV; no vegetated or soil-based mechanism.
##  - rainwater harvesting: EXCLUDED as a separate row. The same practice as
##    rain barrels / cisterns (E-6), 32 of 33 records co-occurring.
audit <- tribble(
  ~vv, ~mm, ~decision, ~verdict,
  "m_pond",      "Ponds / wetlands / bioswales", "PROMOTED to the analysis taxonomy",
    "Citizen-scale vegetated water measure; ground-modifying demand class, requirement profile mirrors rain gardens",
  "m_compost",   "Compost or mulch provision",   "Excluded -- supporting practice",
    "Material input supporting another measure, not an intervention in itself",
  "m_soil",      "Soil amendments",              "Excluded -- supporting practice",
    "Preparation input to planting or infiltration measures",
  "m_greywater", "Greywater reuse",              "Excluded -- not nature-based",
    "Building water-services retrofit; no vegetated or soil-based mechanism",
  "m_rainharv",  "Rainwater harvesting",         "Excluded -- duplicate of E-6",
    "Same practice as rain barrels / cisterns already coded at E-6") %>%
  mutate(n = purrr::map_int(vv, ~ sum(FG[[.x]] == 1, na.rm = TRUE)),
         pct = n / nrow(FG),
         overlap_rain_barrel = purrr::map_int(vv, ~ sum(FG[[.x]] == 1 & FG$m_barrel == 1, na.rm = TRUE)))
audit <- bind_rows(audit,
  tibble(vv = "m_pond_excluded", mm = "Pond/wetland/swale mentions NOT retained",
         decision = "Held back for validation", 
         verdict = "Municipal-asset context or explicit non-offer; re-check during the dataset validation pass",
         n = sum(FG$m_pond_excluded == 1, na.rm = TRUE),
         pct = sum(FG$m_pond_excluded == 1, na.rm = TRUE) / nrow(FG), overlap_rain_barrel = NA_integer_))
save_tab(audit, "14_a2_measure_taxonomy_audit")
msg("  measure-taxonomy audit: ponds promoted (n = %d citizen-scale, %d held back); 4 categories excluded",
    sum(FG$m_pond == 1, na.rm = TRUE), sum(FG$m_pond_excluded == 1, na.rm = TRUE))



## Panel B: a proper cross-section. EVERY measure is drawn as its own element
## with its own leader line, label, share and one-line definition -- nothing is
## pooled into a position zone. Drawing colours are muted-realistic; the dot
## beside each label carries the position palette so the legend stays unified.
mdef <- MEASURES %>%
  mutate(n = map_int(var, ~ sum(FG[[.x]] == 1, na.rm = TRUE)),
         pct = round(100 * n / nrow(FG)))

## anchors: where each measure's element sits on the drawing
anch <- tibble::tribble(
  ~var,         ~ax,  ~ay,
  "m_greenroof", 2.42, 4.68,
  "m_facade",    3.72, 3.30,
  "m_barrel",    4.16, 2.35,
  "m_disconn",   1.02, 2.30,
  "m_raingard",  5.35, 1.95,
  "m_infiltr",   5.35, 1.05,
  "m_depave",    6.55, 1.86,
  "m_trees",     7.55, 3.45,
  "m_planting",  0.52, 1.95,
  "m_turf",      2.85, 1.60,
  "m_pond",      6.05, 1.42)

## LABEL PLACEMENT. Previously every label sat in one right-hand column ordered
## by drawing height, so an element low on the LEFT of the scene (e.g. downspout
## at x = 1.0) received a label high in the column and its leader line crossed
## the entire figure. Labels are now split into a left and a right stack by which
## half of the drawing the element sits in, and within each stack are ordered by
## height. A leader therefore travels only to its own side and never crosses the
## scene. Leaders are drawn as elbows (out horizontally, then to the label) so
## they read as pointers rather than diagonals.
SPLIT_X <- 4.30   # drawing mid-line
SCENE_L <- 0.00   # scene edges; elbows turn OUTSIDE these so no line crosses art
SCENE_R <- 8.15
lab <- mdef %>% left_join(anch, by = "var") %>%
  mutate(side = if_else(ax < SPLIT_X, "L", "R")) %>%
  group_by(side) %>%
  arrange(desc(ay), .by_group = TRUE) %>%
  mutate(ly = seq(6.10, by = -(5.55 / max(n() - 1, 1)), length.out = n()),
         ## elbow turns just OUTSIDE the scene, then runs vertically in the
         ## margin to the label row -- the vertical leg never overlays the drawing
         elbow = if_else(side == "L", SCENE_L - 0.28, SCENE_R + 0.28),
         lx    = if_else(side == "L", SCENE_L - 0.60, SCENE_R + 0.60),
         hj    = if_else(side == "L", 1, 0)) %>%
  ungroup() %>%
  mutate(head = sprintf("%s — %d%%", measure, pct))

leaf <- function(x, y, r = .085, col = "#5E9E6C")
  annotate("point", x = x, y = y, size = r * 28, colour = col, alpha = .85)

## v33 RENUMBERED, not changed. This figure was RETIRED from the main text
## earlier (author: "not sure what figure 7 is doing at all") and its content
## became a table; it is still written for the supplement. The weakness-
## clustering figure now occupies main-text slot 8, so this one moves to S14
## to stop two files both claiming "figure 7". Its content is untouched.
p_a2b <- ggplot() +
  ## sky, soil, ground line
  annotate("rect", xmin = 0, xmax = 8.15, ymin = 1.8, ymax = 6.45, fill = "#EDF3F7") +
  annotate("rect", xmin = 0, xmax = 8.15, ymin = 0.35, ymax = 1.8, fill = "#E4D6BE") +
  annotate("rect", xmin = 0, xmax = 8.15, ymin = 1.72, ymax = 1.8, fill = "#C9B893") +
  ## ---- house ----
  annotate("rect", xmin = 1.25, xmax = 3.6, ymin = 1.8, ymax = 4.35,
           fill = "#FDFCF8", colour = "grey40", linewidth = .5) +
  annotate("rect", xmin = 1.75, xmax = 2.15, ymin = 1.8, ymax = 2.75,
           fill = "#B9C4CC", colour = "grey45", linewidth = .35) +           # door
  annotate("rect", xmin = 2.55, xmax = 3.2, ymin = 3.0, ymax = 3.6,
           fill = "#DCE8F0", colour = "grey45", linewidth = .35) +           # window
  ## FLAT ROOF with a slight parapet. Extensive green roofs -- the type these
  ## programmes fund -- are installed on flat or low-slope roofs, not on steep
  ## pitches, so the schematic shows a flat deck.
  annotate("rect", xmin = 1.05, xmax = 3.80, ymin = 4.35, ymax = 4.52,
           fill = "#CFC6BA", colour = "grey40", linewidth = .5) +            # roof deck
  annotate("segment", x = 1.05, xend = 1.05, y = 4.52, yend = 4.72,
           colour = "grey40", linewidth = .5) +                              # parapet L
  annotate("segment", x = 3.80, xend = 3.80, y = 4.52, yend = 4.72,
           colour = "grey40", linewidth = .5) +                              # parapet R
  ## 1 GREEN ROOF: vegetated substrate layer across the flat deck
  annotate("rect", xmin = 1.10, xmax = 3.75, ymin = 4.52, ymax = 4.68,
           fill = "#79A96C", colour = "#4F7B45", linewidth = .4) +
  leaf(1.55, 4.74) + leaf(2.05, 4.78) + leaf(2.60, 4.74) + leaf(3.20, 4.78) +
  ## 2 GREEN FACADE: climbing greenery on the right wall
  annotate("rect", xmin = 3.52, xmax = 3.62, ymin = 1.8, ymax = 4.1,
           fill = "#8FB98A", colour = NA) +
  leaf(3.57, 2.3, .075) + leaf(3.62, 2.9, .07) + leaf(3.55, 3.5, .075) + leaf(3.63, 3.95, .065) +
  ## 3 RAIN BARREL at the right downspout
  annotate("segment", x = 3.78, xend = 4.16, y = 4.35, yend = 4.35, colour = "grey35", linewidth = .8) +
  annotate("segment", x = 4.16, xend = 4.16, y = 4.35, yend = 2.72, colour = "grey35", linewidth = .8) +
  annotate("rect", xmin = 3.98, xmax = 4.42, ymin = 1.8, ymax = 2.62,
           fill = "#D9A441", colour = "#8a6a2a", linewidth = .45) +
  annotate("segment", x = 3.98, xend = 4.42, y = 2.44, yend = 2.44, colour = "#8a6a2a", linewidth = .3) +
  ## 4 DOWNSPOUT DISCONNECTION on the left, elbowed onto the planting bed
  annotate("segment", x = 1.12, xend = 1.12, y = 4.35, yend = 2.1, colour = "grey35", linewidth = .8) +
  annotate("segment", x = 1.12, xend = 0.72, y = 2.1, yend = 1.92, colour = "grey35", linewidth = .8,
           arrow = arrow(length = unit(.13, "cm"), type = "closed")) +
  annotate("segment", x = 1.34, xend = 1.34, y = 4.35, yend = 4.05, colour = "grey60",
           linewidth = .55, linetype = "13") +   # the sewer stub it no longer feeds
  ## 5 NATIVE / DROUGHT PLANTING bed left of the house
  annotate("rect", xmin = 0.18, xmax = 0.95, ymin = 1.8, ymax = 1.94, fill = "#9CBF74", colour = NA) +
  annotate("segment", x = c(.3,.5,.7,.88), xend = c(.3,.5,.7,.88), y = 1.94, yend = 2.16,
           colour = "#4F7B45", linewidth = .5) +
  annotate("point", x = c(.3,.5,.7,.88), y = 2.2, size = 1.3,
           colour = c("#C86BA8","#D9A441","#C86BA8","#7F5FA8")) +
  ## 6 TURF REPLACEMENT strip in the front yard: mown half -> planted half
  annotate("rect", xmin = 2.35, xmax = 2.85, ymin = 1.8, ymax = 1.9, fill = "#BFD98F", colour = NA) +
  annotate("segment", x = seq(2.4, 2.8, by = .1), xend = seq(2.4, 2.8, by = .1),
           y = 1.8, yend = 1.9, colour = "#96B36A", linewidth = .35) +
  annotate("rect", xmin = 2.9, xmax = 3.4, ymin = 1.8, ymax = 1.92, fill = "#9CBF74", colour = NA) +
  annotate("point", x = c(3.0, 3.15, 3.3), y = 2.0, size = 1.1,
           colour = c("#D9A441", "#C86BA8", "#7F5FA8")) +
  annotate("segment", x = 2.86, xend = 2.9, y = 1.98, yend = 1.98,
           arrow = arrow(length = unit(.09, "cm")), colour = "grey35", linewidth = .4) +
  ## 7 RAIN GARDEN: planted depression fed by the barrel overflow
  annotate("polygon", x = c(4.85, 5.05, 5.65, 5.85), y = c(1.8, 1.62, 1.62, 1.8),
           fill = "#7FA86B", colour = "#4F7B45", linewidth = .4) +
  annotate("segment", x = c(5.05, 5.3, 5.55), xend = c(5.05, 5.3, 5.55), y = 1.66, yend = 1.95,
           colour = "#4F7B45", linewidth = .55) +
  annotate("point", x = c(5.05, 5.3, 5.55), y = 2.0, size = 1.2,
           colour = c("#C86BA8", "#7F5FA8", "#D9A441")) +
  annotate("segment", x = 4.42, xend = 4.85, y = 1.86, yend = 1.78,
           colour = PAL$blue, linewidth = .5, arrow = arrow(length = unit(.1, "cm"))) +
  ## 8 INFILTRATION: soakaway beneath the rain garden, water arrows downwards
  annotate("rect", xmin = 4.95, xmax = 5.75, ymin = 0.72, ymax = 1.28,
           fill = "#9ED0C6", colour = "#3E8377", linewidth = .45, linetype = "22") +
  annotate("point", x = seq(5.05, 5.65, length.out = 5), y = rep(c(1.12, .92), 3)[1:5],
           size = .9, colour = "#3E8377") +
  annotate("segment", x = c(5.1, 5.35, 5.6), xend = c(5.1, 5.35, 5.6), y = .70, yend = .48,
           colour = "#3E8377", linewidth = .4, arrow = arrow(length = unit(.09, "cm"))) +
  ## 8b POND / WETLAND / BIOSWALE: shallow vegetated water feature
  annotate("polygon", x = c(5.85, 6.02, 6.28, 6.45), y = c(1.42, 1.30, 1.30, 1.42),
           fill = "#74add1", colour = "#4575b4", linewidth = .4) +
  annotate("segment", x = c(5.95, 6.15, 6.35), xend = c(5.95, 6.15, 6.35),
           y = 1.42, yend = 1.62, colour = "#4575b4", linewidth = .45) +
  annotate("point", x = c(5.95, 6.15, 6.35), y = 1.66, size = 1.1, colour = "#313695") +
  ## 9 DEPAVING / PERMEABLE: paver strip with open joints
  annotate("rect", xmin = 6.15, xmax = 7.05, ymin = 1.8, ymax = 1.92, fill = "#CDBFA6", colour = NA) +
  annotate("rect", xmin = seq(6.18, 6.9, by = .18), xmax = seq(6.18, 6.9, by = .18) + .13,
           ymin = 1.8, ymax = 1.92, fill = "#B7B2A5", colour = "grey55", linewidth = .25) +
  annotate("segment", x = c(6.33, 6.6, 6.87), xend = c(6.33, 6.6, 6.87), y = 1.78, yend = 1.58,
           colour = "#3E8377", linewidth = .4, arrow = arrow(length = unit(.08, "cm"))) +
  ## 10 TREE with prepared pit
  annotate("rect", xmin = 7.32, xmax = 7.78, ymin = 1.68, ymax = 1.8,
           fill = "#8a6a2a", alpha = .35, colour = "#8a6a2a", linewidth = .3) +
  annotate("rect", xmin = 7.48, xmax = 7.62, ymin = 1.8, ymax = 2.85, fill = "#6b4f2a", colour = NA) +
  annotate("point", x = 7.55, y = 3.35, size = 17, colour = "#4F7B45", alpha = .9) +
  annotate("point", x = 7.28, y = 3.05, size = 11, colour = "#5E9E6C", alpha = .9) +
  annotate("point", x = 7.85, y = 3.05, size = 11, colour = "#5E9E6C", alpha = .9) +
  ## ---- leader lines and the labelled column ----
  ## Leaders as short stubs + a margin run. A dot marks the element itself, a
  ## SHORT stub leaves it, then the line runs in the OUTSIDE margin to the label.
  ## Nothing long is drawn across the artwork.
  geom_point(data = lab, aes(x = ax, y = ay), size = .9, colour = "grey45") +
  geom_segment(data = lab, aes(x = ax, y = ay,
                               xend = ax + if_else(side == "L", -0.22, 0.22), yend = ay),
               colour = "grey62", linewidth = .28) +
  ## diagonal from the stub end straight to the label dot, kept faint
  geom_segment(data = lab, aes(x = ax + if_else(side == "L", -0.22, 0.22), y = ay,
                               xend = lx, yend = ly),
               colour = "grey78", linewidth = .22) +
  geom_point(data = lab, aes(x = lx, y = ly, colour = position), size = 2.5) +
  geom_text(data = lab, aes(x = lx + if_else(side == "L", -0.17, 0.17),
                            y = ly + .075, label = head, hjust = hj),
            size = 2.6, fontface = "bold", colour = PAL$ink) +
  geom_text(data = lab, aes(x = lx + if_else(side == "L", -0.17, 0.17),
                            y = ly - .155, label = str_wrap(definition, 44), hjust = hj),
            vjust = 1, size = 1.85, colour = "grey38", lineheight = .92) +
  scale_colour_manual(values = PAL_ECO, name = "Nature-based family",
                      guide = guide_legend(override.aes = list(size = 3), nrow = 2)) +
  coord_cartesian(xlim = c(-6.4, 14.6), ylim = c(0.2, 6.6), expand = FALSE) +
  labs(title = "a. The catalogued measure families, drawn where each member sits",
       subtitle = sprintf("Each element is one measure with its share of the %d records and a one-line definition, so no term stands unexplained.", nrow(A))) +
  theme_void(base_size = 9) + TITLE() +
  theme(legend.position = "bottom", legend.title = element_text(size = 7, face = "bold"),
        legend.text = element_text(size = 6.5))



## Figure 4 is assembled AFTER p_a5 exists (see "FIGURE 4 ASSEMBLY" below):
## its right-hand panel is the country x measure matrix, which is built later in
## this file. The measure-prevalence table is written here, next to its data.
save_tab(mprev %>% select(measure, tier, position, n, pct, definition), "14_measure_prevalence")

# =============================================================================
# FIGURE A3 | The support instruments, by family
# =============================================================================
iprev <- INSTRUMENTS %>%
  mutate(n = map_int(var, ~ if (.x %in% names(FG)) sum(FG[[.x]] == 1, na.rm = TRUE) else 0L),
         pct = n / nrow(FG)) %>% arrange(desc(n))
save_tab(iprev, "14_instrument_prevalence")

## Panel A: instrument prevalence grouped by family
ifam_tot <- purrr::map_dfr(names(PAL_INSTR), function(g) {
  vars <- INSTRUMENTS$var[INSTRUMENTS$family == g]; vars <- vars[vars %in% names(FG)]
  tibble(family = g, n = sum(rowSums(FG[, vars, drop = FALSE] == 1, na.rm = TRUE) > 0))
}) %>% mutate(pct = n / nrow(FG), share = n / sum(n))
save_tab(ifam_tot, "14_a3_instrument_family_totals")

## Panel A1: composition donut -- how the instrument mix divides across families
don <- ifam_tot %>%
  mutate(family = factor(family, levels = names(PAL_INSTR))) %>%
  arrange(family) %>%
  mutate(ymax = cumsum(share), ymin = lag(ymax, default = 0),
         mid = (ymax + ymin) / 2,
         lab = sprintf("%s\n%s", family, percent(share, .1)))
p_a3a1 <- ggplot(don) +
  geom_rect(aes(xmin = 2.55, xmax = 4, ymin = ymin, ymax = ymax, fill = family),
            colour = "white", linewidth = .7) +
  ggrepel::geom_text_repel(aes(x = 4.15, y = mid, label = lab), size = 2.4,
                           lineheight = .95, direction = "y", box.padding = .25,
                           segment.colour = "grey65", segment.size = .3,
                           min.segment.length = 0, seed = 3) +
  annotate("text", x = 0, y = 0, size = 2.9, fontface = "bold", colour = PAL$ink,
           label = sprintf("%d\nfamily uses\nacross %d\nprogrammes",
                           sum(ifam_tot$n), nrow(FG))) +
  scale_fill_manual(values = PAL_INSTR, guide = "none") +
  coord_polar(theta = "y", clip = "off") + xlim(c(0, 5.6)) +
  labs(title = "a. Share of family totals") +
  theme_void(base_size = 8) +
  theme(plot.title = element_text(size = 8.4, face = "bold", colour = PAL$ink,
                                  margin = margin(b = 5)),
        plot.margin = margin(6, 26, 6, 26))

## Panel A2: trellis of the specific instruments, one uniform bar height per row
iprev2 <- iprev %>%
  mutate(family = factor(family, levels = names(PAL_INSTR))) %>%
  group_by(family) %>% arrange(n, .by_group = TRUE) %>%
  mutate(ilab = factor(instrument, levels = unique(instrument))) %>% ungroup() %>%
  left_join(ifam_tot %>% select(family, fam_n = n), by = "family") %>%
  mutate(fam_lab = sprintf("%s  (%d)", family, fam_n),
         fam_lab = factor(fam_lab, levels = unique(fam_lab[order(family)])))
save_tab(iprev2 %>% select(family, instrument, n, pct), "14_a3_instruments_hierarchy")
p_a3a2 <- ggplot(iprev2, aes(n, ilab, fill = family)) +
  geom_col(width = .62) +
  geom_text(aes(label = sprintf("%d (%s)", n, percent(pct, 1))), hjust = -.08,
            size = 2.15, colour = "grey25") +
  facet_grid(fam_lab ~ ., scales = "free_y", space = "free_y",
             labeller = labeller(fam_lab = label_wrap_gen(12))) +
  scale_fill_manual(values = PAL_INSTR, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, .30))) +
  labs(title = "b. The specific instruments inside each family",
       subtitle = WRAP("Every bar is the same height, so length alone carries the count. The family total in each strip counts programmes offering any member, and is smaller than the sum of its members because programmes combine instruments."),
       x = "Programmes offering the instrument", y = NULL) +
  theme_nbs(7.6) + theme(strip.text.y = element_text(angle = 0))

p_a3a <- p_a3a1 / p_a3a2 + plot_layout(heights = c(1.15, 1.35))

## Panel B: an UpSet view of instrument-FAMILY combinations. Sets, not a
## sequence, so an UpSet is the right device (a Sankey would imply flow).
fam_of <- function(fam) {
  vars <- INSTRUMENTS$var[INSTRUMENTS$family == fam]
  vars <- vars[vars %in% names(FG)]
  as.integer(rowSums(FG[, vars, drop = FALSE] == 1, na.rm = TRUE) > 0)
}
famM <- sapply(names(PAL_INSTR), fam_of)
FG$n_instr_families <- rowSums(famM)
combo <- as_tibble(famM) %>%
  mutate(id = row_number()) %>%
  pivot_longer(-id, names_to = "family", values_to = "on") %>%
  group_by(id) %>%
  summarise(key = paste(sort(family[on == 1]), collapse = "|"), .groups = "drop") %>%
  count(key, name = "n") %>%
  mutate(key = ifelse(key == "", "None documented", key)) %>%
  arrange(desc(n)) %>%
  mutate(rank = row_number())
save_tab(combo, "14_instrument_family_upset")

grid <- combo %>% tidyr::separate_rows(key, sep = "\\|") %>%
  rename(family = key) %>% filter(family != "None documented") %>% mutate(on = 1) %>%
  tidyr::complete(rank = combo$rank, family = names(PAL_INSTR), fill = list(on = 0)) %>%
  left_join(combo %>% select(rank, n), by = "rank") %>%
  mutate(family = factor(family, levels = rev(names(PAL_INSTR))))

p_a3b_top <- ggplot(combo, aes(factor(rank), n)) +
  geom_col(fill = PAL$softblue, width = .68) +
  geom_text(aes(label = n), vjust = -.35, size = 2.2) +
  scale_y_continuous(expand = expansion(mult = c(0, .16))) +
  labs(title = "c. Every observed family combination",
       subtitle = WRAP("Each column is one complete combination -- from a single family to many at once, not pairs. Read a column downwards: filled dots show every family that combination contains; the bar above is how many programmes use exactly that combination. All observed combinations are shown."),
       x = NULL, y = "Programmes") +
  theme_nbs(8) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        plot.margin = margin(7, 9, 0, 7))

p_a3b_grid <- ggplot(grid, aes(factor(rank), family)) +
  geom_point(aes(colour = ifelse(on == 1, as.character(family), "off"),
                 size = factor(on)), show.legend = FALSE) +
  geom_line(data = grid %>% filter(on == 1), aes(group = rank), colour = "grey40", linewidth = .35) +
  scale_colour_manual(values = c(PAL_INSTR, off = "grey88")) +
  scale_size_manual(values = c(`0` = 1.3, `1` = 2.7)) +
  labs(x = NULL, y = NULL) +
  theme_nbs(8) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(), plot.margin = margin(0, 9, 7, 7))

p_a3b <- p_a3b_top / p_a3b_grid + plot_layout(heights = c(1, .62))

fig_a3 <- p_a3a | p_a3b
fig_a3 <- fig_a3 + plot_layout(widths = c(1.3, 1))
## saved as part of Figure 5 (with the by-country panel) after p_a5 is built

# =============================================================================
# FIGURE A4 | The urban implementation context frame + context overlays
# =============================================================================
## Panel A: the 3x3 frame, counts, with hazard overlay
cell <- FG %>% filter(built_form != "Unknown", tenure_class != "Unknown") %>%
  count(built_form, tenure_class, name = "n")
p_a4a <- cell %>%
  ggplot(aes(tenure_class, built_form, fill = n)) +
  geom_tile(colour = "white", linewidth = 1) +
  geom_text(aes(label = n), size = 3.2, fontface = "bold", colour = PAL$ink) +
  scale_fill_seq("Cities", limits = c(0, max(cell$n))) +
  labs(title = "a. The urban implementation context frame",
       ## THIS SUBTITLE OVERPRINTED THE PANEL BELOW IT.
       ## WRAP() defaults to 200 characters, which is right for a full-width
       ## figure and far too wide for the HALF-width column this panel occupies
       ## in figure S4. The text ran past its column and rendered on top of the
       ## next panel's subtitle ("Weakly correlated Sharetofchvhole..."). Wrapped
       ## to the column it actually sits in.
       subtitle = WRAP("Two established dimensions, not a discovered typology: built form (how interventions physically fit, from density) against tenure (who can authorise them, from renter share). Weakly correlated here (|r| <= 0.28), so each carries independent information.", 92),
       x = "Tenure (Context)", y = "Built form (Context)") +
  theme_nbs(8.5)

## Panel B: hazard exposure across the frame (the overlay, not a third axis)
## Multi-response: each specific hazard is its own indicator, so a
## flood-and-heat programme counts under BOTH -- nothing hides in a residual
## "multiple" category. Shares within a cell can exceed 100% by design.
## Four distinct rows: flooding folded into one (fluvial, pluvial/stormwater and
## coastal are one hazard family), then heat, then drought, then biodiversity as
## the standing non-hazard objective. Multi-response: a record counts in every
## row it documents.
HAZ_SPEC <- list(Floods = quote(hx_flood == 1 | hx_coastal == 1),
                 Heat = quote(hx_heat == 1),
                 Drought = quote(hx_drought == 1),
                 `Biodiversity loss` = quote(obj_biodiv == 1))
haz_cell <- purrr::map_dfr(names(HAZ_SPEC), function(h) {
  FG %>% filter(built_form != "Unknown", tenure_class != "Unknown") %>%
    mutate(hit = purrr::map_lgl(seq_len(n()), ~ isTRUE(eval(HAZ_SPEC[[h]],
                                envir = as.list(cur_data_all()[.x, ]))))) %>%
    group_by(built_form, tenure_class) %>%
    summarise(p = mean(hit, na.rm = TRUE), n = n(), .groups = "drop") %>%
    mutate(hazard = h)
}) %>%
  bind_rows(FG %>% filter(built_form != "Unknown", tenure_class != "Unknown") %>%
              group_by(built_form, tenure_class) %>%
              summarise(p = mean(hx_flood != 1 & hx_coastal != 1 & hx_heat != 1 &
                                 hx_drought != 1 & obj_biodiv != 1, na.rm = TRUE),
                        n = n(), .groups = "drop") %>%
              mutate(hazard = "None documented")) %>%
  mutate(hazard = factor(hazard, levels = c(names(HAZ_SPEC), "None documented")))
save_tab(haz_cell, "14_a4_hazard_multiresponse_by_cell")
## Canonical hazard fill from 00b_design.R -- no local re-definition.
PAL_HAZSPEC <- HAZ_FILL
p_a4b <- haz_cell %>%
  ggplot(aes(p, fct_rev(hazard), fill = hazard)) +
  geom_col(width = .74) +
  geom_text(aes(label = ifelse(p >= .06, percent(p, 1), "")), hjust = 1.05, size = 1.8, colour = "white") +
  facet_grid(fct_rev(built_form) ~ tenure_class, switch = "y") +
  scale_fill_manual(values = PAL_HAZSPEC, name = "Documented pressure or objective (Context; multi-response)") +
  scale_x_continuous(labels = percent_format(), breaks = c(0, .5, 1), limits = c(0, 1)) +
  labs(y = "Documented hazard or objective (Context; multi-response)",
       title = "a. The specific hazards each context type faces",
       subtitle = WRAP("MULTI-RESPONSE: a flood-and-heat programme counts under both bars, so heat is never hidden inside a 'multiple' category and cell shares can exceed 100%. Flooding folds fluvial, pluvial/stormwater and coastal into one family; biodiversity is the standing non-hazard objective and is labelled as such, not as a hazard."),
       x = "Share of the cell's programmes citing it",
       y = "Documented hazard or objective (bars in legend order, top to bottom)") +
  theme_nbs(7.5) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        strip.placement = "outside", legend.position = "bottom")

## Panel C: the other context variables, so nothing stands unexplained
ctx_long <- bind_rows(
  FG %>% transmute(dim = "City size (population)",
                   level = cut(pop, c(-Inf, 5e4, 2.5e5, 1e6, Inf),
                               labels = c("< 50k", "50-250k", "250k-1M", "1M +"))),
  FG %>% transmute(dim = "Density", level = density_band),
  FG %>% transmute(dim = "Climate", level = climate_grp),
  FG %>% transmute(dim = "Tenure", level = tenure_class),
  FG %>% transmute(dim = "Built form", level = built_form)) %>%
  filter(!is.na(level), level != "Unknown", level != "unknown") %>%
  count(dim, level) %>% group_by(dim) %>% mutate(p = n / sum(n)) %>% ungroup()
## A4c by COUNTRY (28 is readable): distribution of the continuous context
## variables behind the frame, one dot per city record, median tick per country
ctx_cty <- FG %>%
  filter(!is.na(renter_pct) | !is.na(landuse_n) | !is.na(density)) %>%
  transmute(country_mod = country, region_mod,
            ## POPULATION LEAVES THIS PANEL; LAND USE TAKES ITS PLACE. The
            ## author's instruction was that the MAP already carries population by
            ## dot size, so this panel should show density, LAND USE and tenure --
            ## the three context axes. Land use is CATEGORICAL (A-5's five coded
            ## classes), so it cannot ride the continuous dot-strip with density
            ## and renter share; it gets its own stacked column, built below.
            ## Population is not lost: it sizes the map and enters every model.
            `Renter share (%)` = renter_pct,
            `Density (per km2, log10)` = log10(pmax(density, 10))) %>%
  pivot_longer(-(1:2), names_to = "dim", values_to = "val") %>%
  filter(!is.na(val))
cty_ord <- ctx_cty %>% count(region_mod, country_mod) %>%
  mutate(region_mod = factor(region_mod, levels = c(setdiff(sort(unique(region_mod)), "Other regions"), "Other regions"))) %>%
  arrange(region_mod, desc(n)) %>%
  distinct(country_mod, .keep_all = TRUE)
med_cty <- ctx_cty %>% group_by(country_mod, dim) %>% summarise(m = median(val), .groups = "drop")
## v34b: ONE ENCODING FOR ALL THREE CONTEXT AXES, on the author's reference.
##
## The first attempt put a stacked bar beside two dot strips, which asked the
## reader to switch grammar mid-panel. Everything is now a DOT ON A HORIZONTAL
## SCALE with one country per row, small multiples across the three axes -- the
## layout of the reference image.
##
## One honest difference remains and is stated in the subtitle rather than
## hidden: density and renter share are per-RECORD quantities, so those panels
## carry one pale dot per record with a dark tick at the country median; land use
## is a per-record MULTI-SELECT with no numeric value, so its panels carry the
## country's SHARE of records documenting each class, as a single dark dot on a
## 0-100% scale. Same geometry, same axis, same rows; the unit differs and the
## panel says so.
lu_share <- FG %>%
  filter(!is.na(country)) %>%
  group_by(country_mod = country) %>%
  summarise(`High-rise residential`   = mean(lu_highrise   == 1, na.rm = TRUE),
            `Mixed-use`               = mean(lu_mixed      == 1, na.rm = TRUE),
            `Low-rise residential`    = mean(lu_lowrise    == 1, na.rm = TRUE),
            `Industrial / peri-urban` = mean(lu_industrial == 1, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_longer(-country_mod, names_to = "dim", values_to = "val") %>%
  mutate(val = 100 * val, kind = "share")

## The two continuous axes, on the same country rows, kept as record-level dots
## so the spread inside each country stays visible.
ctx_all <- bind_rows(
  ctx_cty %>% mutate(kind = "record"),
  lu_share %>% mutate(region_mod = cty_ord$region_mod[match(country_mod, cty_ord$country_mod)])) %>%
  filter(!is.na(country_mod)) %>%
  mutate(dim = factor(dim, levels = c("Density (per km2, log10)", "Renter share (%)",
                                      "Low-rise residential", "Mixed-use",
                                      "High-rise residential", "Industrial / peri-urban")),
         country_mod = factor(country_mod, levels = rev(cty_ord$country_mod)))

med_all <- ctx_all %>% filter(kind == "record") %>%
  group_by(country_mod, dim) %>% summarise(m = median(val), .groups = "drop")

## v35b: MULTI-PANEL CLEVELAND DOT PLOT -- CITY-LEVEL, three framed boxes.
##
## Two corrections to the previous attempt, both from the author's own spec:
##
##  1. CITY LEVEL, NOT COUNTRY AVERAGES. The previous version collapsed each
##     country to its median, which is exactly the "country average" the author
##     ruled out. The spec is explicit that the layout should keep "all raw
##     program data visible", and its sketch shows several dots on every country
##     row. So the mark is ONE DOT PER PROGRAMME RECORD (its city), and the
##     country row is only the grouping.
##
##  2. ONE MARK FOR ALL THREE DOMAINS. That was the author's objection to the
##     earlier figure -- a cloud on two axes and a bar or share dot on the third.
##     Land use (A-5) is a per-record multi-select with no numeric value, so it
##     cannot ride a continuous scale; it instead becomes FOUR CATEGORICAL
##     COLUMNS in its own box, exactly as the spec draws it, with one dot per
##     record documenting that class. Same mark, same rows, three boxes.
ctx_pt <- ctx_cty %>%
  filter(!is.na(country_mod)) %>%
  mutate(panel = factor(ifelse(grepl("^Density", dim),
                               "Urban density (per km2, log10)",
                               "Renter tenure (% of households)"),
                        levels = c("Urban density (per km2, log10)",
                                   "Renter tenure (% of households)")),
         country_mod = factor(country_mod, levels = rev(cty_ord$country_mod)))

## Land use at RECORD level: one row per record x documented class.
LU_SHORT <- c(lu_lowrise = "Low-rise", lu_mixed = "Mixed-use",
              lu_highrise = "High-rise", lu_industrial = "Industrial")
ctx_lu_pt <- FG %>%
  filter(!is.na(country)) %>%
  select(country_mod = country, region_mod, all_of(names(LU_SHORT))) %>%
  pivot_longer(all_of(names(LU_SHORT)), names_to = "luvar", values_to = "flag") %>%
  filter(flag == 1) %>%
  mutate(class = factor(unname(LU_SHORT[luvar]), levels = unname(LU_SHORT)),
         panel = factor("Land-use mix (A-5; one dot per record)"),
         country_mod = factor(country_mod, levels = rev(cty_ord$country_mod))) %>%
  filter(!is.na(country_mod))

.dotbase <- function(p) p +
  scale_colour_manual(values = PAL_REGION, name = NULL,
                      guide = guide_legend(override.aes = list(size = 2.6, alpha = 1))) +
  labs(x = NULL, y = NULL) +
  theme_nbs(6.8) +
  theme(legend.position = "none",
        ## the framed box per variable domain, from the author's spec
        panel.border = element_rect(colour = "grey70", fill = NA, linewidth = .35),
        panel.grid.major.x = element_line(colour = "grey93", linewidth = .22),
        panel.grid.major.y = element_line(colour = "grey96", linewidth = .18),
        strip.text = element_text(size = 6.4, face = "bold"))

## FIGURE 3b's MARKS ARE SAVED AS A TABLE. Every other main-text panel could
## be redrawn from output/tables; this one could not, because the density, tenure
## and land-use points lived only in ctx_pt / ctx_lu_pt inside this script. A panel
## whose data exist nowhere but in the plotting code cannot be checked by the
## manuscript verifier, cannot be handed to anyone redrawing the figure, and cannot
## be reconciled against the corpus. One row per programme record per dimension.
save_tab(bind_rows(
  ctx_pt %>% transmute(country = country_mod, region = region_mod,
                       dimension = as.character(dim), value = val,
                       scale = "continuous"),
  ctx_lu_pt %>% transmute(country = country_mod, region = region_mod,
                          dimension = "Land-use mix (A-5)", value = NA_real_,
                          scale = "categorical", class = as.character(class))),
  "14_a4_context_points_by_record")

p_ctx_num <- .dotbase(
  ggplot(ctx_pt, aes(val, country_mod, colour = region_mod)) +
    geom_point(size = .55, alpha = .5, na.rm = TRUE,
               position = position_jitter(height = .22, width = 0, seed = 1)) +
    facet_wrap(~ panel, scales = "free_x", nrow = 1))

p_ctx_lu <- .dotbase(
  ggplot(ctx_lu_pt, aes(class, country_mod, colour = region_mod)) +
    geom_point(size = .55, alpha = .5, na.rm = TRUE,
               position = position_jitter(width = .17, height = .22, seed = 1)) +
    facet_wrap(~ panel, nrow = 1)) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 32, hjust = 1, size = 5.8))

p_a4c <- (p_ctx_num | p_ctx_lu) +
  plot_layout(widths = c(2, 1.15)) +
  plot_annotation(
    title = "b. The urban context behind the corpus, city by city: density, tenure and land use",
    subtitle = WRAP(paste(
      "THREE framed panels, one per variable domain, sharing the country axis on the left. The mark is the same in all",
      "three: ONE DOT PER PROGRAMME RECORD, at its own city -- these are not country averages, and the spread within a",
      "country row is the real variation across its cities. Rows are the 28 coded countries grouped by region, 'Other",
      "regions' last. Density and renter tenure are per-record quantities on a continuous scale; land use (A-5) is a",
      "per-record multi-select with no numeric value, so its four classes are columns and a record documenting three",
      "classes appears in three of them. Dots are jittered vertically so overlapping cities stay countable.",
      "Population is not repeated here; the map above sizes by it. These variables enter every model directly."))) 

## the two halves are assembled as one grob so the shared country axis survives
## into the parent stack, which patchwork otherwise re-lays-out per panel.
p_a4c <- wrap_elements(full = patchwork::patchworkGrob(p_a4c))

## FIGURE 3 NO LONGER TAKES THE WHOLE MODULE DOWN WITH IT.
## p_a1 is built inside `if (HAS("maps"))` at the top of this file, but fig3
## referenced it unconditionally. On a machine without the `maps` package the
## module died with "object 'p_a1' not found" -- and because it dies mid-file,
## EVERY figure after this point was lost too, not just the map. A missing
## optional plotting package must cost one panel, not the figure set.
if (exists("p_a1")) {
  fig3 <- p_a1 / p_a4c + plot_layout(heights = c(1.5, 1, 1.2)) +
    plot_annotation(title = "Figure 3. The corpus: where its cities are, who delivers there, and the urban context behind them",
                    theme = TITLE())
} else {
  msg("NOTE  figure 3: the `maps` package is absent, so the geography panel is omitted; the context panel is drawn alone")
  fig3 <- p_a4c +
    plot_annotation(title = "Figure 3. The corpus: who delivers, and the urban context behind them (geography panel omitted: `maps` not installed)",
                    theme = TITLE())
}
save_fig(fig3, "fig03_geography_delivery", 18, 17)

## make the typology available to later modules
CITY_TYPOLOGY <<- FG %>% select(case_id, built_form, tenure_class, hazard_family, city_type)
save_tab(FG %>% count(built_form, tenure_class, hazard_family), "14_city_typology_counts")

# =============================================================================
# FIGURE A5 | The measures ACROSS the countries (combines the old country bar
# and measure bar into one analytical view; ALL 28 countries, none dropped)
# =============================================================================
## region sequence taken from the reference figure -- North America,
## Western Europe, Central/Eastern Europe, East Asia, then Other regions. The
## previous alphabetical sort put Central/Eastern Europe before East Asia and
## Western Europe last, so the strip read in a different order from the figure
## the author asked me to reproduce.
.REG4B <- c("North America", "Western Europe", "Central/Eastern Europe",
            "East Asia", "Other regions")
cty <- FG %>% count(country, region_mod, name = "n_cty") %>%
  mutate(region_mod = factor(region_mod,
    levels = c(intersect(.REG4B, unique(region_mod)),
               setdiff(unique(region_mod), .REG4B))))
mc <- MEASURES %>% select(var, measure, eco_group, tier) %>%
  crossing(cty) %>%
  rowwise() %>%
  mutate(n_m = sum(FG[[var]][FG$country == country] == 1, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(share = n_m / n_cty)
save_tab(mc %>% select(country, region_mod, n_cty, measure, tier, n_m, share),
         "14_measures_by_country")

## DENOMINATOR IS RECORDS THAT NAME A MEASURE. 20 records in the corpus
## document no measure at all (the programme page states a rebate without
## naming what it covers), so a share over ALL records understated every
## country and made New Zealand -- whose single record is one of those -- read
## as a programme that supports nothing. The label now carries both counts and
## the share denominator is n_meas_rec.
mrec <- FG %>% mutate(.anym = rowSums(across(all_of(MEASURES$var)) == 1, na.rm = TRUE) > 0) %>%
  group_by(country) %>% summarise(n_meas_rec = sum(.anym), .groups = "drop")
cty <- cty %>% left_join(mrec, by = "country")
mc  <- mc %>% select(-share) %>% left_join(mrec, by = "country") %>%
  mutate(share = ifelse(n_meas_rec > 0, n_m / n_meas_rec, NA_real_))
## SHORT country labels, matching the reference figure the author supplied:
## "United States (304)" -- the name and the record count, nothing more. The long
## form ("304 of 320 rec name a measure") was written as a caveat, but it made the
## column headers three times taller than the reference and unreadable at 90
## degrees. The caveat belongs in the subtitle, where it now sits; the marks
## already distinguish the two zero cases (open square = no measure-naming
## record, cross = records exist but none supports this measure).
mklab <- function(country, n_cty, n_meas_rec)
  sprintf("%s (%d)", country, n_cty)
cty_ord <- cty %>% arrange(region_mod, desc(n_cty)) %>%
  mutate(lab = mklab(country, n_cty, n_meas_rec))
mc <- mc %>% mutate(lab = factor(mklab(country, n_cty, n_meas_rec), levels = cty_ord$lab),
                    mrow = factor(measure, levels = rev(MEASURES$measure)),
                    tier = factor(tier, levels = names(PAL_TIER)),
                    eco_group = factor(eco_group, levels = names(PAL_ECO)))

p_a5 <- ggplot(mc, aes(lab, mrow)) +
  ## grey x = the country's measure-naming records exist but none supports this
  ## measure. open square = the country has NO record naming any measure, so the
  ## cell is unknown rather than absent -- previously both drew an x.
  geom_point(data = mc %>% filter(n_meas_rec == 0), shape = 0, size = 1.3,
             colour = "grey62", stroke = .4) +
  geom_point(data = mc %>% filter(n_meas_rec > 0, n_m == 0), shape = 4,
             size = 1.15, colour = "grey45", stroke = .45) +
  ## v55 DOT SIZING AND COLOUR, on two author instructions.
  ##
  ## (1) "some dots are so tiny and we dont really see th progression from few to
  ## many distinctively". scale_size_area maps share to AREA from zero, so a 5%
  ## share drew at sqrt(.05) = 22% of the maximum radius -- about 1 pixel at this
  ## panel size, visually indistinguishable from the absent-marker x beside it, and
  ## the low end of the range carried almost no gradient. scale_radius with an
  ## explicit floor gives every present cell a readable mark and spreads the
  ## progression across the range a reader can actually discriminate. The area
  ## convention is dropped deliberately: this panel is read as a progression within
  ## a row, not as a quantity to be compared by area across rows, and the number
  ## beside each measure carries the count exactly.
  ##
  ## (2) "we can also remove the technical demand of the measure bit as that is
  ## already in fig. 7". The tier was doing double duty here -- as the dot colour AND
  ## as the facet rows -- while figure 7c carries the same three demand classes as
  ## its own axis in the D3a test where they are load-bearing. Colour now carries the
  ## nature-based family instead, which is this panel's own descriptive dimension and
  ## is not shown anywhere else, and the rows are one flat measure axis ordered by
  ## corpus prevalence so the row order matches panel a.
  geom_point(data = mc %>% filter(n_m > 0),
             aes(size = share, colour = eco_group, alpha = n_meas_rec >= 5)) +
  scale_alpha_manual(values = c(`TRUE` = .9, `FALSE` = .35),
                     labels = c(`TRUE` = "5+ measure-naming records", `FALSE` = "under 5 (coarse)"),
                     name = NULL) +
  facet_grid(. ~ region_mod, scales = "free_x", space = "free_x",
             labeller = labeller(region_mod = label_wrap_gen(14))) +
  scale_radius(range = c(1.5, 5.4), labels = percent_format(),
               breaks = c(.1, .25, .5, .75, 1), limits = c(0, 1),
               name = "Share of the country's programmes") +
  scale_colour_manual(values = PAL_ECO, name = "Nature-based family",
                      guide = guide_legend(override.aes = list(size = 3))) +
  labs(title = "d. What each country's programmes support",
       subtitle = WRAP("All 28 countries, grouped by region with 'Other regions' last; dot size is the share of the country's MEASURE-NAMING records that support the measure (20 records corpus-wide name no measure and are excluded from the denominator; where that changes a country's count the label shows both). A grey x = no record there supports the measure; an open square = the country has no measure-naming record, so the cell is unknown, not absent. Rows are the measures in corpus-prevalence order, matching panel a; colour is the nature-based family. The technical-demand class is not repeated here -- it is the D3a axis in figure 7c, where it is what the test turns on."),
       x = NULL, y = NULL,
       caption = WRAP("Shares are within-country and multi-response, so a column does not sum to 100%. Countries with fewer than five records are drawn pale: their shares are coarse by construction and should not be read with the same confidence as the large national systems.", 230)) +
  theme_nbs(8) +
  theme(axis.text.x = element_text(angle = 58, hjust = 1, size = 5.8),
        strip.text.y = element_text(angle = 0), legend.position = "bottom")
## PANEL d REMOVED on author instruction ("we dont need 6d anymore"). The
## country x measure dot matrix it carried is not lost: it becomes the right half
## of figure 4, where it sits against the hazard-measure flow that gives it its
## reason -- which is what the author's reference layout does. p_a5 is therefore
## still built above; it is simply no longer part of figure 5.
fig6 <- fig_a3 +
  plot_annotation(title = "Figure 5. How programmes support participation, and how that support varies by country (descriptive foundation for D3)",
                  theme = TITLE())
save_fig(fig6, "fig05_instruments_support", 17, 12)



## ---------------------------------------------------------------------------
## FIGURE 4 ASSEMBLY | v34, rebuilt to the author's reference image.
##
## Instruction: "maintain the building figure showing the different measures so
## bring that back ... rather than having the separate sankey for the measures
## and a separate to show the distribution across the countries, have one
## combined figure, same as what I have attached".
##
## So:
##   4a = the house cutaway -- the only place a reader learns WHAT each measure
##        is and WHERE on a parcel it sits;
##   4b = ONE combined panel: hazard -> measure flow on the left, country x
##        measure matrix on the right, sharing a single measure axis so a row
##        reads straight across from "which hazards drive this measure" to
##        "which national systems support it". Demand class is the dot colour,
##        exactly as the reference does it, so no third flow axis is needed.
##
## The alignment is the point: both halves are built from ONE ordering vector,
## so a measure sits at the same height in both. Ordering is by demand tier
## (building-integrated first) then by prevalence, matching the reference.
## ROW ORDER TAKEN FROM THE REFERENCE FIGURE the author supplied, which
## runs building-integrated first, then the ground-modifying measures, then the
## standardised products. tier-then-prevalence produced a near-match but put
## rain barrels above the ground measures (it is the single most prevalent
## measure), which is not the reference's reading order.
.REF_ROWS <- c("Green roofs", "Green facades", "Infiltration systems",
               "Depaving / permeable", "Rain gardens", "Trees (pits)",
               "Garden ponds and swales", "Rain barrels / cisterns",
               "Downspout disconnection (to a vegetated or infiltrating receiver)",
               "Native / drought planting", "Turf replacement")
MEAS_ROW_ORDER <- mprev %>%
  mutate(tier = factor(tier, levels = levels(mc$tier))) %>%
  arrange(tier, desc(n)) %>%
  pull(measure) %>%
  as.character()
MEAS_ROW_ORDER <- c(intersect(.REF_ROWS, MEAS_ROW_ORDER),
                    setdiff(MEAS_ROW_ORDER, .REF_ROWS))
MEAS_ROW_ORDER <- MEAS_ROW_ORDER[MEAS_ROW_ORDER %in% unique(as.character(flows$measure))]

## LEFT: hazard -> measure only. The demand tier leaves the flow and becomes the
## dot colour on the right, which is what lets the two halves share one axis.
flows4b <- flows %>%
  group_by(hazard, measure) %>% summarise(n = sum(n), .groups = "drop") %>%
  mutate(hazard  = factor(hazard, levels = names(HAZ4)),
         measure = factor(as.character(measure), levels = rev(MEAS_ROW_ORDER)))
haz_tot <- flows4b %>% group_by(hazard) %>% summarise(n = sum(n), .groups = "drop")

p_4b_flow <- ggplot(flows4b, aes(y = n, axis1 = hazard, axis2 = measure)) +
  ggalluvial::geom_alluvium(aes(fill = hazard), alpha = .62, width = .06,
                            curve_type = "sigmoid") +
  ggalluvial::geom_stratum(width = .06, fill = "grey25", colour = NA) +
  ## hazard labels sit OUTSIDE the left stratum, as in the reference. At
  ## hjust = 1.08 they were drawn a fraction of a stratum-width away, which put
  ## them on top of the ribbons leaving the stratum and made them unreadable
  ## against the fill. nudge_x moves them clear of the bar in axis units, and the
  ## expansion below reserves the room for them.
  geom_text(stat = ggalluvial::StatStratum, size = 2.3, hjust = 1,
            nudge_x = -.055, lineheight = .95, colour = PAL$ink,
            aes(label = ifelse(after_stat(x) == 1,
                               sprintf("%s\n%s", as.character(after_stat(stratum)),
                                       format(after_stat(count), big.mark = ",")), ""))) +
  scale_fill_manual(values = HAZ_FILL, name = "Documented hazard (ribbon colour)") +
  ## v35b: the hazard names are drawn to the LEFT of the first stratum, so they
  ## overhang the panel and were being CLIPPED at its edge ("Floods 1,548"
  ## rendered as a sliver). Expansion alone did not fix it because the text
  ## still starts outside the data range; clipping has to be turned off and
  ## the plot given left margin for the overhang to land in.
  scale_x_continuous(breaks = NULL, expand = expansion(mult = c(.46, .02))) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(.01, .01))) +
  labs(x = NULL, y = NULL) +
  theme_nbs(7.6) +
  theme(legend.position = "bottom", panel.grid = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank(),
        plot.margin = margin(3, 0, 3, 16))

## RIGHT: the same measures, same order, one row each -- countries as columns.
mc4b <- mc %>%
  mutate(measure_lab = as.character(measure)) %>%
  filter(measure_lab %in% MEAS_ROW_ORDER) %>%
  mutate(measure_lab = factor(measure_lab, levels = rev(MEAS_ROW_ORDER)))
meas_tot <- mprev %>% filter(measure %in% MEAS_ROW_ORDER) %>%
  mutate(measure_lab = factor(as.character(measure), levels = rev(MEAS_ROW_ORDER)))

## the facet the right-edge labels belong to -- the rightmost region column.
.LAST_REGION <- levels(droplevels(factor(mc4b$region_mod)))[nlevels(droplevels(factor(mc4b$region_mod)))]
p_4b_matrix <- ggplot(mc4b, aes(lab, measure_lab)) +
  geom_point(data = mc4b %>% filter(n_meas_rec == 0), shape = 0, size = 1.2,
             colour = "grey62", stroke = .4) +
  geom_point(data = mc4b %>% filter(n_meas_rec > 0, n_m == 0), shape = 4,
             size = 1.1, colour = "grey45", stroke = .45) +
  geom_point(data = mc4b %>% filter(n_m > 0),
             aes(size = share, colour = eco_group, alpha = n_meas_rec >= 5)) +
  ## the measure name is a REAL RIGHT-SIDE AXIS, not text positioned at
  ## x = Inf. The text approach put eleven labels inside the matrix -- in a
  ## faceted plot with free x scales, x = Inf resolves per panel, so the labels
  ## landed at whichever panel edge they were assigned rather than outside the
  ## whole strip. An axis is laid out by ggplot outside every facet, which is
  ## where the reference figure puts it. The corpus total is its own narrow
  ## panel to the right (p_4b_tot), so the two columns align like the reference
  ## instead of being nudged apart by a hand-tuned hjust.
  coord_cartesian(clip = "off") +
  facet_grid(. ~ region_mod, scales = "free_x", space = "free_x",
             labeller = labeller(region_mod = label_wrap_gen(14))) +
  scale_alpha_manual(values = c(`TRUE` = .9, `FALSE` = .35),
                     labels = c(`TRUE` = "5+ measure-naming records", `FALSE` = "under 5 (coarse)"),
                     name = NULL) +
  ## v55, two author instructions on THIS panel (figure 4b -- the one that ships).
  ##
  ## (1) "some dots are so tiny and we dont really see th progression from few to
  ## many distinctively". scale_size_area maps share to AREA from zero, so a 5%
  ## share drew at sqrt(.05) = 22% of the maximum radius -- roughly one pixel here,
  ## indistinguishable from the absent-marker x in the neighbouring cell, and the low
  ## end of the range carried almost no gradient. scale_radius with an explicit floor
  ## gives every present cell a readable mark and spreads the progression over a
  ## range a reader can discriminate. Dropping the area convention is deliberate: the
  ## panel is read as a progression along a row, and the exact count sits beside each
  ## measure in the total column, so nothing rests on judging area.
  ##
  ## (2) "we can also remove the technical demand of the measure bit as that is
  ## already in fig. 7". The demand class is the D3a axis in figure 7c, where the test
  ## turns on it. Colour here now carries the NATURE-BASED FAMILY, which is this
  ## panel's own descriptive dimension and appears nowhere else in the paper.
  scale_radius(range = c(1.4, 5.2), labels = percent_format(),
               breaks = c(.1, .25, .5, .75, 1), limits = c(0, 1),
               name = "Share of the country's programmes (dot size)") +
  scale_colour_manual(values = PAL_ECO, name = "Nature-based family (dot colour)",
                      guide = guide_legend(override.aes = list(size = 2.6))) +
  labs(x = NULL, y = NULL) +
  theme_nbs(7.6) +
  ## country names sit at the TOP, written vertically, with the region
  ## header above them -- exactly the arrangement in the reference figure the
  ## author supplied. At the bottom and angled they collided with the legend
  ## block and read as a wall of diagonal text.
  scale_x_discrete(position = "top") +
  scale_y_discrete(position = "right") +
  theme(axis.text.x.top = element_text(angle = 90, hjust = 0, vjust = .5, size = 5.2),
        axis.text.y.right = element_text(size = 5.8, hjust = 0),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93", linewidth = .25),
        panel.grid.major.x = element_blank(),
        strip.placement = "outside",
        legend.position = "bottom", legend.box = "vertical",
        plot.margin = margin(3, 2, 3, 0))

## the corpus total is its OWN narrow panel at the far right, exactly as in
## the reference figure. Drawing it as text inside the matrix meant its position
## depended on a hand-tuned hjust that had to be re-tuned whenever the matrix
## width changed; as a panel it simply shares the measure axis.
p_4b_tot <- ggplot(meas_tot, aes(x = 1, y = measure_lab)) +
  geom_text(aes(label = format(n, big.mark = ",")), size = 2.3,
            colour = PAL$indet, hjust = 1) +
  scale_x_continuous(limits = c(.4, 1.05), breaks = NULL, expand = expansion(0)) +
  labs(x = NULL, y = NULL, title = NULL) +
  theme_nbs(7.6) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.margin = margin(3, 2, 3, 0))

p_a2_combined <- (p_4b_flow | p_4b_matrix | p_4b_tot) +
  plot_layout(widths = c(1, 2.1, .12), guides = "collect") &
  theme(legend.position = "bottom", legend.box = "horizontal",
        legend.title = element_text(size = 6.4), legend.text = element_text(size = 6))

fig_a2 <- (p_a2b / wrap_elements(full = patchwork::patchworkGrob(p_a2_combined))) +
  plot_layout(heights = c(1, 1.35)) +
  plot_annotation(
    title = "Figure 4. What is supported: the measures themselves, the hazards behind them, and where each is supported",
    subtitle = WRAP(paste(
      "Panel a names each measure and shows where on a parcel it sits. Panel b puts the hazard-to-measure flow and the",
      "country matrix on ONE measure axis: read a row straight across from the hazards that drive that measure to the",
      "national systems supporting it, with the dot colour giving the nature-based family each measure belongs to. Multi-response",
      "throughout -- a record documenting three hazards and four measures contributes twelve flows -- so the flow widths are",
      "hazard-measure combinations, not programmes, and the number beside each measure is the count of programmes supporting it.",
      "Whether a supported measure actually delivers the ecological function its hazard requires is the D1 fit test,",
      "assessed in figure 7a, not here.")),
    theme = TITLE())
save_fig(fig_a2, "fig04_measures", 20, 19)

msg("Part A (descriptive overview): 4 figures written.")

msg("Part A (descriptive overview): 4 figures written.")


## ---------------------------------------------------------------------------
## HAZARD FRAMING AND SUPPORT COMPOSITION: THE NUMBERS SECTION 3.2 QUOTES.
##
## These were computed live for the manuscript and had no table, so the
## number-level verifier could only confirm the digits existed SOMEWHERE among
## the run's tables -- presence, not provenance, which is the same weak test
## that hid the figure-3b gap. Both are emitted here so each quoted value has
## one addressable home.
{
  .z  <- function(x) dplyr::coalesce(as.integer(x), 0L)
  .nh <- .z(A$haz_storm) + .z(A$haz_heat) + .z(A$haz_drought) + .z(A$haz_biodiv)
  .ms <- grep("^m_", names(A), value = TRUE)
  .lead <- function(idx, k) {
    v <- sort(vapply(.ms, function(m) sum(.z(A[[m]][idx])), integer(1)), decreasing = TRUE)
    head(v, k)
  }
  .rows <- list()
  for (.h in c("haz_storm", "haz_heat", "haz_drought", "haz_biodiv")) {
    .i <- which(.z(A[[.h]]) == 1)
    .l <- .lead(.i, 3)
    .rows[[.h]] <- data.frame(
      hazard = .h, n_programmes = length(.i),
      pct_corpus = round(100 * length(.i) / nrow(A), 1),
      sole_objective = sum(.z(A[[.h]]) == 1 & .nh == 1),
      lead_measure_1 = names(.l)[1], lead_n_1 = as.integer(.l[1]),
      lead_measure_2 = names(.l)[2], lead_n_2 = as.integer(.l[2]),
      lead_measure_3 = names(.l)[3], lead_n_3 = as.integer(.l[3]),
      stringsAsFactors = FALSE)
  }
  .hz <- do.call(rbind, .rows)
  .hz <- rbind(.hz, data.frame(hazard = "two_or_more_hazards",
    n_programmes = sum(.nh >= 2), pct_corpus = round(100 * mean(.nh >= 2), 1),
    sole_objective = NA_integer_, lead_measure_1 = NA_character_, lead_n_1 = NA_integer_,
    lead_measure_2 = NA_character_, lead_n_2 = NA_integer_,
    lead_measure_3 = NA_character_, lead_n_3 = NA_integer_, stringsAsFactors = FALSE))
  save_tab(.hz, "14_a5_hazard_framing_and_lead_measures")

  ## Support-family composition by country, on the SAME var-to-family map as
  ## figure 5 (14_instrument_prevalence), so the two can never disagree.
  .ip  <- .read_out("14_instrument_prevalence")
  if (!is.null(.ip)) {
    .fams <- split(.ip$var, .ip$family)
    .anyof <- function(idx, vs) {
      vs <- intersect(vs, names(A)); if (!length(vs)) return(0L)
      m <- vapply(vs, function(v) .z(A[[v]][idx]), integer(length(idx)))
      if (is.null(dim(m))) sum(m) else sum(apply(m, 1, max))
    }
    .cy <- A$country
    .keep <- names(which(table(.cy) >= 40))
    .out <- list()
    for (.k in c(.keep, "CORPUS")) {
      .i <- if (.k == "CORPUS") seq_len(nrow(A)) else which(.cy == .k)
      .o <- vapply(.fams, function(vs) .anyof(.i, vs), integer(1))
      .out[[.k]] <- data.frame(country = .k, n = length(.i), family = names(.o),
                               n_programmes = as.integer(.o),
                               pct = round(100 * .o / length(.i), 1),
                               stringsAsFactors = FALSE)
    }
    save_tab(do.call(rbind, .out), "14_a5_support_family_by_country")
  }
}
