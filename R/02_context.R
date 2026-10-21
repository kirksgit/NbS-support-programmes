# =============================================================================
# 02_context.R  |  IMPLEMENTATION CONTEXT
# Context is *not* fit and is never scored. It supplies (a) the conditions
# against which fit is judged and (b) the predictors of Stage-2 models.
# =============================================================================

REGION <- c(
  "United States" = "North America", "Canada" = "North America", "Mexico" = "Latin America",
  "Brazil" = "Latin America", "Argentina" = "Latin America",
  "Japan" = "East Asia", "Korea, Rep." = "East Asia", "Taiwan, Province of China" = "East Asia",
  "Singapore" = "Southeast Asia", "Malaysia" = "Southeast Asia", "India" = "South Asia",
  "Germany" = "Western Europe", "Netherlands" = "Western Europe", "France" = "Western Europe",
  "Belgium" = "Western Europe", "Austria" = "Western Europe", "Switzerland" = "Western Europe",
  "Ireland" = "Western Europe", "United Kingdom" = "Western Europe", "Spain" = "Southern Europe",
  "Denmark" = "Northern Europe", "Norway" = "Northern Europe",
  "Poland" = "Central/Eastern Europe", "Hungary" = "Central/Eastern Europe",
  "Czech Republic" = "Central/Eastern Europe", "Slovak Republic" = "Central/Eastern Europe",
  "Australia" = "Oceania", "New Zealand" = "Oceania"
)

ctx <- raw %>% transmute(
  case_id, city, country, programme,

  region  = unname(REGION[country]),
  region  = ifelse(is.na(region), "Other/unclassified", region),

  ## ---- Population and density ---------------------------------------------
  pop         = coalesce(city_population, first_num(v_A3)),
  pop_log     = log10(pmax(pop, 1000)),
  density     = first_num(v_A6),
  density     = ifelse(density < 50 | density > 60000, NA_real_, density),  # implausible -> NA
  density_band = cut(density, c(-Inf, 1000, 2500, 5000, Inf),
                     labels = c("<1000", "1000-2499", "2500-4999", "5000+")),

  ## ---- Climate (Koppen main group + summer-dry / arid flags) ---------------
  koppen      = str_extract(strip_qual(v_A4), "\\b[ABCDE][SWfsmw][abchkdF]?\\b"),
  climate_grp = case_when(
    str_starts(koppen, "B")  ~ "Arid/semi-arid",
    str_detect(koppen, "^Cs") ~ "Mediterranean (summer-dry)",
    str_detect(koppen, "^Cf") ~ "Temperate humid",
    str_detect(koppen, "^D")  ~ "Continental",
    str_detect(koppen, "^A")  ~ "Tropical",
    TRUE ~ NA_character_),

  ## ---- Land use ------------------------------------------------------------
  lu_highrise = multi_has(v_A5, "high-rise"),
  lu_lowrise  = multi_has(v_A5, "low-rise"),
  lu_mixed    = multi_has(v_A5, "mixed-use"),
  lu_indust   = multi_has(v_A5, "industrial|peri-urban"),

  ## ---- Independently documented hazard exposure (A-7ii, free text) --------
  hx_flood    = kw(v_A7ii, "flood|pluvial|stormwater|sewer back|inundat|hochwasser|overflow|runoff"),
  hx_heat     = kw(v_A7ii, "heat|uhi|island effect|hitze|warming"),
  hx_drought  = kw(v_A7ii, "drought|water scarcity|water stress|aridity|d\u00fcrre"),
  hx_coastal  = kw(v_A7ii, "coastal|sea level|storm surge|tidal"),
  hx_erosion  = kw(v_A7ii, "erosion|landslide|slope"),
  hx_n        = hx_flood + hx_heat + hx_drought + hx_coastal + hx_erosion,

  ## ---- Tenure --------------------------------------------------------------
  renter_pct, owner_pct,
  tenure_level,
  tenure_city_level = as.integer(str_detect(tolower(tenure_level), "city|municipal")),
  renter_band = cut(renter_pct, c(-Inf, 30, 45, 60, Inf),
                    labels = c("<30%", "30-44%", "45-59%", "60%+")),
  renter_dominant = as.integer(renter_pct >= 50)
)

## ---- Country handling for models -------------------------------------------
ctx <- ctx %>%
  group_by(country) %>% mutate(n_country = n()) %>% ungroup() %>%
  mutate(country_mod = ifelse(n_country >= CFG$min_country_n, country, "Other (pooled)"),
         country_mod = factor(country_mod),
         region      = factor(region))

## ---- City grouping ----------------------------------------------------------
## The `City` column and `A-1 City` disagree for 141 cases, because A-1 records
## the parent city or metropolitan authority in parentheses:
## "North York (Toronto)", "Ottakring (Wien/Vienna)", "... (Chicago)".
## Grouping on the parent recovers genuine multi-programme city portfolios that
## exact matching on `City` misses entirely.
norm_city <- function(x) {
  ## TRANSLITERATION IS NOW LOCALE-INDEPENDENT.
  ## This used iconv(..., "ASCII//TRANSLIT"), whose output is platform AND locale
  ## dependent, so the CITY GROUPING -- and therefore the unit-of-analysis count
  ## the paper reports -- changed with the machine it ran on. Measured:
  ##   macOS, LC_ALL=C        "Koeln" -> NA        "Otsu" -> NA
  ##   macOS, LC_ALL=UTF-8    "Koeln" -> k"oln     "Otsu" -> otsu
  ##   glibc, LC_ALL=UTF-8    "Koeln" -> koln      "Otsu" -> otsu
  ## Only the third fires the "koln" -> "cologne" recode below, so on macOS case
  ## 148 (Koeln, GRUEN hoch 3) and case 170 (Nippes/Koeln, Koeln GRUEN hoch 3) --
  ## the same programme -- were counted as two city groups, and the shipped table
  ## read 16 groups / 47 records instead of 17 / 49. Under LC_ALL=C the NA return
  ## additionally collapsed "Otsu" onto the real city Tsu (case 315), inventing a
  ## two-programme city that does not exist.
  ## stringi's ICU transliteration is identical on every platform and locale.
  x <- stringi::stri_trans_general(norm_txt(x), "Latin-ASCII")
  x <- tolower(x)
  x <- str_remove(x, "^(city|town|village|municipality|ville|stadt|gemeente) (of |de |von )?")
  x <- str_replace_all(x, "[^a-z0-9]+", " ")
  str_squish(x)
}

parent_raw <- str_match(norm_txt(raw$v_A1), "\\(([^)]+)\\)")[, 2]
## a parenthetical is a parent city only if it names a place, not a qualifier
## (state abbreviations, regional-district acronyms, river names, ward numbers)
is_qualifier <- is.na(parent_raw) |
  str_detect(parent_raw, regex("^(rdn|kreis|saale|[a-z]{2})$|county|,|district|^[0-9]", ignore_case = TRUE))
parent_city <- ifelse(is_qualifier, NA_character_, parent_raw)

ctx <- ctx %>%
  mutate(city_group = norm_city(coalesce(parent_city, raw$city)),
         city_group = recode(city_group,
                             "wien vienna" = "vienna", "vienna 17th" = "vienna",
                             "koln" = "cologne", "montreal" = "montreal")) %>%
  group_by(country, city_group) %>% mutate(n_prog_in_city = n()) %>% ungroup() %>%
  mutate(portfolio_city = as.integer(n_prog_in_city >= 3))

## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
## LAND-USE MIX (A-5) AS ITS CODED CLASSES, not a count.
##
## Two corrections to the v31 derivation, both found by re-reading the codebook
## and the raw cells rather than trusting the earlier pass:
##
##  (1) THE SEPARATOR IS ";", NOT "|". The codebook prints the option list with
##      pipes, but coders entered multi-selects with semicolons: 420 records use
##      ";" and only 9 use "|". Splitting on "|" therefore returned almost every
##      cell as a single blob, which is why the earlier version could only offer
##      a COUNT of forms -- it never actually saw the classes. Both separators
##      are handled now.
##  (2) THE FIGURE NEEDS THE CLASSES, NOT THE COUNT. A-5 is a five-option
##      multi-select -- High-rise residential, Low-rise residential, Mixed-use,
##      Industrial/peri-urban, Other [specify] -- and "how many forms" answers a
##      different question from "which forms". Per-class flags are derived here
##      and the count is kept alongside for the map.
##
## Prevalence on the current workbook: low-rise 570, mixed-use 528, industrial /
## peri-urban 88, high-rise 28, other 43. These are CONTEXT variables: projected
## passively, entering no fit verdict, exactly like density and tenure.
.lu <- raw$v_A5
.lu_clean <- gsub("\\[[^]]*\\]", "", ifelse(is.na(.lu), "", .lu))   # drop [inferred] notes
.lu_parts <- strsplit(.lu_clean, "[;|]")
.has <- function(pat) vapply(.lu_parts, function(p) any(grepl(pat, p, ignore.case = TRUE)), logical(1))

ctx <- ctx %>% mutate(
  landuse_raw     = .lu,
  lu_highrise     = as.integer(.has("high-rise")),
  lu_lowrise      = as.integer(.has("low-rise")),
  lu_mixed        = as.integer(.has("mixed-use|mixed use")),
  lu_industrial   = as.integer(.has("industrial|peri-urban")),
  lu_other        = as.integer(.has("other")),
  ## how many of the five the record documents -- the map's size channel
  landuse_n       = lu_highrise + lu_lowrise + lu_mixed + lu_industrial + lu_other,
  landuse_n       = ifelse(landuse_n == 0, NA_integer_, landuse_n),
  ## a single DOMINANT class for any display needing one value per record.
  ## Order of precedence is built intensity, most specific first, so a record
  ## documenting both high-rise and mixed-use reads as the denser form.
  landuse_mix = dplyr::case_when(
    lu_highrise   == 1 ~ "High-rise residential",
    lu_mixed      == 1 ~ "Mixed-use",
    lu_lowrise    == 1 ~ "Low-rise residential",
    lu_industrial == 1 ~ "Industrial / peri-urban",
    lu_other      == 1 ~ "Other documented form",
    TRUE               ~ NA_character_))

LU_CLASSES <- c("High-rise residential", "Mixed-use", "Low-rise residential",
                "Industrial / peri-urban", "Other documented form")
msg("Land-use mix (A-5): low-rise %d, mixed-use %d, industrial/peri-urban %d, high-rise %d, other %d; %d not stated",
    sum(ctx$lu_lowrise, na.rm = TRUE), sum(ctx$lu_mixed, na.rm = TRUE),
    sum(ctx$lu_industrial, na.rm = TRUE), sum(ctx$lu_highrise, na.rm = TRUE),
    sum(ctx$lu_other, na.rm = TRUE), sum(is.na(ctx$landuse_mix)))

city_n <- ctx %>% count(country, city_group, name = "n_prog") %>% arrange(desc(n_prog))
save_tab(city_n %>% filter(n_prog > 1), "02_city_groups_multi_programme")

n_multi <- sum(city_n$n_prog > 1); n_port <- sum(city_n$n_prog >= 3)
msg("City grouping: %d city groups; %d with >=2 programmes, %d with >=3 (%d programmes).",
    nrow(city_n), n_multi, n_port, sum(city_n$n_prog[city_n$n_prog >= 3]))
msg("  -> portfolio analysis is possible for %d named cities; city random effects are NOT",
    n_port)
msg("     estimable corpus-wide (%.1f%% of city groups hold a single programme), so models",
    100 * mean(city_n$n_prog == 1))
msg("     use country clustering and the city portfolios are reported as named cases.")

msg("  hazard exposure documented for %.0f%% of cases (mean %.1f hazards)",
    100 * mean(ctx$hx_n > 0, na.rm = TRUE), mean(ctx$hx_n, na.rm = TRUE))


# =============================================================================
# PROGRAMME FAMILIES  |  the corpus contains duplicated programmes
# A regional utility scheme is coded once per municipality it serves, so some
# rows are not independent observations of distinct programmes. Cases are
# grouped into families by identical primary official URL, falling back to
# identical lead body + programme name within a country. Families are used for
# (a) a non-independence sensitivity analysis and (b) an honest statement of how
# many DISTINCT programmes the corpus contains.
# =============================================================================
norm_key <- function(x) {
  x <- tolower(norm_txt(x))
  x <- str_remove_all(x, "\\[[^]]*\\]")
  x <- str_replace_all(x, "[^a-z0-9 ]", " ")
  str_squish(x)
}
fam <- raw %>% transmute(
  case_id, country,
  url_key  = norm_key(str_split_fixed(replace_na(url_primary, ""), "[;\n]", 2)[, 1]),
  name_key = norm_key(v_B2),
  body_key = norm_key(v_B4)) %>%
  mutate(url_key = ifelse(url_key == "" | str_detect(url_key, "^na$"), NA_character_, url_key),
         family_key = case_when(
           !is.na(url_key)                      ~ paste0("url:", url_key),
           name_key != "" & body_key != ""      ~ paste0("nb:", country, "|", body_key, "|", name_key),
           TRUE                                 ~ paste0("case:", case_id)),
         prog_family = as.integer(factor(family_key)))

## SECOND PASS ON THE FAMILY KEY. The URL key splits a programme whose records
## cite different official domains of the same authority: the Delhi Jal Board
## lists two, and four US water districts list a portal page and a programme
## page. Records that share a country, an implementing body and a programme
## name are therefore merged into one family, so the key is the union of the
## two rules rather than a replacement of either. Both name fields are used,
## because B-2 carries the official name where it is documented while the
## record label carries it where B-2 reads "Not stated".
.fk <- fam$family_key
.root <- function(k, map) { while (map[[k]] != k) k <- map[[k]]; k }
.map <- as.list(setNames(unique(.fk), unique(.fk)))
for (.nmcol in list(norm_key(raw$v_B2), norm_key(raw$prog_ss))) {
  .alt <- ifelse(.nmcol != "" & norm_key(raw$v_B4) != "",
                 paste0(fam$country, "|", norm_key(raw$v_B4), "|", .nmcol), NA_character_)
  for (.k in unique(.alt[!is.na(.alt)])) {
    .idx <- which(.alt == .k)
    if (length(.idx) > 1) {
      .keys <- unique(.fk[.idx])
      .r0 <- .root(.keys[1], .map)
      for (.kk in .keys[-1]) { .r <- .root(.kk, .map); if (.r != .r0) .map[[.r]] <- .r0 }
    }
  }
}
fam$family_key <- vapply(.fk, .root, character(1), map = .map, USE.NAMES = FALSE)
fam$prog_family <- as.integer(factor(fam$family_key))

ctx <- ctx %>% left_join(fam %>% select(case_id, prog_family), by = "case_id") %>%
  group_by(prog_family) %>% mutate(family_size = n()) %>% ungroup()

dup <- ctx %>% filter(family_size > 1) %>%
  left_join(raw %>% select(case_id, programme_name = v_B2), by = "case_id") %>%
  select(prog_family, family_size, country, city, programme_name) %>%
  arrange(desc(family_size), prog_family, city)
save_tab(dup, "02_duplicated_programme_families")

msg("Programme families: %d distinct programmes behind %d coded cases; %d cases sit in %d multi-case families",
    n_distinct(ctx$prog_family), nrow(ctx), sum(ctx$family_size > 1),
    n_distinct(ctx$prog_family[ctx$family_size > 1]))
