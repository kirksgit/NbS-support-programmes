# =============================================================================
# 10_context_matching.R  |  CONTEXT-PROGRAMME MATCHING AND READINESS GAPS
# Step 1 derive context profiles (context variables only)
# Step 2 cross-match context profiles with programme typologies
# Step 3 model fit WITHIN contexts (typology x context)
# Step 4 produce context-specific readiness-gap matrices (no overall index)
# =============================================================================

suppressPackageStartupMessages({ library(cluster) })

# -----------------------------------------------------------------------------
# STEP 1 | Context profiles
#
# NOTE ON STATUS. Two representations of context are produced. The MODULAR
# typing in Step 5 is the primary device: a city assembles the modules that
# apply to it (dense, renter-dominant, flood-exposed, heat-exposed, arid, large)
# rather than being forced into a single archetype. The empirical PAM-Gower
# clustering below is retained as a DIAGNOSTIC only. It selects k = 2 with a
# silhouette around 0.37, which separates temperate-and-flood-exposed cities
# from Mediterranean-and-drought-exposed ones. That is a real division but too
# coarse to carry the analysis, and it is reported as a check on the modular
# typing rather than as a context typology in its own right.
# -----------------------------------------------------------------------------
ctx_vars <- M %>% transmute(
  case_id,
  density_band = factor(replace_na(as.character(density_band), "unknown")),
  climate      = factor(climate_grp),
  renter_band  = factor(replace_na(as.character(renter_band), "unknown")),
  pop_band     = cut(pop, c(-Inf, 5e4, 2e5, 1e6, Inf),
                     labels = c("<50k", "50-200k", "200k-1M", "1M+")),
  hz_flood     = factor(hx_flood), hz_heat = factor(hx_heat), hz_drought = factor(hx_drought)
) %>% mutate(pop_band = factor(replace_na(as.character(pop_band), "unknown")))

g <- cluster::daisy(as.data.frame(ctx_vars[, -1]), metric = "gower")
ctx_sil <- map_dfr(2:6, function(k) {
  pm <- cluster::pam(g, k = k, diss = TRUE)
  tibble(k = k, avg_silhouette = pm$silinfo$avg.width)
})
save_tab(ctx_sil, "10_context_cluster_silhouette")
k_ctx <- ctx_sil$k[which.max(ctx_sil$avg_silhouette)]
M$context_type <- factor(cluster::pam(g, k = k_ctx, diss = TRUE)$clustering)
msg("Context clustering (DIAGNOSTIC ONLY): k = %d, silhouette %.3f -- too coarse to carry the analysis; the modular typing in Step 5 is primary",
    k_ctx, max(ctx_sil$avg_silhouette))

ctx_profile <- map_dfr(names(ctx_vars)[-1], function(v) {
  tibble(variable = v, level = as.character(ctx_vars[[v]]), ct = M$context_type) %>%
    count(variable, ct, level) %>% group_by(ct, variable) %>%
    mutate(share = n / sum(n)) %>% ungroup() %>% filter(share > 0.4)
})
save_tab(ctx_profile, "10_context_profiles")

# -----------------------------------------------------------------------------
# STEP 2 | Context x programme-architecture matching matrix
# -----------------------------------------------------------------------------
match_tab <- M %>% count(context_type, typology_k) %>%
  group_by(context_type) %>% mutate(share_within_context = n / sum(n)) %>% ungroup()
save_tab(match_tab, "10_context_typology_matrix")
match_test <- suppressWarnings(chisq.test(table(M$context_type, M$typology_k),
                                          simulate.p.value = TRUE))
msg("  context x typology chi-square p = %.4f", match_test$p.value)

# -----------------------------------------------------------------------------
# STEP 3 | Does the same architecture fit differently in different contexts?
#   Pre-registered: access route x renter context; technical grade x demand
# -----------------------------------------------------------------------------
d9 <- M %>% filter(access_fit != "Indeterminate") %>%
  mutate(route_open = as.integer(access_route %in%
           c("Direct tenant/broad pathway", "Controller/collective pathway",
             "Permission-dependent pathway")),
         typology_k = droplevels(typology_k))
m9 <- glm(route_open ~ typology_k * renter_c + density_c + pop_c + region_mod,
          data = d9, family = binomial())
save_tab(tidy_cl(m9, d9$country, "Typology x renter context -> open access route") %>%
           mutate(n = nobs(m9)), "10_typology_context_interaction")

# -----------------------------------------------------------------------------
# STEP 4 | Readiness-gap matrices, by context type (NO overall index)
# -----------------------------------------------------------------------------
requirement_map <- tribble(
  ~requirement,                                   ~indicator,        ~fit_levels,
  "Hazard-responsive intervention portfolio",     "eco_fit",         "Documented fit",
  "Relevant property-controller pathway",         "access_fit",      "Documented fit",
  "Tenant or collective route",                   "tenant_route",    "1",
  "Appropriate technical enablement",             "tech_fit",        "Documented fit",
  ## Reads the UNIVERSAL financial verdict; the restricted 39-case fin_fit is removed.
  "Financial mechanism suited to target group",   "fit_D3b",         "Documented fit",
  "Administrative guidance",                      "admin_ok",        "1",
  "Maintenance responsibility and support",       "steward_fit",     "Documented fit",
  "Monitoring and review",                        "monitor_ok",      "1"
)

R <- M %>% mutate(
  tenant_route = ifelse(access_fit == "Indeterminate", NA_integer_,
                        as.integer(access_route %in% c("Direct tenant/broad pathway",
                                                       "Controller/collective pathway",
                                                       "Permission-dependent pathway"))),
  ## these two read RETIRED provision-counting constructs. admin_profile was
  ## retired for D3c at v22 (it counts provisions; D3c matches a requirement), and
  ## monitor_config is the same class of object for D4b. The readiness-gap matrices
  ## are reported figures, so a gap computed from a retired construct is a reported
  ## number the framework does not compute. Both now read the canonical verdict, on
  ## the same bar every other component uses here: documented fit.
  admin_ok   = ifelse(fit_D3c %in% c("Indeterminate", "Not applicable"), NA_integer_,
                      as.integer(fit_D3c == "Documented fit")),
  monitor_ok = ifelse(fit_D4b %in% c("Indeterminate", "Not applicable"), NA_integer_,
                      as.integer(fit_D4b == "Documented fit"))
)

gap_matrix <- function(df, group_var) {
  map_dfr(seq_len(nrow(requirement_map)), function(i) {
    ind <- requirement_map$indicator[i]; fitlev <- requirement_map$fit_levels[i]
    df %>% group_by(.grp = .data[[group_var]]) %>%
      summarise(
        requirement = requirement_map$requirement[i],
        n_applicable = sum(!is.na(.data[[ind]])),
        documented = sum(as.character(.data[[ind]]) == fitlev, na.rm = TRUE),
        partial = sum(as.character(.data[[ind]]) == "Partial fit", na.rm = TRUE),
        misfit  = sum(as.character(.data[[ind]]) == "Confirmed misfit", na.rm = TRUE),
        indeterminate = sum(is.na(.data[[ind]]) |
                              as.character(.data[[ind]]) == "Indeterminate", na.rm = TRUE),
        .groups = "drop") %>%
      mutate(pct_documented = 100 * documented / pmax(n_applicable, 1))
  }) %>% rename(!!group_var := .grp) %>%
    select(!!group_var, requirement, everything())
}

save_tab(gap_matrix(R, "context_type"), "10_readiness_gaps_by_context")
save_tab(gap_matrix(R, "typology_k"),   "10_readiness_gaps_by_typology")
save_tab(gap_matrix(R, "region_mod"),   "10_readiness_gaps_by_region")
save_tab(gap_matrix(R %>% mutate(country_mod = as.character(country_mod)), "country_mod"),
         "10_readiness_gaps_by_country")

# -----------------------------------------------------------------------------
# STEP 5 | MODULAR CONTEXT TYPING (framework section 9)
# A city is not one archetype. Requirements are assembled from independent
# modules, so gaps are reported module by module rather than by a single
# composite city type. The empirical context clusters above are retained as a
# secondary, data-driven check.
# -----------------------------------------------------------------------------
R <- R %>% mutate(
  mod_dense   = as.integer(density >= 2500),
  mod_renter  = as.integer(renter_pct >= 50),
  mod_flood   = hx_flood,
  mod_heat    = hx_heat,
  mod_arid    = as.integer(climate_grp == "Arid/semi-arid" | hx_drought == 1),
  mod_large   = as.integer(pop >= 1e6)
)
modules <- c(mod_dense = "Dense urban form", mod_renter = "Renter-dominant tenure",
             mod_flood = "Documented flood exposure", mod_heat = "Documented heat exposure",
             mod_arid  = "Arid / water-stressed", mod_large = "Large city (1M+)")

module_gaps <- imap_dfr(modules, function(label, v) {
  d <- R %>% filter(.data[[v]] == 1)
  if (nrow(d) < 20) return(NULL)
  gap_matrix(d %>% mutate(.all = "all"), ".all") %>%
    mutate(context_module = label, n_programmes = nrow(d)) %>% select(-.all)
})
save_tab(module_gaps, "10_readiness_gaps_by_context_module")

## Module co-occurrence: how often do requirement modules stack in one city?
save_tab(R %>% count(mod_dense, mod_renter, mod_flood, mod_heat, mod_arid, mod_large) %>%
           arrange(desc(n)), "10_context_module_combinations")

msg("Readiness-gap matrices written for context clusters, modules, typology, region and country.")
