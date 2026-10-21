# =============================================================================
# 08_typology.R  |  PROGRAMME ARCHITECTURES (RQ1 / RQ3)
#
# Two rules govern the active variable set.
#
# 1. FIT OUTPUTS ARE NEVER ACTIVE. Earlier drafts clustered on access_route,
#    tech_grade, admin_profile, steward_config and monitor_config. Those carry
#    the framework's own thresholds inside them, so comparing the resulting
#    classes on fit was partly circular. The active set below uses the
#    underlying DESIGN PRIMITIVES -- which enablers, which eligible actors,
#    which stewardship elements -- and the graded fit variables are
#    projected afterwards as supplementary information only.
#
# 2. MISSINGNESS MUST NOT DEFINE A CLASS. "Not documented" is an active category
#    in MCA, so poorly documented programmes cluster together and produce a
#    spurious "architecture" that is really an opacity group. The substantive
#    typology is therefore estimated on records carrying design information on
#    most blocks; the remainder are reported separately as an opacity group.
# =============================================================================

suppressPackageStartupMessages({ library(FactoMineR); library(cluster) })

# -----------------------------------------------------------------------------
# ACTIVE VARIABLES: design primitives only
# -----------------------------------------------------------------------------
act <- M %>% transmute(
  case_id,

  ## Architecture block: environmental orientation
  A_objective = replace_na(obj_class, "Not documented"),
  B_portfolio = replace_na(portfolio, "Not documented"),
  C_spatial   = replace_na(spatial_response, "Not documented"),

  ## Architecture block: who may apply (eligibility elements, not the graded route)
  D_actors = case_when(
    el_inform == 1 | el_ngo == 1                 ~ "Civil society or informal included",
    el_tenant_direct == 1                        ~ "Direct tenant eligibility",
    el_tenant_cond == 1                          ~ "Consent-dependent tenant eligibility",
    el_land == 1 | el_coop == 1 | el_socho == 1  ~ "Property controllers included",
    el_school == 1 | el_sme == 1                 ~ "Institutional actors included",
    el_home == 1                                 ~ "Homeowners only",
    TRUE                                         ~ "Not documented"),

  ## Architecture block: instruments and enabling provisions
  E_instrument = replace_na(instrument_pkg, "Not documented"),
  F_enablers = case_when(
    r_standards == 1 & i_advice == 1 & cost_design == 1 ~ "Standards, advice and design costs",
    r_standards == 1 & i_advice == 1                    ~ "Standards and advice",
    r_fasttrack == 1 | r_feewaiver == 1                 ~ "Permit-based enablement",
    i_advice == 1                                       ~ "Advice only",
    r_standards == 1                                    ~ "Standards only",
    TRUE                                                ~ "None documented"),
  G_payment = replace_na(payment_config, "Not stated"),

  ## Architecture block: applicant-facing procedure
  H_admin = case_when(
    p_guide == 2 & p_forms >= 1 & n_channels >= 2 ~ "Guide, forms and multiple channels",
    p_guide >= 1 & p_forms >= 1                   ~ "Guide and forms",
    p_guide >= 1 | p_forms >= 1                   ~ "Guide or forms only",
    TRUE                                          ~ "None documented"),

  ## Architecture block: what happens after installation
  I_steward = case_when(
    st_resp_named == 1 & s_condition == 1 & s_nonfin_n > 0 ~ "Responsibility, condition and support",
    st_resp_named == 1 & s_nonfin_n > 0                    ~ "Responsibility and support",
    st_resp_named == 1 & s_condition == 1                  ~ "Responsibility and condition",
    st_resp_named == 1                                     ~ "Responsibility only",
    TRUE                                                   ~ "None documented"),
  J_monitor = case_when(
    mon_indicators == 1 & f_review == 1 ~ "Indicators and review",
    mon_indicators == 1                 ~ "Published indicators",
    mon_webpage == 1 & f_feedback == 1  ~ "Webpage reporting and feedback",
    mon_webpage == 1 | f_feedback == 1  ~ "Webpage reporting or feedback",
    TRUE                                ~ "None documented"),

  ## Architecture block: delivery and funding
  K_delivery = as.character(replace_na(as.character(delivery_arch3), "Single body")),
  L_horizon  = as.character(replace_na(as.character(funding_horizon), "Not stated"))
) %>% mutate(across(-case_id, ~ factor(.x)))

ACT_LABELS <- c(
  A_objective  = "Environmental objective class",
  B_portfolio  = "Intervention portfolio class",
  C_spatial    = "Spatial targeting",
  D_actors     = "Eligible-actor set",
  E_instrument = "Instrument package",
  F_enablers   = "Technical enabler set",
  G_payment    = "Payment configuration",
  H_admin      = "Applicant-facing provisions",
  I_steward    = "Post-installation elements",
  J_monitor    = "Monitoring elements",
  K_delivery   = "Delivery architecture",
  L_horizon    = "Funding horizon")

# -----------------------------------------------------------------------------
# DIAGNOSTIC: is any active variable near-constant, or a proxy for country?
# A variable that is 90% one level adds noise rather than structure. A variable
# strongly associated with country means the typology partly encodes national
# programme traditions rather than design logics, which must be stated.
# -----------------------------------------------------------------------------
cramers_v <- function(x, y) {
  tb <- table(x, y); n <- sum(tb)
  as.numeric(sqrt(suppressWarnings(stats::chisq.test(tb)$statistic) /
                    (n * (min(dim(tb)) - 1))))
}
undoc_levels <- c("Not documented", "None documented", "Not stated")

act_diag <- tibble(variable = names(act)[-1]) %>%
  mutate(label             = unname(ACT_LABELS[variable]),
         n_levels          = map_int(variable, ~ nlevels(droplevels(act[[.x]]))),
         modal_level       = map_chr(variable, ~ names(sort(table(act[[.x]]), decreasing = TRUE))[1]),
         modal_share       = map_dbl(variable, ~ max(prop.table(table(act[[.x]])))),
         undocumented_share = map_dbl(variable, ~ mean(as.character(act[[.x]]) %in% undoc_levels)),
         cramers_v_country = map_dbl(variable, ~ cramers_v(act[[.x]], M$country_mod)),
         cramers_v_region  = map_dbl(variable, ~ cramers_v(act[[.x]], M$region_mod)),
         near_constant     = modal_share > 0.90,
         country_patterned = cramers_v_country > 0.30) %>%
  arrange(desc(modal_share))
save_tab(act_diag, "08_active_variable_diagnostics")

msg("Typology actives: %d design variables; %d near-constant; %d strongly country-patterned (V > 0.30: %s)",
    nrow(act_diag), sum(act_diag$near_constant), sum(act_diag$country_patterned),
    paste(act_diag$label[act_diag$country_patterned], collapse = ", "))

if (any(act_diag$near_constant)) {
  drop <- act_diag$variable[act_diag$near_constant]
  msg("  dropping near-constant actives: %s", paste(unname(ACT_LABELS[drop]), collapse = ", "))
  act <- act %>% select(-all_of(drop))
}

# -----------------------------------------------------------------------------
# RESTRICT THE SUBSTANTIVE TYPOLOGY TO SUFFICIENTLY DOCUMENTED RECORDS
# -----------------------------------------------------------------------------
n_blocks <- ncol(act) - 1
undoc_n  <- apply(act[, -1, drop = FALSE], 1,
                  function(r) sum(as.character(r) %in% undoc_levels))
documented_enough <- undoc_n <= 3
msg("  documented subset: %d of %d records carry design information on at least %d of %d blocks",
    sum(documented_enough), nrow(act), n_blocks - 3, n_blocks)

M$opacity_group <- as.integer(!documented_enough)
act_sub <- act[documented_enough, ]

collapse_rare <- function(f, min_p = 0.02) {
  tb <- prop.table(table(f)); rare <- names(tb)[tb < min_p]
  if (!length(rare)) return(droplevels(f))
  f <- as.character(f); f[f %in% rare] <- "Other (rare)"; factor(f)
}
act_mca <- act_sub %>% mutate(across(-case_id, collapse_rare))

# -----------------------------------------------------------------------------
# MCA + HCPC
# -----------------------------------------------------------------------------
## 1 REVERTED an ncp change. ncp MUST stay at CFG$mca_dims: HCPC() below is
## passed the whole `mca` object and therefore clusters on ALL stored components,
## so raising ncp silently re-partitioned the corpus (271/122/198/82 became
## 184/52/309/128). The eigenvalue-table version dependence is fixed at the
## denominator instead -- see the Benzecri note below.
mca <- FactoMineR::MCA(as.data.frame(act_mca[, -1]), graph = FALSE, ncp = CFG$mca_dims)

Q  <- ncol(act_mca) - 1
ev <- mca$eig[, 1]
ev_b <- ifelse(ev > 1 / Q, ((Q / (Q - 1)) * (ev - 1 / Q))^2, 0)
## the Benzecri share is taken over the RETAINED dimensions, not over
## every eigenvalue FactoMineR happens to return. Some releases truncate mca$eig
## to ncp rows and others return it in full, so the old denominator sum(ev_b)
## summed a different number of terms on different machines and moved
## pct_benzecri on every row. The retained set is fixed by CFG$mca_dims, so this
## is deterministic, and the column now means what it says: the share of retained
## Benzecri-corrected inertia carried by each dimension the clustering uses.
.keep <- seq_len(min(CFG$mca_dims, length(ev)))
inertia <- tibble(dim = seq_along(ev), eigen = ev, pct_raw = mca$eig[, 2],
                  benzecri = ev_b, pct_benzecri = 100 * ev_b / sum(ev_b[.keep])) %>%
  ## report exactly the dimensions the clustering USES (CFG$mca_dims). The
  ## row count previously depended on how many eigenvalues the installed
  ## FactoMineR returned for ncp = 5, which differed across versions (10 rows vs
  ## 5) while changing nothing downstream -- the clustering reads 5 dimensions
  ## either way, which is why the coordinates, Jaccards and class profiles all
  ## matched across machines. Truncating makes the table both deterministic and
  ## a truthful statement of what was retained.
  mutate(cum_pct_benzecri = cumsum(pct_benzecri)) %>% filter(dim <= CFG$mca_dims)
save_tab(inertia, "08_mca_inertia")

coords <- mca$ind$coord[, seq_len(CFG$mca_dims), drop = FALSE]
dm   <- dist(coords)
tree <- hclust(dm, "ward.D2")

ch_index <- function(x, cl) {
  k <- length(unique(cl)); n <- nrow(x); gc <- colMeans(x)
  bss <- sum(sapply(split(seq_len(n), cl), function(i)
    length(i) * sum((colMeans(x[i, , drop = FALSE]) - gc)^2)))
  wss <- sum(sapply(split(seq_len(n), cl), function(i)
    sum(scale(x[i, , drop = FALSE], scale = FALSE)^2)))
  (bss / (k - 1)) / (wss / (n - k))
}
diag_k <- map_dfr(CFG$k_range, function(k) {
  cl <- cutree(tree, k)
  tibble(k = k, avg_silhouette = mean(cluster::silhouette(cl, dm)[, 3]),
         calinski_harabasz = ch_index(coords, cl),
         min_cluster_share = min(table(cl)) / length(cl),
         max_cluster_share = max(table(cl)) / length(cl))
})
MIN_SHARE <- 0.05
eligible <- diag_k %>% filter(k >= 2, min_cluster_share >= MIN_SHARE, max_cluster_share <= .80)
if (!nrow(eligible)) eligible <- diag_k %>% filter(k >= 2)
k_best <- eligible$k[which.max(eligible$calinski_harabasz)]

hcpc_auto <- FactoMineR::HCPC(mca, nb.clust = -1, min = 2, max = max(CFG$k_range),
                              graph = FALSE, consol = FALSE)
diag_k <- diag_k %>% mutate(meets_min_share = min_cluster_share >= MIN_SHARE,
                            hcpc_automatic  = k == nlevels(hcpc_auto$data.clust$clust),
                            selected        = k == k_best)
save_tab(diag_k, "08_cluster_diagnostics")

hcpc <- FactoMineR::HCPC(mca, nb.clust = k_best, graph = FALSE, consol = FALSE)

typ <- rep(NA_character_, nrow(M))
typ[documented_enough]  <- paste("Class", as.character(hcpc$data.clust$clust))
typ[!documented_enough] <- "Opacity group"
M$typology_k <- factor(typ, levels = c(paste("Class", sort(unique(hcpc$data.clust$clust))),
                                       "Opacity group"))
msg("  k = %d selected on the documented subset (sizes %s); opacity group n = %d",
    k_best, paste(table(hcpc$data.clust$clust), collapse = ", "), sum(!documented_enough))


## ---- MACRO-ARCHITECTURE (k = 3, HCPC-automatic) ----------------------------
## Silhouette is maximised at k = 2, but that split separates on a single
## variable (maintenance support) and is substantively empty; CH keeps rising
## to 6, where cross-method agreement is weak (ARI ~ 0.2). The HCPC automatic
## rule selects k = 3: the smallest partition separated on SEVERAL design axes
## at once. Kirk: neutral Type labels on a common pedestal -- differences are
## DESCRIBED in TYPE_DESC, never baked into unequal name grammar.
cl3 <- cutree(tree, 3)
prof3 <- tibble(cl = cl3,
                ten   = M$el_tenant_direct[documented_enough] == 1,
                grant = M$i_grant[documented_enough] == 1,
                std   = M$r_standards[documented_enough] == 1,
                supp  = M$st_support[documented_enough] == 1) %>%
  group_by(cl) %>% summarise(across(everything(), ~ mean(.x, na.rm = TRUE)), n = n())
cl_direct  <- prof3$cl[which.max(prof3$ten - prof3$grant)]
rest       <- prof3 %>% filter(cl != cl_direct)
cl_instal  <- rest$cl[which.min(rest$supp)]
cl_compr   <- setdiff(prof3$cl, c(cl_direct, cl_instal))
macro_names <- setNames(c("Type 1", "Type 2", "Type 3"),
                        c(cl_direct, cl_compr, cl_instal))
TYPE_DESC <- c("Type 1" = "relatively more tenant-open and direct-provision",
               "Type 2" = "grant-dominant, broader technical and post-installation provision",
               "Type 3" = "grant-dominant, weaker post-installation provision")
tm <- rep(NA_character_, nrow(M))
tm[documented_enough]  <- unname(macro_names[as.character(cl3)])
tm[!documented_enough] <- "Opacity group"
M$typology_macro <- factor(tm, levels = c("Type 1", "Type 2", "Type 3", "Opacity group"))
sil3 <- mean(cluster::silhouette(cl3, dm)[, 3])
save_tab(bind_cols(prof3, name = unname(macro_names[as.character(prof3$cl)])),
         "08_macro_architecture_profiles")
save_tab(M %>% count(typology_macro), "08_macro_architecture_sizes")
save_tab(tibble(case_id = M$case_id[documented_enough],
                dim1 = coords[, 1], dim2 = coords[, 2],
                typology_macro = unname(macro_names[as.character(cl3)])),
         "08_mca_coords_macro")
msg("  macro architecture: k = 3, HCPC-automatic (avg silhouette %.2f); sizes %s",
    sil3, paste(table(cl3), collapse = ", "))

## export the MCA point cloud so the figures can draw the design space.
## case_id was omitted, so a point in the design space
## could not be traced back to the coding row that produced it -- which makes an
## outlier in the figure unverifiable. Every case-level export now carries it.
mca_out <- as_tibble(coords[, 1:2, drop = FALSE])
names(mca_out) <- c("Dim1", "Dim2")
mca_out$case_id        <- M$case_id[documented_enough]
mca_out$typology_macro <- as.character(M$typology_macro[documented_enough])
mca_out$typology_k     <- as.character(M$typology_k[documented_enough])
mca_out <- mca_out %>% select(case_id, Dim1, Dim2, everything())
save_tab(mca_out, "08_mca_coordinates")

## ---- Bootstrap Jaccard stability (approximate: fixed MCA space) ------------
set.seed(CFG$seed)
boot_jac <- function(k, B = 200) {
  base_cl <- cutree(tree, k)
  sapply(seq_len(B), function(b) {
    idx <- sample(nrow(coords), replace = TRUE)
    tb  <- hclust(dist(coords[idx, , drop = FALSE]), "ward.D2")
    cb  <- cutree(tb, k)
    sapply(seq_len(k), function(cl)
      max(sapply(seq_len(k), function(cl0)
        sum(base_cl[idx] == cl0 & cb == cl) /
          sum(base_cl[idx] == cl0 | cb == cl))))
  }) |> rowMeans()
}
jac <- bind_rows(tibble(k = 2, class = seq_len(2), jaccard = boot_jac(2)),
                 tibble(k = 3, class = seq_len(3), jaccard = boot_jac(3)),
                 tibble(k = k_best, class = seq_len(k_best), jaccard = boot_jac(k_best)))

## THE CLASS INDEX HERE IS NOT THE PROFILE CLASS LABEL.
## boot_jac() indexes by cutree(tree, k) -- a plain Ward cut -- while the
## "Class N" labels used everywhere else (typology_k, the profile tables, the
## figures) come from hcpc$data.clust$clust, which relabels after its k-means
## consolidation step. The two agree on MEMBERSHIP exactly (a clean 1:1
## relabelling, best-match agreement 1.000) but NOT on the index, so reading a
## Jaccard row against a profile row of the same number attributes the wrong
## stability to the wrong class. The mapping is now computed and carried in the
## table itself, and emitted separately, so the two can never be misaligned.
.cut_best <- cutree(tree, k_best)
.hc_best  <- as.character(hcpc$data.clust$clust)
.xt <- table(.cut_best, .hc_best)
.map <- vapply(seq_len(k_best), function(i) {
  .r <- .xt[as.character(i), , drop = TRUE]
  paste("Class", colnames(.xt)[which.max(.r)])
}, character(1))
.agree <- sum(apply(.xt, 1, max)) / sum(.xt)
CLASS_INDEX_MAP <- tibble(k = k_best, class = seq_len(k_best),
                          profile_label = .map,
                          n_in_class = as.vector(table(.cut_best)),
                          membership_agreement = round(.agree, 4))
save_tab(CLASS_INDEX_MAP, "08_class_index_map")
msg("Class index map (cutree -> profile label): %s (agreement %.4f)",
    paste(sprintf("%d->%s", seq_len(k_best), .map), collapse = ", "), .agree)

jac <- jac %>%
  left_join(CLASS_INDEX_MAP %>% select(k, class, profile_label), by = c("k", "class")) %>%
  mutate(profile_label = ifelse(is.na(profile_label),
                                sprintf("k=%d cut %d (no finer-partition label)", k, class),
                                profile_label))
save_tab(jac, "08_bootstrap_jaccard")
msg("  bootstrap Jaccard (fixed-space approximation): k=2 %.2f, k=3 %.2f, k=%d %.2f",
    mean(jac$jaccard[jac$k == 2]), mean(jac$jaccard[jac$k == 3]), k_best, mean(jac$jaccard[jac$k == k_best]))

## ---- v-test description of each class --------------------------------------
desc_tab <- imap_dfr(hcpc$desc.var$category, function(tab, cl) {
  as.data.frame(tab) %>% tibble::rownames_to_column("category") %>%
    mutate(class = cl) %>% as_tibble()
}) %>%
  rename(pct_cat_in_class = `Cla/Mod`, pct_class_with_cat = `Mod/Cla`,
         pct_overall = Global, v_test = v.test, p_value = p.value) %>%
  filter(p_value < 0.01) %>% arrange(class, desc(v_test))
save_tab(desc_tab, "08_class_vtest_description")

# -----------------------------------------------------------------------------
# SUPPLEMENTARY PROJECTION: fit compared across classes, never clustered on
# -----------------------------------------------------------------------------
sup_profile <- M %>%
  group_by(typology_k) %>%
  summarise(n = n(),
            `Documented access fit`   = mean(access_fit == "Documented fit", na.rm = TRUE),
            `Two or more enablers`    = mean(n_enablers >= 2, na.rm = TRUE),
            `Basic or broad admin`    = mean(admin_profile %in%
              c("Basic applicant pathway", "Broad learning + compliance support"), na.rm = TRUE),
            `Maintenance support`     = mean(st_support == 1, na.rm = TRUE),
            `Published monitoring`    = mean(mon_indicators == 1, na.rm = TRUE),
            .groups = "drop")   # weakness counts are added in 09, after this module
save_tab(sup_profile, "08_typology_supplementary_fitness_profile")

## ---- Robustness: PAM on Gower distances, adjusted Rand index ---------------
ari <- function(a, b) {
  tb <- table(a, b); n <- sum(tb); ch2 <- function(x) x * (x - 1) / 2
  s_ij <- sum(ch2(tb)); s_i <- sum(ch2(rowSums(tb))); s_j <- sum(ch2(colSums(tb)))
  e_i <- s_i * s_j / ch2(n)
  (s_ij - e_i) / (0.5 * (s_i + s_j) - e_i)
}
gow  <- cluster::daisy(as.data.frame(act_mca[, -1]), metric = "gower")
pamk <- cluster::pam(gow, k = k_best, diss = TRUE)
save_tab(tibble(comparison = "HCPC vs PAM-Gower at same k",
                adjusted_rand_index = ari(hcpc$data.clust$clust, pamk$clustering),
                pam_avg_silhouette  = pamk$silinfo$avg.width,
                hcpc_avg_silhouette = diag_k$avg_silhouette[diag_k$k == k_best]),
         "08_typology_robustness")

## ---- Validation against context, with effect size not just a p-value -------
val <- function(x, y, nm) {
  tb <- table(x, y)
  ts <- suppressWarnings(chisq.test(tb, simulate.p.value = TRUE))
  tibble(test = nm, p_value = ts$p.value, cramers_v = cramers_v(x, y))
}
save_tab(bind_rows(val(M$typology_k, M$region_mod, "class x region"),
                   val(M$typology_k, M$country_mod, "class x country")),
         "08_typology_validation")

save_tab(M %>% count(typology_k, region_mod) %>%
           group_by(typology_k) %>% mutate(share = n / sum(n)) %>% ungroup(),
         "08_typology_by_region")

fit_by_type <- map_dfr(c("access_fit", "tech_grade", "admin_profile",
                         "steward_config", "monitor_config"), function(v) {
  tb <- table(M$typology_k, as.character(M[[v]]))
  ts <- suppressWarnings(chisq.test(tb, simulate.p.value = TRUE))
  as_tibble(as.data.frame.table(prop.table(tb, 1), stringsAsFactors = FALSE)) %>%
    setNames(c("typology", "level", "share")) %>%
    mutate(domain = v, chisq_p = ts$p.value)
})
save_tab(fit_by_type, "08_fitness_by_typology")

# -----------------------------------------------------------------------------
# ACCESS AGAINST FINANCE -- an explicit effect estimate
#
# Section 3.4 states that access is inversely associated with financial support,
# and until now that claim rested on a comparison of class rates. A reader
# cannot tell from three percentages how large the association is, whether it
# survives the geography and delivery controls used elsewhere in the paper, or
# whether it is an artefact of the same programme appearing in several
# jurisdictions. This block answers all three with one record-level estimate.
#
# The outcome is direct tenant eligibility (C-10) and the exposure is a
# documented capital grant (i_grant), which are the two variables the macro
# profiles already report, so the estimate quantifies exactly the contrast the
# text describes rather than a new construct.
#
# Inference follows the house pattern. Standard errors are clustered on
# programme family rather than on country, because the dependence a reader will
# raise here is the same programme coded once per municipality served, and the
# model is refitted on one record per family as the stricter check.
# -----------------------------------------------------------------------------

af <- M[documented_enough, ] %>%
  transmute(tenant_direct = as.integer(el_tenant_direct == 1),
            grant         = as.integer(i_grant == 1),
            density_c, pop_c, lead_body, region_mod,
            prog_family, country) %>%
  filter(!is.na(tenant_direct), !is.na(grant))

## Descriptive first, so the model has something to be read against.
af_tab <- af %>%
  count(grant, tenant_direct) %>%
  tidyr::pivot_wider(names_from = tenant_direct, values_from = n,
                     names_prefix = "tenant_", values_fill = 0L) %>%
  mutate(n_programmes = tenant_0 + tenant_1,
         pct_tenant   = round(100 * tenant_1 / n_programmes, 1),
         grant        = ifelse(grant == 1, "Capital grant documented",
                                           "No capital grant documented"))
save_tab(af_tab, "08_access_finance_crosstab")

## (a) crude, (b) adjusted for the pre-specified geography and delivery
## controls, (c) one record per programme family.
af_crude <- logit_cl(tenant_direct ~ grant, data = af,
                     label = "Tenant eligibility ~ capital grant | crude",
                     cluster = "prog_family")
af_adj   <- logit_cl(tenant_direct ~ grant + density_c + pop_c + lead_body + region_mod,
                     data = af,
                     label = "Tenant eligibility ~ capital grant | adjusted",
                     cluster = "prog_family")
af_uniq  <- af %>% group_by(prog_family) %>% slice_head(n = 1) %>% ungroup()
af_fam   <- logit_cl(tenant_direct ~ grant + density_c + pop_c + lead_body + region_mod,
                     data = af_uniq,
                     label = "Tenant eligibility ~ capital grant | one record per programme family",
                     cluster = "country")

af_models <- bind_rows(af_crude$tab, af_adj$tab, af_fam$tab) %>%
  filter(term == "grant")
save_tab(af_models, "08_access_finance_effect")

msg(paste("  access against finance: tenant eligibility %.1f%% without a grant",
          "against %.1f%% with one; adjusted OR %.2f (95%% CI %.2f-%.2f, p = %.4f)"),
    af_tab$pct_tenant[af_tab$grant == "No capital grant documented"],
    af_tab$pct_tenant[af_tab$grant == "Capital grant documented"],
    af_adj$tab$or[af_adj$tab$term == "grant"],
    af_adj$tab$or_lo[af_adj$tab$term == "grant"],
    af_adj$tab$or_hi[af_adj$tab$term == "grant"],
    af_adj$tab$p.value[af_adj$tab$term == "grant"])
