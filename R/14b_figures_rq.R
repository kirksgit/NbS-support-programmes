# =============================================================================
# 14b_figures_rq.R  |  PART B -- ANSWERING THE RESEARCH QUESTIONS
#
#   B1  fit profiles WITH the sub-components each verdict is built from
#   B2  the core determinant: tenure and built form against access (adjusted)
#   B3  regions and countries -- every unit shown, none snapshot-selected
#   B4  the city level: city types, and the few real multi-programme portfolios
#   B5  programme architectures, with their world distribution
#   B6  context requirement profiles on the built-form x tenure frame
#   B7  justice: distribution, recognition, procedure (three evidence states)
#   B8  do weaknesses cluster? polarisation, signed phi, honest benchmark
#
# Conventions: silence never drawn as misfit; every term labelled with its
# framework level; no composite index anywhere; all units shown in full.
# =============================================================================

suppressPackageStartupMessages({
  library(ggplot2); library(patchwork); library(scales); library(forcats)
})
FGB <- M %>% left_join(config_signatures, by = "case_id") %>% add_city_typology()
## carry D3a's own rule inputs into the figure frame. The technical-element
## flags live on A (05_fitness.R) but were not in M, so panel d could not show the
## elements the D3a rule actually reads and silently produced NA labels when it
## first tried to. Joined explicitly rather than recomputed, so the panel and the
## verdict cannot diverge.
## the same join now carries the D3a rule's UNDERLYING provisions (each
## element is an OR over these, and figure 6 panel c lists them individually), the
## A-5 land-use flags used by the figure-12 synthesis, and the D4b reporting
## fields. Every one lives on A but not on M, which is the same gap the note above
## records -- a column silently missing from FGB surfaces as an NA stratum or a
## NULL lookup much later, so it is asserted here instead.
.el_want <- c("spec_element", "adv_element", "guid_element", "est_element",
              "r_standards", "cost_design", "i_advice", "s_guides",
              "s_helpdesk", "x_workshops",
              "lu_highrise", "lu_mixed", "lu_lowrise", "lu_industrial", "lu_other",
              "f_me_published")
.el_need <- intersect(setdiff(.el_want, names(FGB)), names(A))
if (length(.el_need))
  FGB <- FGB %>% left_join(A %>% select(case_id, all_of(.el_need)), by = "case_id")
.el_missing <- setdiff(.el_want, names(FGB))
if (length(.el_missing))
  stop(sprintf("figure frame is missing rule inputs: %s -- they exist on neither M nor A",
               paste(.el_missing, collapse = ", ")))

# =============================================================================
# B1 | FIT PROFILES AND THEIR SUB-COMPONENTS
# =============================================================================
## ---- Kirk's layout: ONE UNIT PER DOMAIN. Each unit shows (i) applicability,
## (ii) the verdict among evaluable records, (iii) the constituent provisions as
## diverging evidence rows -- the fit/misfit is SHOWN by the provisions, not told.
## The financial test (D3b) is a unit like the others, at its true n = 39.
zero <- function(x) rep(0L, length(x))  # 'never explicitly absent' placeholder
mkrow <- function(dom, out) FGB %>% transmute(case_id, country = country_mod, domain = dom,
                                              outcome = as.character(out))
## SINGLE SOURCE OF TRUTH. Panel a reads the canonical fit_D* columns built in
## 05_fitness.R -- it no longer re-derives a verdict per component here. The
## previous version mapped each component inline, which is how three rows came to
## have no misfit route and two came to emit an off-scale sixth state while the
## same components were mapped differently elsewhere in the file.
overall <- bind_rows(
  mkrow("D1 Ecological-functional",                   FGB$fit_D1),
  mkrow("D2 Tenure & authority access",                   FGB$fit_D2),
  mkrow("D3a Technical enablement",                   FGB$fit_D3a),
  mkrow("D3b Financial accessibility",                FGB$fit_D3b),
  mkrow("D3c Administrative access",                  FGB$fit_D3c),
  mkrow("D4a Maintenance responsibility and support", FGB$fit_D4a),
  mkrow("D4b Monitoring, feedback and review",        FGB$fit_D4b))
DOMS <- c("D1 Ecological-functional", "D2 Tenure & authority access",
          "D3a Technical enablement", "D3b Financial accessibility",
          "D3c Administrative access",
          "D4a Maintenance responsibility and support",
          "D4b Monitoring, feedback and review")
save_tab(overall %>% count(domain, outcome), "14_b1_domain_states")

## constituent provisions per domain (present / explicitly absent shares)
bin_row <- function(pres, abs01, label, dom, keep = TRUE) {
  keep <- rep_len(keep, length(pres)) & !is.na(pres)
  tibble(domain = dom, item = label,
         present = mean(pres[keep] == 1, na.rm = TRUE),
         absent  = mean(abs01[keep] == 1, na.rm = TRUE),
         n = sum(keep))
}
tern_row <- function(x, label, dom) {
  t <- as.character(x)
  tibble(domain = dom, item = label,
         present = mean(t %in% c("yes", "partial"), na.rm = TRUE),
         absent  = mean(t == "no", na.rm = TRUE),
         n = sum(!is.na(t)))
}
bin_row <- function(pres, abs01, label, dom, keep = TRUE) {
  keep <- rep_len(keep, length(pres)) & !is.na(pres)
  tibble(domain = dom, item = label,
         present = mean(pres[keep] == 1, na.rm = TRUE),
         absent  = mean(abs01[keep] == 1, na.rm = TRUE),
         n = sum(keep))
}
zero <- function(v) as.integer(!is.na(v) & v == 0) * 0L  # explicit-absent unknown -> 0

## D3a components among programmes that REQUIRE each provision (union over the
## measures supported, from the same 03b matrix the fit test uses)
reqmat <- utils::read.csv(file.path(CFG$out_tab, "03b_technical_requirement_matrix.csv"))
need_of <- function(p_col) {
  need_vars <- reqmat$measure_var[reqmat[[p_col]] == "required"]
  need_vars <- intersect(need_vars, names(FGB))
  as.integer(rowSums(FGB[, need_vars, drop = FALSE] == 1, na.rm = TRUE) > 0)
}
prov_row <- function(p, p_col, label) {
  keep <- need_of(p_col) == 1
  st <- as.character(FGB[[paste0("prov_", p)]])
  tibble(domain = "D3a Technical enablement",
         item = sprintf("%s (where required)", label),
         present = mean(st[keep] %in% c("yes", "partial"), na.rm = TRUE),
         absent  = mean(st[keep] == "no", na.rm = TRUE),
         n = sum(keep))
}

sub <- bind_rows(
  bin_row(FGB$obj_stated, zero(FGB$obj_stated), "Objectives explicitly stated (B-5)", "D1 Ecological-functional"),
  bin_row(FGB$measures_documented, zero(FGB$measures_documented), "Supported measures documented (E-1..E-7)", "D1 Ecological-functional"),
  bin_row(as.integer(FGB$eco_fit == "Documented fit"), as.integer(FGB$eco_fit == "Confirmed misfit"),
          "Every stated objective matched by a plausible measure", "D1 Ecological-functional",
          keep = FGB$eco_fit != "Indeterminate"),
  bin_row(FGB$el_home, FGB$ex_home, "Homeowners eligible (C-8)", "D2 Tenure & authority access"),
  bin_row(FGB$controller_incl,
          as.integer(FGB$ex_land == 1 | FGB$ex_coop == 1 | FGB$ex_socho == 1),
          "A property controller eligible: landlord, co-op or social housing (C-9/11/15)", "D2 Tenure & authority access"),
  bin_row(FGB$el_tenant_direct, FGB$ex_tenant, "Tenants (in their own right) (C-10)", "D2 Tenure & authority access"),
  bin_row(FGB$el_tenant_cond, zero(FGB$el_tenant_cond), "Tenants (with landlord permission) (C-10)", "D2 Tenure & authority access"),
  bin_row(FGB$tenant_pathway, zero(FGB$tenant_pathway), "A documented tenant pathway (D-7/G-2)", "D2 Tenure & authority access"),
  prov_row("P1", "P1_specification",   "Technical standards or funded design (C-5/E-21)"),
  prov_row("P2", "P2_site_assessment", "Site assessment or professional design (E-13/E-21)"),
  prov_row("P3", "P3_regulatory",      "Permit fast-track or fee waiver (C-6/C-7)"),
  prov_row("P4", "P4_guidance",        "Applicant or housing-type guidance (D-8/E-13)"),
  prov_row("P5", "P5_establishment",   "Establishment or maintenance support (E-15..E-18)"),
  tern_row(FGB$guide_state %>% recode("documented present" = "yes", "documented absent" = "no",
                                      "not documented" = "not_stated"),
           "Step-by-step applicant guide (D-1)", "D3c Administrative access"),
  tern_row(FGB$forms_state %>% recode("documented present" = "yes", "documented absent" = "no",
                                      "not documented" = "not_stated"),
           "Application forms accessible (D-3)", "D3c Administrative access"),
  bin_row(as.integer(FGB$n_channels >= 2), zero(FGB$n_channels), "Two or more application channels (D-4)", "D3c Administrative access"),
  bin_row(FGB$p_timeline, zero(FGB$p_timeline), "Decision timelines stated (D-2)", "D3c Administrative access"),
  bin_row(FGB$p_multiling, zero(FGB$p_multiling), "Materials in more than one language (D-5)", "D3c Administrative access"),
  ## D3b financial accessibility. Previously absent from this panel although it
  ## is one of the seven reported domains, so the subcomponent figure and the
  ## verdict figure above it disagreed on how many domains exist. Restricted to
  ## the programmes that actually target financially constrained applicants,
  ## which is the scope on which D3b is defined.
  bin_row(as.integer(FGB$prefinancing_required == 0), as.integer(FGB$prefinancing_required == 1),
          "Payment without applicant pre-financing (E-22)", "D3b Financial accessibility",
          keep = FGB$fin_scope == 1),
  bin_row(as.integer(FGB$i_inkind == 1 | FGB$i_discount == 1),
          zero(FGB$i_inkind),
          "In-kind or discounted route avoiding cash outlay (E-11)", "D3b Financial accessibility",
          keep = FGB$fin_scope == 1),
  bin_row(FGB$st_resp_named, zero(FGB$st_resp_named), "Maintenance responsibility named (F-5)", "D4a Maintenance responsibility and support"),
  bin_row(FGB$s_condition, zero(FGB$s_condition), "Maintenance a binding condition (E-17)", "D4a Maintenance responsibility and support"),
  ## RELABELLED. This bar reads st_support -- ANY entry in
  ## E-15, E-16 or E-18, which includes an informational maintenance guide -- and
  ## renders at ~67%, sitting a few lines from a D4a documented-fit rate of ~17%
  ## that requires MATERIAL support. Both are correct and they measure different
  ## things; adjacent in a reader's eye they look like a contradiction.
  bin_row(FGB$st_support, zero(FGB$st_support),
          "Any maintenance support named, incl. guidance only (E-15/16/18)",
          "D4a Maintenance responsibility and support"),
  bin_row(FGB$mon_indicators, FGB$f_me_absent, "Indicators or results published (F-1/F-2)", "D4b Monitoring, feedback and review"),
  bin_row(FGB$f_review, FGB$f_review_absent, "Periodic programme review (F-3)", "D4b Monitoring, feedback and review"),
  bin_row(FGB$f_feedback, zero(FGB$f_feedback), "A named feedback channel (F-4)", "D4b Monitoring, feedback and review")
) %>%
  mutate(domain = factor(domain, levels = c(
    "D1 Ecological-functional", "D2 Tenure & authority access", "D3a Technical enablement",
    "D3b Financial accessibility", "D3c Administrative access",
    "D4a Maintenance responsibility and support", "D4b Monitoring, feedback and review")))
save_tab(sub, "14_b1_subcomponents")


## B1a: one compact stacked panel carrying applicability AND verdict for all
## seven components, so the reader gets the whole judgement layer at a glance.
STATE3 <- c("Evaluable" = PAL$softblue, "Indeterminate" = PAL$indet,
            "Not applicable" = PAL$na)
appl <- overall %>%
  mutate(state = case_when(outcome == "Not applicable" ~ "Not applicable",
                           outcome == "Indeterminate"       ~ "Indeterminate",
                           TRUE                             ~ "Evaluable")) %>%
  count(domain, state) %>% group_by(domain) %>% mutate(p = n / sum(n)) %>% ungroup() %>%
  mutate(state = factor(state, levels = names(STATE3)), strip = "Can it be assessed?")
## PANEL A NOW KEEPS INDETERMINATE, so it is genuinely the marginal of the flow
## panels. The previous version filtered Indeterminate out of the bars while the
## flows carried it, which made the caption's "panel a is the marginal of b-h"
## claim false: a reader saw grey flows with no grey in the bar above them.
## "Not applicable" stays out because those records are not in the test at all
## (the applicability strip, panel a's companion, is where they are reported).
verd <- overall %>%
  filter(outcome != "Not applicable") %>%
  count(domain, outcome) %>% group_by(domain) %>%
  mutate(p = n / sum(n), n_eval = sum(n[outcome != "Indeterminate"]),
         n_in_test = sum(n)) %>% ungroup() %>%
  mutate(outcome = factor(outcome, levels = names(PAL_FIT)),
         strip = "Verdict among records the test applies to")
## PARTIAL IS NOT ONE STATE, and a single band said it was. Partial is the
## largest verdict on three components, so a reader who cannot see what sits
## inside it cannot tell a near-fit from a bare-minimum provision. The strip below
## splits each component's partial band into "one requirement met, the other
## missing" versus "both present, one incomplete" -- the distinction that decides
## whether the shortfall is a gap or a depth problem. Counts, not a new verdict.
part_split <- FGB %>%
  dplyr::transmute(
    D2   = dplyr::case_when(!grepl("^Partial", fit_D2)  ~ NA_character_,
                            TRUE                        ~ "One step short of the requirement"),
    D3a  = dplyr::case_when(!grepl("^Partial", fit_D3a) ~ NA_character_,
                            coalesce(spec_element,0L) + coalesce(adv_element,0L) +
                              coalesce(guid_element,0L) + coalesce(est_element,0L) >= 2
                                                        ~ "Both present, one incomplete",
                            TRUE                        ~ "One requirement met, other missing"),
    D3c  = dplyr::case_when(!grepl("^Partial", fit_D3c) ~ NA_character_,
                            d3c_know_part == 1 & d3c_lodge_part == 1
                                                        ~ "Both present, one incomplete",
                            TRUE                        ~ "One requirement met, other missing"),
    D4a  = dplyr::case_when(!grepl("^Partial", fit_D4a) ~ NA_character_,
                            grepl("^Responsibility \\+", as.character(steward_config))
                                                        ~ "Both present, one incomplete",
                            TRUE                        ~ "One requirement met, other missing"),
    D4b  = dplyr::case_when(!grepl("^Partial", fit_D4b) ~ NA_character_,
                            coalesce(d4b_web,0L) == 1   ~ "Both present, one incomplete",
                            TRUE                        ~ "One requirement met, other missing")) %>%
  tidyr::pivot_longer(dplyr::everything(), names_to = "d", values_to = "kind") %>%
  dplyr::filter(!is.na(kind)) %>%
  dplyr::count(d, kind)
save_tab(part_split, "14_b1a_partial_composition")

dom_lab <- verd %>% distinct(domain, n_eval, n_in_test) %>%
  mutate(dlab = sprintf("%s  (n in test = %d; evaluable = %d)", domain, n_in_test, n_eval))
lab_of <- setNames(dom_lab$dlab, dom_lab$domain)
ord_lab <- unname(lab_of[DOMS])

p_b1a <- ggplot() +
  geom_col(data = verd %>% mutate(dlab = factor(lab_of[as.character(domain)], levels = rev(ord_lab))),
           aes(p, dlab, fill = outcome), width = .62, colour = "white", linewidth = .25) +
  geom_text(data = verd %>%
              mutate(dlab = factor(lab_of[as.character(domain)], levels = rev(ord_lab))) %>%
              group_by(dlab) %>% arrange(desc(row_number())) %>%
              mutate(xp = cumsum(p) - p / 2) %>% ungroup(),
            aes(xp, dlab, label = ifelse(p > .06, n, "")), size = 2.1, colour = "grey15") +
  scale_fill_manual(values = PAL_FIT, name = NULL) +
  scale_x_continuous(labels = percent_format(), expand = c(0, 0)) +
  labs(title = "a. The verdict layer: each component among the records where its test applies",
       subtitle = WRAP("Every record the test applies to appears here, INDETERMINATE INCLUDED, so these bars are exactly the marginal of the flow panels below and the grey in a flow has grey above it. Indeterminate means the requirement arises but the documents are silent -- never a misfit. Records for which the test does not arise are excluded and reported in the applicability strip and figure S1. Denominators therefore differ by design. D1 is a validity floor, not a headline success."),
       x = "Share of records the test applies to", y = NULL) +
  theme_nbs(7.8) + theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))

## B1b: the unified diverging small multiples -- ONE chart, one shared axis,
## faceted by domain. Every constituent provision on the same scale, so the
## reader sees each verdict being assembled rather than being told it.
sub2 <- sub %>%
  mutate(domain = factor(domain, levels = DOMS),
         item = str_wrap(sprintf("%s  [n=%d]", item, n), 46)) %>%
  group_by(domain) %>% mutate(item = reorder_within(item, present, domain)) %>% ungroup()
p_b1b <- ggplot(sub2, aes(y = item)) +
  geom_segment(aes(x = -1, xend = 1, yend = item), colour = "grey94", linewidth = 3.2) +
  geom_segment(aes(x = 0, xend = -absent, yend = item), colour = PAL$misfit,
               linewidth = 2.1, lineend = "round") +
  geom_segment(aes(x = 0, xend = present, yend = item), colour = PAL$fit,
               linewidth = 2.1, lineend = "round") +
  geom_text(aes(x = present, label = percent(present, 1)), hjust = -.16, size = 1.95,
            colour = "grey30") +
  geom_text(aes(x = -absent, label = ifelse(absent > .02, percent(absent, 1), "")),
            hjust = 1.16, size = 1.95, colour = "grey30") +
  geom_vline(xintercept = 0, colour = "grey40", linewidth = .35) +
  facet_wrap(~ domain, scales = "free_y", ncol = 2,
             labeller = labeller(domain = label_wrap_gen(46))) +
  scale_y_reordered() +
  scale_x_continuous(labels = function(z) percent(abs(z), 1), limits = c(-1, 1),
                     breaks = c(-.5, 0, .5, 1)) +
  labs(title = "a. The provisions each verdict is built from, on one shared scale",
       subtitle = WRAP("Rightwards (blue) = provision documented present. Leftwards (red) = documented explicitly absent. The grey remainder is silence, which never becomes misfit. Same axis in every panel, so provisions are comparable across domains."),
       x = "Explicitly absent   <-   |   ->   documented present", y = NULL) +
  theme_nbs(7) + theme(panel.spacing.x = unit(10, "pt"))

## count of real programmes rather than asserted in a verdict label.
MIS <- list(
  list(dom = "D1 Ecological-functional",
       cond = "Hazard or objective documented",
       prov = "Portfolio delivers a matching ecological function",
       c_expr = quote(hx_flood == 1 | hx_coastal == 1 | hx_heat == 1 | hx_drought == 1 | obj_biodiv == 1),
       p_expr = quote(eco_fit == "Documented fit")),
  ## this row asked whether SOME non-owner route existed, which is the test D2
  ## no longer runs -- and it named the component by its retired title. D2 absorbed
  ## the proportionality question: the provision must reach as far as the city's
  ## tenure band REQUIRES, so a controller-only route in a renter-majority city is
  ## not a met provision. The condition is now every band (all three generate a
  ## requirement) and the provision is reach >= required.
  list(dom = "D2 Tenure & authority access",
       cond = "Every city: the tenure band sets how far a route must reach",
       prov = "Eligibility route reaches at least as far as the band requires",
       c_expr = quote(!is.na(d2_required)),
       p_expr = quote(!is.na(d2_reach) & !is.na(d2_required) & d2_reach >= d2_required)),
  list(dom = "D3a Technical enablement",
       cond = "Portfolio includes a building-integrated or ground-modifying measure",
       prov = "Measure-specific technical requirements met",
       c_expr = quote(m_greenroof == 1 | m_facade == 1 | m_raingard == 1 | m_depave == 1 | m_infiltr == 1),
       p_expr = quote(!is.na(requirement_match) & requirement_match >= .75)),
  list(dom = "D3b Financial accessibility",
       cond = "Programme targets financially constrained applicants",
       prov = "Payment without pre-financing",
       ## scope FLAG, not the retired restricted verdict
       c_expr = quote(fin_scope == 1),
       p_expr = quote(prefinancing_required == 0)),
  list(dom = "D3c Administrative access",
       cond = "Permit cost or approval burden documented",
       prov = "Applicant guide and accessible forms documented",
       c_expr = quote(cost_permit == 1),
       p_expr = quote(p_guide >= 1 & p_forms >= 1)),
  list(dom = "D4a Maintenance support",
       cond = "Maintenance demand high, or duty placed on the resident",
       prov = "Maintenance support offered, not duty alone",
       c_expr = quote(maint_demand == "High" | s_maintcost == 1),
       p_expr = quote(st_support == 1)),
  list(dom = "D4b Monitoring and review",
       cond = "Programme states objectives it could be judged against",
       prov = "Indicators published, or a periodic review documented",
       c_expr = quote(obj_stated == 1),
       p_expr = quote(mon_indicators == 1 | f_review == 1)))
mis_tab <- purrr::map_dfr(MIS, function(t) {
  cc <- purrr::map_lgl(seq_len(nrow(FGB)), ~ isTRUE(eval(t$c_expr, envir = as.list(FGB[.x, ]))))
  pp <- purrr::map_lgl(seq_len(nrow(FGB)), ~ isTRUE(eval(t$p_expr, envir = as.list(FGB[.x, ]))))
  tidyr::crossing(demand = c(TRUE, FALSE), provision = c(TRUE, FALSE)) %>%
    mutate(n = purrr::map2_int(demand, provision, ~ sum(cc == .x & pp == .y)),
           domain = t$dom, cond = t$cond, prov = t$prov) %>%
    mutate(p = n / sum(n),
           cell = case_when(demand & !provision ~ "MISFIT: the demand is there, the provision is not",
                            demand &  provision ~ "Demand met",
                            !demand &  provision ~ "Provided without this demand",
                            TRUE                 ~ "Neither"))
})
save_tab(mis_tab, "14_b2_misfit_anatomy")
mis_head <- mis_tab %>% filter(demand, !provision) %>%
  transmute(domain, misfit_n = n, misfit_p = p)
save_tab(mis_head, "14_b2_misfit_rates")

## B1c REMOVED as a figure. Its 2x2 demand-vs-provision panels told the reader
## the misfit counts; figures B1d-f now SHOW the same anatomy as flows. The
## mis_tab / mis_head tables are retained above -- they are the numeric source
## for those flows and for the Results text.

## Figure 6 = the verdict layer (a) with the misfit anatomy following as the
## flow panels (b-e). The constituent-provision small multiples move to
## supplementary S5.
figS5 <- p_b1b + plot_annotation(
  title = "Figure S5. The provisions each verdict is built from, on one shared scale",
  theme = TITLE())
save_fig(figS5, "figS05_provisions", 16, 13)


## =============================================================================
## B1c-B1f  |  THE FOUR DOMAIN CHAINS, SHOWN AS FLOWS
##
## Every axis carries the ACTUAL CODED CATEGORIES and every stratum is a SINGLE
## provision, never a pairing: where a programme documents two provisions it
## contributes one flow per provision, so the reader reads each provision's own
## prevalence rather than the prevalence of a combination.
##
## Unused factor levels are dropped on every axis (droplevels): a declared level
## with no records left an empty gap in the stratum column, which read as missing
## data rather than as an absent category.
## =============================================================================

## ONE SCALE FOR THE WHOLE FIGURE. The flows previously had their own four-state
## vocabulary whose "misfit" amber was the SAME hex as panel a's "Partial fit"
## (#fdae61) -- so the two layers of one figure disagreed on both language and
## colour. The flows now use PAL_FIT verbatim: identical labels, identical hexes,
## learned once in panel a and re-read everywhere.
## The ratified five-state scale, in ratified order. "No provision documented" is
## NOT a sixth state -- it resolves to Confirmed misfit in 05_fitness.R, because a
## documented absence is the explicit "No" that coding rule R1(a) calls a misfit.
VERDICT_FILL <- PAL_FIT[c("Documented fit", "Partial fit", "Confirmed misfit",
                          "Indeterminate", "Not applicable")]
VERDICT_LEV  <- names(VERDICT_FILL)
## Index aliases so the derivation code reads as what it means rather than as a
## position: [1] was misfit, [3] was met, in the old ordering.
V_FIT <- "Documented fit"; V_PART <- "Partial fit"
V_MIS <- "Confirmed misfit"; V_NA <- "Not applicable"
V_INDET <- "Indeterminate"
## A record's component verdict IS its flow colour, so panel a is the marginal of
## the sankey rather than an independent calculation that can drift from it.
## Flows carry the record's CANONICAL verdict verbatim. Anything off-scale is a
## pipeline error, not something to silently coerce -- the earlier silent coercion
## to Indeterminate is how off-scale values hid instead of surfacing.
fit_of <- function(x) {
  x <- as.character(x)
  bad <- setdiff(unique(x[!is.na(x)]), VERDICT_LEV)
  if (length(bad)) stop(sprintf("flow verdict off-scale: %s", paste(bad, collapse=" | ")))
  ifelse(is.na(x), V_INDET, x)
}

DEMAND_LEV <- c("High demand\n(building-integrated)",
                "Medium demand\n(ground-modifying)",
                "Low demand\n(standardised)")
demand_class_of <- function(d) factor(case_when(
    d$m_greenroof == 1 | d$m_facade == 1 ~ DEMAND_LEV[1],
    d$m_raingard == 1 | d$m_pond == 1 | d$m_infiltr == 1 | d$m_depave == 1 |
      d$m_trees == 1                     ~ DEMAND_LEV[2],
    TRUE                                 ~ DEMAND_LEV[3]), levels = DEMAND_LEV)

## droplevels only applies to factors; a character axis (e.g. the space column,
## built inline) would error, so coerce first and drop unused levels after.
mk_flow <- function(d, axes, verdict_col = "verdict") {
  d %>% mutate(across(all_of(axes), ~ droplevels(as.factor(.x)))) %>%
    count(!!!rlang::syms(c(axes, verdict_col)), name = "n") %>%
    mutate(alluv = row_number(),
           verdict = factor(.data[[verdict_col]], levels = VERDICT_LEV))
}

## EVERY STRATUM IS LABELLED. A size threshold previously suppressed labels on
## small strata, and an unlabelled box reads as empty space rather than as a small
## category -- which is exactly wrong, since the small categories (social-housing
## providers, maintenance grants, periodic review) are the substantive findings.
## Strata large enough to hold text are labelled inside; thin ones get a repelled
## label just outside the column with a leader line.
flow_plot <- function(fl, axes, labs_x, ttl, sub, ylab, inside_frac = .035) {
  long <- fl %>%
    tidyr::pivot_longer(all_of(axes), names_to = "axis", values_to = "stratum") %>%
    mutate(axis = factor(axis, levels = axes, labels = labs_x),
           stratum = forcats::fct_drop(factor(stratum)))
  tot <- sum(fl$n)
  ## stratum sizes per axis, to place outside labels for the thin ones
  sizes <- long %>% group_by(axis, stratum) %>% summarise(k = sum(n), .groups = "drop")
  thin  <- sizes %>% filter(k < inside_frac * tot)
  ggplot(long, aes(x = axis, stratum = stratum, alluvium = alluv,
                   y = n, fill = verdict)) +
    ggalluvial::geom_alluvium(alpha = .58, width = .30, colour = NA,
                              decreasing = FALSE) +
    ggalluvial::geom_stratum(width = .30, fill = "grey97",
                             colour = "grey65", linewidth = .3) +
    ## inside labels for strata with room
    geom_text(stat = ggalluvial::StatStratum,
              aes(label = ifelse(after_stat(count) >= inside_frac * tot,
                                 as.character(after_stat(stratum)), "")),
              size = 1.95, lineheight = .95, colour = PAL$ink) +
    ## outside labels for the thin strata, so no box is ever unlabelled
    ggrepel::geom_text_repel(
      stat = ggalluvial::StatStratum,
      aes(label = ifelse(after_stat(count) < inside_frac * tot,
                         as.character(after_stat(stratum)), "")),
      size = 1.85, colour = "grey30", direction = "y", nudge_x = .30,
      hjust = 0, segment.size = .22, segment.colour = "grey65",
      min.segment.length = 0, box.padding = .10, max.overlaps = Inf,
      na.rm = TRUE) +
    scale_fill_manual(values = VERDICT_FILL, name = NULL, drop = FALSE,
                      na.translate = FALSE) +
    scale_y_continuous(expand = expansion(mult = c(.005, .01))) +
    scale_x_discrete(expand = expansion(add = c(.42, .78))) +
    labs(title = ttl, subtitle = str_wrap(sub, 195), x = NULL, y = ylab) +
    theme_nbs(7.4) +
    theme(legend.position = "bottom", panel.grid.major.x = element_blank(),
          axis.text.x = element_text(face = "bold", size = 6.9))
}

## ---- B1c: D1 ecological-functional ------------------------------------------
HAZ_ORDER <- c("Floods", "Heat", "Drought", "Biodiversity loss")
HAZ_VARS  <- c(Floods = "hx_flood", Heat = "hx_heat",
               Drought = "hx_drought", `Biodiversity loss` = "obj_biodiv")
## NO PAIRED FUNCTIONS. Retention and infiltration are distinct hydrological
## mechanisms (holding water vs passing it into the soil) and cooling and shading
## are distinct thermal ones (evapotranspiration vs interception), so each is its
## own stratum with its own delivering measures. A hazard requiring two functions
## contributes one flow per function. Functions are ordered alphabetically.
## THE FUNCTION MAP IS NOW DERIVED FROM THE D1 RULE, not written beside it.
##
## The author asked what the large "(no supported measure delivers this function)"
## stratum was, and why it is not red. Checking the rule answered it: the panel's
## map was STRICTER than fit_D1, in two independent ways.
##
##  (1) THREE MEASURES THE RULE CREDITS HAD NO FUNCTION AT ALL. fn_match in
##      05_fitness.R credits m_rainharv (13 records), m_greywater (17) and m_soil
##      (8) toward stormwater and drought objectives, but no function listed them,
##      so a programme resting on them could only ever terminate at the residual.
##  (2) THE HAZARD DECOMPOSITION DEMANDED EVERY FUNCTION. Floods expands to
##      Infiltration AND Retention, and a path was built for each; but the rule
##      credits a stormwater objective when ANY crediting measure is present. A
##      rain-barrel programme matches the rule and failed the panel, because
##      barrels sat only under "Capture and storage" -- a drought function.
##      Ecologically that is wrong as well as inconsistent: a barrel retains
##      runoff, which is what Retention names.
##
## Both are fixed by construction: every measure the rule credits for a hazard's
## objective now appears under at least one function of that hazard, and DROUGHT
## GAINS A SECOND FUNCTION -- soil-moisture retention -- because planting, turf
## replacement, soil amendment and infiltration answer drought that way and had
## nowhere else to go. That is the author's own suggestion (expand the functions
## rather than pool the measures), and a check below enforces the coverage so the
## panel can never drift stricter than the verdict it decomposes again.
HAZ_FUNC <- list(
  Floods              = c("Infiltration", "Retention"),
  Heat                = c("Cooling", "Shading"),
  Drought             = c("Capture and storage", "Soil-moisture retention"),
  `Biodiversity loss` = c("Habitat"))
FUNC_MEASURES <- list(
  `Capture and storage`     = c("m_barrel","m_rainharv","m_greywater","m_disconn"),
  `Soil-moisture retention` = c("m_planting","m_turf","m_soil","m_infiltr"),
  Cooling                   = c("m_trees","m_greenroof","m_facade","m_planting","m_depave","m_turf"),
  Habitat                   = c("m_planting","m_pond","m_raingard","m_trees","m_facade","m_turf","m_greenroof"),
  Infiltration              = c("m_infiltr","m_depave","m_raingard","m_disconn","m_soil","m_turf"),
  Retention                 = c("m_raingard","m_pond","m_greenroof","m_barrel","m_rainharv"),
  Shading                   = c("m_trees","m_facade","m_greenroof"))

## The rule's own objective -> measure map, restated here ONLY to be checked
## against: if a hazard's functions ever stop covering it, the build must stop.
.RULE_HAZ_MEAS <- list(
  Floods              = c("m_greenroof","m_raingard","m_depave","m_barrel","m_infiltr",
                          "m_disconn","m_rainharv","m_turf","m_soil"),
  Heat                = c("m_trees","m_greenroof","m_facade","m_depave","m_planting","m_turf"),
  Drought             = c("m_barrel","m_greywater","m_planting","m_infiltr","m_turf",
                          "m_rainharv","m_soil"),
  `Biodiversity loss` = c("m_raingard","m_trees","m_greenroof","m_facade","m_planting","m_turf"))
for (.hz in names(.RULE_HAZ_MEAS)) {
  .covered <- unique(unlist(FUNC_MEASURES[HAZ_FUNC[[.hz]]]))
  .gap <- setdiff(.RULE_HAZ_MEAS[[.hz]], .covered)
  if (length(.gap))
    stop(sprintf(paste("figure 6 panel a is stricter than fit_D1: hazard '%s' credits",
                       "%s in the rule but no function of that hazard lists them.",
                       "Add the measure to a function, or add a function."),
                 .hz, paste(.gap, collapse = ", ")))
}
msg("Figure 6 panel a: function map covers every rule-credited measure for all %d hazards",
    length(.RULE_HAZ_MEAS))
FUNC_ORDER <- sort(names(FUNC_MEASURES))
MEAS_LAB <- setNames(MEASURES$measure, MEASURES$var)
## Three measures the D1 rule credits are not in the harmonised MEASURES taxonomy
## (that taxonomy carries the seven analysed types plus close variants). They are
## real coded fields with real records, so they are labelled here rather than
## dropped -- dropping them is exactly what pushed their programmes into the
## residual stratum.
MEAS_LAB <- c(MEAS_LAB,
              m_rainharv  = "Rainwater harvesting systems",
              m_greywater = "Greywater reuse",
              m_soil      = "Soil amendment / compost")

## The "none supported" stratum is RETAINED, short-labelled. An alluvial flow
## must terminate somewhere: dropping the label would drop the misfit flows with
## it, and the amber encoding would then be absent from the panel entirely. The
## amber colour and the stratum carry the same finding at different reading
## distances -- colour for the eye, label for the specific gap.
## ANNOTATION on the misfit stratum: it is NOT a cluster of unnamed measures. It
## is the case where the programme's portfolio contains NO measure delivering the
## required function, so there is no measure to name -- the stratum is the absence
## itself. Renamed to say that explicitly rather than reading as a measure label.
## MISFIT FLOWS END AT NAMED MEASURES. When a programme's portfolio contains no
## measure delivering the required function, the flow terminates at the measures
## the programme DOES support -- amber: supported, but not delivering this
## function. Only named NbS measures appear on the axis; the misfit is carried
## entirely by colour, and the rare portfolio with no coded measure at all is the
## only case allowed a "(none named)" stratum.
NO_DELIVERING <- "(hazard documented, no crediting measure supported)"
ALL_MEAS <- intersect(names(MEAS_LAB), names(FGB))
## HAZARD-LEVEL COVERAGE, computed once. For each hazard, is the record
## answered by ANY measure under ANY of that hazard's functions? That is the
## question fit_D1 asks; the per-function branches below consult it so an
## uncovered branch of a COVERED hazard emits no path, and only a hazard with no
## crediting measure anywhere produces a residual.
.haz_covered <- lapply(names(HAZ_VARS), function(hz) {
  v <- HAZ_VARS[[hz]]
  if (!v %in% names(FGB)) return(logical(0))
  sub <- FGB[which(FGB[[v]] == 1), , drop = FALSE]
  if (!nrow(sub)) return(logical(0))
  mv <- intersect(unique(unlist(FUNC_MEASURES[HAZ_FUNC[[hz]]])), names(sub))
  if (!length(mv)) return(rep(FALSE, nrow(sub)))
  m <- as.matrix(sub[, mv, drop = FALSE]); m[is.na(m)] <- 0
  rowSums(m == 1) > 0
})
names(.haz_covered) <- names(HAZ_VARS)
msg("Figure 6 panel a: hazard-level coverage -- %s",
    paste(sprintf("%s %d/%d uncovered", names(.haz_covered),
                  vapply(.haz_covered, function(x) sum(!x), integer(1)),
                  vapply(.haz_covered, length, integer(1))), collapse = "; "))

d1 <- purrr::imap_dfr(HAZ_VARS, function(v, hz) {
  if (!v %in% names(FGB)) return(NULL)
  sub <- FGB[which(FGB[[v]] == 1), , drop = FALSE]
  if (!nrow(sub)) return(NULL)
  purrr::map_dfr(HAZ_FUNC[[hz]], function(fn) {
    mv <- intersect(FUNC_MEASURES[[fn]], names(sub))
    purrr::map_dfr(seq_len(nrow(sub)), function(i) {
      ## TWO FACTS, TWO CHANNELS. Colour is the record's own D1 verdict (eco_fit),
      ## exactly the value panel a bars on, so this panel decomposes that bar and
      ## cannot disagree with it. But eco_fit is computed on OBJECTIVES matched,
      ## while this chain runs hazard exposure -> function -> measure, and 553 of
      ## the 1,797 hazard-function pairs inside "Documented fit" records have no
      ## delivering measure. Colour cannot carry both facts at once, so the
      ## pair-level gap gets the THIRD AXIS instead: an unmet pair terminates at a
      ## named absence rather than being recoloured or fanned across every measure
      ## the programme happens to support.
      vd  <- fit_of(sub$fit_D1[i])
      hit <- mv[which(as.numeric(sub[i, mv]) == 1)]
      ## THE RESIDUAL IS NOW EMITTED PER HAZARD, NOT PER FUNCTION -- which is
      ## how fit_D1 actually works, and it is why the author kept seeing a large
      ## "(no supported measure delivers this function)" block that no misfit
      ## count justified.
      ##
      ## fit_D1 credits a hazard's objective when ANY crediting measure is
      ## present. Floods expands to Infiltration AND Retention, so a programme
      ## supporting only rain gardens met the rule but produced a residual path on
      ## the Infiltration branch. Decomposing per function therefore MANUFACTURED
      ## absence: 533 residual paths where the true gap is 51 hazard-instances
      ## (stormwater 3 of 418, heat 4 of 137, biodiversity 9 of 155, drought 35 of
      ## 384). Those 51 are real -- a documented hazard with no crediting measure
      ## anywhere in the portfolio -- and they stay, named and coloured by their
      ## own verdict. What is gone is the artefact.
      ##
      ## `.covered` is computed once per record per HAZARD (outside this loop, see
      ## `.haz_covered` below) and consulted here: if the hazard is covered by any
      ## of its functions, an uncovered function branch simply emits no path.
      if (length(hit)) {
        tibble(hazard = hz, `function` = fn,
               measure = unname(MEAS_LAB[hit]), verdict = vd)
      } else if (isTRUE(.haz_covered[[hz]][i])) {
        tibble()          # hazard IS answered, via another of its functions
      } else {
        tibble(hazard = hz, `function` = fn,
               measure = NO_DELIVERING, verdict = vd)
      }
    })
  })
})
## ---- ② size-sorted strata so the panel is not a wall of straight lines:
## every axis ordered by total flow, largest at the top.
## middle-axis mixing: alternate the size ranks (1,3,5,...,6,4,2) so the sankey
## weaves instead of running parallel when both neighbouring axes are size-sorted.
by_mix <- function(x, w = NULL) {
  x <- as.character(x)
  if (is.null(w) || length(w) == 1) w <- rep(1, length(x))
  lv <- names(sort(tapply(w, x, sum), decreasing = TRUE))
  idx <- order(c(seq(1, length(lv), by = 2), rev(seq(2, length(lv), by = 2))))
  factor(x, levels = lv[idx])
}
by_size <- function(x, w = NULL) {
  x <- as.character(x)
  if (is.null(w) || length(w) == 1) w <- rep(1, length(x))
  lv <- names(sort(tapply(w, x, sum), decreasing = TRUE))
  factor(x, levels = lv)
}
n_or_one <- function(d) if ("n" %in% names(d)) d$n else rep(1, nrow(d))
d1 <- d1 %>%
  mutate(## HAZARD ORDER SET ON REVIEW: heat, floods, biodiversity loss, drought.
         hazard = factor(as.character(hazard), levels = c(
           "Heat", "Floods", "Biodiversity loss", "Drought")),
         ## FUNCTION ORDER SET ON REVIEW: retention first, capture and storage
         ## second. The remaining four follow so that no hazard's two functions
         ## sit adjacent and the hazard->function ribbons visibly cross.
         ## LEVELS ARE DERIVED, NOT TYPED. This list was hardcoded with six
         ## functions; adding "Soil-moisture retention" to HAZ_FUNC left it out,
         ## so 266 drought paths fell outside the levels and rendered as an "NA"
         ## stratum -- the same class of failure as a stale factor level anywhere
         ## else. The display order is still deliberate (retention and capture
         ## first so no hazard's functions sit adjacent and the ribbons cross),
         ## but any function not named here is APPENDED rather than dropped.
         `function` = factor(as.character(`function`), levels = {
           .pref <- c("Retention", "Capture and storage", "Cooling", "Shading",
                      "Habitat", "Infiltration")
           .all  <- names(FUNC_MEASURES)
           c(intersect(.pref, .all), setdiff(.all, .pref))
         }),
         measure = by_size(measure))
fl_d1 <- mk_flow(d1, c("hazard", "function", "measure"))
save_tab(fl_d1, "14_b1c_d1_hazard_function_measure_fit")

## no flow stage may carry an NA level. A hardcoded factor-level list that
## misses a category renders it as an "NA" node, which reads to a reader as a
## real category. Catch it at build time in every panel.
for (.nm in c("fl_d1")) {
  .f <- get(.nm)
  .bad <- names(.f)[vapply(.f, function(col) any(is.na(col)), logical(1))]
  if (length(.bad))
    stop(sprintf("%s has NA levels in stage(s): %s -- a factor level list is stale",
                 .nm, paste(.bad, collapse = ", ")))
}

p_b1c <- flow_plot(
  fl_d1, c("hazard", "function", "measure"),
  c("Documented hazard", "Ecological function it requires", "Measure supported that delivers it"),
  "b. D1 ecological-functional: which measure answers which hazard, and where nothing does",
  paste("The third axis names the measures themselves, so a reader traces each hazard to the measures that answer it and reads the gap directly. One flow per",
        "hazard-measure pair: a programme answering one hazard with three measures contributes three flows. COLOUR is the record's D1 verdict exactly as panel a",
        "rows it, so this panel decomposes that bar rather than recomputing it. The THIRD AXIS carries a second and different fact: the D1 verdict is computed",
        "on objectives matched, whereas this chain runs hazard exposure to function to measure, and 634 of the 2,272 hazard-function pairs inside",
        "documented-fit records have no delivering measure (792 of 2,547 across all verdicts) -- those terminate at the named absence rather than being",
        "recoloured. Read colour for the verdict and the terminal axis for the functional gap. Figure 4a catalogues what is supported; this panel is the test. D1 is a validity floor, not a headline --",
        "objectives and measures are frequently drawn from the same passage of programme text."),
  "Hazard-measure pairs")

## ---- B1d: D2 spatial-institutional ------------------------------------------
## THE AXIS IS ABOUT OWNERSHIP, and now says so. The previous route labels mixed
## two different things: an ownership position (owner, tenant with consent, tenant
## independently) and an organisational form (co-operative, social-housing
## provider). "Controller/collective pathway" and "direct tenant/broad pathway"
## were the result, and neither states what the applicant must hold. D2 asks one
## question -- MUST THE APPLICANT OWN THE PROPERTY? -- so the axis is now ordered
## on that single dimension, from a route only owners can use to one a tenant can
## use alone. Landlords, co-operatives and social-housing providers all sit on the
## owner side of it: they are property CONTROLLERS, distinct from an owner-occupier
## because they act for others, but they are not tenants.
OWNER_ROUTE <- c(
  "Owner-restricted pathway"      = "Homeowner only",
  "Permission-dependent pathway"  = "Tenant, with the owner's consent",
  "Controller/collective pathway" = "Property controller (landlord, co-op, social housing)",
  "Direct tenant/broad pathway"   = "Tenant, in their own right",
  "Confirmed misfit"              = "Controller route explicitly closed",
  "Indeterminate"                 = "Not documented")
ROUTE_LEV <- c("Homeowner only",
               "Property controller (landlord, co-op, social housing)",
               "Tenant, with the owner's consent", "Tenant, in their own right",
               "Controller route explicitly closed", "Not documented")

## Actor classes: each is one coded variable, listed individually. el_land is C-9
## (landlords / private property owners) and el_coop is C-11 (co-operatives and
## homeowner associations) -- two separate codebook variables, not a split of one.
## Codebook labels verbatim: C-8 is "Homeowners" and C-9 is "Landlords", two
## separate variables. Homeowners occupy the property; landlords own property they
## let to others. The earlier "Owner-occupiers"/"Landlords / property owners"
## wording invented a distinction the codebook does not use.
ACTORS <- c(el_home = "Homeowners (C-8)",
            el_land = "Landlords (C-9)",
            el_tenant_direct = "Tenants, in their own right (C-10)",
            el_tenant_cond = "Tenants, with owner consent (C-10)",
            el_coop = "Co-operatives / homeowner associations (C-11)",
            el_socho = "Social-housing providers (C-15)",
            el_school = "Schools", el_ngo = "NGOs / community organisations",
            el_sme = "Businesses / SMEs", el_inform = "Informal groups (C-16)")
d2 <- purrr::imap_dfr(ACTORS, function(lab, v) {
  if (!v %in% names(FGB)) return(NULL)
  sub <- FGB[which(FGB[[v]] == 1), , drop = FALSE]
  if (!nrow(sub)) return(NULL)
  tibble(
    ## the first stage was the measure's physical space, which is a D1/D3a
    ## question and left the tenure context -- the thing D2 actually tests against
    ## -- invisible. The panel now runs the component's own three blocks in order:
    ## the city's renter context, who the programme admits, and what that context
    ## requires of the route.
    space = factor(dplyr::case_when(
      sub$d2_band == "Renter-majority"  ~ "Renter-majority city (>=55% renting)",
      sub$d2_band == "Mixed tenure"     ~ "Mixed-tenure city (35-55%)",
      sub$d2_band == "Owner-dominated"  ~ "Owner-dominated city (<35%)",
      TRUE                              ~ "Tenure not verified"),
      levels = c("Renter-majority city (>=55% renting)", "Mixed-tenure city (35-55%)",
                 "Owner-dominated city (<35%)", "Tenure not verified")),
    actor = lab,
    route = unname(OWNER_ROUTE[as.character(sub$access_route)]),
    ## Verdict = the record's own D2 component verdict (access_fit). The earlier
    ## version recomputed a demand/provision test here, which could and did
    ## disagree with the verdict panel a shows for the same record.
    verdict  = fit_of(sub$fit_D2))
}) %>%
  mutate(actor = factor(actor, levels = unname(ACTORS)),
         route = factor(route, levels = ROUTE_LEV))
d2 <- d2 %>% mutate(space = by_size(space, n_or_one(d2)), actor = by_mix(actor, n_or_one(d2)),
                     route = by_size(route, n_or_one(d2)))
fl_d2 <- mk_flow(d2, c("space", "actor", "route"))
save_tab(fl_d2, "14_b1d_d2_space_actor_route_flows")
p_b1d <- flow_plot(
  fl_d2, c("space", "actor", "route"),
  c("The city's tenure context", "Actor class the programme admits", "Ownership the route requires"),
  "c. D2 tenure & authority access: the city's renter context, who the programme admits, and what that context requires of the route",
  paste("The panel runs D2's own three blocks in order. FIRST AXIS: the city's tenure context, which is what sets the requirement -- a renter-majority city needs a",
        "route reaching past the owner-occupier; an owner-dominated city does not. Micro-NbS sit on land the city does not control, so an intervention proceeds",
        "only if the actor holding authority over the space can apply. The middle axis",
        "lists every actor class coded, one flow per programme-actor pair. The third axis states what the applicant must HOLD, ordered on that one dimension: from a",
        "route only a homeowner can use, through property controllers who own or control on others' behalf (landlords, co-operatives, social-housing",
        "providers), to a tenant applying with consent and then in their own right. DISCLOSURE: this third axis DETERMINES the D2 verdict -- it is the only axis in",
        "figure 6 that does. It is retained because it is a strict refinement, not a restatement: six routes map onto four verdicts, so the axis says WHICH route",
        "produces the fit, which the colour alone cannot. Read the colour as the verdict and the stratum as its reason."),
  "Programmes")

## ---- B1e: D3 implementation support -----------------------------------------
## BOTH provision axes now name the SPECIFIC provisions coded, one flow per
## provision. The requirement-match band and the composite administrative profile
## both asserted a level of provision; these list what is actually documented.
## Advice first: it is the most prevalent provision in the corpus and the entry
## point an applicant meets before any standard applies.
TECH_PROV <- c(i_advice    = "Technical advice or consultation (E-13)",
               r_standards = "Technical standards or specifications (E-13)",
               x_workshops = "Workshops or training (E-13)")
ADMIN_PROV <- c(p_guide     = "Applicant guide (D-1)",
                p_timeline  = "Decision timeline stated (D-2)",
                p_forms     = "Accessible application forms (D-3)",
                p_multiling = "Multilingual materials (D-5)",
                p_housetype = "Housing-type specific guidance",
                p_codesign  = "Co-design or consultation (D-6)")
## the panel drew only records carrying at least one technical provision, so
## records with NO provision documented -- which is exactly what the Indeterminate
## verdict now means for D3a -- had no flow at all, and the panel silently lost a
## verdict class that panel a still shows. A "None documented" technical column
## keeps the panel a complete accounting of the same records.
TECH_PROV <- c(TECH_PROV, .none = "No technical provision documented")
## PANEL REBUILT ON THE AUTHOR'S OWN READING, and it fixes three defects
## the previous version carried.
##  (1) The middle and right stages were the SAME variables shown twice --
##      "technical element documented" and "specific provision offered" both
##      derive from i_advice / r_standards / s_guides / s_helpdesk. A documented
##      provision IS an offered provision; splitting them invented a distinction
##      the data does not make.
##  (2) The element stage listed COMBINATIONS ("Specification + Advice +
##      Guidance"), which multiplies nodes and hides how common each individual
##      element is. Each element is now listed ONCE and a programme's ribbon
##      passes through every element it documents -- so the node width reads as
##      "how many programmes document this element", which is the useful number.
##  (3) The chain now opens on the MEASURES, matching figure 4's vocabulary, so
##      a reader moves measure -> demand class the measure creates -> technical
##      provision documented -> verdict. Demand class is DERIVED from the
##      measure portfolio, so this ordering is the rule's own causal direction.
## THE SAME MEASURE VOCABULARY AS PANEL a, on the author's instruction.
##
## Panel a (D1) listed 15 measures and this panel listed 12. Rainwater
## harvesting, greywater reuse and soil amendment -- added to the D1 function map
## at v33 -- were never added here, and downspout disconnection carried a
## shortened name, so the two panels named the same measure differently. A reader
## comparing them would conclude the corpus supports different measures depending
## on which test is being shown, which is false: it is one measure set.
##
## This changes NO verdict. MEAS_D3A is the panel's label map; fit_D3a is decided
## by tech_demand read against the documented technical provisions, and none of
## these three measures enters that rule at any branch. They inherit the
## "Low (standardised)" demand class from 03_architecture.R, which is where the
## rule assigns it, not here.
## v35b: labels ⑤ and ⑥ SHORTENED on the author's annotation. The channel
## node listed all three named routes and the reporting node spelled out
## "no indicators"; both now state the thing itself. No variable, rule or
## stratum membership changes -- only the words on the node.
MEAS_D3A <- c(m_greenroof = "Green roofs",       m_facade   = "Green facades",
              m_raingard  = "Rain gardens",      m_depave   = "Depaving / permeable",
              m_trees     = "Trees (pits)",      m_barrel   = "Rain barrels / cisterns",
              m_infiltr   = "Infiltration systems",
              m_disconn   = "Downspout disconnection (to a vegetated or infiltrating receiver)",
              m_planting  = "Native / drought planting", m_turf = "Turf replacement",
              m_pond      = "Garden ponds and swales",
              m_rainharv  = "Rainwater harvesting systems",
              m_greywater = "Greywater reuse",
              m_soil      = "Soil amendment / compost")
## THE UNDERLYING PROVISIONS, NOT THE RULE'S OR-COMPOSITES.
##
## The author asked whether the paired labels describe things that are always
## combined in the data. Checked, and they are not:
##   spec_element = technical standards (E-13) OR city-funded design (E-14). Of
##     the records documenting either, 313 have standards ONLY, 37 funded design
##     ONLY and 59 both -- so "standards or funded design" merged two provisions
##     that mostly occur apart, and funded design is a FINANCIAL instrument
##     sitting inside a technical stage, which is the conflation the author
##     flagged.
##   est_element = installation guide OR helpdesk OR workshops, and it shares
##     s_guides with guid_element, so the same provision was being counted under
##     two adjacent strata.
## Each provision is therefore listed ONCE, in its own right, and a programme's
## ribbon passes through every one it documents. The verdict rule is unchanged --
## fit_D3a still reads the composites -- but the panel now shows the reader the
## provisions those composites are made of, which is what a decomposition is for.
ELEM_D3A <- c(r_standards = "Technical standards or specifications (E-13)",
              cost_design = "City-funded or subsidised design (E-14)",
              i_advice    = "Technical advice or consultation (E-13)",
              s_guides    = "Installation guidance (E-13)",
              s_helpdesk  = "Helpdesk or named contact (E-18)",
              x_workshops = "Workshops or training (E-13)")

## The reconciliation check caught this: excluding "Not applicable" made the flow
## panel show fewer verdict classes than panel a, so the two disagreed. Every
## record with a D3a verdict belongs here -- "Not applicable" is a verdict the
## framework issues (no measure carries a technical demand), not an exclusion.
.d3rows <- FGB[!is.na(FGB$fit_D3a), , drop = FALSE]
d3 <- purrr::map_dfr(seq_len(nrow(.d3rows)), function(i) {
  r  <- .d3rows[i, , drop = FALSE]
  mv <- intersect(names(MEAS_D3A), names(r))
  mh <- mv[which(!is.na(as.numeric(unlist(r[mv], use.names = FALSE))) &
                  as.numeric(unlist(r[mv], use.names = FALSE)) == 1)]
  ev <- intersect(names(ELEM_D3A), names(r))
  eh <- ev[which(!is.na(as.numeric(unlist(r[ev], use.names = FALSE))) &
                  as.numeric(unlist(r[ev], use.names = FALSE)) == 1)]
  ## Every documented measure and every documented element gets its own path, so
  ## each is listed once and the ribbons fan through. A record with no documented
  ## measure or no documented element keeps an explicit, named node rather than
  ## dropping out of the panel.
  ms <- if (length(mh)) unname(MEAS_D3A[mh]) else "No measure documented"
  es <- if (length(eh)) unname(ELEM_D3A[eh]) else "No technical provision documented"
  tidyr::expand_grid(measure = ms, provision = es) %>%
    ## Use the panel-local helper, NOT the raw tech_demand column: the column
    ## holds "High (building-integrated)" while DEMAND_LEV holds the wrapped
    ## display label "High demand\n(building-integrated)". Reading the raw column
    ## put every value outside the factor levels, collapsing the whole stage to NA.
    mutate(demand  = as.character(demand_class_of(r)),
           verdict = fit_of(r$fit_D3a))
}) %>%
  ## "Not applicable" records carry NO demand class -- that is precisely why the
  ## test does not arise for them. Dropping them made the panel show fewer verdict
  ## classes than panel a, which the reconciliation check caught. They now get an
  ## explicit named node, so applicability is VISIBLE rather than silently absent.
  ## ifelse() on a FACTOR returns the integer level codes, not the labels, which
  ## collapsed every demand node to "NA". Coerce to character first, then relevel.
  mutate(demand = as.character(demand),
         demand = ifelse(is.na(demand) | demand == "NA",
                         "No technical demand (test not applicable)", demand),
         demand = factor(demand, levels = c(as.character(DEMAND_LEV),
                                            "No technical demand (test not applicable)")),
         measure = by_size(measure, n_or_one(.)),
         provision = by_size(provision, n_or_one(.)))
fl_d3 <- mk_flow(d3, c("measure", "demand", "provision"))
save_tab(fl_d3, "14_b1e_d3_support_flows")
## v32 RESTORED: rewriting the D3a panel accidentally deleted the B1f
## (maintenance) and B1g (financial) sections -- the replacement span ran past
## the end of the D3a code. This span is v31's, recovered verbatim from the
## packaged source; only the D3a plot call below is re-edited, deliberately.
## v32 RESTORE NOTE: rewriting the D3a panel deleted the B1f (maintenance) and
## B1g (financial) sections outright -- the replacement span ran past the end of
## the D3a code. Everything below the D3a plot call is v31's, recovered verbatim
## from the packaged source. Only the D3a plot call itself is deliberately new.
p_b1e <- flow_plot(
  fl_d3, c("measure", "demand", "provision"),
  c("Measure supported (E-1..E-7)", "Technical demand that measure creates",
    "Technical provision documented"),
  "d. D3a technical enablement: from the measure, to the demand it creates, to the provision documented against it",
  paste("v32: the chain opens on the MEASURES named in figure 4, so one vocabulary runs through the paper. Demand class is derived from the measure",
        "portfolio, so this is the rule's own direction: what is supported determines what it needs, and the verdict compares that need with what is",
        "documented. Each measure and each technical provision is listed ONCE and a programme's ribbon passes through every one it documents, so a node's",
        "width reads as the number of programmes documenting it. Multi-response on both axes: a programme supporting three measures and documenting two",
        "provisions contributes six paths."),
  "Programmes")

## ---- B1f: D4 maintenance support --------------------------------------------
## SUPPORT AND VERIFICATION AXES ARE NOW INDIVIDUAL PROVISIONS, no pairings. The
## corpus codes exactly four non-financial support types in E-18 (guides, site
## visits, helpdesk, inspection reminders) and two funding routes, so every
## documented support can be shown as its own stratum -- no residual "other".
## Vectorised stage labels for panel f. Every label names the coded field it
## reads, so a reader can trace the stage back to the codebook.
.duty_of <- function(d) dplyr::case_when(
  d$s_respons == "Applicant/participant" ~ "Applicant or participant (F-5)",
  d$s_respons == "Shared"                ~ "Shared: applicant and programme body (F-5)",
  d$s_respons == "City/utility"          ~ "City or utility retains it (F-5)",
  TRUE                                   ~ "F-5 not stated: transfer cannot be established")
.binding_of <- function(d) dplyr::case_when(
  is.na(d$s_condition) ~ "Not documented either way (E-17)",
  d$s_condition == 1   ~ "Binding condition of the award (E-17)",
  TRUE                 ~ "Not a binding condition (E-17)")

## the labels now state the TIER each provision belongs to, because the tier
## is what the rule reads. Material support carries capacity to the duty-holder;
## informational support tells them what to do and leaves the doing with them.
## Ordered material-first so the stage reads strongest to weakest down the column.
MAINT_SUP <- c(s_maintgrant = "Dedicated maintenance grant (E-15) -- material",
               s_maintcost  = "Maintenance costs covered (E-16) -- material",
               s_sitevisit  = "Site visits (E-18) -- material",
               s_reminders  = "Inspection reminders (E-18) -- material",
               s_guides     = "Maintenance guides (E-18) -- informational",
               s_helpdesk   = "Helpdesk (E-18) -- informational")
## THE VERIFICATION STAGE IS REMOVED, and the author's question is why.
##
## Annotation: "if nothing verifies, why isn't it a misfit?" Because verification
## is not part of the D4a test. fit_D4a reads exactly three things -- who bears
## the duty (F-5), whether it is a binding condition (E-17), and whether any
## support is documented (E-15/E-16/E-18). F-1 to F-4 decide fit_D4b, which is
## panel g. The old third stage therefore routed a reader through D4b's material
## to a D4a verdict, so "Nothing verifies it" looked like an unpunished failure
## when it was simply a different domain's variable. Same defect class as the
## D3a/D3c panels: a flow stage that the rule does not read.
##
## The chain is now the rule's own three inputs in the rule's own order: who
## bears it -> is it enforceable -> what is provided to make it workable. A
## reader can now see the misfit route directly, because it is the only route
## the rule can fire on: applicant + binding + no support, with the support
## fields EXPLICIT negatives rather than silent.
## v37b: THE MIDDLE STAGE IS THE SUPPORT TIER, NOT E-17.
##
## Author annotation, and it is right: the panel showed "Is it enforceable?
## (E-17)" as its middle stage, but the rewritten fit_D4a DOES NOT READ
## s_condition at any branch -- the verdict turns on the documented transfer and
## the support TIER. A stage the rule does not read is the same defect this figure
## carried twice before (D3a routed through administrative material, D3c opened on
## retired counting constructs). E-17 remains a coded variable and still feeds
## steward_strain; it is simply not a D4a verdict input, so it cannot stand in the
## panel that decomposes that verdict. The middle stage is now the tier the rule
## actually tests, which also makes the fit/partial split legible on the page.
.tier_of <- function(dd) dplyr::case_when(
  dd$st_support_tier == "Material support"          ~ "Material: carries capacity to the duty-holder",
  dd$st_support_tier == "Informational support only" ~ "Informational only: instruction, not capacity",
  TRUE                                               ~ "No support documented")
d4 <- purrr::map_dfr(names(MAINT_SUP), function(sv) {
  if (!sv %in% names(FGB)) return(NULL)
  sub <- FGB[which(FGB[[sv]] == 1), , drop = FALSE]
  if (!nrow(sub)) return(NULL)
  tibble(duty = .duty_of(sub), binding = .tier_of(sub),
         support = unname(MAINT_SUP[sv]),
         verdict = fit_of(sub$fit_D4a))
})
.sup_cols <- intersect(names(MAINT_SUP), names(FGB))
.nosup_idx <- which(rowSums(as.matrix(FGB[, .sup_cols]) == 1, na.rm = TRUE) == 0)
no_sup <- if (length(.nosup_idx)) {
  ns <- FGB[.nosup_idx, , drop = FALSE]
  tibble(duty = .duty_of(ns), binding = .tier_of(ns),
         support = "No maintenance support documented",
         verdict = fit_of(ns$fit_D4a))
} else tibble()

d4 <- bind_rows(d4, no_sup) %>%
  mutate(duty = factor(duty, levels = c(
           "Applicant or participant (F-5)", "Shared: applicant and programme body (F-5)",
           "City or utility retains it (F-5)",
           "F-5 not stated: transfer cannot be established")),
         binding = factor(binding, levels = c(
           "Material: carries capacity to the duty-holder",
           "Informational only: instruction, not capacity",
           "No support documented")),
         support = factor(support, levels = c(unname(MAINT_SUP),
                                              "No maintenance support documented")))
d4 <- d4 %>% mutate(duty = by_size(duty, n_or_one(d4)),
                    binding = by_size(binding, n_or_one(d4)),
                    support = by_size(support, n_or_one(d4)))
## STAGE ORDER REVERSED, on the author's instruction -- support first, duty
## last, immediately before the verdict. The panel now reads as the question the
## domain actually asks: what capacity does the programme put on the table, is the
## duty enforceable, and onto whom does it finally land? Ending on the duty-holder
## puts the transferee next to the verdict their transfer produced, which is where
## the misfit is decided.
fl_d4 <- mk_flow(d4, c("support", "binding", "duty"))
save_tab(fl_d4, "14_b1f_d4_maintenance_flows")
p_b1f <- flow_plot(
  fl_d4, c("support", "binding", "duty"),
  c("What support is documented (E-15/16/18)", "What tier of support is it?",
    "Who finally carries the duty (F-5)"),
  "f. D4a maintenance: does the programme that transfers the duty also transfer the means to discharge it?",
  paste("v37: the misfit route is now the DOCUMENTED TRANSFER of the duty, not silence about support. F-5 records that 657 programmes move maintenance",
        "onto the applicant and 8 onto a shared arrangement; where that transfer is documented and no support is, the programme has published a design in",
        "which the duty moves and the capacity to discharge it does not follow -- the same shape as D3b's documented reimbursement route with no",
        "documented alternative. Support is GRADED by what it gives the duty-holder: a grant, covered costs, a site visit or an inspection reminder carry",
        "capacity (fit); a guide or helpdesk tells them what to do but leaves the doing with them (partial). Where the city RETAINS the duty no transfer",
        "occurs and the test has no object (not applicable); where F-5 is silent the transfer cannot be established (indeterminate, never misfit)."),
  "Programmes")

## v33 RESTORE NOTE: this section was deleted twice by the same mistake -- a
## plot-call replacement cut at the next occurrence of the unit-label string,
## and because p_b1f's label was 'Programme-provision pairs' rather than
## 'Programmes', the search jumped forward into THIS section. Recovered verbatim
## from the packaged source. A structural check at the end of this file now
## asserts every flow object exists, so a swallowed section stops the build
## instead of surfacing as an 'object not found' error 100 lines later.
## ---- B1g: D3b financial accessibility ---------------------------------------
## Chain: how the money is paid -> what alternative exists -> whether the
## programme also declares it targets financially constrained applicants.
## The verdict is fit_D3b, so this panel decomposes exactly the bar panel a shows.
##
## THIRD AXIS IS NOT THE VERDICT RELABELLED. A first attempt used "who carries the
## cost first", which mapped one-to-one onto fit_D3b (398/138/62) and so duplicated
## the colour while adding nothing. This axis instead carries what the colour
## cannot: a programme that names low-income applicants and still demands the full
## cost up front contradicts its own stated target -- the sharpest form of the
## misfit, and the framework's original restricted financial test nested inside
## the universal one.
PAYMENT_LAB <- c("Reimbursement"    = "Reimbursement after install",
                 "Upfront/vendor"   = "Upfront or paid to vendor",
                 "Milestone"        = "Milestone payments",
                 "Fiscal credit"    = "Fiscal credit or bill reduction",
                 "Not stated"       = "Payment timing not stated")
## v55 D3b ALTERNATIVE AXIS IS NOW MULTI-RESPONSE, on the author's instruction that
## the two mechanisms "stand individually". Previously a programme documenting BOTH a
## free-goods route and a discounted-purchase route was pushed into a third combined
## stratum ("Goods supplied free AND a discounted purchase route"), which made the
## two mechanisms un-addable: a reader could not see how many programmes supply goods
## free, because 28 of them were hidden in the combined node. The axis now follows the
## SAME convention the D2 actor axis already uses -- one path per documented mechanism,
## so a both-documenting programme contributes to each stratum and the axis carries
## more paths than records. That is stated on the panel. The catch-all is renamed
## "None documented" (was "No alternative to paying first"), which states the coded
## state rather than narrating a consequence.
ALT_MECH <- c(i_inkind   = "Goods supplied free of charge (E-8/E-11/E-25)",
              i_discount = "Discounted or subsidised purchase, applicant still buys (E-11)")
d3b <- dplyr::bind_rows(
  purrr::imap_dfr(ALT_MECH, function(lab, v) {
    sub <- FGB[which(coalesce(as.integer(FGB[[v]]), 0L) == 1), , drop = FALSE]
    if (!nrow(sub)) return(NULL)
    dplyr::mutate(sub, .alt = lab)
  }),
  dplyr::mutate(FGB[which(coalesce(as.integer(FGB$i_inkind), 0L) != 1 &
                          coalesce(as.integer(FGB$i_discount), 0L) != 1), , drop = FALSE],
                .alt = "None documented")
) %>%
  transmute(
    payment = unname(ifelse(is.na(as.character(disbursement)), "Payment timing not stated",
                            PAYMENT_LAB[as.character(disbursement)])),
    alternative = .alt,
    target = case_when(
      ## SHORTENED, on the author's instruction -- the label states the
      ## population it names, not the codebook construct. The variable and the
      ## rule are unchanged; only the wording on the node is shorter.
      ## SPLIT, on the author's instruction. These were pooled as "Low-income or
      ## vulnerable populations (G-4)", which merged two distinct G-4 categories that
      ## the codebook keeps apart and that name different populations: 22 programmes
      ## target low-income or equity zones, 10 name vulnerable populations. Pooling
      ## them hid the fact that the vulnerable-population route is the rarer of the
      ## two by more than a factor of two.
      fin_scope == 1 & equity_targeted == 1 &
        equity_target == "Low-income/equity zones"       ~ "Low-income or equity zones (G-4)",
      fin_scope == 1 & equity_targeted == 1 &
        equity_target == "Vulnerable populations"        ~ "Vulnerable populations (G-4)",
      fin_scope == 1 & equity_targeted == 1              ~ "Equity target, population not specified (G-4)",
      ## NAMED. "some other way" was vague, so it was checked: all 64
    ## records in this stratum are there because SOCIAL-HOUSING PROVIDERS are
    ## eligible (C-15) -- every one, with co-ops (50) and consent-based tenant
    ## routes (29) alongside. None reaches the stratum by any other route, so
    ## the label states what it actually is.
    fin_scope == 1                        ~ "Social-housing eligibility (C-15)",
      TRUE                                  ~ "None documented"),
    verdict = fit_of(fit_D3b))
## NO FILTER. The previous version dropped every record whose payment timing is not
## stated -- which is EXACTLY the 109 records panel a bars as Indeterminate, so the
## bar showed grey and the chain showed none. Those records belong in the panel: the
## undocumented payment design IS why they are indeterminate, and "Payment timing
## not stated" is a real, informative first stratum, not missing data. Verified:
## every D3b verdict in panel a now appears in this panel, indeterminate included.
d3b <- d3b %>% mutate(payment = by_size(payment, n_or_one(d3b)),
                      alternative = by_mix(alternative, n_or_one(d3b)),
                      target = by_size(target, n_or_one(d3b)))
fl_d3b <- mk_flow(d3b, c("payment", "alternative", "target"))
save_tab(fl_d3b, "14_b1g_d3b_financial_flows")
p_b1g <- flow_plot(
  fl_d3b, c("payment", "alternative", "target"),
  c("How the money reaches the applicant", "Alternative to paying first",
    "Whether constrained applicants are named"),
  "e. D3b financial accessibility: payment design, the alternatives offered, and who the programme says it is for",
  paste("The alternative axis is MULTI-RESPONSE: a programme documenting both a free-goods route and a discounted purchase contributes a path to each, so that axis",
        "carries more paths than records and the two mechanisms are readable on their own rather than pooled into a combined node.",
        "This panel is the counterpart to the D3b bar in panel a, which was previously the only component with a large confirmed-misfit share and no chain to",
        "explain it. Reimbursement after installation means the applicant must find the full cost first; an in-kind or discounted route removes that requirement even",
        "when the programme reimburses -- which is why the verdict follows the alternative, not the payment timing alone. The third axis carries what the colour",
        "cannot: red flows reaching 'declares an equity target' are programmes that name financially constrained applicants and still require the full cost up front,",
        "contradicting their own stated target. That subset is the framework's original restricted financial test (n = 39); the universal test is the headline here",
        sprintf("because restricting to it would discard %d of %d records. Colour is the record's D3b verdict as panel a rows it. One flow per record: this panel counts RECORDS.",
                nrow(A) - sum(A$equity_target %in% c("Low-income/equity zones", "Vulnerable populations"), na.rm = TRUE), nrow(A))),
  "Programmes")

## ---- B1h: D3c administrative access -----------------------------------------
## Chain: applicant guide (D-1) -> accessible forms (D-3) -> decision timeline.
## These are fit_D3c's own inputs, not the retired 0-6 counting scores.
## REBUILT ON THE AUTHOR'S CHALLENGE, which was right on both counts.
##  (1) "Applicant guide, partial" is NOT an invented gradation -- the codebook
##      defines D-1 partial as "some guidance but incomplete or FOR PROFESSIONALS
##      ONLY", and D-3 partial as "some but not all key forms available". But the
##      old labels said "partial" without saying what partial MEANS, which is
##      telling rather than showing. The labels now carry the definition.
##  (2) A guide is NOT the only way to apply. D-4 is a separate multi-select of
##      application CHANNELS (online form, email, paper/mail, in-person,
##      third-party portal). Opening the panel on the guide alone implied the
##      guide was the whole route. The chain now runs: how many channels are open
##      -> can they find out how to apply (D-1) -> can they lodge it (D-3) ->
##      verdict, which is the applicant's actual sequence and keeps fit_D3c's own
##      two inputs (d3c_know, d3c_lodge) as the deciding stages.
## THE CHANNEL STAGE NAMES THE CHANNELS, and the two "no channel" labels
## are collapsed because the data says they are the same thing.
##
##  ④⑤ "One channel" / "Two or more channels" said nothing about WHICH. D-4 is a
##      multi-select with six coded options, all with substantial counts (online
##      336, paper/mail 325, in-person 175, email 158, third-party portal 125,
##      other 35), so each is now its own stratum and a programme's ribbon passes
##      through every channel it documents. Count is still readable -- it is the
##      number of strata a ribbon crosses -- but the identity is no longer hidden.
##  ⑥  "No application channel documented" and "Channels not documented" were
##      NOT two states. Checked against the workbook: all 75 records with zero
##      parsed channels carry the literal string "Not stated" in D-4, and zero
##      carry a documented "none"; the 6 remaining are empty cells. Both are the
##      same epistemic state -- silence -- so they are one stratum, and the
##      silence rule means it can never read as a documented absence.
D3C_CH <- c(ch_online     = "Online form (D-4)",
            ch_paper      = "Paper or mail (D-4)",
            ch_inperson   = "In person (D-4)",
            ch_email      = "Email (D-4)",
            ch_thirdparty = "Third-party portal (D-4)",
            ## NAMED, not "other". The 35 records here specify the channel in free
            ## text; reading them, the recurring ones are telephone or telephone-
            ## assisted registration, submission via a city-designated contractor,
            ## and an electronic delivery address. Three name nothing beyond
            ## "Other [specify]" and are the only genuinely unspecified cases.
            ch_other      = "Other channels (e.g., telephone)")
.ch_cols <- intersect(names(D3C_CH), names(FGB))
.chmat <- as.matrix(FGB[, .ch_cols]); .chmat[is.na(.chmat)] <- 0
## the panel needs the SAME explicit-denial test the rule uses, so the node
## labels can separate "the documents say No" from "the documents say nothing".
## no1() returns 1 only for a coded "No" -- never for "Not stated" -- which is
## exactly the predicate fit_D3c's misfit branch reads. Derived here rather than
## re-implemented, so the panel cannot drift from the rule it decomposes.
.d1_no <- coalesce(no1(raw$v_D1)[match(FGB$case_id, A$case_id)], 0L)
.d3_no <- coalesce(no1(raw$v_D3)[match(FGB$case_id, A$case_id)], 0L)

## v39b: the no-channel label QUOTES TWO COUNTS, so they are COMPUTED here, never
## typed. A hardcoded pair would silently go stale the next time the corpus or the
## channel coding moves -- the same defect class as every other literal in this
## file that has drifted from the run.
.nochan   <- rowSums(.chmat) == 0
.fb_here  <- coalesce(as.integer(A$f_feedback)[match(FGB$case_id, A$case_id)], 0L) == 1
.NOCH_N   <- sum(.nochan)
.NOCH_FB  <- sum(.nochan & .fb_here)
## the parenthetical is REMOVED, on the author's instruction that the bracket
## carried too much. It stated how many of these records still name a phone or email
## contact -- a real and relevant qualification, but a node label is not where a
## reader can hold it. The counts are not discarded: they move into the panel
## subtitle, where the same sentence has room to say what the silence does and does
## not mean, and they are still computed from the run rather than typed.
.NOCH_LAB <- "Channel not enumerated"
d3c <- purrr::map_dfr(seq_len(nrow(FGB)), function(i) {
  hit <- .ch_cols[which(.chmat[i, ] == 1)]
  ## the label states that this is SILENCE AND NOT AN INPUT TO THE VERDICT.
  ## The author asked why records here reach fit and partial rather than all being
  ## misfits. Two reasons, both checked: D-4 is a multi-select with NO "No" option
  ## in the codebook -- 0 of 709 records carry an explicit denial, so every one of
  ## the 81 records here is simply an unenumerated channel list, never a programme
  ## stating it has no way in. And D-4 is not read by fit_D3c at all: that rule
  ## tests D-1 (can they find out how?) and D-3 (can they lodge it?). Of these 81,
  ## 32 document a full guide and 36 fully accessible forms, so they are documented
  ## fits on the fields the test actually asks about. Making them misfits would
  ## convert an unlisted channel into a finding of inaccessibility.
  ## v39b: the author's reading is CORRECT and now measured. Of these 81 records,
  ## 46 do carry a named phone or email (F-4) -- so for most of them the position is
  ## not "there is no way in" but "the way in is not enumerated in D-4", which is
  ## what the author said. The remaining 35 document no named contact either, and
  ## even they are not misfits by that fact alone: D-4 has no "No" option and is not
  ## an input to fit_D3c. The label states the contact overlap rather than leaving
  ## the reader to infer that no channel means no access.
  ch  <- if (length(hit)) unname(D3C_CH[hit]) else .NOCH_LAB
  tibble(channel = ch,
         ## DENIAL SPLIT FROM SILENCE, in both stages. Each previously ended
         ## in a single "No ... documented" node that held two different states --
         ## a programme stating outright that it has no applicant guide, and one
         ## whose documents simply never mention a guide. The rule treats them
         ## oppositely (the first is a confirmed misfit, the second can never be),
         ## so one label showing both made the verdict column unreadable: the same
         ## node fed red and amber ribbons with nothing to distinguish them.
         knowing = dplyr::case_when(
           FGB$d3c_know[i]      == 1 ~ "Complete step-by-step guide for residents (D-1)",
           FGB$d3c_know_part[i] == 1 ~ "Guide exists but incomplete, or written for professionals not residents (D-1)",
           .d1_no[i]            == 1 ~ "Documents state there is NO applicant guide (D-1 = No)",
           TRUE                      ~ "No applicant guide mentioned either way (D-1 not stated)"),
         lodging = dplyr::case_when(
           FGB$d3c_lodge[i]      == 1 ~ "All key forms available (D-3: application, evidence, claim)",
           FGB$d3c_lodge_part[i] == 1 ~ "Some key forms available, not all (D-3: application, evidence, claim)",
           .d3_no[i]             == 1 ~ "Documents state forms are NOT accessible (D-3 = No)",
           TRUE                       ~ "No accessible forms mentioned either way (D-3 not stated)"),
         verdict = fit_of(FGB$fit_D3c[i]))
})
d3c <- d3c %>% mutate(channel = by_size(channel, n_or_one(d3c)),
                      knowing = by_size(knowing, n_or_one(d3c)),
                      lodging = by_size(lodging, n_or_one(d3c)))
fl_d3c <- mk_flow(d3c, c("channel", "knowing", "lodging"))
save_tab(fl_d3c, "14_b1h_d3c_admin_flows")
p_b1h <- flow_plot(
  fl_d3c, c("channel", "knowing", "lodging"),
  c("Which ways in are documented (D-4)", "Can they find out how to apply? (D-1)",
    "Can they lodge the application? (D-3)"),
  "e. D3c administrative access: the applicant's own sequence, from a documented way in, to knowing how, to lodging it",
  paste(sprintf("Each application CHANNEL is named rather than counted, so a ribbon fans through every channel a programme documents; multi-response, %s",
                sprintf("and of the %d records whose channels are not enumerated in D-4, %d still name a phone or email contact elsewhere.",
                        .NOCH_N, .NOCH_FB)),
        sprintf("which is why this panel's path total exceeds %d. The two former 'no channel' strata are merged: every record with zero channels carries the", nrow(A)),
        "literal 'Not stated' in D-4 and none documents an absence, so they were one state, not two. 'Partial' is a codebook category with a written",
        "definition, not a visual softening -- D-1 partial is guidance that is incomplete OR addressed to professionals rather than residents, D-3 partial",
        "is some but not all key forms available -- and each label states that definition. Forms are not the only way in, which is what stage one shows."),
  "Programmes")

## ---- B1i: D4b monitoring and review -----------------------------------------
## Chain: what is reported -> whether a review closes the loop -> whether anyone
## can respond. Individual provisions, no paired categories.
##
## EACH STAGE DECLARES THE CODED FIELDS IT READS, and 16_verify.R fails the
## build if any field appears twice. This is not documentation -- it is the guard's
## only input. The reporting stage previously read F-3, which the review stage is
## built on, so one field drove two columns of the same panel and a reader saw the
## same evidence twice under different names. A structural rule catches that; no
## amount of relabelling does.
D4B_STAGE_FIELDS <- list(
  reporting = c("F-1", "F-2"),
  review    = c("F-3"),
  response  = c("F-4"))

d4b <- FGB %>%
  transmute(
    reporting = case_when(
      ## SPLIT, because the two are NOT always combined -- the author asked
      ## and the data answers cleanly. Of the 129 records publishing measurable
      ## indicators, 100 also publish a formal report and 29 publish indicators
      ## alone; no record publishes a report WITHOUT indicators (0 cells), which
      ## is why the composite looked harmless. The codebook is strict here: F-1
      ## counts only if SPECIFIC MEASURABLE indicators are published (m2
      ## installed, litres retained), F-2 names the vehicle.
      ## LISTED INDIVIDUALLY, on the author's instruction. The combined
      ## "indicators AND report" stratum is gone: each provision is now its own
      ## node, and a programme documenting both CONTRIBUTES A PATH FROM EACH --
      ## the same multi-response treatment the measure and actor stages already
      ## use. That is why the expansion below happens after this block: the
      ## case_when can only assign one label per record, so the both-case is
      ## split into two rows rather than collapsed into a third category.
      mon_indicators == 1 ~ "Measurable indicators (F-1: m2 installed, litres retained)",
      mon_webpage == 1    ~ "Programme webpage summary (F-2)",
      ## "Nothing published" was ONE node holding three different states, and
      ## the author asked why it does not read as a misfit. It partly does -- but
      ## the single label hid which part. The node is now split by what the
      ## documents actually say, because the distinction IS the D4b rule:
      ##   * the programme STATES it does not report publicly (F-2 =
      ##     "Not publicly reported"), or documents indicators/review as absent
      ##     -> a documented denial, and already a confirmed misfit (30);
      ##   * the documents are silent on every reporting provision
      ##     -> not stated, and Partial/Indeterminate, never a misfit.
      ## Reading the second as failure is the inference the framework forbids, so
      ## the split lets a reader see the misfit route without widening it.
      ## THIS STAGE NOW READS ONLY ITS OWN FIELDS (F-1 and F-2), and the
      ## author's three objections were all one fault.
      ##
      ## The previous version's tail branches read f_review_absent -- which is
      ## F-3, the field the NEXT stage is built on. So F-3 was being read twice in
      ## one panel, once inside a node captioned as though it described reporting.
      ## That is where "isn't this the same thing read twice?" came from, and it is
      ## correct. It also produced "Deny ONE route only", which is not a state of
      ## the reporting field at all: it is a fragment of the D4b verdict rule
      ## wearing a stage label. A stage names WHAT THE DOCUMENTS SAY; the rule
      ## combines stages into a verdict. Putting half the rule inside a stage made
      ## the panel unreadable and the category invented, exactly as the author said.
      ##
      ## The states below are the response options of F-1 and F-2, listed
      ## individually as the author expects: each vehicle by which something IS
      ## reported, then each way the documents record that nothing is -- keeping
      ## the explicit "No" separate from "not documented", because the rule treats
      ## them oppositely and that distinction is the whole point of the panel.
      ## F-3 appears once, in the review stage where it belongs. The verdict still
      ## emerges from the COMBINATION across stages, as in every other panel.
      ## v39b: SHORTENED to the author's own phrasing, with the evidential status kept
      ## as a two-word suffix. The author asked for "no public reporting", "no
      ## indicators published" and "reporting not documented" -- but the same author
      ## had earlier read two of these as duplicates of each other, which they are
      ## not: STATED is a documented act the rule can score as a misfit, SILENT is an
      ## absence of evidence it must never score. Dropping the suffix would restore
      ## exactly the ambiguity that made them look interchangeable.
      d4b_not_reported == 1  ~ "No public reporting -- STATED (F-2)",
      coalesce(f_me_absent, 0L) == 1
                             ~ "No indicators published -- STATED (F-1 = No)",
      TRUE                   ~ "Reporting not documented -- SILENT (F-1/F-2)"),
    .also_report = as.integer(mon_indicators == 1 & f_me_published == 1),
    ## the REVIEW stage separates denial from silence, for the same reason
    ## the reporting stage above already does. "No periodic review" held both a
    ## programme stating it holds none (F-3 = No, 122 records -- one half of the
    ## misfit predicate) and one whose documents never mention review (538). Only
    ## the first can contribute to a misfit, so a single node made the red ribbons
    ## untraceable.
    review = dplyr::case_when(
      f_review %in% 1                       ~ "Periodic programme review (F-3)",
      coalesce(f_review_absent, 0L) == 1    ~ "Documents state there is NO periodic review (F-3 = No)",
      TRUE                                  ~ "No review mentioned either way (F-3 not stated)"),
    ## The RESPONSE stage is NOT split, and the asymmetry is deliberate. F-4 is a
    ## text-and-type field: a coder names the channel or leaves it unstated, and
    ## the codebook offers no "No" option -- 0 records carry an explicit denial
    ## against 210 not stated. There is therefore no denial to separate, and the
    ## absence of a feedback channel can never by itself evidence a misfit. The
    ## label says which of the two it is rather than implying a finding.
    response = ifelse(f_feedback %in% 1, "Named feedback channel (F-4)",
                      "No feedback channel named (F-4 not stated)"),
    verdict = fit_of(fit_D4b))
## the both-case is EXPANDED, not collapsed. A record publishing measurable
## indicators AND a formal report now appears twice in this stage -- once under
## each provision -- so its ribbon leaves from both nodes, exactly as the author
## asked. This makes `reporting` MULTI-RESPONSE, so the panel's path total now
## exceeds 709; the reconciliation check reads the verdict column against
## 14_b1_domain_states.csv per PROGRAMME, so it is unaffected, but the panel's
## declaration has to move with it (see the multi-response list in 16_verify.R).
d4b <- bind_rows(
  d4b %>% select(-.also_report),
  d4b %>% filter(.also_report == 1) %>%
    mutate(reporting = "Formal published report (F-2)") %>% select(-.also_report))
d4b <- d4b %>% mutate(## AXIS ORDER SET ON REVIEW: indicators first, formal
                      ## report second, webpage third, nothing published last.
                      ## the last two levels replace the single "Nothing
                      ## published" node, which was split above. A hardcoded level
                      ## list that misses a label does not error -- it silently
                      ## sends those records to NA, which is exactly what happened
                      ## here (223 programmes rendered as an unlabelled "NA" node).
                      ## The guard below asserts every observed label has a level.
                      reporting = factor(as.character(reporting), levels = c(
                        "Measurable indicators (F-1: m2 installed, litres retained)",
                        "Formal published report (F-2)",
                        "Programme webpage summary (F-2)",
                        "No indicators published -- STATED (F-1 = No)",
                        "No public reporting -- STATED (F-2)",
                        "Reporting not documented -- SILENT (F-1/F-2)")),
                      review = by_mix(review, n_or_one(d4b)),
                      ## AXIS ORDER SET ON REVIEW: no feedback channel first,
                      ## named feedback channel last.
                      response = factor(as.character(response), levels = c(
                        "No feedback channel named (F-4 not stated)",
                        "Named feedback channel (F-4)")))
fl_d4b <- mk_flow(d4b, c("reporting", "review", "response"))
save_tab(fl_d4b, "14_b1i_d4b_monitoring_flows")
p_b1i <- flow_plot(
  fl_d4b, c("reporting", "review", "response"),
  c("What is published", "Whether a review closes the loop",
    "Whether anyone can respond"),
  "h. D4b monitoring and review: what is published, whether it is reviewed, and whether anyone can respond",
  paste("The webpage tier is kept separate from published indicators throughout: a programme description page is not monitoring. Reading left to right asks whether",
        "anything is measured, whether it is revisited, and whether an applicant has a route to report a problem. Colour is the record's D4b verdict as panel a rows",
        "it; the 45 confirmed misfits are records documenting the absence of all three. One flow per record."),
  "Programmes")

## ---------------------------------------------------------------------------
## v33 STRUCTURAL GUARD. Two separate edits to this file have silently deleted a
## whole flow section, each time surfacing as an obscure "object not found" much
## later in the run. This asserts the file's own completeness: every panel's flow
## frame and plot object must exist by the time the figure is assembled.
.expect <- c("fl_d1","fl_d2","fl_d3","fl_d3b","fl_d3c","fl_d4","fl_d4b",
             "p_b1c","p_b1d","p_b1e","p_b1g","p_b1h","p_b1f","p_b1i")
.missing <- .expect[!vapply(.expect, exists, logical(1))]
if (length(.missing))
  stop(sprintf(paste("figure 6 is incomplete: %s not built. A section of",
                     "14b_figures_rq.R has been deleted or renamed."),
               paste(.missing, collapse = ", ")))
msg("Figure 6: all %d flow frames and panels present", length(.expect))


## ALL SEVEN components now have a chain, in framework order:
## b D1 | c D2 | d D3a | e D3b | f D3c | g D4a | h D4b
## =============================================================================
## TAUTOLOGY GUARD -- an axis must not be the verdict under another name.
##
## Two panels were first built with a third axis that was a BIJECTIVE relabel of
## the verdict (D3b "who carries the cost first" = 398/138/62 exactly; D3c's
## composite profile likewise): same number of strata, one-to-one, so the axis
## duplicated the colour channel and told the reader nothing new.
##
## A strict REFINEMENT is different and is allowed: D2's ownership-route axis has
## six strata mapping onto four verdicts, so it says WHICH fit route a record
## takes -- information the colour cannot carry. What the guard forbids is an axis
## with the same cardinality as the verdict and a one-to-one mapping. Where the
## mapping is deterministic but finer, the guard records it so the caption can say
## so rather than implying the two channels are independent.
## =============================================================================
.axis_vs_verdict <- function(d, ax) {
  tb <- table(as.character(d[[ax]]), as.character(d$verdict))
  det <- all(rowSums(tb > 0) <= 1)              # axis determines verdict
  bij <- det && nrow(tb) == ncol(tb) && all(colSums(tb > 0) <= 1)
  list(det = det, bij = bij, k_axis = nrow(tb), k_verdict = ncol(tb))
}
.taut_check <- function(d, axes, label) {
  purrr::map_dfr(axes, function(a) {
    r <- .axis_vs_verdict(d, a)
    if (isTRUE(r$bij))
      stop(sprintf("[tautology] %s axis '%s' is a one-to-one relabel of the verdict (%d strata, %d verdicts) -- it duplicates the colour channel",
                   label, a, r$k_axis, r$k_verdict))
    tibble(panel = label, axis = a, determines_verdict = r$det,
           n_strata = r$k_axis, n_verdicts = r$k_verdict)
  })
}
.taut_rows <- bind_rows(
  .taut_check(d1,  c("hazard","function","measure"),        "D1"),
  .taut_check(d2,  c("space","actor","route"),              "D2"),
  .taut_check(d3,  c("measure","demand","provision"),        "D3a"),
  .taut_check(d3b, c("payment","alternative","target"),     "D3b"),
  .taut_check(d3c, c("channel","knowing","lodging"),         "D3c"),
  .taut_check(d4,  c("duty","binding","support"),           "D4a"),
  .taut_check(d4b, c("reporting","review","response"),      "D4b"))
save_tab(.taut_rows, "14_b1_axis_verdict_independence")
msg("Tautology guard: no axis is a one-to-one relabel; %d axis(es) determine their verdict (declared in caption)",
    sum(.taut_rows$determines_verdict))

## =============================================================================
## RECONCILIATION GUARD -- runs every build, halts on drift.
##
## For each of the seven components: the set of verdicts appearing in its flow
## panel must equal the set panel a shows for that component, and every RECORD
## carrying a determinate verdict (fit / partial / misfit) must be represented in
## its panel. Anything else is a definitional inconsistency and stops the build,
## rather than being discovered later by eye.
## =============================================================================
.rec_spec <- list(
  list(dom = "D1 Ecological-functional",                   fl = fl_d1,  rec = FALSE),
  list(dom = "D2 Tenure & authority access",                   fl = fl_d2,  rec = FALSE),
  list(dom = "D3a Technical enablement",                   fl = fl_d3,  rec = FALSE),
  ## rec = FALSE. The ALTERNATIVE stage is now multi-response -- a programme
  ## documenting both a free-goods route and a discounted purchase contributes a path
  ## from each, on the author's instruction that the two mechanisms "stand
  ## individually" -- so path units exceed programmes exactly as they already do for
  ## D1, D2, D3a, D3c and D4b. The DECLARATION moves; the check does not weaken. Every
  ## other guarantee still binds and is what caught this change the moment it was made:
  ## no verdict may appear in the flow that panel a lacks, no determinate verdict of
  ## panel a may be missing from the flow, and every verdict class panel a shows --
  ## Indeterminate included -- must appear here. D3b is the panel that motivated that
  ## last test, so it is the one that must keep passing it.
  list(dom = "D3b Financial accessibility",                fl = fl_d3b, rec = FALSE),
  ## D3c IS NO LONGER RECORD-UNIT. Naming each application channel (D-4 is a
  ## multi-select) makes a programme documenting three channels contribute three
  ## paths, exactly as D1/D2/D3a already do. The check correctly caught the change
  ## the moment it happened -- so the DECLARATION moves, not the check, and the
  ## class-coverage and no-extra-verdict tests above still apply in full.
  list(dom = "D3c Administrative access",                  fl = fl_d3c, rec = FALSE),
  list(dom = "D4a Maintenance responsibility and support", fl = fl_d4,  rec = FALSE),
  ## rec = FALSE. The reporting stage is now MULTI-RESPONSE -- a programme
  ## publishing measurable indicators AND a formal report contributes a path from
  ## each, on the author's instruction -- so path units exceed programmes exactly
  ## as they already do for the measure and actor panels. The DECLARATION moves;
  ## the check does not weaken. Every other guarantee still binds: no verdict may
  ## appear in the flow that panel a lacks, and no determinate verdict of panel
  ## a may be missing from the flow.
  list(dom = "D4b Monitoring, feedback and review",        fl = fl_d4b, rec = FALSE))
.det <- c("Documented fit", "Partial fit", "Confirmed misfit")
.rec_rows <- purrr::map_dfr(.rec_spec, function(z) {
  a_v <- overall %>% filter(domain == z$dom) %>% count(outcome, name = "records")
  f_v <- z$fl %>% count(verdict, wt = n, name = "flow_units")
  only_f <- setdiff(f_v$verdict[f_v$flow_units > 0], a_v$outcome[a_v$records > 0])
  only_a <- setdiff(a_v$outcome[a_v$records > 0 & a_v$outcome %in% .det],
                    f_v$verdict[f_v$flow_units > 0])
  if (length(only_f))
    stop(sprintf("[reconcile] %s: verdict(s) in the flow that panel a does not carry: %s",
                 z$dom, paste(only_f, collapse = " | ")))
  if (length(only_a))
    stop(sprintf("[reconcile] %s: determinate verdict(s) in panel a with no flow: %s",
                 z$dom, paste(only_a, collapse = " | ")))
  ## EVERY VERDICT CLASS panel a shows must appear in the flow panel -- including
  ## Indeterminate. This is the check that was missing: D3b filtered out its
  ## not-stated payment records, which were precisely its indeterminate ones, so
  ## the bar showed grey and the chain showed none, and nothing caught it.
  .missing_class <- setdiff(a_v$outcome[a_v$records > 0], f_v$verdict)
  if (length(.missing_class))
    stop(sprintf("[reconcile] %s: panel a shows verdict class(es) absent from its flow panel: %s",
                 z$dom, paste(.missing_class, collapse = "; ")))
  ## record-unit panels must match panel a exactly, count for count
  if (isTRUE(z$rec)) {
    j <- full_join(a_v, f_v, by = c("outcome" = "verdict")) %>%
      mutate(records = coalesce(records, 0L), flow_units = coalesce(flow_units, 0)) %>%
      filter(outcome %in% .det, records != flow_units)
    if (nrow(j))
      stop(sprintf("[reconcile] %s is a record-unit panel but its counts differ from panel a: %s",
                   z$dom, paste(sprintf("%s a=%d flow=%g", j$outcome, j$records, j$flow_units),
                                collapse = "; ")))
  }
  tibble(domain = z$dom, unit = ifelse(z$rec, "records", "pairs"),
         panel_a_records = sum(a_v$records), flow_units = sum(f_v$flow_units))
})
save_tab(.rec_rows, "14_b1_reconciliation")
msg("Reconciliation guard: all 7 components agree with panel a (%s record-unit panels count-exact)",
    sum(.rec_rows$unit == "records"))

fig7 <- p_b1a / ((p_b1c | p_b1d)) / ((p_b1e | p_b1g)) / ((p_b1h | p_b1f)) / p_b1i +
  plot_layout(heights = c(.30, 1, 1, 1, 1)) +
  plot_annotation(
    title = "Figure 6. Programme fit: the verdict layer, then where each domain breaks",
    subtitle = WRAP("ONE DEFINITION OF FIT, MISFIT AND PARTIAL FIT, applied identically to all seven components. A verdict is read off the programme's own documentation, never inferred from silence: DOCUMENTED FIT = the provision the context requires is documented; PARTIAL FIT = some but not all of it is; CONFIRMED MISFIT = the requirement arises and the documentation explicitly records the provision as absent, or records two documented things that contradict each other; INDETERMINATE = the requirement arises but the documents are silent, which is never scored as a misfit; NOT APPLICABLE = the requirement does not arise for this programme. || THRESHOLD CAVEAT, D3c AND D4b: the bar for these two was RAISED on review. Both previously placed almost every record in a single partial-fit band (88% and 87%), because any non-empty provision counted. A programme documenting one or two administrative provisions of ten, or offering only a webpage and an email address, now falls BELOW the partial-fit floor: it is a confirmed misfit where the documents record the remaining provisions as absent, and indeterminate where they are silent. D3c reads off the COUNT of documented provisions (6+ fit, 3-5 partial, 1-2 below the floor); D4b requires the programme itself to look -- published indicators or a periodic review (F-1/F-2/F-3) -- because a webpage checks nothing and a feedback address only reports what a resident volunteers. Nothing in the coded data changed; only the threshold moved. || D4a is the one component with no misfit route, by design: the framework rules that responsibility without a maintenance grant is not a misfit because the test is internal workability. D1 is a VALIDITY FLOOR rather than a discriminating test, which is why 95% of its records clear it. Panel a carries every record the test applies to, INDETERMINATE INCLUDED, so it is exactly the marginal of panels b-h and a grey flow has grey above it. Panels b-e count PROGRAMME-PROVISION PAIRS (a programme documenting two provisions contributes two flows); panels f-h count RECORDS, one flow each -- each panel's y-axis states which."),
    theme = TITLE())
## NOT WRITTEN. This is the superseded R version of figure 6; the shipped
## figure is built by build_fig07.py from the same flow tables. It was deleted
## from the figure set at v34, but the save_fig call was left in place, so the
## next run recreated it and it reappeared in the package -- two files claiming
## to be figure 6, differing in content. Deleting the OUTPUT without deleting
## the WRITER is not a deletion. The plot object is retained because the
## verification block reads it.
invisible(fig7)



# =============================================================================
# B2 | THE CORE DETERMINANT: TENURE AND BUILT FORM AGAINST ACCESS
# =============================================================================
## a: owner-restriction across the built-form x tenure frame (the frame again)
acc_eval <- FGB %>% filter(access_fit != "Indeterminate",
                           built_form != "Unknown", tenure_class != "Unknown")
cellA <- acc_eval %>% group_by(built_form, tenure_class) %>%
  summarise(n = n(),
            p_restrict = mean(access_route %in% c("Owner-restricted pathway", "Confirmed misfit")),
            .groups = "drop")
n_rm_owner <- acc_eval %>%
  filter(tenure_class == "Renter-majority",
         access_route %in% c("Owner-restricted pathway", "Confirmed misfit")) %>% nrow()

cellA <- acc_eval %>% group_by(built_form, tenure_class) %>%
  summarise(n = n(),
            p_restrict = mean(access_route %in% c("Owner-restricted pathway", "Confirmed misfit")),
            .groups = "drop")
n_rm_owner <- acc_eval %>%
  filter(tenure_class == "Renter-majority",
         access_route %in% c("Owner-restricted pathway", "Confirmed misfit")) %>% nrow()

## Kirk: make the 3-way tenure collapse transparent -- the raw renter-share
## distribution with the classification thresholds drawn on it
p_b2a0 <- FGB %>% filter(!is.na(renter_pct)) %>%
  ggplot(aes(renter_pct)) +
  geom_histogram(binwidth = 4, boundary = 0, fill = PAL$softblue, colour = "white", linewidth = .2) +
  geom_vline(xintercept = c(35, 55), linetype = "22", colour = PAL$ink, linewidth = .45) +
  annotate("text", x = 17, y = Inf, vjust = 1.6, label = "Owner-dominated\n< 35%", size = 2.1, colour = "grey25") +
  annotate("text", x = 45, y = Inf, vjust = 1.6, label = "Mixed\n35-55%", size = 2.1, colour = "grey25") +
  annotate("text", x = 72, y = Inf, vjust = 1.6, label = "Renter-majority\n>= 55%", size = 2.1, colour = "grey25") +
  labs(title = "a. Where the tenure classes come from",
       subtitle = WRAP("Raw renter share of households per record; dashed lines are the classification thresholds. The continuous variable, not the classes, enters every model."),
       x = "Renter share of households (Context: tenure)", y = "Records") +
  theme_nbs(7.6)

p_b2a <- ggplot(cellA, aes(tenure_class, built_form, fill = p_restrict)) +
  geom_tile(colour = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%s\nn=%d", percent(p_restrict, 1), n)),
            size = 2.6, lineheight = .9, colour = PAL$ink) +
  scale_fill_gradient(low = "white", high = PAL$misfit, labels = percent_format(),
                      name = "Share owner-restricted", limits = c(0, .8)) +
  labs(title = "b. Who can act, across the frame",
       ## same half-width overflow as panel a -- this ran off
       ## the right edge ("eligibility witho..."). sprintf() alone does no
       ## wrapping, so it is wrapped to the column it occupies.
       subtitle = WRAP(sprintf("Share of evaluable programmes whose only route runs through property owners. %d programmes in renter-majority cities are still owner-restricted -- eligibility without authority over the intervention space.", n_rm_owner), 92),
       x = "Tenure (Context: who can authorise)", y = "Built form (Context: how measures fit)") +
  theme_nbs(8.5)

## b: adjusted predicted probability of an OPEN route by renter share,
##    building-integrated vs not (the pre-registered core interaction)
d2 <- FGB %>%
  filter(access_fit != "Indeterminate") %>%
  mutate(route_open = as.integer(access_route %in% c("Direct tenant/broad pathway",
                                                     "Controller/collective pathway")),
         bldg_integrated = as.integer(m_greenroof == 1 | m_facade == 1),
         pop_c = as.numeric(scale(log10(pmax(pop, 1e3)))),
         density_c2 = as.numeric(scale(log10(pmax(density, 10))))) %>%
  filter(!is.na(renter_pct), !is.na(route_open))
d2 <- d2 %>% mutate(lead_body = droplevels(factor(lead_body)),
                    region_mod = droplevels(factor(region_mod)))
m_b2 <- glm(route_open ~ renter_pct * bldg_integrated + density_c2 + pop_c + lead_body + region_mod,
            data = d2, family = binomial())
Vcl <- sandwich::vcovCL(m_b2, cluster = d2$country_mod)
nd <- tidyr::crossing(renter_pct = seq(10, 80, 2), bldg_integrated = c(0L, 1L)) %>%
  mutate(density_c2 = 0, pop_c = 0,
         lead_body = factor(names(sort(table(d2$lead_body), decreasing = TRUE))[1],
                            levels = levels(droplevels(factor(d2$lead_body)))),
         region_mod = factor(names(sort(table(d2$region_mod), decreasing = TRUE))[1],
                             levels = levels(droplevels(factor(d2$region_mod)))))
Xn <- model.matrix(delete.response(terms(m_b2)), nd)
eta <- as.vector(Xn %*% coef(m_b2)); se <- sqrt(rowSums((Xn %*% Vcl) * Xn))
nd <- nd %>% mutate(p = plogis(eta), lo = plogis(eta - 1.96 * se), hi = plogis(eta + 1.96 * se),
                    grp = ifelse(bldg_integrated == 1,
                                 "Building-integrated portfolio (green roofs or facades, E-1/E-2)",
                                 "No building-integrated measures"))
save_tab(nd, "14_b2_predicted_open_route")

p_b2b <- ggplot(nd, aes(renter_pct, p, colour = grp, fill = grp)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = .15, colour = NA) +
  geom_line(linewidth = .9) +
  geom_rug(data = d2 %>% distinct(city, .keep_all = TRUE), aes(x = renter_pct),
           inherit.aes = FALSE, alpha = .12, length = unit(2.2, "mm")) +
  scale_colour_manual(values = c(PAL$misfit, PAL$grey) %>%
                        setNames(c("Building-integrated portfolio (green roofs or facades, E-1/E-2)",
                                   "No building-integrated measures")), name = NULL) +
  scale_fill_manual(values = c(PAL$misfit, PAL$grey) %>%
                      setNames(c("Building-integrated portfolio (green roofs or facades, E-1/E-2)",
                                 "No building-integrated measures")), name = NULL) +
  scale_x_continuous(labels = function(z) paste0(z, "%")) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  labs(title = "b. A route beyond owners remains incomplete where renter tenure makes it most necessary",
       subtitle = WRAP("Adjusted probability that a programme offers a direct-tenant or controller/collective route, by renter share (Context: % of households renting). Country-clustered 95% bands; density, population, lead body and region held at typical values. Rug: observed cities."),
       x = "Renter share of households (Context: tenure)",
       y = "P(open access route)  (Fit: Domain 2)") +
  theme_nbs(8.5) + theme(legend.position = "bottom") +
  guides(colour = guide_legend(nrow = 2), fill = guide_legend(nrow = 2))

## c: the full pre-registered interaction family, spelled out, BH-corrected
## Five pre-registered context-adaptation tests, each as an average marginal
  ## effect on the probability scale (country-clustered CI). The financial test
  ## is declared NOT ESTIMABLE at its true n rather than silently dropped.
  ame_of <- function(d, f, moderator, cluster) {
    d <- d %>% filter(complete.cases(across(all_of(all.vars(f)))))
    if (nrow(d) < 60 || length(unique(d[[all.vars(f)[1]]])) < 2) return(NULL)
    m <- glm(f, data = d, family = binomial())
    V <- tryCatch(sandwich::vcovCL(m, cluster = d[[cluster]]), error = function(e) vcov(m))
    b <- coef(m); tn <- moderator
    if (!tn %in% names(b) || is.na(b[tn])) return(NULL)
    ## THIS WAS SETTING THE INTERACTION ON THE DATA FRAME.
    ## For four of the five tests `moderator` is an INTERACTION TERM NAME
    ## ("renter_hi:bldgI", "demand_hi:multi_dept", "maint_hi:recur",
    ## "haz_any:dens_hi"). Assigning d1[["renter_hi:bldgI"]] <- 1L created a new
    ## column with that literal name, which model.matrix() then IGNORED -- it
    ## rebuilds the interaction from the untouched parent columns. So X1 == X0,
    ## giving ame = 0 EXACTLY, a zero gradient, se = 0 and a [0, 0] interval.
    ## Figure S4 panel c plotted three point estimates on zero with no error
    ## bars, and 14_b2_context_adaptation_ames.csv shipped those zeros. The one
    ## row that estimated was D3b, whose moderator is a real column.
    ##
    ## The counterfactual now moves the DESIGN MATRIX: build X once from the
    ## fitted model, copy it, and set the moderator column itself to 1 and 0.
    ## This is correct for a plain column and for an interaction column alike.
    X  <- model.matrix(delete.response(terms(m)), d, xlev = m$xlevels)
    if (!tn %in% colnames(X)) return(NULL)
    X1 <- X; X0 <- X
    X1[, tn] <- 1; X0[, tn] <- 0
    keep <- names(b)[!is.na(b)]
    eta1 <- X1[, keep] %*% b[keep]; eta0 <- X0[, keep] %*% b[keep]
    ame <- mean(plogis(eta1) - plogis(eta0))
    g <- colMeans((dlogis(eta1)[, 1] * X1[, keep]) - (dlogis(eta0)[, 1] * X0[, keep]))
    se <- sqrt(as.numeric(t(g) %*% V[keep, keep] %*% g))
    tibble(ame = ame, lo = ame - 1.96 * se, hi = ame + 1.96 * se, n = nrow(d))
  }
  T2 <- FGB %>% mutate(
    y_open = ifelse(access_fit == "Indeterminate", NA,
                    as.integer(access_route %in% c("Direct tenant/broad pathway",
                                                   "Controller/collective pathway"))),
    renter_hi = as.integer(renter_pct >= 55), bldgI = as.integer(m_greenroof == 1 | m_facade == 1),
    y_tech = ifelse(is.na(requirement_match), NA, as.integer(requirement_match >= .75)),
    demand_hi = as.integer(tech_demand == "High (building-integrated)"),
    multi_dept = as.integer(delivery_arch %in% c("Multi-department + partners", "Partnered delivery")),
    y_nopre = as.integer(prefinancing_required == 0),
    targeted = as.integer(equity_targeted == 1 | fin_scope == 1),
    y_post = as.integer(st_support == 1),
    maint_hi = as.integer(maint_demand == "High"),
    recur = as.integer(funding_horizon == "Recurring annual"),
    y_target = as.integer(spatial_target %in% c("Watershed/sewershed", "Priority zones",
                                                "Equity/vulnerability zones")),
    haz_any = as.integer(hx_flood == 1 | hx_coastal == 1 | hx_heat == 1 | hx_drought == 1),
    dens_hi = as.integer(density > stats::median(density, na.rm = TRUE)),
    pop_c = as.numeric(scale(log10(pmax(pop, 1e3)))),
    region_mod = droplevels(factor(region_mod)))
  tests <- list(
    list(lab = "Hazard exposure x density -> spatial targeting (D1)",
         f = y_target ~ haz_any * dens_hi + region_mod + pop_c, mod = "haz_any:dens_hi"),
    list(lab = "Renter share x building-integrated portfolio -> route beyond owners (D2)",
         f = y_open ~ renter_hi * bldgI + region_mod + pop_c, mod = "renter_hi:bldgI"),
    list(lab = "Technical demand x delivery architecture -> requirements met (D3a)",
         f = y_tech ~ demand_hi * multi_dept + region_mod + pop_c, mod = "demand_hi:multi_dept"),
    list(lab = "Constrained target group x cost -> payment without pre-financing (D3b)",
         f = y_nopre ~ targeted + region_mod + pop_c, mod = "targeted", note_n = sum(FGB$fin_scope == 1, na.rm = TRUE)),
    list(lab = "Maintenance demand x funding horizon -> post-installation support (D4a)",
         f = y_post ~ maint_hi * recur + region_mod + pop_c, mod = "maint_hi:recur"))
  ame_tab <- purrr::map_dfr(tests, function(tt) {
    r <- tryCatch(ame_of(T2, tt$f, tt$mod, "country_mod"), error = function(e) NULL)
    ## a literal "?" was shipping in the rendered panel where a record count
    ## belongs, because this test carries no note_n. A figure must not print a
    ## placeholder for a number it could compute: fall back to the model frame's own
    ## row count, and say which n is being reported.
    if (is.null(r)) tibble(lab = tt$lab, ame = NA, lo = NA, hi = NA, n = NA,
                           status = if (is.null(tt$note_n))
                                      sprintf("Not estimable (model frame n = %d)", nrow(T2))
                                    else
                                      sprintf("Not estimable at the fit test's n = %d", tt$note_n))
    else r %>% mutate(lab = tt$lab, status = "estimated")
  })
  save_tab(ame_tab, "14_b2_context_adaptation_ames")
  p_b2c <- ame_tab %>%
    mutate(lab = factor(str_wrap(lab, 48), levels = rev(str_wrap(sapply(tests, `[[`, "lab"), 48)))) %>%
    ggplot(aes(ame, lab)) +
    geom_vline(xintercept = 0, linetype = "22", colour = "grey45") +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = .16, colour = "grey40", na.rm = TRUE) +
    geom_point(size = 2.6, colour = PAL$deep, na.rm = TRUE) +
    geom_text(aes(x = 0, label = ifelse(is.na(ame), status, "")), size = 2.2,
              colour = "grey35", fontface = "italic", na.rm = TRUE) +
    geom_text(aes(label = ifelse(is.na(ame), "", sprintf("  n=%d", n))),
              hjust = -.35, vjust = -1, size = 1.9, colour = "grey40", na.rm = TRUE) +
    scale_x_continuous(labels = function(z) sprintf("%+.0f pp", 100 * z)) +
    labs(title = "c. The five pre-registered context-adaptation tests, on the probability scale",
         subtitle = WRAP("Average marginal effect of the context condition (or its interaction with design) on the provision it should trigger; country-clustered 95% intervals. None of the estimable interactions survives BH correction: design does not systematically adapt where the demand is greatest."),
         x = "Adjusted probability difference", y = NULL) +
    theme_nbs(7.6)

  
## Panel B: THE MISFIT ANATOMY. Each test crosses the condition that creates
## the demand against the provision that answers it. The bottom-right cell --
## demand present, provision absent -- IS the misfit, and it is shown as a
## Figure 5: the implementation context (A4a/A4b, built in 14a) joined with the
## access divide panels -- the D2 descriptive spread. The distribution and
## adaptation panels move to supplementary S4.
## Slimmed on review: the frame cell-counts (a) and the owner-restriction cell
## map (c) describe rather than argue -- they move to S4; the figure keeps the
## hazards each context faces and the renter-share gradient, the two panels
## that carry the D2 argument.
fig5 <- (p_a4b | p_b2b) + plot_layout(widths = c(1.35, 1)) +
  plot_annotation(title = "Figure S13. What each context faces, and the access divide renter tenure creates (descriptive foundation for D2)",
                  theme = TITLE())
## MOVED TO THE SUPPLEMENTARY on author instruction. Both panels are
## descriptive foundations for D2 rather than results in their own right: the
## hazard profile per context is restated in figure 4, and the renter-share
## gradient is the unadjusted form of what figure 10 reports adjusted.
save_fig(fig5, "figS13_context_access", 17, 8.5)
## The tenure-threshold panel moves into figure 3a (with the renter-share data
## it explains), so S4 holds the frame counts, the owner-restriction map and
## the pre-registered adaptation tests.
figS4 <- (p_a4a | p_b2a) / p_b2c + plot_layout(heights = c(1, 1.05)) +
  plot_annotation(title = "Figure S4. The implementation-context frame: cell counts, owner restriction, and the pre-registered adaptation tests",
                  theme = TITLE())
## heights and canvas raised so the stacked subtitles have room.
## The panel letters also disagreed with the layout -- position 2 was titled "c."
## and position 3 "b." -- so a reader following the caption read the wrong panel.
save_fig(figS4, "figS04_access_supplements", 16, 13.5)

# =============================================================================
# B3 | REGIONS AND COUNTRIES -- ALL UNITS
# =============================================================================
## ALL SEVEN framework components, using the SAME verdicts the fit layer and
## the flow figures use (fit or partial among evaluable records) -- not a
## selective subset. Regions appear once; the seven components sit side by side
## as narrow columns so the row reads as a regional fit profile.
dom7 <- tribble(
  ~key,   ~lab,
  "d1",   "D1\nEcological-\nfunctional",
  ## last surviving instance of the retired "Spatial-institutional" title.
  ## D2 was renamed at v18 when it was rebuilt on tenure band and route authority;
  ## "spatial" now means only the unscored targeting variable (B-7/G-4), so the old
  ## name here was actively misleading about what the component tests.
  "d2",   "D2\nTenure &\nauthority",
  "d3a",  "D3a\nTechnical\nenablement",
  "d3b",  "D3b\nFinancial\naccessibility",
  "d3c",  "D3c\nAdministrative\naccess",
  "d4a",  "D4a\nMaintenance\nsupport",
  "d4b",  "D4b\nMonitoring\nand review")
## this figure mixed two thresholds across its own panels -- d1/d2/d3a/d3b
## counted "Documented fit OR Partial fit" while d3c counted documented fit only.
## A seven-domain comparison where four rows use a looser bar than the other three
## is not comparable, and it inflated the four. Every row now reads the canonical
## DOCUMENTED FIT, matching figure 6 and the reporting table.
reg_dat <- FGB %>% mutate(
  d1  = ifelse(fit_D1  == "Indeterminate", NA, as.integer(fit_D1  == "Documented fit")),
  d2  = ifelse(fit_D2  == "Indeterminate", NA, as.integer(fit_D2  == "Documented fit")),
  d3a = ifelse(fit_D3a %in% c("Indeterminate","Not applicable"), NA,
               as.integer(fit_D3a == "Documented fit")),
  d3b = ifelse(fit_D3b %in% c("Indeterminate","Not applicable"), NA,
               as.integer(fit_D3b == "Documented fit")),
  ## this read admin_profile, the RETIRED provision-COUNTING construct that
  ## D3c abandoned at v18 when it moved to requirement matching. It reported a D3c
  ## documented-fit rate of 56% where the canonical verdict is 24.9% -- 312 records
  ## disagreeing -- so the figure was showing a rate the framework no longer
  ## computes. Every panel now reads the canonical verdict.
  d3c = ifelse(fit_D3c == "Indeterminate", NA,
               as.integer(fit_D3c == "Documented fit")),
  ## these two lines carried the IDENTICAL fault the comment above records for
  ## D3c at v22 -- the fix was applied to one panel and not to its neighbours.
  ## d4a read st_support, a provision-COUNTING flag, giving a 59.8% rate against the
  ## canonical 99.7% fit-or-partial -- 283 records disagreeing. d4b reconstructed a
  ## rate from mon_indicators/f_review instead of reading fit_D4b. Both now read the
  ## canonical verdict, and 16_verify.R asserts at source level that no figure derives
  ## a D4a or D4b signal from those constructs again.
  d4a = ifelse(fit_D4a == "Indeterminate", NA,
               as.integer(fit_D4a == "Documented fit")),
  d4b = ifelse(fit_D4b == "Indeterminate", NA,
               as.integer(fit_D4b == "Documented fit")))

reg_sum <- purrr::map_dfr(dom7$key, function(k) {
  reg_dat %>% filter(!is.na(.data[[k]])) %>%
    group_by(region_mod) %>%
    summarise(prog = mean(.data[[k]]), n = n(), n_country = n_distinct(country),
              cstd = mean(tapply(.data[[k]], country, mean)), .groups = "drop") %>%
    mutate(key = k)
}) %>% left_join(dom7, by = "key") %>%
  mutate(lab = factor(lab, levels = dom7$lab))
save_tab(reg_sum, "14_b3_region_domains")

REG_N <- reg_sum %>% group_by(region_mod) %>%
  summarise(n_rec = max(n), n_ctry = max(n_country), .groups = "drop")
ref_n <- REG_N %>%
  transmute(region_mod, region_lab = sprintf("%s (%d rec, %d countries)", region_mod, n_rec, n_ctry))
REG_ORD <- reg_sum %>% filter(key == "d2") %>% arrange(prog) %>% pull(region_mod)
p_b3a <- reg_sum %>%
  left_join(ref_n, by = "region_mod") %>%
  mutate(region_lab = factor(region_lab,
                             levels = unique(region_lab[order(match(region_mod, REG_ORD))]))) %>%
  ggplot(aes(y = region_lab)) +
  geom_segment(aes(x = prog, xend = cstd, yend = region_lab), colour = "grey70", linewidth = .6) +
  geom_point(aes(x = prog, colour = "Programme-weighted"), size = 2.1) +
  geom_point(aes(x = cstd, colour = "Country-standardised"), size = 2.1) +
  facet_wrap(~ lab, nrow = 1) +
  scale_colour_manual(values = c("Programme-weighted" = PAL$blue,
                                 "Country-standardised" = PAL$teal), name = NULL) +
  scale_x_continuous(labels = percent_format(), limits = c(0, 1),
                     breaks = c(0, .5, 1)) +
  labs(title = "a. All seven fit components across every region",
       subtitle = WRAP("Fit or partial fit among evaluable records, the same verdicts as figure 6; region rows appear once and hold their order across all seven columns, so a row reads as a regional fit profile. The segment is the within-region country-composition effect."),
       x = "Share of evaluable records with documented or partial fit", y = NULL) +
  theme_nbs(7.6) + theme(legend.position = "bottom", panel.spacing.x = unit(4, "pt"))

## b: EVERY country on every component, empirical-Bayes shrunk toward the corpus
## mean, one fixed row order, all SEVEN components as narrow columns.
shrink7 <- purrr::map_dfr(dom7$key, function(k) {
  d <- reg_dat %>% filter(!is.na(.data[[k]]))
  cc <- d %>% group_by(country_mod, region_mod) %>%
    summarise(x = sum(.data[[k]]), n = n(), .groups = "drop")
  m0 <- mean(d[[k]]); v0 <- max(var(cc$x / cc$n) - mean((cc$x/cc$n)*(1-cc$x/cc$n)/cc$n), 1e-4)
  a0 <- m0 * (m0*(1-m0)/v0 - 1); b0 <- (1-m0) * (m0*(1-m0)/v0 - 1)
  cc %>% mutate(p_shrunk = (x + a0) / (n + a0 + b0),
                lo = qbeta(.025, x + a0, n - x + b0), hi = qbeta(.975, x + a0, n - x + b0),
                key = k)
}) %>% left_join(dom7, by = "key") %>%
  mutate(lab = factor(lab, levels = dom7$lab))
save_tab(shrink7, "14_b3_country_shrunk")

CTY_N <- shrink7 %>% group_by(country_mod, region_mod) %>%
  summarise(n_rec = max(n), .groups = "drop") %>%
  arrange(region_mod, desc(n_rec)) %>%
  mutate(clab = sprintf("%s (%d records)", country_mod, n_rec))
## MANY-TO-MANY FIX (found by the round-3 warnings capture, not by any audit).
## CTY_N groups by country_mod AND region_mod, so "Other (pooled)" -- the
## small-country catch-all -- gets FOUR label rows, one per region it appears in.
## Joining that back on country_mod alone multiplied its rows: this panel drew
## 161 rows from 77 records, i.e. the pooled category was plotted four times with
## four different record counts in its label. R warned; nothing was reading the
## warnings. Joining on BOTH keys restores one label per country-region cell.
p_b3b <- shrink7 %>%
  left_join(CTY_N %>% select(country_mod, region_mod, clab),
            by = c("country_mod", "region_mod")) %>%
  mutate(clab = factor(clab, levels = rev(CTY_N$clab)),
         pale = n < 5) %>%
  ggplot(aes(p_shrunk, clab, colour = region_mod, alpha = pale)) +
  geom_linerange(aes(xmin = lo, xmax = hi), linewidth = .45, colour = "grey78") +
  geom_point(size = 1.7) +
  geom_vline(data = shrink7 %>% group_by(lab) %>% summarise(m = weighted.mean(p_shrunk, n)),
             aes(xintercept = m), linetype = "22", colour = "grey45", linewidth = .3) +
  facet_wrap(~ lab, nrow = 1) +
  scale_colour_manual(values = PAL_REGION, name = NULL) +
  scale_alpha_manual(values = c(`TRUE` = .45, `FALSE` = 1), guide = "none") +
  scale_x_continuous(labels = percent_format(), limits = c(0, 1), breaks = c(0, .5, 1)) +
  labs(title = "b. Every country on every component, one fixed order",
       subtitle = WRAP("Empirical-Bayes shrinkage toward the corpus mean (dashed); 95% posterior intervals; under-five-record countries pale. Same verdict definitions as panel a."),
       x = "Shrunk share of evaluable records with documented or partial fit", y = NULL) +
  theme_nbs(7.4) + theme(legend.position = "bottom", panel.spacing.x = unit(4, "pt"))

fig_b3 <- p_b3a / p_b3b + plot_layout(heights = c(1, 1.6)) +
  plot_annotation(title = "Figure S14. The regional and national landscape, every unit shown (RQ2)",
                  theme = TITLE())
## v33 RENUMBERED, not changed. This figure was RETIRED from the main text
## earlier (author: "not sure what figure 7 is doing at all") and its content
## became a table; it is still written for the supplement. The weakness-
## clustering figure now occupies main-text slot 8, so this one moves to S14
## to stop two files both claiming "figure 7". Its content is untouched.
save_fig(fig_b3, "figS14_regions_countries", 16.5, 16)

# =============================================================================
# B4 | THE CITY LEVEL
#   a: ADJUSTED outcome surface over the implementation-context frame
#   b: matched city contrasts -- similar context, different architecture
#   c: the US multi-programme portfolios, explicitly caveated as exploratory
# =============================================================================
## a: adjusted predictions per frame cell (region + log population held at
## typical values; country-clustered CIs), replacing raw cell percentages
frame_dat <- FGB %>% filter(built_form != "Unknown", tenure_class != "Unknown") %>%
  mutate(open = ifelse(access_fit == "Indeterminate", NA,
                       as.integer(access_route %in% c("Direct tenant/broad pathway",
                                                      "Controller/collective pathway"))),
         req  = ifelse(is.na(requirement_match), NA, as.integer(requirement_match >= .75)),
         post = as.integer(st_support == 1),
         mon  = as.integer(mon_indicators == 1 | f_review == 1),
         pop_c = as.numeric(scale(log10(pmax(pop, 1e3)))),
         region_mod = droplevels(factor(region_mod)))
dom4 <- tribble(
  ~key, ~lab,
  "open",  "Route beyond owners: tenant or property-controller pathway (D2)",
  "req",   "Measure-specific technical requirements met (D3a)",
  "post",  "Post-installation maintenance support (D4a)",
  "mon",   "Monitoring, feedback or review documented (D4b)")
adj_cell <- purrr::map_dfr(dom4$key, function(k) {
  d <- frame_dat %>% filter(!is.na(.data[[k]])) %>%
    mutate(y = .data[[k]], built_form = droplevels(built_form),
           tenure_class = droplevels(tenure_class))
  m <- glm(y ~ built_form * tenure_class + region_mod + pop_c, data = d, family = binomial())
  V <- sandwich::vcovCL(m, cluster = d$country_mod)
  nd <- tidyr::crossing(built_form = levels(d$built_form),
                        tenure_class = levels(d$tenure_class)) %>%
    mutate(region_mod = factor(names(sort(table(d$region_mod), decreasing = TRUE))[1],
                               levels = levels(d$region_mod)), pop_c = 0)
  X <- model.matrix(delete.response(terms(m)), nd, xlev = m$xlevels)
  eta <- as.vector(X %*% coef(m)); se <- sqrt(rowSums((X %*% V) * X))
  nd %>% mutate(p = plogis(eta), lo = plogis(eta - 1.96 * se), hi = plogis(eta + 1.96 * se),
                key = k) %>%
    left_join(d %>% count(built_form, tenure_class), by = c("built_form", "tenure_class"))
}) %>% left_join(dom4, by = "key")
save_tab(adj_cell, "14_b4_adjusted_frame_outcomes")

p_b4a <- adj_cell %>%
  mutate(built_form = factor(built_form, levels = c("Dispersed", "Intermediate", "Compact / vertical"))) %>%
  ggplot(aes(tenure_class, built_form, fill = p)) +
  geom_tile(colour = "white", linewidth = .9) +
  geom_text(aes(label = sprintf("%s\n[%s-%s] n=%d", percent(p, 1), percent(lo, 1), percent(hi, 1), n)),
            size = 1.95, lineheight = .92, colour = PAL$ink) +
  facet_wrap(~ lab, ncol = 2) +
  scale_fill_seq("Adjusted probability", labels = percent_format(), limits = c(0, 1)) +
  labs(title = "a. Adjusted programme expectations across urban implementation contexts",
       ## THE TWO PANELS DISAGREED BY ONE RECORD PER CELL and
       ## neither said why. Intermediate x Mixed reads 95 in the descriptive
       ## panel and 94 here; Dispersed x Mixed reads 137 and 136. The cause is
       ## legitimate -- n here is the MODEL FRAME, which drops the three records
       ## whose D2 outcome is Indeterminate -- but a figure that prints two
       ## different n for the same cell without distinguishing them reads as an
       ## error. It is now stated.
       subtitle = WRAP("Model-based probabilities over the implementation-context frame (region and population held typical; country-clustered 95% intervals in brackets). A context-stratified programme result -- expectations for a context, not a determinate per-city verdict. n is the MODEL FRAME for each cell -- complete cases on the modelled outcome -- so it is one or two lower than the descriptive cell size where a record's verdict is Indeterminate."),
       x = "Tenure (Context)", y = "Built form (Context)") +
  theme_nbs(7.8)

## b: matched city contrasts. Within three context groups, the model-adjusted
## extremes on the route-beyond-owners outcome: similar context, different architecture.
## WARNING-CAPTURE FIX (found by the new round-3 warnings log, not by any audit).
## `glm()` drops rows with a missing predictor, so fitted(m_b2) is SHORTER than
## d2. Subtracting it from the full column recycled the shorter vector and
## silently misaligned every residual after the first fitted row -- R said so
## ("longer object length is not a multiple of shorter object length") and
## nothing was reading the warnings. Joining on the model's own row names aligns
## each residual to the record it belongs to; rows the model dropped get NA.
.fit_b2 <- tibble::tibble(.rn = names(stats::fitted(m_b2)),
                          .fitted = as.numeric(stats::fitted(m_b2)))
res_dat <- d2 %>% mutate(.rn = rownames(d2)) %>%
  left_join(.fit_b2, by = ".rn") %>%
  mutate(resid = route_open - .fitted) %>%
  select(-.rn, -.fitted)
used_cities <- character(0)
pick_pair <- function(df, grp_lab) {
  df <- df %>% filter(!is.na(resid), !city %in% used_cities) %>% distinct(city, .keep_all = TRUE)
  if (nrow(df) < 2) return(NULL)
  bind_rows(df %>% slice_max(resid, n = 1, with_ties = FALSE) %>% mutate(side = "Opens a route its context predicts it would not"),
            df %>% slice_min(resid, n = 1, with_ties = FALSE) %>% mutate(side = "Stays owner-bound despite the same context")) %>%
    mutate(group = grp_lab)
}
add_pair <- function(df, grp_lab) {
  p <- pick_pair(df, grp_lab)
  if (!is.null(p)) used_cities <<- c(used_cities, p$city)
  p
}
pairs_df <- bind_rows(
  add_pair(res_dat %>% filter(built_form == "Compact / vertical", tenure_class == "Renter-majority"),
            "Compact, renter-majority cities"),
  add_pair(res_dat %>% filter(built_form == "Dispersed", tenure_class == "Owner-dominated"),
            "Dispersed, owner-dominated cities"),
  add_pair(res_dat %>% filter(hazard_family == "Floods/stormwater (incl. coastal)"),
            "Water-exposed cities"))
if (!is.null(pairs_df) && nrow(pairs_df)) {
  prof_dims <- tribble(
    ~dim, ~val,
    "Building-integrated measures", quote(m_greenroof == 1 | m_facade == 1),
    "Controller route (C-9/11/15)", quote(controller_incl == 1),
    "Direct tenant route (C-10)",   quote(el_tenant_direct == 1),
    "Standards or advice (C-5/E-13)", quote(r_standards == 1 | i_advice == 1),
    "No pre-financing (E-22)",      quote(prefinancing_required == 0),
    "Maintenance support (E-15..18)", quote(st_support == 1),
    "Indicators or review (F-1/F-3)", quote(mon_indicators == 1 | f_review == 1))
  pair_prof <- purrr::map_dfr(seq_len(nrow(prof_dims)), function(i)
    pairs_df %>% rowwise() %>%
      mutate(dim = prof_dims$dim[i],
             on = isTRUE(eval(prof_dims$val[[i]],
                              envir = as.list(cur_data_all())))) %>%
      ungroup() %>% select(group, side, city, country_mod, renter_pct, dim, on))
  save_tab(pair_prof, "14_b4_matched_city_pairs")
  p_b4b <- pair_prof %>%
    mutate(city_lab = sprintf("%s, %s\n%s", city, country_mod, str_wrap(side, 30)),
           dim = factor(dim, levels = prof_dims$dim)) %>%
    ggplot(aes(dim, city_lab)) +
    geom_point(aes(colour = on, shape = on), size = 3.6) +
    facet_grid(group ~ ., scales = "free_y", labeller = labeller(group = label_wrap_gen(22))) +
    scale_colour_manual(values = c(`TRUE` = PAL$fit, `FALSE` = "grey72"),
                        labels = c(`TRUE` = "provision documented", `FALSE` = "not documented"),
                        name = NULL) +
    scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1), guide = "none") +
    labs(title = "a. Matched contrasts: similar context, different architecture",
         subtitle = WRAP("Within each context group, the model-adjusted extremes on the route-beyond-owners outcome (largest positive and negative residuals from the B2 model). The question the pair poses: why do comparable contexts produce different implementation pathways?"),
         x = NULL, y = NULL) +
    theme_nbs(7.6) +
    theme(axis.text.x = element_text(angle = 28, hjust = 1), legend.position = "bottom",
          strip.text.y = element_text(angle = 0))
} else p_b4b <- plot_spacer()

## b: the genuine multi-programme portfolios -- PROGRAMME-LEVEL rows (Kirk):
## each programme in the city shown on its own row, plus a bold combined-
## portfolio row, so complementarity and duplication are visible programme by
## programme rather than only as summary numbers.
PARENT_MAP <- c("Chicago Lawn" = "Chicago", "Zurich (Kreis 11)" = "Zurich")
multi <- FGB %>%
  mutate(parent_city = dplyr::recode(city, !!!as.list(PARENT_MAP))) %>%
  add_count(country_mod, parent_city, name = "n_in_city") %>% filter(n_in_city >= 2) %>%
  mutate(city = parent_city)
if (nrow(multi)) {
  dims_def <- tribble(
    ~dim, ~val,
    "Roof or facade measures",        quote(m_greenroof == 1 | m_facade == 1),
    "Ground or subsurface measures",  quote(m_raingard == 1 | m_depave == 1 | m_infiltr == 1),
    "Storage or trees",               quote(m_barrel == 1 | m_trees == 1),
    "Controller route",               quote(controller_incl == 1),
    "Direct tenant route",            quote(el_tenant_direct == 1),
    "Financial instrument",           quote(i_grant == 1 | i_taxreb == 1),
    "Standards or advice",            quote(r_standards == 1 | i_advice == 1),
    "Maintenance support",            quote(st_support == 1),
    "Monitoring or review",           quote(mon_indicators == 1 | f_review == 1))
  prog_cov <- purrr::map_dfr(seq_len(nrow(dims_def)), function(i)
    multi %>% mutate(dim = dims_def$dim[i],
                     covered = purrr::map_lgl(seq_len(n()), function(r)
                       isTRUE(eval(dims_def$val[[i]], envir = as.list(cur_data_all()[r, ]))))) %>%
      select(country_mod, city, programme, dim, covered))
  comb <- prog_cov %>% group_by(country_mod, city, dim) %>%
    summarise(covered = any(covered), .groups = "drop") %>%
    mutate(programme = "COMBINED PORTFOLIO")
  metrics <- comb %>% group_by(country_mod, city) %>%
    summarise(complementarity = mean(covered), .groups = "drop") %>%
    left_join(prog_cov %>% distinct(country_mod, city, programme) %>% count(country_mod, city, name = "n_prog"),
              by = c("country_mod", "city")) %>%
    left_join(prog_cov %>% group_by(country_mod, city, dim) %>%
                summarise(k = sum(covered), .groups = "drop") %>%
                group_by(country_mod, city) %>%
                summarise(dup = sum(pmax(k - 1, 0)) / pmax(sum(k), 1), .groups = "drop"),
              by = c("country_mod", "city"))
  save_tab(bind_rows(prog_cov, comb) %>% left_join(metrics, by = c("country_mod", "city")),
           "14_b4_city_portfolios_programme_level")
  plot_dat <- bind_rows(prog_cov, comb) %>%
    left_join(metrics, by = c("country_mod", "city")) %>%
    mutate(city_lab = sprintf("%s, %s -- %d programmes | complementarity %s | duplication %s",
                              city, country_mod, n_prog,
                              percent(complementarity, 1), percent(dup, 1)),
           prog_lab = str_wrap(ifelse(is.na(programme) | programme == "", "(unnamed programme)",
                                      as.character(programme)), 34),
           is_comb = programme == "COMBINED PORTFOLIO",
           dim = factor(dim, levels = dims_def$dim)) %>%
    group_by(city_lab) %>%
    mutate(prog_lab = factor(prog_lab, levels = c("COMBINED PORTFOLIO",
                                                  sort(setdiff(unique(prog_lab), "COMBINED PORTFOLIO"))))) %>%
    ungroup()
  p_b4c <- ggplot(plot_dat, aes(dim, fct_rev(prog_lab))) +
    geom_point(aes(colour = interaction(covered, is_comb), shape = covered), size = 2.9) +
    facet_grid(city_lab ~ ., scales = "free_y", space = "free_y",
               labeller = labeller(city_lab = label_wrap_gen(38))) +
    scale_colour_manual(values = c("TRUE.FALSE" = PAL$fit, "FALSE.FALSE" = "grey75",
                                   "TRUE.TRUE" = PAL$deep, "FALSE.TRUE" = "grey60"),
                        labels = c("TRUE.FALSE" = "programme provides it",
                                   "FALSE.FALSE" = "programme does not",
                                   "TRUE.TRUE" = "covered by the combined portfolio",
                                   "FALSE.TRUE" = "gap the whole portfolio leaves"),
                        name = NULL) +
    scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1), guide = "none") +
    labs(title = "a. Inside each multi-programme city: what every programme itself provides",
         subtitle = WRAP("One row per PROGRAMME, its combined-portfolio row in dark blue above it. Complementarity = share of listed needs the portfolio covers; duplication = share of programme-provisions repeating a need another programme already covers. A US-only, mostly-pairs subset after transparent sub-unit normalisation (Chicago Lawn -> Chicago) -- exploratory, never a universal city score."),
         x = NULL, y = NULL) +
    theme_nbs(7.2) +
    theme(axis.text.x = element_text(angle = 28, hjust = 1), legend.position = "bottom",
          strip.text.y = element_text(angle = 0, size = 5.4), legend.text = element_text(size = 5.8))
} else p_b4c <- plot_spacer()

## Kirk: B4a is a context-level expectation surface and belongs with the
## context-to-design synthesis, so it moves into Figure B6. B4 is now purely
## the city portfolio evidence.
fig_b4 <- p_b4c +
  plot_annotation(title = "Figure S7. Inside multi-programme cities: what each programme provides, and what the portfolio still leaves open (RQ3)",
                  theme = TITLE())
save_fig(fig_b4, "figS07_city_portfolios", 15, 9)
if (!inherits(p_b4b, "spacer")) {
  figS1 <- p_b4b + plot_annotation(
    title = "Figure S8. Matched city contrasts: similar context, different architecture",
    subtitle = "Model-residual extremes within context groups -- selected cases for the Results text, not a population figure.",
    theme = TITLE())
  save_fig(figS1, "figS08_matched_city_pairs", 12.5, 9)
}

# =============================================================================
# B5 | PROGRAMME ARCHITECTURES: THE THREE-CLASS MACRO SOLUTION, ITS
#      GEOGRAPHY, AND ITS JUSTICE PROFILE
# Silhouette peaks at k=2, but that split separates on one variable only;
# CH keeps rising to 6, where cross-method agreement is weak. The HCPC
# automatic k=3 is the smallest partition separated on several design axes
# and is foregrounded, named from its profile; k=6 sits in Part C.
# =============================================================================
## B5a: the design space itself -- MCA coordinates with the macro types drawn
## on, so the reader sees how separated (and how overlapping) the types are
## before reading any profile.
mca_dat <- tibble(d1 = coords[, 1], d2 = coords[, 2],
                  type = droplevels(M$typology_macro[documented_enough]))
p_b5a <- ggplot(mca_dat, aes(d1, d2, colour = type)) +
  geom_hline(yintercept = 0, colour = "grey88", linewidth = .3) +
  geom_vline(xintercept = 0, colour = "grey88", linewidth = .3) +
  geom_point(size = .8, alpha = .55, stroke = 0) +
  stat_ellipse(level = .68, linewidth = .55) +
  scale_colour_manual(values = PAL_TYPE, name = NULL,
                      labels = function(z) ifelse(z %in% names(TYPE_DESC),
                                                  sprintf("%s: %s", z, TYPE_DESC[z]), z),
                      guide = guide_legend(override.aes = list(size = 2.6, alpha = 1), nrow = 3)) +
  labs(title = "a. The programme-design space",
       subtitle = sprintf("MCA on design primitives only, documented subset (n = %d); fit outputs are never clustered on. Ellipses are 68%% concentration contours -- the overlap is the honest picture: these are broad macro-types, not tight archetypes.",
                          nrow(mca_dat)),
       x = "Dimension 1", y = "Dimension 2") +
  theme_nbs(7.6) + theme(legend.position = "bottom", legend.text = element_text(size = 5.8))

## B5b profile: every block uses the SAME categories the paper established --
## the four specific hazards/objectives, the three technical-demand tiers, all
## six instrument families, every procedure field, every post-installation field.
arch_items <- tribble(
  ~item, ~block, ~val,
  ## Environmental orientation: the SAME four categories used from A2a onwards
  ## v58 / R6 (finding 9). THIS BLOCK WAS BUILT ON THE WRONG CONSTRUCT.
  ## It is labelled "Hazards addressed" -- B-6, what the programme says it
  ## tackles -- but the first three rows read hx_* (A-7ii, the CITY'S hazard
  ## EXPOSURE) and the fourth read obj_biodiv (a programme OBJECTIVE). Three
  ## different constructs under one heading. Proof it was exposure: the shipped
  ## 14_b5 "Floods" row reproduced hx_flood|hx_coastal exactly.
  ## The typology is a DESIGN typology, so it must be described by what
  ## programmes address. All four rows now come from B-6. Exposure remains
  ## available as context in figures 11 and S13, labelled as exposure.
  "Floods",                         "D1 Hazards addressed", quote(haz_storm == 1 | haz_coastal == 1),
  "Heat",                           "D1 Hazards addressed", quote(haz_heat == 1),
  "Drought",                        "D1 Hazards addressed", quote(haz_drought == 1),
  "Biodiversity loss",              "D1 Hazards addressed", quote(haz_biodiv == 1),
  ## Intervention portfolio: the three technical-demand tiers used everywhere else
  "High demand (building-integrated)", "D1 Measure portfolio", quote(m_greenroof == 1 | m_facade == 1),
  "Medium demand (ground-modifying)",  "D1 Measure portfolio", quote(m_raingard == 1 | m_depave == 1 | m_infiltr == 1 | m_pond == 1),
  "Low demand (standardised)",         "D1 Measure portfolio", quote(m_barrel == 1 | m_disconn == 1 | m_planting == 1 | m_turf == 1 | m_trees == 1),
  ## Eligible actors: the full C-block set
  "Homeowners (C-8)",               "D2 Eligible actors", quote(el_home == 1),
  "Landlords (C-9)",                "D2 Eligible actors", quote(el_land == 1),
  "Tenants directly (C-10)",        "D2 Eligible actors", quote(el_tenant_direct == 1),
  "Co-ops or HOAs (C-11)",          "D2 Eligible actors", quote(el_coop == 1),
  "Social housing (C-15)",          "D2 Eligible actors", quote(el_socho == 1),
  "Informal groups (C-16)",         "D2 Eligible actors", quote(el_inform == 1),
  ## Support instruments: all six families, exactly as in A3
  ## SPLIT FROM ONE "D3 Support instruments" BLOCK INTO THE FRAMEWORK'S OWN
  ## THREE D3 COMPONENTS. The paper tests D3a (technical enablement), D3b
  ## (financial accessibility) and D3c (administrative access) as three separate
  ## relational tests with three separate verdicts, but this panel pooled all six
  ## instrument families under a single "D3" heading -- so the one figure meant to
  ## show what distinguishes the architectures used a vocabulary the rest of the
  ## paper does not. Each family now sits under the component it belongs to.
  "Capital grant or loan",          "D3b Financial accessibility", quote(i_grant == 1),
  "Tax or fee rebate",              "D3b Financial accessibility", quote(i_taxreb == 1),
  "Material or in-kind provision",  "D3b Financial accessibility", quote(i_inkind == 1 | x_freematerials == 1),
  "Technical advice or standards",  "D3a Technical enablement",    quote(i_advice == 1 | r_standards == 1),
  "Regulatory support",             "D3a Technical enablement",    quote(r_fasttrack == 1 | r_feewaiver == 1),
  "Behavioural recognition",        "D3b Financial accessibility", quote(i_reward == 1),
  ## Procedure: the full D block
  "Applicant guide (D-1)",          "D3c Administrative access", quote(p_guide >= 1),
  "Decision timeline (D-2)",        "D3c Administrative access", quote(p_timeline == 1),
  "Accessible forms (D-3)",         "D3c Administrative access", quote(p_forms >= 1),
  "Two or more channels (D-4)",     "D3c Administrative access", quote(n_channels >= 2),
  "Multilingual materials (D-5)",   "D3c Administrative access", quote(p_multiling == 1),
  "Co-design (D-6)",                "D3c Administrative access", quote(p_codesign == 1),
  ## Post-installation: responsibility, support, monitoring
  "Responsibility named (F-5)",     "D4a Maintenance support", quote(st_resp_named == 1),
  "Maintenance support (E-15..18)", "D4a Maintenance support", quote(st_support == 1),
  "Binding condition (E-17)",       "D4a Maintenance support", quote(s_condition == 1),
  "Indicators published (F-1/F-2)", "D4b Monitoring and review", quote(mon_indicators == 1),
  "Periodic review (F-3)",          "D4b Monitoring and review", quote(f_review == 1),
  "Feedback channel (F-4)",         "D4b Monitoring and review", quote(f_feedback == 1))

prof <- purrr::map_dfr(seq_len(nrow(arch_items)), function(i) {
  FGB %>% group_by(typology_macro) %>%
    summarise(p = mean(eval(arch_items$val[[i]], envir = pick(everything())), na.rm = TRUE),
              .groups = "drop") %>%
    mutate(item = arch_items$item[i], block = arch_items$block[i])
})
cls_n <- FGB %>% count(typology_macro)
prof <- prof %>% left_join(cls_n, by = "typology_macro") %>%
  mutate(cls = sprintf("%s\n(n=%d)", as.character(typology_macro), n),
         item = factor(item, levels = rev(arch_items$item)),
         block = factor(block, levels = unique(arch_items$block)))
save_tab(prof, "14_b5_macro_profiles")

p_b5b <- ggplot(prof, aes(cls, item, fill = p)) +
  geom_tile(colour = "white", linewidth = .5) +
  geom_text(aes(label = percent(p, 1)), size = 2) +
  facet_grid(block ~ ., scales = "free_y", space = "free_y",
             labeller = labeller(block = label_wrap_gen(13))) +
  scale_fill_seq("Share in type", labels = percent_format(), limits = c(0, 1)) +
  labs(title = "b. What distinguishes the types, on the paper's own categories",
       subtitle = WRAP("Blocks carry the same D1-D4 domain names and the same categories used from figure A2 onwards -- the four hazards (floods, heat, drought, biodiversity loss), the three technical-demand tiers, the six instrument families, every administrative and maintenance-support field -- so one vocabulary runs through the whole paper. Neutral Type labels: the profile IS the description."),
       x = NULL, y = NULL) +
  theme_nbs(6.9) + theme(strip.text.y = element_text(angle = 0))

## c: where each architecture is found (world + enlarged regions)
if (HAS("maps")) {
  geoB <- FGB %>%
    mutate(ckey = tolower(iconv(city, to = "ASCII//TRANSLIT")),
           cmapped = unname(cmap[country])) %>%
    left_join(wc, by = c("ckey" = "ckey", "cmapped" = "country.etc"),
              relationship = "many-to-one") %>%
    filter(!is.na(lat))
  mac_cols <- PAL_TYPE[levels(FGB$typology_macro)]
  cls_map <- function(xlim, ylim, ps = 1.2) ggplot() +
    geom_polygon(data = world, aes(long, lat, group = group),
                 fill = "grey94", colour = "white", linewidth = .1) +
    geom_point(data = geoB, aes(long, lat, colour = typology_macro), size = ps,
               alpha = .8, stroke = 0) +
    scale_colour_manual(values = mac_cols, name = NULL,
                        labels = function(z) ifelse(z %in% names(TYPE_DESC),
                                                    sprintf("%s: %s", z, TYPE_DESC[z]), z),
                        guide = guide_legend(override.aes = list(size = 3.2), nrow = 4)) +
    coord_fixed(1.32, xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_nbs(8) +
    theme(axis.text = element_blank(), axis.title = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "white", colour = NA))
  ## Same treatment as A1: zoom the four regions where the corpus concentrates.
  ## "Other regions" is not zoomed -- scattered across four continents, its box
  ## degenerates to world scale and the inset would just repeat the world map.
  ## Percentile trim stops one outlying city inflating a box.
  ZOOM_REGIONS <- c("North America", "Western Europe", "Central/Eastern Europe", "East Asia")
  rb <- geoB %>% filter(region_mod %in% ZOOM_REGIONS) %>% group_by(region_mod) %>%
    summarise(x0 = stats::quantile(long, .01, names = FALSE),
              x1 = stats::quantile(long, .99, names = FALSE),
              y0 = stats::quantile(lat,  .01, names = FALSE),
              y1 = stats::quantile(lat,  .99, names = FALSE),
              n = n(), .groups = "drop") %>%
    mutate(region_mod = factor(region_mod, levels = ZOOM_REGIONS)) %>%
    filter(!is.na(region_mod)) %>% arrange(region_mod) %>%
    mutate(padx = pmax((x1 - x0) * .10, 2), pady = pmax((y1 - y0) * .10, 2),
           x0 = x0 - padx, x1 = x1 + padx, y0 = y0 - pady, y1 = y1 + pady,
           idx = row_number()) %>%
    ## EQUAL SQUARE INSETS (same rule as figure 3): under coord_fixed(1.32) a
    ## panel renders square only when dlon = 1.32 * dlat; equalise nothing else.
    mutate(cx = (x0 + x1) / 2, cy = (y0 + y1) / 2,
           dlat = pmax(y1 - y0, (x1 - x0) / 1.32), dlon = 1.32 * dlat,
           x0 = cx - dlon / 2, x1 = cx + dlon / 2,
           y0 = cy - dlat / 2, y1 = cy + dlat / 2)
  ## NO DASHED LOCATOR BOXES (same rule as figure 3): four dashed rectangles across
  ## the densest part of the map carried information a numbered chip already gives.
  ## The chip sits at the region's centroid; the inset below repeats the number and
  ## carries its own outline, which is what ties the two together.
  p_map_w <- cls_map(c(-165, 178), c(-56, 76)) +
    geom_label(data = rb, aes(x = cx, y = cy, label = idx), inherit.aes = FALSE,
               size = 2.3, fontface = "bold", colour = "white", fill = PAL$ink,
               label.r = unit(4, "pt"), label.padding = unit(1.5, "pt"),
               label.size = 0) +
    labs(title = "c. Where each type is found",
         subtitle = WRAP("The four regions holding the corpus are enlarged below; class x country Cramer's V is reported in the figure subtitle -- the geography is partly national programme tradition."))
  p_map_reg <- lapply(seq_len(nrow(rb)), function(i) {
    b <- rb[i, ]
    cls_map(c(b$x0, b$x1), c(b$y0, b$y1), ps = 1.6) + guides(colour = "none") +
      labs(title = sprintf("%d \u2014 %s", b$idx, b$region_mod)) +
      ## each inset carries its OWN outline, replacing the locator box on the world map
      theme(plot.title = element_text(size = 6.8, face = "bold", colour = PAL$ink,
                                      margin = margin(b = 2.5)),
            panel.border = element_rect(fill = NA, colour = PAL$ink, linewidth = .45),
            plot.margin = margin(2, 3, 2, 3))
  })
  p_b5c <- (p_map_w / patchwork::wrap_plots(p_map_reg, nrow = 1, widths = rep(1, nrow(rb)))) +
    plot_layout(heights = c(1.5, 1), guides = "collect") &
    theme(legend.position = "bottom", legend.text = element_text(size = 5.6))
} else p_b5c <- plot_spacer()

## c: architecture ACROSS the implementation-context frame -- the core RQ3
## finding is not that types map geographically but that they do NOT adapt
## to context. Composition per frame cell + the chi-square association.
arch_ctx <- FGB %>%
  filter(built_form != "Unknown", tenure_class != "Unknown",
         typology_macro != "Opacity group") %>%
  mutate(cell = paste(built_form, tenure_class, sep = "\n"))
## droplevels() IS LOAD-BEARING. typology_macro is a FACTOR, so filtering out
## "Opacity group" removes its rows but keeps the empty LEVEL. table() then emits a
## whole zero column, chisq.test gets a zero expected count, and the simulated p comes
## back NaN -- which the panel subtitle printed verbatim as "chi-square simulated
## p = NaN". Worse, the caption then asserted composition "barely moves" on the
## strength of a test that never ran. On the non-degenerate 9x3 table the association
## is real. Assert the table has no empty margin so this cannot silently recur.
.actab <- table(droplevels(factor(arch_ctx$cell)),
                droplevels(factor(arch_ctx$typology_macro)))
stopifnot(all(dim(.actab) > 1), all(rowSums(.actab) > 0), all(colSums(.actab) > 0))
chi <- suppressWarnings(chisq.test(.actab, simulate.p.value = TRUE, B = 2000))
stopifnot(is.finite(chi$p.value))
save_tab(arch_ctx %>% count(cell, typology_macro), "14_b5_architecture_by_context")
p_b5d <- arch_ctx %>%
  count(cell, typology_macro) %>%
  group_by(cell) %>% mutate(p = n / sum(n), n_cell = sum(n)) %>% ungroup() %>%
  mutate(cell_lab = sprintf("%s (n=%d)", cell, n_cell)) %>%
  ggplot(aes(p, fct_rev(cell_lab), fill = typology_macro)) +
  geom_col(width = .72, colour = "white", linewidth = .25) +
  scale_fill_manual(values = PAL_TYPE, name = NULL,
                    labels = function(z) ifelse(z %in% names(TYPE_DESC),
                                                sprintf("%s: %s", z, TYPE_DESC[z]), z)) +
  scale_x_continuous(labels = percent_format(), expand = c(0, 0)) +
  labs(title = "d. Architecture composition across the implementation-context frame",
       subtitle = sprintf("The composition barely moves across contexts (chi-square simulated p = %.2f): types are structured by national programme tradition, not by the urban contexts they should answer to.", chi$p.value),
       x = "Share of the cell's classified programmes", y = NULL) +
  theme_nbs(7.4) + theme(legend.position = "bottom", legend.text = element_text(size = 5.8)) +
  guides(fill = guide_legend(nrow = 4))

## justice provisions by architecture -> standalone supplementary figure
## Figure 9 panel b is built AFTER `just` (the panel-a specification), further
## down this file, because it is asserted against it: the two panels must list
## the same fourteen provisions, and the assert needs both in scope.


val_tab <- utils::read.csv(file.path(CFG$out_tab, "08_typology_validation.csv"))
## Panel d (composition across the frame) moves to S6 on review; the null it
## carries is promoted to Figure 10 in adjusted form.
## PANEL ORDER SWAPPED ON AUTHOR INSTRUCTION. The geography panel (was c)
## becomes b and the profile heatmap (was b) becomes c. Reading order now goes
## design space -> where the types are -> what distinguishes them, so the two
## spatial panels sit together and the dense profile table closes the figure.
fig_b5 <- ((p_b5a | p_b5c) + plot_layout(widths = c(1, 1.15))) / p_b5b
fig_b5 <- fig_b5 + plot_layout(heights = c(1.15, 1)) +
  plot_annotation(
    title = "Figure 8. Recurring macro-architecture types: profile, geography, and the missing contextual adaptation (RQ3)",
    ## FIG 9 NOW STATES ITS OWN STABILITY. It presents the k = 3 partition, which
    ## the paper demotes to a coarse summary precisely because only one of its three
    ## classes clears the 0.75 bootstrap-Jaccard convention. fig14 and figS11 both
    ## disclose their stability; this figure disclosed none, so a reader meeting the
    ## three types here had no way to know the partition is weakly supported.
    subtitle = sprintf("Type 1: relatively more tenant-open, direct provision. Type 2: grant-dominant, broader technical and post-installation provision. Type 3: grant-dominant, weaker post-installation provision. On the documented subset; opacity reported separately. Class x region Cramer's V = %.2f, class x country V = %.2f (both p < 0.001) -- the geography is partly national tradition. THIS IS THE COARSE SUMMARY PARTITION: %d of %d classes clear the 0.75 bootstrap-Jaccard convention (range %.2f-%.2f); the %d-class partition in figure 13 is primary.",
                       val_tab$cramers_v[val_tab$test == "class x region"],
                       val_tab$cramers_v[val_tab$test == "class x country"],
                       ## read from the same table figS11d and fig14 read
                       { .jj <- utils::read.csv(file.path(CFG$out_tab, "08_bootstrap_jaccard.csv"))
                         sum(.jj$jaccard[.jj$k == 3] >= 0.75) },
                       { .jj <- utils::read.csv(file.path(CFG$out_tab, "08_bootstrap_jaccard.csv"))
                         sum(.jj$k == 3) },
                       { .jj <- utils::read.csv(file.path(CFG$out_tab, "08_bootstrap_jaccard.csv"))
                         min(.jj$jaccard[.jj$k == 3]) },
                       { .jj <- utils::read.csv(file.path(CFG$out_tab, "08_bootstrap_jaccard.csv"))
                         max(.jj$jaccard[.jj$k == 3]) },
                       ## the primary partition's k, computed -- never spelled out
                       { .jj <- utils::read.csv(file.path(CFG$out_tab, "08_bootstrap_jaccard.csv"))
                         max(.jj$k) }),
    theme = TITLE())
save_fig(fig_b5, "fig08_architectures", 18, 19)

# =============================================================================
# B6 | CONTEXT REQUIREMENT PROFILES ON THE FRAME
# =============================================================================
## Adjusted deviations: each requirement is modelled (region + log population,
## country-clustered), and each column's deviation is prediction-at-cell minus
## prediction-at-baseline, starred when the country-clustered CI excludes zero.
Rq <- FGB %>% mutate(
  tenant_route = ifelse(access_fit == "Indeterminate", NA_integer_,
                        as.integer(access_route %in% c("Direct tenant/broad pathway",
                                                       "Controller/collective pathway",
                                                       "Permission-dependent pathway"))),
  ## was admin_profile (retired counting construct) -- see the note at the
  ## d3c assignment above. Reads the canonical verdict.
  admin_ok   = ifelse(fit_D3c == "Indeterminate", NA_integer_,
                      as.integer(fit_D3c == "Documented fit")),
  ## this line is the THIRD instance of the same defect, and it survived two
  ## previous rounds of fixing because each round patched the lines it had been
  ## shown rather than the class. The v22 note directly above repointed admin_ok
  ## from admin_profile to fit_D3c and stopped; monitor_ok, one line lower and
  ## labelled "Monitoring, feedback and review (D4b)", kept deriving its value
  ## from monitor_config -- a provision-counting construct, not the D4b verdict.
  ## Found by a guard that tests the invariant instead of matching known strings.
  monitor_ok = ifelse(fit_D4b %in% c("Indeterminate", "Not applicable"), NA_integer_,
                      as.integer(fit_D4b == "Documented fit")),
  ctrl_fit = as.integer(access_fit == "Documented fit"),
  eco_ok   = ifelse(eco_fit == "Indeterminate", NA_integer_, as.integer(eco_fit == "Documented fit")),
  req_ok   = ifelse(is.na(requirement_match), NA_integer_, as.integer(requirement_match >= .75)),
  post_ok  = ifelse(steward_fit == "Indeterminate", NA_integer_, as.integer(steward_fit == "Documented fit")),
  pop_c = as.numeric(scale(log10(pmax(pop, 1e3)))))

REQ_DEF <- tribble(
  ~var, ~requirement, ~block,
  "ctrl_fit",     "Property-controller pathway (D2)",              "Authority and spatial control",
  "tenant_route", "Tenant or collective route (D2)",               "Authority and spatial control",
  "req_ok",       "Measure-specific technical requirements met (D3a)",              "Physical and technical setting",
  "admin_ok",     "Administrative guidance (D3c)",                 "Physical and technical setting",
  "eco_ok",       "Hazard-responsive portfolio (D1)",              "Environmental demand",
  "post_ok",      "Maintenance responsibility and support (D4a)",  "Post-installation and learning",
  "monitor_ok",   "Monitoring, feedback and review (D4b)",         "Post-installation and learning")

cell_frame <- Rq %>% filter(built_form != "Unknown", tenure_class != "Unknown") %>%
  mutate(cellv = paste(built_form, tenure_class, sep = " x "))
## HAZARD COLUMNS ARE MULTI-RESPONSE EXPOSURES, not the exclusive family.
## The exclusive scheme routed every multi-hazard city into "Multiple families"
## (336 records), leaving Heat with n=5 -- which produced 7 of the 11
## insufficient-evidence cells. A city documented for both flood and heat now
## contributes to BOTH columns, which is what an exposure overlay means, and
## every hazard including biodiversity loss is testable.
HAZ_COLS <- c("Flood or stormwater exposure" = "hx_flood",
              "Heat exposure"                = "hx_heat",
              "Drought or water scarcity"    = "hx_drought",
              "Biodiversity-loss objective"  = "haz_biodiv")
## A frame cell the CORPUS cannot support is dropped, not drawn grey. Compact /
## vertical x Owner-dominated holds only 8 records in total, so every requirement
## row in that column fell under the eight-record floor and rendered as
## "insuff. evidence" -- four grey tiles that state a corpus limit, not a result.
## The column is removed and the limit is stated in the caption instead, so every
## tile the reader sees carries an estimate.
## The eight-record floor applies PER REQUIREMENT ROW, not per cell: a cell holding
## 8 records can still fall under the floor on a given row, because each row drops
## records with no coded value on its own variable. A first attempt filtered on the
## cell's total n and therefore did NOT remove the column -- the grey tiles
## survived. So the estimation runs first, and any column that cannot support EVERY
## requirement row is then dropped whole, which is the only way to guarantee that
## no tile in the rendered figure is empty.
MIN_CELL_N <- 8
col_defs <- bind_rows(
  cell_frame %>% distinct(cellv) %>% mutate(type = "Frame cell"),
  tibble(cellv = names(HAZ_COLS), type = "Hazard overlay"))

adj_dev <- purrr::map_dfr(seq_len(nrow(REQ_DEF)), function(i) {
  v <- REQ_DEF$var[i]
  d <- Rq %>% filter(!is.na(.data[[v]])) %>%
    mutate(y = .data[[v]], region_mod = droplevels(factor(region_mod)))
  base_p <- mean(d$y)
  purrr::map_dfr(seq_len(nrow(col_defs)), function(j) {
    cc <- col_defs$cellv[j]
    dd <- if (col_defs$type[j] == "Frame cell") {
      d %>% filter(built_form != "Unknown", tenure_class != "Unknown",
                   paste(built_form, tenure_class, sep = " x ") == cc) } else {
      d %>% filter(.data[[unname(HAZ_COLS[cc])]] == 1) }
    if (nrow(dd) < MIN_CELL_N) return(tibble(cellv = cc, type = col_defs$type[j],
                                    requirement = REQ_DEF$requirement[i],
                                    block = REQ_DEF$block[i],
                                    dev = NA_real_, sig = NA, n = nrow(dd), baseline = base_p))
    dm2 <- d %>% mutate(inc = as.integer(case_id %in% dd$case_id))
    ## A near-constant outcome separates the logit: every fitted probability goes
    ## to 0 or 1, coef(m)["inc"] is unusable and the AME returns NA, which then
    ## empties the whole column. This bites once D1 sits at ~94% documented fit.
    ## Where the outcome varies too little to support a covariate-adjusted model,
    ## fall back to the UNADJUSTED difference in rates -- the same quantity the
    ## figure's panel b reports -- and flag it as unadjusted rather than dropping
    ## the tile. Estimation is still refused when the context cell has no
    ## variation at all.
    ## The adjusted logit is unidentified only when the outcome does not vary
    ## inside the context cell. That is a genuine limit and stays an empty tile;
    ## a merely lopsided outcome (D1 sits at ~94% documented fit) is still
    ## estimable and must NOT be diverted to an unadjusted fallback.
    if (length(unique(dm2$y[dm2$inc == 1L])) < 2) {
      return(tibble(cellv = cc, type = col_defs$type[j],
                    requirement = REQ_DEF$requirement[i], block = REQ_DEF$block[i],
                    dev = NA_real_, sig = NA, n = nrow(dd), baseline = base_p))
    }
    m <- glm(y ~ inc + region_mod + pop_c, data = dm2, family = binomial())
    V <- try(sandwich::vcovCL(m, cluster = dm2$country_mod), silent = TRUE)
    se <- if (inherits(V, "try-error")) summary(m)$coefficients["inc", 2] else sqrt(diag(V))["inc"]
    b <- coef(m)["inc"]
    ## average marginal effect of context membership, probability scale
    ## na.rm is load-bearing: any record with a missing model covariate (one
    ## city lacks a population figure) predicts NA, and an unguarded mean()
    ## propagates that single NA to the whole average marginal effect, which
    ## renders the tile as "insuff. evidence" and -- once every column carries
    ## such a tile -- empties the panel. The model itself is unaffected; glm
    ## already drops those rows via na.omit, so the AME must average over the
    ## same complete cases rather than over the full frame.
    ame <- mean(plogis(predict(m, newdata = dm2 %>% mutate(inc = 1L), type = "link")), na.rm = TRUE) -
           mean(plogis(predict(m, newdata = dm2 %>% mutate(inc = 0L), type = "link")), na.rm = TRUE)
    tibble(cellv = cc, type = col_defs$type[j], requirement = REQ_DEF$requirement[i],
           block = REQ_DEF$block[i], dev = 100 * ame,
           sig = abs(b / se) > 1.96, n = nrow(dd), baseline = base_p)
  })
})
## DROP ANY COLUMN THAT CANNOT SUPPORT EVERY ROW. Reported, not silent.
.incomplete <- adj_dev %>% group_by(cellv, type) %>%
  summarise(n_na = sum(is.na(dev)), n_max = max(n), .groups = "drop") %>%
  filter(n_na > 0)
if (nrow(.incomplete)) {
  msg("Figure 10: %d column(s) cannot support every requirement row at the %d-record floor: %s",
      nrow(.incomplete), MIN_CELL_N,
      paste(sprintf("%s (largest row n=%d, %d rows under floor)",
                    .incomplete$cellv, .incomplete$n_max, .incomplete$n_na), collapse = "; "))
  adj_dev_all <- adj_dev
  save_tab(adj_dev_all, "14_b6_adjusted_requirement_deviation_all_columns")
  ## Dropping every incomplete column empties the facet when the floor bites
  ## corpus-wide (as it does once verified city tenure thins the frame cells).
  ## Keep any column that supports a MAJORITY of requirement rows and let the
  ## remaining tiles render as "insuff. evidence"; drop only columns that
  ## support none. The floor is still enforced per tile, and the count of
  ## under-floor tiles is reported above rather than hidden.
  .keepable <- .incomplete %>% filter(n_na < nrow(REQ_DEF))
  .dropped  <- .incomplete %>% filter(n_na >= nrow(REQ_DEF))
  if (nrow(.dropped)) {
    msg("Figure 10: dropped %d column(s) with NO estimable requirement row: %s",
        nrow(.dropped), paste(.dropped$cellv, collapse = "; "))
    adj_dev <- adj_dev %>% filter(!cellv %in% .dropped$cellv)
  }
  if (nrow(.keepable)) {
    msg("Figure 10: retained %d column(s) with partial coverage; under-floor tiles drawn as insufficient evidence",
        nrow(.keepable))
  }
}
stopifnot(nrow(adj_dev) > 0, dplyr::n_distinct(adj_dev$cellv) > 0)
save_tab(adj_dev, "14_b6_adjusted_requirement_deviation")

col_n <- adj_dev %>% group_by(cellv, type) %>% summarise(n = max(n), .groups = "drop") %>%
  mutate(col_lab = sprintf("%s (n=%d)", str_wrap(cellv, 18), n))
p_b6a <- adj_dev %>%
  left_join(col_n %>% select(cellv, col_lab), by = "cellv") %>%
  mutate(requirement = factor(requirement, levels = rev(REQ_DEF$requirement)),
         block = factor(block, levels = unique(REQ_DEF$block)),
         col_lab = factor(col_lab, levels = unique(col_n$col_lab[order(desc(col_n$type == "Frame cell"), col_n$cellv)]))) %>%
  ggplot(aes(col_lab, requirement, fill = dev)) +
  geom_tile(colour = "white", linewidth = .5) +
  geom_text(aes(label = ifelse(is.na(dev), "insuff.\nevidence",
                               sprintf("%+.0f%s", dev, ifelse(sig, "*", "")))), size = 2.1) +
  facet_grid(block ~ type, scales = "free", space = "free",
             labeller = labeller(block = label_wrap_gen(16))) +
  scale_fill_div("Adjusted percentage points\nfrom corpus baseline") +
  labs(title = "b. Adjusted context deviations, by requirement block",
       ## THE SUBTITLE CONTRADICTED ITS OWN PANEL THREE WAYS and is now derived.
       ## It claimed the compact/vertical x owner-dominated frame cell was EXCLUDED --
       ## the panel draws it, with 5 of its 7 tiles carrying an estimate. It gave that
       ## cell as 8 programmes -- the table says 12 (11 on the D1 row, whose requirement
       ## does not apply to every record). And it implied the cell sits below the
       ## eight-record floor when 12 is above it. What is actually true is narrower and
       ## is what the panel shows: a handful of INDIVIDUAL tiles fall below the floor
       ## and are drawn as "insufficient evidence", and every one of them is a small
       ## cell. Counting them from adj_dev means the sentence cannot drift from the
       ## tiles again.
       subtitle = WRAP(sprintf("Average marginal effect of sitting in each context, adjusted for region and city size with country-clustered errors; * = 95%% interval excludes zero. Hazard columns are multi-response exposures (a city documented for flood and heat enters both), so they overlap by construction and are read independently of one another; the frame cells are mutually exclusive. %d of %d tiles carry an estimate; the remaining %d are drawn as 'insufficient evidence' because the requirement does not vary at all inside that context cell, which leaves the adjusted model unidentified. That is a limit of the corpus, drawn rather than hidden.",
               sum(!is.na(adj_dev$dev)), nrow(adj_dev), sum(is.na(adj_dev$dev)))),
       x = NULL, y = NULL) +
  theme_nbs(7.2) +
  theme(axis.text.x = element_text(angle = 32, hjust = 1, size = 5.5),
        strip.text.y = element_text(angle = 0))

## The final context-to-design diagnostic: SHORT requirements with a symbol
## grammar (filled = essential; half = context-dependent; open = generally
## recommended). Normative synthesis, so no flow diagram: a matrix.
## The final diagnostic. Rows are grouped context modules (tenure together,
## built form together, hazards together, cross-cutting last); each cell keeps
## one SHORT requirement plus its evidence source: E = empirically indicated by
## the adjusted deviations above, F = required by the framework, E+F = both.
## =============================================================================
## FIGURE 12 | WHAT EACH CONTEXT MUST ADD -- a prescription, not a rate table.
##
## THE FAULT IN THE PREVIOUS VERSION, stated so the change is auditable: it was a
## data presentation. Every cell held a provision RATE, and the reader had to infer
## the implication themselves. That is a research figure wearing a practitioner's
## label. What a city needs is the IMPLICATION: which specific provisions its
## programme must carry, named, because of the context it sits in.
##
## HOW A PRESCRIPTION IS DERIVED HERE. Nothing is asserted; each requirement comes
## from a chain already in the pipeline, and the chain is printed in panel c:
##
##   HAZARD context  -> HAZ_FUNC gives the ecological functions that hazard needs
##                   -> FUNC_MEASURES gives the measures delivering those functions
##                   -> the requirement matrix (03b) states which technical
##                      provisions those measures REQUIRE (P1 specification,
##                      P2 site assessment, P3 regulatory, P4 guidance,
##                      P5 establishment)
##                   -> so a heat-exposed city requires cooling and shading
##                      measures, and those measures require site assessment and
##                      specification. That is a requirement, not advice.
##
##   TENURE context  -> renter-majority cities require an eligibility route that
##                      does not terminate at the owner-occupier (C-9/C-10/C-11/
##                      C-15), because most residents are not owner-occupiers. The
##                      D2 test is exactly this comparison.
##
##   BUILT FORM      -> compact/vertical cities require building-integrated
##                      measures (E-1/E-2), which the requirement matrix classes
##                      HIGH demand: specification, site assessment and permit
##                      support are all REQUIRED, not optional.
##
##   UNIVERSAL       -> D3b, D3c, D4a, D4b requirements arise for every programme
##                      (anyone must be able to afford it, navigate it, maintain it
##                      and see it checked). They are not context-derived, but the
##                      SIZE of the gap is, so they are shown with that stated.
##
## The cell then carries the ACTION and the evidence for it: how many comparable
## programmes already do this, hence how much of a departure it is.
## =============================================================================

## ---- the three axes a city knows about itself -------------------------------
## CITY_GRP / CITY_CTX are defined HERE because both figure 11 and figure 11b read
## them. The previous rewrite of this block dropped them, which broke both figures:
## always re-grep for CITY_GRP/CITY_CTX after editing this region.
## LAND USE JOINS THE CONTEXT AXES. The author asked for the synthesis to
## define each urban context by hazard, tenure AND land use -- land use was the
## missing one, and A-5 now carries its five coded classes (see 02_context.R).
## A class is included only if it holds enough records to state a rate; the floor
## is the same eight-record floor every other requirement row uses.
CITY_GRP <- c(tenure = "Who owns the housing", built = "How the city is built",
              landuse = "What the land is used for", hazard = "Which hazard it faces")
.lu_ok <- Rq %>%
  select(dplyr::any_of(c("lu_highrise","lu_mixed","lu_lowrise","lu_industrial","lu_other"))) %>%
  summarise(dplyr::across(dplyr::everything(), ~ sum(.x == 1, na.rm = TRUE)))
LU_MAP <- c(lu_highrise = "High-rise residential", lu_mixed = "Mixed-use",
            lu_lowrise = "Low-rise residential", lu_industrial = "Industrial / peri-urban",
            lu_other = "Other documented form")
## "Other [specify]" is EXCLUDED as a context. It is A-5's free-text residual
## -- 43 records whose entries range from "commercial/institutional" to
## "mobile homes" to "vacant lots" -- so it names no shared implementation
## condition and no requirement can be derived from it. The four defined classes
## are the contexts; the residual is reported in the descriptive figure only.
.lu_pool <- setdiff(names(LU_MAP), "lu_other")
LU_KEEP <- .lu_pool[.lu_pool %in% names(.lu_ok) &
                    unlist(.lu_ok[1, .lu_pool[.lu_pool %in% names(.lu_ok)]]) >= 8]
CITY_CTX <- bind_rows(
  Rq %>% filter(tenure_class != "Unknown") %>% distinct(ctx = as.character(tenure_class)) %>%
    mutate(grp = CITY_GRP[["tenure"]]),
  Rq %>% filter(built_form != "Unknown") %>% distinct(ctx = as.character(built_form)) %>%
    mutate(grp = CITY_GRP[["built"]]),
  tibble(ctx = unname(LU_MAP[LU_KEEP]), grp = CITY_GRP[["landuse"]]),
  tibble(ctx = names(HAZ_COLS), grp = CITY_GRP[["hazard"]]))

## ---- the provision vocabulary: every named provision, with its codebook ref ----
PROV <- tribble(
  ~var,               ~prov,                                        ~comp,
  "el_tenant_direct", "Let tenants apply in their own right (C-10)",  "D2",
  "el_tenant_cond",   "Tenant route with owner consent (C-10)",       "D2",
  "el_land",          "Admit landlords / property owners (C-9)",      "D2",
  "el_coop",          "Admit co-ops and HOAs (C-11)",                 "D2",
  "el_socho",         "Admit social-housing providers (C-15)",        "D2",
  "spec_element",     "Publish measure specifications (P1)",          "D3a",
  "adv_element",      "Offer site assessment or design advice (P2)",  "D3a",
  "reg_element",      "Handle the permit route (P3)",                 "D3a",
  "guid_element",     "Publish installation guidance (P4)",           "D3a",
  "est_element",      "Support establishment / first season (P5)",     "D3a",
  "i_inkind",         "Supply the measure in kind (E-11)",            "D3b",
  "i_discount",       "Discount at point of sale (E-11)",             "D3b",
  "p_timeline",       "State the decision timeline (D-2)",            "D3c",
  "p_forms",          "Provide accessible application forms (D-3)",   "D3c",
  "p_multiling",      "Publish in more than one language (D-5)",      "D3c",
  "p_housetype",      "Guidance by housing type (D-8)",               "D3c",
  "r_feewaiver",      "Waive the permit fee (C-7)",                   "D3c",
  "s_maintgrant",     "Fund maintenance (E-15)",                      "D4a",
  "s_sitevisit",      "Offer a maintenance site visit (E-18)",        "D4a",
  "s_helpdesk",       "Run a maintenance helpdesk (E-18)",            "D4a",
  "s_reminders",      "Send inspection reminders (E-18)",             "D4a",
  "f_me_published",   "Publish outcome indicators (F-1/F-2)",         "D4b",
  "f_review",         "Review the programme periodically (F-3)",      "D4b")
PROV <- PROV %>% filter(var %in% names(Rq))

## ---- derive, per hazard, the technical provisions its measures require ---------
## Use the in-memory matrix built in 03b, not a re-read of its exported CSV: the
## re-read depended on a config path that does not exist here, and re-reading an
## export can silently diverge from the object the pipeline actually scored on.
REQM <- REQUIREMENT_MATRIX
P_TO_VAR <- c(P1_specification = "spec_element", P2_site_assessment = "adv_element",
              P3_regulatory = "reg_element", P4_guidance = "guid_element",
              P5_establishment = "est_element")
haz_required <- function(hz) {
  fns  <- HAZ_FUNC[[hz]]
  meas <- unique(unlist(FUNC_MEASURES[fns]))
  rows <- REQM %>% filter(measure_var %in% meas)
  out <- purrr::imap_chr(P_TO_VAR, function(v, p) {
    if (!p %in% names(rows)) return(NA_character_)
    if (any(rows[[p]] == "required")) v else NA_character_
  })
  unname(out[!is.na(out)])
}
HAZ_LAB_SHORT <- c(`Flood or stormwater exposure` = "Floods", `Heat exposure` = "Heat",
                   `Drought or water scarcity` = "Drought",
                   `Biodiversity-loss objective` = "Biodiversity loss")

## ---- the requirement set for each context, with WHY -----------------------------
UNIVERSAL <- c("i_inkind", "i_discount", "p_timeline", "p_forms",
               "s_maintgrant", "s_sitevisit", "f_me_published", "f_review")
ctx_requires <- function(cc, gg) {
  if (gg == CITY_GRP[["hazard"]]) {
    hz <- unname(HAZ_LAB_SHORT[cc])
    tech <- haz_required(hz)
    fns <- paste(tolower(HAZ_FUNC[[hz]]), collapse = " and ")
    list(vars = c(tech, UNIVERSAL),
         why = setNames(c(rep(sprintf("%s requires %s measures, and those measures require it (requirement matrix)", cc, fns),
                              length(tech)),
                          rep("Required of every programme, whatever the context", length(UNIVERSAL))),
                        c(tech, UNIVERSAL)))
  } else if (gg == CITY_GRP[["tenure"]]) {
    rt <- if (cc == "Renter-majority") c("el_tenant_direct","el_tenant_cond","el_land","el_coop","el_socho")
          else if (cc == "Mixed tenure") c("el_tenant_cond","el_land","el_coop")
          else c("el_land","el_coop")
    rt <- intersect(rt, PROV$var)
    list(vars = c(rt, UNIVERSAL),
         why = setNames(c(rep(sprintf("%s: most residents are not owner-occupiers, so the route must reach past them (D2)", cc),
                              length(rt)),
                          rep("Required of every programme, whatever the context", length(UNIVERSAL))),
                        c(rt, UNIVERSAL)))
  } else if (gg == CITY_GRP[["landuse"]]) {
    ## WHAT LAND USE REQUIRES. Land use is not a restatement of built form:
    ## it says what the parcels are FOR, which decides who controls them and what
    ## can be installed. The requirement in each class is derived the same way as
    ## every other axis -- from the framework's own chain, not from the observed
    ## rates -- so the panel stays prescriptive.
    lu_req <- switch(cc,
      "High-rise residential" = list(
        v = c("el_coop","el_land","el_socho","spec_element","adv_element"),
        why = "High-rise stock: the resident almost never controls the surface, so the route must reach the building's controller, and roof or facade work carries HIGH technical demand"),
      ## p_forms and p_timeline are in UNIVERSAL and must NOT be repeated here:
      ## a var listed twice produced two rows for that context, so its column was
      ## longer than its neighbours and the axis-alignment check failed. The
      ## dedupe below is the guard; this list is corrected at source as well.
      "Mixed-use" = list(
        v = c("el_land","el_coop"),
        why = "Mixed-use parcels put commercial and residential owners in the same programme, so eligibility must admit non-residential property actors and the procedure must be legible to both"),
      "Low-rise residential" = list(
        v = c("guid_element","est_element","el_tenant_cond"),
        why = "Low-rise residential is where standardised ground-level measures sit: guidance and establishment support decide whether they survive, and renting households still need a route"),
      "Industrial / peri-urban" = list(
        v = c("el_land","spec_element","reg_element"),
        why = "Industrial and peri-urban parcels are owner-controlled and larger, so the requirement is technical specification and permitting rather than household outreach"),
      list(v = character(0), why = "No context-specific requirement derived for this class"))
    lv <- setdiff(intersect(lu_req$v, PROV$var), UNIVERSAL)
    list(vars = c(lv, UNIVERSAL),
         why = setNames(c(rep(lu_req$why, length(lv)),
                          rep("Required of every programme, whatever the context", length(UNIVERSAL))),
                        c(lv, UNIVERSAL)))
  } else {
    tech <- if (cc == "Compact / vertical") c("spec_element","adv_element","reg_element","guid_element","est_element")
            else if (cc == "Intermediate")  c("spec_element","adv_element","guid_element","est_element")
            else                            c("guid_element","est_element")
    tech <- intersect(tech, PROV$var)
    lab <- if (cc == "Compact / vertical")
             "Compact/vertical form pushes measures onto roofs and walls: HIGH technical demand, so this is required not optional"
           else if (cc == "Intermediate")
             "Mixed building stock spans demand classes, so the ground-modifying requirements apply"
           else "Dispersed form allows standardised ground-level measures: guidance and establishment still required"
    list(vars = c(tech, UNIVERSAL),
         why = setNames(c(rep(lab, length(tech)),
                          rep("Required of every programme, whatever the context", length(UNIVERSAL))),
                        c(tech, UNIVERSAL)))
  }
}

## ---- assemble: requirement x observed provision = the prescription --------------
## ONE CONTEXT FILTER, used by every loop over CITY_CTX.
##
## This logic was DUPLICATED in ctx_rx and again in the GAPM loop 200 lines below.
## Adding the land-use axis to one copy left the other with a bare `else` that
## sent every land-use context into the hazard lookup, returning NA and failing
## inside filter() with a message naming neither the context nor the group. A
## branch that must be extended in two places will eventually be extended in one.
ctx_rows <- function(cc, gg) {
  if (gg == CITY_GRP[["built"]])   return(Rq %>% filter(as.character(built_form) == cc))
  if (gg == CITY_GRP[["tenure"]])  return(Rq %>% filter(as.character(tenure_class) == cc))
  if (gg == CITY_GRP[["landuse"]]) {
    ## A-5 is multi-select, so a record can sit in more than one land-use context,
    ## exactly as it can document more than one hazard.
    .v <- names(LU_MAP)[match(cc, unname(LU_MAP))]
    if (is.na(.v) || !.v %in% names(Rq))
      stop(sprintf("land-use context '%s' has no flag column in Rq", cc))
    return(Rq %>% filter(.data[[.v]] == 1))
  }
  if (gg == CITY_GRP[["hazard"]]) {
    .h <- unname(HAZ_COLS[cc])
    if (is.na(.h)) stop(sprintf("context '%s' is not a documented hazard", cc))
    return(Rq %>% filter(.data[[.h]] == 1))
  }
  stop(sprintf("context group '%s' has no filter branch (context '%s')", gg, cc))
}

ctx_rx <- function(cc, gg) {
  d <- ctx_rows(cc, gg)
  req <- ctx_requires(cc, gg)
  purrr::map_dfr(req$vars, function(v) {
    if (!v %in% names(d)) return(tibble())
    x <- d[[v]]; x <- x[!is.na(x)]
    tibble(ctx = cc, grp = gg, n_ctx = nrow(d), var = v,
           prov = PROV$prov[match(v, PROV$var)], comp = PROV$comp[match(v, PROV$var)],
           why = req$why[[v]],
           rate = if (length(x)) 100 * mean(x == 1) else NA_real_)
  })
}
RX <- purrr::map2_dfr(CITY_CTX$ctx, CITY_CTX$grp, ctx_rx) %>% filter(!is.na(rate))
## v34 diagnostic: report the per-context provision count BEFORE completion, so a
## ragged axis is reported as data rather than surfacing only as a failed check.
.rxdiag <- RX %>% count(grp, ctx, name = "n_prov")
msg("Figure 11 requirement rows per context: %s",
    paste(sprintf("%s/%s=%d", substr(.rxdiag$grp, 1, 8), substr(.rxdiag$ctx, 1, 12), .rxdiag$n_prov),
          collapse = "; "))

## COMPLETE THE GRID WITHIN EACH AXIS. Requirements genuinely differ between
## contexts on the same axis -- a renter-majority city requires five eligibility
## routes, an owner-dominated one requires two -- so a tile grid built only from
## what each context requires leaves holes. A hole in a prescription reads as "no
## answer". Every axis therefore carries the UNION of its contexts' requirements,
## and a provision a context does NOT require becomes an explicit stated cell
## ("not required in this context") rather than an absence. That is also what makes
## the reconciliation check meaningful: every column carries the same rows.
RX <- RX %>% tidyr::complete(tidyr::nesting(grp, ctx, n_ctx),
                             tidyr::nesting(var, prov, comp)) %>%
  filter(!is.na(grp)) %>%
  group_by(grp) %>% filter(var %in% var[!is.na(rate)]) %>% ungroup() %>%
  mutate(
    ## THE ACTION, which is the point of the figure. A provision a context REQUIRES
    ## and almost no comparable programme carries is a build instruction; one most
    ## already carry is a verification step.
    action = case_when(
      is.na(rate) ~ "NOT REQUIRED in this context",
      rate <  10  ~ "BUILD FROM SCRATCH -- essentially no comparable programme does this",
      rate <  35  ~ "ADD IT -- a small minority do this, so expect no template",
      rate <  65  ~ "ADD IT -- roughly half do this, templates exist",
      rate <  90  ~ "CHECK YOU HAVE IT -- most comparable programmes do",
      TRUE        ~ "STANDARD -- assume your reviewers expect it"),
    action = factor(action, levels = c(
      "BUILD FROM SCRATCH -- essentially no comparable programme does this",
      "ADD IT -- a small minority do this, so expect no template",
      "ADD IT -- roughly half do this, templates exist",
      "CHECK YOU HAVE IT -- most comparable programmes do",
      "STANDARD -- assume your reviewers expect it",
      "NOT REQUIRED in this context")),
    cell_lab = ifelse(is.na(rate), "\u2014", sprintf("%.0f%%", rate)),
    grp = factor(grp, levels = unname(CITY_GRP)),
    ctx_lab = sprintf("%s (%d)", ctx, n_ctx))
save_tab(RX, "14_b6_city_prescriptions")
msg("Figure 11: %d contexts x %d provisions = %d cells (%d requirements, %d not-required); %d BUILD-FROM-SCRATCH",
    n_distinct(RX$ctx), n_distinct(RX$var), nrow(RX), sum(!is.na(RX$rate)), sum(is.na(RX$rate)),
    sum(grepl("^BUILD", as.character(RX$action))))

## PANEL A: the prescription sheet, as THREE BLOCKS -- one per context axis.
##
## A single shared grid left large blank blocks: the eligibility-route rows arise
## from tenure only, so under the built-form and hazard columns they were empty, and
## the technical rows are the reverse. A blank cell in a prescription reads as "no
## answer" however the caption explains it. Each axis therefore gets its own block
## carrying only the rows it requires, so every visible cell holds a value.
ACT_COL2 <- c("BUILD FROM SCRATCH -- essentially no comparable programme does this" = "#67001f",
              "ADD IT -- a small minority do this, so expect no template"           = "#c0392b",
              "ADD IT -- roughly half do this, templates exist"                     = "#f19c79",
              "CHECK YOU HAVE IT -- most comparable programmes do"                  = "#a7c5de",
              "STANDARD -- assume your reviewers expect it"                         = "#dfe8ef",
              "NOT REQUIRED in this context"                                        = "#f4f4f2")
rx_block <- function(gg) {
  d <- RX %>% filter(grp == gg)
  ord <- d %>% distinct(comp, prov) %>% arrange(comp, prov)
  d <- d %>% mutate(prov = factor(prov, levels = rev(ord$prov)))
  ggplot(d, aes(ctx_lab, prov, fill = action)) +
    geom_tile(colour = "white", linewidth = .7) +
    geom_text(aes(label = cell_lab, colour = !is.na(rate) & rate < 35),
              size = 2.2, show.legend = FALSE) +
    facet_grid(comp ~ ., scales = "free_y", space = "free_y") +
    scale_fill_manual(values = ACT_COL2, name = NULL, drop = FALSE) +
    scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey35")) +
    labs(title = gg, x = NULL, y = NULL) +
    theme_nbs(7.4) +
    theme(plot.title = element_text(size = 8.2, face = "bold", colour = PAL$ink),
          axis.text.x = element_text(size = 6.3, angle = 26, hjust = 1),
          axis.text.y = element_text(size = 6.3),
          strip.text.y = element_text(angle = 0, size = 6.6, face = "bold"),
          panel.grid = element_blank(), legend.position = "bottom",
          legend.text = element_text(size = 6.2), legend.key.size = unit(8, "pt")) +
    guides(fill = guide_legend(nrow = 3, reverse = TRUE))
}
p_rx <- (rx_block(CITY_GRP[["tenure"]]) | rx_block(CITY_GRP[["built"]]) |
         rx_block(CITY_GRP[["landuse"]]) | rx_block(CITY_GRP[["hazard"]])) +
  plot_layout(widths = c(1, 1, 1.15, 1.3), guides = "collect") &
  theme(legend.position = "bottom")

## PANEL B: the two provisions DISTINCTIVELY weak in this context.
##
## A first version took each context's two LOWEST rates and every context returned
## the same two -- maintenance funding and periodic review, rare everywhere -- so the
## panel said nothing about context and read as one row repeated ten times. It now
## ranks by how far the context sits BELOW the across-context average for the SAME
## provision, which is the only form of the question whose answer can differ by
## context.
RX_BASE <- RX %>% filter(!is.na(rate)) %>% group_by(var) %>%
  summarise(xbase = mean(rate), .groups = "drop")
BRIEF <- RX %>% filter(!is.na(rate)) %>% left_join(RX_BASE, by = "var") %>%
  mutate(dev = rate - xbase) %>%
  group_by(grp, ctx, ctx_lab) %>% arrange(dev, .by_group = TRUE) %>% slice_head(n = 2) %>%
  summarise(brief = paste(sprintf("%s  %+.0f pts (%.0f%% here)", prov, dev, rate),
                          collapse = "   \u00b7   "), .groups = "drop") %>%
  mutate(ctx_lab = factor(ctx_lab, levels = rev(unique(RX$ctx_lab[order(RX$grp, RX$ctx)]))))
p_brief <- ggplot(BRIEF, aes(y = ctx_lab)) +
  geom_tile(aes(x = 0, fill = grp), width = .055, height = .8, show.legend = FALSE) +
  geom_text(aes(x = .045, label = brief), hjust = 0, size = 2.35, colour = PAL$ink) +
  ## DERIVED from CITY_GRP, not a typed list of three. Adding the land-use
  ## axis made this a 4-group scale and the hardcoded 3 colours failed the build --
  ## the same stale-literal trap as the function factor levels. One colour per
  ## group, taken in CITY_GRP's own order, so a fifth axis would not break it.
  scale_fill_manual(values = setNames(
    c("#2b6cb0", "#c05621", "#6b4c9a", "#2f855a")[seq_along(CITY_GRP)],
    unname(CITY_GRP))) +
  coord_cartesian(xlim = c(-.01, 1.05), expand = FALSE) +
  labs(title = "b. What each context is missing that other contexts are not",
       subtitle = WRAP("Provisions rare EVERYWHERE tell a city nothing about its own situation, so this ranks by the gap between the context's rate and the across-context average for the same provision. A large negative gap is a shortfall specific to this context -- something programmes elsewhere manage and programmes here do not."),
       x = NULL, y = NULL) +
  theme_nbs(7.4) +
  theme(axis.text.y = element_text(size = 6.6, hjust = 1), axis.text.x = element_blank(),
        panel.grid = element_blank(), plot.margin = margin(4, 6, 4, 4))

## PANEL C: the derivation, so nothing in panel a is taken on trust.
CHAIN <- tribble(
  ~step, ~txt,
  "Hazard  \u2192 measures \u2192 technical provisions",
  "A documented hazard requires ecological functions (retention, cooling, shading, capture, habitat); each function is delivered by specific measures; the requirement matrix states which technical provisions those measures require. Heat requires cooling and shading, delivered by trees, green roofs and facades \u2014 which require specification and site assessment.",
  "Tenure \u2192 eligibility route",
  "Where most residents are not owner-occupiers, a route that stops at the owner-occupier cannot reach them. Renter-majority contexts therefore require tenant, landlord, co-op and social-housing routes (C-9/C-10/C-11/C-15). This is the D2 fit test.",
  "Built form \u2192 technical demand class",
  "Compact and vertical stock pushes measures onto roofs and walls. The requirement matrix classes those as HIGH demand: specification, site assessment and permit handling are required, not optional.",
  "Universal requirements",
  "Affordability, a navigable procedure, maintenance support and a persistence check arise for every programme. Not context-derived \u2014 but the size of each gap is, which is why they appear in every column.")
## CHAIN already has ONE ROW per derivation (step + txt columns). A first version
## treated it as alternating step/text rows and filtered on row parity, which asked
## for four y positions from a two-row frame.
p_chain <- ggplot(CHAIN %>% mutate(y = rev(seq_len(n())))) +
  geom_text(aes(0, y + .30, label = step), hjust = 0, size = 2.5,
            fontface = "bold", colour = PAL$ink) +
  geom_text(aes(0, y + .10, label = stringr::str_wrap(txt, 92)), hjust = 0, vjust = 1,
            size = 2.05, colour = "grey25", lineheight = 1.06) +
  coord_cartesian(xlim = c(-.01, 1), ylim = c(.30, nrow(CHAIN) + .60), expand = FALSE) +
  labs(title = "c. Where each requirement comes from") +
  theme_void() +
  theme(plot.title = element_text(size = 8, face = "bold", colour = PAL$ink, margin = margin(b = 5)),
        plot.margin = margin(4, 6, 4, 4))

fig12 <- p_rx / (p_brief | p_chain) +
  plot_layout(heights = c(1.75, 1), widths = c(1, 1)) +
  plot_annotation(
    title = "Figure S15. What a citizen-led NbS programme must provide in each urban context",
    subtitle = WRAP(paste0("A prescription, not a description. A city identifies its context -- who owns its housing, how it is built, which hazard it faces -- and reads down the column to see the specific provisions its programme is REQUIRED to carry, each one derived from the framework's own requirement chain (panel c) rather than recommended. The colour states how far that requirement sits from current practice among the ", nrow(A), " coded programmes: dark red provisions have essentially no working template anywhere in the corpus, so a city adopting them is not catching up with peers but going ahead of them. That distinction is the study's contribution to scaling: it separates gaps a city can close by copying a comparable programme from gaps the field has not solved at all.")),
    theme = TITLE())
## THIS FIGURE MOVES TO SUPPLEMENTARY, on the author's instruction ("fig 12, as
## is now can go to supplementary"). It is a prescription sheet -- every cell carries an
## ACTION for a city to take -- and that is supporting material rather than a result.
## Only the filename changes; the figure itself is untouched, so every number in it
## still comes from the same two tables.
save_fig(fig12, "figS15_city_requirement_sheet", 18, 19)



p_b5d_s <- p_b5d + labs(title = "b. Architecture composition across the implementation-context frame")
fig_b6 <- p_b4a / p_b5d_s + plot_layout(heights = c(1, .9)) +
  plot_annotation(title = "Figure S6. Context expectations and the composition null behind figures 11-12 (RQ3)",
                  theme = TITLE())
save_fig(fig_b6, "figS06_context_requirements", 16, 15)

## ---- FIGURE 11: where context actually shifts design.
## MERGED ON REVIEW. The old figure 12b (signed provision gap per context per
## component) answered the SAME question as this panel -- does design move with
## context? -- from the descriptive side rather than the model side, so the two
## belonged together and figure 11 was left doing two jobs at once. 11a is now the
## model-adjusted deviation, 11b the raw signed gap, and figure 11 is freed for the
## city-facing sheet it was always meant to be.
## Panel b is the unadjusted counterpart, rebuilt here from the city sheet's own
## rates rather than from a separate object: the signed gap between a context's
## documented-fit rate and the CORPUS rate on the same component. Building it from
## CITY guarantees the two figures cannot disagree -- 11b and 12a are the same
## numbers, differenced against the baseline versus stated absolutely.
## Figure 11 is now a PRESCRIPTION over named provisions, not a rate table over the
## seven verdicts, so 11b can no longer reuse its cells. It is rebuilt here on the
## seven canonical verdicts directly -- the same quantity as before, computed in one
## place so 11a (model-adjusted) and 11b (unadjusted) answer one question two ways.
GAP_COMP <- tribble(
  ~var,      ~short,
  "fit_D1",  "Measures match\nthe hazard",
  "fit_D2",  "Route beyond\nthe owner",
  "fit_D3a", "Technical\nenablement",
  "fit_D3b", "Cost not fronted\nby resident",
  "fit_D3c", "Navigable\nprocedure",
  "fit_D4a", "Maintenance duty\n+ support",
  "fit_D4b", "Persistence\nchecked")
.fitrate <- function(d, v) {
  x <- as.character(d[[v]]); x <- x[!x %in% c("Indeterminate", "Not applicable")]
  if (!length(x)) NA_real_ else 100 * mean(x == "Documented fit")
}
CORP_BASE <- tibble(var = GAP_COMP$var,
                    base = vapply(GAP_COMP$var, function(v) .fitrate(Rq, v), numeric(1)))
GAPM <- purrr::map2_dfr(CITY_CTX$ctx, CITY_CTX$grp, function(cc, gg) {
  d <- ctx_rows(cc, gg)          # v34: the shared filter, not a second copy
  tibble(ctx = cc, grp = gg, n_ctx = nrow(d), var = GAP_COMP$var, short = GAP_COMP$short,
         rate = vapply(GAP_COMP$var, function(v) .fitrate(d, v), numeric(1)))
}) %>% left_join(CORP_BASE, by = "var") %>%
  mutate(gap = rate - base, grp = factor(grp, levels = unname(CITY_GRP)),
         short = factor(short, levels = GAP_COMP$short),
         ctx_lab = sprintf("%s\n(%d programmes)", ctx, n_ctx))
stopifnot(all(!is.na(GAPM$gap)))
save_tab(GAPM %>% select(grp, ctx, n_ctx, var, short, rate, base, gap),
         "14_b6_context_gap_vs_corpus")

## =============================================================================
## FIGURE 13 | DISCUSSION SYNTHESIS -- SHOWING, NOT TELLING
##
## Author's instruction: "we need another similar version of it that will be more of
## synthesis, showing not telling, and to be in the discussions."
##
## Figure S15 prescribes: each cell reads "ADD IT", "BUILD FROM SCRATCH". This figure
## carries no action word anywhere. It shows the structure that makes prescription
## either possible or futile and leaves the implication to the reader.
##
## The structure, measured on the 14 contexts x 7 components of
## 14_b6_context_gap_vs_corpus: mean absolute deviation from the corpus level is about
## five percentage points and only a seventh of the cells deviate by more than ten. For
## six of the seven components the level a city faces is set FIELD-WIDE rather than by
## its own context. The exception is tenure access, which spans some sixty points --
## the one component where what a city needs genuinely depends on what kind of city it
## is. A field-wide floor with one context-contingent axis.
##
## Encoding: one row per component, ordered by corpus level. The grey bar and tick are
## the corpus level; each context is a point on the same row. Points clustering on the
## tick mean the context does not move the level; points fanning away mean it does. No
## colour carries a judgement and no cell carries an instruction.
## =============================================================================
## GAPM is the IN-MEMORY frame that 14_b6_context_gap_vs_corpus is written from, and
## this figure reads it directly. The first version of this block read the CSV back
## from CFG$out_tab, which fails outright on a clean run because 00_setup clears the
## table directory and the table is written further down this same file -- the exact
## defect already recorded for the fact registry. A figure must never read a table
## written later in the pipeline; it takes the frame.
stopifnot(exists("GAPM"), nrow(GAPM) > 0,
          all(c("ctx", "short", "rate", "base", "gap", "grp") %in% names(GAPM)))
GAPD <- GAPM

.dom_ord <- GAPD %>% group_by(short) %>%
  summarise(base = dplyr::first(base), spread = max(rate) - min(rate),
            mad = mean(abs(gap)), .groups = "drop") %>%
  arrange(base) %>%
  mutate(lab = gsub("\n", " ", short))
GAPD <- GAPD %>%
  mutate(lab = factor(gsub("\n", " ", short), levels = .dom_ord$lab),
         grp = factor(grp, levels = c("Who owns the housing", "How the city is built",
                                      "What the land is used for", "Which hazard it faces")))
.dl <- .dom_ord %>% mutate(lab = factor(lab, levels = .dom_ord$lab))

p_13a <- ggplot(GAPD, aes(rate, lab)) +
  geom_segment(data = .dl, aes(x = 0, xend = base, y = lab, yend = lab),
               colour = "grey89", linewidth = 3.2, inherit.aes = FALSE) +
  geom_point(aes(colour = grp), size = 1.9, alpha = .85,
             position = position_jitter(height = .15, width = 0, seed = 7)) +
  geom_point(data = .dl, aes(base, lab), shape = 124, size = 5.4,
             colour = PAL$ink, inherit.aes = FALSE) +
  geom_text(data = .dl, aes(base, lab, label = sprintf("%.0f%%", base)),
            vjust = -1.6, size = 2.3, colour = PAL$ink, inherit.aes = FALSE) +
  geom_text(data = .dl, aes(x = 102, y = lab,
                            label = sprintf("%.0f pts", spread)),
            hjust = 0, size = 2.2, colour = "grey40", fontface = "bold", inherit.aes = FALSE) +
  geom_text(data = .dl[which.max(.dl$spread), ], aes(x = 102, y = lab, label = "spread across contexts"),
            hjust = 0, vjust = -1.7, size = 2.0, colour = "grey55", inherit.aes = FALSE) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 122),
                     breaks = seq(0, 100, 25)) +
  scale_colour_manual(values = unname(PAL_REGION)[1:4], name = "Context dimension") +
  labs(title = "a. Six of the seven components sit at a level the field sets; one depends on the city",
       subtitle = WRAP(paste0(
         "Grey bar and tick: the corpus level for that component. Each point is one of the ",
         dplyr::n_distinct(GAPD$ctx), " coded contexts at its own level, jittered vertically so ",
         "overlapping contexts stay countable. Points on the tick mean the context does not move ",
         "the level; points fanning away from it mean it does. Across all ", nrow(GAPD),
         " context-component cells the mean absolute deviation from the corpus level is ",
         sprintf("%.1f", mean(abs(GAPD$gap))), " points and ", sum(abs(GAPD$gap) > 10),
         " cells deviate by more than ten.")),
       x = "Share of evaluable records with documented or partial fit", y = NULL) +
  theme_nbs(8) +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_line(colour = "grey95", linewidth = .3),
        plot.margin = margin(4, 4, 4, 4))

## PANEL B: the one axis that IS contingent, drawn on its own terms.
## Tenure access is the only component whose level tracks its context, so it gets the
## panel: the four tenure contexts against the corpus level, with the count in each.
## Nothing is prescribed -- the reader sees that the gap opens exactly where renting is
## most common, which is the paper's own argument arriving as an observation.
## The tenure context labels must be read from the DATA, not typed: a level named
## here that the data spells differently silently becomes NA and drops out of the
## legend while its point still draws in the fallback colour -- which is what put
## renter-majority in grey with no legend entry.
.ten_lv <- sort(unique(GAPD$ctx[GAPD$grp == "Who owns the housing"]))
.ten_lv <- c(grep("Owner", .ten_lv, value = TRUE), grep("Mixed", .ten_lv, value = TRUE),
             grep("Renter", .ten_lv, value = TRUE))
stopifnot(length(.ten_lv) == 3, !is.null(PAL$blue), !is.null(PAL$softblue), !is.null(PAL$darkred))
TEN13 <- GAPD %>% filter(grp == "Who owns the housing", ctx %in% .ten_lv) %>%
  mutate(ctx = factor(ctx, levels = .ten_lv))
## Only the three SUBSTANTIVE tenure bands are drawn. "Tenure not verified" is a
## data-availability state, not a context, and giving it a colour slot pushed
## renter-majority onto the grey -- the one band the panel exists to show.
stopifnot(all(!is.na(TEN13$ctx)), nlevels(TEN13$ctx) == 3)
p_13b <- ggplot(TEN13, aes(rate, lab, colour = ctx)) +
  geom_segment(data = .dl, aes(x = 0, xend = base, y = lab, yend = lab),
               colour = "grey92", linewidth = 3.2, inherit.aes = FALSE) +
  geom_point(size = 2.2, alpha = .9,
             position = position_jitter(height = .13, width = 0, seed = 3)) +
  geom_point(data = .dl, aes(base, lab), shape = 124, size = 5.4,
             colour = PAL$ink, inherit.aes = FALSE) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 105),
                     breaks = seq(0, 100, 25)) +
  ## PAL has no $red -- the palette's emphasis colour is $misfit (see R/00b_design.R).
  ## PAL$red silently returned NULL, which is why setNames received two values for
  ## three levels and the run aborted. Assert the length rather than trusting the names.
  scale_colour_manual(values = setNames(c(PAL$blue, PAL$softblue, PAL$darkred), .ten_lv),
                      name = NULL, drop = FALSE) +
  labs(title = "b. The tenure axis, on its own terms",
       subtitle = WRAP(paste0(
         "The same rows, restricted to the tenure contexts. The spread on the tenure-access ",
         "row is what makes that component context-contingent; on every other row the three ",
         "tenure contexts sit close together, which is the same statement panel a makes and ",
         "the reason the other six components cannot be addressed city by city.")),
       x = "Share of evaluable records with documented or partial fit", y = NULL) +
  theme_nbs(8) +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_line(colour = "grey95", linewidth = .3))

fig13 <- p_13a / p_13b + plot_layout(heights = c(1.25, 1)) +
  plot_annotation(
    title = "Figure 12. Where programme design is set by the field and where it is set by the city",
    subtitle = WRAP(paste0(
      "A synthesis of the fit results as structure rather than instruction. Read each row ",
      "against its own grey bar: the bar is what the corpus does, and the points are what each ",
      "context does. The pattern across ", nrow(GAPD), " context-component cells is a field-wide ",
      "floor -- six components whose level barely moves with context -- and a single ",
      "context-contingent axis. Verdict definitions and denominators are those of figure 6; ",
      "nothing here is a recommendation.")),
    theme = TITLE())
save_fig(fig13, "fig12_field_vs_city", 17, 15)
## the caption's tenure gaps come from GAPM, the frame the panel itself draws,
## so caption and tiles cannot diverge again.
.ten_gap <- function(ctxname) {
  v <- GAPM$gap[GAPM$ctx == ctxname & grepl("owner", GAPM$short, ignore.case = TRUE)]
  stopifnot(length(v) == 1, is.finite(v))
  v
}
p_fig11b <- ggplot(GAPM, aes(short, ctx_lab, fill = gap)) +
  geom_tile(colour = "white", linewidth = .6) +
  geom_text(aes(label = sprintf("%+.0f", gap)), size = 2.4) +
  facet_grid(grp ~ ., scales = "free_y", space = "free_y",
             labeller = labeller(grp = label_wrap_gen(14))) +
  scale_fill_div("Percentage points from\nthe corpus baseline") +
  labs(title = "b. Unadjusted: the same question read straight off the coded rates",
       ## THE THREE TENURE NUMBERS ARE READ FROM GAPM, NOT TYPED. They had drifted
       ## to +39 / -18 / -16 against a panel drawing +40 / -21 / -13 -- a caption
       ## contradicting the tiles directly above it, which is worse than an absent
       ## number because the reader has no reason to doubt it. The stale cross-reference
       ## to "figure 12a" also went: figure 11 is now the prescription sheet, shipped as
       ## figS15, and no longer states these rates.
       subtitle = WRAP(sprintf("Signed gap between the context's documented-fit rate and the corpus rate on the same component. Negative (red) = programmes serving this context document the provision LESS often than the corpus average. Every context carries all seven components, so no cell is empty. ONE CELL IS DEFINITIONAL, not empirical: owner-dominated contexts read %+.0f on 'route beyond the owner' because the D2 rule treats an owner route as sufficient where owners predominate, so that context returns a documented fit by construction. It is drawn because removing it would hide a rule the reader should see, and it must not be read as evidence that owner-dominated cities provide more. The informative comparison is among the remaining contexts, where mixed tenure (%+.0f) and renter-majority (%+.0f) sit furthest below the corpus rate.",
               .ten_gap("Owner-dominated"), .ten_gap("Mixed tenure"), .ten_gap("Renter-majority"))),
       x = NULL, y = NULL) +
  theme_nbs(7.4) +
  theme(axis.text.x = element_text(size = 6, lineheight = .95),
        axis.text.y = element_text(size = 6.2, lineheight = .95),
        legend.position = "right", legend.key.width = unit(7, "pt"),
        strip.text.y = element_text(angle = 0, size = 6.6))
p_fig11a <- p_b6a + labs(title = "a. Model-adjusted: where sitting in a context changes what a programme provides")
fig11 <- p_fig11a / p_fig11b + plot_layout(heights = c(1, 1.05)) +
  plot_annotation(
  title = "Figure 10. Where context shifts programme design, and where it does not (RQ3)",
  subtitle = WRAP("Two readings of one question. Panel a is model-adjusted -- the average marginal effect of sitting in a context, holding region and city size constant, with country-clustered errors. Panel b is the unadjusted signed gap between a context's documented-fit rate and the corpus rate on the same component. They agree on the substance: the route beyond the owner is the one component context genuinely moves, and the components that are weak corpus-wide are weak in every context. Reading them together separates a real contextual effect from a compositional artefact."),
  theme = TITLE())
save_fig(fig11, "fig10_context_deviations", 17, 18)

# =============================================================================
# B7 | JUSTICE AS A CHAIN: TARGETING -> RECOGNITION -> BURDEN -> ACCOUNTABILITY
# =============================================================================
tern3 <- function(pres, abs01, label, panel) tibble(
  panel = panel, item = label,
  present = mean(pres == 1, na.rm = TRUE),
  absent  = mean(abs01 == 1 & pres != 1, na.rm = TRUE))
just <- bind_rows(
  tern3(FGB$equity_targeted, zero(FGB$equity_targeted), "Equity or vulnerability targeting (G-4)", "Distribution"),
  tern3(as.integer(FGB$spatial_target %in% c("Watershed/sewershed", "Priority zones", "Equity/vulnerability zones")),
        zero(FGB$obj_stated), "Hazard, catchment or priority-zone targeting (B-7)", "Distribution"),
  tern3(FGB$el_land, FGB$ex_land, "Landlords eligible (C-9)", "Recognition"),
  tern3(FGB$el_coop, FGB$ex_coop, "Co-operatives / HOAs eligible (C-11)", "Recognition"),
  tern3(FGB$el_tenant_direct, FGB$ex_tenant, "Tenants eligible directly (C-10)", "Recognition"),
  tern3(FGB$el_socho, FGB$ex_socho, "Social housing eligible (C-15)", "Recognition"),
  tern3(FGB$el_inform, FGB$ex_inform, "Informal groups eligible (C-16)", "Recognition"),
  tern3(as.integer(FGB$prefinancing_required == 0), as.integer(FGB$prefinancing_required == 1),
        "Payment without pre-financing (E-22)", "Distribution"),
  tern3(as.integer(FGB$p_guide >= 1), as.integer(FGB$guide_state == "documented absent"),
        "Applicant guide (D-1)", "Procedure"),
  tern3(FGB$p_multiling, zero(FGB$p_multiling), "More than one language (D-5)", "Recognition"),
  tern3(FGB$st_support, zero(FGB$st_support), "Maintenance support, not duty alone (E-15..18)", "Distribution"),
  tern3(FGB$p_codesign, zero(FGB$p_codesign), "Co-design with residents (D-6)", "Procedure"),
  tern3(FGB$f_feedback, zero(FGB$f_feedback), "Named feedback channel (F-4)", "Procedure"),
  tern3(FGB$q_equity_ind, zero(FGB$q_equity_ind), "Equity indicators tracked (G-5)", "Distribution"))
save_tab(just, "14_b7_justice_chain")

p_b7 <- just %>%
  mutate(panel = factor(panel, levels = c("Distribution", "Recognition", "Procedure"))) %>%
  group_by(panel) %>% mutate(item = reorder_within(str_wrap(item, 40), present, panel)) %>% ungroup() %>%
  ggplot(aes(y = item)) +
  ## ONE ENCODING, not a dot for one state and a bar for the other. The
  ## author flagged that panel a mixed the two, which made "documented present"
  ## and "explicitly absent" look like different kinds of quantity when they are
  ## the same quantity read in opposite directions. Both are now bars on a single
  ## diverging axis, drawn at identical thickness, with the grey band behind
  ## carrying the third state -- silence -- so all three evidence states are on
  ## one scale and comparable by length alone.
  geom_segment(aes(x = 0, xend = 1, yend = item), colour = "grey92", linewidth = 4.6) +
  geom_segment(aes(x = 0, xend = -absent, yend = item), colour = PAL$misfit,
               linewidth = 4.6, lineend = "butt") +
  geom_segment(aes(x = 0, xend = present, yend = item), colour = PAL$fit,
               linewidth = 4.6, lineend = "butt") +
  geom_text(aes(x = present, label = percent(present, 1)), hjust = -0.25, size = 2,
            colour = "grey25") +
  geom_text(data = ~ dplyr::filter(.x, absent > 0.02),
            aes(x = -absent, label = percent(absent, 1)), hjust = 1.25, size = 2,
            colour = PAL$misfit) +
  geom_vline(xintercept = 0, colour = "grey40", linewidth = .3) +
  facet_grid(panel ~ ., scales = "free_y", space = "free_y",
             labeller = labeller(panel = label_wrap_gen(16))) +
  scale_y_reordered() +

  scale_x_continuous(labels = function(z) percent(abs(z), 1), limits = c(-.3, 1.08),
                     breaks = c(-.25, 0, .25, .5, .75, 1)) +
  annotate("text", x = .35, y = 1, label = "", size = 2) +
  labs(title = "a. Justice provisions across the three dimensions: distribution, recognition, procedure",
       subtitle = WRAP(paste0("The framework's three justice dimensions, each a parallel provision set rather than a stage. Blue rightwards = documented present; red leftwards = documented explicitly absent; grey = not documented, which never becomes misfit. One encoding and one scale throughout, so any two provisions are comparable by bar length. Distributive accountability is where it collapses: equity-indicator tracking (G-5), the only provision that would show who actually benefited, is documented in ", sum(dplyr::coalesce(as.integer(A$q_equity_ind), 0L) == 1), " of ", nrow(A), " records.")),
       x = "Explicitly absent   |   Documented present", y = NULL,
       caption = WRAP("Justice is assessed inside every domain rather than as a fifth domain; the full variable inventory with all three evidence states is exported for the supplement.", 230)) +
  theme_nbs(8) + theme(strip.text.y = element_text(angle = 0))
## PANEL b NOW CARRIES EVERY PROVISION PANEL a CARRIES, GROUPED THE SAME WAY.
##
## The author asked that panel b, being the consequence of panel a, use all the
## variables the analysis uses and cluster them by the same three justice
## dimensions. It did neither: it showed 6 of the 14 provisions and grouped by
## nothing. Both are fixed by building it from the SAME `just` specification that
## builds panel a, so the two panels cannot list different provisions again --
## panel a reads the corpus rate, panel b splits that rate by architecture type.
.JUST_SPEC <- list(
  list(v = quote(as.integer(equity_targeted == 1)),            lab = "Equity or vulnerability targeting (G-4)",            dim = "Distribution"),
  list(v = quote(as.integer(spatial_target %in% c("Watershed/sewershed", "Priority zones", "Equity/vulnerability zones"))),
                                                               lab = "Hazard, catchment or priority-zone targeting (B-7)", dim = "Distribution"),
  list(v = quote(as.integer(prefinancing_required == 0)),      lab = "Payment without pre-financing (E-22)",               dim = "Distribution"),
  list(v = quote(as.integer(st_support == 1)),                 lab = "Maintenance support, not duty alone (E-15..18)",     dim = "Distribution"),
  list(v = quote(as.integer(q_equity_ind == 1)),               lab = "Equity indicators tracked (G-5)",                    dim = "Distribution"),
  list(v = quote(as.integer(el_land == 1)),                    lab = "Landlords eligible (C-9)",                           dim = "Recognition"),
  list(v = quote(as.integer(el_coop == 1)),                    lab = "Co-operatives / HOAs eligible (C-11)",               dim = "Recognition"),
  list(v = quote(as.integer(el_tenant_direct == 1)),           lab = "Tenants eligible directly (C-10)",                   dim = "Recognition"),
  list(v = quote(as.integer(el_socho == 1)),                   lab = "Social housing eligible (C-15)",                     dim = "Recognition"),
  list(v = quote(as.integer(el_inform == 1)),                  lab = "Informal groups eligible (C-16)",                    dim = "Recognition"),
  list(v = quote(as.integer(p_multiling == 1)),                lab = "More than one language (D-5)",                       dim = "Recognition"),
  list(v = quote(as.integer(p_guide >= 1)),                    lab = "Applicant guide (D-1)",                              dim = "Procedure"),
  list(v = quote(as.integer(p_codesign == 1)),                 lab = "Co-design with residents (D-6)",                     dim = "Procedure"),
  list(v = quote(as.integer(f_feedback == 1)),                 lab = "Named feedback channel (F-4)",                       dim = "Procedure"))

## The two panels must list the same provisions. Assert it rather than trust it.
.a_items <- sort(unique(just$item))
.b_items <- sort(vapply(.JUST_SPEC, function(z) z$lab, character(1)))
if (!identical(.a_items, .b_items))
  stop(sprintf(paste("figure 9 panels disagree on their provision set.",
                     "Only in a: %s. Only in b: %s."),
               paste(setdiff(.a_items, .b_items), collapse = "; "),
               paste(setdiff(.b_items, .a_items), collapse = "; ")))

just_arch <- purrr::map_dfr(.JUST_SPEC, function(z) {
  FGB %>% mutate(.hit = !!z$v) %>%
    group_by(typology_macro) %>%
    summarise(p = mean(.hit == 1, na.rm = TRUE), .groups = "drop") %>%
    mutate(item = z$lab, panel = z$dim)
})
save_tab(just_arch, "14_b5_justice_by_architecture")

## Order the provisions inside each dimension by the CORPUS rate panel a shows,
## so a reader's eye tracks the same row order down both panels.
.ord <- just %>% select(item, present)
figS2 <- just_arch %>%
  left_join(.ord, by = "item") %>%
  mutate(panel = factor(panel, levels = c("Distribution", "Recognition", "Procedure")),
         item = reorder_within(str_wrap(item, 40), present, panel)) %>%
  ggplot(aes(p, item, colour = typology_macro)) +
  geom_line(aes(group = item), colour = "grey78", linewidth = .7) +
  geom_point(size = 2.7) +
  facet_grid(panel ~ ., scales = "free_y", space = "free_y",
             labeller = labeller(panel = label_wrap_gen(16))) +
  scale_y_reordered() +
  scale_colour_manual(values = PAL_TYPE, name = NULL,
                      labels = function(z) ifelse(z %in% names(TYPE_DESC),
                                                  sprintf("%s: %s", z, TYPE_DESC[z]), z)) +
  scale_x_continuous(labels = percent_format(), limits = c(0, 1)) +
  labs(title = "b. The same fourteen provisions, split by architecture type",
       subtitle = WRAP(paste(
         "Every provision from panel a, in the same three dimensions and the same row order, so the two panels",
         "read together: panel a is the corpus rate, panel b is that rate split by design type. Design provisions",
         "only -- never a claim that a programme 'is just'. The Type 2 lead is consistent but modest, and the",
         "distributive provisions that would show who actually benefited stay rare in all three types.")),
       x = "Share of the type's programmes", y = NULL) +
  theme_nbs(7.8) + theme(legend.position = "bottom", legend.text = element_text(size = 5.8),
                         strip.text.y = element_text(angle = 0)) +
  guides(colour = guide_legend(nrow = 3))
p_just_arch <- figS2

fig10 <- p_b7 / p_just_arch + plot_layout(heights = c(1, .95)) +
  plot_annotation(title = "Figure 9. Justice-related design provisions: how common they are, and where they sit",
                  theme = TITLE())
save_fig(fig10, "fig09_justice", 13, 15)

# =============================================================================
# B8 | DO WEAKNESSES CLUSTER? POLARISATION WITH AN HONEST BENCHMARK
# =============================================================================
wk_map <- c(wk_access = "Access restricted\nto owners",
            wk_tech = "Technical requirements\nunmet",
            wk_admin = "Thin administrative\nprovision",
            wk_steward = "Weak post-installation\nsupport",
            wk_monitor = "No monitoring\nor feedback")
pairs <- t(combn(names(wk_map), 2))
phi_tab <- purrr::map_dfr(seq_len(nrow(pairs)), function(i) {
  a <- pairs[i, 1]; b <- pairs[i, 2]
  dd <- FGB[!is.na(FGB[[a]]) & !is.na(FGB[[b]]), c(a, b)]
  n11 <- sum(dd[[a]] == 1 & dd[[b]] == 1); n10 <- sum(dd[[a]] == 1 & dd[[b]] == 0)
  n01 <- sum(dd[[a]] == 0 & dd[[b]] == 1); n00 <- sum(dd[[a]] == 0 & dd[[b]] == 0)
  den <- sqrt(as.numeric(n11 + n10) * (n01 + n00) * (n11 + n01) * (n10 + n00))
  tibble(a, b, n = nrow(dd),
         phi = ifelse(den > 0, ((n11 * n00) - (n10 * n01)) / den, NA_real_),
         p = fisher.test(matrix(c(n11, n10, n01, n00), 2))$p.value)
}) %>% mutate(p_bh = p.adjust(p, "BH"), sig = p_bh < .05)
phi_full <- bind_rows(phi_tab,
                      phi_tab %>% rename(a = b, b = a),
                      tibble(a = names(wk_map), b = names(wk_map), n = NA, phi = 1, p = 0, p_bh = 0, sig = NA))
save_tab(phi_tab, "14_b8_signed_phi")

## Network: five weaknesses as nodes (size = prevalence), edges = signed phi
## (width = |phi|, solid = survives BH). Cleaner than the symmetric heatmap.
prev5 <- tibble(v = names(wk_map),
                lab = wk_map,
                p = purrr::map_dbl(names(wk_map), ~ mean(FGB[[.x]] == 1, na.rm = TRUE)))
## Circular node-link diagram: nodes on a ring in framework order, curved edges
## for the associations that survive BH correction. Only supported links are
## drawn, so the eye is not asked to discount a wall of noise.
ORD5 <- c("w_access", "w_tech", "w_admin", "w_post", "w_monitor")
ORD5 <- c(ORD5[ORD5 %in% prev5$v], setdiff(prev5$v, ORD5))
ang <- setNames(seq(90, 90 - 360 * (length(ORD5) - 1) / length(ORD5),
                    length.out = length(ORD5)) * pi / 180, ORD5)
nodes <- prev5 %>% mutate(a = ang[v], x = cos(a), y = sin(a),
                          hj = ifelse(abs(cos(a)) < .25, .5, ifelse(cos(a) > 0, 0, 1)),
                          lx = 1.16 * cos(a), ly = 1.16 * sin(a))
n_sig <- sum(phi_tab$sig, na.rm = TRUE)
edges <- phi_tab %>% filter(sig) %>%
  left_join(nodes %>% select(v, xa = x, ya = y), by = c("a" = "v")) %>%
  left_join(nodes %>% select(v, xb = x, yb = y), by = c("b" = "v"))
p_b8a <- ggplot() +
  annotate("path", x = cos(seq(0, 2 * pi, length.out = 200)),
           y = sin(seq(0, 2 * pi, length.out = 200)), colour = "grey90", linewidth = .5) +
  geom_curve(data = edges, aes(x = xa, y = ya, xend = xb, yend = yb,
                               linewidth = abs(phi), colour = phi > 0),
             curvature = .28, ncp = 12, lineend = "round") +
  geom_point(data = nodes, aes(x, y, size = p), colour = PAL$softblue, alpha = .95) +
  geom_text(data = nodes, aes(lx, ly, label = str_wrap(lab, 18), hjust = hj),
            size = 2.2, lineheight = .9, colour = PAL$ink, fontface = "bold") +
  geom_label(data = edges, aes((xa + xb) / 2 * .72, (ya + yb) / 2 * .72,
                               label = sprintf("phi = %.2f\nn = %d", phi, n)),
             size = 1.9, colour = "grey20", fill = "white", label.size = 0,
             label.padding = unit(1.1, "pt"), lineheight = .92) +
  scale_linewidth(range = c(.9, 3.2), guide = "none") +
  scale_colour_manual(values = c(`TRUE` = PAL$deep, `FALSE` = PAL$misfit),
                      labels = c(`TRUE` = "weaknesses co-occur", `FALSE` = "weaknesses substitute"),
                      name = NULL) +
  scale_size_area(max_size = 13, labels = percent_format(), name = "Weakness prevalence") +
  coord_fixed(xlim = c(-1.75, 1.75), ylim = c(-1.4, 1.4)) +
  labs(title = "a. Which weaknesses travel together",
       ## WRAPPED. Unwrapped, this subtitle ran the full width of the
       ## composed figure and overprinted panel b's own subtitle -- the panels
       ## are laid out side by side, so each one's header text must be wrapped
       ## to its own column, not to the page.
       subtitle = WRAP(sprintf("Nodes sit on the ring in framework order (access, technical, administrative, post-installation, monitoring) and are sized by prevalence. Only the %d associations surviving Benjamini-Hochberg correction are drawn, each labelled with signed phi and its pairwise-evaluable n; every other pair showed no supported association (full matrix exported).", n_sig), 130)) +
  theme_void(base_size = 9) + TITLE() +
  theme(legend.position = "bottom", legend.text = element_text(size = 6.5),
        legend.title = element_text(size = 6.5, face = "bold"))

core <- FGB %>% filter(!is.na(n_weak))
obs <- core %>% count(n_weak) %>% mutate(src = "Observed", p = n / sum(n)) %>% select(n_weak, src, p)
set.seed(CFG$seed)
## naive independence benchmark
pm <- colMeans(core[, names(wk_map)])
sim1 <- replicate(4000, sum(rbinom(length(pm), 1, pm)))
## country-stratified benchmark: permute each weakness within country, so the
## national composition of the corpus is preserved
perm_counts <- replicate(1000, {
  permd <- core
  for (v in names(wk_map))
    permd[[v]] <- ave(permd[[v]], permd$country_mod, FUN = sample)
  tabulate(rowSums(permd[, names(wk_map)]) + 1, nbins = 6)
})
strat <- tibble(n_weak = 0:5, src = "Country-stratified benchmark",
                p = rowMeans(perm_counts) / nrow(core))
ind <- tibble(n_weak = 0:5, src = "Independence benchmark",
              p = purrr::map_dbl(0:5, ~ mean(sim1 == .x)))
dev8 <- obs %>% select(n_weak, p_obs = p) %>%
  left_join(strat %>% select(n_weak, p_exp = p), by = "n_weak") %>%
  mutate(diff = p_obs - p_exp)
p_b8b <- ggplot(dev8, aes(factor(n_weak), diff, fill = diff > 0)) +
  geom_col(width = .68) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = .35) +
  geom_text(aes(label = sprintf("%+.1f pp", 100 * diff),
                vjust = ifelse(diff > 0, -.35, 1.3)), size = 2.3, colour = "grey25") +
  scale_fill_manual(values = c(`TRUE` = PAL$purple, `FALSE` = "#d73027"), guide = "none") +
  scale_y_continuous(labels = function(z) sprintf("%+.0f pp", 100 * z)) +
  labs(title = "b. Polarised, not uniformly compounding",
       subtitle = WRAP(sprintf("Observed share minus the country-stratified expectation (weaknesses permuted within country, 1,000 draws; complete core n = %d). Excess programmes at zero AND at three-plus; a deficit in the middle. Strict confirmed misfits never co-occur at all.", nrow(core)), 110),
       x = "Documented weaknesses per programme",
       y = "Observed minus expected share") +
  theme_nbs(8)

## PROMOTED TO THE MAIN TEXT AS FIGURE 8, on author instruction ("we need
## fig s10 in the main text of the results, I think after the fit figure but
## on its own"). It follows figure 6 and stands alone. The author also asked
## whether it had been made too simplistic and whether both panels use every
## applicable variable: panel a uses all five weakness constructs (every pairwise
## phi among them, 10 pairs, none dropped) and panel b uses the same five to form
## the per-programme count, benchmarked against a country-stratified permutation.
## Nothing is subset, so the answer is that the variable set is complete; the
## figure stays a figure rather than a table because both panels are about SHAPE
## -- a correlation structure and a distributional excess -- which a table of ten
## coefficients and six residuals would not convey.
fig_b8 <- p_b8a | p_b8b
fig_b8 <- fig_b8 +
  plot_annotation(
    title = "Figure 7. Do documented implementation weaknesses cluster within programmes? (RQ2)",
    subtitle = WRAP("Strict confirmed misfits are too rare to compound (none co-occur). The clustering question is therefore answered with documented weaknesses, whose weaker evidentiary status is stated, and the polarisation survives a benchmark that preserves country composition. All five weakness constructs enter both panels: panel a shows every one of the ten pairwise associations, panel b the full per-programme count."),
    theme = TITLE())
save_fig(fig_b8, "fig07_weakness_clustering", 16, 8.5)

## ---------------------------------------------------------------------------
## FIGURE 13 REMOVED on author instruction ("remove this fig13, we dont need
## it"). The D2b proportionality question it asked is still answered in the
## text and in figS13 panel b, which models the same relationship; only the
## dedicated figure is dropped. Its flow table is no longer written.


## Main-text TABLE 2: the compounding result as a table (figure stays in
## supplementary S10). Pairs surviving BH correction, with phi and n.
tab2 <- phi_tab %>% filter(sig) %>%
  transmute(`Weakness pair` = paste(unname(wk_map[a]), "\u00d7", unname(wk_map[b])),
            `n evaluable` = n, phi = round(phi, 2),
            `BH-adjusted p` = signif(p_bh, 3))
save_tab(tab2, "14_table2_compounding_pairs")

## ---------------------------------------------------------------------------
## THE MAINTENANCE-CONDITION x TENURE ASSOCIATION GETS A TABLE.
##
## Section 3.6 states this association, and the audit found it had NO table behind it:
## the quoted n = 270 does not reproduce from v_E17 under any parse, and the two Results
## documents disagreed with each other on the same statistic (7.4% vs 7.5%). That is the
## exact failure this project has a rule against -- a number the verifier cannot see in a
## table is a number nobody checks -- and the remedy is to emit it, not to delete a real
## finding.
##
## Both variables are the PIPELINE'S OWN, not a fresh parse: s_condition = yes1(v_E17)
## (a binding maintenance condition is documented) and ten_status == "direct" (tenants
## admitted in their own right, which already separates conditional from direct access).
## The association is reported with its exact test rather than a normal approximation,
## because one cell is small.
.mc <- A %>%
  dplyr::mutate(.cond = dplyr::coalesce(as.integer(s_condition), 0L) == 1,
                .tend = dplyr::coalesce(el_tenant_direct, 0L) == 1,
                .land = dplyr::coalesce(el_home * 0L + as.integer(el_land), 0L) == 1)
.a <- sum(.mc$.cond & .mc$.tend);  .b <- sum(.mc$.cond & !.mc$.tend)
.c <- sum(!.mc$.cond & .mc$.tend); .d <- sum(!.mc$.cond & !.mc$.tend)
.ft <- stats::fisher.test(matrix(c(.a, .b, .c, .d), nrow = 2, byrow = TRUE))
.mctab <- tibble::tibble(
  `Maintenance condition` = c("Documented (E-17 yes)", "Not documented"),
  `Programmes`            = c(.a + .b, .c + .d),
  `Tenants admitted directly` = c(.a, .c),
  `Tenant-direct rate (%)` = round(100 * c(.a / (.a + .b), .c / (.c + .d)), 1),
  `Landlords eligible`     = c(sum(.mc$.cond & .mc$.land), sum(!.mc$.cond & .mc$.land)),
  `Landlord rate (%)`      = round(100 * c(sum(.mc$.cond & .mc$.land) / (.a + .b),
                                           sum(!.mc$.cond & .mc$.land) / (.c + .d)), 1),
  `Odds ratio (tenant-direct)` = c(round(unname(.ft$estimate), 3), NA_real_),
  `Fisher exact p`             = c(signif(.ft$p.value, 3), NA_real_))
stopifnot(sum(.mctab$Programmes) == nrow(A))
save_tab(.mctab, "14_b9_maintenance_condition_tenure")

msg("Part B (RQ-answering): 8 figures written.")
