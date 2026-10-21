# Support programmes for functional and just citizen-led nature-based solutions at scale

Replication materials for Enu et al., *Support programmes for functional and just
citizen-led nature-based solutions at scale*.

Everything the paper reports is computed here from the coded corpus of 707 programme records across 28 countries. The repository
contains the data, the analysis code, the reference outputs of the authors' run and a
launcher that reproduces those outputs and compares them cell by cell with yours.

## Run it

```bash
./reproduce.sh
```

The launcher checks that R and the required packages are present, runs the pipeline,
rebuilds the one figure drawn in Python, compares every output table against the
authors' run, and then runs the verification passes. It exits non-zero if anything
differs.

To run only the analysis, open `run_all.R` and press Source in RStudio, or run
`Rscript run_all.R` from anywhere. The script sets its own working directory to
the folder it sits in, so it does not matter where your R session happens to be
pointing; it prints the folder it moved to when it moves. Everything after that
step is relative to that folder.

Requirements are R 4.6.0 with `FactoMineR`, `MASS`, `brglm2`, `broom`, `cluster`,
`dplyr`, `forcats`, `ggalluvial`, `ggplot2`, `ggrepel`, `lmtest`, `maps`, `nnet`,
`patchwork`, `purrr`, `readr`, `readxl`, `rlang`, `sandwich`, `scales`, `stringi`,
`stringr`, `tibble`, `tidyr` and `writexl`, plus `jsonlite` for the two
spec-lock checks, and Python 3 with `pandas` and
`matplotlib` for the comparison and the one Python figure. The launcher names any
package that is missing and stops rather than running a partial analysis.

A full run takes a few minutes and writes to `output/`, which is not tracked.
`SESSION_INFO.txt` records the R version, the platform, the random seed and the
package versions the reference outputs were produced with.

## What is here

Listed in the order you meet them rather than alphabetically, so the two things
you actually run come first.

| Path | Contents |
|---|---|
| `reproduce.sh` | The single command. Checks prerequisites, runs the pipeline, rebuilds the Python figure, compares every table against ours and runs the verification passes |
| `run_all.R` | The analysis on its own, sourcing the 23 modules in order. Sets its own working directory, so it runs from anywhere, and classifies every warning it raises |
| `R/` | The analysis, as 23 modules run in order by `run_all.R` |
| `data/` | The coded corpus of 707 programme records, with every value carrying an evidence note, a source URL and an access date, plus the tenure provenance file, the frozen city geocodes and the measure classification |
| `protocol/` | The 75-variable coding instrument, the eligibility criteria, the search vocabulary and the screening and extraction protocols given to the assistants |
| `verify/` | The table comparator, the claim-level checker, the manuscript number checker, the corpus exporter, the Python figure, the spec regenerator and the tenure band sensitivity |
| `expected/tables/` | The authors' run, 158 CSVs, one per table, which `reproduce.sh` compares yours against |
| `expected/sensitivity/` | Output of the standalone checks under `verify/`, which `run_all.R` does not produce and the comparator therefore does not read |
| `QUOTABLE_FACTS.csv` | The keyed register of every number the paper quotes, emitted by `18_fact_registry` and read by the manuscript checker so that a figure in the prose is matched to the table it came from |
| `FITNESS_RULES.json` | The applicability and decision rules behind every fit verdict, in machine-readable form, read at run time by the spec-lock check and regenerated from the run by `verify/regenerate_spec.R` so it cannot drift from the analysis it describes |
| `SESSION_INFO.txt` | The R version and package versions the shipped run in `expected/` was produced on, so a difference in your run can be traced to a version rather than guessed at |
| `LICENSE` | The licence covering the code and the coded corpus |

## How the analysis is organised

`run_all.R` sources the modules in this order. Each writes its tables to
`output/tables/` through `save_tab`, which registers them in the run manifest. The
warnings log is the one file written outside that path.

| Module | What it does |
|---|---|
| `00_setup`, `00b_design` | Configuration, the fixed seed, shared helpers, palette and taxonomies |
| `01_load_clean`, `02_context` | Reads the coding sheet; builds the implementation context and the programme families |
| `03_architecture`, `03b_requirement_matrix`, `04_configurations` | Programme architecture, the technical requirement matrix, and observed configurations itemised before any grouping |
| `05_fitness` | The seven fit components and their evidentiary routes |
| `06_evaluability`, `07_fitness_models` | Who can be evaluated at all, then what explains fit, with the sensitivity analyses |
| `08_typology` | Multiple correspondence analysis, the architecture classes, their stability, and the access against finance estimate |
| `09_compound_misfit`, `10_context_matching` | Whether weaknesses cluster; context modules and readiness gaps |
| `11_reporting`, `12_classification_audit`, `13_provenance_validation` | Results tables, a test of whether the labels are rule artefacts, and the parser against the coders' own grades |
| `15_glossary`, `18_fact_registry`, `17_temporal` | The variable dictionary, every quotable number emitted once and keyed, and provision by launch period |
| `14a`, `14b`, `14c` | Figures, in three parts |
| `16_verify` | A standing correctness audit that halts the run on failure |

## Where the paper's numbers come from

| Paper | Output |
|---|---|
| Corpus, hazards, measures and support architecture (Results 3.1) | `14_measure_prevalence`, `14_measures_by_country`, `14_a2_hazard_function_measure_flows`, `14_instrument_prevalence`, `14_instrument_family_upset`, `14_a3_instrument_family_totals` |
| Fit verdicts (Results 3.3) | `14_b1_domain_states`, `05_misfit_route_composition`, `05_compound_headline_by_route` |
| Co-occurrence (Results 3.3) | `09_pairwise_compound_misfit`, `09_compounding_vs_independence` |
| Typologies and their stability (Results 3.4) | `08_macro_architecture_profiles`, `08_cluster_diagnostics`, `08_bootstrap_jaccard` |
| Access against financial support (Results 3.4) | `08_access_finance_crosstab`, `08_access_finance_effect` |
| Context and cohorts (Results 3.5) | `07_interaction_family_BH`, `10_readiness_gaps_by_context`, `17_temporal_by_launch_period` |
| Justice provisions (Results 3.6) | `11_equity_design_provisions`, `14_b7_justice_chain`, `14_b9_maintenance_condition_tenure` |
| Validation (Methods 6.3) | `13_evidence_quality_covered_subset`, `13_provenance_subset_comparison` |
| Programme-family dependence (Methods 6.1) | `14_c2_family_sensitivity`, `07_S4_programme_family_nonindependence` |

## Figures

The pipeline writes no image files by default, and the reason is worth stating
rather than leaving a reader to wonder. The figures in the paper were composed
separately, several of them around drawn illustrations that no plotting library
produces, so a ggplot rendering of the same variables would not resemble the
published figure. Shipping both would leave a reader holding two versions of one
figure with nothing to say which is authoritative.

What a reader actually needs is the numbers behind each published figure, and
those are in `expected/tables` under the mapping above. Every panel's underlying
frame is written there: the hazard-to-measure flows, the country-by-measure
shares, the verdict states, the class profiles.

The figure modules still run on every pass, because they are not only drawing
code. They compute those flow frames, reconcile each panel against the verdict
layer, and run the tautology guard, and sixteen checks in `16_verify.R` read the
objects they build. Removing them would drop sixteen correctness checks to save
some image files. Only the writing to disk is switched off.

To render them anyway, set `NBS_FIGURES=on` before running:

```bash
NBS_FIGURES=on ./reproduce.sh
```

## The tenure layer

City tenure shares come from national statistical agencies rather than from the
programme documents, so they carry their own provenance in
`data/TENURE_SOURCES.csv`. Every one of the 707 records names the agency, the
table, the geography and the reference year it was taken from, gives a link to
that table, and records the date it was read.

The link identifies the source table rather than certifying the figure, and that
distinction matters because agencies define tenure differently. Germany's census
tabulates households where this layer counts dwellings, and its published
Eigentumsquote counts occupied dwellings only. Switzerland's figures turn on
whether co-operative tenancies count as ownership, which moves Basel between
13.4% and 22.5%. Spain's turn on whether the denominator is all main dwellings
or only those owned and rented. Ireland tabulates Cork City together with Cork
County. A reader comparing a published national figure against the share
recorded here should expect a difference of a few points for these reasons.

The shares are derived accordingly. Where an agency publishes dwelling or
household counts rather than shares, the percentage is computed from those
counts; where it publishes only the owner-occupied rate, the renter share is its
complement. National categories do not always align with the owner and renter
split used here, most visibly where housing-cooperative stock is classified
outside owner occupation.

Nothing in the analysis reads this file. Tenure enters the pipeline from the
workbook's own columns and only as a three-band class cut at 35% and 55% renters,
so a revision of a few points moves a result only where it crosses a band.
`verify/tenure_boundary_sensitivity.R` moves every record sitting exactly on a
cut to the other side and recomputes: six verdict cells move, all in D2, and the
share of programmes carrying at least one confirmed misfit does not change.

## Verification

Five checks run, and all five must pass. Three run after the analysis, one runs inside it, and one compares the manuscript prose.

The **table comparison** reads every CSV in `expected/tables/` and in your `output/tables/`
and compares them cell by cell. It first plants known values in a copy of the reference
set and confirms it catches them, so a comparator that passed by failing to look would be
caught before its result is trusted.

The **claim-level checker** tests relations rather than values, so a number that is
internally consistent but used in a false relation is caught.

The **manuscript checker** asserts that every number in a manuscript text placed beside
this file appears in some output table. It reads `UNIT_OF_ANALYSIS.md`, and it reads the
Results prose as well when a `RESULTS.md` is placed here.

The **standing audit** in `16_verify` runs inside the analysis and halts it on failure.

## Notes

The corpus records each programme as documented on the date its sources were accessed,
between July and August 2026. Administrative pages change and links decay, so a source
re-checked later may no longer show what the evidence note quotes.

The contents of this repository are released under CC BY 4.0; see `LICENSE`.
The programme documents the corpus cites remain the property of the authorities
that published them and are not redistributed here, so the corpus records the
URL and the access date of every source instead.
