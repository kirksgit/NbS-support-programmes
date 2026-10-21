# Inclusion and exclusion record

Screening initially retained 819 candidates, and eligibility reassessment,
source verification, duplicate consolidation and case-identity reconciliation
produced the final corpus of 707 programme records. That is the funnel the paper
reports and the study pipeline figure draws.

This file documents its last step, from the 709 rows the screening workbook held
when it closed to the 707 analysed. It accounts for every row of that
difference, and a standing check in `R/16_verify.R` fails the build if any row
becomes unaccounted for.

```
819 candidates retained at screening
  ... eligibility reassessment, source verification, duplicate
      consolidation and case-identity reconciliation
709 rows in the screening workbook at close
  - 2 excluded at final screening   (4 Mandurah, 436 Bowie)
  - 1 resolved into two records     (702 West Lynchburg)
  + 1 added after screening closed  (721 Lyon)
  = 707 analysed records
```

## Excluded at final screening

**4 Mandurah, Australia** and **436 Bowie, United States** were excluded at
final screening for failing the inclusion criteria, and neither shares a city,
a programme name or a source URL with any analysed record.

## Resolved rather than excluded

**702 West Lynchburg, United States** described two programmes in a single
row. Its B-2 field concatenated the names of the Stormwater Utility Credit
Program and the Rain Barrel Workshop Program, and its primary URL is the one
cited by analysed case 556, so the row was separated into cases 556 and 719
and the city string was corrected to Lynchburg. No case was lost and the
corpus gained one record here.

## Added after screening closed

**721 Lyon, France** is the 2026 edition of the Renaturons Lyon call for
projects, added after the screening workbook was frozen. Lyon therefore holds
three analysed records.
