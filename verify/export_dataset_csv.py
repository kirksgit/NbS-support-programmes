#!/usr/bin/env python3
"""Export the coded workbook's Coding Sheet to CSV.

WHY THIS EXISTS. verify_manuscript.py reads the corpus with pandas.read_csv
while the shipped corpus is an .xlsx, so without this exporter the one check
that asserts every number in the prose appears in the run's tables cannot run
at all.

Usage:  python3 verify/export_dataset_csv.py [workbook.xlsx] [out.csv]
Defaults to data/NBS_707_FINAL_VERIFIED.xlsx -> output/derived/corpus.csv
"""
import sys, os, csv
import openpyxl

src = sys.argv[1] if len(sys.argv) > 1 else "data/NBS_707_FINAL_VERIFIED.xlsx"
dst = sys.argv[2] if len(sys.argv) > 2 else "output/derived/corpus.csv"
os.makedirs(os.path.dirname(dst), exist_ok=True)

wb = openpyxl.load_workbook(src, data_only=True, read_only=True)
ws = wb["Coding Sheet"]
rows = [r for r in ws.iter_rows(values_only=True)]
wb.close()

## Row 1 is a section banner, row 2 the variable names, row 3 the template /
## instruction row. Matches CFG$header_row = 2 and CFG$drop_rows = 1 in R/00_setup.R.
header = [str(x) if x is not None else "" for x in rows[1]]
data = [r for r in rows[3:] if len(r) > 0 and r[0] not in (None, "")]

with open(dst, "w", newline="", encoding="utf-8") as fh:
    w = csv.writer(fh)
    w.writerow(header)
    for r in data:
        w.writerow(["" if c is None else c for c in list(r) + [""] * (len(header) - len(r))])

print(f"  exported {len(data)} records x {len(header)} columns -> {dst}")
