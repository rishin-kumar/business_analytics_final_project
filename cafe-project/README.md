# Prague Café Project – Batch 1 (cappuccino price and location)

Run from this folder, in order:

    Rscript analysis/01_cleaning.R
    Rscript analysis/02_descriptives.R
    Rscript analysis/03_tests.R

Then compile `report/cafe_report.tex` (pdflatex twice). Seed 2026, 5,000 bootstrap resamples.
Packages: ggplot2, dplyr, xtable, sandwich, lmtest, car, boot, scales, broom, tidyr, readr.

Before submitting, search the report for red TODO notes:
1. A1 prior guess: set `BENCHMARK` in `analysis/03_tests.R` to YOUR guess (placeholder 90) and rerun.
2. Partner name, GitHub link, R/package versions (`sessionInfo()`).
