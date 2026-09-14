# Stroke Meta-Analysis — Aquatic vs Land-based/Conventional Therapy

## Files in this folder

- **Stroke Dataset.xlsx** — cleaned, tidy version of `Copy of Extract Data (STROKE).xlsx`,
  one row per study, for 6 outcomes: `BBS V2`, `MWT V2`, `10 MWT V2`, `TUG V2`, `MBI V2`, `FRT V2`.
  Each row has: study info (No, Study, Country, Design, Stroke_Type, Total_N, Follow_Up) and,
  for Group 1 = Aquatic/experimental arm and Group 2 = Land-based/Control arm (Group 3 only
  for the rare 3-arm trials): N, Age (mean/SD), Male/Female N, Baseline and After
  mean/SD (or median/Q1/Q3 where that's all the source reported).
- **meta_analysis.R** — the R script you asked for (uses `metafor`). Run it from this folder
  (or edit the `setwd()` line near the top) and it will read `Stroke Dataset.xlsx` and
  regenerate everything below. It: (1) drops rows whose Stroke_Type contains "Unspecified",
  (2) recodes anything that isn't exactly "Chronic" to "Subacute", (3) uses Chronic/Subacute
  as meta-analysis subgroups, (4) pools Hedges' g (Aquatic vs Land-based/Control, post-treatment
  scores) with a random-effects (DerSimonian-Laird) model, overall and per subgroup.
- **significant_tables.xlsx** — one sheet per outcome; each sheet has the Overall, Chronic and
  Subacute pooled results (Hedges' g, 95% CI, z, p-value, whether p<0.05, tau², I², Q,
  heterogeneity p-value) plus a Chronic-vs-Subacute subgroup-difference test.
- **Forest Plot/**, **Funnel Plot/**, **Leave-One-Out Plot/** — one plot per outcome (6 each).

## Important note on how these particular files were produced

R could not be reached from this sandbox (no network route to CRAN, and the bridge to your
computer's R install was unavailable during this session), so the **numbers, tables and plots
in this folder were computed in Python** (numpy/scipy/matplotlib), using the exact same
statistics as `meta_analysis.R` (Hedges' g, DerSimonian-Laird random effects, same
inclusion/exclusion rules). **`meta_analysis.R` is the real deliverable** — run it yourself in
R/RStudio on `Stroke Dataset.xlsx` and it will reproduce (and let you extend) everything here
using `metafor` directly.

## Data notes worth knowing before you report these numbers

- **Effect measure**: Hedges' g on the **After** (post-treatment) score, Aquatic vs
  Land-based/Control. Baseline scores are in the dataset if you'd rather analyze change scores.
- **3-arm studies** (Sagrario Pérez-de la Cruz 2021; Park chung 2018, appearing in BBS/MWT/10 MWT/TUG):
  the third arm (`AQ + PT` or `AG`, Anti-Gravity) is kept as Group 3 in the dataset but excluded
  from the pooled analysis, which only compares Group 1 vs Group 2.
- **MBI V2** — two studies (Zhang Y 2016 fully, Lee S. 2018 dropped as Unspecified) report
  median (IQR) instead of mean (SD). Mean/SD were approximated (mean ≈ median,
  SD ≈ (Q3−Q1)/1.35) for pooling; treat the MBI V2 result with more caution than the others.
- **Chan K., et al 2016** (Canada; appears in BBS/MWT/TUG): the source spreadsheet's "After"
  values for this study are tiny decimals (0.1–0.5) with no SD — almost certainly a data-entry
  issue in the original file. These studies were **excluded** from pooling for lack of a usable
  SD; worth checking the original source for this study if you want it included.
- Excluded-row counts per outcome (Unspecified removed, then missing-SD studies dropped):
  BBS V2 16→13, MWT V2 8→6, 10 MWT V2 4→4, TUG V2 10→9, MBI V2 5→3, FRT V2 5→5.
