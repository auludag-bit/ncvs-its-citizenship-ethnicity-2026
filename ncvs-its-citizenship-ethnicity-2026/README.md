# Replication Code: Citizenship, Ethnicity, and Identity Theft Victimization in the Pandemic Era

**Paper:** Uludag, A. Y., Sevinc, B., & Bisgin, H. (2026). Citizenship, Ethnicity, and Identity Theft Victimization in the Pandemic Era: Evidence from the 2021 National Crime Victimization Survey Identity Theft Supplement. *Proceedings of the 2026 APWG Symposium on Electronic Crime Research (eCrime 2026)*, IEEE.

**Authors:**
- Asim Yigit Uludag — Early College Alliance, Eastern Michigan University (auludag@emich.edu)
- Bilal Sevinc — School of Criminal Justice, Michigan State University (sevincbi@msu.edu)
- Halil Bisgin — Department of Computer Science, University of Michigan–Flint (bisgin@umich.edu)

---

## What This Repository Contains

This repository contains all analytic code needed to replicate the results reported in the paper. The analysis proceeds in two stages:

1. **Data preparation** (Python) — loads, inspects, and builds the analytic sample from the raw ICPSR data files.
2. **Primary analysis** (R) — estimates all design-corrected logistic regression models reported in the paper's tables, using Taylor-series linearized standard errors via the `survey` package.

The Python stage produces cleaned `.parquet` files that are read by some of the supplementary R scripts. The primary R script (`step4_svyglm.R`) reads the raw `.dta` files directly and does not depend on the Python output.

---

## Why Two Languages?

Python was used for data preparation and initial exploration. When a reviewer correctly pointed out that `statsmodels` GLM does not implement Taylor-series linearization for complex survey designs — meaning it produces model-based standard errors that understate true sampling variance for a stratified, clustered design like the NCVS — we re-estimated all primary tables using R's `svyglm` from the `survey` package.

**The Python scripts are retained in the repository because:**
- They document the full sample construction logic, including several methodological corrections discovered during analysis (the VS008 completion filter, the VS063 out-of-scope recode, the V2026 income recode).
- They produce the `.parquet` files used by the supplementary R scripts for multiple-comparisons correction and sensitivity decomposition.
- They provide a readable, step-by-step trace of how the analytic sample was built.

**For replication of the paper's tables, run the R scripts.** The Python scripts are supporting documentation.

---

## Data Access

The data are **not included** in this repository. Both datasets are publicly available at no cost through the Inter-university Consortium for Political and Social Research (ICPSR) at the University of Michigan.

| Wave | ICPSR Study | DOI | Format |
|---|---|---|---|
| 2018 NCVS-ITS | 37923 | 10.3886/ICPSR37923.v2 | Stata `.dta` |
| 2021 NCVS-ITS | 38501 | 10.3886/ICPSR38501.v1 | Stata `.dta` |

**To download:**
1. Go to https://www.icpsr.umich.edu
2. Search for study number 37923 (2018) and 38501 (2021)
3. Log in or register (free)
4. Download in **Stata (.dta)** format
5. Place the files in your working directory alongside the scripts

The expected filenames are:
- `37923-0001-Data.dta` (2018 wave)
- `38501-0001-Data.dta` (2021 wave)

These datasets are distributed under ICPSR's standard Terms of Use, which require registration and prohibit redistribution. By downloading the data you agree to those terms.

---

## Repository Structure

```
.
├── README.md               ← This file
├── python/
│   ├── step1_load_inspect.py       ← Load data, print frequencies
│   ├── step2_build_sample.py       ← Build analytic sample, save parquet files
│   ├── step3_regression.py         ← Python logistic regression (exploration only)
│   ├── step3b_sensitivity_employ.py ← Employment sensitivity check
│   └── step3c_income_desc.py       ← Income distribution for Table I
├── R/
│   ├── step4_svyglm.R              ← PRIMARY: Tables II–IV (design-corrected)
│   ├── step5_benefits_harmonize.R  ← Benefits fraud harmonization investigation
│   ├── step6_multiple_comparisons.R ← Holm correction, bootstrap CIs, cell counts
│   ├── step7d_correct_ns.R         ← Verify model Ns from model.frame()
│   └── step8_sensitivity_decompose.R ← Employment sensitivity decomposition
├── paper/
│   ├── ecrime_conference.tex       ← LaTeX source (eCrime 2026 IEEE format)
│   ├── references.bib              ← BibTeX bibliography
│   └── figure1_predicted_probs.py  ← Figure 1 (matplotlib)
└── docs/
    └── variable_map.md             ← Codebook-verified variable reference
```

---

## Requirements

### Python
Tested on Python 3.10+.

```bash
pip install pandas pyreadstat numpy pyarrow statsmodels tabulate matplotlib
```

### R
Tested on R 4.3+.

```r
install.packages(c("haven", "survey", "dplyr"))
```

---

## Step-by-Step Replication

### Step 0 — Prepare your working directory

Place both `.dta` files in a single working directory. All scripts assume they are run from that directory.

```
your_working_directory/
├── 37923-0001-Data.dta
├── 38501-0001-Data.dta
└── [scripts from this repository]
```

Copy the Python scripts from `python/` and the R scripts from `R/` into this directory, or edit the file paths at the top of each script.

---

### Step 1 — Inspect the raw data (Python)

```bash
python step1_load_inspect.py
```

**What it does:** Loads both `.dta` files, checks that all expected variables are present, and prints raw frequency tables for key variables. Compare the output against the codebooks to confirm variable coding before building anything.

**Expected output:**
- 2018: 139,997 rows, 6,614 columns
- 2021: 133,753 rows, 5,441 columns
- All primary variables present; VS017D, VS045A, VS045B absent in 2018 (expected — these are 2021-only variables)
- Weight sums: ~258M (2018), ~263M (2021) — approximates US population 16+

---

### Step 2 — Build the analytic sample (Python)

```bash
pip install pyarrow  # if not already installed
python step2_build_sample.py
```

**What it does:** Applies the ITS-completer filter, age restriction, citizenship and Hispanic recodes, outcome variable construction, control variable recodes, and gender exclusions. Saves three `.parquet` files for downstream use.

**Key methodological decisions documented in this script:**

1. **ITS-completer filter:** `VS008 ∈ {1, 2}` (personal or telephone interview). This is the authoritative completion indicator. An earlier version of this code used "at least one outcome screener ≠ 9" as the filter, which incorrectly captured only ~21,000 of the 93,300 2021 completers because the dual reference-period design routes lifetime non-victims past the past-year questions with code 9.

2. **Outcome recode:** Among confirmed ITS completers, code 9 on any past-year outcome screener (VS012 etc.) means the respondent was routed past the question because they reported never experiencing that type — this is a legitimate negative response and is recoded to 0 (not a victim), not missing.

3. **Income recode:** V2026 codes 8 and 9 are valid income bands ($20,000–$29,999), not missing codes. Only codes 98 and 99 are missing. An initial version of the code incorrectly recoded 8 and 9 as missing; this was corrected.

**Expected output:**
- 2018 analytic N: 102,015 (9.84% victimization)
- 2021 analytic N: 92,865 (8.19% victimization)
- Saves: `analytic_2018.parquet`, `analytic_2021.parquet`, `analytic_pooled.parquet`

---

### Step 3 — Check income distribution for Table I (Python)

```bash
python step3c_income_desc.py
```

**What it does:** Reads income directly from the raw `.dta` files (bypassing the parquet files, which stored the pre-correction income recode) and computes the weighted income distribution for Table I. Also requires the `.parquet` files from Step 2 in the same directory for the analytic sample row filter.

---

### Step 4 — Primary analysis: design-corrected models (R) ← **Start here for table replication**

```bash
Rscript step4_svyglm.R
```

Or open in RStudio and run interactively.

**What it does:** This is the primary analysis script. It:
- Rebuilds the analytic sample from scratch using the raw `.dta` files (does not depend on Python output)
- Declares the NCVS survey design: `id = ~V2118` (half-sample/PSU), `strata = ~V2117` (pseudostratum), `weights = ~VS363` (final ITS weight)
- Estimates all models using `svyglm` with `family = quasibinomial("logit")`
- Reports ORs, 95% CIs, and p-values using design-correct Taylor-series standard errors
- Produces Tables II, III, and IV from the paper

**Why `svyglm` and not Python statsmodels?** The NCVS uses a stratified, multi-stage cluster sample. Design-based (Taylor-series) standard errors account for the pseudostrata and half-sample structure. Model-based standard errors — as produced by statsmodels GLM with frequency weights — ignore this structure and understate standard errors, potentially making borderline results appear significant. This is why all primary tables in the paper use `svyglm`.

**Note on `fit$data` vs `model.frame(fit)`:** `svyglm` stores the full design dataset in `fit$data`, but the model is estimated on the complete-case subset after listwise deletion. Use `nrow(model.frame(fit))` for the correct model N. The adjusted models (which include income) have smaller Ns than the unadjusted models due to ~22% missing on V2026.

**Expected key results:**
- 2018 adjusted model: noncitizen OR ≈ 0.75 (p < .01), Hispanic OR ≈ 0.83 (p < .001), interaction ns
- 2021 adjusted model: noncitizen OR ≈ 0.84 (marginal), Hispanic OR ≈ 0.78 (p < .001), interaction ns
- Pooled year interactions: all non-significant; joint Wald F ≈ 0.70, p ≈ .556

---

### Step 5 — Benefits fraud harmonization (R)

```bash
Rscript step5_benefits_harmonize.R
```

**What it does:** Investigates whether the 12.1% → 78.8% shift in government-benefits fraud (VS075) between waves is comparable across waves. Examines VS064 and VS076 flag variables to determine whether attempted vs. completed incidents can be distinguished. Conclusion: they cannot — both flags record post-collection recoding decisions, not attempt/completion status. The shift is reported with an explicit harmonization caveat in the paper.

---

### Step 6 — Multiple comparisons and bootstrap CIs (R)

```bash
Rscript step6_multiple_comparisons.R
```

**What it does:** Three analyses in response to reviewer concerns about the subtype analysis:
1. **Cell counts:** Actual Hispanic × Noncitizen victim counts in each subtype (existing: n=44, new: n=4, other: n=5)
2. **Holm-adjusted p-values:** For all nine tests in Table IV (3 subtypes × 3 predictors). Only the Hispanic/existing-account protective effect survives correction.
3. **Bootstrap CIs:** 1,000 pseudostrata-aware bootstrap iterations for the Hisp × Noncit/new-account OR (3.33). Bootstrap 95% CI: [0.42, 16.84] — wider than the Wald CI and confirming the estimate is driven by a 4-victim cell.

**Note:** The bootstrap takes several minutes to run.

---

### Step 7 — Verify model Ns (R)

```bash
Rscript step7d_correct_ns.R
```

**What it does:** Runs all primary models and reports the correct N using `nrow(model.frame(fit))` for each. Use this to verify the Ns reported in Tables II–IV. This script was written to diagnose a reporting error in an earlier version where `nrow(fit$data)` (the full design dataset) was mistakenly used instead of `nrow(model.frame(fit))` (the model-complete subset).

---

### Step 8 — Employment sensitivity decomposition (R)

```bash
Rscript step8_sensitivity_decompose.R
```

**What it does:** Decomposes the shift in the Hispanic × Noncitizen interaction term when employment sector is added as a control. Four models (M1–M4) isolate whether the shift is driven by (a) adding employment as a covariate or (b) restricting to the employed subsample. Result: 95% of the shift is attributable to sample restriction (removing retirees, students, homemakers), not to the employment covariate itself.

---

### Figure 1 — Predicted probabilities

```bash
cd paper/
pip install matplotlib
python figure1_predicted_probs.py
```

Saves `figure1_predicted_probs.pdf` and `figure1_predicted_probs.png`. Upload the PDF to Overleaf alongside `ecrime_conference.tex`.

---

## Key Methodological Corrections Made During Analysis

These are documented here for full transparency. Each correction is reflected in the final scripts.

| Issue | Incorrect approach | Corrected approach | Where fixed |
|---|---|---|---|
| ITS-completer filter | `any(outcome != 9)` — captured only ~21K of 93K completers | `VS008 ∈ {1, 2}` — correct completion flag | `step2_build_sample.py` |
| Out-of-scope recode | Code 9 → NaN (missing) | Code 9 → 0 (not a victim, among completers) | `step2_build_sample.py` |
| Income missing codes | V2026 codes 8, 9 → NaN | Only codes 98, 99 → NaN; 8 and 9 are valid $20–29K bands | `step2_build_sample.py`, `step3c_income_desc.py` |
| Standard errors | statsmodels GLM (model-based) | svyglm Taylor-series (design-based) | `step4_svyglm.R` |
| Model N reporting | `nrow(fit$data)` (full dataset) | `nrow(model.frame(fit))` (complete cases) | `step7d_correct_ns.R` |
| V3075 employment labels | Codes mislabeled (1 = "Govt") | Code 1 = Private company (per codebook) | `step3b_sensitivity_employ.py`, `step8_sensitivity_decompose.R` |

---

## Variable Reference

See `docs/variable_map.md` for a complete, codebook-verified mapping of all variable names, coding schemes, and missing-value codes used in the analysis.

---

## Citation

If you use this code in your own research, please cite the paper:

```bibtex
@inproceedings{uludag_sevinc_bisgin_2026,
  author    = {Uludag, Asim Yigit and Sevinc, Bilal and Bisgin, Halil},
  title     = {Citizenship, Ethnicity, and Identity Theft Victimization
               in the Pandemic Era: Evidence from the 2021 {National Crime
               Victimization Survey Identity Theft Supplement}},
  booktitle = {Proceedings of the 2026 {APWG} Symposium on Electronic
               Crime Research ({eCrime} 2026)},
  year      = {2026},
  publisher = {{IEEE}}
}
```

---

## License

The code in this repository is released under the MIT License. The data are governed by ICPSR's Terms of Use; see https://www.icpsr.umich.edu/web/pages/datamanagement/dua.html.
