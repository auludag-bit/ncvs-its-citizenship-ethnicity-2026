# Citizenship, Ethnicity, and Identity Theft Victimization in the Pandemic Era

Code for the poster **"Citizenship, Ethnicity, and Identity Theft Victimization in the Pandemic Era: Evidence from the 2021 NCVS-ITS,"** presented at the 81st Annual Meeting of the American Society of Criminology (ASC), Chicago, IL, November 2026.

**Authors**
- Asim Yigit Uludag, Early College Alliance, Eastern Michigan University (auludag@emich.edu)
- Halil Bisgin, Department of Computer Science, University of Michigan–Flint (bisgin@umich.edu)

All results come from survey-weighted logistic regression with design-correct standard errors (Taylor-series linearization in R's `survey` package), using the 2018 and 2021 National Crime Victimization Survey Identity Theft Supplement (NCVS-ITS).

## Where each poster number comes from

| Poster element | Script | Archived output |
|---|---|---|
| Corrected samples (2018 N = 101,389; 2021 N = 92,211) and the odds ratios in the results chart | `R/step10_corrected_pipeline.R` | `logs/step10_corrected_pipeline.log` |
| "Same estimate, two error bars": OR 1.47 on the original study's 71,984 cases, ordinary vs. design-correct standard errors | `R/step10_corrected_pipeline.R` (Section 3 rebuilds the original sample) and `R/step14_factorial_audit.R` (all three estimators) | `logs/step10_corrected_pipeline.log`, `logs/step14_factorial_audit.log` |
| "Did any effect change between 2018 and 2021?": the pooled model's year × effect t-tests and the joint Wald F-test | `R/step11_crossyear_subtypes.R`, Section B | `logs/step11_crossyear_subtypes.log` |
| Weighted prevalence (9.29% in 2018, 7.71% in 2021) | `R/step11_crossyear_subtypes.R`, Section A | `logs/step11_crossyear_subtypes.log` |
| Government-benefits fraud among "other misuse" victims (12.1% to 78.8%) | `R/step11_crossyear_subtypes.R`, Section D | `logs/step11_crossyear_subtypes.log` |
| Victimization-subtype models, 2021, including email and social-media account misuse (not printed on the poster) | `R/step11_crossyear_subtypes.R`, Section C | `logs/step11_crossyear_subtypes.log` |
| Use of identity-theft protection services by group (1.5% vs. 8.2–11.0%) | `R/step10_corrected_pipeline.R` (protective-behavior models) | `logs/step10_corrected_pipeline.log` |
| Why screener code 9 ("out of scope") counts as "not a victim" rather than missing | `R/step11_scope_checks.R`, Section A | `logs/step11_scope_checks.log` |

The poster's mathematical extension (Panel 5) is a separate project and is not in this repository.

## Data

The data are **not included**: ICPSR's terms of use forbid redistributing them. Both files are free from the Inter-university Consortium for Political and Social Research (ICPSR) after registering.

| Wave | ICPSR study | DOI | File the scripts expect |
|---|---|---|---|
| 2018 NCVS-ITS | 37923 | [10.3886/ICPSR37923.v2](https://doi.org/10.3886/ICPSR37923.v2) | `37923-0001-Data.dta` |
| 2021 NCVS-ITS | 38501 | [10.3886/ICPSR38501.v1](https://doi.org/10.3886/ICPSR38501.v1) | `38501-0001-Data.dta` |

Download both in Stata (`.dta`) format and put them in one folder. Stata itself is not needed; R reads the files with the `haven` package.

## How to run

You need R with the `survey`, `haven`, and `dplyr` packages:

```r
install.packages(c("survey", "haven", "dplyr"))
```

Run each script from the repository folder, pointing `NCVS_DATA_DIR` at the folder that holds the two data files:

```bash
NCVS_DATA_DIR=/path/to/data Rscript R/step10_corrected_pipeline.R
NCVS_DATA_DIR=/path/to/data Rscript R/step11_crossyear_subtypes.R
NCVS_DATA_DIR=/path/to/data Rscript R/step11_scope_checks.R
NCVS_DATA_DIR=/path/to/data Rscript R/step14_factorial_audit.R
```

Each script prints its results to the console. `step10` and `step11_scope_checks` also write CSV files of aggregate results (no microdata) to `out_ccnc/`; set `NCVS_OUT_DIR` to use a different folder.

## About the archived output

`logs/` holds the console output from the runs that produced the poster's numbers. The only edit is that local folder paths were replaced with `[local path removed]`.

The code is the same as the version that produced those logs. For this repository the scripts were given shorter names and their comments were tidied; no line of code was changed.

## Citation

Uludag, A. Y., & Bisgin, H. (2026, November). *Citizenship, ethnicity, and identity theft victimization in the pandemic era: Evidence from the 2021 NCVS-ITS* [Poster presentation]. 81st Annual Meeting of the American Society of Criminology, Chicago, IL.
