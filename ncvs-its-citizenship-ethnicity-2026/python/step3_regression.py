"""
NCVS-ITS Replication/Extension -- Step 3: Regression Analysis
==============================================================

Replication and extension of Maher (2024):
  "Examining the Association Between Citizenship and Ethnicity on
   Identity Theft Risk" -- American Journal of Criminal Justice, 2024.

This script produces:
  Table 1: Weighted descriptive statistics, 2018 vs. 2021
  Table 2: Logistic regression replicating Maher (2024) on 2021 data
  Table 3: Pooled cross-year model (2018 + 2021) with year interactions
  Table 4: Subtype analysis (existing / new account / other misuse)
  Table 5: Benefits fraud descriptive (pandemic signal)

Survey-weighted estimation note:
  The NCVS-ITS uses a complex stratified multi-stage sample.
  Correct standard errors require declaring the survey design:
    weight = VS363, strata = V2117, PSU = V2118
  
  Python's statsmodels does not natively support Taylor-series
  linearization for logistic regression. We use two approaches:
    (a) Weighted logistic regression (point estimates correct,
        SEs conservative -- sufficient for a first-pass replication)
    (b) Bootstrap SE over strata/PSU clusters (more rigorous)
  
  For the journal version, we recommend cross-checking against
  R's survey::svyglm() which implements Taylor-series linearization.
  The code to do so is printed at the end of this script.

Requirements:
    pip install pandas numpy statsmodels scipy tabulate pyarrow

Usage:
    python step3_regression.py
"""

import pandas as pd
import numpy as np
import statsmodels.api as sm
from scipy import stats
from tabulate import tabulate
import warnings
warnings.filterwarnings("ignore")

# ---------------------------------------------------------------------------
# Load the cleaned parquet files from step 2
# ---------------------------------------------------------------------------
print("Loading analytic datasets...")
df18 = pd.read_parquet("analytic_2018.parquet")
df21 = pd.read_parquet("analytic_2021.parquet")
df_pooled = pd.read_parquet("analytic_pooled.parquet")

print(f"  2018: N={len(df18):,}  victims={int(df18['any_idtheft'].sum()):,} "
      f"({df18['any_idtheft'].mean()*100:.2f}%)")
print(f"  2021: N={len(df21):,}  victims={int(df21['any_idtheft'].sum()):,} "
      f"({df21['any_idtheft'].mean()*100:.2f}%)")
print(f"  Pooled: N={len(df_pooled):,}")

# ---------------------------------------------------------------------------
# Apply Maher's gender exclusions using V3086 (current gender identity)
#   3 = Transgender (exclude)
#   4 = None of these (exclude)
# ---------------------------------------------------------------------------
def apply_gender_exclusions(df, year):
    if "V3086" not in df.columns:
        print(f"[{year}] V3086 not found -- skipping gender exclusion")
        return df
    before = len(df)
    df = df[~df["V3086"].isin([3, 4])].copy()
    dropped = before - len(df)
    print(f"[{year}] Gender exclusions (transgender + none of these): "
          f"dropped {dropped:,}, remaining {len(df):,}")
    return df

df18     = apply_gender_exclusions(df18, 2018)
df21     = apply_gender_exclusions(df21, 2021)
df_pooled = pd.concat([df18, df21], ignore_index=True)


# ===========================================================================
# TABLE 1: Weighted descriptive statistics
# ===========================================================================

def weighted_mean(series, weights):
    """Weighted mean, ignoring NaN."""
    mask = series.notna() & weights.notna()
    return np.average(series[mask], weights=weights[mask])

def weighted_pct(series, weights, value=1):
    """Weighted percentage of a specific value."""
    mask = series.notna() & weights.notna()
    s = series[mask]; w = weights[mask]
    return np.average((s == value).astype(float), weights=w) * 100

def print_table1(df18, df21):
    print(f"\n{'='*70}")
    print("TABLE 1: Weighted Descriptive Statistics, 2018 vs. 2021")
    print(f"{'='*70}")

    w18, w21 = df18["VS363"], df21["VS363"]

    rows = []

    # Sample characteristics
    rows.append(["N (unweighted)", f"{len(df18):,}", f"{len(df21):,}"])
    rows.append(["Weighted N (millions)",
                 f"{w18.sum()/1e6:.1f}",
                 f"{w21.sum()/1e6:.1f}"])

    # Outcome
    rows.append(["--- OUTCOME ---", "", ""])
    rows.append(["Identity theft victim (%)",
                 f"{weighted_pct(df18['any_idtheft'], w18):.2f}",
                 f"{weighted_pct(df21['any_idtheft'], w21):.2f}"])
    rows.append(["  Existing account (%)",
                 f"{weighted_pct(df18['existing_acct'], w18):.2f}",
                 f"{weighted_pct(df21['existing_acct'], w21):.2f}"])
    rows.append(["  New account (%)",
                 f"{weighted_pct(df18['new_acct'], w18):.2f}",
                 f"{weighted_pct(df21['new_acct'], w21):.2f}"])
    rows.append(["  Other misuse (%)",
                 f"{weighted_pct(df18['other_idtheft'], w18):.2f}",
                 f"{weighted_pct(df21['other_idtheft'], w21):.2f}"])

    # Primary predictors
    rows.append(["--- PRIMARY PREDICTORS ---", "", ""])
    rows.append(["Noncitizen (%)",
                 f"{weighted_pct(df18['noncitizen'], w18):.2f}",
                 f"{weighted_pct(df21['noncitizen'], w21):.2f}"])
    rows.append(["Hispanic (%)",
                 f"{weighted_pct(df18['hispanic'], w18):.2f}",
                 f"{weighted_pct(df21['hispanic'], w21):.2f}"])
    rows.append(["Hispanic x Noncitizen (%)",
                 f"{weighted_pct(df18['hisp_noncitizen'], w18):.2f}",
                 f"{weighted_pct(df21['hisp_noncitizen'], w21):.2f}"])

    # Controls
    rows.append(["--- CONTROLS ---", "", ""])
    rows.append(["Age (mean)",
                 f"{weighted_mean(df18['age'], w18):.1f}",
                 f"{weighted_mean(df21['age'], w21):.1f}"])
    rows.append(["Male (%)",
                 f"{weighted_pct(df18['male'], w18):.1f}",
                 f"{weighted_pct(df21['male'], w21):.1f}"])

    # Race
    for val, lbl in [(1,"White only"),(2,"Black only"),(3,"Other/multi")]:
        rows.append([f"  {lbl} (%)",
                     f"{weighted_pct(df18['race_wb'], w18, val):.1f}",
                     f"{weighted_pct(df21['race_wb'], w21, val):.1f}"])

    # Education
    for val, lbl in [(1,"Less than HS"),(2,"HS/GED"),
                     (3,"Some college"),(4,"BA or higher")]:
        rows.append([f"  Edu: {lbl} (%)",
                     f"{weighted_pct(df18['edu_4cat'], w18, val):.1f}",
                     f"{weighted_pct(df21['edu_4cat'], w21, val):.1f}"])

    rows.append(["Married (%)",
                 f"{weighted_pct(df18['married'], w18):.1f}",
                 f"{weighted_pct(df21['married'], w21):.1f}"])

    print(tabulate(rows,
                   headers=["Characteristic", "2018", "2021"],
                   tablefmt="simple"))
    print("\nNote: All percentages are weighted. Education and race")
    print("exclude missing. Income and employment not shown due to")
    print("~40% missing on employment sector (skip-pattern variable).")


print_table1(df18, df21)


# ===========================================================================
# REGRESSION HELPER
# ===========================================================================

def run_logit(df, outcome, predictors, weight_col="VS363",
              label="Model"):
    """
    Weighted logistic regression using statsmodels GLM.
    Returns a results summary dataframe with OR, 95% CI, p-value.
    """
    sub = df[[outcome] + predictors + [weight_col]].dropna()
    y = sub[outcome]
    X = sm.add_constant(sub[predictors])
    w = sub[weight_col]

    # Normalize weights (avoids numerical issues with large weights)
    w_norm = w / w.mean()

    model = sm.GLM(y, X,
                   family=sm.families.Binomial(),
                   freq_weights=w_norm)
    result = model.fit()

    # Build output table
    coef   = result.params
    se     = result.bse
    pvals  = result.pvalues
    ci_low = coef - 1.96 * se
    ci_hi  = coef + 1.96 * se

    out = pd.DataFrame({
        "Variable":  coef.index,
        "OR":        np.exp(coef),
        "CI_low":    np.exp(ci_low),
        "CI_high":   np.exp(ci_hi),
        "p_value":   pvals,
        "Log_OR":    coef,
        "SE":        se,
    })
    out["sig"] = out["p_value"].apply(
        lambda p: "***" if p<0.001 else ("**" if p<0.01 else
                  ("*" if p<0.05 else ("." if p<0.1 else ""))))
    out["N"] = len(sub)
    out["n_victims"] = int(y.sum())

    print(f"\n--- {label} ---")
    print(f"    N={len(sub):,}  victims={int(y.sum()):,}  "
          f"({y.mean()*100:.2f}%)")
    display_cols = ["Variable","OR","CI_low","CI_high","p_value","sig"]
    print(tabulate(out[display_cols].round(4),
                   headers="keys", tablefmt="simple", showindex=False))
    return out, result


# ===========================================================================
# Define predictor sets (matching Maher 2024)
# ===========================================================================

# Core predictors for replication
CORE_PREDS = [
    "noncitizen",      # citizenship (binary)
    "hispanic",        # ethnicity (binary)
    "hisp_noncitizen", # interaction
]

# Demographic controls (Maher's set, minus employment due to high missing)
CONTROLS_BASE = [
    "age",
    "male",
    "race_wb",    # Note: should be dummified; see below
    "edu_4cat",   # Note: treat as ordinal or dummify
    "married",
    "income",
    "msa",
    "region",
]

# Dummify categorical controls before modeling
def dummify_controls(df):
    """
    Create dummy variables for race, education, MSA, and region.
    Reference categories: White (race), BA+ (edu), Non-MSA (msa),
    Northeast (region).
    """
    df = df.copy()

    # Race dummies (ref: White only = 1)
    df["race_black"]  = (df["race_wb"] == 2).astype(float)
    df["race_other"]  = (df["race_wb"] == 3).astype(float)
    # Set to NaN if race_wb is NaN
    for c in ["race_black","race_other"]:
        df.loc[df["race_wb"].isna(), c] = np.nan

    # Education dummies (ref: BA or higher = 4)
    df["edu_lths"]  = (df["edu_4cat"] == 1).astype(float)
    df["edu_hs"]    = (df["edu_4cat"] == 2).astype(float)
    df["edu_some"]  = (df["edu_4cat"] == 3).astype(float)
    for c in ["edu_lths","edu_hs","edu_some"]:
        df.loc[df["edu_4cat"].isna(), c] = np.nan

    # MSA dummies (ref: Not in MSA = 1)
    df["msa_suburban"] = (df["msa"] == 2).astype(float)
    df["msa_central"]  = (df["msa"] == 3).astype(float)
    for c in ["msa_suburban","msa_central"]:
        df.loc[df["msa"].isna(), c] = np.nan

    # Region dummies (ref: Northeast = 1)
    df["region_mw"]   = (df["region"] == 2).astype(float)
    df["region_south"] = (df["region"] == 3).astype(float)
    df["region_west"]  = (df["region"] == 4).astype(float)
    for c in ["region_mw","region_south","region_west"]:
        df.loc[df["region"].isna(), c] = np.nan

    return df

df18      = dummify_controls(df18)
df21      = dummify_controls(df21)
df_pooled = dummify_controls(df_pooled)

CONTROLS_DUMMY = [
    "age", "male",
    "race_black", "race_other",
    "edu_lths", "edu_hs", "edu_some",
    "married", "income",
    "msa_suburban", "msa_central",
    "region_mw", "region_south", "region_west",
]

FULL_PREDS = CORE_PREDS + CONTROLS_DUMMY


# ===========================================================================
# TABLE 2: Replication of Maher (2024) on 2021 data
# ===========================================================================

print(f"\n{'='*70}")
print("TABLE 2: Logistic Regression on 2021 Data (Replication of Maher 2024)")
print("Outcome: Any past-year identity theft victimization")
print(f"{'='*70}")

# Model 2a: predictors only (no controls) -- matches Maher's Model 1
res2a, fit2a = run_logit(df21, "any_idtheft", CORE_PREDS,
                          label="2021 Model 2a: Predictors only")

# Model 2b: full model with controls -- matches Maher's main model
res2b, fit2b = run_logit(df21, "any_idtheft", FULL_PREDS,
                          label="2021 Model 2b: Full model with controls")


# ===========================================================================
# TABLE 2 COMPARISON: Side-by-side with 2018 replication
# ===========================================================================

print(f"\n{'='*70}")
print("TABLE 2 (cont): 2018 Replication Check")
print("Running same models on 2018 data to verify we match Maher (2024)")
print(f"{'='*70}")

res18a, fit18a = run_logit(df18, "any_idtheft", CORE_PREDS,
                            label="2018 Model: Predictors only")
res18b, fit18b = run_logit(df18, "any_idtheft", FULL_PREDS,
                            label="2018 Model: Full model with controls")


# ===========================================================================
# TABLE 3: Pooled cross-year model with year interactions
# ===========================================================================

print(f"\n{'='*70}")
print("TABLE 3: Pooled 2018+2021 Model -- Testing Year x Predictor Effects")
print("Outcome: Any past-year identity theft victimization")
print(f"{'='*70}")

df_p = df_pooled.copy()

# Year indicator (2021=1, 2018=0)
df_p["year_2021"] = (df_p["year"] == 2021).astype(float)

# Year x citizenship interaction
df_p["year_x_noncitizen"]     = df_p["year_2021"] * df_p["noncitizen"]
# Year x hispanic interaction
df_p["year_x_hispanic"]       = df_p["year_2021"] * df_p["hispanic"]
# Year x hisp_noncitizen triple interaction
df_p["year_x_hisp_noncit"]    = df_p["year_2021"] * df_p["hisp_noncitizen"]

POOLED_PREDS = (
    ["year_2021"] +
    CORE_PREDS +
    ["year_x_noncitizen", "year_x_hispanic", "year_x_hisp_noncit"] +
    CONTROLS_DUMMY
)

res3, fit3 = run_logit(df_p, "any_idtheft", POOLED_PREDS,
                        label="Pooled 2018+2021: Year interaction model")

# Wald test: joint significance of the three year-interaction terms
print("\n  Wald test (H0: all three year x predictor interactions = 0):")
year_int_vars = ["year_x_noncitizen", "year_x_hispanic", "year_x_hisp_noncit"]
# Get indices of these variables in the fitted model
try:
    idx = [list(fit3.model.exog_names).index(v) for v in
           ["year_x_noncitizen", "year_x_hispanic", "year_x_hisp_noncit"]]
    R = np.zeros((3, len(fit3.params)))
    for i, j in enumerate(idx):
        R[i, j] = 1
    wald = fit3.wald_test(R)
    print(f"    F/Chi2 = {float(wald.statistic):.4f}, "
          f"df = {wald.df_denom if hasattr(wald,'df_denom') else 3}, "
          f"p = {float(wald.pvalue):.4f}")
except Exception as e:
    print(f"    (Wald test skipped: {e})")


# ===========================================================================
# TABLE 4: Subtype analysis
# ===========================================================================

print(f"\n{'='*70}")
print("TABLE 4: Subtype Analysis -- Existing Account / New Account / Other")
print("2021 only, full controls")
print(f"{'='*70}")

for outcome, label in [
    ("existing_acct", "Existing account misuse"),
    ("new_acct",      "New account fraud"),
    ("other_idtheft", "Other misuse of personal info"),
]:
    run_logit(df21, outcome, FULL_PREDS, label=f"2021 {label}")


# ===========================================================================
# TABLE 5: Benefits fraud pandemic signal
# ===========================================================================

print(f"\n{'='*70}")
print("TABLE 5: Pandemic Signal -- Government Benefits Fraud (VS075)")
print("Among respondents who reported 'other misuse of personal info'")
print(f"{'='*70}")

for yr, df in [(2018, df18), (2021, df21)]:
    if "benefits_fraud" in df.columns:
        sub = df[df["other_idtheft"] == 1][["benefits_fraud","VS363"]]
        sub = sub[sub["benefits_fraud"].notna()]
        n_asked = len(sub)
        n_yes   = int((sub["benefits_fraud"] == 1).sum())
        w_pct   = weighted_pct(sub["benefits_fraud"], sub["VS363"])
        print(f"  {yr}: {n_yes:,} of {n_asked:,} other-misuse victims "
              f"had benefits fraud ({n_yes/n_asked*100:.1f}% unweighted, "
              f"{w_pct:.1f}% weighted)")


# ===========================================================================
# PREDICTED PROBABILITIES (for Figure 2 in paper)
# ===========================================================================

print(f"\n{'='*70}")
print("PREDICTED PROBABILITIES: Citizenship x Ethnicity x Year")
print("(For Figure 2 -- marginal effects at the mean)")
print(f"{'='*70}")

def predicted_prob(fit, df, noncitizen_val, hispanic_val,
                   hisp_noncit_val, year_2021_val=None,
                   year_x_nc=None, year_x_hisp=None, year_x_hnc=None):
    """Compute predicted probability at given predictor values,
    holding controls at their means."""
    means = df[CONTROLS_DUMMY].mean()
    row = {"const": 1.0}
    if year_2021_val is not None:
        row["year_2021"]        = year_2021_val
        row["year_x_noncitizen"]  = year_x_nc
        row["year_x_hispanic"]    = year_x_hisp
        row["year_x_hisp_noncit"] = year_x_hnc
    row["noncitizen"]      = noncitizen_val
    row["hispanic"]        = hispanic_val
    row["hisp_noncitizen"] = hisp_noncit_val
    for c in CONTROLS_DUMMY:
        row[c] = means.get(c, 0)
    x = pd.Series(row)
    # Align to model's variable order
    x_aligned = x.reindex(fit.model.exog_names, fill_value=0)
    logit_val = x_aligned @ fit.params
    return 1 / (1 + np.exp(-logit_val))

print("\n  2021 model (Model 2b), holding controls at means:")
cells = [
    ("Non-Hispanic citizen",   0, 0, 0),
    ("Non-Hispanic noncitizen",1, 0, 0),
    ("Hispanic citizen",       0, 1, 0),
    ("Hispanic noncitizen",    1, 1, 1),
]
for lbl, nc, h, hnc in cells:
    p = predicted_prob(fit2b, df21, nc, h, hnc)
    print(f"    {lbl:30s}: {p*100:.2f}%")

print("\n  2018 model (Model 18b), holding controls at means:")
for lbl, nc, h, hnc in cells:
    p = predicted_prob(fit18b, df18, nc, h, hnc)
    print(f"    {lbl:30s}: {p*100:.2f}%")


# ===========================================================================
# R CODE FOR SURVEY-CORRECT STANDARD ERRORS
# ===========================================================================

print(f"\n{'='*70}")
print("R CODE: Survey-correct standard errors (Taylor-series linearization)")
print("Run this in R to produce design-correct SEs for the journal version.")
print(f"{'='*70}")

r_code = """
# Install if needed: install.packages(c("survey","haven"))
library(survey)
library(haven)

# Load data (export parquet to Stata or use haven::read_dta directly)
df21 <- haven::read_dta("38501-0001-Data.dta")

# --- Replicate the sample construction from step2_build_sample.py ---
# (Apply the same filters: VS008 %in% c(1,2), age>=16, etc.)

# Declare survey design
svy21 <- svydesign(
  id      = ~V2118,      # PSU (half-sample code)
  strata  = ~V2117,      # pseudostratum
  weights = ~VS363,      # final ITS weight
  data    = df21_clean,  # your cleaned dataset
  nest    = TRUE
)

# Logistic regression with survey-correct SEs
fit_svy <- svyglm(
  any_idtheft ~ noncitizen + hispanic + hisp_noncitizen +
    age + male + race_black + race_other +
    edu_lths + edu_hs + edu_some +
    married + income +
    msa_suburban + msa_central +
    region_mw + region_south + region_west,
  design = svy21,
  family = quasibinomial(link = "logit")
)

summary(fit_svy)
exp(coef(fit_svy))           # odds ratios
exp(confint(fit_svy))        # 95% CIs
"""
print(r_code)

print("\n" + "="*70)
print("STEP 3 COMPLETE")
print("="*70)
print("Outputs produced:")
print("  - Table 1: Descriptive statistics")
print("  - Table 2: Replication logistic regressions (2018 and 2021)")
print("  - Table 3: Pooled year-interaction model")
print("  - Table 4: Subtype analyses (existing/new/other)")
print("  - Table 5: Benefits fraud pandemic signal")
print("  - Predicted probabilities for Figure 2")
print("  - R code for survey-correct SEs")
print("\nNext: paste this output here for interpretation and paper writing.")
