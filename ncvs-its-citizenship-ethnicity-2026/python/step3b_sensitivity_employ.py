"""
NCVS-ITS -- Step 3b: Sensitivity Check (corrected)
====================================================

Employment sector (V3075) coding per ICPSR 38501 codebook:
  1 = Private company  (reference category)
  2 = Federal government
  3 = State/county/local government
  4 = Self-employed
  8 = Residue (-> NaN)
  9 = Out of universe (-> NaN, i.e. not employed)

We dummify sector into three indicators (federal, state_local,
self_employed) with private company as the reference, then add
these to the primary model with listwise deletion on the ~40%
who are not employed / out of universe.

Requirements: pip install pandas numpy statsmodels tabulate pyarrow
Usage: python step3b_sensitivity_employ.py
"""

import pandas as pd
import numpy as np
import statsmodels.api as sm
from tabulate import tabulate
import warnings
warnings.filterwarnings("ignore")

# ---------------------------------------------------------------------------
print("Loading analytic datasets...")
df21 = pd.read_parquet("analytic_2021.parquet")
print(f"  2021: N={len(df21):,}")

if "V3086" in df21.columns:
    before = len(df21)
    df21 = df21[~df21["V3086"].isin([3, 4])].copy()
    print(f"  Gender exclusions: dropped {before-len(df21):,}, "
          f"remaining {len(df21):,}")

# ---------------------------------------------------------------------------
# Fix employment sector coding
# ---------------------------------------------------------------------------
df21["employ_raw"] = df21["employ"].replace({8: np.nan, 9: np.nan,
                                              -1: np.nan})
# Correct labels per codebook
employ_labels = {1.0: "Private company (ref)",
                 2.0: "Federal govt",
                 3.0: "State/county/local govt",
                 4.0: "Self-employed"}

print(f"\nEmployment sector distribution (V3075), corrected labels:")
vc = df21["employ_raw"].value_counts(dropna=False).sort_index()
for val, n in vc.items():
    lbl = employ_labels.get(val, "Missing/not employed")
    print(f"  {str(val):>6}: {n:>8,}  ({n/len(df21)*100:.1f}%)  {lbl}")

# Dummify: reference = private company (code 1)
df21["emp_federal"]    = (df21["employ_raw"] == 2).astype(float)
df21["emp_state_loc"]  = (df21["employ_raw"] == 3).astype(float)
df21["emp_self"]       = (df21["employ_raw"] == 4).astype(float)
for c in ["emp_federal","emp_state_loc","emp_self"]:
    df21.loc[df21["employ_raw"].isna(), c] = np.nan

# ---------------------------------------------------------------------------
# Dummify standard controls
# ---------------------------------------------------------------------------
def dummify(df):
    df = df.copy()
    df["race_black"]   = (df["race_wb"]==2).astype(float)
    df["race_other"]   = (df["race_wb"]==3).astype(float)
    for c in ["race_black","race_other"]:
        df.loc[df["race_wb"].isna(), c] = np.nan
    df["edu_lths"] = (df["edu_4cat"]==1).astype(float)
    df["edu_hs"]   = (df["edu_4cat"]==2).astype(float)
    df["edu_some"] = (df["edu_4cat"]==3).astype(float)
    for c in ["edu_lths","edu_hs","edu_some"]:
        df.loc[df["edu_4cat"].isna(), c] = np.nan
    df["msa_suburban"] = (df["msa"]==2).astype(float)
    df["msa_central"]  = (df["msa"]==3).astype(float)
    for c in ["msa_suburban","msa_central"]:
        df.loc[df["msa"].isna(), c] = np.nan
    df["region_mw"]    = (df["region"]==2).astype(float)
    df["region_south"] = (df["region"]==3).astype(float)
    df["region_west"]  = (df["region"]==4).astype(float)
    for c in ["region_mw","region_south","region_west"]:
        df.loc[df["region"].isna(), c] = np.nan
    return df

df21 = dummify(df21)

# ---------------------------------------------------------------------------
# Predictor sets
# ---------------------------------------------------------------------------
CORE = ["noncitizen","hispanic","hisp_noncitizen"]

CONTROLS_BASE = [
    "age","male",
    "race_black","race_other",
    "edu_lths","edu_hs","edu_some",
    "married","income",
    "msa_suburban","msa_central",
    "region_mw","region_south","region_west",
]

CONTROLS_EMPLOY = CONTROLS_BASE + [
    "emp_federal","emp_state_loc","emp_self"
]

# ---------------------------------------------------------------------------
def run_logit(df, outcome, predictors, weight_col="VS363"):
    sub = df[[outcome]+predictors+[weight_col]].dropna()
    y   = sub[outcome]
    X   = sm.add_constant(sub[predictors])
    w   = sub[weight_col]
    w_n = w / w.mean()
    res = sm.GLM(y, X, family=sm.families.Binomial(),
                 freq_weights=w_n).fit()
    return res, len(sub), int(y.sum())

# ---------------------------------------------------------------------------
print("\nRunning Model A (no employment, primary model)...")
fitA, nA, vA = run_logit(df21, "any_idtheft", CORE+CONTROLS_BASE)
print(f"  N={nA:,}  victims={vA:,} ({vA/nA*100:.2f}%)")

print("Running Model B (with employment dummies, listwise deletion)...")
fitB, nB, vB = run_logit(df21, "any_idtheft", CORE+CONTROLS_EMPLOY)
print(f"  N={nB:,}  victims={vB:,} ({vB/nB*100:.2f}%)")
print(f"  Listwise deletion dropped: {nA-nB:,} ({(nA-nB)/nA*100:.1f}%)")

# ---------------------------------------------------------------------------
print(f"\n{'='*68}")
print("SIDE-BY-SIDE: Primary coefficients, Model A vs. Model B")
print(f"{'='*68}")

rows = []
sig_cat = lambda p: 0 if p>=0.1 else (1 if p>=0.05 else
                    (2 if p>=0.01 else (3 if p>=0.001 else 4)))
sig_str = lambda p: ("***" if p<0.001 else ("**" if p<0.01 else
                     ("*" if p<0.05 else ("." if p<0.1 else "ns"))))

for var in CORE:
    orA    = np.exp(fitA.params[var])
    pA     = fitA.pvalues[var]
    ciA_lo = np.exp(fitA.params[var] - 1.96*fitA.bse[var])
    ciA_hi = np.exp(fitA.params[var] + 1.96*fitA.bse[var])

    orB    = np.exp(fitB.params[var])
    pB     = fitB.pvalues[var]
    ciB_lo = np.exp(fitB.params[var] - 1.96*fitB.bse[var])
    ciB_hi = np.exp(fitB.params[var] + 1.96*fitB.bse[var])

    # Assess change
    dir_A = orA >= 1; dir_B = orB >= 1
    both_ns = pA >= 0.05 and pB >= 0.05

    if dir_A != dir_B and not both_ns:
        assess = "DIRECTION CHANGED (sig)"
    elif dir_A != dir_B and both_ns:
        assess = "direction flip (both ns -- noise)"
    elif sig_cat(pA) != sig_cat(pB):
        assess = "sig level shifted"
    else:
        assess = "stable"

    rows.append([var,
                 f"{orA:.3f}", f"[{ciA_lo:.3f},{ciA_hi:.3f}]", sig_str(pA),
                 f"{orB:.3f}", f"[{ciB_lo:.3f},{ciB_hi:.3f}]", sig_str(pB),
                 assess])

print(tabulate(rows,
    headers=["Variable",
             "OR-A","95% CI-A","Sig-A",
             "OR-B","95% CI-B","Sig-B",
             "Assessment"],
    tablefmt="simple"))

# Employment coefficients for context
print(f"\nEmployment sector coefficients in Model B")
print(f"(ref = private company employees):")
for var in ["emp_federal","emp_state_loc","emp_self"]:
    OR  = np.exp(fitB.params[var])
    p   = fitB.pvalues[var]
    clo = np.exp(fitB.params[var] - 1.96*fitB.bse[var])
    chi = np.exp(fitB.params[var] + 1.96*fitB.bse[var])
    print(f"  {var:15s}: OR={OR:.3f} [{clo:.3f},{chi:.3f}] {sig_str(p)}")

# ---------------------------------------------------------------------------
print(f"\n{'='*68}")
print("VERDICT AND RECOMMENDED METHODS LANGUAGE")
print(f"{'='*68}")

direction_flips_sig = any("DIRECTION CHANGED (sig)" in r[7] for r in rows)
sig_shifts          = any("sig level shifted" in r[7] for r in rows)
noise_flips         = any("noise" in r[7] for r in rows)

if not direction_flips_sig and not sig_shifts:
    print("""
  FINDING: Primary coefficients are stable when employment sector
  is added with listwise deletion, despite a 39% reduction in
  sample size. Direction changes in non-significant coefficients
  reflect sampling noise, not substantive reversal.

  RECOMMENDED METHODS STATEMENT (replace placeholder):

  "Employment sector (V3075; private company, federal government,
  state/local government, self-employed) is excluded from primary
  models because approximately 40% of respondents are not in the
  labor force or did not respond to this item, which is asked only
  of currently employed respondents. A sensitivity analysis
  including employment sector dummies with listwise deletion
  (n = [INSERT N FROM MODEL B], approximately 39% smaller than
  the primary sample) produces substantively identical estimates
  for the three primary predictors; results are available from
  the authors."
""")
else:
    print("""
  WARNING: One or more significant coefficients changed direction
  or significance category. Review results carefully before
  finalizing the Methods statement. Do NOT claim stability.
""")
    for r in rows:
        if r[7] not in ("stable","direction flip (both ns -- noise)"):
            print(f"  -> {r[0]}: {r[7]}")
