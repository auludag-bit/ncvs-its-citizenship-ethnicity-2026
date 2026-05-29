"""
NCVS-ITS Replication/Extension -- Step 1: Load and Inspect
==========================================================

Replication and extension of Maher (2024) using the 2018 (ICPSR 37923)
and 2021 (ICPSR 38501) NCVS Identity Theft Supplements.

THIS SCRIPT: loads both Stata .dta files, confirms the key variables are
present, and prints raw frequency tables so we can verify coding against
the codebook BEFORE constructing any analytic variables.

Run this on the machine where the .dta files live. Nothing here computes
results for the paper yet -- it is a verification/inspection pass.

Requirements:
    pip install pandas pyreadstat

Usage:
    1. Edit the two PATH variables below to point to your .dta files.
    2. Run:  python step1_load_inspect.py
    3. Read the printed output and compare against variable_map.md.
"""

import pandas as pd
import pyreadstat

# ---------------------------------------------------------------------------
# EDIT THESE TWO PATHS to point to your local .dta files
# ---------------------------------------------------------------------------
PATH_2018 = "37923-0001-Data.dta"   # ICPSR 37923 (2018 ITS)
PATH_2021 = "38501-0001-Data.dta"   # ICPSR 38501 (2021 ITS)

# ---------------------------------------------------------------------------
# Variables we expect to use (from variable_map.md, verified vs. codebook)
# ---------------------------------------------------------------------------
KEY_VARS = {
    "design": {
        "weight": "VS363",        # final ITS person weight
        "strata": "V2117",        # pseudostratum
        "psu":    "V2118",        # half-sample / SECU
    },
    "predictors": {
        "citizenship": "V3083",   # 1-4 citizen, 5 noncitizen
        "hispanic":    "V3024",   # 1 yes, 2 no
    },
    "outcome_components_pastyr": {
        "existing_bank":  "VS012",  # existing checking/savings/debit/ATM past 12mo
        "existing_cc":    "VS017",  # credit card past 12mo
        "existing_other": "VS019",  # other existing accounts past 12mo
        "new_account":    "VS041",  # new accounts fraudulently created past 12mo
        "other_misuse":   "VS063",  # personal info other fraudulent purposes past 12mo
    },
    "pandemic_2021_only": {
        "email_social_ever":   "VS017D",  # NEW in 2021
        "new_email":           "VS045A",  # NEW in 2021
        "new_social":          "VS045B",  # NEW in 2021
        "govt_benefits_fraud": "VS075",   # pandemic-benefit fraud proxy
        "tax_fraud":           "VS065",   # tax-refund fraud
    },
    "controls": {
        "age":         "V3014",   # allocated age (or V3013 original)
        "sex":         "V3018",   # allocated sex (or V3017 original)
        "marital":     "V3015",   # marital status current
        "education":   "V3020",   # educational attainment
        "race":        "V3023A",  # race recode
        "income":      "V2026",   # household income
        "employ_sect": "V3075",   # private/govt/self
        "msa":         "V2129",   # MSA status
        "region":      "V2127B",  # census region
        "place_size":  "V2126B",  # place size
    },
}


def flatten(d):
    """Flatten the nested KEY_VARS dict to {label: varname}."""
    out = {}
    for group in d.values():
        out.update(group)
    return out


def load_wave(path, year):
    print(f"\n{'='*70}\nLoading {year} from: {path}\n{'='*70}")
    df, meta = pyreadstat.read_dta(path)
    print(f"  rows: {df.shape[0]:,}   columns: {df.shape[1]:,}")
    return df, meta


def check_presence(df, year):
    """Report which expected variables are present/absent in this wave."""
    print(f"\n--- Variable presence check, {year} ---")
    allvars = flatten(KEY_VARS)
    cols_upper = {c.upper(): c for c in df.columns}  # case-insensitive lookup
    present, absent = {}, []
    for label, var in allvars.items():
        if var.upper() in cols_upper:
            present[label] = cols_upper[var.upper()]
        else:
            absent.append((label, var))
    for label, var in present.items():
        print(f"   [ok]  {label:22s} -> {var}")
    if absent:
        print(f"\n   MISSING in {year} (expected for 2021-only vars in the 2018 file):")
        for label, var in absent:
            print(f"   [--]  {label:22s} -> {var}")
    return present


def freqs(df, present, year):
    """Print raw value counts (including missing codes) for key categoricals."""
    print(f"\n--- Raw frequency tables, {year} (compare to codebook) ---")
    interest = ["citizenship", "hispanic", "existing_bank", "existing_cc",
                "existing_other", "new_account", "other_misuse",
                "email_social_ever", "govt_benefits_fraud",
                "sex", "race", "marital", "education", "msa", "region"]
    for label in interest:
        if label in present:
            var = present[label]
            print(f"\n   {label} ({var}):")
            vc = df[var].value_counts(dropna=False).sort_index()
            for val, n in vc.items():
                print(f"      {str(val):>8} : {n:,}")


def weight_summary(df, present, year):
    """Sanity-check the survey weight and design variables."""
    print(f"\n--- Design variable summary, {year} ---")
    if "weight" in present:
        w = df[present["weight"]]
        print(f"   weight {present['weight']}: "
              f"n_nonnull={w.notna().sum():,}, "
              f"min={w.min():.2f}, max={w.max():.2f}, "
              f"mean={w.mean():.2f}, sum={w.sum():,.0f}")
        print(f"      (sum of weights approximates the US population 16+ "
              f"represented by ITS interviews)")
    for key in ("strata", "psu"):
        if key in present:
            v = df[present[key]]
            print(f"   {key} {present[key]}: "
                  f"n_unique={v.nunique()}, n_nonnull={v.notna().sum():,}")


def main():
    df18, meta18 = load_wave(PATH_2018, 2018)
    p18 = check_presence(df18, 2018)
    freqs(df18, p18, 2018)
    weight_summary(df18, p18, 2018)

    df21, meta21 = load_wave(PATH_2021, 2021)
    p21 = check_presence(df21, 2021)
    freqs(df21, p21, 2021)
    weight_summary(df21, p21, 2021)

    print(f"\n{'='*70}\nDONE. Next step: confirm every frequency table above matches\nthe codebook, then we build the analytic variables in step 2.\n{'='*70}")


if __name__ == "__main__":
    main()
