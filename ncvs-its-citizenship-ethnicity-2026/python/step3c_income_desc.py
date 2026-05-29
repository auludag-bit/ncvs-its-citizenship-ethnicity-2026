"""
Step 3c (corrected): Income distribution for Table I
=====================================================

CRITICAL FIX: In step2_build_sample.py, V2026 was recoded with the
generic missing-code replacer {8: nan, 9: nan}, which wiped out
valid income bands:
  Code 8 = $20,000-$24,999  (VALID)
  Code 9 = $25,000-$29,999  (VALID)
  Code 99 = residue/missing  (only code that should be NaN)

This script reads DIRECTLY from the raw .dta files to get correct
income values, then merges with the parquet analytic sample IDs
to restrict to the correct analytic sample.

Usage: python step3c_income_desc.py
Requires: analytic_2018.parquet, analytic_2021.parquet,
          37923-0001-Data.dta, 38501-0001-Data.dta
"""

import pandas as pd
import numpy as np
import pyreadstat

# ---------------------------------------------------------------------------
# Load analytic samples (for the correct row set)
# ---------------------------------------------------------------------------
print("Loading analytic samples...")
df18 = pd.read_parquet("analytic_2018.parquet")
df21 = pd.read_parquet("analytic_2021.parquet")

# Apply gender exclusions (same as step 3)
if "V3086" in df18.columns:
    df18 = df18[~df18["V3086"].isin([3, 4])].copy()
if "V3086" in df21.columns:
    df21 = df21[~df21["V3086"].isin([3, 4])].copy()

print(f"  2018 analytic N: {len(df18):,}")
print(f"  2021 analytic N: {len(df21):,}")

# ---------------------------------------------------------------------------
# Load raw V2026 directly from .dta (bypassing the bad step2 recode)
# ---------------------------------------------------------------------------
print("\nLoading raw income from .dta files...")

raw18, _ = pyreadstat.read_dta("37923-0001-Data.dta",
                                usecols=["V2026", "VS363"])
raw21, _ = pyreadstat.read_dta("38501-0001-Data.dta",
                                usecols=["V2026", "VS363"])

# Correct missing codes for V2026: only 98 and 99 are missing
# (codes 1-18 are valid income bands; code 14 intentionally skipped
#  since 2017 per codebook note)
def recode_income_correct(x):
    if pd.isna(x) or x in [98, 99]: return np.nan
    elif x <= 7:  return 1   # < $20,000
    elif x <= 9:  return 2   # $20,000-$29,999 (codes 8 and 9 ARE valid)
    elif x <= 12: return 3   # $30,000-$49,999
    elif x == 13: return 4   # $50,000-$74,999
    elif x >= 15: return 5   # >= $75,000
    else:         return np.nan

raw18["inc_band"] = raw18["V2026"].apply(recode_income_correct)
raw21["inc_band"] = raw21["V2026"].apply(recode_income_correct)

# ---------------------------------------------------------------------------
# We need to merge the corrected income back onto the analytic samples.
# The parquet files don't have a unique row ID, but both were loaded
# from the same .dta in the same order. We can reconstruct this by
# identifying which rows in the raw .dta were kept in the analytic sample
# using the weight variable VS363 as a join key -- but weights are not
# unique. Instead, use the index: the parquet preserves original row order
# within the ITS-completer subset. We need a reliable join key.
#
# Safest approach: re-filter the raw .dta to the analytic sample using
# the same VS363 weight + V3083 citizenship + V3024 hispanic values
# as a composite key, then compute the income distribution directly
# from the raw file restricted to ITS completers with correct weights.
# ---------------------------------------------------------------------------

# Load VS008 (ITS completion flag) from raw files
raw18_full, _ = pyreadstat.read_dta("37923-0001-Data.dta",
                                     usecols=["VS008","V3083","V3024",
                                              "V3086","V3014","V2026",
                                              "VS363"])
raw21_full, _ = pyreadstat.read_dta("38501-0001-Data.dta",
                                     usecols=["VS008","V3083","V3024",
                                              "V3086","V3014","V2026",
                                              "VS363"])

def build_analytic(df_raw):
    df = df_raw.copy()
    # ITS completers
    df = df[df["VS008"].isin([1, 2])].copy()
    # Age 16+
    df = df[df["V3014"] >= 16].copy()
    # Drop missing citizenship
    df = df[~df["V3083"].isin([8, 9, -1])].copy()
    # Drop missing Hispanic
    df = df[~df["V3024"].isin([8, -1])].copy()
    # Gender exclusions
    if "V3086" in df.columns:
        df = df[~df["V3086"].isin([3, 4])].copy()
    # Correct income recode
    df["inc_band"] = df["V2026"].apply(recode_income_correct)
    return df

print("Rebuilding analytic samples with correct income...")
a18 = build_analytic(raw18_full)
a21 = build_analytic(raw21_full)
print(f"  2018: {len(a18):,}  (expect ~102,015)")
print(f"  2021: {len(a21):,}  (expect ~92,867)")

# ---------------------------------------------------------------------------
# Weighted income distribution
# ---------------------------------------------------------------------------
labels = {1: "< $20,000",
          2: "$20,000--$29,999",
          3: "$30,000--$49,999",
          4: "$50,000--$74,999",
          5: ">= \\$75,000"}

print(f"\n{'='*55}")
print("Weighted income distribution (%) for Table I")
print(f"{'Band':28s}  {'2018':>8}  {'2021':>8}")
print("-" * 50)

results = {}
for band in [1,2,3,4,5]:
    row = []
    for yr, df in [(2018, a18), (2021, a21)]:
        mask = df["inc_band"].notna() & df["VS363"].notna()
        sub  = df[mask]
        w    = sub["VS363"]
        pct  = np.average((sub["inc_band"]==band).astype(float),
                          weights=w) * 100
        row.append(pct)
    results[band] = row
    print(f"  {labels[band]:26s}  {row[0]:>7.1f}%  {row[1]:>7.1f}%")

print()
for yr, df in [(2018, a18), (2021, a21)]:
    miss = df["inc_band"].isna().sum()
    pct  = miss / len(df) * 100
    print(f"  {yr} missing income: {miss:,} ({pct:.1f}%)")

print(f"\n{'='*55}")
print("COPY THESE VALUES INTO TABLE I:")
print(f"{'='*55}")
for band, row in results.items():
    lbl = labels[band].replace("\\$", "$")
    print(f"  \\quad {lbl} (\\%) & {row[0]:.1f} & {row[1]:.1f} \\\\")
