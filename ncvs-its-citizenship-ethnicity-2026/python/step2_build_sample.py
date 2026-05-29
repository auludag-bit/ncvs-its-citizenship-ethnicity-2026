"""
NCVS-ITS Replication/Extension -- Step 2: Build Analytic Sample (v2)
=====================================================================

CRITICAL FIX from step 1 output:
  The original ITS-completer filter ("at least one outcome != 9") was
  too restrictive. It captured only ~21,000 respondents in 2021 instead
  of the ~93,300 who completed the ITS, because the 2021 dual-reference-
  period design routes lifetime non-victims directly past the past-year
  screeners (coding them 9 = out of scope on VS012 etc.).

  The correct filter is VS008 == 1 or 2 (ITS interview completed,
  personal or telephone). VS008 == 5 = noninterview (exclude).

  Additionally, for ITS completers routed past a past-year screener
  (VS012 etc. = 9) because they said "never" to the lifetime version,
  the correct recode is 0 (not a victim), NOT missing. These are
  legitimately not victims of that type.

Requirements:
    pip install pandas pyreadstat numpy pyarrow

Usage:
    1. Edit PATH_2018 and PATH_2021 below.
    2. pip install pandas pyreadstat numpy pyarrow
    3. python step2_build_sample.py
"""

import pandas as pd
import numpy as np
import pyreadstat

# ---------------------------------------------------------------------------
# EDIT THESE PATHS
# ---------------------------------------------------------------------------
PATH_2018 = "37923-0001-Data.dta"
PATH_2021 = "38501-0001-Data.dta"


# ===========================================================================
# HELPERS
# ===========================================================================

def load(path, year):
    print(f"\n{'='*65}\nLoading {year}: {path}\n{'='*65}")
    df, meta = pyreadstat.read_dta(path)
    print(f"  Raw rows: {df.shape[0]:,}  cols: {df.shape[1]:,}")
    return df


# ===========================================================================
# STEP A: Restrict to ITS completers using VS008
# ===========================================================================

def restrict_to_its_completers(df, year):
    """
    VS008 - ITS TYPE OF INTERVIEW
      1 = ITS Interview - Personal   (completed)
      2 = ITS Interview - Telephone  (completed)
      5 = ITS Noninterview           (exclude)

    Keep only VS008 in {1, 2}.
    """
    # Print VS008 distribution first
    print(f"\n[{year}] VS008 (ITS interview type) distribution:")
    vc = df["VS008"].value_counts(dropna=False).sort_index()
    labels = {1: "Personal interview", 2: "Telephone interview",
              5: "Noninterview", np.nan: "Missing"}
    for val, n in vc.items():
        lbl = labels.get(val, str(val))
        print(f"   {str(val):>6}: {n:,}  ({lbl})")

    before = len(df)
    df_its = df[df["VS008"].isin([1, 2])].copy()
    print(f"\n[{year}] ITS completers (VS008=1 or 2): "
          f"{len(df_its):,} of {before:,} "
          f"({len(df_its)/before*100:.1f}%)")
    return df_its


# ===========================================================================
# STEP B: Age restriction (16+)
# ===========================================================================

def restrict_age(df, year):
    before = len(df)
    df = df[df["V3014"].notna()].copy()
    df = df[~df["V3014"].isin([-1])].copy()
    df = df[df["V3014"] >= 16].copy()
    print(f"[{year}] After age 16+ restriction: {len(df):,} "
          f"(dropped {before - len(df):,})")
    return df


# ===========================================================================
# STEP C: Citizenship and Hispanic recodes
# ===========================================================================

def recode_citizenship(df, year):
    """
    V3083: 1-3=US-born citizen, 4=naturalized, 5=noncitizen
           8=residue, 9=blank -> NaN
    """
    df = df.copy()
    raw = df["V3083"].replace({8: np.nan, 9: np.nan, -1: np.nan})
    before = len(df)
    df = df[raw.notna()].copy()
    raw = df["V3083"].replace({8: np.nan, 9: np.nan, -1: np.nan})
    print(f"[{year}] After dropping missing citizenship: "
          f"{len(df):,} (dropped {before - len(df):,})")

    df["citizen_2cat"] = raw.map({1:1, 2:1, 3:1, 4:1, 5:0})
    df["citizen_3cat"] = raw.map({1:1, 2:1, 3:1, 4:2, 5:3})
    df["noncitizen"]   = (df["citizen_2cat"] == 0).astype(int)

    print(f"[{year}] citizenship (2-cat): "
          f"citizen={int((df['citizen_2cat']==1).sum()):,}, "
          f"noncitizen={int((df['citizen_2cat']==0).sum()):,}")
    return df


def recode_hispanic(df, year):
    """V3024: 1=Yes, 2=No, 8=residue -> NaN"""
    df = df.copy()
    before = len(df)
    df = df[~df["V3024"].isin([8, -1])].copy()
    print(f"[{year}] After dropping missing hispanic: "
          f"{len(df):,} (dropped {before - len(df):,})")
    df["hispanic"] = df["V3024"].map({1: 1, 2: 0})
    print(f"[{year}] hispanic: "
          f"Yes={int((df['hispanic']==1).sum()):,}, "
          f"No={int((df['hispanic']==0).sum()):,}")
    return df


# ===========================================================================
# STEP D: Build the outcome variable (FIXED)
# ===========================================================================

OUTCOME_COMPONENTS = ["VS012", "VS017", "VS019", "VS041", "VS063"]

def build_outcome(df, year):
    """
    For ITS completers (VS008 in {1,2}):

    Component recode:
      1 -> 1  (Yes, victim of this type in past 12 months)
      2 -> 0  (No, not a victim of this type in past 12 months)
      9 -> 0  (Out of scope: routed past because said "never" to lifetime
               screener -- correctly treated as not a victim of this type)
      8, 98, 99 -> NaN (residue/refused/DK -- genuinely unknown)

    any_idtheft = max across components:
      1 if any component is 1 (victim)
      0 if all components are 0/never (not a victim)
      NaN if any are genuinely unknown AND none are 1
    """
    df = df.copy()

    # The key fix: 9 (out of scope among completers) = not a victim = 0
    comp_recode = {1: 1, 2: 0, 9: 0, 8: np.nan, 98: np.nan, 99: np.nan,
                   -1: np.nan}

    for var in OUTCOME_COMPONENTS:
        df[f"_{var}"] = df[var].map(comp_recode)

    comp_cols = [f"_{v}" for v in OUTCOME_COMPONENTS]
    df["any_idtheft"]   = df[comp_cols].max(axis=1)
    df["existing_acct"] = df[["_VS012", "_VS017", "_VS019"]].max(axis=1)
    df["new_acct"]      = df["_VS041"]
    df["other_idtheft"] = df["_VS063"]

    n_valid = df["any_idtheft"].notna().sum()
    n_victim = (df["any_idtheft"] == 1).sum()
    prevalence = n_victim / n_valid * 100

    print(f"\n[{year}] Outcome (any_idtheft):")
    print(f"  1 (victim):     {int(n_victim):,}")
    print(f"  0 (non-victim): {int((df['any_idtheft']==0).sum()):,}")
    print(f"  NaN (missing):  {int(df['any_idtheft'].isna().sum()):,}")
    print(f"  Prevalence:     {prevalence:.2f}%  "
          f"(compare to BJS report: ~9% for both years)")

    print(f"\n[{year}] Subtype breakdown among victims "
          f"(n={int(n_victim):,}):")
    v = df[df["any_idtheft"] == 1]
    for col, lbl in [("existing_acct","Existing account"),
                     ("new_acct","New account"),
                     ("other_idtheft","Other misuse")]:
        n = int((v[col]==1).sum())
        pct = n / int(n_victim) * 100
        print(f"    {lbl:20s}: {n:,} ({pct:.1f}% of victims)")
    return df


# ===========================================================================
# STEP E: Interaction term
# ===========================================================================

def build_interaction(df):
    df = df.copy()
    df["hisp_noncitizen"] = (
        (df["hispanic"] == 1) & (df["noncitizen"] == 1)
    ).astype(float)
    df.loc[df["hispanic"].isna() | df["noncitizen"].isna(),
           "hisp_noncitizen"] = np.nan
    return df


# ===========================================================================
# STEP F: Control variable recodes
# ===========================================================================

def recode_controls(df, year):
    df = df.copy()

    # Age: continuous
    df["age"] = df["V3014"].replace({-1: np.nan})
    df["age_grp"] = pd.cut(df["age"],
        bins=[15,24,34,49,64,120],
        labels=["16-24","25-34","35-49","50-64","65+"])

    # Sex: male dummy
    df["male"] = df["V3018"].map({1: 1, 2: 0})

    # Race: 1=White only, 2=Black only, 3=Other/multi
    df["race_wb"] = df["V3023A"].apply(
        lambda x: 1 if x==1 else (2 if x==2 else (
                  3 if (pd.notna(x) and 3<=x<=20) else np.nan)))

    # Marital: married dummy + 5-category
    df["marital"] = df["V3015"].replace({8: np.nan, -1: np.nan})
    df["married"] = (df["marital"]==1).astype(float)
    df.loc[df["marital"].isna(), "married"] = np.nan

    # Education: collapse to 4 levels
    def edu_collapse(x):
        if pd.isna(x) or x == 98: return np.nan
        elif x <= 11:              return 1   # Less than HS
        elif x in [27, 28]:        return 2   # HS diploma / GED
        elif x in [40, 41]:        return 3   # Some college / AA
        elif x >= 42:              return 4   # BA or higher
        else:                      return np.nan
    df["edu_4cat"] = df["V3020"].apply(edu_collapse)

    # Household income
    df["income"] = df["V2026"].replace({8: np.nan, 9: np.nan, -1: np.nan})

    # Employment sector
    df["employ"] = df["V3075"].replace({8: np.nan, 9: np.nan, -1: np.nan})

    # MSA, region
    df["msa"]    = df["V2129"].replace({8: np.nan, 9: np.nan, -1: np.nan})
    df["region"] = df["V2127B"].replace({8: np.nan, 9: np.nan, -1: np.nan})

    print(f"\n[{year}] Control non-missing counts:")
    for col in ["age","male","race_wb","edu_4cat","married",
                "income","employ","msa","region"]:
        n   = df[col].notna().sum()
        pct = n / len(df) * 100
        print(f"  {col:15s}: {n:,} ({pct:.1f}%)")
    return df


# ===========================================================================
# STEP G: Pandemic variables
# ===========================================================================

def build_pandemic_vars(df, year):
    df = df.copy()
    if "VS075" in df.columns:
        df["benefits_fraud"] = df["VS075"].map({1:1, 2:0, 9:np.nan,
                                                99:np.nan, -1:np.nan})
        n   = df["benefits_fraud"].notna().sum()
        yes = (df["benefits_fraud"]==1).sum()
        pct = yes/n*100 if n > 0 else 0
        print(f"\n[{year}] benefits_fraud (VS075): "
              f"{yes:,} Yes out of {n:,} valid ({pct:.1f}%)")
    if "VS017D" in df.columns:
        df["email_social_misuse"] = df["VS017D"].map(
            {1:1, 2:0, 9:0, 8:np.nan, -1:np.nan})
    return df


# ===========================================================================
# STEP H: Gender-identity check + final exclusions
# ===========================================================================

def apply_final_exclusions(df, year):
    # Check for transgender/gender-other variable near V3084
    gender_adj = [c for c in df.columns
                  if c.upper().startswith("V308") or
                     c.upper().startswith("V309")]
    print(f"\n[{year}] Variables adjacent to V3084 (gender check): "
          f"{gender_adj[:10]}")

    # If V3085 (or similar) carries gender identity, exclude here.
    # The binary V3018 does not carry transgender codes in the PUF;
    # document this as a comparability limitation with Maher (2024).

    before = len(df)
    core = ["any_idtheft","noncitizen","hispanic","age","male"]
    df = df.dropna(subset=core).copy()
    print(f"[{year}] After dropping missing on core vars: "
          f"{len(df):,} (dropped {before-len(df):,})")
    return df


# ===========================================================================
# STEP I: Sample summary
# ===========================================================================

def sample_summary(df, year):
    print(f"\n{'='*65}")
    print(f"FINAL ANALYTIC SAMPLE: {year}")
    print(f"{'='*65}")
    print(f"  N = {len(df):,}")
    w = df["VS363"]
    print(f"  Weighted N = {w.sum():,.0f}")
    print(f"  (US pop 16+ represented; expect ~258M-263M)")

    print(f"\n  Outcome:")
    pct = df["any_idtheft"].mean()*100
    print(f"    Victims: {int(df['any_idtheft'].sum()):,} ({pct:.2f}%)")
    print(f"    (BJS report target: ~9% for both years)")

    print(f"\n  Primary predictors:")
    for col, lbl in [("noncitizen","Noncitizen"),
                     ("hispanic","Hispanic"),
                     ("hisp_noncitizen","Hisp x Noncitizen")]:
        n   = int(df[col].sum())
        pct = df[col].mean()*100
        print(f"    {lbl:22s}: {n:,} ({pct:.1f}%)")

    print(f"\n  Subtypes (among all respondents):")
    for col, lbl in [("existing_acct","Existing account"),
                     ("new_acct","New account"),
                     ("other_idtheft","Other misuse")]:
        pct = df[col].mean()*100
        print(f"    {lbl:22s}: {pct:.2f}%")
    return df


# ===========================================================================
# MAIN
# ===========================================================================

def process_wave(path, year):
    df = load(path, year)
    df = restrict_to_its_completers(df, year)
    df = restrict_age(df, year)
    df = recode_citizenship(df, year)
    df = recode_hispanic(df, year)
    df = build_outcome(df, year)
    df = build_interaction(df)
    df = recode_controls(df, year)
    df = build_pandemic_vars(df, year)
    df = apply_final_exclusions(df, year)
    df["year"] = year
    sample_summary(df, year)
    return df


def main():
    df18 = process_wave(PATH_2018, 2018)
    df21 = process_wave(PATH_2021, 2021)

    df_pooled = pd.concat([df18, df21], ignore_index=True)
    print(f"\n{'='*65}")
    print(f"POOLED SAMPLE (2018 + 2021)")
    print(f"{'='*65}")
    print(f"  Total N = {len(df_pooled):,}")
    print(f"  2018:    {(df_pooled['year']==2018).sum():,}")
    print(f"  2021:    {(df_pooled['year']==2021).sum():,}")

    df18.to_parquet("analytic_2018.parquet", index=False)
    df21.to_parquet("analytic_2021.parquet", index=False)
    df_pooled.to_parquet("analytic_pooled.parquet", index=False)
    print(f"\n  Saved: analytic_2018.parquet")
    print(f"  Saved: analytic_2021.parquet")
    print(f"  Saved: analytic_pooled.parquet")
    print(f"\nNext step: run step3_regression.py")


if __name__ == "__main__":
    main()
