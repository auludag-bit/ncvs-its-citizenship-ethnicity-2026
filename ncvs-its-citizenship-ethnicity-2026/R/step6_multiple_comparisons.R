# ==========================================================================
# Step 6: Multiple Comparisons and Cell Counts (Response to Reviewer 9)
# ==========================================================================
# Addresses three specific requests:
#   1. Holm- and BH-adjusted p-values for the 9 interaction terms
#      in Table IV (3 subtypes x 3 predictors)
#   2. Actual cell count for Hispanic x Noncitizen x new-account victims
#   3. Bootstrap CIs for the OR=3.33 estimate (more reliable than Wald
#      at small cell counts with large odds ratios)
#
# Note: Since Rec. 6, the primary models use svyglm. The new-account
# Hisp x Noncit OR is now 3.33, p=.122 (not 2.66, p=.045), so the
# multiple-comparisons concern is already partly addressed. We report
# corrections for completeness and transparency.
# ==========================================================================

options(survey.lonely.psu = "adjust")
library(haven)
library(survey)
library(dplyr)

cat("Loading and building analytic sample...\n")
df21_raw <- read_dta("38501-0001-Data.dta")

build_analytic <- function(df) {
  df %>%
    filter(VS008 %in% c(1, 2)) %>%
    filter(V3014 >= 16) %>%
    filter(!(V3083 %in% c(8, 9, -1))) %>%
    filter(!(V3024 %in% c(8, -1))) %>%
    filter(!(V3086 %in% c(3, 4))) %>%
    mutate(
      noncitizen      = as.integer(V3083 == 5),
      hispanic        = as.integer(V3024 == 1),
      hisp_noncitizen = as.integer(V3083 == 5 & V3024 == 1),
      c012 = case_when(VS012==1~1, VS012%in%c(2,9)~0, TRUE~NA_real_),
      c017 = case_when(VS017==1~1, VS017%in%c(2,9)~0, TRUE~NA_real_),
      c019 = case_when(VS019==1~1, VS019%in%c(2,9)~0, TRUE~NA_real_),
      c041 = case_when(VS041==1~1, VS041%in%c(2,9)~0, TRUE~NA_real_),
      c063 = case_when(VS063==1~1, VS063%in%c(2,9)~0, TRUE~NA_real_)
    ) %>%
    mutate(
      existing_acct = pmax(c012, c017, c019, na.rm=FALSE),
      new_acct      = c041,
      other_idtheft = c063,
      age          = ifelse(V3014==-1, NA_real_, as.numeric(V3014)),
      male         = as.integer(V3018==1),
      race_black   = as.integer(V3023A==2),
      race_other   = as.integer(V3023A>=3 & V3023A<=20),
      edu_lths     = as.integer(V3020<=11),
      edu_hs       = as.integer(V3020%in%c(27,28)),
      edu_some     = as.integer(V3020%in%c(40,41)),
      married      = as.integer(V3015==1),
      income       = case_when(V2026%in%c(98,99)~NA_real_,
                               TRUE~as.numeric(V2026)),
      msa_suburban = as.integer(V2129==2),
      msa_central  = as.integer(V2129==3),
      region_mw    = as.integer(V2127B==2),
      region_south = as.integer(V2127B==3),
      region_west  = as.integer(V2127B==4)
    ) %>%
    filter(!is.na(new_acct), !is.na(noncitizen),
           !is.na(hispanic), !is.na(age), !is.na(male))
}

df21 <- build_analytic(df21_raw)
svy21 <- svydesign(id=~V2118, strata=~V2117, weights=~VS363,
                   data=df21, nest=TRUE)

cat(sprintf("2021 analytic N: %s\n", format(nrow(df21), big.mark=",")))

CTRL <- paste("age + male + race_black + race_other +",
              "edu_lths + edu_hs + edu_some +",
              "married + income +",
              "msa_suburban + msa_central +",
              "region_mw + region_south + region_west")

SEP <- paste(rep("=",65), collapse="")

# --------------------------------------------------------------------------
# Part 1: Actual cell counts for Hispanic x Noncitizen x each subtype
# --------------------------------------------------------------------------
cat("\n", SEP, "\n")
cat("PART 1: HISPANIC x NONCITIZEN CELL COUNTS BY SUBTYPE\n")
cat(SEP, "\n\n")

cross <- df21 %>%
  mutate(group = case_when(
    hispanic==1 & noncitizen==1 ~ "Hisp x Noncit",
    hispanic==1 & noncitizen==0 ~ "Hisp x Citizen",
    hispanic==0 & noncitizen==1 ~ "NonHisp x Noncit",
    TRUE                        ~ "NonHisp x Citizen"
  ))

for (outcome in c("existing_acct","new_acct","other_idtheft")) {
  cat(sprintf("\n%s:\n", outcome))
  tbl <- cross %>%
    filter(!is.na(.data[[outcome]])) %>%
    group_by(group) %>%
    summarise(
      n_total   = n(),
      n_victims = sum(.data[[outcome]]==1, na.rm=TRUE),
      pct       = round(n_victims/n_total*100, 1),
      .groups   = "drop"
    )
  print(tbl, n=10)
}

# --------------------------------------------------------------------------
# Part 2: Nine p-values (3 subtypes x 3 predictors) with corrections
# --------------------------------------------------------------------------
cat("\n", SEP, "\n")
cat("PART 2: RAW AND ADJUSTED p-VALUES FOR TABLE IV (9 TESTS)\n")
cat(SEP, "\n")

KEY <- c("noncitizen","hispanic","hisp_noncitizen")
outcomes <- c("existing_acct","new_acct","other_idtheft")
labels   <- c("Existing account","New account","Other misuse")

raw_p  <- numeric(9)
raw_or <- numeric(9)
vars   <- character(9)
outs   <- character(9)

idx <- 1
for (i in seq_along(outcomes)) {
  outcome <- outcomes[i]
  f <- as.formula(paste(outcome,"~ noncitizen + hispanic +
                         hisp_noncitizen +", CTRL))
  fit <- svyglm(f, design=svy21, family=quasibinomial("logit"))
  co <- coef(fit)[KEY]
  se <- sqrt(diag(vcov(fit)))[KEY]
  pv <- 2*pnorm(-abs(co/se))
  for (k in seq_along(KEY)) {
    raw_p[idx]  <- pv[k]
    raw_or[idx] <- exp(co[k])
    vars[idx]   <- KEY[k]
    outs[idx]   <- labels[i]
    idx <- idx + 1
  }
}

# Apply corrections
p_holm <- p.adjust(raw_p, method="holm")
p_bh   <- p.adjust(raw_p, method="BH")

results <- data.frame(
  Outcome   = outs,
  Predictor = vars,
  OR        = round(raw_or, 3),
  p_raw     = round(raw_p,  4),
  p_holm    = round(p_holm, 4),
  p_BH      = round(p_bh,   4),
  sig_raw   = ifelse(raw_p <.05,"*","ns"),
  sig_holm  = ifelse(p_holm<.05,"*","ns"),
  sig_BH    = ifelse(p_bh  <.05,"*","ns")
)
print(results, row.names=FALSE)

cat(sprintf("\nNote: %d tests; Bonferroni threshold = %.4f\n",
    9, 0.05/9))
cat("Any result surviving Bonferroni? ",
    any(raw_p < 0.05/9), "\n")

# --------------------------------------------------------------------------
# Part 3: Bootstrap CIs for Hisp x Noncitizen on new-account fraud
# --------------------------------------------------------------------------
cat("\n", SEP, "\n")
cat("PART 3: BOOTSTRAP CIs FOR Hisp x Noncit, NEW-ACCOUNT FRAUD\n")
cat("(More reliable than Wald CIs at small cell counts)\n")
cat(SEP, "\n")

set.seed(42)
n_boot <- 1000
boot_coefs <- numeric(n_boot)

cat(sprintf("Running %d bootstrap iterations...\n", n_boot))

f_new <- as.formula(paste("new_acct ~ noncitizen + hispanic +
                            hisp_noncitizen +", CTRL))

for (b in 1:n_boot) {
  # Resample within strata (pseudostrata-aware bootstrap)
  boot_df <- df21 %>%
    group_by(V2117) %>%
    slice_sample(prop=1, replace=TRUE) %>%
    ungroup()
  tryCatch({
    boot_svy <- svydesign(id=~V2118, strata=~V2117,
                          weights=~VS363, data=boot_df, nest=TRUE)
    fit_b <- svyglm(f_new, design=boot_svy,
                    family=quasibinomial("logit"))
    boot_coefs[b] <- coef(fit_b)["hisp_noncitizen"]
  }, error=function(e) {
    boot_coefs[b] <<- NA_real_
  })
  if (b %% 200 == 0) cat(sprintf("  %d/%d done\n", b, n_boot))
}

boot_coefs <- boot_coefs[!is.na(boot_coefs)]
cat(sprintf("Valid bootstrap samples: %d of %d\n",
    length(boot_coefs), n_boot))

# Percentile bootstrap CI
ci_lo <- quantile(boot_coefs, 0.025)
ci_hi <- quantile(boot_coefs, 0.975)

cat(sprintf("\nBootstrap 95%% CI for Hisp x Noncit (new-account fraud):\n"))
cat(sprintf("  Log-OR:  %.4f  [%.4f, %.4f]\n",
    mean(boot_coefs), ci_lo, ci_hi))
cat(sprintf("  OR:      %.4f  [%.4f, %.4f]\n",
    exp(mean(boot_coefs)), exp(ci_lo), exp(ci_hi)))
cat(sprintf("  Wald CI (for comparison): OR=3.33 [0.73, 15.28]\n"))
cat(sprintf("  Bootstrap p (2-tailed):   %.4f\n",
    2*min(mean(boot_coefs>0), mean(boot_coefs<0))))

cat("\n", SEP, "\n")
cat("DONE. Paste full output for paper updates.\n")
cat(SEP, "\n")
