# ==========================================================================
# NCVS-ITS -- Step 11: Cross-year and subtype analyses, under the same
# corrected pipeline as step10_corrected_pipeline.R
# ==========================================================================
# Analyses that step10 does not run, all on the same corrected recoding
# (allocated income V2026A, fixed education code 27, no V3086 exclusion,
# residue codes -> NA) and the same corrected samples (2018 N=101,389;
# 2021 N=92,211):
#   A. sample descriptives: weighted prevalence and group shares by wave;
#   B. the pooled 2018+2021 model with year-by-predictor interactions and
#      a joint Wald test;
#   C. victimization-subtype models (2021), Holm-corrected within families;
#   D. the government-benefits-fraud comparison;
#   E. a household-clustered SE robustness check (2021).
#
# Usage: NCVS_DATA_DIR=/path/to/data Rscript step11_crossyear_subtypes.R
# ==========================================================================

options(survey.lonely.psu = "adjust")
suppressMessages({
  library(haven); library(survey); library(dplyr)
})

DATA_DIR <- Sys.getenv("NCVS_DATA_DIR", unset = ".")
SEP <- paste(rep("=", 70), collapse = "")

COLS_COMMON <- c(
  "VS008", "V3014", "V3083", "V3024", "V3018", "V3023A", "V3020", "V3015",
  "V2026A", "V2129", "V2127B", "V2117", "V2118", "VS363", "V2002",
  "VS012", "VS017", "VS019", "VS041", "VS063", "VS075"
)
COLS_2021 <- c(COLS_COMMON, "VS017E")

cat(sprintf("Loading raw data from: %s\n", normalizePath(DATA_DIR)))
df18_raw <- read_dta(file.path(DATA_DIR, "37923-0001-Data.dta"), col_select = all_of(COLS_COMMON))
df21_raw <- read_dta(file.path(DATA_DIR, "38501-0001-Data.dta"), col_select = all_of(COLS_2021))
df18_raw <- zap_labels(df18_raw); df21_raw <- zap_labels(df21_raw)

income_rank <- function(x) { r <- as.numeric(x); ifelse(r >= 15, r - 1, r) }

recode_all <- function(df) {
  df %>% mutate(
    completer = as.integer(VS008 %in% c(1, 2)),
    noncitizen = case_when(V3083 == 5 ~ 1L, V3083 %in% 1:4 ~ 0L, TRUE ~ NA_integer_),
    hispanic   = case_when(V3024 == 1 ~ 1L, V3024 == 2 ~ 0L, TRUE ~ NA_integer_),
    hisp_noncitizen = as.integer(noncitizen == 1L & hispanic == 1L),
    citgrp = factor(
      case_when(noncitizen == 0L & hispanic == 0L ~ "cit_nonhisp",
                noncitizen == 0L & hispanic == 1L ~ "cit_hisp",
                noncitizen == 1L & hispanic == 0L ~ "noncit_nonhisp",
                noncitizen == 1L & hispanic == 1L ~ "noncit_hisp"),
      levels = c("cit_nonhisp", "cit_hisp", "noncit_nonhisp", "noncit_hisp")),
    c012 = case_when(VS012 == 1 ~ 1, VS012 %in% c(2, 9) ~ 0, TRUE ~ NA_real_),
    c017 = case_when(VS017 == 1 ~ 1, VS017 %in% c(2, 9) ~ 0, TRUE ~ NA_real_),
    c019 = case_when(VS019 == 1 ~ 1, VS019 %in% c(2, 9) ~ 0, TRUE ~ NA_real_),
    c041 = case_when(VS041 == 1 ~ 1, VS041 %in% c(2, 9) ~ 0, TRUE ~ NA_real_),
    c063 = case_when(VS063 == 1 ~ 1, VS063 %in% c(2, 9) ~ 0, TRUE ~ NA_real_),
    any_idtheft   = pmax(c012, c017, c019, c041, c063, na.rm = FALSE),
    existing_acct = pmax(c012, c017, c019, na.rm = FALSE),
    new_account   = c041,
    other_misuse  = c063,
    age    = ifelse(V3014 %in% c(-1, 99), NA_real_, as.numeric(V3014)),
    male   = case_when(V3018 == 1 ~ 1L, V3018 == 2 ~ 0L, TRUE ~ NA_integer_),
    race_black = case_when(V3023A == 2 ~ 1L, V3023A %in% c(1, 3:20) ~ 0L, TRUE ~ NA_integer_),
    race_other = case_when(V3023A %in% 3:20 ~ 1L, V3023A %in% c(1, 2) ~ 0L, TRUE ~ NA_integer_),
    edu_lths = case_when(V3020 <= 27 ~ 1L, V3020 %in% c(28, 40:45) ~ 0L, TRUE ~ NA_integer_),
    edu_hs   = case_when(V3020 == 28 ~ 1L, V3020 <= 27 | V3020 %in% 40:45 ~ 0L, TRUE ~ NA_integer_),
    edu_some = case_when(V3020 %in% c(40, 41) ~ 1L, V3020 <= 28 | V3020 %in% 42:45 ~ 0L, TRUE ~ NA_integer_),
    married  = case_when(V3015 == 1 ~ 1L, V3015 %in% 2:5 ~ 0L, TRUE ~ NA_integer_),
    income     = income_rank(V2026A),
    msa_suburban = as.integer(V2129 == 2),
    msa_central  = as.integer(V2129 == 3),
    region_mw    = as.integer(V2127B == 2),
    region_south = as.integer(V2127B == 3),
    region_west  = as.integer(V2127B == 4)
  )
}

df18 <- recode_all(df18_raw); df21 <- recode_all(df21_raw)
df18$year <- 0; df21$year <- 1
df21$email_social <- case_when(df21$VS017E == 1 ~ 1, df21$VS017E %in% c(2, 9) ~ 0, TRUE ~ NA_real_)

df18w <- df18 %>% filter(!is.na(VS363) & VS363 > 0)
df21w <- df21 %>% filter(!is.na(VS363) & VS363 > 0)

CTRL <- paste("age + male + race_black + race_other +",
              "edu_lths + edu_hs + edu_some + married + income +",
              "msa_suburban + msa_central + region_mw + region_south + region_west")
CTRL_VARS <- c("age","male","race_black","race_other","edu_lths","edu_hs","edu_some",
               "married","income","msa_suburban","msa_central","region_mw","region_south","region_west")

des18 <- svydesign(id = ~V2118, strata = ~V2117, weights = ~VS363, data = df18w, nest = TRUE)
des21 <- svydesign(id = ~V2118, strata = ~V2117, weights = ~VS363, data = df21w, nest = TRUE)

dom_expr <- function(df, outcome) {
  df$completer == 1L & !is.na(df$citgrp) & !is.na(df[[outcome]]) &
    !is.na(df$age) & df$age >= 16 & complete.cases(df[, CTRL_VARS])
}

report_interaction <- function(fit, label) {
  sm <- summary(fit)$coefficients
  cat(sprintf("%-45s N=%-8s OR=%.4f  p=%.4g\n", label,
      format(nrow(model.frame(fit)), big.mark=","),
      exp(sm["hisp_noncitizen", "Estimate"]), sm["hisp_noncitizen", ncol(sm)]))
  invisible(sm)
}

# ==========================================================================
cat("\n", SEP, "\nA. SAMPLE DESCRIPTIVES (corrected pipeline)\n", SEP, "\n", sep = "")
for (w in c("2018","2021")) {
  df <- if (w == "2018") df18w else df21w
  dom <- dom_expr(df, "any_idtheft")
  n <- sum(dom)
  prev <- weighted.mean(df$any_idtheft[dom], df$VS363[dom])
  hisp <- weighted.mean(df$hispanic[dom], df$VS363[dom])
  noncit <- weighted.mean(df$noncitizen[dom], df$VS363[dom])
  cat(sprintf("%s: N=%s  prevalence=%.2f%%  hispanic=%.2f%%  noncitizen=%.2f%%\n",
      w, format(n, big.mark=","), 100*prev, 100*hisp, 100*noncit))
}

# ==========================================================================
cat("\n", SEP, "\nB. POOLED CROSS-YEAR MODEL (corrected pipeline)\n", SEP, "\n", sep = "")
common <- intersect(names(df18w), names(df21w))
pooled <- bind_rows(df18w[, common], df21w[, common])
pooled$stratum2 <- interaction(pooled$year, pooled$V2117, drop = TRUE)
pooled$psu2 <- interaction(pooled$year, pooled$V2118, drop = TRUE)
des_p <- svydesign(id = ~psu2, strata = ~stratum2, weights = ~VS363, data = pooled, nest = TRUE)
dom_p <- dom_expr(pooled, "any_idtheft")
fit_pooled <- svyglm(as.formula(paste(
  "any_idtheft ~ year*noncitizen + year*hispanic + year*hisp_noncitizen +", CTRL)),
  design = subset(des_p, dom_p), family = quasibinomial("logit"))
print(summary(fit_pooled)$coefficients[c("year","year:noncitizen","year:hispanic",
      "year:hisp_noncitizen"), ], digits = 4)
wt <- regTermTest(fit_pooled, ~year:noncitizen + year:hispanic + year:hisp_noncitizen, method = "Wald")
cat(sprintf("Joint Wald test, 3 interaction terms: F=%.3f, df=(%.0f,%.1f), p=%.4g\n",
    as.numeric(wt$Ftest), as.numeric(wt$df), as.numeric(wt$ddf), as.numeric(wt$p)))

# ==========================================================================
cat("\n", SEP, "\nC. SUBTYPE ANALYSIS, 2021 (corrected pipeline)\n", SEP, "\n", sep = "")
subtypes <- c("existing_acct", "new_account", "other_misuse", "email_social")
sub_res <- data.frame()
for (oc in subtypes) {
  dom <- dom_expr(df21w, oc)
  fit <- svyglm(as.formula(paste(oc, "~ noncitizen + hispanic + hisp_noncitizen +", CTRL)),
                design = subset(des21, dom), family = quasibinomial("logit"))
  sm <- summary(fit)$coefficients
  n_events <- sum(df21w[[oc]][dom] == 1, na.rm = TRUE)
  n_hn_events <- sum(df21w[[oc]][dom] == 1 & df21w$hisp_noncitizen[dom] == 1, na.rm = TRUE)
  n_hn_total  <- sum(dom & df21w$hisp_noncitizen == 1, na.rm = TRUE)
  report_interaction(fit, oc)
  sub_res <- rbind(sub_res, data.frame(
    outcome = oc, n = nrow(model.frame(fit)), events = n_events,
    hisp_or = exp(sm["hispanic","Estimate"]), hisp_p = sm["hispanic", ncol(sm)],
    noncit_or = exp(sm["noncitizen","Estimate"]), noncit_p = sm["noncitizen", ncol(sm)],
    inter_or = exp(sm["hisp_noncitizen","Estimate"]), inter_p = sm["hisp_noncitizen", ncol(sm)],
    hn_events = n_hn_events, hn_total = n_hn_total))
}
sub_res$inter_p_holm <- p.adjust(sub_res$inter_p, "holm")
sub_res$hisp_p_holm  <- p.adjust(sub_res$hisp_p,  "holm")
# Holm correction for the noncitizen family too, the same treatment as the
# interaction and Hispanic families above.
sub_res$noncit_p_holm <- p.adjust(sub_res$noncit_p, "holm")
options(width = 200)
cat("\nSubtype summary (Holm across the 4 tests, separately within the interaction, Hispanic, and noncitizen families):\n")
print(sub_res, row.names = FALSE)

# ==========================================================================
cat("\n", SEP, "\nD. BENEFITS-FRAUD PANDEMIC SIGNAL (corrected other_misuse pipeline)\n", SEP, "\n", sep = "")
for (w in c("2018","2021")) {
  df <- if (w == "2018") df18w else df21w
  vic <- df %>% filter(completer == 1, other_misuse == 1, VS075 %in% c(1,2))
  n_asked <- nrow(vic); n_yes <- sum(vic$VS075 == 1)
  w_pct <- weighted.mean(vic$VS075 == 1, vic$VS363)
  cat(sprintf("%s: other-misuse victims with valid VS075 n=%d, unweighted %.1f%% (%d/%d), weighted %.1f%%\n",
      w, n_asked, 100*n_yes/n_asked, n_yes, n_asked, 100*w_pct))
}

# ==========================================================================
cat("\n", SEP, "\nE. HOUSEHOLD-CLUSTERED SE ROBUSTNESS CHECK, 2021 (corrected pipeline)\n", SEP, "\n", sep = "")
dom21 <- dom_expr(df21w, "any_idtheft")
d21 <- df21w[dom21, ]
cat(sprintf("2021 domain: %s respondents in %s unique households (%.2f per household)\n",
    format(nrow(d21), big.mark=","), format(length(unique(d21$V2002)), big.mark=","),
    nrow(d21)/length(unique(d21$V2002))))
fit_official <- svyglm(as.formula(paste("any_idtheft ~ noncitizen + hispanic + hisp_noncitizen +", CTRL)),
                        design = subset(des21, dom21), family = quasibinomial("logit"))
des_hh <- svydesign(id = ~V2002, weights = ~VS363, data = d21)
fit_hh <- svyglm(as.formula(paste("any_idtheft ~ noncitizen + hispanic + hisp_noncitizen +", CTRL)),
                  design = des_hh, family = quasibinomial("logit"))
cat("Official pseudostratum/half-sample design:\n")
print(summary(fit_official)$coefficients[c("noncitizen","hispanic","hisp_noncitizen"),], digits=4)
cat("Household-clustered design:\n")
print(summary(fit_hh)$coefficients[c("noncitizen","hispanic","hisp_noncitizen"),], digits=4)

cat("\n", SEP, "\nCOMPLETE.\n", SEP, "\n", sep = "")
