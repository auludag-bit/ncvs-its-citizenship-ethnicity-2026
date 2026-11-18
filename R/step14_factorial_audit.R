# ==========================================================================
# NCVS-ITS -- Step 14: Sample x specification x estimator factorial for the
# 2018 Hispanic x Noncitizen interaction, plus a victimization-adjusted
# sensitivity for the protective-behavior models (Section C). Also the
# source of the restriction test (OR=1.41, p=.076, N=74,908).
#
# Why: the decomposition compares the original study's sample under its
# own specification (age/educ-scale/income-bands/gender/race + 5 protective
# behaviors, NO urbanicity/region) against the corrected sample under the
# corrected specification (dummy education, allocated income rank,
# urbanicity, region, NO protective behaviors). "Sample vs estimator" is
# therefore confounded with specification. This script holds each factor
# fixed in turn.
#
# Recodes copied verbatim from the corrected pipeline (step10, by way of
# step13) and from step10 Section 3 (the original-sample rebuild). Row ids
# are attached before any filtering so the original study's 71,984 cases
# can be located in the corrected data.
#
# Usage: NCVS_DATA_DIR=/path/to/data Rscript step14_factorial_audit.R
# Output: console only, archived as logs/step14_factorial_audit.log.
# ==========================================================================
options(survey.lonely.psu = "adjust", width = 200)
suppressMessages({ library(haven); library(survey); library(dplyr) })

DATA_DIR <- Sys.getenv("NCVS_DATA_DIR", unset = ".")
SEP <- paste(rep("=", 70), collapse = "")

COLS_COMMON <- c(
  "VS008", "V3014", "V3083", "V3024", "V3018", "V3023A", "V3020", "V3015",
  "V2026A", "V2129", "V2127B", "V2117", "V2118", "VS363",
  "VS012", "VS017", "VS019", "VS041", "VS063",
  "VS345", "VS347", "VS349", "VS353", "VS355")
COLS_2018 <- c(COLS_COMMON, "VS009", "V3013", "V3086")
df18_raw <- zap_labels(read_dta(file.path(DATA_DIR, "37923-0001-Data.dta"), col_select = all_of(COLS_2018)))
df21_raw <- zap_labels(read_dta(file.path(DATA_DIR, "38501-0001-Data.dta"), col_select = all_of(COLS_COMMON)))
df18_raw$rid <- seq_len(nrow(df18_raw))

yn <- function(x) case_when(x == 1 ~ 1, x == 2 ~ 0, TRUE ~ NA_real_)
income_rank <- function(x) { r <- as.numeric(x); ifelse(r >= 15, r - 1, r) }

recode_all <- function(df) {
  df %>% mutate(
    completer = as.integer(VS008 %in% c(1, 2)),
    noncitizen = case_when(V3083 == 5 ~ 1L, V3083 %in% 1:4 ~ 0L, TRUE ~ NA_integer_),
    hispanic   = case_when(V3024 == 1 ~ 1L, V3024 == 2 ~ 0L, TRUE ~ NA_integer_),
    hisp_noncitizen = as.integer(noncitizen == 1L & hispanic == 1L),
    citgrp = factor(case_when(noncitizen == 0L & hispanic == 0L ~ "cit_nonhisp",
                              noncitizen == 0L & hispanic == 1L ~ "cit_hisp",
                              noncitizen == 1L & hispanic == 0L ~ "noncit_nonhisp",
                              noncitizen == 1L & hispanic == 1L ~ "noncit_hisp"),
      levels = c("cit_nonhisp", "cit_hisp", "noncit_nonhisp", "noncit_hisp")),
    c012 = case_when(VS012 == 1 ~ 1, VS012 %in% c(2, 9) ~ 0, TRUE ~ NA_real_),
    c017 = case_when(VS017 == 1 ~ 1, VS017 %in% c(2, 9) ~ 0, TRUE ~ NA_real_),
    c019 = case_when(VS019 == 1 ~ 1, VS019 %in% c(2, 9) ~ 0, TRUE ~ NA_real_),
    c041 = case_when(VS041 == 1 ~ 1, VS041 %in% c(2, 9) ~ 0, TRUE ~ NA_real_),
    c063 = case_when(VS063 == 1 ~ 1, VS063 %in% c(2, 9) ~ 0, TRUE ~ NA_real_),
    any_idtheft = pmax(c012, c017, c019, c041, c063, na.rm = FALSE),
    age    = ifelse(V3014 %in% c(-1, 99), NA_real_, as.numeric(V3014)),
    male   = case_when(V3018 == 1 ~ 1L, V3018 == 2 ~ 0L, TRUE ~ NA_integer_),
    race_black = case_when(V3023A == 2 ~ 1L, V3023A %in% c(1, 3:20) ~ 0L, TRUE ~ NA_integer_),
    race_other = case_when(V3023A %in% 3:20 ~ 1L, V3023A %in% c(1, 2) ~ 0L, TRUE ~ NA_integer_),
    edu_lths = case_when(V3020 <= 27 ~ 1L, V3020 %in% c(28, 40:45) ~ 0L, TRUE ~ NA_integer_),
    edu_hs   = case_when(V3020 == 28 ~ 1L, V3020 <= 27 | V3020 %in% 40:45 ~ 0L, TRUE ~ NA_integer_),
    edu_some = case_when(V3020 %in% c(40, 41) ~ 1L, V3020 <= 28 | V3020 %in% 42:45 ~ 0L, TRUE ~ NA_integer_),
    married  = case_when(V3015 == 1 ~ 1L, V3015 %in% 2:5 ~ 0L, TRUE ~ NA_integer_),
    income   = income_rank(V2026A),
    msa_suburban = as.integer(V2129 == 2), msa_central = as.integer(V2129 == 3),
    region_mw = as.integer(V2127B == 2), region_south = as.integer(V2127B == 3),
    region_west = as.integer(V2127B == 4),
    shred = yn(VS349), passwords = yn(VS345), monitoring = yn(VS347),
    protection = yn(VS355), software = yn(VS353)
  )
}

# Maher-coded covariates (step10 Section 3, verbatim), computed on ALL rows
maher_codes <- function(df) {
  df %>% mutate(
    race_may     = case_when(V3023A == 1 ~ 0, V3023A == 2 ~ 1, V3023A %in% 3:20 ~ 2, TRUE ~ NA_real_),
    age_c        = ifelse(V3013 == 99, NA_real_, as.numeric(V3013)),
    married_c    = case_when(V3015 == 1 ~ 1, V3015 %in% 2:5 ~ 0, TRUE ~ NA_real_),
    gender_c     = case_when(V3086 == 1 ~ 1, V3086 == 2 ~ 0, V3086 %in% c(3, 4, 8) ~ NA_real_,
                             TRUE ~ as.numeric(V3086)),
    education_c  = case_when(V3020 == 98 ~ NA_real_, V3020 <= 27 ~ 0, V3020 == 28 ~ 1,
                             V3020 %in% c(40, 41) ~ 2, V3020 == 42 ~ 3, V3020 == 43 ~ 4,
                             V3020 == 44 ~ 5, V3020 == 45 ~ 6, TRUE ~ NA_real_),
    income_c     = case_when(V2026A %in% 1:3 ~ 0, V2026A %in% 4:7 ~ 1, V2026A %in% 8:9 ~ 2,
                             V2026A %in% 10:11 ~ 3, V2026A == 12 ~ 4, V2026A == 13 ~ 5,
                             V2026A == 15 ~ 6, V2026A %in% 16:18 ~ 7, TRUE ~ NA_real_),
    shred_c      = case_when(VS349 == 1 ~ 1, VS349 == 2 ~ 0, TRUE ~ NA_real_),
    passwords_c  = case_when(VS345 == 1 ~ 1, VS345 == 2 ~ 0, TRUE ~ NA_real_),
    monitoring_c = case_when(VS347 == 1 ~ 1, VS347 == 2 ~ 0, TRUE ~ NA_real_),
    protection_c = case_when(VS355 == 1 ~ 1, VS355 == 2 ~ 0, VS355 == 9 ~ 9, TRUE ~ NA_real_),
    software_c   = case_when(VS353 == 1 ~ 1, VS353 == 2 ~ 0, VS353 == 9 ~ 9, TRUE ~ NA_real_)
  )
}

df18 <- maher_codes(recode_all(df18_raw))
df21 <- recode_all(df21_raw)

# ---- Reconstruct Maher's exact 71,984 (step10 Section 3 logic) ----
coop <- df18 %>% filter(VS009 >= 8) %>% mutate(
  dv1 = case_when(VS012 == 1 ~ 1, VS012 == 2 ~ 0, TRUE ~ NA_real_),
  dv2 = case_when(VS017 == 1 ~ 1, VS017 == 2 ~ 0, TRUE ~ NA_real_),
  dv3 = case_when(VS019 == 1 ~ 1, VS019 == 2 ~ 0, TRUE ~ NA_real_),
  dv4 = case_when(VS041 == 1 ~ 1, VS041 == 2 ~ 0, TRUE ~ NA_real_),
  dv5 = case_when(VS063 == 1 ~ 1, VS063 == 2 ~ 0, TRUE ~ NA_real_),
  dv  = pmin(dv1 + dv2 + dv3 + dv4 + dv5, 1))
MVARS <- c("dv", "noncitizen", "hispanic", "race_may", "age_c", "married_c", "gender_c",
           "education_c", "income_c", "shred_c", "software_c", "passwords_c", "monitoring_c", "protection_c")
coop_cc <- coop[complete.cases(coop[, MVARS]), ]
cat(sprintf("Maher rebuild N = %s (target 71,984); dv==any_idtheft on all: %s\n",
    format(nrow(coop_cc), big.mark = ","), all(coop_cc$dv == coop_cc$any_idtheft, na.rm = FALSE)))
S_M <- coop_cc$rid

# ---- Formulas ----
CORE <- "any_idtheft ~ noncitizen + hispanic + hisp_noncitizen"
M_COV  <- "factor(race_may) + age_c + married_c + gender_c + education_c + income_c"
M_PB   <- "shred_c + software_c + passwords_c + monitoring_c + protection_c"
C_COV  <- paste("age + male + race_black + race_other + edu_lths + edu_hs + edu_some +",
                "married + income + msa_suburban + msa_central + region_mw + region_south + region_west")
C_PB   <- "shred + passwords + monitoring + protection + software"
SPECS <- list(
  "Maher coding + PB"   = paste(CORE, "+", M_COV, "+", M_PB),
  "Maher coding, no PB" = paste(CORE, "+", M_COV),
  "Corrected, no PB"    = paste(CORE, "+", C_COV),
  "Corrected + PB"      = paste(CORE, "+", C_COV, "+", C_PB))

df18w <- df18 %>% filter(!is.na(VS363) & VS363 > 0)
df21w <- df21 %>% filter(!is.na(VS363) & VS363 > 0)
des18 <- svydesign(id = ~V2118, strata = ~V2117, weights = ~VS363, data = df18w, nest = TRUE)
des21 <- svydesign(id = ~V2118, strata = ~V2117, weights = ~VS363, data = df21w, nest = TRUE)

base_dom <- function(df) df$completer == 1L & !is.na(df$citgrp) & !is.na(df$any_idtheft) &
  !is.na(df$age) & df$age >= 16
samples18 <- list(
  "S_M Maher's exact 71,984" = df18w$rid %in% S_M,
  "S_R corrected, bank AND card holders" = base_dom(df18w) & df18w$VS012 != 9 & df18w$VS017 != 9,
  "S_C corrected full" = base_dom(df18w))

fit3 <- function(des, df, dom_s, fml) {
  f <- as.formula(fml)
  dom <- dom_s & complete.cases(df[, setdiff(all.vars(f), character(0))])
  dat <- df[dom, ]
  get_row <- function(fit, est_label) {
    sm <- summary(fit)$coefficients
    b <- sm["hisp_noncitizen", "Estimate"]; se <- sm["hisp_noncitizen", 2]; p <- sm["hisp_noncitizen", ncol(sm)]
    dfree <- if (inherits(fit, "svyglm")) degf(fit$survey.design) else Inf
    q <- if (is.finite(dfree)) qt(.975, dfree) else qnorm(.975)
    data.frame(estimator = est_label, N = nrow(dat), OR = round(exp(b), 3),
               lo = round(exp(b - q * se), 2), hi = round(exp(b + q * se), 2), p = round(p, 4))
  }
  fu <- glm(f, data = dat, family = binomial("logit"))
  dat$w_norm <- dat$VS363 / mean(dat$VS363)
  fw <- glm(f, data = dat, weights = w_norm, family = quasibinomial("logit"))
  fd <- svyglm(f, design = subset(des, dom), family = quasibinomial("logit"))
  rbind(get_row(fu, "unweighted/model-SE"), get_row(fw, "weighted/model-SE"), get_row(fd, "weighted/design-SE"))
}

cat("\n", SEP, "\nA. 2018 FACTORIAL: sample x specification x estimator\n", SEP, "\n", sep = "")
grid <- data.frame()
for (sn in names(samples18)) for (sp in names(SPECS)) {
  r <- fit3(des18, df18w, samples18[[sn]], SPECS[[sp]])
  r$sample <- sn; r$spec <- sp
  grid <- rbind(grid, r)
}
print(grid[, c("sample", "spec", "estimator", "N", "OR", "lo", "hi", "p")], row.names = FALSE)

cat("\nSanity: step10-style design built on coop_cc alone (should give p~.052):\n")
des_coop <- svydesign(id = ~V2118, strata = ~V2117, weights = ~VS363, data = coop_cc, nest = TRUE)
fc <- svyglm(as.formula(SPECS[["Maher coding + PB"]]), design = des_coop, family = quasibinomial("logit"))
print(signif(summary(fc)$coefficients["hisp_noncitizen", ], 4))

cat("\n", SEP, "\nB. 2021 corrected sample, +/- PB, three estimators\n", SEP, "\n", sep = "")
for (sp in c("Corrected, no PB", "Corrected + PB")) {
  r <- fit3(des21, df21w, base_dom(df21w), SPECS[[sp]]); r$spec <- sp; print(r, row.names = FALSE)
}

cat("\n", SEP, "\nC. ACJS: protective-behavior interaction, with and without past-year victimization control\n", SEP, "\n", sep = "")
BEH <- c("passwords", "monitoring", "protection", "software", "shred")
for (w in c("2018", "2021")) {
  des <- if (w == "2018") des18 else des21
  df  <- if (w == "2018") df18w else df21w
  out <- data.frame()
  for (b in BEH) {
    dom0 <- df$completer == 1L & !is.na(df$citgrp) & !is.na(df[[b]]) & !is.na(df$age) & df$age >= 16 &
      complete.cases(df[, c("age","male","race_black","race_other","edu_lths","edu_hs","edu_some","married",
                             "income","msa_suburban","msa_central","region_mw","region_south","region_west")])
    f0 <- as.formula(paste(b, "~ noncitizen + hispanic + hisp_noncitizen +", C_COV))
    fit0 <- svyglm(f0, design = subset(des, dom0), family = quasibinomial("logit"))
    dom1 <- dom0 & !is.na(df$any_idtheft)
    f1 <- as.formula(paste(b, "~ noncitizen + hispanic + hisp_noncitizen + any_idtheft +", C_COV))
    fit1 <- svyglm(f1, design = subset(des, dom1), family = quasibinomial("logit"))
    s0 <- summary(fit0)$coefficients; s1 <- summary(fit1)$coefficients
    out <- rbind(out, data.frame(behavior = b, N0 = sum(dom0),
      or0 = exp(s0["hisp_noncitizen", 1]), p0 = s0["hisp_noncitizen", ncol(s0)],
      N1 = sum(dom1), or1 = exp(s1["hisp_noncitizen", 1]), p1 = s1["hisp_noncitizen", ncol(s1)],
      vict_or = exp(s1["any_idtheft", 1])))
  }
  out$p0_holm <- p.adjust(out$p0, "holm"); out$p1_holm <- p.adjust(out$p1, "holm")
  cat(sprintf("\n%s (0 = as in step10; 1 = + past-year victimization control)\n", w))
  print(out %>% mutate(across(c(or0, or1, vict_or), ~round(., 3)),
                       across(c(p0, p1, p0_holm, p1_holm), ~signif(., 3))), row.names = FALSE)
}
cat("\nCOMPLETE.\n")
