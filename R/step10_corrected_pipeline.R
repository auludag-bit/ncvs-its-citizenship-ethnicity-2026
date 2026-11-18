# ==========================================================================
# NCVS-ITS -- Step 10: Corrected analysis pipeline, 2018 and 2021 waves
# ==========================================================================
# The main analysis driver. One run builds the corrected analytic samples
# (2018 N=101,389; 2021 N=92,211), fits the design-correct survey-weighted
# logistic regressions, rebuilds the original study's exact 2018 sample
# (Section 3), and fits the protective-behavior models. It prints every
# number to the console and writes CSV files of aggregate results only
# (no microdata).
#
# Recoding fixes relative to the project's earlier pipeline (all verified
# against the .dta value labels directly, not assumed):
#   1. EDUCATION FIX: V3020 code 27 is "12th grade (NO diploma)", not
#      "High school grad" (that is 28). Old code counted 27 as HS-grad;
#      corrected here to less-than-HS. Residue 98 -> NA (was silently
#      falling into the BA+ reference category).
#   2. MARITAL FIX: V3015 residue code 8 -> NA (was "not married").
#   3. RACE FIX: V3023A residue/invalid codes (-1, 98, 99) -> NA
#      (defensive; not observed in these waves' tabulations).
#   4. GENDER: the arbitrary V3086 in {3,4} exclusion (dropped transgender
#      / "none of these" respondents while keeping residue and blank) is
#      REMOVED. Sex control uses V3018 (SEX, ALLOCATED; values 1/2 only,
#      no residue codes in either wave).
#   5. INCOME: uses V2026A (HOUSEHOLD INCOME, ALLOCATED) instead of raw
#      V2026. V2026A is complete (no 98/99), eliminating the ~22% listwise
#      loss. NOTE: V2026A band codes run 1..18 with NO code 14.  To keep a
#      well-defined ordinal index, bands are mapped to their rank (1..17).
#   6. DESIGN BEFORE DOMAIN: the survey design object is constructed on the
#      full positive-weight ITS file FIRST; all analytic restrictions
#      (valid predictor codes, age, victim status) are applied with
#      subset() on the design (domain estimation), never by filtering rows
#      before svydesign().
#   7. SURVEY DF: inference uses svyglm's t-based p-values / confint with
#      design degrees of freedom, not the normal approximation.
#   8. 2021 IS THE PRIMARY WAVE; 2018 is a separate replication. There is
#      deliberately NO pooled cross-year interaction model in this script
#      (BJS documents four instrument changes across waves).
#
# ANALYSIS PLAN (declared before results were seen; families for
# multiplicity correction):
#   PRIMARY OUTCOME:  any institutional contact among past-year victims
#                     (VS098 fin. institution | VS100 credit bureau |
#                      VS133 law enforcement), 2021, adjusted model.
#   PRIMARY TEST:     4-group (citizenship x ethnicity) differences in the
#                     primary outcome: joint Wald test, then the 6 pairwise
#                     group contrasts, Holm-corrected within this family.
#   SECONDARY FAMILY A: the three contact components + police report taken
#                     (VS135, conditional on VS133=1), 2021: Holm across
#                     the 4 outcomes' Hispanic-noncitizen contrasts.
#   SECONDARY FAMILY B: protective-behavior adoption (5 outcomes), per
#                     wave: Holm across the 5 interaction terms.
#   REPLICATION (2018) and sensitivity analyses are reported without
#   correction, labeled as such.
#
# Usage:
#   NCVS_DATA_DIR=/path/to/data Rscript step10_corrected_pipeline.R
# Outputs: console (full numbers) + CSVs under $NCVS_OUT_DIR (default
# ./out_ccnc relative to the working directory).
# ==========================================================================

options(survey.lonely.psu = "adjust")
suppressMessages({
  library(haven)
  library(survey)
  library(dplyr)
})

DATA_DIR <- Sys.getenv("NCVS_DATA_DIR", unset = ".")
OUT_DIR  <- Sys.getenv("NCVS_OUT_DIR",  unset = "out_ccnc")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

SEP <- paste(rep("=", 70), collapse = "")

# --------------------------------------------------------------------------
# Load (column-selected; files are ~2GB each with 1000+ columns)
# --------------------------------------------------------------------------
COLS_COMMON <- c(
  "VS008", "V3014", "V3083", "V3024", "V3018", "V3023A", "V3020", "V3015",
  "V2026A", "V2129", "V2127B", "V2117", "V2118", "VS363",
  "VS012", "VS017", "VS019", "VS041", "VS063",
  "VS345", "VS347", "VS349", "VS353", "VS355",
  "VS098", "VS100", "VS133", "VS135", "VS306"
)
COLS_2018 <- c(COLS_COMMON, "VS009", "V3013", "V3086")   # Section 3 rebuild needs these
COLS_2021 <- c(COLS_COMMON, "VS010A", "VS016A", "VS017E")

cat(sprintf("Loading raw data from: %s\n", normalizePath(DATA_DIR)))
df18_raw <- read_dta(file.path(DATA_DIR, "37923-0001-Data.dta"),
                     col_select = all_of(COLS_2018))
df21_raw <- read_dta(file.path(DATA_DIR, "38501-0001-Data.dta"),
                     col_select = all_of(COLS_2021))
df18_raw <- zap_labels(df18_raw)
df21_raw <- zap_labels(df21_raw)

# --------------------------------------------------------------------------
# Recodes (corrected; applied to the full file, rows are NOT dropped here)
# --------------------------------------------------------------------------
yn <- function(x) case_when(x == 1 ~ 1, x == 2 ~ 0, TRUE ~ NA_real_)

# V2026A band codes 1..13,15..18 -> ordinal rank 1..17
income_rank <- function(x) {
  r <- as.numeric(x)
  ifelse(r >= 15, r - 1, r)
}

recode_all <- function(df) {
  df %>% mutate(
    completer = as.integer(VS008 %in% c(1, 2)),
    # Predictors of interest (residue codes -> NA, not reference)
    noncitizen = case_when(V3083 == 5 ~ 1L, V3083 %in% 1:4 ~ 0L,
                           TRUE ~ NA_integer_),
    hispanic   = case_when(V3024 == 1 ~ 1L, V3024 == 2 ~ 0L,
                           TRUE ~ NA_integer_),
    hisp_noncitizen = as.integer(noncitizen == 1L & hispanic == 1L),
    citgrp = factor(
      case_when(noncitizen == 0L & hispanic == 0L ~ "cit_nonhisp",
                noncitizen == 0L & hispanic == 1L ~ "cit_hisp",
                noncitizen == 1L & hispanic == 0L ~ "noncit_nonhisp",
                noncitizen == 1L & hispanic == 1L ~ "noncit_hisp"),
      levels = c("cit_nonhisp", "cit_hisp", "noncit_nonhisp", "noncit_hisp")),
    # Outcome components: among completers, screener code 9 (routed past
    # the item as a lifetime non-victim) is a legitimate 0, not missing
    c012 = case_when(VS012 == 1 ~ 1, VS012 %in% c(2, 9) ~ 0, TRUE ~ NA_real_),
    c017 = case_when(VS017 == 1 ~ 1, VS017 %in% c(2, 9) ~ 0, TRUE ~ NA_real_),
    c019 = case_when(VS019 == 1 ~ 1, VS019 %in% c(2, 9) ~ 0, TRUE ~ NA_real_),
    c041 = case_when(VS041 == 1 ~ 1, VS041 %in% c(2, 9) ~ 0, TRUE ~ NA_real_),
    c063 = case_when(VS063 == 1 ~ 1, VS063 %in% c(2, 9) ~ 0, TRUE ~ NA_real_),
    any_idtheft   = pmax(c012, c017, c019, c041, c063, na.rm = FALSE),
    existing_acct = pmax(c012, c017, c019, na.rm = FALSE),
    # Controls
    age    = ifelse(V3014 %in% c(-1, 99), NA_real_, as.numeric(V3014)),
    male   = case_when(V3018 == 1 ~ 1L, V3018 == 2 ~ 0L, TRUE ~ NA_integer_),
    race_black = case_when(V3023A == 2 ~ 1L, V3023A %in% c(1, 3:20) ~ 0L,
                           TRUE ~ NA_integer_),
    race_other = case_when(V3023A %in% 3:20 ~ 1L, V3023A %in% c(1, 2) ~ 0L,
                           TRUE ~ NA_integer_),
    # EDUCATION (fixed): 27 = 12th grade NO diploma -> less than HS;
    # 28 = HS graduate; 40/41 = some college/associate; 42-45 = BA+ (ref);
    # 98 = residue -> NA
    edu_lths = case_when(V3020 <= 27 ~ 1L, V3020 %in% c(28, 40:45) ~ 0L,
                         TRUE ~ NA_integer_),
    edu_hs   = case_when(V3020 == 28 ~ 1L,
                         V3020 <= 27 | V3020 %in% 40:45 ~ 0L,
                         TRUE ~ NA_integer_),
    edu_some = case_when(V3020 %in% c(40, 41) ~ 1L,
                         V3020 <= 28 | V3020 %in% 42:45 ~ 0L,
                         TRUE ~ NA_integer_),
    married  = case_when(V3015 == 1 ~ 1L, V3015 %in% 2:5 ~ 0L,
                         TRUE ~ NA_integer_),          # 8 residue -> NA
    income     = income_rank(V2026A),                  # allocated, complete
    inc_band   = factor(pmin(pmax(findInterval(
                   as.numeric(V2026A), c(1, 4, 8, 10, 12, 13, 15, 16)), 1), 8)),
    msa_suburban = as.integer(V2129 == 2),
    msa_central  = as.integer(V2129 == 3),
    region_mw    = as.integer(V2127B == 2),
    region_south = as.integer(V2127B == 3),
    region_west  = as.integer(V2127B == 4),
    # Protective behaviors (1=Yes, 2=No; 8/9/98/99 -> NA)
    shred      = yn(VS349),
    passwords  = yn(VS345),
    monitoring = yn(VS347),
    protection = yn(VS355),
    software   = yn(VS353),
    # Prior (outside past year) misuse of personal information
    prior_victim = yn(VS306),
    # Institutional contact among past-year victims
    contact_fin    = yn(VS098),
    contact_bureau = yn(VS100),
    contact_le     = yn(VS133),
    police_report  = yn(VS135)
  ) %>% mutate(
    any_contact = case_when(
      contact_fin == 1 | contact_bureau == 1 | contact_le == 1 ~ 1,
      !is.na(contact_fin) | !is.na(contact_bureau) | !is.na(contact_le) ~ 0,
      TRUE ~ NA_real_
    )
  )
}

df18 <- recode_all(df18_raw)
df21 <- recode_all(df21_raw)

cat(sprintf("2018: rows=%d, VS363>0: %d, completers: %d\n",
    nrow(df18), sum(df18$VS363 > 0, na.rm = TRUE), sum(df18$completer)))
cat(sprintf("2021: rows=%d, VS363>0: %d, completers: %d\n",
    nrow(df21), sum(df21$VS363 > 0, na.rm = TRUE), sum(df21$completer)))

# --------------------------------------------------------------------------
# Survey designs on the FULL positive-weight file; domains via subset()
# --------------------------------------------------------------------------
df18w <- df18 %>% filter(!is.na(VS363) & VS363 > 0)
df21w <- df21 %>% filter(!is.na(VS363) & VS363 > 0)

des18 <- svydesign(id = ~V2118, strata = ~V2117, weights = ~VS363,
                   data = df18w, nest = TRUE)
des21 <- svydesign(id = ~V2118, strata = ~V2117, weights = ~VS363,
                   data = df21w, nest = TRUE)

CTRL <- paste("age + male + race_black + race_other +",
              "edu_lths + edu_hs + edu_some + married + income +",
              "msa_suburban + msa_central +",
              "region_mw + region_south + region_west")
CTRL_VARS <- c("age", "male", "race_black", "race_other", "edu_lths",
               "edu_hs", "edu_some", "married", "income", "msa_suburban",
               "msa_central", "region_mw", "region_south", "region_west")

# Domain: completer, valid group, valid controls, valid outcome
dom_expr <- function(df, outcome) {
  df$completer == 1L & !is.na(df$citgrp) & !is.na(df[[outcome]]) &
    !is.na(df$age) & df$age >= 16 &
    complete.cases(df[, CTRL_VARS])
}

# --------------------------------------------------------------------------
# Helpers: OR table with survey-df t inference; predictive margins with
# delta-method SEs (survey-standardized over the domain sample)
# --------------------------------------------------------------------------
print_fit <- function(fit, label, rows) {
  cat(sprintf("\n--- %s ---\n", label))
  sm <- summary(fit)$coefficients
  ci <- suppressMessages(confint(fit))
  rows <- rows[rows %in% rownames(sm)]
  res <- data.frame(
    Variable = rows,
    OR      = round(exp(sm[rows, "Estimate"]), 4),
    CI_low  = round(exp(ci[rows, 1]), 4),
    CI_high = round(exp(ci[rows, 2]), 4),
    p_value = signif(sm[rows, ncol(sm)], 4),
    row.names = NULL)
  print(res, row.names = FALSE)
  cat(sprintf("N (model frame) = %s;  design df = %.0f\n",
      format(nrow(model.frame(fit)), big.mark = ","), degf(fit$survey.design)))
  invisible(res)
}

# Predictive margins by citgrp: g-computation over the model frame with
# survey weights; delta-method variance using the fit's design-based vcov.
svy_margins <- function(fit, dat, wts) {
  X0  <- model.matrix(formula(fit), dat)
  V   <- vcov(fit)
  b   <- coef(fit)
  lev <- levels(dat$citgrp)
  ddf <- degf(fit$survey.design)
  out <- data.frame(); grads <- list()
  for (g in lev) {
    dg <- dat; dg$citgrp <- factor(g, levels = lev)
    Xg <- model.matrix(formula(fit), dg)
    eta <- as.vector(Xg %*% b); p <- plogis(eta)
    m  <- sum(wts * p) / sum(wts)
    gr <- as.vector(t(Xg) %*% (wts * p * (1 - p))) / sum(wts)
    se <- sqrt(t(gr) %*% V %*% gr)[1, 1]
    grads[[g]] <- gr
    out <- rbind(out, data.frame(
      group = g, margin = m, se = se,
      ci_lo = m - qt(.975, ddf) * se, ci_hi = m + qt(.975, ddf) * se))
  }
  pairs <- combn(lev, 2, simplify = FALSE)
  ctr <- data.frame()
  for (pr in pairs) {
    d  <- out$margin[out$group == pr[2]] - out$margin[out$group == pr[1]]
    gr <- grads[[pr[2]]] - grads[[pr[1]]]
    se <- sqrt(t(gr) %*% V %*% gr)[1, 1]
    tv <- d / se
    ctr <- rbind(ctr, data.frame(
      contrast = paste(pr[2], "-", pr[1]), rd = d, se = se,
      ci_lo = d - qt(.975, ddf) * se, ci_hi = d + qt(.975, ddf) * se,
      p = 2 * pt(-abs(tv), ddf)))
  }
  ctr$p_holm <- p.adjust(ctr$p, "holm")
  list(margins = out, contrasts = ctr)
}

# Fit adjusted models in both parameterizations on one domain, print
# everything, return the pieces needed downstream.
run_outcome <- function(des, df, outcome, wave, family_label,
                        extra_domain = TRUE, ctrl = CTRL) {
  dom <- dom_expr(df, outcome) & extra_domain
  dsub <- subset(des, dom)
  f_int <- as.formula(paste(outcome,
    "~ noncitizen + hispanic + hisp_noncitizen +", ctrl))
  f_grp <- as.formula(paste(outcome, "~ citgrp +", ctrl))
  fit_int <- svyglm(f_int, design = dsub, family = quasibinomial("logit"))
  fit_grp <- svyglm(f_grp, design = dsub, family = quasibinomial("logit"))

  print_fit(fit_int, sprintf("%s %s (%s): adjusted, dummy+interaction",
                             wave, outcome, family_label),
            c("noncitizen", "hispanic", "hisp_noncitizen"))
  wt <- regTermTest(fit_grp, ~citgrp, method = "Wald")
  cat(sprintf("Joint Wald test, citgrp (4 groups): F=%.3f, df=(%.0f, %.1f), p=%.4g\n",
      as.numeric(wt$Ftest), as.numeric(wt$df), as.numeric(wt$ddf),
      as.numeric(wt$p)))

  mf  <- model.frame(fit_grp)
  dat <- df[dom, , drop = FALSE]
  dat <- dat[complete.cases(dat[, all.vars(f_grp)]), , drop = FALSE]
  mg  <- svy_margins(fit_grp, dat, dat$VS363)
  cat("\nSurvey-standardized predicted probabilities (predictive margins):\n")
  cnt <- dat %>% group_by(citgrp) %>%
    summarise(n = n(), events = sum(.data[[outcome]]), .groups = "drop")
  mg$margins <- left_join(mg$margins, cnt, by = c("group" = "citgrp"))
  # signif() only the estimates; n/events are integers and must print exactly
  print(mg$margins %>% mutate(across(c(margin, se, ci_lo, ci_hi),
                                     ~signif(., 4))),
        row.names = FALSE)
  cat("\nPairwise risk differences (Holm within this outcome):\n")
  print(mg$contrasts %>% mutate(across(where(is.numeric), ~signif(., 4))),
        row.names = FALSE)
  invisible(list(fit_int = fit_int, fit_grp = fit_grp, margins = mg,
                 n = nrow(mf), counts = cnt))
}

# ==========================================================================
cat("\n", SEP, "\nSECTION 1: VICTIMIZATION MODELS (2021 PRIMARY, 2018 REPLICATION)\n",
    SEP, "\n", sep = "")

# Bivariate (no controls) first, for the descriptive baseline
for (w in c("2021", "2018")) {
  des <- if (w == "2021") des21 else des18
  df  <- if (w == "2021") df21w else df18w
  dom <- df$completer == 1L & !is.na(df$citgrp) & !is.na(df$any_idtheft) &
         !is.na(df$age) & df$age >= 16
  fitb <- svyglm(any_idtheft ~ noncitizen + hispanic + hisp_noncitizen,
                 design = subset(des, dom), family = quasibinomial("logit"))
  print_fit(fitb, sprintf("%s any_idtheft: bivariate (no controls)", w),
            c("noncitizen", "hispanic", "hisp_noncitizen"))
}

res21 <- run_outcome(des21, df21w, "any_idtheft", "2021", "PRIMARY WAVE")
res18 <- run_outcome(des18, df18w, "any_idtheft", "2018", "REPLICATION")

write.csv(bind_rows(mutate(res21$margins$margins, wave = "2021"),
                    mutate(res18$margins$margins, wave = "2018")),
          file.path(OUT_DIR, "fig1_victimization_margins.csv"), row.names = FALSE)

# ==========================================================================
cat("\n", SEP, "\nSECTION 2: INSTITUTIONAL CONTACT AMONG PAST-YEAR VICTIMS\n",
    "(PRIMARY OUTCOME: any_contact, 2021)\n", SEP, "\n", sep = "")

# Universe check: how the contact items distribute among victims
for (w in c("2021", "2018")) {
  df <- if (w == "2021") df21w else df18w
  vic <- df %>% filter(completer == 1L, any_idtheft == 1)
  cat(sprintf("\n%s victims (unweighted n=%d): raw contact-item codes\n",
              w, nrow(vic)))
  for (v in c("VS098", "VS100", "VS133", "VS135")) {
    tb <- table(vic[[v]], useNA = "ifany")
    cat(sprintf("  %s: %s\n", v,
        paste(sprintf("%s=%d", names(tb), tb), collapse = "  ")))
  }
  cat(sprintf("  any_contact: %s\n",
      paste(capture.output(print(table(vic$any_contact, useNA = "ifany"))),
            collapse = " | ")))
}

vic_dom21 <- df21w$any_idtheft == 1 & !is.na(df21w$any_idtheft)
vic_dom18 <- df18w$any_idtheft == 1 & !is.na(df18w$any_idtheft)

contact21 <- run_outcome(des21, df21w, "any_contact", "2021",
                         "PRIMARY OUTCOME (victims only)",
                         extra_domain = vic_dom21)
contact18 <- run_outcome(des18, df18w, "any_contact", "2018",
                         "REPLICATION (victims only)",
                         extra_domain = vic_dom18)

write.csv(bind_rows(mutate(contact21$margins$margins, wave = "2021"),
                    mutate(contact18$margins$margins, wave = "2018")),
          file.path(OUT_DIR, "fig_contact_margins.csv"), row.names = FALSE)

cat("\n--- SECONDARY FAMILY A: components, 2021, victims only ---\n")
famA <- data.frame()
for (oc in c("contact_fin", "contact_bureau", "contact_le")) {
  r <- run_outcome(des21, df21w, oc, "2021", "component", extra_domain = vic_dom21)
  sm <- summary(r$fit_int)$coefficients
  famA <- rbind(famA, data.frame(outcome = oc,
    or = exp(sm["hisp_noncitizen", "Estimate"]),
    p  = sm["hisp_noncitizen", ncol(sm)]))
}
# Police report taken, conditional on any law-enforcement contact
le_dom21 <- vic_dom21 & df21w$contact_le == 1 & !is.na(df21w$contact_le)
r <- run_outcome(des21, df21w, "police_report", "2021",
                 "component (contacted LE only)", extra_domain = le_dom21)
sm <- summary(r$fit_int)$coefficients
famA <- rbind(famA, data.frame(outcome = "police_report",
  or = exp(sm["hisp_noncitizen", "Estimate"]),
  p  = sm["hisp_noncitizen", ncol(sm)]))
famA$p_holm <- p.adjust(famA$p, "holm")
cat("\nFamily A summary (hisp_noncitizen interaction ORs, Holm):\n")
print(famA %>% mutate(across(where(is.numeric), ~signif(., 4))), row.names = FALSE)

# ==========================================================================
cat("\n", SEP, "\nSECTION 3: RECONCILIATION -- COOPER'S EXACT 2018 SAMPLE,\n",
    "THREE-ROW ESTIMATOR DECOMPOSITION\n", SEP, "\n", sep = "")

# Rebuild the original study's exact sample (Maher, 2024) from the
# author's Stata do-file, reproduced faithfully INCLUDING its
# choices we do not endorse (screener 9 -> missing; education 0-6 and
# income bands 0-7 entered as continuous; V3086-based gender with blank
# code 9 retained), because the point is exact replication.
coop <- df18_raw %>%
  filter(VS009 >= 8) %>%
  mutate(
    dv1 = case_when(VS012 == 1 ~ 1, VS012 == 2 ~ 0, TRUE ~ NA_real_),
    dv2 = case_when(VS017 == 1 ~ 1, VS017 == 2 ~ 0, TRUE ~ NA_real_),
    dv3 = case_when(VS019 == 1 ~ 1, VS019 == 2 ~ 0, TRUE ~ NA_real_),
    dv4 = case_when(VS041 == 1 ~ 1, VS041 == 2 ~ 0, TRUE ~ NA_real_),
    dv5 = case_when(VS063 == 1 ~ 1, VS063 == 2 ~ 0, TRUE ~ NA_real_),
    dv  = pmin(dv1 + dv2 + dv3 + dv4 + dv5, 1),
    dich_citizen = case_when(V3083 %in% 1:4 ~ 0, V3083 == 5 ~ 1,
                             TRUE ~ NA_real_),
    hispanic     = case_when(V3024 == 1 ~ 1, V3024 == 2 ~ 0, TRUE ~ NA_real_),
    race_may     = case_when(V3023A == 1 ~ 0, V3023A == 2 ~ 1,
                             V3023A %in% 3:20 ~ 2, TRUE ~ NA_real_),
    age_c        = ifelse(V3013 == 99, NA_real_, as.numeric(V3013)),
    married_c    = case_when(V3015 == 1 ~ 1, V3015 %in% 2:5 ~ 0,
                             TRUE ~ NA_real_),
    gender_c     = case_when(V3086 == 1 ~ 1, V3086 == 2 ~ 0,
                             V3086 %in% c(3, 4, 8) ~ NA_real_,
                             TRUE ~ as.numeric(V3086)),   # blank 9 retained, as in original
    education_c  = case_when(V3020 == 98 ~ NA_real_,
                             V3020 <= 27 ~ 0, V3020 == 28 ~ 1,
                             V3020 %in% c(40, 41) ~ 2, V3020 == 42 ~ 3,
                             V3020 == 43 ~ 4, V3020 == 44 ~ 5,
                             V3020 == 45 ~ 6, TRUE ~ NA_real_),
    income_c     = case_when(V2026A %in% 1:3 ~ 0, V2026A %in% 4:7 ~ 1,
                             V2026A %in% 8:9 ~ 2, V2026A %in% 10:11 ~ 3,
                             V2026A == 12 ~ 4, V2026A == 13 ~ 5,
                             V2026A == 15 ~ 6, V2026A %in% 16:18 ~ 7,
                             TRUE ~ NA_real_),
    shred_c      = case_when(VS349 == 1 ~ 1, VS349 == 2 ~ 0, TRUE ~ NA_real_),
    passwords_c  = case_when(VS345 == 1 ~ 1, VS345 == 2 ~ 0, TRUE ~ NA_real_),
    monitoring_c = case_when(VS347 == 1 ~ 1, VS347 == 2 ~ 0, TRUE ~ NA_real_),
    protection_c = case_when(VS355 == 1 ~ 1, VS355 == 2 ~ 0,
                             VS355 == 9 ~ 9, TRUE ~ NA_real_),
    software_c   = case_when(VS353 == 1 ~ 1, VS353 == 2 ~ 0,
                             VS353 == 9 ~ 9, TRUE ~ NA_real_),
    interact_c   = dich_citizen * hispanic
  )
# NOTE on protection_c/software_c: the original do-file recodes 8/98/99 to
# missing for VS353/VS355 but never touches code 9 for those two items, so
# a code-9 value (if present) survives into the model there; dv components,
# VS345/347/349, and the others do get 9 -> missing. Reproduced literally.

f_coop <- dv ~ dich_citizen + hispanic + interact_c + factor(race_may) +
  age_c + married_c + gender_c + education_c + income_c +
  shred_c + software_c + passwords_c + monitoring_c + protection_c

coop_cc <- coop %>% filter(complete.cases(coop[, all.vars(f_coop)]))
cat(sprintf("Cooper-rebuild complete-case N = %s (target: 71,984)\n",
    format(nrow(coop_cc), big.mark = ",")))

# Row A: unweighted MLE, model-based SEs (the original study's estimator)
fitA <- glm(f_coop, data = coop_cc, family = binomial("logit"))
# Row B: survey-weighted point estimation, model-based (naive) SEs
coop_cc$w_norm <- coop_cc$VS363 / mean(coop_cc$VS363)
fitB <- glm(f_coop, data = coop_cc, weights = w_norm,
            family = quasibinomial("logit"))
# Row C: survey-weighted with Taylor-linearized design covariance
des_coop <- svydesign(id = ~V2118, strata = ~V2117, weights = ~VS363,
                      data = coop_cc, nest = TRUE)
fitC <- svyglm(f_coop, design = des_coop, family = quasibinomial("logit"))

decomp <- data.frame()
for (nm in c("A", "B", "C")) {
  ft <- get(paste0("fit", nm))
  sm <- summary(ft)$coefficients
  est <- sm["interact_c", "Estimate"]; se <- sm["interact_c", 2]
  pv  <- sm["interact_c", ncol(sm)]
  decomp <- rbind(decomp, data.frame(
    row = c(A = "Unweighted, model-based SE",
            B = "Survey-weighted, model-based SE",
            C = "Survey-weighted, Taylor-linearized SE")[nm],
    OR = round(exp(est), 3), SE = round(se, 4), p = signif(pv, 3)))
}
cat("\nThree-row decomposition, Hispanic x Noncitizen interaction:\n")
print(decomp, row.names = FALSE)
cat("(Rows B and C must have identical ORs -- same weighted MLE,\n",
    "different covariance estimator.)\n", sep = "")
write.csv(decomp, file.path(OUT_DIR, "tab_reconcile_decomp.csv"),
          row.names = FALSE)

# ==========================================================================
cat("\n", SEP, "\nSECTION 4: PROTECTIVE-BEHAVIOR ADOPTION (SECONDARY; CORRECTED CODING)\n",
    SEP, "\n", sep = "")

BEH <- c("passwords", "monitoring", "protection", "software", "shred")
rates <- data.frame(); famB <- list(`2021` = data.frame(), `2018` = data.frame())
for (w in c("2021", "2018")) {
  des <- if (w == "2021") des21 else des18
  df  <- if (w == "2021") df21w else df18w
  for (b in BEH) {
    dom <- dom_expr(df, b)
    dsub <- subset(des, dom)
    rt <- svyby(as.formula(paste0("~", b)), ~citgrp, dsub, svymean,
                na.rm = TRUE)
    rates <- rbind(rates, data.frame(wave = w, behavior = b,
      group = rt$citgrp, rate = rt[[b]], se = rt$se))
    fit <- svyglm(as.formula(paste(b,
      "~ noncitizen + hispanic + hisp_noncitizen +", CTRL)),
      design = dsub, family = quasibinomial("logit"))
    sm <- summary(fit)$coefficients
    famB[[w]] <- rbind(famB[[w]], data.frame(behavior = b,
      or = exp(sm["hisp_noncitizen", "Estimate"]),
      p  = sm["hisp_noncitizen", ncol(sm)]))
  }
  famB[[w]]$p_holm <- p.adjust(famB[[w]]$p, "holm")
  cat(sprintf("\n%s adjusted interaction ORs (Family B, Holm across 5):\n", w))
  print(famB[[w]] %>% mutate(across(where(is.numeric), ~signif(., 4))),
        row.names = FALSE)
}
cat("\nWeighted adoption rates by group:\n")
print(rates %>% mutate(rate = round(100 * rate, 1), se = round(100 * se, 2)),
      row.names = FALSE)
# Adoption ratios (noncit_hisp vs each other group) -- replaces "30-50%"
cat("\nAdoption ratios: noncit_hisp rate / other-group rate:\n")
rw <- rates %>% group_by(wave, behavior) %>% summarise(
  vs_cit_nonhisp = round(rate[group == "noncit_hisp"] /
                         rate[group == "cit_nonhisp"], 2),
  vs_cit_hisp    = round(rate[group == "noncit_hisp"] /
                         rate[group == "cit_hisp"], 2),
  vs_noncit_nonhisp = round(rate[group == "noncit_hisp"] /
                            rate[group == "noncit_nonhisp"], 2),
  .groups = "drop")
print(as.data.frame(rw), row.names = FALSE)
write.csv(rates, file.path(OUT_DIR, "fig2_protective_rates.csv"),
          row.names = FALSE)

# ==========================================================================
cat("\n", SEP, "\nSECTION 5: SENSITIVITY ANALYSES (2021 PRIMARY MODELS)\n",
    SEP, "\n", sep = "")

sens_row <- function(fit, label) {
  sm <- summary(fit)$coefficients
  data.frame(spec = label,
    or = round(exp(sm["hisp_noncitizen", "Estimate"]), 3),
    p  = signif(sm["hisp_noncitizen", ncol(sm)], 3),
    n  = nrow(model.frame(fit)))
}
sens <- data.frame()
dom_v <- dom_expr(df21w, "any_idtheft")
d21v  <- subset(des21, dom_v)
sens <- rbind(sens, sens_row(svyglm(as.formula(paste(
  "any_idtheft ~ noncitizen + hispanic + hisp_noncitizen +", CTRL)),
  design = d21v, family = quasibinomial("logit")), "Baseline (income as rank)"))
CTRL_CAT <- sub("income", "inc_band", CTRL)
sens <- rbind(sens, sens_row(svyglm(as.formula(paste(
  "any_idtheft ~ noncitizen + hispanic + hisp_noncitizen +", CTRL_CAT)),
  design = d21v, family = quasibinomial("logit")), "Income as 8 categories"))
sens <- rbind(sens, sens_row(svyglm(as.formula(paste(
  "any_idtheft ~ noncitizen + hispanic + hisp_noncitizen + I(age^2) +", CTRL)),
  design = d21v, family = quasibinomial("logit")), "Age quadratic"))
dom_p <- dom_v & !is.na(df21w$prior_victim)
sens <- rbind(sens, sens_row(svyglm(as.formula(paste(
  "any_idtheft ~ noncitizen + hispanic + hisp_noncitizen + prior_victim +", CTRL)),
  design = subset(des21, dom_p), family = quasibinomial("logit")),
  "Prior victimization control"))
dom_e <- dom_v & (df21w$VS010A == 1 | df21w$VS016A == 1) &
         !(is.na(df21w$VS010A) & is.na(df21w$VS016A))
dom_e[is.na(dom_e)] <- FALSE
sens <- rbind(sens, sens_row(svyglm(as.formula(paste(
  "any_idtheft ~ noncitizen + hispanic + hisp_noncitizen +", CTRL)),
  design = subset(des21, dom_e), family = quasibinomial("logit")),
  "Ever had bank acct or credit card"))

# Same battery for the PRIMARY contact outcome
dom_cv <- dom_expr(df21w, "any_contact") & vic_dom21
d21c   <- subset(des21, dom_cv)
sens <- rbind(sens, sens_row(svyglm(as.formula(paste(
  "any_contact ~ noncitizen + hispanic + hisp_noncitizen +", CTRL)),
  design = d21c, family = quasibinomial("logit")),
  "CONTACT: baseline"))
sens <- rbind(sens, sens_row(svyglm(as.formula(paste(
  "any_contact ~ noncitizen + hispanic + hisp_noncitizen +", CTRL_CAT)),
  design = d21c, family = quasibinomial("logit")),
  "CONTACT: income categorical"))
sens <- rbind(sens, sens_row(svyglm(as.formula(paste(
  "any_contact ~ noncitizen + hispanic + hisp_noncitizen + I(age^2) +", CTRL)),
  design = d21c, family = quasibinomial("logit")),
  "CONTACT: age quadratic"))
dom_cp <- dom_cv & !is.na(df21w$prior_victim)
sens <- rbind(sens, sens_row(svyglm(as.formula(paste(
  "any_contact ~ noncitizen + hispanic + hisp_noncitizen + prior_victim +", CTRL)),
  design = subset(des21, dom_cp), family = quasibinomial("logit")),
  "CONTACT: prior victimization"))

cat("\nHispanic x Noncitizen interaction across specifications:\n")
print(sens, row.names = FALSE)
write.csv(sens, file.path(OUT_DIR, "tab_sensitivity.csv"), row.names = FALSE)

cat("\n", SEP, "\nCOMPLETE. CSV outputs in: ", normalizePath(OUT_DIR), "\n",
    SEP, "\n", sep = "")
