# ==========================================================================
# NCVS-ITS -- Step 11 (scope checks): "Out of scope" (code 9) audit +
# visibility benchmark
# ==========================================================================
# Two jobs:
#
# (A) SCOPE AUDIT. In the NCVS-ITS codebooks, screener code 9 is labeled
#     "Out of Scope", NOT "non-victim". The corrected samples rest on the
#     claim that among ITS COMPLETERS, code 9 is a legitimate negative
#     rather than missing data (the original study treated it as missing).
#     That claim must be verified against the instrument's actual skip
#     structure, not asserted. This script cross-tabulates each
#     outcome screener's code 9 against its gate question to establish
#     exactly why those respondents were routed past the item.
#     It also checks whether "out of scope" rates on the CONTACT items
#     and the PROTECTIVE-PRACTICE items differ by citizenship/ethnicity
#     group -- if they do, a composite built across those items is not
#     the same measure for every group, which would be a real confound.
#
# (B) VISIBILITY BENCHMARK (supplementary). The visibility rate
#     nu_a = rho_a x kappa_a is estimated DIRECTLY as a single survey mean
#     of the indicator (victimized AND contacted), rather than by
#     multiplying two covariate-adjusted margins, which gives a correct
#     design-based SE with no delta-method assumption. Also reported:
#     unadjusted rho_a and kappa_a, pairwise contrasts, and the
#     decomposition of the label deficit into its recognition-stage and
#     contact-stage shares.
#
# Usage: NCVS_DATA_DIR=/path/to/data Rscript step11_scope_checks.R
# ==========================================================================

options(survey.lonely.psu = "adjust")
suppressMessages({
  library(haven); library(survey); library(dplyr)
})

DATA_DIR <- Sys.getenv("NCVS_DATA_DIR", unset = ".")
OUT_DIR  <- Sys.getenv("NCVS_OUT_DIR",  unset = "out_ccnc")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)
SEP <- paste(rep("=", 70), collapse = "")

f18 <- file.path(DATA_DIR, "37923-0001-Data.dta")
f21 <- file.path(DATA_DIR, "38501-0001-Data.dta")

# --------------------------------------------------------------------------
# 0. Header scan: find the "ever" gate variables and confirm code labels
# --------------------------------------------------------------------------
cat(SEP, "\n0. GATE-VARIABLE DISCOVERY AND CODE LABELS\n", SEP, "\n", sep = "")
for (nm in c("2018", "2021")) {
  hdr <- read_dta(if (nm == "2018") f18 else f21, n_max = 0)
  labs <- vapply(names(hdr), function(n) {
    l <- attr(hdr[[n]], "label"); if (is.null(l)) "" else l }, character(1))
  cat("\n--- ", nm, ": candidate gates / screeners ---\n", sep = "")
  keep <- grep("^VS0(1|3|4|6)", names(hdr))
  keep <- keep[grepl("EVER|MISUSE|HAVE YOU|ACCOUNT", toupper(labs[keep]))]
  for (i in head(keep, 40)) cat(sprintf("  %-9s %s\n", names(hdr)[i],
                                        substr(labs[i], 1, 66)))
  # Value labels for code 9 on the outcome screeners
  for (v in c("VS012", "VS017", "VS019", "VS041", "VS063")) {
    if (!v %in% names(hdr)) next
    lb <- attr(hdr[[v]], "labels")
    if (!is.null(lb)) cat(sprintf("  %s value labels: %s\n", v,
        paste(sprintf("%s=%s", lb, names(lb)), collapse = "; ")))
  }
}

# --------------------------------------------------------------------------
# 1. Load analysis columns
# --------------------------------------------------------------------------
COMMON <- c("VS008", "V3014", "V3083", "V3024", "VS363", "V2117", "V2118",
            "VS012", "VS017", "VS019", "VS041", "VS063",
            "VS098", "VS100", "VS133", "VS135",
            "VS345", "VS347", "VS349", "VS353", "VS355")
G21 <- c("VS010A", "VS016A", "VS016B", "VS017K", "VS033K", "VS017C", "VS017E")
G18 <- c("VS010", "VS014", "VS016")

pick <- function(f, want) {
  hdr <- read_dta(f, n_max = 0)
  want[want %in% names(hdr)]
}
c18 <- pick(f18, c(COMMON, G18))
c21 <- pick(f21, c(COMMON, G21))
cat("\nLoading data...\n")
d18 <- zap_labels(read_dta(f18, col_select = all_of(c18)))
d21 <- zap_labels(read_dta(f21, col_select = all_of(c21)))

prep <- function(df) {
  df %>% mutate(
    completer = as.integer(VS008 %in% c(1, 2)),
    noncitizen = case_when(V3083 == 5 ~ 1L, V3083 %in% 1:4 ~ 0L, TRUE ~ NA_integer_),
    hispanic   = case_when(V3024 == 1 ~ 1L, V3024 == 2 ~ 0L, TRUE ~ NA_integer_),
    citgrp = factor(case_when(
      noncitizen == 0L & hispanic == 0L ~ "cit_nonhisp",
      noncitizen == 0L & hispanic == 1L ~ "cit_hisp",
      noncitizen == 1L & hispanic == 0L ~ "noncit_nonhisp",
      noncitizen == 1L & hispanic == 1L ~ "noncit_hisp"),
      levels = c("cit_nonhisp", "cit_hisp", "noncit_nonhisp", "noncit_hisp")),
    age = ifelse(V3014 %in% c(-1, 99), NA_real_, as.numeric(V3014)),
    c012 = case_when(VS012 == 1 ~ 1, VS012 %in% c(2, 9) ~ 0, TRUE ~ NA_real_),
    c017 = case_when(VS017 == 1 ~ 1, VS017 %in% c(2, 9) ~ 0, TRUE ~ NA_real_),
    c019 = case_when(VS019 == 1 ~ 1, VS019 %in% c(2, 9) ~ 0, TRUE ~ NA_real_),
    c041 = case_when(VS041 == 1 ~ 1, VS041 %in% c(2, 9) ~ 0, TRUE ~ NA_real_),
    c063 = case_when(VS063 == 1 ~ 1, VS063 %in% c(2, 9) ~ 0, TRUE ~ NA_real_),
    any_idtheft = pmax(c012, c017, c019, c041, c063, na.rm = FALSE),
    yn98 = case_when(VS098 == 1 ~ 1, VS098 == 2 ~ 0, TRUE ~ NA_real_),
    yn00 = case_when(VS100 == 1 ~ 1, VS100 == 2 ~ 0, TRUE ~ NA_real_),
    yn33 = case_when(VS133 == 1 ~ 1, VS133 == 2 ~ 0, TRUE ~ NA_real_),
    any_contact = case_when(
      yn98 == 1 | yn00 == 1 | yn33 == 1 ~ 1,
      !is.na(yn98) | !is.na(yn00) | !is.na(yn33) ~ 0, TRUE ~ NA_real_),
    # Sensitivity: only the two near-universally administered items
    contact_2 = case_when(
      yn98 == 1 | yn33 == 1 ~ 1,
      !is.na(yn98) | !is.na(yn33) ~ 0, TRUE ~ NA_real_)
  )
}
d18 <- prep(d18); d21 <- prep(d21)

# --------------------------------------------------------------------------
# 2. SCOPE AUDIT: what does code 9 mean among completers?
# --------------------------------------------------------------------------
cat("\n", SEP, "\n2. OUTCOME-SCREENER CODE 9 AMONG ITS COMPLETERS\n", SEP, "\n", sep = "")
SCREEN <- c("VS012", "VS017", "VS019", "VS041", "VS063")
for (nm in c("2021", "2018")) {
  df <- (if (nm == "2021") d21 else d18) %>% filter(completer == 1L)
  cat(sprintf("\n--- %s completers (n=%s) ---\n", nm,
              format(nrow(df), big.mark = ",")))
  for (v in SCREEN) {
    tb <- table(df[[v]], useNA = "ifany")
    cat(sprintf("  %-6s %s\n", v,
        paste(sprintf("%s=%s", names(tb), format(as.integer(tb), big.mark = ",")),
              collapse = "  ")))
  }
}

cat("\n--- 2021: code 9 cross-tabulated against its gate question ---\n")
gate_check <- function(df, item, gate, gate_label) {
  if (!all(c(item, gate) %in% names(df))) {
    cat(sprintf("  %s vs %s: gate not in file\n", item, gate)); return(invisible())
  }
  s <- df %>% filter(completer == 1L)
  tb <- table(item9 = s[[item]] == 9, gate = s[[gate]], useNA = "ifany")
  cat(sprintf("\n  %s == 9  BY  %s (%s)\n", item, gate, gate_label))
  print(tb)
}
gate_check(d21, "VS017", "VS016A", "ever had a credit card account")
gate_check(d21, "VS017", "VS016B", "credit card ever misused")
gate_check(d21, "VS012", "VS010A", "ever had checking/savings account")
gate_check(d21, "VS019", "VS017K", "other existing accounts ever misused")
gate_check(d21, "VS041", "VS033K", "new accounts ever fraudulently created")
cat("\n--- 2018: code 9 cross-tabulated against its gate question ---\n")
gate_check(d18, "VS017", "VS014", "currently has >=1 credit card")
gate_check(d18, "VS017", "VS016", "had a credit card in the past year")
gate_check(d18, "VS012", "VS010", "had a bank account in past 12 months")

# --------------------------------------------------------------------------
# 3. Is "out of scope" itself group-correlated? (measure-comparability risk)
# --------------------------------------------------------------------------
cat("\n", SEP, "\n3. OUT-OF-SCOPE RATES BY GROUP\n", SEP, "\n", sep = "")
oos_by_group <- function(df, vars, base_label, victims_only = FALSE) {
  s <- df %>% filter(completer == 1L, !is.na(citgrp))
  if (victims_only) s <- s %>% filter(any_idtheft == 1)
  cat(sprintf("\n--- %s (denominator: %s) ---\n", base_label,
              format(nrow(s), big.mark = ",")))
  for (v in vars) {
    if (!v %in% names(s)) next
    t <- s %>% group_by(citgrp) %>%
      summarise(n = n(),
                oos9 = sum(.data[[v]] == 9, na.rm = TRUE),
                pct9 = round(100 * oos9 / n, 1),
                other_miss = sum(.data[[v]] %in% c(8, 98, 99), na.rm = TRUE),
                .groups = "drop")
    cat(sprintf("  %s:\n", v))
    print(as.data.frame(t), row.names = FALSE)
  }
}
oos_by_group(d21, c("VS098", "VS100", "VS133"), "2021 contact items, victims only",
             victims_only = TRUE)
oos_by_group(d18, c("VS098", "VS100", "VS133"), "2018 contact items, victims only",
             victims_only = TRUE)
oos_by_group(d21, c("VS345", "VS347", "VS349", "VS353", "VS355"),
             "2021 protective practices, all completers")
oos_by_group(d18, c("VS345", "VS347", "VS349", "VS353", "VS355"),
             "2018 protective practices, all completers")

# --------------------------------------------------------------------------
# 4. VISIBILITY BENCHMARK: direct estimation of nu_a, rho_a, kappa_a
# --------------------------------------------------------------------------
cat("\n", SEP, "\n4. VISIBILITY BENCHMARK (DIRECT, UNADJUSTED, DESIGN-BASED)\n",
    SEP, "\n", sep = "")

bench <- function(df, wave) {
  df <- df %>% mutate(
    nu = case_when(any_idtheft == 0 ~ 0,
                   any_idtheft == 1 & any_contact == 1 ~ 1,
                   any_idtheft == 1 & any_contact == 0 ~ 0,
                   TRUE ~ NA_real_))
  dw <- df %>% filter(!is.na(VS363) & VS363 > 0)
  des <- svydesign(id = ~V2118, strata = ~V2117, weights = ~VS363,
                   data = dw, nest = TRUE)
  dom <- dw$completer == 1L & !is.na(dw$citgrp) & !is.na(dw$age) & dw$age >= 16
  d_all <- subset(des, dom & !is.na(dw$any_idtheft))
  d_vic <- subset(des, dom & !is.na(dw$any_idtheft) & dw$any_idtheft == 1)

  rho <- svyby(~any_idtheft, ~citgrp, d_all, svymean, na.rm = TRUE)
  kap <- svyby(~any_contact, ~citgrp, d_vic, svymean, na.rm = TRUE)
  nu  <- svyby(~nu, ~citgrp, d_all, svymean, na.rm = TRUE, covmat = TRUE)
  k2  <- svyby(~contact_2, ~citgrp, d_vic, svymean, na.rm = TRUE)

  cat(sprintf("\n### %s: unadjusted weighted rates (%%), design SEs\n", wave))
  out <- data.frame(
    group  = rho$citgrp,
    rho    = round(100 * coef(rho), 2), rho_se = round(100 * SE(rho), 2),
    kappa  = round(100 * coef(kap), 2), kap_se = round(100 * SE(kap), 2),
    nu     = round(100 * coef(nu),  3), nu_se  = round(100 * SE(nu),  3),
    kappa2 = round(100 * coef(k2),  2))
  print(out, row.names = FALSE)
  cat("  (kappa2 = contact using only the two universally administered\n",
      "   items VS098|VS133, excluding the skip-heavy credit-bureau item)\n",
      sep = "")
  cat(sprintf("  check nu ~= rho*kappa: %s\n",
      paste(round(100 * coef(rho) * coef(kap) / 100, 3), collapse = ", ")))

  # Pairwise contrasts on nu, with the design covariance
  V <- vcov(nu); e <- coef(nu); lev <- as.character(rho$citgrp)
  ddf <- degf(d_all)
  cat("\n  Pairwise differences in nu (percentage points):\n")
  ct <- data.frame()
  for (i in 1:(length(lev) - 1)) for (j in (i + 1):length(lev)) {
    d <- e[j] - e[i]
    se <- sqrt(V[i, i] + V[j, j] - 2 * V[i, j])
    p <- 2 * pt(-abs(d / se), ddf)
    ct <- rbind(ct, data.frame(
      contrast = paste(lev[j], "-", lev[i]),
      diff_pp = round(100 * d, 3), se_pp = round(100 * se, 3),
      ci_lo = round(100 * (d - qt(.975, ddf) * se), 3),
      ci_hi = round(100 * (d + qt(.975, ddf) * se), 3),
      p = signif(p, 3)))
  }
  ct$p_holm <- signif(p.adjust(ct$p, "holm"), 3)
  print(ct, row.names = FALSE)

  # Decomposition of the label-generation deficit (ref = cit_nonhisp)
  cat("\n  Deficit decomposition, each group vs. citizen non-Hispanic:\n")
  r <- coef(rho); k <- coef(kap); n_ <- coef(nu)
  dec <- data.frame(
    group = lev,
    nu_ratio    = round(n_ / n_[1], 3),
    rho_ratio   = round(r / r[1], 3),
    kappa_ratio = round(k / k[1], 3),
    pct_from_recognition = round(100 * log(r / r[1]) /
                                 (log(r / r[1]) + log(k / k[1])), 1),
    pct_from_contact     = round(100 * log(k / k[1]) /
                                 (log(r / r[1]) + log(k / k[1])), 1))
  print(dec, row.names = FALSE)
  write.csv(cbind(wave = wave, out), file.path(
    OUT_DIR, sprintf("tab_visibility_%s.csv", wave)), row.names = FALSE)
  invisible(list(out = out, ct = ct, dec = dec))
}
b21 <- bench(d21, "2021")
b18 <- bench(d18, "2018")

# --------------------------------------------------------------------------
# 5. Sensitivity: adjusted contact model on the 2-item composite
# --------------------------------------------------------------------------
cat("\n", SEP, "\n5. ADJUSTED CONTACT MODEL, TWO-ITEM COMPOSITE (SENSITIVITY)\n",
    SEP, "\n", sep = "")
cat("Run in step10 with outcome contact_2 if this differs materially from\n",
    "the any_contact result; reported here as unadjusted rates above.\n", sep = "")

cat("\n", SEP, "\nCOMPLETE.\n", SEP, "\n", sep = "")
