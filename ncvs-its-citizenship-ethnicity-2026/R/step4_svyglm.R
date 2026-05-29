# ==========================================================================
# NCVS-ITS Replication/Extension -- Step 4: Design-correct SEs via svyglm
# ==========================================================================
# Produces design-corrected OR tables for Tables II-IV using Taylor-series
# linearization with NCVS pseudostrata (V2117) and half-sample (V2118).
#
# Requirements:
#   install.packages(c("haven", "survey", "dplyr"))
#
# Usage:
#   Rscript step4_svyglm.R
#   (run from the folder containing both .dta files)
#
# Output: paste the full console output back to Claude for table updates.
# ==========================================================================

options(survey.lonely.psu = "adjust")
library(haven)
library(survey)
library(dplyr)

cat("Loading raw data...\n")
df18_raw <- read_dta("37923-0001-Data.dta")
df21_raw <- read_dta("38501-0001-Data.dta")

# --------------------------------------------------------------------------
# Build analytic sample (mirrors step2_build_sample.py)
# --------------------------------------------------------------------------
build_analytic <- function(df) {
  df <- df %>%
    filter(VS008 %in% c(1, 2)) %>%
    filter(V3014 >= 16) %>%
    filter(!(V3083 %in% c(8, 9, -1))) %>%
    filter(!(V3024 %in% c(8, -1))) %>%
    filter(!(V3086 %in% c(3, 4))) %>%
    mutate(
      noncitizen      = as.integer(V3083 == 5),
      hispanic        = as.integer(V3024 == 1),
      hisp_noncitizen = as.integer(V3083 == 5 & V3024 == 1),
      # Outcome components: 9 = out of scope among completers = 0 (not victim)
      c012 = case_when(VS012==1~1, VS012%in%c(2,9)~0, TRUE~NA_real_),
      c017 = case_when(VS017==1~1, VS017%in%c(2,9)~0, TRUE~NA_real_),
      c019 = case_when(VS019==1~1, VS019%in%c(2,9)~0, TRUE~NA_real_),
      c041 = case_when(VS041==1~1, VS041%in%c(2,9)~0, TRUE~NA_real_),
      c063 = case_when(VS063==1~1, VS063%in%c(2,9)~0, TRUE~NA_real_)
    ) %>%
    mutate(
      any_idtheft   = pmax(c012, c017, c019, c041, c063, na.rm=FALSE),
      existing_acct = pmax(c012, c017, c019, na.rm=FALSE),
      new_acct      = c041,
      other_idtheft = c063,
      # Controls
      age          = ifelse(V3014 == -1, NA_real_, as.numeric(V3014)),
      male         = as.integer(V3018 == 1),
      race_black   = as.integer(V3023A == 2),
      race_other   = as.integer(V3023A >= 3 & V3023A <= 20),
      edu_lths     = as.integer(V3020 <= 11),
      edu_hs       = as.integer(V3020 %in% c(27, 28)),
      edu_some     = as.integer(V3020 %in% c(40, 41)),
      married      = as.integer(V3015 == 1),
      # Corrected income recode (codes 8,9 are valid income bands)
      income       = case_when(V2026 %in% c(98,99)~NA_real_,
                               TRUE~as.numeric(V2026)),
      msa_suburban = as.integer(V2129 == 2),
      msa_central  = as.integer(V2129 == 3),
      region_mw    = as.integer(V2127B == 2),
      region_south = as.integer(V2127B == 3),
      region_west  = as.integer(V2127B == 4)
    ) %>%
    filter(!is.na(any_idtheft), !is.na(noncitizen),
           !is.na(hispanic), !is.na(age), !is.na(male))
  return(df)
}

cat("Building analytic samples...\n")
df18 <- build_analytic(df18_raw)
df21 <- build_analytic(df21_raw)
cat(sprintf("  2018: N=%s  victims=%s (%.2f%%)\n",
    format(nrow(df18), big.mark=","),
    format(sum(df18$any_idtheft, na.rm=TRUE), big.mark=","),
    mean(df18$any_idtheft, na.rm=TRUE)*100))
cat(sprintf("  2021: N=%s  victims=%s (%.2f%%)\n",
    format(nrow(df21), big.mark=","),
    format(sum(df21$any_idtheft, na.rm=TRUE), big.mark=","),
    mean(df21$any_idtheft, na.rm=TRUE)*100))

# --------------------------------------------------------------------------
# Declare survey designs
# --------------------------------------------------------------------------
svy18 <- svydesign(id=~V2118, strata=~V2117, weights=~VS363,
                   data=df18, nest=TRUE)
svy21 <- svydesign(id=~V2118, strata=~V2117, weights=~VS363,
                   data=df21, nest=TRUE)

# Pooled design
df_pooled <- bind_rows(
  mutate(df18, year_2021=0L),
  mutate(df21, year_2021=1L)
) %>% mutate(
  year_x_noncitizen  = year_2021 * noncitizen,
  year_x_hispanic    = year_2021 * hispanic,
  year_x_hisp_noncit = year_2021 * hisp_noncitizen
)
svy_pooled <- svydesign(id=~V2118, strata=~V2117, weights=~VS363,
                        data=df_pooled, nest=TRUE)

# --------------------------------------------------------------------------
# Formulas
# --------------------------------------------------------------------------
CTRL <- paste("age + male + race_black + race_other +",
              "edu_lths + edu_hs + edu_some +",
              "married + income +",
              "msa_suburban + msa_central +",
              "region_mw + region_south + region_west")

f_main <- as.formula(paste(
  "any_idtheft ~ noncitizen + hispanic + hisp_noncitizen +", CTRL))

f_pool <- as.formula(paste(
  "any_idtheft ~ year_2021 +",
  "noncitizen + hispanic + hisp_noncitizen +",
  "year_x_noncitizen + year_x_hispanic + year_x_hisp_noncit +", CTRL))

# --------------------------------------------------------------------------
# Helper: print OR table for selected rows
# --------------------------------------------------------------------------
SEP <- paste(rep("=", 68), collapse="")

print_ors <- function(fit, label, rows) {
  cat(sprintf("\n--- %s ---\n", label))
  co  <- coef(fit)[rows]
  se  <- sqrt(diag(vcov(fit)))[rows]
  pv  <- 2 * pnorm(-abs(co / se))
  or  <- exp(co)
  cil <- exp(co - 1.96 * se)
  cih <- exp(co + 1.96 * se)
  sig <- ifelse(pv<.001,"***", ifelse(pv<.01,"**",
               ifelse(pv<.05,"*",  ifelse(pv<.1, ".", "ns"))))
  res <- data.frame(Variable=rows, OR=round(or,4),
                    CI_low=round(cil,4), CI_high=round(cih,4),
                    p_value=round(pv,4), sig=sig, row.names=NULL)
  print(res, row.names=FALSE)
  invisible(res)
}

KEY  <- c("noncitizen","hispanic","hisp_noncitizen")
RACE <- c("race_black","race_other")
POOL <- c("year_2021","noncitizen","hispanic","hisp_noncitizen",
          "year_x_noncitizen","year_x_hispanic","year_x_hisp_noncit")

# --------------------------------------------------------------------------
cat("\n", SEP, "\n")
cat("TABLE 2: svyglm -- 2018 and 2021 full models\n")
cat(SEP, "\n")

fit18 <- svyglm(f_main, design=svy18, family=quasibinomial("logit"))
fit21 <- svyglm(f_main, design=svy21, family=quasibinomial("logit"))

print_ors(fit18, "2018 full model", c(KEY, RACE))
print_ors(fit21, "2021 full model", c(KEY, RACE))

# --------------------------------------------------------------------------
cat("\n", SEP, "\n")
cat("TABLE 3: svyglm -- pooled year-interaction model\n")
cat(SEP, "\n")

fit_pool <- svyglm(f_pool, design=svy_pooled, family=quasibinomial("logit"))
print_ors(fit_pool, "Pooled 2018+2021", POOL)

cat("\nWald test (joint: year_x_noncitizen, year_x_hispanic, year_x_hisp_noncit):\n")
wald <- regTermTest(fit_pool,
                    c("year_x_noncitizen",
                      "year_x_hispanic",
                      "year_x_hisp_noncit"),
                    method = "Wald")
print(wald)

# --------------------------------------------------------------------------
cat("\n", SEP, "\n")
cat("TABLE 4: svyglm -- subtype analysis, 2021\n")
cat(SEP, "\n")

for (outcome in c("existing_acct", "new_acct", "other_idtheft")) {
  f_sub <- as.formula(paste(
    outcome, "~ noncitizen + hispanic + hisp_noncitizen +", CTRL))
  fit_sub <- svyglm(f_sub, design=svy21, family=quasibinomial("logit"))
  print_ors(fit_sub, sprintf("2021 %s", outcome), c(KEY, RACE))
}

cat("\n", SEP, "\n")
cat("COMPLETE. Paste this full output for table updates.\n")
cat(SEP, "\n")
