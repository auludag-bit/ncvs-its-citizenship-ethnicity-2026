# ==========================================================================
# Step 8: Sensitivity Analysis Decomposition (Response to Reviewer 13)
# ==========================================================================
# The sensitivity analysis (step 3b) showed the Hispanic x Noncitizen
# interaction shift from OR=0.85 (ns) to OR=0.72 (p=.052) when employment
# sector dummies are added with listwise deletion. This script decomposes
# whether the shift is driven by:
#   (A) Adding employment as a control variable, OR
#   (B) Restricting to the employed subsample (listwise deletion effect)
#
# Three intermediate models:
#   M1: Primary model (no employment, full income-complete sample) = baseline
#   M2: Same predictors + employment dummies, full income-complete sample
#       (employment dummies added but NAs NOT listwise-deleted)
#       -> isolates the effect of adding employment as a control
#   M3: Primary predictors only, restricted to employment-complete subsample
#       (listwise on employment but no employment dummies)
#       -> isolates the sample restriction effect
#   M4: Full sensitivity model (employment dummies + listwise deletion)
#       = the step 3b result
#
# V3075 coding (per codebook):
#   1 = Private company, 2 = Federal govt,
#   3 = State/county/local govt, 4 = Self-employed
#   9 = Out of universe (not employed) -> NA
# ==========================================================================

options(survey.lonely.psu = "adjust")
library(haven); library(survey); library(dplyr)

cat("Loading data...\n")
df21_raw <- read_dta("38501-0001-Data.dta")

build_analytic <- function(df) {
  df %>%
    filter(VS008 %in% c(1,2), V3014>=16,
           !(V3083 %in% c(8,9,-1)), !(V3024 %in% c(8,-1)),
           !(V3086 %in% c(3,4))) %>%
    mutate(
      noncitizen=as.integer(V3083==5),
      hispanic=as.integer(V3024==1),
      hisp_noncitizen=as.integer(V3083==5 & V3024==1),
      c012=case_when(VS012==1~1,VS012%in%c(2,9)~0,TRUE~NA_real_),
      c017=case_when(VS017==1~1,VS017%in%c(2,9)~0,TRUE~NA_real_),
      c019=case_when(VS019==1~1,VS019%in%c(2,9)~0,TRUE~NA_real_),
      c041=case_when(VS041==1~1,VS041%in%c(2,9)~0,TRUE~NA_real_),
      c063=case_when(VS063==1~1,VS063%in%c(2,9)~0,TRUE~NA_real_)
    ) %>%
    mutate(
      any_idtheft=pmax(c012,c017,c019,c041,c063,na.rm=FALSE),
      age=ifelse(V3014==-1,NA_real_,as.numeric(V3014)),
      male=as.integer(V3018==1),
      race_black=as.integer(V3023A==2),
      race_other=as.integer(V3023A>=3&V3023A<=20),
      edu_lths=as.integer(V3020<=11),
      edu_hs=as.integer(V3020%in%c(27,28)),
      edu_some=as.integer(V3020%in%c(40,41)),
      married=as.integer(V3015==1),
      income=case_when(V2026%in%c(98,99)~NA_real_,TRUE~as.numeric(V2026)),
      msa_suburban=as.integer(V2129==2), msa_central=as.integer(V2129==3),
      region_mw=as.integer(V2127B==2), region_south=as.integer(V2127B==3),
      region_west=as.integer(V2127B==4),
      # Employment sector (corrected coding per codebook)
      employ_raw=case_when(V3075 %in% c(8,9,-1)~NA_real_,
                           TRUE~as.numeric(V3075)),
      emp_federal  =as.integer(employ_raw==2),
      emp_state_loc=as.integer(employ_raw==3),
      emp_self     =as.integer(employ_raw==4)
    ) %>%
    # Set employment dummies to NA when employ_raw is NA
    mutate(
      emp_federal  =ifelse(is.na(employ_raw),NA_real_,emp_federal),
      emp_state_loc=ifelse(is.na(employ_raw),NA_real_,emp_state_loc),
      emp_self     =ifelse(is.na(employ_raw),NA_real_,emp_self)
    ) %>%
    filter(!is.na(any_idtheft),!is.na(noncitizen),
           !is.na(hispanic),!is.na(age),!is.na(male))
}

df21 <- build_analytic(df21_raw)
svy21 <- svydesign(id=~V2118,strata=~V2117,weights=~VS363,
                   data=df21,nest=TRUE)

SEP <- paste(rep("=",65),collapse="")

# Employment missing pattern
cat(sprintf("\nEmployment missing in analytic sample: %s of %s (%.1f%%)\n",
    sum(is.na(df21$employ_raw)), nrow(df21),
    mean(is.na(df21$employ_raw))*100))
cat("Employment distribution (V3075):\n")
print(table(df21$employ_raw, useNA="always"))

# Base controls (no employment)
CTRL_BASE <- paste("age+male+race_black+race_other+edu_lths+edu_hs+edu_some+",
                   "married+income+msa_suburban+msa_central+",
                   "region_mw+region_south+region_west")
CTRL_EMPL <- paste(CTRL_BASE,
                   "+emp_federal+emp_state_loc+emp_self")

get_interaction <- function(fit, label) {
  mf  <- model.frame(fit)
  n   <- nrow(mf)
  co  <- coef(fit)["hisp_noncitizen"]
  se  <- sqrt(diag(vcov(fit)))["hisp_noncitizen"]
  pv  <- 2*pnorm(-abs(co/se))
  or  <- exp(co)
  cil <- exp(co-1.96*se); cih <- exp(co+1.96*se)
  sig <- ifelse(pv<.001,"***",ifelse(pv<.01,"**",
               ifelse(pv<.05,"*",ifelse(pv<.1,".","ns"))))
  cat(sprintf("  %-45s N=%6s  OR=%.3f [%.3f,%.3f]  p=%.3f %s\n",
      label, format(n,big.mark=","), or, cil, cih, pv, sig))
  invisible(list(n=n,or=or,cil=cil,cih=cih,p=pv,sig=sig))
}

cat("\n",SEP,"\n")
cat("DECOMPOSITION: What drives OR shift from 0.85 to 0.72?\n")
cat(SEP,"\n\n")

# M1: Primary model (baseline -- replicate paper Table II Model 4)
f_m1 <- as.formula(paste("any_idtheft~noncitizen+hispanic+hisp_noncitizen+",
                          CTRL_BASE))
fit_m1 <- svyglm(f_m1, design=svy21, family=quasibinomial("logit"))
r1 <- get_interaction(fit_m1,
  "M1: Primary model (no employment)")

# M2: Add employment dummies, keep full income-complete sample
# (employment NAs set to 0 -- i.e., treat missing=not employed)
# This is WRONG methodologically but lets us isolate the covariate effect
# Better: use a "not employed" category for NAs
df21_m2 <- df21 %>%
  mutate(
    emp_federal   = ifelse(is.na(emp_federal),   0L, emp_federal),
    emp_state_loc = ifelse(is.na(emp_state_loc), 0L, emp_state_loc),
    emp_self      = ifelse(is.na(emp_self),      0L, emp_self)
  )
svy21_m2 <- svydesign(id=~V2118,strata=~V2117,weights=~VS363,
                       data=df21_m2,nest=TRUE)
f_m2 <- as.formula(paste("any_idtheft~noncitizen+hispanic+hisp_noncitizen+",
                          CTRL_EMPL))
fit_m2 <- svyglm(f_m2, design=svy21_m2, family=quasibinomial("logit"))
r2 <- get_interaction(fit_m2,
  "M2: +employment dummies (NAs=not employed, no listwise)")

# M3: Restrict to employment-complete sample, no employment dummies
# Isolates sample restriction effect only
svy21_m3 <- subset(svy21, !is.na(employ_raw))
f_m3 <- as.formula(paste("any_idtheft~noncitizen+hispanic+hisp_noncitizen+",
                          CTRL_BASE))
fit_m3 <- svyglm(f_m3, design=svy21_m3, family=quasibinomial("logit"))
r3 <- get_interaction(fit_m3,
  "M3: Employed subsample only, no employment dummies")

# M4: Full sensitivity model (employment dummies + listwise) = step 3b result
f_m4 <- as.formula(paste("any_idtheft~noncitizen+hispanic+hisp_noncitizen+",
                          CTRL_EMPL))
fit_m4 <- svyglm(f_m4, design=svy21, family=quasibinomial("logit"))
r4 <- get_interaction(fit_m4,
  "M4: Full sensitivity (employment dummies + listwise)")

cat("\n\nInterpretation:\n")
cat(sprintf("  M1->M2 shift (adding employment, same sample): OR %.3f->%.3f\n",
    r1$or, r2$or))
cat(sprintf("  M1->M3 shift (restricting sample, no employment): OR %.3f->%.3f\n",
    r1$or, r3$or))
cat(sprintf("  M3->M4 shift (adding employment within subsample): OR %.3f->%.3f\n",
    r3$or, r4$or))

# Employment sector coefficients for context
cat("\nEmployment sector ORs in M4 (ref=private company):\n")
for (v in c("emp_federal","emp_state_loc","emp_self")) {
  co  <- coef(fit_m4)[v]
  se  <- sqrt(diag(vcov(fit_m4)))[v]
  pv  <- 2*pnorm(-abs(co/se))
  cat(sprintf("  %-15s: OR=%.3f  p=%.3f %s\n",
      v, exp(co), pv,
      ifelse(pv<.05,"*",ifelse(pv<.1,".","ns"))))
}

# Full coefficient table for M4 (supplementary table)
cat("\n",SEP,"\n")
cat("FULL SENSITIVITY MODEL TABLE (M4) -- for supplement\n")
cat(SEP,"\n")
co  <- coef(fit_m4)
se  <- sqrt(diag(vcov(fit_m4)))
pv  <- 2*pnorm(-abs(co/se))
or  <- exp(co); cil <- exp(co-1.96*se); cih <- exp(co+1.96*se)
sig <- ifelse(pv<.001,"***",ifelse(pv<.01,"**",
             ifelse(pv<.05,"*",ifelse(pv<.1,".","ns"))))
tab <- data.frame(Variable=names(or),
                  OR=round(or,4),CI_lo=round(cil,4),CI_hi=round(cih,4),
                  p=round(pv,4),sig=sig,row.names=NULL)
# Show primary predictors and employment sector only
show_rows <- c("noncitizen","hispanic","hisp_noncitizen",
               "emp_federal","emp_state_loc","emp_self")
print(tab[tab$Variable %in% show_rows,], row.names=FALSE)
mf4 <- model.frame(fit_m4)
cat(sprintf("\nN=%s  victims=%s  (%.2f%%)\n",
    format(nrow(mf4),big.mark=","),
    format(sum(mf4$any_idtheft==1),big.mark=","),
    mean(mf4$any_idtheft==1)*100))

cat("\n",SEP,"\n")
cat("DONE. Paste full output.\n")
cat(SEP,"\n")
