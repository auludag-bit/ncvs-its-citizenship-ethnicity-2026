# ==========================================================================
# Step 7d: Get correct model N from model.frame() for all tables
# ==========================================================================
options(survey.lonely.psu = "adjust")
library(haven); library(survey); library(dplyr)

df18_raw <- read_dta("37923-0001-Data.dta")
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
      existing_acct=pmax(c012,c017,c019,na.rm=FALSE),
      new_acct=c041, other_idtheft=c063,
      age=ifelse(V3014==-1,NA_real_,as.numeric(V3014)),
      male=as.integer(V3018==1),
      race_black=as.integer(V3023A==2),
      race_other=as.integer(V3023A>=3&V3023A<=20),
      edu_lths=as.integer(V3020<=11),
      edu_hs=as.integer(V3020%in%c(27,28)),
      edu_some=as.integer(V3020%in%c(40,41)),
      married=as.integer(V3015==1),
      income=case_when(V2026%in%c(98,99)~NA_real_,TRUE~as.numeric(V2026)),
      msa_suburban=as.integer(V2129==2),msa_central=as.integer(V2129==3),
      region_mw=as.integer(V2127B==2),region_south=as.integer(V2127B==3),
      region_west=as.integer(V2127B==4)
    ) %>%
    filter(!is.na(any_idtheft),!is.na(noncitizen),
           !is.na(hispanic),!is.na(age),!is.na(male))
}

df18 <- build_analytic(df18_raw)
df21 <- build_analytic(df21_raw)

svy18 <- svydesign(id=~V2118,strata=~V2117,weights=~VS363,data=df18,nest=TRUE)
svy21 <- svydesign(id=~V2118,strata=~V2117,weights=~VS363,data=df21,nest=TRUE)

df_pooled <- bind_rows(
  mutate(df18,year_2021=0L),
  mutate(df21,year_2021=1L)
) %>% mutate(
  year_x_noncitizen =year_2021*noncitizen,
  year_x_hispanic   =year_2021*hispanic,
  year_x_hisp_noncit=year_2021*hisp_noncitizen)
svy_pool <- svydesign(id=~V2118,strata=~V2117,weights=~VS363,
                      data=df_pooled,nest=TRUE)

CTRL <- paste("age+male+race_black+race_other+edu_lths+edu_hs+edu_some+",
              "married+income+msa_suburban+msa_central+",
              "region_mw+region_south+region_west")
f_main <- as.formula(paste("any_idtheft~noncitizen+hispanic+hisp_noncitizen+",CTRL))
f_unaj <- as.formula("any_idtheft~noncitizen+hispanic+hisp_noncitizen")
f_pool <- as.formula(paste("any_idtheft~year_2021+noncitizen+hispanic+",
  "hisp_noncitizen+year_x_noncitizen+year_x_hispanic+year_x_hisp_noncit+",CTRL))

get_n <- function(fit) {
  mf <- model.frame(fit)
  outcome <- names(mf)[1]
  list(n = nrow(mf),
       victims = sum(mf[[outcome]]==1, na.rm=TRUE))
}

SEP <- paste(rep("=",60),collapse="")
cat("\n",SEP,"\n")
cat("CORRECT MODEL Ns FROM model.frame()\n")
cat(SEP,"\n\n")

# Table II: 2018 unadjusted
fit18_u <- svyglm(f_unaj,design=svy18,family=quasibinomial("logit"))
r <- get_n(fit18_u)
cat(sprintf("Table II Model 1 (2018 unadj): N=%s, victims=%s\n",
    format(r$n,big.mark=","),format(r$victims,big.mark=",")))

# Table II: 2018 adjusted
fit18_a <- svyglm(f_main,design=svy18,family=quasibinomial("logit"))
r <- get_n(fit18_a)
cat(sprintf("Table II Model 2 (2018 adj):   N=%s, victims=%s\n",
    format(r$n,big.mark=","),format(r$victims,big.mark=",")))

# Table II: 2021 unadjusted
fit21_u <- svyglm(f_unaj,design=svy21,family=quasibinomial("logit"))
r <- get_n(fit21_u)
cat(sprintf("Table II Model 3 (2021 unadj): N=%s, victims=%s\n",
    format(r$n,big.mark=","),format(r$victims,big.mark=",")))

# Table II: 2021 adjusted
fit21_a <- svyglm(f_main,design=svy21,family=quasibinomial("logit"))
r <- get_n(fit21_a)
cat(sprintf("Table II Model 4 (2021 adj):   N=%s, victims=%s\n",
    format(r$n,big.mark=","),format(r$victims,big.mark=",")))

# Table III: pooled
fit_p <- svyglm(f_pool,design=svy_pool,family=quasibinomial("logit"))
r <- get_n(fit_p)
cat(sprintf("Table III (pooled adj):        N=%s, victims=%s\n",
    format(r$n,big.mark=","),format(r$victims,big.mark=",")))

# Table IV: subtypes
for (outcome in c("existing_acct","new_acct","other_idtheft")) {
  f_s <- as.formula(paste(outcome,"~noncitizen+hispanic+hisp_noncitizen+",CTRL))
  fit_s <- svyglm(f_s,design=svy21,family=quasibinomial("logit"))
  r <- get_n(fit_s)
  cat(sprintf("Table IV %-15s:        N=%s, victims=%s\n",
      outcome,format(r$n,big.mark=","),format(r$victims,big.mark=",")))
}

cat("\n",SEP,"\n")
cat("DONE\n")
cat(SEP,"\n")
