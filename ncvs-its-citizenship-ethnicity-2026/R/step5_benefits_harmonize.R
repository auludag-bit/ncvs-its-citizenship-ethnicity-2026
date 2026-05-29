# ==========================================================================
# Step 5: Harmonized Benefits Fraud Comparison (Response to Reviewer 7)
# ==========================================================================
# Codebook findings:
#
# VS063 (other misuse denominator):
#   2018 label: "PERSONAL INFORMATION FOR OTHER FRAUDULENT PURPOSES"
#   2018 question: "Has someone used OR ATTEMPTED TO USE your personal
#     information for some other fraudulent purpose..."
#   2021 label: "PERSONAL INFORMATION USED FOR OTHER FRAUDULENT PURPOSES
#     IN PAST 12 MONTHS"
#   2021 question: "Has this happened during the past 12 months...?"
#     (asked only of those who confirmed lifetime occurrence in an
#      earlier screener -- attempted incidents excluded)
#
# VS075 (government benefits fraud numerator):
#   2018 question: "As far as you know, did the person USE OR ATTEMPT TO
#     USE your personal information... To apply for government benefits?"
#   2021 question: "In which of the following ways has someone USED your
#     personal information during the past 12 months?... To apply for
#     government benefits?"
#
# Key differences:
#   (1) DENOMINATOR: 2018 VS063 includes attempted incidents;
#       2021 VS063 excludes them (confirmed incidents only).
#   (2) NUMERATOR: 2018 VS075 asks about USE OR ATTEMPT;
#       2021 VS075 asks about USE only.
#   Both differences inflate the apparent 2018 denominator and
#   deflate the apparent 2018 numerator relative to 2021,
#   making the raw 12.1% -> 78.8% comparison non-comparable.
#
# Harmonization strategy:
#   We cannot remove attempted incidents from the 2018 NUMERATOR
#   (VS075) because VS075 is only asked of VS063=1 respondents
#   and there is no flag in the public-use file identifying which
#   VS063=1 cases were attempted vs. completed at the VS075 level.
#   However, we CAN check whether a VS063-level attempted-incident
#   flag exists (VS064) that would let us restrict the 2018
#   denominator to confirmed/completed incidents only.
#   This script checks VS064 and reports the harmonized comparison.
# ==========================================================================

library(haven)
library(dplyr)

cat("Loading data...\n")
df18 <- read_dta("37923-0001-Data.dta")
df21 <- read_dta("38501-0001-Data.dta")

# Build ITS-completer analytic samples (same as step4)
build_its <- function(df) {
  df %>%
    filter(VS008 %in% c(1, 2)) %>%
    filter(V3014 >= 16) %>%
    filter(!(V3083 %in% c(8, 9, -1))) %>%
    filter(!(V3024 %in% c(8, -1))) %>%
    filter(!(V3086 %in% c(3, 4)))
}

a18 <- build_its(df18)
a21 <- build_its(df21)

cat(sprintf("ITS completers: 2018 N=%s, 2021 N=%s\n",
    format(nrow(a18), big.mark=","),
    format(nrow(a21), big.mark=",")))

# --------------------------------------------------------------------------
# Step 1: Show the raw comparison (as currently reported in paper)
# --------------------------------------------------------------------------
cat("\n", paste(rep("=",60), collapse=""), "\n")
cat("RAW COMPARISON (currently in paper -- non-harmonized)\n")
cat(paste(rep("=",60), collapse=""), "\n")

for (yr in c("2018","2021")) {
  df <- if (yr=="2018") a18 else a21
  # Other-misuse victims (VS063 == 1)
  other_victims <- df %>% filter(VS063 == 1)
  n_other <- nrow(other_victims)
  # Benefits fraud among them (VS075 == 1)
  n_benefits <- sum(other_victims$VS075 == 1, na.rm=TRUE)
  n_asked    <- sum(other_victims$VS075 %in% c(1,2), na.rm=TRUE)
  pct_unw    <- n_benefits / n_asked * 100
  # Weighted
  w_sub  <- other_victims %>% filter(VS075 %in% c(1,2))
  w_pct  <- weighted.mean(w_sub$VS075 == 1, as.numeric(w_sub$VS363)) * 100
  cat(sprintf("  %s: other-misuse victims (VS063=1): n=%s\n",
      yr, format(n_other, big.mark=",")))
  cat(sprintf("       VS075 asked (non-missing): n=%s\n",
      format(n_asked, big.mark=",")))
  cat(sprintf("       Benefits fraud (VS075=1): n=%s\n",
      format(n_benefits, big.mark=",")))
  cat(sprintf("       Unweighted %%: %.1f%%\n", pct_unw))
  cat(sprintf("       Weighted %%:   %.1f%%\n\n", w_pct))
}

# --------------------------------------------------------------------------
# Step 2: Examine VS064 (other-misuse flag) to identify attempted incidents
# --------------------------------------------------------------------------
cat(paste(rep("=",60), collapse=""), "\n")
cat("EXAMINING VS064 (other-misuse flag) FOR ATTEMPT IDENTIFICATION\n")
cat(paste(rep("=",60), collapse=""), "\n")

if ("VS064" %in% names(a18)) {
  cat("\n2018 VS064 distribution (among VS063=1 cases):\n")
  a18 %>%
    filter(VS063 == 1) %>%
    count(VS064) %>%
    print()
} else {
  cat("VS064 not found in 2018 data\n")
}

if ("VS064" %in% names(a21)) {
  cat("\n2021 VS064 distribution (among VS063=1 cases):\n")
  a21 %>%
    filter(VS063 == 1) %>%
    count(VS064) %>%
    print()
} else {
  cat("VS064 not found in 2021 data\n")
}

# --------------------------------------------------------------------------
# Step 3: Harmonized comparison using VS076 (benefits fraud flag)
# to identify whether VS075=1 cases were completed vs. attempted
# --------------------------------------------------------------------------
cat("\n", paste(rep("=",60), collapse=""), "\n")
cat("EXAMINING VS076 (benefits-fraud flag) FOR ATTEMPT IDENTIFICATION\n")
cat(paste(rep("=",60), collapse=""), "\n")

for (yr in c("2018","2021")) {
  df <- if (yr=="2018") a18 else a21
  if ("VS076" %in% names(df)) {
    cat(sprintf("\n%s VS076 distribution (among VS075=1):\n", yr))
    df %>%
      filter(VS075 == 1) %>%
      count(VS076) %>%
      print()
  }
}

# --------------------------------------------------------------------------
# Step 4: Check VS063 question universe -- does 2018 include
# respondents who only had attempted (not completed) other misuse?
# Look at VS062 (other-misuse: ever) as the lifetime screener
# --------------------------------------------------------------------------
cat("\n", paste(rep("=",60), collapse=""), "\n")
cat("CHECKING VS062 (lifetime other-misuse screener) IN BOTH WAVES\n")
cat(paste(rep("=",60), collapse=""), "\n")

for (vname in c("VS062","VS060","VS061")) {
  for (yr in c("2018","2021")) {
    df <- if (yr=="2018") a18 else a21
    if (vname %in% names(df)) {
      vc <- table(df[[vname]], useNA="always")
      cat(sprintf("  %s %s: ", yr, vname))
      print(vc)
    }
  }
}

cat("\n", paste(rep("=",60), collapse=""), "\n")
cat("DONE. Paste full output for reviewer response.\n")
cat(paste(rep("=",60), collapse=""), "\n")
