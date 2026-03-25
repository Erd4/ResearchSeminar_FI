##############################################################
### Merge Macro / Control Variables                        ###
### Research Seminar Financial Institutions                 ###
### Spring Semester 2026                                    ###
##############################################################

#------------------------------------------------------------#
# Load libraries
#------------------------------------------------------------#

library(readr)
library(dplyr)

#------------------------------------------------------------#
# Load base datasets (with Region already merged)
#------------------------------------------------------------#

df_all <- read.csv("MASTER_EU27_CH_OECD.csv", stringsAsFactors = FALSE)
df_sav <- read.csv("MASTER_SAV_EU27_CH_OECD.csv", stringsAsFactors = FALSE)


#------------------------------------------------------------#
# Load macro / control datasets
#------------------------------------------------------------#

finstruc <- read.csv("(OPEN) To be merged/fin_structure_OECD_patched.csv",
                      stringsAsFactors = FALSE)
macro    <- read.csv("(OPEN) To be merged/macro_panel_vf.csv",
                      stringsAsFactors = FALSE)

#------------------------------------------------------------#
# Prepare join keys
#------------------------------------------------------------#

# Base data uses "FY2010" format; macro files use numeric year
# -> extract numeric year from fiscal_year

df_all$year <- as.integer(sub("FY", "", df_all$fiscal_year))
df_sav$year <- as.integer(sub("FY", "", df_sav$fiscal_year))

# Harmonise country names across macro files
# (fin_structure / macro_panel use "United States" and "Turkey",
#  MASTER uses "USA" and "Türkiye")

finstruc$SP_COUNTRY_NAME <- ifelse(finstruc$SP_COUNTRY_NAME == "United States",
                                    "USA", finstruc$SP_COUNTRY_NAME)
finstruc$SP_COUNTRY_NAME <- ifelse(finstruc$SP_COUNTRY_NAME == "Turkey",
                                    "Türkiye", finstruc$SP_COUNTRY_NAME)

macro$Country <- ifelse(macro$Country == "United States", "USA", macro$Country)
macro$Country <- ifelse(macro$Country == "Turkey", "Türkiye", macro$Country)

#------------------------------------------------------------#
# Merge function
#------------------------------------------------------------#

merge_macro <- function(df) {

  # 1) Financial Structure (join on country + year)
  #    Columns: mktcap_pct_gdp, pvcredit_pct_gdp, FinStructure, pop65_pct
  df <- merge(df,
              finstruc[, c("SP_COUNTRY_NAME", "fiscal_year",
                           "mktcap_pct_gdp", "pvcredit_pct_gdp",
                           "FinStructure", "pop65_pct")],
              by.x = c("SP_COUNTRY_NAME", "year"),
              by.y = c("SP_COUNTRY_NAME", "fiscal_year"),
              all.x = TRUE)

  # 2) Macro Panel (join on country + year)
  #    Columns: gdp_per_capita_usd, gdp_growth_pct, unemployment_rate_pct,
  #             interest_rate_pct, pension_assets_pct_gdp
  df <- merge(df,
              macro[, c("Country", "Year",
                        "gdp_per_capita_usd", "gdp_growth_pct",
                        "unemployment_rate_pct", "interest_rate_pct",
                        "pension_assets_pct_gdp")],
              by.x = c("SP_COUNTRY_NAME", "year"),
              by.y = c("Country", "Year"),
              all.x = TRUE)

  # Drop helper year column
  df$year <- NULL

  return(df)
}

#------------------------------------------------------------#
# Run merges
#------------------------------------------------------------#

master_all <- merge_macro(df_all)
master_sav <- merge_macro(df_sav)

#------------------------------------------------------------#
# Construct Deposit_Ratio = Total Deposits / Total Assets
#------------------------------------------------------------#

master_all$Deposit_Ratio <- master_all$var_273760 / master_all$var_329639
master_sav$Deposit_Ratio <- master_sav$var_273760 / master_sav$var_329639

#------------------------------------------------------------#
# Quick NA check on new columns
#------------------------------------------------------------#

new_cols <- c("mktcap_pct_gdp", "pvcredit_pct_gdp", "FinStructure", "pop65_pct",
              "gdp_per_capita_usd", "gdp_growth_pct", "unemployment_rate_pct",
              "interest_rate_pct", "pension_assets_pct_gdp")

cat("\n--- NA counts (ALL) ---\n")
print(colSums(is.na(master_all[, new_cols])))

cat("\n--- NA counts (SAV) ---\n")
print(colSums(is.na(master_sav[, new_cols])))

#------------------------------------------------------------#
# Rename columns for final output
#------------------------------------------------------------#

# Variable mapping from S&P "SNL Financial Institutions" database
var_rename <- c(
  # Other Variables
  "var_273789" = "Tier_1_Ratio",
  "var_274042" = "Non_Performing_Loans",
  "var_308854" = "Net_Debt",
  "var_329633" = "EBIT",
  # Balance Sheet — Assets
  "var_273694" = "Total_Gross_Loans",
  "var_273678" = "Loan_Loss_Reserve",
  "var_273695" = "Total_Net_Loans",
  "var_273685" = "Total_Securities",
  "var_329637" = "Cash_and_Cash_Equivalents",
  "var_273689" = "Total_Intangible_Assets",
  "var_273690" = "Fixed_Assets",
  "var_329639" = "Total_Assets",
  # Balance Sheet — Liabilities
  "var_273760" = "Total_Deposits",
  "var_273704" = "Total_Subordinated_Debt",
  "var_329640" = "Total_Debt",
  "var_329641" = "Total_Equity",
  "var_273714" = "Total_Liabilities",
  "var_283140" = "Senior_Debt",
  # Income Statement
  "var_273916" = "Interest_Income",
  "var_329634" = "Interest_Expense",
  "var_273918" = "Net_Interest_Income",
  "var_273923" = "Non_Interest_Income",
  "var_273922" = "Other_Non_Interest_Income",
  "var_273930" = "Provision_for_Loan_Loss_Reserve",
  "var_278301" = "Operating_Revenue",
  "var_273927" = "Other_Expense",
  "var_273928" = "Operating_Expense",
  "var_273926" = "Compensation_and_Benefits",
  "var_273936" = "Net_Income_Before_Taxes",
  "var_273937" = "Provision_for_Taxes",
  "var_273768" = "Net_Income"
)

rename_vars <- function(df) {
  nms <- names(df)
  for (old in names(var_rename)) {
    nms[nms == old] <- var_rename[old]
  }
  # Also rename SP_COUNTRY_NAME -> Country
  nms[nms == "SP_COUNTRY_NAME"] <- "Country"
  names(df) <- nms
  return(df)
}

master_all <- rename_vars(master_all)
master_sav <- rename_vars(master_sav)

# Final output now has: Country, Region, Country_Region + readable variable names

#------------------------------------------------------------#
# Save master datasets
#------------------------------------------------------------#

write.csv(master_all, "MASTER_EU27_CH_OECD.csv", row.names = FALSE)
write.csv(master_sav, "MASTER_SAV_EU27_CH_OECD.csv", row.names = FALSE)

View(master_all)
View(master_sav)
