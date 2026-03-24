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
# Save master datasets
#------------------------------------------------------------#

write.csv(master_all, "MASTER_EU27_CH_OECD.csv", row.names = FALSE)
write.csv(master_sav, "MASTER_SAV_EU27_CH_OECD.csv", row.names = FALSE)

View(master_all)
View(master_sav)
