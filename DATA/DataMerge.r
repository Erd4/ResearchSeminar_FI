##############################################################
### Data Merge                                              ###
### Research Seminar Financial Institutions                 ###
### Spring Semester 2026                                    ###
##############################################################

#------------------------------------------------------------#
# Load libraries
#------------------------------------------------------------#

library(readr)
library(dplyr)

#------------------------------------------------------------#
# Set working directory
#------------------------------------------------------------#

setwd("DATA/")

#------------------------------------------------------------#
# Load base datasets (Stripped CSVs)
#------------------------------------------------------------#

df_all <- read.csv("ToMerge/ALL/EU27_CH_Stripped.csv", stringsAsFactors = FALSE)
df_sav <- read.csv("ToMerge/SAV/SAV_EU27_CH_Stripped.csv", stringsAsFactors = FALSE)

cat("ALL base rows:", nrow(df_all), "\n")
cat("SAV base rows:", nrow(df_sav), "\n")

#------------------------------------------------------------#
# Load macro / control datasets
#------------------------------------------------------------#

age65       <- read.csv("ToMerge/ALL/age65_eu27_ch_2010_2025.csv", stringsAsFactors = FALSE)
agg_rr      <- read.csv("ToMerge/ALL/aggregate_rr_2010_2025.csv", stringsAsFactors = FALSE)
finstruc    <- read.csv("ToMerge/ALL/finstructure_patched_2010_2025.csv", stringsAsFactors = FALSE)
pension     <- read.csv("ToMerge/ALL/pension_system_classification.csv", stringsAsFactors = FALSE)

#------------------------------------------------------------#
# Prepare join keys
#------------------------------------------------------------#

# Stripped files use "FY2010" format; macro files use numeric 2010
# -> extract numeric year from fiscal_year

df_all$year <- as.integer(sub("FY", "", df_all$fiscal_year))
df_sav$year <- as.integer(sub("FY", "", df_sav$fiscal_year))

# Harmonise country names across macro files
# (macro data uses "Slovak Republic", stripped data uses "Slovakia")

age65$country    <- ifelse(age65$country    == "Slovak Republic", "Slovakia", age65$country)
agg_rr$country   <- ifelse(agg_rr$country   == "Slovak Republic", "Slovakia", agg_rr$country)
finstruc$country <- ifelse(finstruc$country == "Slovak Republic", "Slovakia", finstruc$country)
pension$country  <- ifelse(pension$country  == "Slovak Republic", "Slovakia", pension$country)

#------------------------------------------------------------#
# Merge function
#------------------------------------------------------------#

merge_master <- function(df) {

  # 1) Aging: age65_pct (join on country + year)
  df <- merge(df, age65[, c("country", "year", "age65_pct")],
              by.x = c("SP_COUNTRY_NAME", "year"),
              by.y = c("country", "year"),
              all.x = TRUE)

  # 2) Aggregate Replacement Ratio (join on country + year)
  df <- merge(df, agg_rr[, c("country", "year", "agg_replacement_ratio")],
              by.x = c("SP_COUNTRY_NAME", "year"),
              by.y = c("country", "year"),
              all.x = TRUE)

  # 3) Financial Structure (join on country + year)
  df <- merge(df, finstruc[, c("country", "year", "mktcap_pct_gdp", "pvcredit_pct_gdp", "FinStructure")],
              by.x = c("SP_COUNTRY_NAME", "year"),
              by.y = c("country", "year"),
              all.x = TRUE)

  # 4) Pension System Classification (join on country only)
  df <- merge(df, pension[, c("country", "pension_system", "Bismarckian", "Beveridgean")],
              by.x = "SP_COUNTRY_NAME",
              by.y = "country",
              all.x = TRUE)

  # Drop helper year column and reorder
  df$year <- NULL

  # Reorder: identifiers first, then original vars, then macro vars
  col_order <- c(
    "Entity.ID", "fiscal_year", "Entity.Name", "SNL_Industry", "SP_COUNTRY_NAME",
    "Total_Deposits", "Total_Assets", "Deposit_Ratio",
    "age65_pct", "agg_replacement_ratio",
    "mktcap_pct_gdp", "pvcredit_pct_gdp", "FinStructure",
    "pension_system", "Bismarckian", "Beveridgean"
  )

  df <- df[, col_order]

  return(df)
}

#------------------------------------------------------------#
# Run merges
#------------------------------------------------------------#

master_all <- merge_master(df_all)
master_sav <- merge_master(df_sav)

cat("\nALL master rows:", nrow(master_all), " cols:", ncol(master_all), "\n")
cat("SAV master rows:", nrow(master_sav), " cols:", ncol(master_sav), "\n")

#------------------------------------------------------------#
# Quick NA check on new columns
#------------------------------------------------------------#

new_cols <- c("age65_pct", "agg_replacement_ratio",
              "mktcap_pct_gdp", "pvcredit_pct_gdp", "FinStructure",
              "pension_system", "Bismarckian", "Beveridgean")

cat("\n--- NA counts (ALL) ---\n")
print(colSums(is.na(master_all[, new_cols])))

cat("\n--- NA counts (SAV) ---\n")
print(colSums(is.na(master_sav[, new_cols])))

#------------------------------------------------------------#
# Save master datasets
#------------------------------------------------------------#

write.csv(master_all, "ToMerge/ALL/master.csv", row.names = FALSE)
write.csv(master_sav, "ToMerge/SAV/master.csv", row.names = FALSE)
