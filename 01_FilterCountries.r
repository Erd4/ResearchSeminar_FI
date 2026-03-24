##############################################################
### Filter MASTER to EU-27 + CH + OECD                     ###
### Research Seminar Financial Institutions                 ###
### Spring Semester 2026                                    ###
##############################################################

#------------------------------------------------------------#
# Load libraries
#------------------------------------------------------------#

library(readr)
library(dplyr)

#------------------------------------------------------------#
# Load data
#------------------------------------------------------------#

df <- read.csv("MASTER.csv", stringsAsFactors = FALSE)

cat("Raw MASTER rows:", nrow(df), "\n")
cat("Columns:", ncol(df), "\n")

# Convert financial variable columns (6 onward) to numeric
for (i in 6:ncol(df)) {
  df[, i] <- as.numeric(as.character(df[, i]))
}

#------------------------------------------------------------#
# Filtering DF (only keep EU-27 + CH + OECD countries)
#------------------------------------------------------------#

unique(df$SP_COUNTRY_NAME)

eu27_ch_oecd <- c(
  # EU-27
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia",
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece",
  "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
  "Malta", "Netherlands", "Poland", "Portugal", "Romania",
  "Slovakia", "Slovenia", "Spain", "Sweden",
  # CH
  "Switzerland",
  # OECD (non-EU27/CH)
  "Australia", "Canada", "Chile", "Colombia", "Costa Rica",
  "Iceland", "Israel", "Japan", "Mexico", "New Zealand", "Norway",
  "South Korea", "Türkiye", "United Kingdom", "USA"
)

df_filtered <- df[df$SP_COUNTRY_NAME %in% eu27_ch_oecd, ]

cat("Filtered rows (EU27+CH+OECD):", nrow(df_filtered), "\n")
cat("Countries kept:", length(unique(df_filtered$SP_COUNTRY_NAME)), "\n")

#------------------------------------------------------------#
# Split: ALL institutions vs SAV only
#------------------------------------------------------------#

unique(df_filtered$SNL_INDUSTRY)

df_SAV <- df_filtered[df_filtered$SNL_INDUSTRY == "Savings Bank/Thrift/Mutual", ]

#------------------------------------------------------------#
# Save filtered dataframes
#------------------------------------------------------------#

write.csv(df_filtered, "MASTER_EU27_CH_OECD.csv", row.names = FALSE)
write.csv(df_SAV,      "MASTER_SAV_EU27_CH_OECD.csv", row.names = FALSE)
