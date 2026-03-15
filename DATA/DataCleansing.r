##############################################################
### Data Cleansing                                          ###
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
df <- read.csv("RS_FI_2026.csv", stringsAsFactors = FALSE)

# Convert financial variable columns (6:35) to numeric
for (i in 6:ncol(df)) {
  df[, i] <- as.numeric(as.character(df[, i]))
}

# Inspect
names(df)
nrow(df)
View(df)

#------------------------------------------------------------#
# Fitering DF (only show banks in EU-27 and CH)
#------------------------------------------------------------#

unique(df$SP_COUNTRY_NAME)

eu27_ch <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Czechia",
                  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece",
                  "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
                  "Malta", "Netherlands", "Poland", "Portugal", "Romania",
                  "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland")


                
df_eu27_ch <- df[df$SP_COUNTRY_NAME %in% eu27_ch, ]

nrow(df_eu27_ch)
View(df_eu27_ch)

#------------------------------------------------------------#
# Fitering DF (only show savings banks in EU-27 and CH)
#------------------------------------------------------------#

unique(df$SNL_INDUSTRY)

df_SAV_eu27_ch <- df_eu27_ch[df_eu27_ch$SNL_INDUSTRY == "Savings Bank/Thrift/Mutual", ]

nrow(df_SAV_eu27_ch)
View(df_SAV_eu27_ch)

#------------------------------------------------------------#
# Add Deposit Ratio (Deposits/Assets)
#------------------------------------------------------------#

df_eu27_ch$`Deposit Ratio` <- df_eu27_ch$var_273760 / df_eu27_ch$var_329639
df_SAV_eu27_ch$`Deposit Ratio` <- df_SAV_eu27_ch$var_273760 / df_SAV_eu27_ch$var_329639

#------------------------------------------------------------#
# Save filtered dataframes
#------------------------------------------------------------#

write.csv(df_eu27_ch, "RS_FI_2026_EU27_CH.csv", row.names = FALSE)
write.csv(df_SAV_eu27_ch, "RS_FI_2026_SAV_EU27_CH.csv", row.names = FALSE)

