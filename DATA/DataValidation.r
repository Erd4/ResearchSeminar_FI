setwd("DATA/")
# Load filtered CSVs
df1 <- read.csv("RS_FI_2026_EU27_CH.csv")
df2 <- read.csv("RS_FI_2026_SAV_EU27_CH.csv")

# Rename Columns
col_map <- c(
  # Identifiers
  Entity.ID          = "Entity.ID",
  fiscal_year        = "fiscal_year",
  Entity.Name        = "Entity.Name",
  SNL_INDUSTRY       = "SNL_Industry",
  SP_COUNTRY_NAME    = "SP_COUNTRY_NAME",
  # Ratio
  Deposit.Ratio      = "Deposit_Ratio",
  # Balance Sheet
  var_329639         = "Total_Assets",
  var_273760         = "Total_Deposits"
)

names(df1) <- col_map[names(df1)]
names(df2) <- col_map[names(df2)]

# Keep only identifier, Total Deposits, Total Assets, and Deposit Ratio
keep_cols <- c(
  "Entity.ID", "fiscal_year", "Entity.Name", "SNL_Industry", "SP_COUNTRY_NAME",
  "Total_Deposits", "Total_Assets", "Deposit_Ratio"
)

df1 <- df1[, keep_cols]
df2 <- df2[, keep_cols]

# Check for NAs
print(colSums(is.na(df1)))

print(colSums(is.na(df2)))

write.csv(df1, "ToMerge/EU27_CH_Stripped.csv", row.names = FALSE)
write.csv(df2, "ToMerge/SAV_EU27_CH_Stripped.csv", row.names = FALSE)

