# ============================================================
# FinStructure Calculation
# FinStructure_ct = ln( (1 + StockMarketCap/GDP) / (1 + PrivateCredit/GDP) )
# Countries: EU-27 + Switzerland | Years: 2007-2025
# ============================================================

library(tidyr)
library(dplyr)
library(readr)

# data_path <- "/Users/giovannibrenni/Desktop/FS26/Research Seminar/Data"
# setwd("/Users/giovannibrenni/Desktop/FS26/Research Seminar/Data")

# ── 1. Country list ──────────────────────────────────────────
eu27_plus_ch <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus",
  "Czechia", "Denmark", "Estonia", "Finland", "France",
  "Germany", "Greece", "Hungary", "Ireland", "Italy",
  "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands",
  "Poland", "Portugal", "Romania", "Slovak Republic", "Slovenia",
  "Spain", "Sweden", "Switzerland"
)

years_of_interest <- as.character(2007:2025)

# ── 2. Helper: read World Bank CSV (skips 4-row header) ──────
read_wb <- function(path, value_col_name) {
  df <- read_csv(path, skip = 4, show_col_types = FALSE)
  
  df %>%
    filter(`Country Name` %in% eu27_plus_ch) %>%
    select(`Country Name`, `Country Code`, all_of(years_of_interest)) %>%
    pivot_longer(
      cols      = all_of(years_of_interest),
      names_to  = "year",
      values_to = value_col_name
    ) %>%
    mutate(year = as.integer(year))
}

# ── 3. Load both series ──────────────────────────────────────
mktcap  <- read_wb("Market capitalization of listed domestic companies.csv",
                   "mktcap_pct_gdp")
pvcredit <- read_wb("Domestic credit to private sector by banks.csv",
                    "pvcredit_pct_gdp")

# ── 4. Merge & compute FinStructure ─────────────────────────
finstructure <- mktcap %>%
  left_join(pvcredit, by = c("Country Name", "Country Code", "year")) %>%
  mutate(
    # World Bank values are already in %, so divide by 100 to get ratio to GDP
    mktcap_ratio   = mktcap_pct_gdp  / 100,
    pvcredit_ratio = pvcredit_pct_gdp / 100,
    
    # Core formula — produces NA automatically when either input is NA
    FinStructure = log((1 + mktcap_ratio) / (1 + pvcredit_ratio))
  ) %>%
  select(
    country     = `Country Name`,
    country_code = `Country Code`,
    year,
    mktcap_pct_gdp,
    pvcredit_pct_gdp,
    FinStructure
  ) %>%
  arrange(country, year)

# ── 5. Quick summary ─────────────────────────────────────────
cat("=== Countries found ===\n")
print(sort(unique(finstructure$country)))

cat("\n=== Missing-value overview (FinStructure) ===\n")
finstructure %>%
  group_by(country) %>%
  summarise(
    n_total   = n(),
    n_missing = sum(is.na(FinStructure)),
    .groups   = "drop"
  ) %>%
  print(n = 30)

cat("\n=== Sample rows ===\n")
print(head(finstructure, 10))

# ── 6. Export ────────────────────────────────────────────────
write_csv(finstructure, "finstructure_eu27_ch_2007_2025.csv")
cat("\nFile saved: finstructure_eu27_ch_2007_2025.csv\n")


