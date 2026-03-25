library(tidyr)
library(dplyr)
library(readr)

data_path <- "/Users/giovannibrenni/Downloads/Data"
setwd(data_path)

# ── 1. Country list & years ───────────────────────────────────
target_countries <- c(
  "Australia", "Canada", "Chile", "Colombia", "Costa Rica",
  "Iceland", "Israel", "Japan", "Korea, Rep.", "Mexico",
  "New Zealand", "Norway", "Turkiye", "United Kingdom", "United States"
)

years_of_interest <- as.character(2010:2025)

# ── 2. Helper: read World Bank CSV ───────────────────────────
read_wb <- function(path, value_col_name) {
  df <- read_csv(path, show_col_types = FALSE)
  names(df) <- gsub("\xEF\xBB\xBF", "", names(df))
  
  df %>%
    filter(`Country Name` %in% target_countries) %>%
    select(`Country Name`, `Country Code`, all_of(years_of_interest)) %>%
    pivot_longer(
      cols      = all_of(years_of_interest),
      names_to  = "Year",
      values_to = value_col_name
    ) %>%
    mutate(Year = as.integer(Year))
}

# ── 3. Load all three series ──────────────────────────────────
mktcap   <- read_wb("mkt_cap.csv",           "mktcap_pct_gdp")
pvcredit <- read_wb("pvcredit.csv",          "pvcredit_pct_gdp")
pop65    <- read_wb("Population AGe65.csv",  "pop65_pct")

# ── 4. Merge & compute ────────────────────────────────────────
panel <- mktcap %>%
  left_join(pvcredit, by = c("Country Name", "Country Code", "Year")) %>%
  left_join(pop65,    by = c("Country Name", "Country Code", "Year")) %>%
  mutate(
    `Country Name` = case_when(
      `Country Name` == "Korea, Rep." ~ "South Korea",
      `Country Name` == "Turkiye"     ~ "Turkey",
      TRUE                            ~ `Country Name`
    ),
    mktcap_ratio   = mktcap_pct_gdp   / 100,
    pvcredit_ratio = pvcredit_pct_gdp / 100,
    FinStructure   = log((1 + mktcap_ratio) / (1 + pvcredit_ratio))
  ) %>%
  select(
    `Country Name`, `Country Code`, Year,
    mktcap_pct_gdp, pvcredit_pct_gdp, FinStructure, pop65_pct
  ) %>%
  arrange(`Country Name`, Year)

# ── 5. Quick checks ───────────────────────────────────────────
cat("=== Countries found ===\n")
print(sort(unique(panel$`Country Name`)))

cat("\n=== Sample rows ===\n")
print(head(panel, 15))

# ── 6. Export ─────────────────────────────────────────────────
write_csv(panel, "fin_structure_panel.csv")
cat("\nSaved: fin_structure_panel.csv\n")
