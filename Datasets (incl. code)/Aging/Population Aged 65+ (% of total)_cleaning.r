# ============================================================
# Population Aged 65+ (% of total) - Clean & Reshape
# ============================================================

library(dplyr)
library(readr)
library(tidyr)

# ── 0. SET YOUR PATH ─────────────────────────────────────────
setwd("/Users/giovannibrenni/Desktop/FS26/Research Seminar/Data")

# ── 1. Country list ───────────────────────────────────────────
eu27_plus_ch <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus",
  "Czechia", "Denmark", "Estonia", "Finland", "France",
  "Germany", "Greece", "Hungary", "Ireland", "Italy",
  "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands",
  "Poland", "Portugal", "Romania", "Slovak Republic", "Slovenia",
  "Spain", "Sweden", "Switzerland"
)

years_of_interest <- as.character(2010:2025)

# ── 2. Load & reshape ─────────────────────────────────────────
raw <- read_csv("API_SP_POP_65UP_TO_ZS_DS2_en_csv_v2_1929.csv",
                skip = 4, show_col_types = FALSE)

age65 <- raw %>%
  filter(`Country Name` %in% eu27_plus_ch) %>%
  select(`Country Name`, `Country Code`, all_of(years_of_interest)) %>%
  pivot_longer(
    cols      = all_of(years_of_interest),
    names_to  = "year",
    values_to = "age65_pct"
  ) %>%
  mutate(year = as.integer(year)) %>%
  select(
    country      = `Country Name`,
    country_code = `Country Code`,
    year,
    age65_pct
  ) %>%
  arrange(country, year)

# ── 3. Quick summary ──────────────────────────────────────────
cat("=== Coverage by country ===\n")
age65 %>%
  group_by(country) %>%
  summarise(
    n_available = sum(!is.na(age65_pct)),
    n_missing   = sum(is.na(age65_pct)),
    .groups = "drop"
  ) %>%
  print(n = 30)

cat("\n=== Overall summary ===\n")
print(summary(age65$age65_pct))

# ── 4. Save ───────────────────────────────────────────────────
write_csv(age65, "age65_eu27_ch_2010_2025.csv")
cat("\nSaved: age65_eu27_ch_2010_2025.csv\n")