# ============================================================
# Aggregate Replacement Ratio - Clean & Reshape
# ============================================================

library(dplyr)
library(readr)
library(tidyr)
library(readxl)

# ── 0. SET YOUR PATH ─────────────────────────────────────────
setwd("/Users/giovannibrenni/Desktop/FS26/Research Seminar/Data")

# ── 1. Country codes lookup ───────────────────────────────────
country_codes <- c(
  "Belgium" = "BEL", "Bulgaria" = "BGR", "Czechia" = "CZE",
  "Denmark" = "DNK", "Germany" = "DEU", "Estonia" = "EST",
  "Ireland" = "IRL", "Greece" = "GRC", "Spain" = "ESP",
  "France" = "FRA", "Croatia" = "HRV", "Italy" = "ITA",
  "Cyprus" = "CYP", "Latvia" = "LVA", "Lithuania" = "LTU",
  "Luxembourg" = "LUX", "Hungary" = "HUN", "Malta" = "MLT",
  "Netherlands" = "NLD", "Austria" = "AUT", "Poland" = "POL",
  "Portugal" = "PRT", "Romania" = "ROU", "Slovenia" = "SVN",
  "Slovakia" = "SVK", "Finland" = "FIN", "Sweden" = "SWE",
  "Switzerland" = "CHE"
)

# ── 2. Load & reshape ─────────────────────────────────────────
raw <- read_excel("Raw_Aggregate_RR.xlsx", sheet = "Sheet 1", skip = 8)

# Hardcode column names
colnames(raw)[1] <- "country"
colnames(raw)[2:ncol(raw)] <- c("2010","2011","2012","2013","2014",
                                "2015","2016","2017","2018","2019",
                                "2020","2021","2022","2023","2024","2025")

# Keep only EU27 + CH
raw <- raw %>% filter(country %in% names(country_codes))

# Pivot to long format
rr_long <- raw %>%
  pivot_longer(
    cols = -country,
    names_to = "year",
    values_to = "agg_replacement_ratio",
    values_transform = list(agg_replacement_ratio = as.character)
  ) %>%
  mutate(
    year = as.integer(year),
    country_code = country_codes[country],
    agg_replacement_ratio = na_if(agg_replacement_ratio, ":"),
    agg_replacement_ratio = as.numeric(agg_replacement_ratio)
  ) %>%
  select(country, country_code, year, agg_replacement_ratio) %>%
  arrange(country, year)

# ── 3. Quick summary ──────────────────────────────────────────
cat("=== Coverage by country ===\n")
rr_long %>%
  group_by(country) %>%
  summarise(
    n_available = sum(!is.na(agg_replacement_ratio)),
    n_missing   = sum(is.na(agg_replacement_ratio)),
    .groups = "drop"
  ) %>%
  print(n = 30)

cat("\n=== Overall summary ===\n")
print(summary(rr_long$agg_replacement_ratio))

# ── 4. Save ───────────────────────────────────────────────────
write_csv(rr_long, "aggregate_rr_2010_2025.csv")
cat("\nSaved: aggregate_rr_2010_2025.csv\n")
