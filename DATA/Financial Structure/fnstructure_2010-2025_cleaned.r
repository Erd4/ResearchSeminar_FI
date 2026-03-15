setwd("/Users/giovannibrenni/Desktop/FS26/Research Seminar/Data")

# ── 1. Reload both CSVs ──────────────────────────────────────
finstructure_base <- read_csv("finstructure_eu27_ch_2007_2025.csv",
                              show_col_types = FALSE) %>%
  select(country, country_code, year, mktcap_pct_gdp, pvcredit_pct_gdp, FinStructure)

finstructure_patched <- read_csv("finstructure_eu27_ch_2007_2025_additions.csv",
                                 show_col_types = FALSE) %>%
  select(country, country_code, year, mktcap_pct_gdp, pvcredit_pct_gdp, FinStructure)

# ── 2. Drop 2007-2009 ────────────────────────────────────────
finstructure_base    <- finstructure_base    %>% filter(year >= 2010)
finstructure_patched <- finstructure_patched %>% filter(year >= 2010)

# ── 3. Save ──────────────────────────────────────────────────
write_csv(finstructure_base,    "finstructure_base_2010_2025.csv")
write_csv(finstructure_patched, "finstructure_patched_2010_2025.csv")
cat("Files saved.\n")


# ── 4. Descriptives function ─────────────────────────────────
describe_fs <- function(df, label) {
  cat("\n", rep("=", 60), "\n", sep = "")
  cat("DATASET:", label, "\n")
  cat(rep("=", 60), "\n", sep = "")
  cat("Total rows:", nrow(df), "| Countries:", n_distinct(df$country), 
      "| Years:", min(df$year), "-", max(df$year), "\n")
  
  cat("\n--- Coverage by country ---\n")
  df %>%
    group_by(country) %>%
    summarise(
      n_available   = sum(!is.na(FinStructure)),
      n_missing     = sum(is.na(FinStructure)),
      pct_available = round(100 * n_available / n(), 1),
      .groups = "drop"
    ) %>%
    arrange(pct_available) %>%
    print(n = 30)
  
  cat("\n--- Overall FinStructure summary ---\n")
  print(summary(df$FinStructure))
  
  cat("\n--- By country (mean, sd, min, max) ---\n")
  df %>%
    filter(!is.na(FinStructure)) %>%
    group_by(country) %>%
    summarise(
      mean = round(mean(FinStructure), 3),
      sd   = round(sd(FinStructure),   3),
      min  = round(min(FinStructure),  3),
      max  = round(max(FinStructure),  3),
      n    = n(),
      .groups = "drop"
    ) %>%
    arrange(mean) %>%
    print(n = 30)
}

# ── 5. Run ───────────────────────────────────────────────────
describe_fs(finstructure_base,    "World Bank only (base)")
describe_fs(finstructure_patched, "Patched (World Bank + ECB + BIS + Nasdaq)")
