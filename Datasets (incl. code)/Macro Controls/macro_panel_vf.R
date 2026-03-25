# =============================================================================
# Build macro panel dataset: EU27+CH / OECD countries, 2010 onwards
# Columns: Country, Year, gdp_per_capita_usd, gdp_growth_pct,
#          unemployment_rate_pct, interest_rate_pct, pension_assets_pct_gdp
#
# Interest rate source priority:
#   1. IMF (Interest_rates.csv)         — annual, direct
#   2. BIS (BIS_Interest_rates.csv)     — monthly end-of-month, averaged to annual
#      BIS also fills Eurozone countries (Euro area rate) for years not in IMF
#
# Pension assets: OECD (Pension_Assets.xlsx) — pension assets as % of GDP
# =============================================================================

library(tidyverse)
library(readxl)

# -----------------------------------------------------------------------------
# 0.  Country scope + helper mappings
# -----------------------------------------------------------------------------

countries_in_scope <- c(
  "Australia", "Austria", "Belgium", "Bulgaria", "Canada", "Chile",
  "Colombia", "Costa Rica", "Croatia", "Cyprus", "Czechia", "Denmark",
  "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
  "Iceland", "Ireland", "Israel", "Italy", "Japan", "Latvia", "Lithuania",
  "Luxembourg", "Malta", "Mexico", "Netherlands", "New Zealand", "Norway",
  "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "South Korea",
  "Spain", "Sweden", "Switzerland", "Turkey", "United Kingdom",
  "United States"
)

# Eurozone countries that share the ECB rate (used to fill from BIS "Euro area")
eurozone_countries <- c(
  "Austria", "Belgium", "Croatia", "Cyprus", "Estonia", "Finland", "France",
  "Germany", "Greece", "Ireland", "Italy", "Latvia", "Lithuania",
  "Luxembourg", "Malta", "Netherlands", "Portugal", "Slovakia",
  "Slovenia", "Spain"
)

# -----------------------------------------------------------------------------
# 1.  Helper: read World Bank wide-format CSV -> long tibble
# -----------------------------------------------------------------------------

read_wb_long <- function(path, value_col_name) {
  wide <- read_csv(
    path,
    skip        = 4,
    col_types   = cols(.default = col_character()),
    name_repair = "minimal"
  )
  wide %>%
    select(`Country Name`, matches("^\\d{4}$")) %>%
    pivot_longer(
      cols      = -`Country Name`,
      names_to  = "Year",
      values_to = value_col_name
    ) %>%
    rename(Country = `Country Name`) %>%
    mutate(
      Year             = as.integer(Year),
      !!value_col_name := as.numeric(.data[[value_col_name]])
    )
}

# -----------------------------------------------------------------------------
# 2.  Load World Bank files
# -----------------------------------------------------------------------------

gdppc <- read_wb_long(
  "C:/Users/noelb/OneDrive/Desktop/Raw Data Research Seminar/Worldbank_GDPPC.csv",
  "gdp_per_capita_usd"
)

gdp_growth <- read_wb_long(
  "C:/Users/noelb/OneDrive/Desktop/Raw Data Research Seminar/Worldbank_GDP Growth.csv",
  "gdp_growth_pct"
)

unemployment <- read_wb_long(
  "C:/Users/noelb/OneDrive/Desktop/Raw Data Research Seminar/Unemployment RAte.csv",
  "unemployment_rate_pct"
)

# -----------------------------------------------------------------------------
# 3.  Load IMF interest rates -> long tibble
# -----------------------------------------------------------------------------

imf_raw <- read_csv(
  "C:/Users/noelb/OneDrive/Desktop/Raw Data Research Seminar/Interest_rates.csv",
  col_types = cols(.default = col_character())
)

imf_long <- imf_raw %>%
  select(COUNTRY, matches("^\\d{4}$")) %>%
  pivot_longer(
    cols      = -COUNTRY,
    names_to  = "Year",
    values_to = "interest_rate_pct"
  ) %>%
  rename(Country = COUNTRY) %>%
  mutate(
    Year              = as.integer(Year),
    interest_rate_pct = as.numeric(interest_rate_pct)
  ) %>%
  filter(!is.na(interest_rate_pct))

# -----------------------------------------------------------------------------
# 4.  Load BIS interest rates -> annual averages
# -----------------------------------------------------------------------------

bis_raw <- read_csv(
  "C:/Users/noelb/OneDrive/Desktop/Raw Data Research Seminar/BIS_Interest_rates.csv",
  skip      = 3,
  col_names = FALSE,
  col_types = cols(.default = col_character())
)

bis_dates <- as.character(bis_raw[1, ])
bis_data  <- bis_raw[-1, ]
bis_years <- str_extract(bis_dates, "\\d{4}$")
colnames(bis_data) <- c("Country", bis_years[-1])

bis_long <- bis_data %>%
  pivot_longer(cols = -Country, names_to = "Year", values_to = "rate") %>%
  mutate(Year = as.integer(Year), rate = as.numeric(rate)) %>%
  filter(!is.na(rate), Year >= 2010) %>%
  group_by(Country, Year) %>%
  summarise(interest_rate_bis = mean(rate, na.rm = TRUE), .groups = "drop") %>%
  mutate(Country = case_when(
    Country == "Korea"   ~ "South Korea",
    Country == "Türkiye" ~ "Turkey",
    TRUE                 ~ Country
  ))

ecb_rate <- bis_long %>%
  filter(Country == "Euro area") %>%
  select(Year, ecb_rate = interest_rate_bis)

# -----------------------------------------------------------------------------
# 5.  Build interest rate column with fallback logic
# -----------------------------------------------------------------------------

country_year_grid <- expand_grid(
  Country = countries_in_scope,
  Year    = 2010:2025
)

interest <- country_year_grid %>%
  left_join(imf_long, by = c("Country", "Year")) %>%
  left_join(bis_long, by = c("Country", "Year")) %>%
  left_join(ecb_rate, by = "Year") %>%
  mutate(
    interest_rate_pct = case_when(
      !is.na(interest_rate_pct)                           ~ interest_rate_pct,
      !is.na(interest_rate_bis)                           ~ interest_rate_bis,
      Country %in% eurozone_countries & !is.na(ecb_rate) ~ ecb_rate,
      TRUE                                                ~ NA_real_
    )
  ) %>%
  select(Country, Year, interest_rate_pct)

# -----------------------------------------------------------------------------
# 6.  Load OECD pension assets (% of GDP) -> long tibble
#
#     Sheet "Table": years (2010-2024) in cols D:R (row 6), countries in col B
#     Row 7 = "Reference area" label, rows 8+ = country data
#     Some in-scope countries appear under "Non-OECD economies" lower in the sheet
# -----------------------------------------------------------------------------

# Read the block: col B (country) through col R (2024), rows 6-103
pension_raw <- read_excel(
  "C:/Users/noelb/OneDrive/Desktop/Raw Data Research Seminar/Pension_Assets.xlsx",
  sheet     = "Table",
  range     = "B6:R103",
  col_names = FALSE
)

# Row 1 = year header (col1="Time period", col2=blank, cols 3-17 = 2010-2024)
# Row 2 = "Reference area" label -> skip
# Rows 3+ = country data
years     <- as.integer(as.numeric(unlist(pension_raw[1, 3:17])))
data_rows <- pension_raw[3:nrow(pension_raw), ]

# Assign clean names before any dplyr operations to avoid NA-name error
colnames(data_rows) <- c("Country", "blank_col", as.character(years))

pension_long <- data_rows %>%
  select(-blank_col) %>%
  filter(
    !is.na(Country),
    !str_detect(as.character(Country), "^Non-OECD"),
    !str_detect(as.character(Country), "^\u00a9"),
    !str_detect(as.character(Country), "^[[:space:]]*$")
  ) %>%
  mutate(Country = str_trim(str_remove(as.character(Country), "^\u00b7\\s*"))) %>%
  filter(nchar(Country) > 0) %>%
  pivot_longer(
    cols      = -Country,
    names_to  = "Year",
    values_to = "pension_assets_pct_gdp"
  ) %>%
  mutate(
    Year                   = as.integer(Year),
    pension_assets_pct_gdp = as.numeric(pension_assets_pct_gdp)
  ) %>%
  mutate(Country = case_when(
    Country == "Korea"           ~ "South Korea",
    Country == "T\u00fcrkiye"   ~ "Turkey",
    Country == "Slovak Republic" ~ "Slovakia",
    TRUE                         ~ Country
  )) %>%
  filter(Country %in% countries_in_scope, Year >= 2010, !is.na(pension_assets_pct_gdp))

# -----------------------------------------------------------------------------
# 7.  Join all series, apply scope + year filter
# -----------------------------------------------------------------------------

panel <- gdppc %>%
  full_join(gdp_growth,   by = c("Country", "Year")) %>%
  full_join(unemployment, by = c("Country", "Year")) %>%
  mutate(Country = case_when(
    Country == "Korea, Rep."     ~ "South Korea",
    Country == "Turkiye"         ~ "Turkey",
    Country == "Slovak Republic" ~ "Slovakia",
    TRUE                         ~ Country
  )) %>%
  filter(Country %in% countries_in_scope, Year >= 2010) %>%
  left_join(interest,     by = c("Country", "Year")) %>%
  left_join(pension_long, by = c("Country", "Year")) %>%
  arrange(Country, Year) %>%
  filter(!(is.na(gdp_per_capita_usd)    &
             is.na(gdp_growth_pct)         &
             is.na(unemployment_rate_pct)  &
             is.na(interest_rate_pct)      &
             is.na(pension_assets_pct_gdp)))

# -----------------------------------------------------------------------------
# 8.  Inspect
# -----------------------------------------------------------------------------

glimpse(panel)
print(panel, n = 20)

cat("\nCountries in final panel:\n")
print(sort(unique(panel$Country)))

cat("\nYear range:", range(panel$Year), "\n")
cat("Rows in panel:", nrow(panel), "\n")

cat("\nPension asset coverage (non-NA) by country:\n")
panel %>%
  group_by(Country) %>%
  summarise(
    years_with_pension = sum(!is.na(pension_assets_pct_gdp)),
    years_total        = n()
  ) %>%
  print(n = 50)

# -----------------------------------------------------------------------------
# 9.  Save
# -----------------------------------------------------------------------------

write_csv(panel, "C:/Users/noelb/OneDrive/Desktop/Raw Data Research Seminar/macro_panel_vf.csv")
cat("Saved: macro_panel.csv\n")