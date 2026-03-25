library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# ---- 1. countries to keep ----
eu27_ch <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia",
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece",
  "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
  "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia",
  "Slovenia", "Spain", "Sweden", "Switzerland"
)

# ---- 2. file path ----
age85_file <- "C:/Users/noelb/Downloads/Demographics_Eurostat.xlsx"

# ---- 3. read the country block only ----
# A = Country
# B:P = 2010:2024
raw_age85 <- read_excel(
  age85_file,
  sheet = 3,
  range = "A15:P1000",
  col_names = FALSE
)

# ---- 4. clean and reshape ----
age85_data <- raw_age85 %>%
  setNames(c("Country", as.character(2010:2024))) %>%
  mutate(
    Country = str_trim(Country),
    Country = recode(Country, "Slovak Rep." = "Slovakia")
  ) %>%
  filter(Country %in% eu27_ch) %>%
  pivot_longer(
    cols = `2010`:`2024`,
    names_to = "Year",
    values_to = "Share_85_plus_raw"
  ) %>%
  mutate(
    Share_85_plus = parse_number(as.character(Share_85_plus_raw)),
    Year = as.integer(Year)
  ) %>%
  select(Country, Year, Share_85_plus) %>%
  arrange(Country, Year)

# ---- 5. check result ----
View(age85_data)
glimpse(age85_data)
