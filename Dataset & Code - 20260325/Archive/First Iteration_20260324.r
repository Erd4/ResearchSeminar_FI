library(tidyverse)
library(plm)

setwd("/Users/giovannibrenni/Desktop/FS26/Research Seminar/Master")

# Load your data
df <- read_csv("MASTER_EU27_CH_OECD_20260324.csv")

# Basic structure
glimpse(df)


#--------Step 1: Banks Split------------
# How many unique banks, countries, years?
df %>% summarise(
  n_banks    = n_distinct(Entity.ID),
  n_countries = n_distinct(Country),
  n_years    = n_distinct(fiscal_year),
  n_obs      = n()
)

# Distribution of observations across bank types
df %>% 
  count(SNL_INDUSTRY, sort = TRUE) %>% 
  mutate(pct = round(n / sum(n) * 100, 1))

# Full summary by bank type: observations, unique banks, and country coverage
df %>% 
  group_by(SNL_INDUSTRY) %>% 
  summarise(
    n_obs       = n(),
    n_banks     = n_distinct(Entity.ID),
    n_countries = n_distinct(Country)
  ) %>% 
  arrange(desc(n_obs))


### -- Section to be updated accroding to new research focus----
# Observations per country
#df %>% 
  #filter(SNL_INDUSTRY %in% c("Savings Bank/Thrift/Mutual")) %>%
  #count(Country, sort = TRUE)

# Identify which countries have no savings banks in the sample
#df %>% 
  #filter(SNL_INDUSTRY == "Savings Bank/Thrift/Mutual") %>%
  #distinct(Country) %>%
  #pull(Country) -> savings_countries

# Which of your 28 countries are absent?
#df %>% 
  #distinct(Country) %>% 
  #filter(!Country %in% savings_countries)

### ---------------------------------------

# Full country list and cluster count
df_clean %>%
  summarise(
    n_country        = n_distinct(Country),
    n_country_region = n_distinct(Country_Region)
  )

# Which countries are in the sample
df_clean %>%
  distinct(Country) %>%
  arrange(Country) %>%
  print(n = Inf)




#--------Step 2: Filtering Dataset------------
# Filter to savings banks and banks — working dataset from here on
df_sav <- df %>%
  filter(SNL_INDUSTRY %in% c("Savings Bank/Thrift/Mutual", "Bank"))

# Create clean analytical sample — preserve df_sav as raw, never overwrite it
# Zero deposit ratio = zero deposits, almost certainly a data error
df_clean <- df_sav %>% 
  filter(!is.na(Deposit_Ratio) & Deposit_Ratio > 0) %>%
  filter(!is.na(Entity.ID))

# Check for implausible values in the denominator
df_clean %>% 
  summarise(
    n_zero_assets     = sum(Total_Assets == 0, na.rm = TRUE),
    n_negative_assets = sum(Total_Assets < 0, na.rm = TRUE)
  )

# Overall missingness by variable on clean analytical sample — in absolute counts
df_clean %>% 
  summarise(across(everything(), ~ sum(is.na(.)))) %>% 
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>% 
  arrange(desc(n_missing))

# Check missingness of key regressors by country
# Confirms coverage of new OECD countries for FinStructure and pension variable
df_clean %>%
  group_by(Country) %>%
  summarise(
    n_obs              = n(),
    miss_pop65         = sum(is.na(pop65_pct)),
    miss_finstructure  = sum(is.na(FinStructure)),
    miss_pension       = sum(is.na(pension_assets_pct_gdp))
  ) %>%
  arrange(desc(miss_finstructure)) %>%
  print(n = Inf)

# Observation count and FinStructure gaps by country
df_clean %>% 
  group_by(Country) %>% 
  summarise(
    n_obs              = n(),
    n_missing_fs       = sum(is.na(FinStructure)),
    n_years_missing_fs = n_distinct(fiscal_year[is.na(FinStructure)]),
    years_affected     = paste(sort(unique(fiscal_year[is.na(FinStructure)])), 
                               collapse = ", ")
  ) %>% 
  arrange(desc(n_obs)) %>% 
  print(n = Inf)

# Analytical sample size with and without FinStructure
df_clean %>% 
  summarise(
    n_obs_total         = n(),
    n_obs_with_fs       = sum(!is.na(FinStructure)),
    n_banks_with_fs     = n_distinct(Entity.ID[!is.na(FinStructure)]),
    n_countries_with_fs = n_distinct(Country[!is.na(FinStructure)])
  )



# Drop Deposit_Ratio >= 0.99 — clearly implausible, almost certainly data errors
# Keeps legitimate high-deposit savings banks while removing impossible values
df_clean <- df_clean %>%
  filter(Deposit_Ratio < 0.99)

# Confirm
df_clean %>%
  summarise(
    n_obs  = n(),
    max_dr = max(Deposit_Ratio, na.rm = TRUE)
  )


#--------Step 3: Within/Between Variance Decomposition------------

# Declare panel structure on clean analytical sample
library(plm)
df_clean <- df_clean %>% filter(!is.na(fiscal_year))
pd <- pdata.frame(df_clean, index = c("Entity.ID", "fiscal_year"))

# Decompose variance for key variables into within and between components
sapply(c("Deposit_Ratio", "pop65_pct", "FinStructure", "pension_assets_pct_gdp"), 
       function(var) {
         x <- pd[[var]]
         c(overall_sd = round(sd(as.numeric(x), na.rm = TRUE), 4),
           between_sd = round(sd(as.numeric(Between(x)), na.rm = TRUE), 4),
           within_sd  = round(sd(as.numeric(Within(x)), na.rm = TRUE), 4))
       }) %>% 
  t() %>% 
  as.data.frame() %>%
  mutate(within_between_ratio = round(within_sd / between_sd, 3))

#--------Step 4a: Descriptive Statistics------------

# Compute bank-level control variables first
df_clean <- df_clean %>%
  mutate(
    log_assets   = log(Total_Assets),
    loan_ratio   = Total_Net_Loans / Total_Assets,
    equity_ratio = Total_Equity / Total_Assets,
    roa          = Net_Income_Before_Taxes / Total_Assets
  )

# Core summary statistics for all key variables
df_clean %>%
  select(Deposit_Ratio, pop65_pct, FinStructure, pension_assets_pct_gdp,
         log_assets, loan_ratio, equity_ratio, roa,
         gdp_growth_pct, interest_rate_pct) %>%
  summarise(across(everything(), list(
    n      = ~ sum(!is.na(.)),
    mean   = ~ round(mean(., na.rm = TRUE), 4),
    sd     = ~ round(sd(., na.rm = TRUE), 4),
    min    = ~ round(min(., na.rm = TRUE), 4),
    median = ~ round(median(., na.rm = TRUE), 4),
    max    = ~ round(max(., na.rm = TRUE), 4)
  ))) %>%
  pivot_longer(everything(),
               names_to  = c("variable", "stat"),
               names_sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = stat, values_from = value)

#--------Step 4b: Descriptive Statistics by FinStructure type------------
# Summary statistics split by bank-based vs market-based countries
# Motivation for H1 — do deposit ratios differ systematically by system type?
df_clean %>%
  filter(!is.na(FinStructure)) %>%
  mutate(system_type = ifelse(FinStructure < 0, "Bank-based", "Market-based")) %>%
  group_by(system_type) %>%
  summarise(
    n_obs        = n(),
    n_countries  = n_distinct(Country),
    mean_deposit = round(mean(Deposit_Ratio, na.rm = TRUE), 4),
    sd_deposit   = round(sd(Deposit_Ratio, na.rm = TRUE), 4),
    mean_age65   = round(mean(pop65_pct, na.rm = TRUE), 4),
    mean_FinStr  = round(mean(FinStructure, na.rm = TRUE), 4),
    mean_pension = round(mean(pension_assets_pct_gdp, na.rm = TRUE), 4)
  )

#--------Step 4c: Correlation Matrix------------

# Correlation matrix for all key variables
# Key pairs to watch: pop65_pct x FinStructure, pension x FinStructure
df_clean %>%
  select(Deposit_Ratio, pop65_pct, FinStructure, pension_assets_pct_gdp,
         log_assets, loan_ratio, equity_ratio, roa,
         gdp_growth_pct, interest_rate_pct) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(3)



#--------Step 4d: Descriptive Statistics Plots (Final Version)------------
install.packages("ggrepel")
library(ggplot2)
library(gridExtra)
library(ggrepel)

# Consistent color scheme for system type across all plots
system_colors <- c("Bank-based" = "#2C5F2D", "Market-based" = "#B85042")

# Countries to label in plot 2 — selected for geographic/institutional diversity
label_countries <- c("Germany", "Italy", "Ireland", "Switzerland", "Sweden")

# Base dataset for plots 3 and 4 — requires FinStructure to be non-missing
country_year <- df_clean %>%
  filter(!is.na(pop65_pct), !is.na(FinStructure)) %>%
  group_by(Country, fiscal_year) %>%
  summarise(
    mean_age65   = mean(pop65_pct, na.rm = TRUE),
    mean_fs      = mean(FinStructure, na.rm = TRUE),
    mean_deposit = mean(Deposit_Ratio, na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  mutate(
    fiscal_year = as.integer(gsub("FY", "", fiscal_year)),
    system_type = ifelse(mean_fs < 0, "Bank-based", "Market-based")
  )

# Base dataset for plot 2 — uses all Age65 data, no FinStructure dependency
country_year_age <- df_clean %>%
  filter(!is.na(pop65_pct)) %>%
  group_by(Country, fiscal_year) %>%
  summarise(
    mean_age65 = mean(pop65_pct, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  mutate(fiscal_year = as.integer(gsub("FY", "", fiscal_year)))

# Plot 1 — Deposit Ratio distribution by financial system type
p1 <- df_clean %>%
  filter(!is.na(FinStructure)) %>%
  mutate(system_type = ifelse(FinStructure < 0, "Bank-based", "Market-based")) %>%
  ggplot(aes(x = Deposit_Ratio, fill = system_type)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = system_colors) +
  labs(title = "Deposit Ratio by Financial System Type",
       x = "Deposit Ratio", y = "Density", fill = "System Type") +
  theme_minimal(base_size = 13)

# Plot 2 — Age65 over time with selected country labels, legend removed
p2 <- country_year_age %>%
  ggplot(aes(x = fiscal_year, y = mean_age65, group = Country)) +
  geom_line(color = "grey60", alpha = 0.6, linewidth = 0.7) +
  geom_line(
    data = country_year_age %>% filter(Country %in% label_countries),
    aes(color = Country), linewidth = 1
  ) +
  geom_text_repel(
    data = country_year_age %>%
      filter(Country %in% label_countries, fiscal_year == 2024),
    aes(label = Country, color = Country),
    nudge_x = 0.5, direction = "y", hjust = 0, size = 3.5, show.legend = FALSE
  ) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2), 
                     expand = expansion(add = c(0, 3))) +
  labs(title = "Population Aged 65+ Over Time by Country",
       subtitle = "Selected countries highlighted",
       x = "Year", y = "Age 65+ (%)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# Plot 3 — FinStructure over time, legend removed
p3 <- country_year %>%
  ggplot(aes(x = fiscal_year, y = mean_fs,
             group = Country, color = system_type)) +
  geom_line(alpha = 0.8, linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = system_colors) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2)) +
  labs(title = "Financial Structure Over Time by Country",
       subtitle = "Dashed line = threshold between bank-based and market-based",
       x = "Year", y = "FinStructure Index") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# Plot 4 — Deposit Ratio vs Age65 scatter at country-year level
# key motivating plot for H1 — do slopes differ by system type?
p4 <- country_year %>%
  ggplot(aes(x = mean_age65, y = mean_deposit, color = system_type)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = system_colors) +
  labs(title = "Deposit Ratio vs. Age 65+ by System Type",
       x = "Age 65+ (%)", y = "Mean Deposit Ratio", color = "System Type") +
  theme_minimal(base_size = 13)

# Save to file at high resolution — ready for PPT and paper
ggsave("descriptive_plots.png",
       grid.arrange(p1, p2, p3, p4, ncol = 2),
       width = 16, height = 12, dpi = 300)

install.packages("RColorBrewer")
library(RColorBrewer)
# Plot 4 — filtered countries only
focus_countries <- c("Germany", "Switzerland", "France", "Italy", 
                     "Netherlands", "Spain", "Portugal")

p4 <- country_year %>%
  filter(Country %in% focus_countries) %>%
  ggplot(aes(x = mean_age65, y = mean_deposit, color = Country)) +
  geom_point(alpha = 0.6, size = 1.8) +
  geom_smooth(aes(group = system_type, linetype = system_type),
              method = "lm", se = TRUE, color = "black", linewidth = 0.8) +
  scale_color_brewer(palette = "Paired") +
  labs(title = "Deposit Ratio vs. Age 65+ by System Type",
       subtitle = "Selected Continental European countries",
       x = "Age 65+ (%)", y = "Mean Deposit Ratio",
       color = "Country", linetype = "System Type") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right")


#--------Step 5: Regression Analysis------------

library(fixest)
library(car)
library(plm)

# Model 1 — Baseline OLS: aging only, no fixed effects, no controls
# Establishes unconditional relationship between aging and deposit ratios
# Standard errors clustered at country level — treatment assignment level
model1 <- feols(
  Deposit_Ratio ~ pop65_pct,
  data    = df_clean,
  cluster = ~Country
)
summary(model1)

# Model 2 — Add FinStructure as standalone (no interaction yet)
# Does financial system structure affect deposit ratios unconditionally?
# Standard errors clustered at country level
model2 <- feols(
  Deposit_Ratio ~ pop65_pct + FinStructure,
  data    = df_clean,
  cluster = ~Country
)
summary(model2)

# Model 3 — Add interaction term (core test of H1)
# Does aging interact with financial structure?
# First formal test of H1
# Standard errors clustered at country level
model3 <- feols(
  Deposit_Ratio ~ pop65_pct * FinStructure,
  data    = df_clean,
  cluster = ~Country
)
summary(model3)

# VIF check after Model 3 — assess multicollinearity between main effects
# and interaction term
# Requires standard lm object — refit without clustering for VIF only
model3_lm <- lm(
  Deposit_Ratio ~ pop65_pct * FinStructure,
  data = df_clean
)
vif(model3_lm, type = "predictor")

# Model 4a — Add ARR only on top of Model 3
# Tests whether pension generosity affects deposit ratios independently
# Standard errors clustered at country level
model4a <- feols(
  Deposit_Ratio ~ pop65_pct * FinStructure + pension_assets_pct_gdp,
  data    = df_clean,
  cluster = ~Country
)
summary(model4a)

# Model 4b — Add pension system dummies on top of Model 4a
# Bismarckian and Beveridgean entered separately, Multipillar is baseline
# Standard errors clustered at country level
model4b <- feols(
  Deposit_Ratio ~ pop65_pct * FinStructure + 
    pension_assets_pct_gdp + Bismarckian + Beveridgean,
  data    = df_clean,
  cluster = ~Country
)
summary(model4b)

# VIF check after Model 4b — key concern: Bismarckian x FinStructure overlap
model4b_lm <- lm(
  Deposit_Ratio ~ pop65_pct * FinStructure + 
    pension_assets_pct_gdp + Bismarckian + Beveridgean,
  data = df_clean
)
vif(model4b_lm, type = "predictor")

# Model 5 — Bank and macro controls (WORK IN PROGRESS — controls not yet collected)
# To be added: log(Total Assets), Loan Ratio, Equity Ratio, ROA, GDP growth, interest rate
# model5 <- feols(
#   Deposit_Ratio ~ pop65_pct * FinStructure +
#     pension_assets_pct_gdp + Bismarckian + Beveridgean +
#     log_assets + loan_ratio + equity_ratio + roa +
#     gdp_growth + interest_rate,
#   data    = df_clean,
#   cluster = ~Country
# )

# Model 6 — Preferred specification
# Two-way fixed effects: bank FE + year FE
# Bank FE absorbs all time-invariant bank characteristics
# Year FE absorbs common macro shocks affecting all banks in a given year
# Standard errors clustered at country level — treatment assignment level
# Wild cluster bootstrap used for preferred inference — see Step 6a
# Reference: Cameron, Gelbach & Miller (2008)
model6_preliminary <- feols(
  Deposit_Ratio ~ pop65_pct * FinStructure + 
    pension_assets_pct_gdp + Bismarckian + Beveridgean |
    Entity.ID + fiscal_year,
  data    = df_clean,
  cluster = ~Country
)
summary(model6_preliminary)

#--------Step 5d: Main Regression Table------------

# Sequential model building table
# All models use standard errors clustered at country level
# Bootstrap p-values added for Model 6 after Step 6a is run
# Intercept suppressed — uninformative in this context
etable(model1, model2, model3, model4a, model4b, model6_preliminary,
       headers  = c("(1) Baseline", "(2) + FinStr",
                    "(3) + Interact", "(4a) + ARR",
                    "(4b) + Pension", "(6) FE"),
       digits   = 3,
       se.below = TRUE,
       depvar   = FALSE,
       drop     = "Intercept",
       dict     = c(
         "pop65_pct"              = "Age 65+",
         "FinStructure"           = "FinStructure",
         "pension_assets_pct_gdp"  = "ARR",
         "Bismarckian"            = "Bismarckian",
         "Beveridgean"            = "Beveridgean",
         "pop65_pct:FinStructure" = "Age 65+ x FinStructure"
       ),
       extralines = list(
         "_SE Type"       = c("Clustered", "Clustered", "Clustered",
                              "Clustered", "Clustered", "Clustered"),
         "_Fixed Effects" = c("No", "No", "No", "No", "No", "Yes")
       ),
       notes = "Standard errors clustered at country level in parentheses throughout. Wild cluster bootstrap p-values reported separately for Model 6 following Cameron, Gelbach & Miller (2008), given the small number of clusters (N=26 countries).",
       style.tex = style.tex("base"),
       tex = TRUE)

#--------Step 6: Diagnostic Tests------------

# ============================================================
# WILD CLUSTER BOOTSTRAP — CORRECTED IMPLEMENTATION
# Reference: Cameron, Gelbach & Miller (2008)
#            Cameron & Miller (2015, Journal of Human Resources)
# ============================================================

# ============================================================
# WILD CLUSTER BOOTSTRAP — FINAL VERSION
# Reference: Cameron, Gelbach & Miller (2008)
#            Cameron & Miller (2015, Journal of Human Resources)
# ============================================================

install.packages(c("dqrng", "JuliaConnectoR", "dreamerr", "collapse", 
                   "gtools", "Rcpp", "RcppArmadillo", "Matrix", "foreach"))
library(fwildclusterboot)

# Recreate exact dataset used by Model 6
df_boot_base <- df_clean %>%
  filter(!is.na(FinStructure) & !is.na(pension_assets_pct_gdp)) %>%
  arrange(Entity.ID, fiscal_year)

# Refit Model 6 to identify and extract singleton fixed effects
model6_check <- feols(
  Deposit_Ratio ~ pop65_pct * FinStructure + 
    pension_assets_pct_gdp + Bismarckian + Beveridgean |
    Entity.ID + fiscal_year,
  data    = df_boot_base,
  cluster = ~Country
)

# Remove singletons — boottest cannot handle them
removed_fe <- model6_check$fixef_removed$Entity.ID

df_boot_clean <- df_boot_base %>%
  filter(!Entity.ID %in% removed_fe) %>%
  mutate(
    Entity.ID   = as.numeric(Entity.ID),
    fiscal_year = as.numeric(gsub("FY", "", fiscal_year))
  )

# Refit model on clean dataset for bootstrap
model6_boot <- feols(
  Deposit_Ratio ~ pop65_pct * FinStructure + pension_assets_pct_gdp |
    Entity.ID + fiscal_year,
  data    = df_boot_clean,
  cluster = ~Country
)

# Run wild cluster bootstrap with Rademacher weights
set.seed(42)
dqrng::dqset.seed(42)

boot_age65    <- boottest(model6_boot, clustid = "Country", param = "pop65_pct",             B = 120)
boot_finstr   <- boottest(model6_boot, clustid = "Country", param = "FinStructure",           B = 120)
boot_interact <- boottest(model6_boot, clustid = "Country", param = "pop65_pct:FinStructure", B = 120)
boot_arr      <- boottest(model6_boot, clustid = "Country", param = "pension_assets_pct_gdp",  B = 120)

# Summaries and diagnostic plots
summary(boot_age65)
summary(boot_finstr)
summary(boot_interact)
summary(boot_arr)

plot(boot_age65)
plot(boot_finstr)
plot(boot_interact)
plot(boot_arr)



#----Step 6a continued: Final Regression Table with Bootstrap p-values----
# Extract p-values from boottest objects
p_boot_age65    <- boot_age65$p_val
p_boot_finstr   <- boot_finstr$p_val
p_boot_interact <- boot_interact$p_val
p_boot_arr      <- boot_arr$p_val

# Format helper
format_boot_p <- function(p, B = 9999) {
  min_p <- 1 / B
  if (p < min_p) return(paste0("< ", round(min_p, 4)))
  return(as.character(round(p, 4)))
}

# Now build the table
etable(model1, model2, model3, model4a, model4b, model6_preliminary,
       headers  = c("(1) Baseline", "(2) + FinStr",
                    "(3) + Interact", "(4a) + ARR",
                    "(4b) + Pension", "(6) FE"),
       digits   = 3,
       se.below = TRUE,
       depvar   = FALSE,
       drop     = "Intercept",
       dict     = c(
         "pop65_pct"              = "Age 65+",
         "FinStructure"           = "FinStructure",
         "pension_assets_pct_gdp"  = "ARR",
         "Bismarckian"            = "Bismarckian",
         "Beveridgean"            = "Beveridgean",
         "pop65_pct:FinStructure" = "Age 65+ x FinStructure"
       ),
       extralines = list(
         "_SE Type"               = c("Clustered", "Clustered", "Clustered",
                                      "Clustered", "Clustered", "Clustered"),
         "_Fixed Effects"         = c("No", "No", "No", "No", "No", "Yes"),
         "_Boot p (Age65)"        = c("", "", "", "", "",
                                      format_boot_p(p_boot_age65)),
         "_Boot p (FinStructure)" = c("", "", "", "", "",
                                      format_boot_p(p_boot_finstr)),
         "_Boot p (Age65xFS)"     = c("", "", "", "", "",
                                      format_boot_p(p_boot_interact)),
         "_Boot p (ARR)"          = c("", "", "", "", "",
                                      format_boot_p(p_boot_arr))
       ),
       notes = "Standard errors clustered at country level in parentheses. Wild cluster bootstrap p-values reported for Model 6 following Cameron, Gelbach & Miller (2008). Bootstrap uses Rademacher weights with 9,999 iterations.",
       style.tex = style.tex("base"),
       tex = TRUE)


#----Step 6b: Hausman Test (FE vs RE)----

# Tests whether fixed effects or random effects is the appropriate estimator
# Expected result: reject null -> FE confirmed as correct estimator
# Reference: Hausman (1978)
pd <- pdata.frame(df_boot_base, index = c("Entity.ID", "fiscal_year"))

fe_hausman <- plm(Deposit_Ratio ~ pop65_pct * FinStructure + pension_assets_pct_gdp,
                  data  = pd,
                  model = "within")

re_hausman <- plm(Deposit_Ratio ~ pop65_pct * FinStructure + pension_assets_pct_gdp,
                  data  = pd,
                  model = "random")

phtest(fe_hausman, re_hausman)

#----Step 6c: Serial Correlation Test----

# Breusch-Godfrey/Wooldridge test for serial correlation in panel residuals
# Expected: significant — deposit ratios are persistent over time
# Addressed by clustering standard errors following Petersen (2009)
pbgtest(fe_hausman)

#----Step 6d: Robustness — Alternative SE Specifications (Appendix)----

# Two-way clustering: bank + country level
# More conservative than country-only clustering
# Follows Doerr et al. (2022) directly
model6_twoway <- feols(
  Deposit_Ratio ~ pop65_pct * FinStructure + 
    pension_assets_pct_gdp + Bismarckian + Beveridgean |
    Entity.ID + fiscal_year,
  data    = df_boot_base,
  cluster = ~Entity.ID + Country
)

# Collapsed to country-year level — most conservative specification
# Reduces sample to ~310 observations
# Eliminates bank-level variation entirely
df_country_year <- df_boot_base %>%
  group_by(Country, fiscal_year) %>%
  summarise(
    Deposit_Ratio         = mean(Deposit_Ratio, na.rm = TRUE),
    pop65_pct             = first(pop65_pct),
    FinStructure          = first(FinStructure),
    pension_assets_pct_gdp = first(pension_assets_pct_gdp),
    Bismarckian           = first(Bismarckian),
    Beveridgean           = first(Beveridgean),
    n_banks               = n(),
    .groups               = "drop"
  )

model6_collapsed <- feols(
  Deposit_Ratio ~ pop65_pct * FinStructure + 
    pension_assets_pct_gdp + Bismarckian + Beveridgean |
    Country + fiscal_year,
  data    = df_country_year,
  cluster = ~Country
)

# Robustness table for appendix
etable(model6_preliminary, model6_twoway, model6_collapsed,
       headers  = c("Country Cluster", "Two-way Cluster", "Country-Year"),
       digits   = 3,
       se.below = TRUE,
       depvar   = FALSE,
       drop     = "Intercept",
       dict     = c(
         "pop65_pct"              = "Age 65+",
         "FinStructure"           = "FinStructure",
         "pension_assets_pct_gdp"  = "ARR",
         "pop65_pct:FinStructure" = "Age 65+ x FinStructure"
       ),
       extralines = list(
         "_SE Type"    = c("Country Cluster", "Two-way Cluster", "Country Cluster"),
         "_Obs. Level" = c("Bank-Year", "Bank-Year", "Country-Year")
       ),
       notes = "Robustness checks for Model 6. Column 1 is the baseline with country-clustered SEs. Column 2 uses two-way clustering following Doerr et al. (2022). Column 3 collapses to country-year level — the most conservative specification.",
       style.tex = style.tex("base"),
       tex = TRUE)

#----Step 6e: DK Alternative (NOT ACTIVE)----

# Driscoll-Kraay standard errors kept for reference only
# NOT used as primary specification — three reasons:
# 1. Requires large T asymptotics: T=14 violates this (Driscoll & Kraay 1998)
# 2. Well-calibrated only at T=40+: our T=14 is well below (Hoechle 2007)
# 3. Consistency unproven with year FE included (Vogelsang 2012)

# model6_dk <- feols(
#   Deposit_Ratio ~ pop65_pct * FinStructure +
#     pension_assets_pct_gdp + Bismarckian + Beveridgean |
#     Entity.ID + fiscal_year,
#   data     = df_clean,
#   panel.id = ~Entity.ID + fiscal_year,
#   vcov     = "DK"
# )
# summary(model6_dk)

# model5_dk <- feols(
#   Deposit_Ratio ~ pop65_pct * FinStructure +
#     pension_assets_pct_gdp + Bismarckian + Beveridgean +
#     log_assets + loan_ratio + equity_ratio + roa +
#     gdp_growth + interest_rate,
#   data     = df_clean,
#   panel.id = ~Entity.ID + fiscal_year,
#   vcov     = "DK"
# )

# etable(model1, model2, model3, model4a, model4b, model6_dk,
#        headers  = c("(1) Baseline", "(2) + FinStr",
#                     "(3) + Interact", "(4a) + ARR",
#                     "(4b) + Pension", "(6) FE + DK"),
#        digits   = 3,
#        se.below = TRUE,
#        depvar   = FALSE,
#        drop     = "Intercept",
#        dict     = c(
#          "pop65_pct"              = "Age 65+",
#          "FinStructure"           = "FinStructure",
#          "pension_assets_pct_gdp"  = "ARR",
#          "Bismarckian"            = "Bismarckian",
#          "Beveridgean"            = "Beveridgean",
#          "pop65_pct:FinStructure" = "Age 65+ x FinStructure"
#        ),
#        notes = "DK standard errors for reference only. Not recommended: T=14 violates large-T requirement.",
#        style.tex = style.tex("base"),
#        tex = TRUE)