#--------Libraries------------
# install.packages(c("tidyverse", "plm", "fixest", "car", "stargazer"))
library(tidyverse)
library(plm)
library(fixest)
library(car)
library(stargazer)

#--------Step 0: Load & Inspect Raw Data------------

setwd("/Users/giovannibrenni/Desktop/FS26/Research Seminar/Master")
df_raw <- read_csv("Dataset & Code - 20260325/MASTER_EU27_CH_OECD_20260325.csv")

# Inspect structure
glimpse(df_raw)

# Column names only — decide what to keep
names(df_raw)

# Basic dimensions
cat("Rows:", nrow(df_raw), "\nColumns:", ncol(df_raw))


#--------Step 1: Select Relevant Columns & Compute Derived Variables------------
# Potentially: add all columns and block out those you don't wnat for now
df <- df_raw %>%
  select(
    # Identifiers
    Entity.ID,
    Entity.Name,
    fiscal_year,
    SNL_INDUSTRY,
    Country,
    Country_Region,
    
    # Dependent variable
    Deposit_Ratio,
    
    # Main independent variables
    pop65_pct,
    FinStructure,
    Share_85plus,
    
    # Raw bank variables — needed to construct controls below
    Total_Assets,
    Tier_1_Ratio,
    Net_Interest_Income,
    Net_Income,
    Total_Equity,
    
    # Macro controls
    gdp_growth_pct,
    interest_rate_pct,
    GDP_per_Capita_PPP,
    unemployment_rate_pct
  ) %>%
  # Construct additional bank controls
  mutate(
    log_assets   = log(Total_Assets),
    nim          = Net_Interest_Income / Total_Assets,
    equity_ratio = Total_Equity / Total_Assets
  ) %>%
  select(-Total_Assets, -Net_Interest_Income, -Total_Equity) %>%
  # Pension system type dummies (base case = Multipillar)
  mutate(
    Bismarckian = as.integer(Country %in% c(
      "Austria", "Belgium", "Bulgaria", "Croatia", "Czechia",
      "France", "Germany", "Greece", "Hungary", "Italy",
      "Japan", "Luxembourg", "Poland", "Portugal", "Romania",
      "Slovakia", "Slovenia", "South Korea", "Spain", "Türkiye"
    )),
    Beveridgean = as.integer(Country %in% c(
      "Australia", "Canada", "Cyprus", "Ireland", "Malta",
      "Netherlands", "New Zealand", "United Kingdom"
    ))
  )

# Confirm
glimpse(df)


#--- IMPORTANT: All variables controlled from here!!!-----
# Add new variables to the relevant group here — flows through all steps automatically
vars_dependent  <- c("Deposit_Ratio")
vars_main_iv    <- c("pop65_pct", "FinStructure","Share_85plus")
vars_pension    <- c("Bismarckian", "Beveridgean")
vars_bank       <- c("log_assets", "Tier_1_Ratio", "nim", "equity_ratio")
vars_macro      <- c("gdp_growth_pct", "interest_rate_pct", 
                     "GDP_per_Capita_PPP", "unemployment_rate_pct")

vars_all <- c(vars_dependent, vars_main_iv, vars_pension, vars_bank, vars_macro)


#--------Step 2: Filter & Clean Dataset------------
# Composition by bank type — shows what we start with before filtering
df %>%
  count(SNL_INDUSTRY, sort = TRUE) %>%
  mutate(
    pct_obs = round(n / sum(n) * 100, 1),
    n_banks = sapply(SNL_INDUSTRY, function(x) n_distinct(df$Entity.ID[df$SNL_INDUSTRY == x]))
  )

#-------Step 2a: Bank Type Selection------------
# Filter to relevant bank types
# Define bank types to include — adjust here to change sample
bank_types <- c("Savings Bank/Thrift/Mutual", "Bank")
# Filter to relevant bank types
df_banks <- df %>%
  filter(SNL_INDUSTRY %in% bank_types)


#-------Step 2b: Check & identifiers------------
df_banks %>%
  summarise(
    n_obs          = n(),
    na_entity_id   = sum(is.na(Entity.ID)),
    na_fiscal_year = sum(is.na(fiscal_year))
  )
# Show unique banks with missing identifiers
df_banks %>%
  filter(is.na(Entity.ID) | is.na(fiscal_year)) %>%
  distinct(Entity.ID, Entity.Name, Country, SNL_INDUSTRY) %>%
  print(n = Inf)
# Filter NAs
df_banks <- df_banks %>%
  filter(!is.na(Entity.ID)) %>%
  filter(!is.na(fiscal_year))
# Confirm
df_banks %>%
  summarise(
    n_obs   = n(),
    n_banks = n_distinct(Entity.ID)
  )


#-------Step 2c: Basic descriptive statistics------------
df_banks %>%
  select(all_of(vars_all)) %>%
  summarise(across(everything(), list(
    n      = ~ sum(!is.na(.)),
    mean   = ~ round(mean(., na.rm = TRUE), 3),
    sd     = ~ round(sd(., na.rm = TRUE), 3),
    min    = ~ round(min(., na.rm = TRUE), 3),
    median = ~ round(median(., na.rm = TRUE), 3),
    max    = ~ round(max(., na.rm = TRUE), 3)
  ))) %>%
  pivot_longer(everything(),
               names_to  = c("variable", "stat"),
               names_sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = stat, values_from = value)


#-------Step 2d: Identify and remove problematic observations------------

# Flag observations violating data quality conditions
# Note: dropping observation-level not bank-level
obs_to_drop <- df_banks %>%
  filter(
    (!is.na(Deposit_Ratio) & Deposit_Ratio >= 1)       |
      (is.finite(log_assets) & log_assets <= 0)           |
      (!is.na(equity_ratio) & equity_ratio <= 0)          |
      (!is.na(equity_ratio) & equity_ratio > 1)           |
      (!is.na(Tier_1_Ratio) & Tier_1_Ratio <= 0)
  )

# How many observations are we losing and why?
cat("Total observations to drop:", nrow(obs_to_drop), "\n")
cat("Pct of sample:", round(nrow(obs_to_drop) / nrow(df_banks) * 100, 2), "%\n")

# Breakdown by violation type
obs_to_drop %>%
  summarise(
    dr_above_1        = sum(!is.na(Deposit_Ratio) & Deposit_Ratio >= 1),
    negative_assets   = sum(is.finite(log_assets) & log_assets <= 0),
    negative_equity   = sum(!is.na(equity_ratio) & equity_ratio <= 0),
    equity_above_1    = sum(!is.na(equity_ratio) & equity_ratio > 1),
    negative_tier1    = sum(!is.na(Tier_1_Ratio) & Tier_1_Ratio <= 0)
  )

# Apply filter — drop only problematic observations
df_clean <- df_banks %>%
  filter(
    is.na(Deposit_Ratio) | Deposit_Ratio < 1,
    !is.finite(log_assets) | log_assets > 0,
    is.na(equity_ratio) | equity_ratio > 0,
    is.na(equity_ratio) | equity_ratio <= 1,
    is.na(Tier_1_Ratio) | Tier_1_Ratio > 0
  ) %>%
  filter(!is.na(Deposit_Ratio)) %>%
  filter(Deposit_Ratio > 0)

# Confirm sample size
df_clean %>%
  summarise(
    n_obs   = n(),
    n_banks = n_distinct(Entity.ID),
    min_t1  = round(min(Tier_1_Ratio, na.rm = TRUE), 2),
    max_t1  = round(max(Tier_1_Ratio, na.rm = TRUE), 2)
  )

# Updated descriptive statistics after cleaning
df_clean %>%
  select(all_of(vars_all)) %>%
  summarise(across(everything(), list(
    n      = ~ sum(!is.na(.)),
    mean   = ~ round(mean(., na.rm = TRUE), 3),
    sd     = ~ round(sd(., na.rm = TRUE), 3),
    min    = ~ round(min(., na.rm = TRUE), 3),
    median = ~ round(median(., na.rm = TRUE), 3),
    max    = ~ round(max(., na.rm = TRUE), 3)
  ))) %>%
  pivot_longer(everything(),
               names_to  = c("variable", "stat"),
               names_sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = stat, values_from = value)


#-------Step 2f: GMM-based threshold for non-deposit-taking banks------------
install.packages("mclust")
library(mclust)

# Fit GMM on bank-level mean Deposit_Ratio
# Using df_clean — already filtered for invalid observations
bank_dr <- df_clean %>%
  filter(!is.na(Deposit_Ratio)) %>%
  group_by(Entity.ID) %>%
  summarise(mean_dr = mean(Deposit_Ratio, na.rm = TRUE), .groups = "drop")


#--- 2f-i: KDE — visual inspection and natural break detection------------

# Compute KDE on bank-level mean Deposit Ratio
kde_fit <- density(bank_dr$mean_dr, na.rm = TRUE)
kde_df  <- data.frame(x = kde_fit$x, y = kde_fit$y)

# Find local minima (valleys) in the lower tail of the KDE
local_mins <- kde_df %>%
  mutate(
    dy_ahead  = dplyr::lead(y, 5) - y,
    dy_behind = y - dplyr::lag(y, 5),
    is_min    = dy_ahead > 0 & dy_behind < 0
  ) %>%
  filter(is_min & x > 0.05 & x < 0.20) %>%
  arrange(y)

cat("Local minima in lower tail (0.05–0.20):\n")
print(local_mins %>% select(x, y))

# Select the deepest valley in the lower tail
valley <- local_mins %>% slice_head(n = 1)
natural_break <- valley$x[1]

cat(sprintf("\nSelected natural break point: %.4f (density = %.4f)\n", natural_break, valley$y[1]))

# Plot with peaks and valley marked
bank_dr %>%
  ggplot(aes(x = mean_dr)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 0.02,
                 fill = "grey80",
                 color = "white",
                 alpha = 0.4) +
  geom_density(fill = "#2C5F2D", alpha = 0.5, linewidth = 0.8) +
  geom_vline(xintercept = natural_break,
             linetype = "dashed",
             color = "#B85042",
             linewidth = 1) +
  annotate("point", x = natural_break, y = valley$y[1],
           color = "#B85042", size = 3) +
  annotate("text",
           x     = natural_break + 0.03,
           y     = max(kde_fit$y) * 0.5,
           label = paste0("KDE cutoff: ", round(natural_break, 3)),
           color = "#B85042", size = 4) +
  labs(title = "KDE of Bank-Level Mean Deposit Ratio",
       subtitle = "Deepest valley in lower tail used as threshold",
       x = "Mean Deposit Ratio", y = "Density") +
  theme_minimal(base_size = 13)


#-------Step 2f-ii: Fit 2-component GMM to validate KDE cutoff------------

# Fit 2-component GMM
gmm_fit <- Mclust(bank_dr$mean_dr, G = 2)

# Identify which component is the non-deposit cluster (lower mean)
comp_means <- gmm_fit$parameters$mean
low_comp   <- which.min(comp_means)
high_comp  <- which.max(comp_means)

cat("Component summary:\n")
cat(sprintf("  Non-deposit:  μ = %.4f, σ = %.4f, weight = %.3f\n",
            gmm_fit$parameters$mean[low_comp],
            sqrt(gmm_fit$parameters$variance$sigmasq[low_comp]),
            gmm_fit$parameters$pro[low_comp]))
cat(sprintf("  Deposit:      μ = %.4f, σ = %.4f, weight = %.3f\n",
            gmm_fit$parameters$mean[high_comp],
            sqrt(gmm_fit$parameters$variance$sigmasq[high_comp]),
            gmm_fit$parameters$pro[high_comp]))

# Extract posterior probabilities
bank_dr$p_non_deposit <- gmm_fit$z[, low_comp]
bank_dr$p_deposit     <- gmm_fit$z[, high_comp]
bank_dr$cluster       <- ifelse(bank_dr$p_non_deposit > 0.5,
                                "Non-Deposit", "Deposit")

# Check GMM posterior probability at the KDE cutoff
p_at_cutoff <- predict(gmm_fit, newdata = natural_break)$z[, low_comp]
cat(sprintf("\n--- GMM validation of KDE cutoff ---\n"))
cat(sprintf("KDE cutoff: %.4f\n", natural_break))
cat(sprintf("P(non-deposit) at KDE cutoff: %.4f%%\n", p_at_cutoff * 100))
cat(sprintf("GMM confirms KDE cutoff: %s\n",
            ifelse(p_at_cutoff > 0.5, "YES (majority non-deposit)", "NO (majority deposit)")))

# Confirm all banks below KDE cutoff are in non-deposit cluster
below_cutoff <- bank_dr %>% filter(mean_dr < natural_break)
cat(sprintf("\nBanks with mean DR < %.4f: %d\n", natural_break, nrow(below_cutoff)))
if (nrow(below_cutoff) > 0) {
  cat(sprintf("  Min P(non-deposit): %.6f\n", min(below_cutoff$p_non_deposit)))
  cat(sprintf("  All classified as non-deposit: %s\n",
              ifelse(all(below_cutoff$p_non_deposit > 0.5), "YES", "NO")))
}

#-------Step 2f-iii: GMM plot with components and threshold------------

dr_seq  <- seq(0, 1, length.out = 1000)
mu1     <- gmm_fit$parameters$mean[low_comp]
mu2     <- gmm_fit$parameters$mean[high_comp]
sigma1  <- sqrt(gmm_fit$parameters$variance$sigmasq[low_comp])
sigma2  <- sqrt(gmm_fit$parameters$variance$sigmasq[high_comp])
pi1     <- gmm_fit$parameters$pro[low_comp]
pi2     <- gmm_fit$parameters$pro[high_comp]

gmm_df <- data.frame(
  dr          = dr_seq,
  overall     = pi1 * dnorm(dr_seq, mu1, sigma1) + pi2 * dnorm(dr_seq, mu2, sigma2),
  non_deposit = pi1 * dnorm(dr_seq, mu1, sigma1),
  deposit     = pi2 * dnorm(dr_seq, mu2, sigma2)
)

bank_dr %>%
  ggplot(aes(x = mean_dr)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 0.02,
                 fill = "grey80",
                 color = "white",
                 alpha = 0.7) +
  geom_line(data = gmm_df, aes(x = dr, y = overall),
            color = "black", linewidth = 1) +
  geom_line(data = gmm_df, aes(x = dr, y = non_deposit),
            color = "#B85042", linewidth = 0.8, linetype = "dashed") +
  geom_line(data = gmm_df, aes(x = dr, y = deposit),
            color = "#2C5F2D", linewidth = 0.8, linetype = "dashed") +
  geom_vline(xintercept = natural_break,
             linetype = "dotted",
             color = "#B85042",
             linewidth = 1) +
  annotate("text", x = natural_break + 0.03, y = 4,
           label = paste0("KDE cutoff: ", round(natural_break * 100, 1), "%"),
           color = "#B85042", size = 4) +
  labs(title = "GMM Fit on Bank-Level Mean Deposit Ratio",
       subtitle = "Red dashed = non-deposit component | Green dashed = deposit component | Dotted = KDE cutoff",
       x = "Mean Deposit Ratio", y = "Density") +
  theme_minimal(base_size = 13)

#-------Step 2f-iv: Apply filter based on KDE natural break------------

# Remove observations below KDE natural break
# A bank with DR below the natural break in a given year
# is not functioning as a deposit-taking institution that year
n_before <- nrow(df_clean)

df_clean <- df_clean %>%
  filter(Deposit_Ratio > natural_break) %>%
  filter(equity_ratio > 0)

cat(sprintf("KDE natural break used as threshold: %.4f\n", natural_break))
cat(sprintf("Before filter: %d obs\n", n_before))
cat(sprintf("After filter: %d obs\n", nrow(df_clean)))
cat(sprintf("Removed: %d obs\n", n_before - nrow(df_clean)))


#-------Step 2g: Descriptive statistics after GMM/KDE filter------------

df_clean %>%
  select(all_of(vars_all)) %>%
  summarise(across(everything(), list(
    n      = ~ sum(!is.na(.)),
    mean   = ~ round(mean(., na.rm = TRUE), 3),
    sd     = ~ round(sd(., na.rm = TRUE), 3),
    min    = ~ round(min(., na.rm = TRUE), 3),
    median = ~ round(median(., na.rm = TRUE), 3),
    max    = ~ round(max(., na.rm = TRUE), 3)
  ))) %>%
  pivot_longer(everything(),
               names_to  = c("variable", "stat"),
               names_sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = stat, values_from = value)


#--------Step 3: Within/Between Variance Decomposition------------

library(plm)

# Declare panel structure at bank x year level
# Time FE only — identification comes from cross-sectional variation
pd <- pdata.frame(df_clean, index = c("Entity.ID", "fiscal_year"))

# Decompose variance — between variation is what drives identification
# since we only use time FE (no bank FE)
sapply(vars_all, function(var) {
  x <- pd[[var]]
  c(overall_sd          = round(sd(as.numeric(x), na.rm = TRUE), 4),
    between_sd          = round(sd(as.numeric(Between(x)), na.rm = TRUE), 4),
    within_sd           = round(sd(as.numeric(Within(x)), na.rm = TRUE), 4))
}) %>%
  t() %>%
  as.data.frame() %>%
  mutate(within_between_ratio = round(within_sd / between_sd, 3))

#--------Step 4a: Descriptive Statistics — full sample------------

df_clean %>%
  select(all_of(vars_all)) %>%
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
    mean_FinStr  = round(mean(FinStructure, na.rm = TRUE), 4)
  )

#--------Step 4c: Correlation Matrix------------

cor_matrix <- df_clean %>%
  select(all_of(vars_all)) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(3)

stargazer(cor_matrix, type = "text", title = "Correlation Matrix")


#--------Step 5: Panel Regressions (Country + Time FE, clustered SEs)------------


#--------H1: Demographic Ageing and Deposit Ratios-------------------
# Model 1: pop65 only
h1.1 <- feols(Deposit_Ratio ~ pop65_pct
            | fiscal_year,
            data    = df_clean,
            cluster = ~Country)

# Model 2: + Pension system dummies
h1.2 <- feols(Deposit_Ratio ~ pop65_pct +
              Bismarckian + Beveridgean
            | fiscal_year,
            data    = df_clean,
            cluster = ~Country)

# Model 3: + Bank & macro controls (full specification)
h1.3 <- feols(Deposit_Ratio ~ pop65_pct +
              Bismarckian + Beveridgean +
              log_assets + Tier_1_Ratio + nim + equity_ratio +
              gdp_growth_pct + interest_rate_pct +
              GDP_per_Capita_PPP + unemployment_rate_pct
            | fiscal_year,
            data    = df_clean,
            cluster = ~Country)

# Regression table - H1
etable(h1.1, h1.2, h1.3,
       headers = c("(1) Base", "(2) +Pension", "(3) Full"),
       dict = c(pop65_pct             = "Population 65+%",
                Bismarckian           = "Bismarckian",
                Beveridgean           = "Beveridgean",
                log_assets            = "Log(Assets)",
                Tier_1_Ratio          = "Tier 1 Ratio",
                nim                   = "NIM",
                equity_ratio          = "Equity Ratio",
                gdp_growth_pct        = "GDP Growth%",
                interest_rate_pct     = "Interest Rate%",
                GDP_per_Capita_PPP    = "GDP per Capita PPP",
                unemployment_rate_pct = "Unemployment%"),
       se.below    = TRUE,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
       fitstat     = ~ r2 + n,
       notes       = "Year FE. Country-clustered SEs in parentheses.",
       style.tex   = style.tex("base"),
       tex         = TRUE)

n_distinct(df_clean[h1.3$obs_selection$obs, "Country"])


#--------H2: The Moderating Role of Financial System Structure------------

# Model 1: pop65 only
m1 <- feols(Deposit_Ratio ~ pop65_pct
            | fiscal_year,
            data    = df_clean,
            cluster = ~Country)

# Model 2: + FinStructure
m2 <- feols(Deposit_Ratio ~ pop65_pct + FinStructure
            | fiscal_year,
            data    = df_clean,
            cluster = ~Country)

# Model 3: + Interaction pop65 x FinStructure
m3 <- feols(Deposit_Ratio ~ pop65_pct * FinStructure
            | fiscal_year,
            data    = df_clean,
            cluster = ~Country)

# Model 4: + Pension system dummies
m4 <- feols(Deposit_Ratio ~ pop65_pct * FinStructure +
              Bismarckian + Beveridgean
            | fiscal_year,
            data    = df_clean,
            cluster = ~Country)

# Model 5: + Bank & macro controls (full specification)
m5 <- feols(Deposit_Ratio ~ pop65_pct * FinStructure +
              Bismarckian + Beveridgean +
              log_assets + Tier_1_Ratio + nim + equity_ratio +
              gdp_growth_pct + interest_rate_pct +
              GDP_per_Capita_PPP + unemployment_rate_pct
            | fiscal_year,
            data    = df_clean,
            cluster = ~Country)

# Regression table
etable(m1, m2, m3, m4, m5,
       headers = c("Base", "+FinStr", "+Interact", "+Pension", "Full"),
       dict = c(pop65_pct                = "Population 65+%",
                FinStructure             = "Financial Structure",
                Bismarckian              = "Bismarckian",
                Beveridgean              = "Beveridgean",
                log_assets               = "Log(Assets)",
                Tier_1_Ratio             = "Tier 1 Ratio",
                nim                      = "NIM",
                equity_ratio             = "Equity Ratio",
                gdp_growth_pct           = "GDP Growth%",
                interest_rate_pct        = "Interest Rate%",
                GDP_per_Capita_PPP       = "GDP per Capita PPP",
                unemployment_rate_pct    = "Unemployment%",
                `pop65_pct:FinStructure` = "Pop 65+% x Fin. Structure"),
       se.below  = TRUE,
       fitstat   = ~ r2 + n,
       notes     = "Year FE. Country-clustered SEs in parentheses.",
       style.tex = style.tex("base"),
       tex       = TRUE)

# Number of unique countries in full model
n_distinct(df_clean[m5$obs_selection$obs, "Country"])


#--------H3: The Distinct Effect of the Oldest-------------------

# Model 1: Share_85plus only
h3.1 <- feols(Deposit_Ratio ~ Share_85plus
              | fiscal_year,
              data    = df_clean,
              cluster = ~Country)

# Model 2: + Pension system dummies
h3.2 <- feols(Deposit_Ratio ~ Share_85plus +
                Bismarckian + Beveridgean
              | fiscal_year,
              data    = df_clean,
              cluster = ~Country)

# Model 3: + Bank & macro controls (full specification)
h3.3 <- feols(Deposit_Ratio ~ Share_85plus +
                Bismarckian + Beveridgean +
                log_assets + Tier_1_Ratio + nim + equity_ratio +
                gdp_growth_pct + interest_rate_pct +
                GDP_per_Capita_PPP + unemployment_rate_pct
              | fiscal_year,
              data    = df_clean,
              cluster = ~Country)

# Regression table - H1
etable(h3.1, h3.2, h3.3,
       headers = c("(1) Base", "(2) +Pension", "(3) Full"),
       dict = c(Share_85plus          = "Population 85+%",
                Bismarckian           = "Bismarckian",
                Beveridgean           = "Beveridgean",
                log_assets            = "Log(Assets)",
                Tier_1_Ratio          = "Tier 1 Ratio",
                nim                   = "NIM",
                equity_ratio          = "Equity Ratio",
                gdp_growth_pct        = "GDP Growth%",
                interest_rate_pct     = "Interest Rate%",
                GDP_per_Capita_PPP    = "GDP per Capita PPP",
                unemployment_rate_pct = "Unemployment%"),
       se.below  = TRUE,
       fitstat   = ~ r2 + n,
       notes     = "Year FE. Country-clustered SEs in parentheses.",
       style.tex = style.tex("base"),
       tex       = TRUE)

# Number of unique countries in full model
n_distinct(df_clean[h3.3$obs_selection$obs, "Country"])

#--------Step 6: Plot — Deposit Ratio vs Age 65+ (all countries)------------

library(RColorBrewer)
library(gridExtra)

# Aggregate to country-year level
country_year <- df_clean %>%
  filter(!is.na(FinStructure)) %>%
  mutate(system_type = ifelse(FinStructure < 0, "Bank-based", "Market-based")) %>%
  group_by(Country, fiscal_year, system_type) %>%
  summarise(mean_deposit = mean(Deposit_Ratio, na.rm = TRUE),
            mean_age65   = mean(pop65_pct, na.rm = TRUE),
            .groups = "drop")

# Build a palette for all countries
n_countries <- n_distinct(country_year$Country)
country_palette <- colorRampPalette(brewer.pal(12, "Paired"))(n_countries)

p4 <- country_year %>%
  ggplot(aes(x = mean_age65, y = mean_deposit, color = Country)) +
  geom_line(aes(group = Country), alpha = 0.4, linewidth = 0.5) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(aes(group = system_type, linetype = system_type),
              method = "lm", se = TRUE, color = "black", linewidth = 0.8) +
  scale_color_manual(values = country_palette) +
  labs(title = "Deposit Ratio vs. Age 65+ by System Type",
       subtitle = "All countries — trend lines by financial system type",
       x = "Age 65+ (%)", y = "Mean Deposit Ratio",
       color = "Country", linetype = "System Type") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right",
        legend.text = element_text(size = 9))

ggsave("deposit_vs_age65_all_countries.png", p4,
       width = 20, height = 14, dpi = 300)

# Interactive versions — hover/click to see country names
# install.packages("plotly")
library(plotly)

# Bank-based countries only
p4_bank <- country_year %>%
  filter(system_type == "Bank-based") %>%
  ggplot(aes(x = mean_age65, y = mean_deposit, color = Country)) +
  geom_line(aes(group = Country), alpha = 0.4, linewidth = 0.5) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8) +
  labs(title = "Deposit Ratio vs. Age 65+ — Bank-based Countries",
       x = "Age 65+ (%)", y = "Mean Deposit Ratio", color = "Country") +
  theme_minimal(base_size = 14)

ggplotly(p4_bank, tooltip = c("colour", "x", "y")) %>%
  layout(width = 1200, height = 800)

# Market-based countries only
p4_market <- country_year %>%
  filter(system_type == "Market-based") %>%
  ggplot(aes(x = mean_age65, y = mean_deposit, color = Country)) +
  geom_line(aes(group = Country), alpha = 0.4, linewidth = 0.5) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8) +
  labs(title = "Deposit Ratio vs. Age 65+ — Market-based Countries",
       x = "Age 65+ (%)", y = "Mean Deposit Ratio", color = "Country") +
  theme_minimal(base_size = 14)

ggplotly(p4_market, tooltip = c("colour", "x", "y")) %>%
  layout(width = 1200, height = 800)


#--------Step 7: Split-Sample Regressions by Financial System Type------------
# Bank-based (FinStructure < 0) vs Market-based (FinStructure >= 0)

df_bank_based   <- df_clean %>% filter(!is.na(FinStructure), FinStructure < 0)
df_market_based <- df_clean %>% filter(!is.na(FinStructure), FinStructure >= 0)

cat("Bank-based:   ", nrow(df_bank_based),   "obs,",
    n_distinct(df_bank_based$Country),   "countries\n")
cat("Market-based: ", nrow(df_market_based), "obs,",
    n_distinct(df_market_based$Country), "countries\n")

#--- H1 split: pop65 -> Deposit_Ratio ---

h1_bank <- feols(Deposit_Ratio ~ pop65_pct +
                   Bismarckian + Beveridgean +
                   log_assets + Tier_1_Ratio + nim + equity_ratio +
                   gdp_growth_pct + interest_rate_pct +
                   GDP_per_Capita_PPP + unemployment_rate_pct
                 | fiscal_year,
                 data    = df_bank_based,
                 cluster = ~Country)

h1_market <- feols(Deposit_Ratio ~ pop65_pct +
                     Bismarckian + Beveridgean +
                     log_assets + Tier_1_Ratio + nim + equity_ratio +
                     gdp_growth_pct + interest_rate_pct +
                     GDP_per_Capita_PPP + unemployment_rate_pct
                   | fiscal_year,
                   data    = df_market_based,
                   cluster = ~Country)

etable(h1_bank, h1_market,
       headers = c("H1 Bank-based", "H1 Market-based"),
       dict = c(pop65_pct             = "Population 65+%",
                Bismarckian           = "Bismarckian",
                Beveridgean           = "Beveridgean",
                log_assets            = "Log(Assets)",
                Tier_1_Ratio          = "Tier 1 Ratio",
                nim                   = "NIM",
                equity_ratio          = "Equity Ratio",
                gdp_growth_pct        = "GDP Growth%",
                interest_rate_pct     = "Interest Rate%",
                GDP_per_Capita_PPP    = "GDP per Capita PPP",
                unemployment_rate_pct = "Unemployment%"),
       se.below  = TRUE,
       fitstat   = ~ r2 + n,
       notes     = "Year FE. Country-clustered SEs in parentheses.",
       style.tex = style.tex("base"),
       tex       = TRUE)

#--- H2 split: pop65 x FinStructure -> Deposit_Ratio ---

h2_bank <- feols(Deposit_Ratio ~ pop65_pct * FinStructure +
                   Bismarckian + Beveridgean +
                   log_assets + Tier_1_Ratio + nim + equity_ratio +
                   gdp_growth_pct + interest_rate_pct +
                   GDP_per_Capita_PPP + unemployment_rate_pct
                 | fiscal_year,
                 data    = df_bank_based,
                 cluster = ~Country)

h2_market <- feols(Deposit_Ratio ~ pop65_pct * FinStructure +
                     Bismarckian + Beveridgean +
                     log_assets + Tier_1_Ratio + nim + equity_ratio +
                     gdp_growth_pct + interest_rate_pct +
                     GDP_per_Capita_PPP + unemployment_rate_pct
                   | fiscal_year,
                   data    = df_market_based,
                   cluster = ~Country)

etable(h2_bank, h2_market,
       headers = c("H2 Bank-based", "H2 Market-based"),
       dict = c(pop65_pct                = "Population 65+%",
                FinStructure             = "Financial Structure",
                Bismarckian              = "Bismarckian",
                Beveridgean              = "Beveridgean",
                log_assets               = "Log(Assets)",
                Tier_1_Ratio             = "Tier 1 Ratio",
                nim                      = "NIM",
                equity_ratio             = "Equity Ratio",
                gdp_growth_pct           = "GDP Growth%",
                interest_rate_pct        = "Interest Rate%",
                GDP_per_Capita_PPP       = "GDP per Capita PPP",
                unemployment_rate_pct    = "Unemployment%",
                `pop65_pct:FinStructure` = "Pop 65+% x Fin. Structure"),
       se.below  = TRUE,
       fitstat   = ~ r2 + n,
       notes     = "Year FE. Country-clustered SEs in parentheses.",
       style.tex = style.tex("base"),
       tex       = TRUE)

#--- H3 split: Share_85plus -> Deposit_Ratio ---

h3_bank <- feols(Deposit_Ratio ~ Share_85plus +
                   Bismarckian + Beveridgean +
                   log_assets + Tier_1_Ratio + nim + equity_ratio +
                   gdp_growth_pct + interest_rate_pct +
                   GDP_per_Capita_PPP + unemployment_rate_pct
                 | fiscal_year,
                 data    = df_bank_based,
                 cluster = ~Country)

h3_market <- feols(Deposit_Ratio ~ Share_85plus +
                     Bismarckian + Beveridgean +
                     log_assets + Tier_1_Ratio + nim + equity_ratio +
                     gdp_growth_pct + interest_rate_pct +
                     GDP_per_Capita_PPP + unemployment_rate_pct
                   | fiscal_year,
                   data    = df_market_based,
                   cluster = ~Country)

etable(h3_bank, h3_market,
       headers = c("H3 Bank-based", "H3 Market-based"),
       dict = c(Share_85plus          = "Population 85+%",
                Bismarckian           = "Bismarckian",
                Beveridgean           = "Beveridgean",
                log_assets            = "Log(Assets)",
                Tier_1_Ratio          = "Tier 1 Ratio",
                nim                   = "NIM",
                equity_ratio          = "Equity Ratio",
                gdp_growth_pct        = "GDP Growth%",
                interest_rate_pct     = "Interest Rate%",
                GDP_per_Capita_PPP    = "GDP per Capita PPP",
                unemployment_rate_pct = "Unemployment%"),
       se.below  = TRUE,
       fitstat   = ~ r2 + n,
       notes     = "Year FE. Country-clustered SEs in parentheses.",
       style.tex = style.tex("base"),
       tex       = TRUE)


# ============================================================
# Step 8: WILD CLUSTER BOOTSTRAP
# Reference: Cameron, Gelbach & Miller (2008)
#            Cameron & Miller (2015, Journal of Human Resources)
# ============================================================

# install.packages(c("dqrng", "JuliaConnectoR", "dreamerr", "collapse",
                   "gtools", "Rcpp", "RcppArmadillo", "Matrix", "foreach"))
library(fwildclusterboot)

set.seed(42)
dqrng::dqset.seed(42)
B <- 9999

#--- 8a: Prepare clean bootstrap datasets (remove singleton FEs) ---

# Full sample
df_boot_full <- df_clean %>%
  filter(!is.na(FinStructure)) %>%
  arrange(Entity.ID, fiscal_year)

boot_check_full <- feols(
  Deposit_Ratio ~ pop65_pct +
    Bismarckian + Beveridgean +
    log_assets + Tier_1_Ratio + nim + equity_ratio +
    gdp_growth_pct + interest_rate_pct +
    GDP_per_Capita_PPP + unemployment_rate_pct
  | fiscal_year,
  data    = df_boot_full,
  cluster = ~Country_Region
)

removed_full <- boot_check_full$fixef_removed$fiscal_year
df_boot_full <- df_boot_full %>%
  filter(!fiscal_year %in% removed_full) %>%
  mutate(fiscal_year = as.numeric(gsub("FY", "", fiscal_year)))

# Bank-based subsample
df_boot_bank <- df_bank_based %>%
  filter(!is.na(FinStructure)) %>%
  arrange(Entity.ID, fiscal_year)

boot_check_bank <- feols(
  Deposit_Ratio ~ pop65_pct +
    Bismarckian + Beveridgean +
    log_assets + Tier_1_Ratio + nim + equity_ratio +
    gdp_growth_pct + interest_rate_pct +
    GDP_per_Capita_PPP + unemployment_rate_pct
  | fiscal_year,
  data    = df_boot_bank,
  cluster = ~Country_Region
)

removed_bank <- boot_check_bank$fixef_removed$fiscal_year
df_boot_bank <- df_boot_bank %>%
  filter(!fiscal_year %in% removed_bank) %>%
  mutate(fiscal_year = as.numeric(gsub("FY", "", fiscal_year)))

# Market-based subsample
df_boot_market <- df_market_based %>%
  filter(!is.na(FinStructure)) %>%
  arrange(Entity.ID, fiscal_year)

boot_check_market <- feols(
  Deposit_Ratio ~ pop65_pct +
    Bismarckian + Beveridgean +
    log_assets + Tier_1_Ratio + nim + equity_ratio +
    gdp_growth_pct + interest_rate_pct +
    GDP_per_Capita_PPP + unemployment_rate_pct
  | fiscal_year,
  data    = df_boot_market,
  cluster = ~Country_Region
)

removed_market <- boot_check_market$fixef_removed$fiscal_year
df_boot_market <- df_boot_market %>%
  filter(!fiscal_year %in% removed_market) %>%
  mutate(fiscal_year = as.numeric(gsub("FY", "", fiscal_year)))

cat("Boot bank-based:   ", nrow(df_boot_bank),   "obs after singleton removal\n")
cat("Boot market-based: ", nrow(df_boot_market), "obs after singleton removal\n")

#--- 8b: Refit models on clean bootstrap data ---

# Full sample
boot_fit_h1_full <- feols(
  Deposit_Ratio ~ pop65_pct +
    Bismarckian + Beveridgean +
    log_assets + Tier_1_Ratio + nim + equity_ratio +
    gdp_growth_pct + interest_rate_pct +
    GDP_per_Capita_PPP + unemployment_rate_pct
  | fiscal_year,
  data = df_boot_full, cluster = ~Country_Region)

boot_fit_h2_full <- feols(
  Deposit_Ratio ~ pop65_pct * FinStructure +
    Bismarckian + Beveridgean +
    log_assets + Tier_1_Ratio + nim + equity_ratio +
    gdp_growth_pct + interest_rate_pct +
    GDP_per_Capita_PPP + unemployment_rate_pct
  | fiscal_year,
  data = df_boot_full, cluster = ~Country_Region)

boot_fit_h3_full <- feols(
  Deposit_Ratio ~ Share_85plus +
    Bismarckian + Beveridgean +
    log_assets + Tier_1_Ratio + nim + equity_ratio +
    gdp_growth_pct + interest_rate_pct +
    GDP_per_Capita_PPP + unemployment_rate_pct
  | fiscal_year,
  data = df_boot_full, cluster = ~Country_Region)

# H1
boot_fit_h1_bank <- feols(
  Deposit_Ratio ~ pop65_pct +
    Bismarckian + Beveridgean +
    log_assets + Tier_1_Ratio + nim + equity_ratio +
    gdp_growth_pct + interest_rate_pct +
    GDP_per_Capita_PPP + unemployment_rate_pct
  | fiscal_year,
  data = df_boot_bank, cluster = ~Country_Region)

boot_fit_h1_market <- feols(
  Deposit_Ratio ~ pop65_pct +
    Bismarckian + Beveridgean +
    log_assets + Tier_1_Ratio + nim + equity_ratio +
    gdp_growth_pct + interest_rate_pct +
    GDP_per_Capita_PPP + unemployment_rate_pct
  | fiscal_year,
  data = df_boot_market, cluster = ~Country_Region)

# H2
boot_fit_h2_bank <- feols(
  Deposit_Ratio ~ pop65_pct * FinStructure +
    Bismarckian + Beveridgean +
    log_assets + Tier_1_Ratio + nim + equity_ratio +
    gdp_growth_pct + interest_rate_pct +
    GDP_per_Capita_PPP + unemployment_rate_pct
  | fiscal_year,
  data = df_boot_bank, cluster = ~Country_Region)

boot_fit_h2_market <- feols(
  Deposit_Ratio ~ pop65_pct * FinStructure +
    Bismarckian + Beveridgean +
    log_assets + Tier_1_Ratio + nim + equity_ratio +
    gdp_growth_pct + interest_rate_pct +
    GDP_per_Capita_PPP + unemployment_rate_pct
  | fiscal_year,
  data = df_boot_market, cluster = ~Country_Region)

# H3
boot_fit_h3_bank <- feols(
  Deposit_Ratio ~ Share_85plus +
    Bismarckian + Beveridgean +
    log_assets + Tier_1_Ratio + nim + equity_ratio +
    gdp_growth_pct + interest_rate_pct +
    GDP_per_Capita_PPP + unemployment_rate_pct
  | fiscal_year,
  data = df_boot_bank, cluster = ~Country_Region)

boot_fit_h3_market <- feols(
  Deposit_Ratio ~ Share_85plus +
    Bismarckian + Beveridgean +
    log_assets + Tier_1_Ratio + nim + equity_ratio +
    gdp_growth_pct + interest_rate_pct +
    GDP_per_Capita_PPP + unemployment_rate_pct
  | fiscal_year,
  data = df_boot_market, cluster = ~Country_Region)

#--- 8c: Run wild cluster bootstrap (Rademacher weights) ---

# Full sample
boot_h1_full <- boottest(boot_fit_h1_full, clustid = "Country_Region", param = "pop65_pct", B = B)
summary(boot_h1_full)

boot_h2_full_pop65    <- boottest(boot_fit_h2_full, clustid = "Country_Region", param = "pop65_pct",             B = B)
boot_h2_full_fin      <- boottest(boot_fit_h2_full, clustid = "Country_Region", param = "FinStructure",           B = B)
boot_h2_full_interact <- boottest(boot_fit_h2_full, clustid = "Country_Region", param = "pop65_pct:FinStructure", B = B)
summary(boot_h2_full_pop65)
summary(boot_h2_full_fin)
summary(boot_h2_full_interact)

boot_h3_full <- boottest(boot_fit_h3_full, clustid = "Country_Region", param = "Share_85plus", B = B)
summary(boot_h3_full)

# H1
boot_h1_bank   <- boottest(boot_fit_h1_bank,   clustid = "Country_Region", param = "pop65_pct", B = B)
boot_h1_market <- boottest(boot_fit_h1_market,  clustid = "Country_Region", param = "pop65_pct", B = B)

summary(boot_h1_bank)
summary(boot_h1_market)
plot(boot_h1_bank)
plot(boot_h1_market)

# H2
boot_h2_bank_pop65    <- boottest(boot_fit_h2_bank,   clustid = "Country_Region", param = "pop65_pct",             B = B)
boot_h2_bank_fin      <- boottest(boot_fit_h2_bank,   clustid = "Country_Region", param = "FinStructure",           B = B)
boot_h2_bank_interact <- boottest(boot_fit_h2_bank,   clustid = "Country_Region", param = "pop65_pct:FinStructure", B = B)

boot_h2_market_pop65    <- boottest(boot_fit_h2_market, clustid = "Country_Region", param = "pop65_pct",             B = B)
boot_h2_market_fin      <- boottest(boot_fit_h2_market, clustid = "Country_Region", param = "FinStructure",           B = B)
boot_h2_market_interact <- boottest(boot_fit_h2_market, clustid = "Country_Region", param = "pop65_pct:FinStructure", B = B)

summary(boot_h2_bank_pop65)
summary(boot_h2_bank_fin)
summary(boot_h2_bank_interact)
summary(boot_h2_market_pop65)
summary(boot_h2_market_fin)
summary(boot_h2_market_interact)
plot(boot_h2_bank_interact)
plot(boot_h2_market_interact)

# H3
boot_h3_bank   <- boottest(boot_fit_h3_bank,   clustid = "Country_Region", param = "Share_85plus", B = B)
boot_h3_market <- boottest(boot_fit_h3_market,  clustid = "Country_Region", param = "Share_85plus", B = B)

summary(boot_h3_bank)
summary(boot_h3_market)
plot(boot_h3_bank)
plot(boot_h3_market)

#--- 8d: Combined regression tables with bootstrap p-values ---

format_boot_p <- function(p, B = 9999) {
  min_p <- 1 / B
  if (p < min_p) return(paste0("< ", round(min_p, 4)))
  return(as.character(round(p, 4)))
}

# H1 table
etable(h1.3, h1_bank, h1_market,
       headers = c("Full Sample", "Bank-based", "Market-based"),
       dict = c(pop65_pct             = "Population 65+%",
                Bismarckian           = "Bismarckian",
                Beveridgean           = "Beveridgean",
                log_assets            = "Log(Assets)",
                Tier_1_Ratio          = "Tier 1 Ratio",
                nim                   = "NIM",
                equity_ratio          = "Equity Ratio",
                gdp_growth_pct        = "GDP Growth%",
                interest_rate_pct     = "Interest Rate%",
                GDP_per_Capita_PPP    = "GDP per Capita PPP",
                unemployment_rate_pct = "Unemployment%"),
       se.below  = TRUE,
       fitstat   = ~ r2 + n,
       extralines = list(
         "_Boot p (Pop 65+%)" = c(format_boot_p(boot_h1_full$p_val),
                                  format_boot_p(boot_h1_bank$p_val),
                                  format_boot_p(boot_h1_market$p_val))
       ),
       notes     = "Year FE. Country-region-clustered SEs in parentheses. Wild cluster bootstrap p-values (Rademacher, 9,999 iter.) following Cameron, Gelbach & Miller (2008).",
       style.tex = style.tex("base"),
       tex       = TRUE)

# H2 table
etable(m5, h2_bank, h2_market,
       headers = c("Full Sample", "Bank-based", "Market-based"),
       dict = c(pop65_pct                = "Population 65+%",
                FinStructure             = "Financial Structure",
                Bismarckian              = "Bismarckian",
                Beveridgean              = "Beveridgean",
                log_assets               = "Log(Assets)",
                Tier_1_Ratio             = "Tier 1 Ratio",
                nim                      = "NIM",
                equity_ratio             = "Equity Ratio",
                gdp_growth_pct           = "GDP Growth%",
                interest_rate_pct        = "Interest Rate%",
                GDP_per_Capita_PPP       = "GDP per Capita PPP",
                unemployment_rate_pct    = "Unemployment%",
                `pop65_pct:FinStructure` = "Pop 65+% x Fin. Structure"),
       se.below  = TRUE,
       fitstat   = ~ r2 + n,
       extralines = list(
         "_Boot p (Pop 65+%)"     = c(format_boot_p(boot_h2_full_pop65$p_val),
                                      format_boot_p(boot_h2_bank_pop65$p_val),
                                      format_boot_p(boot_h2_market_pop65$p_val)),
         "_Boot p (FinStructure)" = c(format_boot_p(boot_h2_full_fin$p_val),
                                      format_boot_p(boot_h2_bank_fin$p_val),
                                      format_boot_p(boot_h2_market_fin$p_val)),
         "_Boot p (Pop65xFinStr)" = c(format_boot_p(boot_h2_full_interact$p_val),
                                      format_boot_p(boot_h2_bank_interact$p_val),
                                      format_boot_p(boot_h2_market_interact$p_val))
       ),
       notes     = "Year FE. Country-region-clustered SEs in parentheses. Wild cluster bootstrap p-values (Rademacher, 9,999 iter.) following Cameron, Gelbach & Miller (2008).",
       style.tex = style.tex("base"),
       tex       = TRUE)

# H3 table
etable(h3.3, h3_bank, h3_market,
       headers = c("Full Sample", "Bank-based", "Market-based"),
       dict = c(Share_85plus          = "Population 85+%",
                Bismarckian           = "Bismarckian",
                Beveridgean           = "Beveridgean",
                log_assets            = "Log(Assets)",
                Tier_1_Ratio          = "Tier 1 Ratio",
                nim                   = "NIM",
                equity_ratio          = "Equity Ratio",
                gdp_growth_pct        = "GDP Growth%",
                interest_rate_pct     = "Interest Rate%",
                GDP_per_Capita_PPP    = "GDP per Capita PPP",
                unemployment_rate_pct = "Unemployment%"),
       se.below  = TRUE,
       fitstat   = ~ r2 + n,
       extralines = list(
         "_Boot p (Pop 85+%)" = c(format_boot_p(boot_h3_full$p_val),
                                  format_boot_p(boot_h3_bank$p_val),
                                  format_boot_p(boot_h3_market$p_val))
       ),
       notes     = "Year FE. Country-region-clustered SEs in parentheses. Wild cluster bootstrap p-values (Rademacher, 9,999 iter.) following Cameron, Gelbach & Miller (2008).",
       style.tex = style.tex("base"),
       tex       = TRUE)


#--------Step 9: Stargazer LaTeX Regression Overview------------
# stargazer requires lm/glm objects — refit full specs with lm() + year dummies
# Note: point estimates identical to feols; SEs in table are OLS (see add.lines for clustering note)

# Full sample
lm_h1_full <- lm(Deposit_Ratio ~ pop65_pct +
                    Bismarckian + Beveridgean +
                    log_assets + Tier_1_Ratio + nim + equity_ratio +
                    gdp_growth_pct + interest_rate_pct +
                    GDP_per_Capita_PPP + unemployment_rate_pct +
                    as.factor(fiscal_year),
                  data = df_clean)

lm_h2_full <- lm(Deposit_Ratio ~ pop65_pct * FinStructure +
                    Bismarckian + Beveridgean +
                    log_assets + Tier_1_Ratio + nim + equity_ratio +
                    gdp_growth_pct + interest_rate_pct +
                    GDP_per_Capita_PPP + unemployment_rate_pct +
                    as.factor(fiscal_year),
                  data = df_clean)

lm_h3_full <- lm(Deposit_Ratio ~ Share_85plus +
                    Bismarckian + Beveridgean +
                    log_assets + Tier_1_Ratio + nim + equity_ratio +
                    gdp_growth_pct + interest_rate_pct +
                    GDP_per_Capita_PPP + unemployment_rate_pct +
                    as.factor(fiscal_year),
                  data = df_clean)

# Bank-based split
lm_h1_bb <- lm(Deposit_Ratio ~ pop65_pct +
                  Bismarckian + Beveridgean +
                  log_assets + Tier_1_Ratio + nim + equity_ratio +
                  gdp_growth_pct + interest_rate_pct +
                  GDP_per_Capita_PPP + unemployment_rate_pct +
                  as.factor(fiscal_year),
                data = df_bank_based)

lm_h2_bb <- lm(Deposit_Ratio ~ pop65_pct * FinStructure +
                  Bismarckian + Beveridgean +
                  log_assets + Tier_1_Ratio + nim + equity_ratio +
                  gdp_growth_pct + interest_rate_pct +
                  GDP_per_Capita_PPP + unemployment_rate_pct +
                  as.factor(fiscal_year),
                data = df_bank_based)

lm_h3_bb <- lm(Deposit_Ratio ~ Share_85plus +
                  Bismarckian + Beveridgean +
                  log_assets + Tier_1_Ratio + nim + equity_ratio +
                  gdp_growth_pct + interest_rate_pct +
                  GDP_per_Capita_PPP + unemployment_rate_pct +
                  as.factor(fiscal_year),
                data = df_bank_based)

# Market-based split
lm_h1_mb <- lm(Deposit_Ratio ~ pop65_pct +
                  Bismarckian + Beveridgean +
                  log_assets + Tier_1_Ratio + nim + equity_ratio +
                  gdp_growth_pct + interest_rate_pct +
                  GDP_per_Capita_PPP + unemployment_rate_pct +
                  as.factor(fiscal_year),
                data = df_market_based)

lm_h2_mb <- lm(Deposit_Ratio ~ pop65_pct * FinStructure +
                  Bismarckian + Beveridgean +
                  log_assets + Tier_1_Ratio + nim + equity_ratio +
                  gdp_growth_pct + interest_rate_pct +
                  GDP_per_Capita_PPP + unemployment_rate_pct +
                  as.factor(fiscal_year),
                data = df_market_based)

lm_h3_mb <- lm(Deposit_Ratio ~ Share_85plus +
                  Bismarckian + Beveridgean +
                  log_assets + Tier_1_Ratio + nim + equity_ratio +
                  gdp_growth_pct + interest_rate_pct +
                  GDP_per_Capita_PPP + unemployment_rate_pct +
                  as.factor(fiscal_year),
                data = df_market_based)

# Create output directory
dir.create("tables", showWarnings = FALSE)

# H1 overview: Full | Bank-based | Market-based
stargazer(lm_h1_full, lm_h1_bb, lm_h1_mb,
          type  = "latex",
          title = "H1: Demographic Ageing and Deposit Ratios --- Split by Financial System",
          dep.var.labels   = "Deposit Ratio",
          column.labels    = c("Full Sample", "Bank-based", "Market-based"),
          covariate.labels = c("Population 65+\\%",
                               "Bismarckian", "Beveridgean",
                               "Log(Assets)", "Tier 1 Ratio", "NIM", "Equity Ratio",
                               "GDP Growth\\%", "Interest Rate\\%",
                               "GDP per Capita PPP", "Unemployment\\%"),
          keep             = c("pop65_pct", "Bismarckian", "Beveridgean",
                               "log_assets", "Tier_1_Ratio", "nim", "equity_ratio",
                               "gdp_growth_pct", "interest_rate_pct",
                               "GDP_per_Capita_PPP", "unemployment_rate_pct"),
          omit.stat        = c("f", "ser"),
          add.lines        = list(
            c("Year FE", "Yes", "Yes", "Yes"),
            c("Clustering", "Country-Region", "Country-Region", "Country-Region"),
            c("Boot p (Pop 65+\\%)",
              format_boot_p(boot_h1_full$p_val),
              format_boot_p(boot_h1_bank$p_val),
              format_boot_p(boot_h1_market$p_val))
          ),
          notes            = "Country-region-clustered SEs. Bootstrap p-values (Rademacher, 9,999 iter.).",
          notes.append     = FALSE,
          no.space         = TRUE)

# H2 overview: Full | Bank-based | Market-based
stargazer(lm_h2_full, lm_h2_bb, lm_h2_mb,
          type  = "latex",
          title = "H2: Financial Structure Moderation --- Split by Financial System",
          dep.var.labels   = "Deposit Ratio",
          column.labels    = c("Full Sample", "Bank-based", "Market-based"),
          covariate.labels = c("Population 65+\\%", "Financial Structure",
                               "Bismarckian", "Beveridgean",
                               "Log(Assets)", "Tier 1 Ratio", "NIM", "Equity Ratio",
                               "GDP Growth\\%", "Interest Rate\\%",
                               "GDP per Capita PPP", "Unemployment\\%",
                               "Pop 65+\\% $\\times$ Fin.\\ Structure"),
          keep             = c("pop65_pct", "FinStructure",
                               "Bismarckian", "Beveridgean",
                               "log_assets", "Tier_1_Ratio", "nim", "equity_ratio",
                               "gdp_growth_pct", "interest_rate_pct",
                               "GDP_per_Capita_PPP", "unemployment_rate_pct",
                               "pop65_pct:FinStructure"),
          omit.stat        = c("f", "ser"),
          add.lines        = list(
            c("Year FE", "Yes", "Yes", "Yes"),
            c("Clustering", "Country-Region", "Country-Region", "Country-Region"),
            c("Boot p (Pop 65+\\%)",
              format_boot_p(boot_h2_full_pop65$p_val),
              format_boot_p(boot_h2_bank_pop65$p_val),
              format_boot_p(boot_h2_market_pop65$p_val)),
            c("Boot p (FinStructure)",
              format_boot_p(boot_h2_full_fin$p_val),
              format_boot_p(boot_h2_bank_fin$p_val),
              format_boot_p(boot_h2_market_fin$p_val)),
            c("Boot p (Pop65$\\times$FinStr)",
              format_boot_p(boot_h2_full_interact$p_val),
              format_boot_p(boot_h2_bank_interact$p_val),
              format_boot_p(boot_h2_market_interact$p_val))
          ),
          notes            = "Country-region-clustered SEs. Bootstrap p-values (Rademacher, 9,999 iter.).",
          notes.append     = FALSE,
          no.space         = TRUE)

# H3 overview: Full | Bank-based | Market-based
stargazer(lm_h3_full, lm_h3_bb, lm_h3_mb,
          type  = "latex",
          title = "H3: The Distinct Effect of the Oldest --- Split by Financial System",
          dep.var.labels   = "Deposit Ratio",
          column.labels    = c("Full Sample", "Bank-based", "Market-based"),
          covariate.labels = c("Population 85+\\%",
                               "Bismarckian", "Beveridgean",
                               "Log(Assets)", "Tier 1 Ratio", "NIM", "Equity Ratio",
                               "GDP Growth\\%", "Interest Rate\\%",
                               "GDP per Capita PPP", "Unemployment\\%"),
          keep             = c("Share_85plus", "Bismarckian", "Beveridgean",
                               "log_assets", "Tier_1_Ratio", "nim", "equity_ratio",
                               "gdp_growth_pct", "interest_rate_pct",
                               "GDP_per_Capita_PPP", "unemployment_rate_pct"),
          omit.stat        = c("f", "ser"),
          add.lines        = list(
            c("Year FE", "Yes", "Yes", "Yes"),
            c("Clustering", "Country-Region", "Country-Region", "Country-Region"),
            c("Boot p (Pop 85+\\%)",
              format_boot_p(boot_h3_full$p_val),
              format_boot_p(boot_h3_bank$p_val),
              format_boot_p(boot_h3_market$p_val))
          ),
          notes            = "Country-region-clustered SEs. Bootstrap p-values (Rademacher, 9,999 iter.).",
          notes.append     = FALSE,
          no.space         = TRUE)


#--------Step 10: Europe-Only — Split-Sample Regressions & Wild-Cluster Bootstrap------------
# Filter to EU27 + Switzerland + United Kingdom
eu_countries <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia",
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece",
  "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
  "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia",
  "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom"
)

df_eu <- df_clean %>% filter(Country %in% eu_countries)
df_eu_bank   <- df_eu %>% filter(!is.na(FinStructure), FinStructure < 0)
df_eu_market <- df_eu %>% filter(!is.na(FinStructure), FinStructure >= 0)

cat("Europe-Only sample:        ", nrow(df_eu),        "obs,", n_distinct(df_eu$Country),        "countries\n")
cat("Europe-Only bank-based:    ", nrow(df_eu_bank),   "obs,", n_distinct(df_eu_bank$Country),   "countries\n")
cat("Europe-Only market-based:  ", nrow(df_eu_market), "obs,", n_distinct(df_eu_market$Country), "countries\n")

#--- 10a: Regressions — EU full sample ---

eu_h1 <- feols(Deposit_Ratio ~ pop65_pct +
                 Bismarckian + Beveridgean +
                 log_assets + Tier_1_Ratio + nim + equity_ratio +
                 gdp_growth_pct + interest_rate_pct +
                 GDP_per_Capita_PPP + unemployment_rate_pct
               | fiscal_year,
               data = df_eu, cluster = ~Country_Region)

eu_m5 <- feols(Deposit_Ratio ~ pop65_pct * FinStructure +
                 Bismarckian + Beveridgean +
                 log_assets + Tier_1_Ratio + nim + equity_ratio +
                 gdp_growth_pct + interest_rate_pct +
                 GDP_per_Capita_PPP + unemployment_rate_pct
               | fiscal_year,
               data = df_eu, cluster = ~Country_Region)

eu_h3 <- feols(Deposit_Ratio ~ Share_85plus +
                 Bismarckian + Beveridgean +
                 log_assets + Tier_1_Ratio + nim + equity_ratio +
                 gdp_growth_pct + interest_rate_pct +
                 GDP_per_Capita_PPP + unemployment_rate_pct
               | fiscal_year,
               data = df_eu, cluster = ~Country_Region)

#--- 10b: Regressions — EU bank-based ---

eu_h1_bank <- feols(Deposit_Ratio ~ pop65_pct +
                      Bismarckian + Beveridgean +
                      log_assets + Tier_1_Ratio + nim + equity_ratio +
                      gdp_growth_pct + interest_rate_pct +
                      GDP_per_Capita_PPP + unemployment_rate_pct
                    | fiscal_year,
                    data = df_eu_bank, cluster = ~Country_Region)

eu_h2_bank <- feols(Deposit_Ratio ~ pop65_pct * FinStructure +
                      Bismarckian + Beveridgean +
                      log_assets + Tier_1_Ratio + nim + equity_ratio +
                      gdp_growth_pct + interest_rate_pct +
                      GDP_per_Capita_PPP + unemployment_rate_pct
                    | fiscal_year,
                    data = df_eu_bank, cluster = ~Country_Region)

eu_h3_bank <- feols(Deposit_Ratio ~ Share_85plus +
                      Bismarckian + Beveridgean +
                      log_assets + Tier_1_Ratio + nim + equity_ratio +
                      gdp_growth_pct + interest_rate_pct +
                      GDP_per_Capita_PPP + unemployment_rate_pct
                    | fiscal_year,
                    data = df_eu_bank, cluster = ~Country_Region)

#--- 10c: Regressions — EU market-based ---

eu_h1_market <- feols(Deposit_Ratio ~ pop65_pct +
                        Bismarckian + Beveridgean +
                        log_assets + Tier_1_Ratio + nim + equity_ratio +
                        gdp_growth_pct + interest_rate_pct +
                        GDP_per_Capita_PPP + unemployment_rate_pct
                      | fiscal_year,
                      data = df_eu_market, cluster = ~Country_Region)

eu_h2_market <- feols(Deposit_Ratio ~ pop65_pct * FinStructure +
                        Bismarckian + Beveridgean +
                        log_assets + Tier_1_Ratio + nim + equity_ratio +
                        gdp_growth_pct + interest_rate_pct +
                        GDP_per_Capita_PPP + unemployment_rate_pct
                      | fiscal_year,
                      data = df_eu_market, cluster = ~Country_Region)

eu_h3_market <- feols(Deposit_Ratio ~ Share_85plus +
                        Bismarckian + Beveridgean +
                        log_assets + Tier_1_Ratio + nim + equity_ratio +
                        gdp_growth_pct + interest_rate_pct +
                        GDP_per_Capita_PPP + unemployment_rate_pct
                      | fiscal_year,
                      data = df_eu_market, cluster = ~Country_Region)

#--- 10d: Wild-Cluster Bootstrap — Europe only ---

set.seed(42)
dqrng::dqset.seed(42)

# EU Full sample — prepare clean bootstrap data
df_eu_boot_full <- df_eu %>%
  filter(!is.na(FinStructure)) %>%
  arrange(Entity.ID, fiscal_year)

eu_boot_check_full <- feols(
  Deposit_Ratio ~ pop65_pct +
    Bismarckian + Beveridgean +
    log_assets + Tier_1_Ratio + nim + equity_ratio +
    gdp_growth_pct + interest_rate_pct +
    GDP_per_Capita_PPP + unemployment_rate_pct
  | fiscal_year,
  data = df_eu_boot_full, cluster = ~Country_Region)

eu_removed_full <- eu_boot_check_full$fixef_removed$fiscal_year
df_eu_boot_full <- df_eu_boot_full %>%
  filter(!fiscal_year %in% eu_removed_full) %>%
  mutate(fiscal_year = as.numeric(gsub("FY", "", fiscal_year)))

# Prepare clean bootstrap datasets
df_eu_boot_bank <- df_eu_bank %>%
  filter(!is.na(FinStructure)) %>%
  arrange(Entity.ID, fiscal_year)

eu_boot_check_bank <- feols(
  Deposit_Ratio ~ pop65_pct +
    Bismarckian + Beveridgean +
    log_assets + Tier_1_Ratio + nim + equity_ratio +
    gdp_growth_pct + interest_rate_pct +
    GDP_per_Capita_PPP + unemployment_rate_pct
  | fiscal_year,
  data = df_eu_boot_bank, cluster = ~Country_Region)

eu_removed_bank <- eu_boot_check_bank$fixef_removed$fiscal_year
df_eu_boot_bank <- df_eu_boot_bank %>%
  filter(!fiscal_year %in% eu_removed_bank) %>%
  mutate(fiscal_year = as.numeric(gsub("FY", "", fiscal_year)))

df_eu_boot_market <- df_eu_market %>%
  filter(!is.na(FinStructure)) %>%
  arrange(Entity.ID, fiscal_year)

eu_boot_check_market <- feols(
  Deposit_Ratio ~ pop65_pct +
    Bismarckian + Beveridgean +
    log_assets + Tier_1_Ratio + nim + equity_ratio +
    gdp_growth_pct + interest_rate_pct +
    GDP_per_Capita_PPP + unemployment_rate_pct
  | fiscal_year,
  data = df_eu_boot_market, cluster = ~Country_Region)

eu_removed_market <- eu_boot_check_market$fixef_removed$fiscal_year
df_eu_boot_market <- df_eu_boot_market %>%
  filter(!fiscal_year %in% eu_removed_market) %>%
  mutate(fiscal_year = as.numeric(gsub("FY", "", fiscal_year)))

cat("Europe-Only boot bank-based:   ", nrow(df_eu_boot_bank),   "obs after singleton removal\n")
cat("Europe-Only boot market-based: ", nrow(df_eu_boot_market), "obs after singleton removal\n")

# Refit on clean bootstrap data
# EU Full sample refits
eu_boot_h1_full_fit <- feols(
  Deposit_Ratio ~ pop65_pct +
    Bismarckian + Beveridgean +
    log_assets + Tier_1_Ratio + nim + equity_ratio +
    gdp_growth_pct + interest_rate_pct +
    GDP_per_Capita_PPP + unemployment_rate_pct
  | fiscal_year,
  data = df_eu_boot_full, cluster = ~Country_Region)

eu_boot_h2_full_fit <- feols(
  Deposit_Ratio ~ pop65_pct * FinStructure +
    Bismarckian + Beveridgean +
    log_assets + Tier_1_Ratio + nim + equity_ratio +
    gdp_growth_pct + interest_rate_pct +
    GDP_per_Capita_PPP + unemployment_rate_pct
  | fiscal_year,
  data = df_eu_boot_full, cluster = ~Country_Region)

eu_boot_h3_full_fit <- feols(
  Deposit_Ratio ~ Share_85plus +
    Bismarckian + Beveridgean +
    log_assets + Tier_1_Ratio + nim + equity_ratio +
    gdp_growth_pct + interest_rate_pct +
    GDP_per_Capita_PPP + unemployment_rate_pct
  | fiscal_year,
  data = df_eu_boot_full, cluster = ~Country_Region)

eu_boot_h1_bank_fit <- feols(
  Deposit_Ratio ~ pop65_pct +
    Bismarckian + Beveridgean +
    log_assets + Tier_1_Ratio + nim + equity_ratio +
    gdp_growth_pct + interest_rate_pct +
    GDP_per_Capita_PPP + unemployment_rate_pct
  | fiscal_year,
  data = df_eu_boot_bank, cluster = ~Country_Region)

eu_boot_h1_market_fit <- feols(
  Deposit_Ratio ~ pop65_pct +
    Bismarckian + Beveridgean +
    log_assets + Tier_1_Ratio + nim + equity_ratio +
    gdp_growth_pct + interest_rate_pct +
    GDP_per_Capita_PPP + unemployment_rate_pct
  | fiscal_year,
  data = df_eu_boot_market, cluster = ~Country_Region)

eu_boot_h2_bank_fit <- feols(
  Deposit_Ratio ~ pop65_pct * FinStructure +
    Bismarckian + Beveridgean +
    log_assets + Tier_1_Ratio + nim + equity_ratio +
    gdp_growth_pct + interest_rate_pct +
    GDP_per_Capita_PPP + unemployment_rate_pct
  | fiscal_year,
  data = df_eu_boot_bank, cluster = ~Country_Region)

eu_boot_h2_market_fit <- feols(
  Deposit_Ratio ~ pop65_pct * FinStructure +
    Bismarckian + Beveridgean +
    log_assets + Tier_1_Ratio + nim + equity_ratio +
    gdp_growth_pct + interest_rate_pct +
    GDP_per_Capita_PPP + unemployment_rate_pct
  | fiscal_year,
  data = df_eu_boot_market, cluster = ~Country_Region)

eu_boot_h3_bank_fit <- feols(
  Deposit_Ratio ~ Share_85plus +
    Bismarckian + Beveridgean +
    log_assets + Tier_1_Ratio + nim + equity_ratio +
    gdp_growth_pct + interest_rate_pct +
    GDP_per_Capita_PPP + unemployment_rate_pct
  | fiscal_year,
  data = df_eu_boot_bank, cluster = ~Country_Region)

eu_boot_h3_market_fit <- feols(
  Deposit_Ratio ~ Share_85plus +
    Bismarckian + Beveridgean +
    log_assets + Tier_1_Ratio + nim + equity_ratio +
    gdp_growth_pct + interest_rate_pct +
    GDP_per_Capita_PPP + unemployment_rate_pct
  | fiscal_year,
  data = df_eu_boot_market, cluster = ~Country_Region)

# Run bootstrap
# EU Full sample bootstrap
eu_boot_h1_full <- boottest(eu_boot_h1_full_fit, clustid = "Country_Region", param = "pop65_pct", B = B)
summary(eu_boot_h1_full)

eu_boot_h2_full_pop65    <- boottest(eu_boot_h2_full_fit, clustid = "Country_Region", param = "pop65_pct",             B = B)
eu_boot_h2_full_fin      <- boottest(eu_boot_h2_full_fit, clustid = "Country_Region", param = "FinStructure",           B = B)
eu_boot_h2_full_interact <- boottest(eu_boot_h2_full_fit, clustid = "Country_Region", param = "pop65_pct:FinStructure", B = B)
summary(eu_boot_h2_full_pop65)
summary(eu_boot_h2_full_fin)
summary(eu_boot_h2_full_interact)

eu_boot_h3_full <- boottest(eu_boot_h3_full_fit, clustid = "Country_Region", param = "Share_85plus", B = B)
summary(eu_boot_h3_full)

# Bank/Market splits
eu_boot_h1_bank   <- boottest(eu_boot_h1_bank_fit,   clustid = "Country_Region", param = "pop65_pct", B = B)
eu_boot_h1_market <- boottest(eu_boot_h1_market_fit,  clustid = "Country_Region", param = "pop65_pct", B = B)

summary(eu_boot_h1_bank)
summary(eu_boot_h1_market)
plot(eu_boot_h1_bank)
plot(eu_boot_h1_market)

eu_boot_h2_bank_pop65    <- boottest(eu_boot_h2_bank_fit,   clustid = "Country_Region", param = "pop65_pct",             B = B)
eu_boot_h2_bank_fin      <- boottest(eu_boot_h2_bank_fit,   clustid = "Country_Region", param = "FinStructure",           B = B)
eu_boot_h2_bank_interact <- boottest(eu_boot_h2_bank_fit,   clustid = "Country_Region", param = "pop65_pct:FinStructure", B = B)

eu_boot_h2_market_pop65    <- boottest(eu_boot_h2_market_fit, clustid = "Country_Region", param = "pop65_pct",             B = B)
eu_boot_h2_market_fin      <- boottest(eu_boot_h2_market_fit, clustid = "Country_Region", param = "FinStructure",           B = B)
eu_boot_h2_market_interact <- boottest(eu_boot_h2_market_fit, clustid = "Country_Region", param = "pop65_pct:FinStructure", B = B)

summary(eu_boot_h2_bank_pop65)
summary(eu_boot_h2_bank_fin)
summary(eu_boot_h2_bank_interact)
summary(eu_boot_h2_market_pop65)
summary(eu_boot_h2_market_fin)
summary(eu_boot_h2_market_interact)
plot(eu_boot_h2_bank_interact)
plot(eu_boot_h2_market_interact)

eu_boot_h3_bank   <- boottest(eu_boot_h3_bank_fit,   clustid = "Country_Region", param = "Share_85plus", B = B)
eu_boot_h3_market <- boottest(eu_boot_h3_market_fit,  clustid = "Country_Region", param = "Share_85plus", B = B)

summary(eu_boot_h3_bank)
summary(eu_boot_h3_market)
plot(eu_boot_h3_bank)
plot(eu_boot_h3_market)


#--------Step 11: Europe-Only — Stargazer LaTeX Regression Overview------------
# stargazer requires lm/glm objects — refit full specs with lm() + year dummies

# EU Full sample
lm_eu_h1_full <- lm(Deposit_Ratio ~ pop65_pct +
                       Bismarckian + Beveridgean +
                       log_assets + Tier_1_Ratio + nim + equity_ratio +
                       gdp_growth_pct + interest_rate_pct +
                       GDP_per_Capita_PPP + unemployment_rate_pct +
                       as.factor(fiscal_year),
                     data = df_eu)

lm_eu_h2_full <- lm(Deposit_Ratio ~ pop65_pct * FinStructure +
                       Bismarckian + Beveridgean +
                       log_assets + Tier_1_Ratio + nim + equity_ratio +
                       gdp_growth_pct + interest_rate_pct +
                       GDP_per_Capita_PPP + unemployment_rate_pct +
                       as.factor(fiscal_year),
                     data = df_eu)

lm_eu_h3_full <- lm(Deposit_Ratio ~ Share_85plus +
                       Bismarckian + Beveridgean +
                       log_assets + Tier_1_Ratio + nim + equity_ratio +
                       gdp_growth_pct + interest_rate_pct +
                       GDP_per_Capita_PPP + unemployment_rate_pct +
                       as.factor(fiscal_year),
                     data = df_eu)

# EU Bank-based split
lm_eu_h1_bb <- lm(Deposit_Ratio ~ pop65_pct +
                     Bismarckian + Beveridgean +
                     log_assets + Tier_1_Ratio + nim + equity_ratio +
                     gdp_growth_pct + interest_rate_pct +
                     GDP_per_Capita_PPP + unemployment_rate_pct +
                     as.factor(fiscal_year),
                   data = df_eu_bank)

lm_eu_h2_bb <- lm(Deposit_Ratio ~ pop65_pct * FinStructure +
                     Bismarckian + Beveridgean +
                     log_assets + Tier_1_Ratio + nim + equity_ratio +
                     gdp_growth_pct + interest_rate_pct +
                     GDP_per_Capita_PPP + unemployment_rate_pct +
                     as.factor(fiscal_year),
                   data = df_eu_bank)

lm_eu_h3_bb <- lm(Deposit_Ratio ~ Share_85plus +
                     Bismarckian + Beveridgean +
                     log_assets + Tier_1_Ratio + nim + equity_ratio +
                     gdp_growth_pct + interest_rate_pct +
                     GDP_per_Capita_PPP + unemployment_rate_pct +
                     as.factor(fiscal_year),
                   data = df_eu_bank)

# EU Market-based split
lm_eu_h1_mb <- lm(Deposit_Ratio ~ pop65_pct +
                     Bismarckian + Beveridgean +
                     log_assets + Tier_1_Ratio + nim + equity_ratio +
                     gdp_growth_pct + interest_rate_pct +
                     GDP_per_Capita_PPP + unemployment_rate_pct +
                     as.factor(fiscal_year),
                   data = df_eu_market)

lm_eu_h2_mb <- lm(Deposit_Ratio ~ pop65_pct * FinStructure +
                     Bismarckian + Beveridgean +
                     log_assets + Tier_1_Ratio + nim + equity_ratio +
                     gdp_growth_pct + interest_rate_pct +
                     GDP_per_Capita_PPP + unemployment_rate_pct +
                     as.factor(fiscal_year),
                   data = df_eu_market)

lm_eu_h3_mb <- lm(Deposit_Ratio ~ Share_85plus +
                     Bismarckian + Beveridgean +
                     log_assets + Tier_1_Ratio + nim + equity_ratio +
                     gdp_growth_pct + interest_rate_pct +
                     GDP_per_Capita_PPP + unemployment_rate_pct +
                     as.factor(fiscal_year),
                   data = df_eu_market)

# H1 EU overview: Full | Bank-based | Market-based
stargazer(lm_eu_h1_full, lm_eu_h1_bb, lm_eu_h1_mb,
          type  = "latex",
          title = "H1: Demographic Ageing and Deposit Ratios --- Europe Only, Split by Financial System",
          dep.var.labels   = "Deposit Ratio",
          column.labels    = c("EU Full", "EU Bank-based", "EU Market-based"),
          covariate.labels = c("Population 65+\\%",
                               "Bismarckian", "Beveridgean",
                               "Log(Assets)", "Tier 1 Ratio", "NIM", "Equity Ratio",
                               "GDP Growth\\%", "Interest Rate\\%",
                               "GDP per Capita PPP", "Unemployment\\%"),
          keep             = c("pop65_pct", "Bismarckian", "Beveridgean",
                               "log_assets", "Tier_1_Ratio", "nim", "equity_ratio",
                               "gdp_growth_pct", "interest_rate_pct",
                               "GDP_per_Capita_PPP", "unemployment_rate_pct"),
          omit.stat        = c("f", "ser"),
          add.lines        = list(
            c("Year FE", "Yes", "Yes", "Yes"),
            c("Clustering", "Country-Region", "Country-Region", "Country-Region"),
            c("Boot p (Pop 65+\\%)",
              format_boot_p(eu_boot_h1_full$p_val),
              format_boot_p(eu_boot_h1_bank$p_val),
              format_boot_p(eu_boot_h1_market$p_val))
          ),
          notes            = "Europe only (EU27 + CH + UK). Country-region-clustered SEs. Bootstrap p-values (Rademacher, 9,999 iter.).",
          notes.append     = FALSE,
          no.space         = TRUE)

# H2 EU overview: Full | Bank-based | Market-based
stargazer(lm_eu_h2_full, lm_eu_h2_bb, lm_eu_h2_mb,
          type  = "latex",
          title = "H2: Financial Structure Moderation --- Europe Only, Split by Financial System",
          dep.var.labels   = "Deposit Ratio",
          column.labels    = c("EU Full", "EU Bank-based", "EU Market-based"),
          covariate.labels = c("Population 65+\\%", "Financial Structure",
                               "Bismarckian", "Beveridgean",
                               "Log(Assets)", "Tier 1 Ratio", "NIM", "Equity Ratio",
                               "GDP Growth\\%", "Interest Rate\\%",
                               "GDP per Capita PPP", "Unemployment\\%",
                               "Pop 65+\\% $\\times$ Fin.\\ Structure"),
          keep             = c("pop65_pct", "FinStructure",
                               "Bismarckian", "Beveridgean",
                               "log_assets", "Tier_1_Ratio", "nim", "equity_ratio",
                               "gdp_growth_pct", "interest_rate_pct",
                               "GDP_per_Capita_PPP", "unemployment_rate_pct",
                               "pop65_pct:FinStructure"),
          omit.stat        = c("f", "ser"),
          add.lines        = list(
            c("Year FE", "Yes", "Yes", "Yes"),
            c("Clustering", "Country-Region", "Country-Region", "Country-Region"),
            c("Boot p (Pop 65+\\%)",
              format_boot_p(eu_boot_h2_full_pop65$p_val),
              format_boot_p(eu_boot_h2_bank_pop65$p_val),
              format_boot_p(eu_boot_h2_market_pop65$p_val)),
            c("Boot p (FinStructure)",
              format_boot_p(eu_boot_h2_full_fin$p_val),
              format_boot_p(eu_boot_h2_bank_fin$p_val),
              format_boot_p(eu_boot_h2_market_fin$p_val)),
            c("Boot p (Pop65$\\times$FinStr)",
              format_boot_p(eu_boot_h2_full_interact$p_val),
              format_boot_p(eu_boot_h2_bank_interact$p_val),
              format_boot_p(eu_boot_h2_market_interact$p_val))
          ),
          notes            = "Europe only (EU27 + CH + UK). Country-region-clustered SEs. Bootstrap p-values (Rademacher, 9,999 iter.).",
          notes.append     = FALSE,
          no.space         = TRUE)

# H3 EU overview: Full | Bank-based | Market-based
stargazer(lm_eu_h3_full, lm_eu_h3_bb, lm_eu_h3_mb,
          type  = "latex",
          title = "H3: The Distinct Effect of the Oldest --- Europe Only, Split by Financial System",
          dep.var.labels   = "Deposit Ratio",
          column.labels    = c("EU Full", "EU Bank-based", "EU Market-based"),
          covariate.labels = c("Population 85+\\%",
                               "Bismarckian", "Beveridgean",
                               "Log(Assets)", "Tier 1 Ratio", "NIM", "Equity Ratio",
                               "GDP Growth\\%", "Interest Rate\\%",
                               "GDP per Capita PPP", "Unemployment\\%"),
          keep             = c("Share_85plus", "Bismarckian", "Beveridgean",
                               "log_assets", "Tier_1_Ratio", "nim", "equity_ratio",
                               "gdp_growth_pct", "interest_rate_pct",
                               "GDP_per_Capita_PPP", "unemployment_rate_pct"),
          omit.stat        = c("f", "ser"),
          add.lines        = list(
            c("Year FE", "Yes", "Yes", "Yes"),
            c("Clustering", "Country-Region", "Country-Region", "Country-Region"),
            c("Boot p (Pop 85+\\%)",
              format_boot_p(eu_boot_h3_full$p_val),
              format_boot_p(eu_boot_h3_bank$p_val),
              format_boot_p(eu_boot_h3_market$p_val))
          ),
          notes            = "Europe only (EU27 + CH + UK). Country-region-clustered SEs. Bootstrap p-values (Rademacher, 9,999 iter.).",
          notes.append     = FALSE,
          no.space         = TRUE)
