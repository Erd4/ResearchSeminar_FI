# Research Seminar — Financial Institutions

**Spring Semester 2026**

This repository contains the data pipeline and analysis for a research seminar project studying **deposit ratios across European banks** (EU-27 + Switzerland), with a focus on how macro-level factors — population aging, pension system design, and financial market structure — relate to bank funding composition.

## Research Question

How do demographic aging, pension system characteristics, and the structure of a country's financial system affect the deposit-to-asset ratio of commercial and savings banks in Europe?

## Data

The project merges bank-level data from **S&P Capital IQ** with several macro-level control variables:

| Variable | Source | Coverage |
|---|---|---|
| Total Deposits, Total Assets, Deposit Ratio | S&P Capital IQ (SNL) | EU-27 + CH, FY2010–FY2025 |
| Population Aged 65+ (% of total) | World Bank | EU-27 + CH, 2010–2025 |
| Aggregate Replacement Ratio | Eurostat | EU-27 + CH, 2010–2025 |
| Market Cap / GDP, Private Credit / GDP | World Bank, ECB, BIS, Nasdaq | EU-27 + CH, 2010–2025 |
| Financial Structure Index | Computed (see below) | EU-27 + CH, 2010–2025 |
| Pension System Classification | Literature (Bismarckian / Beveridgean) | EU-27 + CH, time-invariant |

The **Financial Structure Index** is calculated as:

```
FinStructure = ln( (1 + StockMarketCap/GDP) / (1 + PrivateCredit/GDP) )
```

Higher values indicate a more market-based financial system; lower (more negative) values indicate a more bank-based system.

### Sample sizes

- **All banks (EU-27 + CH):** ~83,000 bank-year observations
- **Savings banks only:** ~41,000 bank-year observations

## Repository Structure

```
.
├── scripts/
│   ├── 01_data_cleansing.R          # Filter raw S&P data to EU-27+CH, compute deposit ratio
│   ├── 02_data_validation.R         # Rename columns, strip to key variables
│   ├── 03_data_merge.R              # Merge bank-level data with all macro controls
│   ├── cleaning_aging.R             # Clean World Bank age-65+ data
│   ├── cleaning_financial_structure.R       # Compute FinStructure index from WB series
│   ├── cleaning_financial_structure_post.R  # Patch missing values, trim to 2010-2025
│   └── cleaning_pension_controls.R  # Clean Eurostat aggregate replacement ratio
│
├── data/
│   ├── raw/                         # Source datasets (cleaned from original downloads)
│   │   ├── RS_FI_2026_EU27_CH.csv           # All EU-27+CH banks
│   │   ├── RS_FI_2026_SAV_EU27_CH.csv       # Savings banks only
│   │   ├── aging/                            # Age-65+ shares
│   │   ├── financial_structure/              # Market cap, private credit, FinStructure
│   │   └── pension_controls/                 # Replacement ratios, system classifications
│   │
│   └── processed/                   # Merged master datasets (ready for analysis)
│       ├── all/
│       │   ├── EU27_CH_Stripped.csv          # Bank-level data (key columns only)
│       │   └── master.csv                    # Full merged panel
│       └── savings/
│           ├── SAV_EU27_CH_Stripped.csv
│           └── master.csv
│
├── .gitignore
└── README.md
```

## Pipeline

The scripts are designed to be run in order:

1. **`cleaning_*.R`** — Each script cleans one macro-level source dataset (aging, financial structure, pension controls) and outputs a tidy CSV.
2. **`01_data_cleansing.R`** — Filters the raw S&P Capital IQ extract to EU-27 + Switzerland, computes the deposit ratio, and exports two subsets (all banks / savings banks only).
3. **`02_data_validation.R`** — Renames cryptic variable codes to human-readable names and strips the data down to the key columns needed for the merge.
4. **`03_data_merge.R`** — Joins the bank-level panel with all macro controls on country + year, producing the final `master.csv` files.

## Requirements

- **R** (≥ 4.0) with packages: `dplyr`, `readr`, `tidyr`, `readxl`

## License

For academic use only. The underlying bank-level data is proprietary (S&P Capital IQ) and subject to its terms of use.
