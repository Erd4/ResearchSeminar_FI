##############################################################
### Merge Region Mappings (Germany + USA)                  ###
### Research Seminar Financial Institutions                 ###
### Spring Semester 2026                                    ###
##############################################################

#------------------------------------------------------------#
# Load libraries
#------------------------------------------------------------#

library(readr)
library(dplyr)

#------------------------------------------------------------#
# Load filtered base datasets
#------------------------------------------------------------#

df_all <- read.csv("MASTER_EU27_CH_OECD.csv", stringsAsFactors = FALSE)
df_sav <- read.csv("MASTER_SAV_EU27_CH_OECD.csv", stringsAsFactors = FALSE)

#------------------------------------------------------------#
# Load region mapping files
#------------------------------------------------------------#

# Germany SAV regions (Bundesländer)
de_sav <- read.csv("(OPEN) To be merged/Germany_country-region_SAV.csv",
                    stringsAsFactors = FALSE)

# USA Bank regions (US states)
us_bank <- read.csv("(OPEN) To be merged/USA_country-region_BANK.csv",
                     stringsAsFactors = FALSE)

# USA SAV regions (US states)
us_sav <- read.csv("(OPEN) To be merged/USA_country-region_SAV.csv",
                    stringsAsFactors = FALSE)

#------------------------------------------------------------#
# Harmonise column names across region files
#------------------------------------------------------------#

# Germany SAV: Entity.ID, Entity.Name, Region
# USA Bank:   Entity.ID, Entity.Name, SP_COUNTRY_NAME, SNL_INDUSTRY, Region
# USA SAV:    Entity.ID, Entity.Name, SP_COUNTRY_NAME, SNL_INDUSTRY, Region

# Keep only Entity.ID + Region, then stack
de_sav_map  <- de_sav[, c("Entity.ID", "Region")]
us_bank_map <- us_bank[, c("Entity.ID", "Region")]
us_sav_map  <- us_sav[, c("Entity.ID", "Region")]

region_map <- rbind(de_sav_map, us_bank_map, us_sav_map)

cat("\nTotal region mappings:", nrow(region_map), "\n")
cat("Unique Entity IDs:", length(unique(region_map$Entity.ID)), "\n")

# Check what duplicate Entity IDs are (should be none, but just to be sure)
dupes <- region_map$Entity.ID[duplicated(region_map$Entity.ID)]
region_map[region_map$Entity.ID %in% dupes, ]

#------------------------------------------------------------#
# Merge Region onto base datasets (left join on Entity.ID)
#------------------------------------------------------------#

merge_region <- function(df) {
  df <- merge(df, region_map,
              by = "Entity.ID",
              all.x = TRUE)

  # For non-Germany/USA entities, Region stays NA (as expected)
  cat("  Rows with Region:", sum(!is.na(df$Region)), "/", nrow(df), "\n")

  return(df)
}

df_all <- merge_region(df_all)

df_sav <- merge_region(df_sav)

#------------------------------------------------------------#
# Reorder: move Region right after SP_COUNTRY_NAME
#------------------------------------------------------------#

reorder_region <- function(df) {
  cols <- names(df)
  country_pos <- which(cols == "SP_COUNTRY_NAME")
  region_pos  <- which(cols == "Region")

  # Remove Region from current position, insert after SP_COUNTRY_NAME
  cols <- cols[-region_pos]
  cols <- append(cols, "Region", after = country_pos)

  return(df[, cols])
}

df_all <- reorder_region(df_all)
df_sav <- reorder_region(df_sav)

#------------------------------------------------------------#
# Save
#------------------------------------------------------------#

write.csv(df_all, "MASTER_EU27_CH_OECD.csv", row.names = FALSE)
write.csv(df_sav, "MASTER_SAV_EU27_CH_OECD.csv", row.names = FALSE)

View(df_all)
View(df_sav)