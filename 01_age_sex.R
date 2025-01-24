# Association Between Age, Sex, and Z-Scores

# Load necessary data and scripts
load("~/Dropbox/ENIGMA/derivatives/derivatives.RData")
source("~/Dropbox/ENIGMA/20_Covariates.R")

# Loop through columns to analyze the association of age and sex with z-scores
for (column in 2:36) {
  column_name <- colnames(MDD_cov)[column]
  print(column_name)
  result <- summary(aov(MDD_cov[, column] ~ MDD_cov$Age + MDD_cov$Sex))
  print(result)
}

# Table Overview of AD, Rem, and Recur

# Load and merge data
library(readr)
ENIGMA_thick_cov <- read_csv("ENIGMA_thick_cov.csv")
ENIGMA_thick_site <- ENIGMA_thick_cov[, c("SubjID", "Site_name")]

# Convert 'Rem' to a factor
MDD_cov$Rem <- as.factor(MDD_cov$Rem)
MDD_cov$AD <- as.factor(MDD_cov$AD)
MDD_cov$Acur <- as.factor(MDD_cov$Acur)

# Merge datasets by SubjID
MDD_cov_site <- merge(MDD_cov, ENIGMA_thick_site, by = "SubjID")

# Create tables for Rem, AD, and Recur by site
library(dplyr)
table_Rem <- as.data.frame(t(table(MDD_cov_site$Rem, MDD_cov_site$Site_name)))
table_AD <- as.data.frame(t(table(MDD_cov_site$AD, MDD_cov_site$Site_name)))
table_Recur <- as.data.frame(t(table(MDD_cov_site$Recur, MDD_cov_site$Site_name)))

# Write the tables to CSV files
write_csv(table_Rem, file = "01_table_Rem.csv")
write_csv(table_AD, file = "01_table_AD.csv")
write_csv(table_Recur, file = "01_table_Recur.csv")