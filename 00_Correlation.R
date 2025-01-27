# seperate between train and test split
# CO Johanna Bayer
# 25 March 22: adding ComBat Gam

library(ggplot2)
library(Hmisc)
library(corrplot)
library(rstanarm)
library(emmeans)
library(dplyr)
library(reshape2)
library(formattable)
library(tidyverse)

folder  <- c("analysis_test_MDD_random_06_RIFS_SPLINES")
region <-
  list.files("~/Dropbox/ENIGMA/analysis_test_MDD_random_06_RIFS_SPLINES")

#HC train
cors_train <- data.frame(matrix(nrow = 35, ncol = 2))
for (m in 1:length(region)) {
  load(
    paste0(
      "~/Dropbox/ENIGMA/analysis_test_MDD_random_06_RIFS_SPLINES/",
      region[m],
      "/z_scores.RData"
    )
  )
  load(
    paste0(
      "~/Dropbox/ENIGMA/analysis_test_MDD_random_06_RIFS_SPLINES/",
      region[m],
      "/model_Data.RData"
    )
  )
  
  
  
  pred <- z_scores %>%
    filter(Set == "train") %>%
    select(mean)
  
  obs <- xtrain[6]
  
  # calculate the correlation between train and test for the same region
  
  cors_train[m, 1] <- cor(obs, pred, use = "complete.obs")
  cors_train[m, 2] <- region[m]
}

assign("Train_cor", cors_train)

#MDD test
MDD_test <- data.frame(matrix(nrow = 35, ncol = 2))
for (m in 1:length(region)) {
  load(
    paste0(
      "~/Dropbox/ENIGMA/analysis_test_MDD_random_06_RIFS_SPLINES/",
      region[m],
      "/z_scores.RData"
    )
  )
  load(
    paste0(
      "~/Dropbox/ENIGMA/analysis_test_MDD_random_06_RIFS_SPLINES/",
      region[m],
      "/model_Data.RData"
    )
  )
  
  
  
  pred <- z_scores %>%
    filter(Set == "test") %>%
    select(mean)
  
  obs <- xtest[6]
  
  # calculate the correlation between train and test for the same region
  
  MDD_test[m, 1] <- cor(obs, pred, use = "complete.obs")
  MDD_test[m, 2] <- region[m]
}

assign("Test_MDD", MDD_test)

#NDD test

cors_test <- data.frame(matrix(nrow = 35, ncol = 2))
for (m in 1:length(region)) {
  load(
    paste0(
      "~/Dropbox/ENIGMA/analysis_test_cont_random_06_RIFS_SPLINES/",
      region[m],
      "/z_scores.RData"
    )
  )
  load(
    paste0(
      "~/Dropbox/ENIGMA/analysis_test_cont_random_06_RIFS_SPLINES/",
      region[m],
      "/model_Data.RData"
    )
  )
  
  
  
  pred <- z_scores %>%
    filter(Set == "test") %>%
    select(mean)
  
  obs <- xtest[6]
  
  # calculate the correlation between train and test for the same region
  
  cors_test[m, 1] <- cor(obs, pred, use = "complete.obs")
  cors_test[m, 2] <- region[m]
}

assign("Test_cor", cors_test)

mean(Test_cor$X1)
mean(Train_cor$X1)
mean(MDD_test$X1)

# bind correlation

cor_table <- Train_cor %>%
  full_join(Test_cor, by = "X2") %>%
  full_join(MDD_test, by = "X2") %>%
  rename(
    "Train" = "X1.x",
    "region" = "X2",
    "Test" = "X1.y",
    "MDD" = "X1"
  )


file_path <- "00_cor_table.csv"
if (!file.exists(file_path)) {
  # Create and write the file
  write.csv(cor_table, file = file_path, row.names = FALSE)
  cat("File created successfully.\n")
} else {
  cat("File already exists. No changes made.\n")
}



