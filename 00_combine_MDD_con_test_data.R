# this script wil combine the z-scores from the MDD test data set and the 
# control test set for better use in one data frame. 
# Input: z-scores 
# outout: combined table test_tables.RData
# date: 11.06.2022

library(dplyr)
library(ggplot2)
#load the imput data
load("~/Dropbox/ENIGMA/imp_random_06_caret.RData")

folderlist <-
  list.files("~/Dropbox/ENIGMA/results_test_MDD_random_06_RIFS_SPLINES/")


MDD_true <- data.frame(imp_test_MDD$SubjID)
colnames(MDD_true)[1] <- "SubjID"
MDD_pred <- data.frame(imp_test_MDD$SubjID)
colnames(MDD_pred)[1] <- "SubjID"
MDD_zscore <- data.frame(imp_test_MDD$SubjID)
colnames(MDD_zscore)[1] <- "SubjID"

cont_true <- data.frame(imp_test_cont$SubjID)
colnames(cont_true)[1] <- "SubjID"
cont_pred <- data.frame(imp_test_cont$SubjID)
colnames(cont_pred)[1] <- "SubjID"
cont_zscore <- data.frame(imp_test_cont$SubjID)
colnames(cont_zscore)[1] <- "SubjID"


for (i in 1:length(folderlist)) {
  #make sure you use test healthy control data
  load(
    paste0(
      "~/Dropbox/ENIGMA/analysis_test_MDD_random_06_RIFS_SPLINES/",
      folderlist[i],
      "/z_scores.RData"
    )
  )
  load(
    paste0(
      "~/Dropbox/ENIGMA/analysis_test_MDD_random_06_RIFS_SPLINES/",
      folderlist[i],
      "/model_Data.RData"
    )
  )
  
  assign("MDD_z", z_scores)
  assign("MDD_xtest", xtest)
  
  MDD_z <- MDD_z[MDD_z$Set == "test", ]
  load(
    paste0(
      "~/Dropbox/ENIGMA/analysis_test_cont_random_06_RIFS_SPLINES/",
      folderlist[i],
      "/z_scores.RData"
    )
  )
  load(
    paste0(
      "~/Dropbox/ENIGMA/analysis_test_cont_random_06_RIFS_SPLINES/",
      folderlist[i],
      "/model_Data.RData"
    )
  )
  assign("cont_z", z_scores)
  assign("cont_xtest", xtest)
  
  cont_z <- cont_z[cont_z$Set == "test", ]
  
  MDD_true1 <- cbind(MDD_xtest[c(1, 6)])
  MDD_pred1 <- cbind(MDD_xtest[c(1)], MDD_z[c(1)])
  colnames(MDD_pred1)[2] <- folderlist[i]
  MDD_zscore1 <- cbind(MDD_xtest[c(1)], MDD_z[c(3)])
  colnames(MDD_zscore1)[2] <- folderlist[i]
  
  cont_true1 <- cbind(cont_xtest[c(1, 6)])
  cont_pred1 <- cbind(cont_xtest[c(1)], cont_z[c(1)])
  colnames(cont_pred1)[2] <- folderlist[i]
  cont_zscore1 <- cbind(cont_xtest[c(1)], cont_z[c(3)])
  colnames(cont_zscore1)[2] <- folderlist[i]
  
  
  MDD_true <- merge(MDD_true, MDD_true1, by = "SubjID")
  MDD_pred <- merge(MDD_pred, MDD_pred1, by = "SubjID")
  MDD_zscore <- merge(MDD_zscore, MDD_zscore1, by = "SubjID")
  
  cont_true <- merge(cont_true, cont_true1, by = "SubjID")
  cont_pred <- merge(cont_pred, cont_pred1, by = "SubjID")
  cont_zscore <- merge(cont_zscore, cont_zscore1, by = "SubjID")
  
  #cont_pheno<-merge(cont, pheno, by="SubjID")
}

save(
  imp_test_MDD,
  cont_pred,
  cont_true,
  imp_test_cont,
  cont_zscore,
  MDD_pred,
  MDD_true,
  MDD_zscore,
  preProcValues,
  file = "test_tables.RData"
)