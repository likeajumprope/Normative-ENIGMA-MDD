# Clauclating load cores group differences and clinical associations
require(nnet)
require(tidyverse)
library(broom)
library(dplyr)


load("~/Dropbox/ENIGMA/derivatives/derivatives.RData")
source("~/Dropbox/ENIGMA/20_Covariates.R")
# How many individuals do at least have one positive deviation
MDD_positive <- (as.matrix((MDD_zscore[, c(2:36)] > 1.96)) * 1)
MDD_negative <- (as.matrix((MDD_zscore[, c(2:36)] < (-1.96))) * -1)

sum(rowSums(MDD_positive) >= 1) / nrow(MDD_positive)

sum(rowSums(MDD_negative) <= (-1)) / nrow(MDD_negative)

MDD_pos <- sum(rowSums(MDD_positive) >= 1)
MDD_neg <- sum(rowSums(MDD_negative) <= (-1))

#controls

cont_positive <- (as.matrix((cont_zscore[, c(2:36)] > 1.96)) * 1)
cont_negative <- (as.matrix((cont_zscore[, c(2:36)] < (-1.96))) * -1)

sum(rowSums(cont_positive) >= 1) / nrow(cont_positive)

sum(rowSums(cont_negative) <= (-1)) / nrow(cont_negative)

cont_pos <- sum(rowSums(cont_positive) >= 1)
cont_neg <- sum(rowSums(cont_negative) <= (-1))


#Correlation of load with clinical variables ----
MDD_positive_load <- rowSums(MDD_positive)
MDD_negative_load <- rowSums(MDD_negative)

cont_positive_load <- rowSums(cont_positive)
cont_negative_load <- rowSums(cont_negative)

# covariates

MDD_load <-
  data.frame(MDD_positive_load, MDD_negative_load, "SubjID" = MDD_zscore$SubjID)
MDD_load_cov <- merge(MDD_load, cov, by.x = "SubjID", all.x = TRUE)

cont_load <-
  data.frame(cont_positive_load, cont_negative_load, "SubjID" = cont_zscore$SubjID)
cont_load_cov <- merge(cont_load, cov, by.x = "SubjID", all.x = TRUE)

colnames(cont_load_cov)[2:3]<-c("positive_load", "negative_load")
colnames(MDD_load_cov)[2:3]<-c("positive_load", "negative_load")
load_cov<-rbind(cont_load_cov, MDD_load_cov)


load_cov <- load_cov[complete.cases(sev_cov[, c("Age", "Sex", "Dx")]), ]
# delete all rows that have Dx = 0 and anything else but 0 on Rem, Recur, and AD
remove_subjects <- load_cov[
  load_cov$Dx == "0" & (
    (load_cov$Recur != "0" & !is.na(load_cov$Recur)) | 
      (load_cov$Rem != "0" & !is.na(load_cov$Rem)) | 
      (load_cov$AD != "0" & !is.na(load_cov$AD))), 
]

# remove subjects that do not have 
load_cov <- anti_join(load_cov, remove_subjects, by = "SubjID")
load_cov_MDD <-load_cov[load_cov$Dx==1,]
load_cov_cont <-load_cov[load_cov$Dx==0,]

if(write){
  xlsx::write.xlsx(load_cov_MDD, file = "./40_analysis/loadz_MDD.xlsx", col.names = TRUE, row.names = TRUE)
  xlsx::write.xlsx(load_cov_cont, file = "./40_analysis/loadz_cont.xlsx", col.names = TRUE, row.names = TRUE)
}
