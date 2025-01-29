library(rstatix)
library(tidyverse)
library(ggplot2)
library(nnet)
library(xlsx)

###-------
# make sev_cov and xcel table
### -----
#load("~/Dropbox/ENIGMA/derivatives/derivatives.RData")
load("~/Dropbox/ENIGMA/derivatives/derivatives.RData")
#source("~/Dropbox/ENIGMA/0_Covariates.R")
source("~/Dropbox/ENIGMA/20_Covariates.R")

MDD_zscore_df<-MDD_zscore %>% #3645
  data.frame() %>%
  select(!"SubjID")

cont_zscore_df<-cont_zscore %>% #2119
  data.frame() %>%
  select(!"SubjID")

severity_MDD_pos<-data.frame("severity" =do.call(`pmax`, MDD_zscore_df)) #3645
severity_MDD_neg<-data.frame("severity" =do.call(`pmin`, MDD_zscore_df)) #3645

severity_cont_pos<-data.frame("severity" =do.call(`pmax`, cont_zscore_df)) #2119
severity_cont_neg<-data.frame("severity"= do.call(`pmin`, cont_zscore_df)) #2119

severity_MDD_pos$Dx<-"MDD" #3645
severity_MDD_neg$Dx<-"MDD" #3645
severity_cont_pos$Dx<-"cont" #2119
severity_cont_neg$Dx<-"cont" #2119

severity_neg<-rbind(severity_cont_neg, severity_MDD_neg) #5764
severity_pos<-rbind(severity_cont_pos, severity_MDD_pos) #5764

MDD_cov<-merge(MDD_zscore, cov, by="SubjID", all.x = TRUE) #3645
cont_cov<-merge(cont_zscore, cov, by="SubjID", all.x = TRUE) #2119

# corleations severity
MDD_cov<-cbind(MDD_cov, severity_MDD_neg$severity, severity_MDD_pos$severity) #3645
colnames(MDD_cov)[c(49,50)]<-c("neg_sev", "pos_sev") #3645

MDD_cov$AO<-as.numeric(MDD_cov$AO) #3645

cont_cov<-cbind(cont_cov, severity_cont_neg$severity, severity_cont_pos$severity) #2119
cont_cov$AO<-as.numeric(cont_cov$AO) #2119
colnames(cont_cov)[c(49,50)]<-c("neg_sev", "pos_sev") #2119

sev_cov<-data.frame(rbind(cont_cov, MDD_cov)) #5764
# delete all rows that don't have age, sex and Dx
sev_cov <- sev_cov[complete.cases(sev_cov[, c("Age", "Sex", "Dx")]), ]
# delete all rows that have Dx = 0 and anything else but 0 on Rem, Recur, and AD
remove_subjects <- sev_cov[
    sev_cov$Dx == "0" & (
    (sev_cov$Recur != "0" & !is.na(sev_cov$Recur)) | 
    (sev_cov$Rem != "0" & !is.na(sev_cov$Rem)) | 
    (sev_cov$AD != "0" & !is.na(sev_cov$AD))), 
]

# remove subjects that do not have 
sev_cov <- anti_join(sev_cov, remove_subjects, by = "SubjID")
sev_cov_MDD <-sev_cov[sev_cov$Dx==1,]
sev_cov_cont <-sev_cov[sev_cov$Dx==0,]

if(write){
  xlsx::write.xlsx(sev_cov, file = "./40_analysis/sev_cov.xlsx", col.names = TRUE, row.names = TRUE)
  xlsx::write.xlsx(sev_cov_MDD, file = "./40_analysis/sev_cov_MDD.xlsx", col.names = TRUE, row.names = TRUE)
  xlsx::write.xlsx(sev_cov_cont, file = "./40_analysis/sev_cov_cont.xlsx", col.names = TRUE, row.names = TRUE)
}
