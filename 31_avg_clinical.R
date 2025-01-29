# link avg  z-score to clinical covariates
# https://stats.oarc.ucla.edu/r/dae/multinomial-logistic-regression/

require(nnet)
require(tidyverse)
library(broom)
library(dplyr)


load("~/Dropbox/ENIGMA/derivatives/derivatives.RData")
source("20_Covariates.R")

MDD_cov_z<-MDD_zscore%>%
  left_join(cov, by="SubjID")%>%
  drop_na(Sex)%>%
  drop_na(Age)

cont_cov_z<-cont_zscore%>%
  left_join(cov, by="SubjID")%>%
  drop_na(Sex)%>%
  drop_na(Age)


cov_z<-data.frame(rbind(cont_cov_z, MDD_cov_z))

cov_z$avg<-rowMeans(cov_z[(which(colnames(cov_z) == "M_bankssts_thickavg")): 
                        which(colnames(cov_z) == "M_transversetemporal_thickavg")])

cov_z <- cov_z[complete.cases(cov_z[, c("Age", "Sex", "Dx")]), ]
# delete all rows that have Dx = 0 and anything else but 0 on Rem, Recur, and AD
remove_subjects <- cov_z[
  cov_z$Dx == "0" & (
    (cov_z$Recur != "0" & !is.na(cov_z$Recur)) | 
      (cov_z$Rem != "0" & !is.na(cov_z$Rem)) | 
      (cov_z$AD != "0" & !is.na(cov_z$AD))), 
]

# remove subjects that do not have 
cov_z <- anti_join(cov_z, remove_subjects, by = "SubjID")
cov_z_MDD <-cov_z[cov_z$Dx==1,]
cov_z_cont <-cov_z[cov_z$Dx==0,]

if(write){
  xlsx::write.xlsx(cov_z_MDD, file = "./40_analysis/avz_MDD.xlsx", col.names = TRUE, row.names = TRUE)
  xlsx::write.xlsx(cov_z_cont, file = "./40_analysis/avz_cont.xlsx", col.names = TRUE, row.names = TRUE)
}