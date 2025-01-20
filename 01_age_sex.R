# association between age and sex and z-scores

load("~/Dropbox/ENIGMA/derivatives/derivatives.RData")
source("~/Dropbox/ENIGMA/20_Covariates.R")


for(column in 2:36){
  print(colnames(MDD_cov[column]))
  print(summary(aov(MDD_cov[,column] ~ MDD_cov$Age+ MDD_cov$Sex)))
}

