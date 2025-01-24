#Covariates cleaning and merging

library(tidyverse)

#prepare covariates
covariates <- read.csv(file = "raw_data/Covariates_lifespan.csv")
BMI <-
  read.csv((file = "raw_data/ENIGMA_MDD_BMI_Lifespan_11082021.csv"))
CTQ <- read.csv((file = "raw_data/ENIGMA_CTQ_16Dec2021.csv"))
CTQ$CTQ_Total[CTQ$CTQ_Total < 20] <- NA

cov <- merge(covariates, BMI, by.x = "SubjID", all.x = TRUE)
cov <- merge(cov, CTQ, by.x = "SubjID", all.x = TRUE)
cov$AO <- as.numeric(cov$AO)
cov$AD[(cov$AD == 0 & cov$Dx == 1)] <- NA

cov <-
  subset(
    cov,
    select = c(
      "SubjID",
      "Dx",
      "Age",
      "Sex",
      "Recur",
      "AD",
      "Rem",
      "AO",
      "BDI",
      "HDRS",
      "ADcur",
      "BMI" ,
      "CTQ_Total"
    )
  )

factorize <- function(cov) {
  cov$Sex <- as.factor(cov$Sex)
}

cov$Sex <- as.factor(cov$Sex)
cov$Recur <- as.factor(cov$Recur)
cov$AD <- as.factor(cov$AD)
cov$Rem <- as.factor(cov$Rem)
cov$Dx <- as.factor(cov$Dx)
cov$ADcur <- as.numeric(cov$ADcur) # I am almost sure we are not using that one

# laod z-score data from the server
load("~/Dropbox/ENIGMA/derivatives/derivatives.RData")

# merge with MDD and HC
MDD_cov<-MDD_zscore %>%
  left_join(cov, by ="SubjID")

MDD_cov$Age<-imp_test_MDD$Age
MDD_cov$Sex<-imp_test_MDD$Sex

cont_cov<-cont_zscore %>%
  left_join(cov, by ="SubjID")

cont_cov$Age<-imp_test_cont$Age
cont_cov$Sex<-imp_test_cont$Sex

