######## two models
library(emmeans)
library(reshape2)
library(stringr)
library(matrixStats)
library(tidyverse)

source("src/srmse_func.R")
source("src/unPreProc.R")

# MDD folder
folder  <- c("analysis_test_MDD_random_06_RIFS_SPLINES")
region<-list.files("~/Dropbox/ENIGMA/analysis_test_MDD_random_06_RIFS_SPLINES")

SRMSE<-list()
for (m in 1:length(folder)){
  for(n in 1:length(region)){

  load(paste0("~/Dropbox/ENIGMA/",folder[m],"/", region[n],"/z_scores.RData"))
  load(paste0("~/Dropbox/ENIGMA/",folder[m],"/", region[n],"/model_Data.RData"))
  
  vec<-which(names(preProcValues$mean)==region[n])
  
   pred <- z_scores %>%
     filter(Set=="train")%>%
     select(mean)%>%
     mutate_at("mean", as.numeric)
  # 
  # pred<-pred*preProcValues$std[[vec]]+ preProcValues$mean[[vec]]
  # 
  pred<-z_scores$mean
  obs <-xtrain[6]
  

  SRMSE[n]<-srmse_func(obs, pred)
  rm(list=c("pred", "obs"))
}
}
assign("SRMSE_Train_cont", SRMSE)
SRMSE_Train_cont<-do.call("rbind", SRMSE_Train_cont)

# MDD folder
folder  <- c("analysis_test_MDD_random_06_RIFS_SPLINES")
region<-list.files("~/Dropbox/ENIGMA/analysis_test_MDD_random_06_RIFS_SPLINES")

SRMSE<-list()
for (m in 1:length(folder)){
  for(n in 1:length(region)){
    
    load(paste0("~/Dropbox/ENIGMA/",folder[m],"/", region[n],"/z_scores.RData"))
    load(paste0("~/Dropbox/ENIGMA/",folder[m],"/", region[n],"/model_Data.RData"))
    
    vec<-which(names(preProcValues$mean)==region[n])
    
    pred <- z_scores %>%
      filter(Set=="test")%>%
      select(mean)%>%
      mutate_at("mean", as.numeric)
    # 
    # pred<-pred*preProcValues$std[[vec]]+ preProcValues$mean[[vec]]
    # 
    #pred<-z_scores$mean
    obs <-xtest[6]
    
    
    SRMSE[n]<-srmse_func(obs, pred)
    rm(list=c("pred", "obs"))
  }
}
assign("SRMSE_Test_MDD", SRMSE)
SRMSE_Test_MDD<-do.call("rbind", SRMSE_Test_MDD)

# test control

folder  <- c("analysis_test_cont_random_06_RIFS_SPLINES")
region<-list.files("~/Dropbox/ENIGMA/analysis_test_cont_random_06_RIFS_SPLINES")

SRMSE<-list()
for (m in 1:length(folder)){
  for(n in 1:length(region)){
    
    load(paste0("~/Dropbox/ENIGMA/",folder[m],"/", region[n],"/z_scores.RData"))
    load(paste0("~/Dropbox/ENIGMA/",folder[m],"/", region[n],"/model_Data.RData"))
    
    vec<-which(names(preProcValues$mean)==region[n])
    
    pred <- z_scores %>%
      filter(Set=="test")%>%
      select(mean)%>%
      mutate_at("mean", as.numeric)
    # 
    # pred<-pred*preProcValues$std[[vec]]+ preProcValues$mean[[vec]]
    # 
    #pred<-z_scores$mean
    obs <-xtest[6]
    
    
    SRMSE[n]<-srmse_func(obs, pred)
    rm(list=c("pred", "obs"))
  }
}

assign("SRMSE_Test_cont", SRMSE)
SRMSE_Test_cont<-do.call("rbind", SRMSE_Test_cont)

mean(SRMSE_Train_cont)
mean(SRMSE_Test_cont)
mean(SRMSE_Test_MDD)

R_frame<-data.frame("region"=region,"Train"=SRMSE_Train_cont, "test"=SRMSE_Test_cont, "MDD"=SRMSE_Test_MDD)
write.csv(R_frame, file="00_RMSE_table.csv")