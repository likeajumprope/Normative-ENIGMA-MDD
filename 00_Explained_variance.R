library(emmeans)

explained_var<-function(ytrue, ypred){
  
  df<-cbind(ytrue,ypred)
  df_na<-df[complete.cases(df),]
  
  exp_var <-1-(var(df_na[,1]-df_na[,2])/var(df_na[,1]))
  print(exp_var)
  return(exp_var)
}

folder  <- c("analysis_test_MDD_random_06_RIFS_SPLINES")
region<-list.files("~/Dropbox/ENIGMA/analysis_test_MDD_random_06_RIFS_SPLINES")

EV<-data.frame(matrix(ncol=2, nrow=35))
for (m in 1:length(folder)){
  for(n in 1:length(region)){
    
    load(paste0("~/Dropbox/ENIGMA/",folder[m],"/", region[n],"/z_scores.RData"))
    load(paste0("~/Dropbox/ENIGMA/",folder[m],"/", region[n],"/model_Data.RData"))
    
    
    ypred <- z_scores %>%
      filter(Set=="train")%>%
      select(mean)
    
    ytrue <-xtrain[6]

    
    EV[n,2]<-explained_var(ytrue, ypred)
    EV[n,1]<-region[n]
  }
}

assign("train_EV", EV)


folder  <- c("analysis_test_MDD_random_06_RIFS_SPLINES")
region<-list.files("~/Dropbox/ENIGMA/analysis_test_MDD_random_06_RIFS_SPLINES")

EV<-data.frame(matrix(ncol=2, nrow=35))
for (m in 1:length(folder)){
  for(n in 1:length(region)){
    
    load(paste0("~/Dropbox/ENIGMA/",folder[m],"/", region[n],"/z_scores.RData"))
    load(paste0("~/Dropbox/ENIGMA/",folder[m],"/", region[n],"/model_Data.RData"))
    
    
    ypred <- z_scores %>%
      filter(Set=="test")%>%
      select(mean)
    
    ytrue <-xtest[6]
    
    
    EV[n,2]<-explained_var(ytrue, ypred)
    EV[n,1]<-region[n]
  }
}

assign("MDD_EV", EV)



folder  <- c("analysis_test_cont_random_06_RIFS_SPLINES")
region<-list.files("~/Dropbox/ENIGMA/analysis_test_cont_random_06_RIFS_SPLINES")

EV<-data.frame(matrix(ncol=2, nrow=35))
for (m in 1:length(folder)){
  for(n in 1:length(region)){
    
    load(paste0("~/Dropbox/ENIGMA/",folder[m],"/", region[n],"/z_scores.RData"))
    load(paste0("~/Dropbox/ENIGMA/",folder[m],"/", region[n],"/model_Data.RData"))
    
    
    ypred <- z_scores %>%
      filter(Set=="test")%>%
      select(mean)
    
    ytrue <-xtest[6]
    
    
    EV[n,2]<-explained_var(ytrue, ypred)
    EV[n,1]<-region[n]
  }
}

assign("Test_EV", EV)

EV_table <- train_EV %>%
  full_join(Test_EV, by ="X1")%>%
  full_join(MDD_EV, by ="X1") %>%
  rename("Train" = "X2.x", "region"="X1", 
         "Test"="X2.y", "MDD"="X2")

mean(EV_table$Train)
mean(EV_table$Test)
mean(EV_table$MDD)
write.csv(EV_table, file="00_EV_table.csv")