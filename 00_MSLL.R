library(reshape2)
compute_MSLL <-function(ytrue, ypred, train){
  
  train_mean<-mean(train[,1], na.rm=TRUE)
  train_sig<-my_var(train)
  ypred_var<-my_var(ypred)
  
  Y_train_mean <-rep(train_mean, nrow(ytrue))
  Y_train_sig <-rep(train_sig, nrow(ytrue))
  
  
  loss = 0.5*log(2*pi*ypred_var)+(ytrue-ypred)^2/(2*ypred_var)-
    0.5*log(2*pi*train_sig)-(ytrue-train_mean)^2 /(2*train_sig)
  
  MSLL<-colMeans(loss, na.rm=TRUE)
  return(MSLL)
}

my_var<-function(x){
  sum_x<-0
  mean_x<-mean(x[,1], na.rm=TRUE)
  for(i in 1:nrow(x)){
    diff<-(x[i,1]-mean_x)^2
    sum_x=sum(sum_x,diff, na.rm=TRUE)
  }
  end_var<-sum_x/nrow(x)
  return(end_var)
}

compute_MSLL(ytrue, ypred, train)


ytrue <-data.frame(c(3, -0.5, 2, 7))
ypred = data.frame(c(2.5, 0.0, 2,8))
train = data.frame(c(3.5, 0, 1.5, 8))

#MDD test set
folder  <- c("analysis_test_MDD_random_06_RIFS_SPLINES")
region<-list.files("~/Dropbox/ENIGMA/analysis_test_MDD_random_06_RIFS_SPLINES")

MSLL<-data.frame(matrix(ncol=2, nrow=35))
for (m in 1:length(folder)){
  for(n in 1:length(region)){
    
    load(paste0("~/Dropbox/ENIGMA/",folder[m],"/", region[n],"/z_scores.RData"))
    load(paste0("~/Dropbox/ENIGMA/",folder[m],"/", region[n],"/model_Data.RData"))
    

  ypred <- z_scores %>%
    filter(Set=="test")%>%
    select(mean)

  ytrue <-xtest[6]
  train<-xtrain[6]

  MSLL[n,2]<-compute_MSLL(ytrue, ypred, train)
  MSLL[n,1]<-region[n[]]
  }
}

assign("MDD_MSLL", MSLL)
#cont test set

folder  <- c("analysis_test_cont_random_06_RIFS_SPLINES")
region<-list.files("~/Dropbox/ENIGMA/analysis_test_cont_random_06_RIFS_SPLINES")

MSLL<-data.frame(matrix(ncol=2, nrow=35))
for (m in 1:length(folder)){
  for(n in 1:length(region)){
    
    load(paste0("~/Dropbox/ENIGMA/",folder[m],"/", region[n],"/z_scores.RData"))
    load(paste0("~/Dropbox/ENIGMA/",folder[m],"/", region[n],"/model_Data.RData"))
    
    
    ypred <- z_scores %>%
      filter(Set=="test")%>%
      select(mean)
    
    ytrue <-xtest[6]
    train<-xtrain[6]
    
    MSLL[n,2]<-compute_MSLL(ytrue, ypred, train)
    MSLL[n,1]<-region[n[]]
  }
}
assign("cont_MSLL", MSLL)

# bind

MSLL_table <- cont_MSLL %>%
  full_join(MDD_MSLL, by ="X1")%>%
  rename("region"="X1", 
         "Test"="X2.x", "MDD"="X2.y")

mean(MSLL_table$MDD)
mean(MSLL_table$Test)

write.csv(MSLL_table, file="00_MSLL_table.csv")