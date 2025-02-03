library(rlist)
library(brms)
library(here)

regions<-read.csv(here("regions_thickness.txt"))[2]

dir.create("input_RIFS_GP_MDD_random_06")
for (i in 1:nrow(regions)) {
  load(paste0(here("input_test_MDD_random_06/",regions[i,],"/model_data.RData")))
  setwd("input_RIFS_GP_MDD_random_06")
  dir.create(path=paste(regions[i,]))
  setwd(paste(regions[i,]))
  
  
  xtrain2<-xtrain; xtest2<-xtest
  xtrain2$Site<-as.character(xtrain2$Site)
  xtest2$Site<-as.character(xtest2$Site)
  colnames(xtrain2)[6]<- "region"
  colnames(xtest2)[6]<- "region"
  to_stan_RIFS_model3_GP<- make_standata(region ~Age+(1|Site)+Sex+gp(Age), data=xtrain2)
  to_stan_RIFS_model3_GP_test<- make_standata(region ~Age+(1|Site)+Sex+gp(Age), data=xtest2)
  
  # add unstandardized ages for GP predictor (Age of training and testing split is already standardized)
  to_stan_RIFS_model3_GP$Xgp_1<-NULL
  to_stan_RIFS_model3_GP_test$Xgp_1<-NULL
  to_stan_RIFS_model3_GP$Xgp_1<-matrix(1, nrow(xtrain2))
  to_stan_RIFS_model3_GP_test$Xgp_1<-matrix(1,nrow(xtest2))
  
  to_stan_RIFS_model3_GP$Xgp_1[1:nrow(xtrain2),1]<-(xtrain2$Age)
  to_stan_RIFS_model3_GP_test$Xgp_1[1:nrow(xtest2),1]<-xtest2$Age
  
  # modify test set
  
  to_stan_RIFS_model3_GP<- list.append(to_stan_RIFS_model3_GP,
                                                N_test= to_stan_RIFS_model3_GP_test$N, 
                                                X_test=to_stan_RIFS_model3_GP_test$X, 
                                                J_1_test= to_stan_RIFS_model3_GP_test$J_1,  
                                                Z_1_1_test = to_stan_RIFS_model3_GP_test$Z_1_1,
                                                Xgp_2=to_stan_RIFS_model3_GP_test$Xgp_1,
                                                Y_test= to_stan_RIFS_model3_GP_test$Y
  )
  
  save(to_stan_RIFS_model3_GP, file="to_stan_RIFS_model3_GP.RData")
  save(xtrain, xtest, preProcValues, file="model_Data.RData")
  
  #save as csv files
  write.csv(to_stan_RIFS_model3_GP$N, file="N.csv", row.names = FALSE)
  write.csv(to_stan_RIFS_model3_GP$Y, file="Y.csv", row.names = FALSE)
  write.csv(to_stan_RIFS_model3_GP$K, file="K.csv", row.names = FALSE)
  write.csv(to_stan_RIFS_model3_GP$X, file="X.csv", row.names = FALSE)
  write.csv(to_stan_RIFS_model3_GP$Z_1_1, file="Z_1_1.csv", row.names = FALSE)
  write.csv(to_stan_RIFS_model3_GP$Dgp_1, file="Dgp_1.csv", row.names = FALSE)
  write.csv(to_stan_RIFS_model3_GP$Kgp_1, file="Kgp_1.csv", row.names = FALSE)
  write.csv(to_stan_RIFS_model3_GP$Xgp_1, file="Xgp_1.csv", row.names = FALSE)
  write.csv(to_stan_RIFS_model3_GP$J_1, file="J_1.csv", row.names = FALSE)
  write.csv(to_stan_RIFS_model3_GP$N_1, file="N_1.csv", row.names = FALSE)
  write.csv(to_stan_RIFS_model3_GP$M_1, file="M_1.csv", row.names = FALSE)
  write.csv(to_stan_RIFS_model3_GP$NC_1, file="NC_1.csv", row.names = FALSE)
  write.csv(to_stan_RIFS_model3_GP$prior_only, file="prior_only.csv", row.names = FALSE)
  write.csv(to_stan_RIFS_model3_GP_test$N, file="N_test.csv", row.names = FALSE)
  write.csv(to_stan_RIFS_model3_GP_test$X, file="X_test.csv", row.names = FALSE)
  write.csv(to_stan_RIFS_model3_GP_test$J_1, file="J_1_test.csv", row.names = FALSE)
  write.csv(to_stan_RIFS_model3_GP_test$Z_1_1, file="Z_1_1_test.csv", row.names = FALSE)
  write.csv(to_stan_RIFS_model3_GP_test$Xgp_1, file="Xgp_2.csv", row.names = FALSE)
  write.csv(to_stan_RIFS_model3_GP_test$Y, file="Y_test.csv", row.names = FALSE)
  
  
  rm(xtrain, xtrain2, xtest, xtest2,preProcParams,to_stan_RIFS_model3_GP,to_stan_RIFS_model3_GP_test)
  setwd("./../..")
}