library(caret)
library(here)

load(here("imp_random_06_caret.RData"))
fraction<-"random_06"

#regions<-read.csv(paste0("~/Dropbox/Multitask/ABIDE_dataset/regionlist.txt"), stringsAsFactors = FALSE, header=FALSE)
regions<-colnames(imp_train_cont)[1:35]
train_data<-imp_train_cont
train_data_name<-"train_cont"

test_data<-imp_test_cont
test_data_name<-"test_cont"

test_data<-imp_test_MDD
test_data_name<-"test_MDD"


train_data<-data.frame(train_data)
test_data<-data.frame(test_data)

test_data<-test_data[complete.cases(test_data),]

train_data$Site<-as.factor(train_data$Site)
train_data$Sex<-as.factor(train_data$Sex)
train_data$Dx<-as.factor(train_data$Dx)
train_data$SubjID<-as.factor(train_data$SubjID)

test_data$Site<-as.factor(test_data$Site)
test_data$Sex<-as.factor(test_data$Sex)
test_data$Dx<-as.factor(test_data$Dx)
test_data$SubjID<-as.factor(test_data$SubjID)


dir.create(paste0("input_",test_data_name, "_", fraction))
for(i in 1:length(regions)){
  # get regions column by column
  region<-regions[i]
  #unlink(region[i], recursive=TRUE)
  
  #make subset for training and test data
  subset<-c("SubjID","Age","Sex","Site", "Dx",paste(region))
  xtrain<-train_data[subset]
  xtest<-test_data[subset]

  
  #preProcValues<-preProcess(xtrain2)
  #xtrain<-predict(preProcValues, xtrain2)
  #xtest<-predict(preProcValues, xtest2)

  
  dir.create(file.path(paste0("input_",test_data_name, "_", fraction,"/",region)))
  setwd(paste0("input_",test_data_name, "_", fraction,"/",region))
  save(xtrain, xtest, preProcValues, file="model_data.RData")
  #rm(xtrain, xtest, preProcValues,xtrain2, xtest2)
  rm(xtrain, xtest)
  setwd('../..')
}

write.csv(regions, here("regions_thickness.txt"))
#colnames(test_cont)[1:35]


