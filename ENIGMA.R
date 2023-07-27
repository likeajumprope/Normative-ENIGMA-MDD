# Demographics data and data preparation
# test

library(ggplot2)
library(stringr)
library(splitstackshape)
library(patchwork)
library(randomForest)
library(ggsci)
library(caret)
library(officer)
install.packages("here")
library(here)

MAKE_PLOTS = FALSE


setwd("~/Dropbox/ENIGMA")

# Read data and additional data
ENIGMA_full <-
  read.csv("raw_data/Subcort_Cort_lifespan.csv") #read dataset
covariates <- read.csv("raw_data/Covariates_lifespan.csv")

covariates <-
  covariates[!(is.na(covariates$Age) | is.na(covariates$Sex)), ]

#these are the updates
MODECT_SubCort_Cort <-
  read.csv("raw_data/MODECT_SubCort_Cort.csv") # replace with existing variables
CSAN_Basic_Covariates <-
  read.csv("raw_data/CSAN_Basic_Covariates.csv")
CSAN_SubCort_Cort <- read.csv("raw_data/CSAN_SubCort_Cort.csv")

#merge covaraites
covariates <-
  rbind(covariates[, 2:14], CSAN_Basic_Covariates[, c(1:4, 6:14)])

ENIGMA_full[grepl("MODECT" , ENIGMA_full$SubjID),][, c(2:237)] <-
  MODECT_SubCort_Cort
ENIGMA_full <- ENIGMA_full[, c(2:238)]
CSAN_SubCort_Cort$Site <- 65

colnames(ENIGMA_full) <- colnames(CSAN_SubCort_Cort)

ENIGMA_full <- rbind(ENIGMA_full, CSAN_SubCort_Cort)

#~~~~~~~~~~~~~~~~~~~~
# get only variables that are averaged between left and right
ENIGMA <- ENIGMA_full[, grepl("M" , names(ENIGMA_full))]
# add necessary additional variables without M and rename
ENIGMA <-
  cbind(ENIGMA,
        ENIGMA_full$FullSurfArea,
        ENIGMA_full$SubjID,
        ENIGMA_full$Site)
colnames(ENIGMA)[c(78:80)] <- c("FullSurfArea", "SubjID",  "Site")

# add covariates
ENIGMA_cov <-
  merge(x = ENIGMA,
        y = covariates,
        by = "SubjID",
        all.x = TRUE)

# remove cotrical tickness values that are impossible
#sum(ENIGMA_cov$MThickness[ENIGMA_cov$MThickness>10], na.rm=TRUE)

#I cannot use individuals with NA as their disease status
ENIGMA_cov <- ENIGMA_cov[!is.na(ENIGMA_cov$Dx), ]

if (!file.exists(ENIGMA)) {
  save(ENIGMA, ENIGMA_cov, file = "ENIGMA.RData")
}


#~~~~~~~~~~~~~~~~~~~~~~~
# thickness variables only
ENIGMA_thick <- ENIGMA_cov[, grep("hick", colnames(ENIGMA_cov))]
cov_thick <-
  subset(ENIGMA_cov, select = c("SubjID", "Site", "Dx", "Age", "Sex"))

# How many people are lost due to no Dx variable
nrow(ENIGMA) - nrow(ENIGMA_cov) #306

colSums(is.na(ENIGMA_thick))

#exclude all individuals with many values missing
nrow(cov_thick[rowSums(is.na(ENIGMA_thick)) > 4, ]) #440

cov_thick <- cov_thick[!rowSums(is.na(ENIGMA_thick)) > 4, ]
ENIGMA_thick <- ENIGMA_thick[!rowSums(is.na(ENIGMA_thick)) > 4, ]

#ENIGMA_thick<-sum((is.na(ENIGMA_thick$Sex)))
#~~~~~~~~~~~~~~~~~~~~~~
#save
ENIGMA_thick_cov <- cbind(ENIGMA_thick, cov_thick)

#~~~~~~~~~~~~~~~~~~~~~
# Add site name for site
ENIGMA_thick_cov$SubjID <- as.character(ENIGMA_thick_cov$SubjID)
levels(ENIGMA_thick_cov$SubjID)
ENIGMA_thick_cov$Site_name <-
  str_split(ENIGMA_thick_cov$SubjID,
            "\\_",
            simplify = TRUE,
            n = 2)[, 1]

ENIGMA_thick_cov$Site_name[grepl("Episca*" , ENIGMA_thick_cov$SubjID)] <-
  "Episca"
ENIGMA_thick_cov$Site_name[grepl("AdF*" , ENIGMA_thick_cov$SubjID)] <-
  "AdF"

ENIGMA_thick_cov$Site_name <-
  paste0(ENIGMA_thick_cov$Site, "_", ENIGMA_thick_cov$Site_name)

ENIGMA_thick_cov$Site_name <- as.factor(ENIGMA_thick_cov$Site_name)
levels(ENIGMA_thick_cov$Site_name)

ENIGMA_thick_cov[grepl("AdF" , ENIGMA_thick_cov$SubjID),]

# remove sites

unique(ENIGMA_thick_cov$Site_name)

# sites that will get removed

removed_sites<-ENIGMA_thick_cov[grepl("1_NESDA|32_Gron|1_NESDA|2_NESDA|3_NESDA|22_Novo|
                                          |42_Novo|37_AMC|58_MODECT|15_CODE|16_CODE|17_CODE|
                                          |18_CODE|19_CODE|13_Dub3T|14_Dub1.5", ENIGMA_thick_cov$Site_name),] # 763

ENIGMA_thick_cov<-ENIGMA_thick_cov[!grepl("1_NESDA|32_Gron|1_NESDA|2_NESDA|3_NESDA|22_Novo|
                                          |42_Novo|37_AMC|58_MODECT|15_CODE|16_CODE|17_CODE|
                                          |18_CODE|19_CODE|13_Dub3T|14_Dub1.5", ENIGMA_thick_cov$Site_name),]
#boxplots, one per site

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ENIGMA_thick_cov$Site_name<-as.factor(ENIGMA_thick_cov$Site_name)
ENIGMA_thick_cov$Dx<-as.factor(ENIGMA_thick_cov$Dx)
ENIGMA_thick_cov$Sex<-as.factor(ENIGMA_thick_cov$Sex)

# visualize boxplots
if(MAKE_PLOTS){
  start <- which(colnames(ENIGMA_thick_cov) == "M_bankssts_thickavg")
  end <- which(colnames(ENIGMA_thick_cov) == "MThickness")
  for (i in start:end) {
    plot <-
      ggplot(ENIGMA_thick_cov,
             aes(x = Site_name, y = ENIGMA_thick_cov[, i], fill = Dx)) +
      #scale_fill_manual(values=cbPalette)+
      geom_boxplot(outlier.colour = "black") +
      scale_x_discrete(breaks = levels(ENIGMA_thick_cov$Site_name)) +
      xlab("Site") + ylab(paste(colnames(ENIGMA_thick_cov[i]))) + theme(axis.text.x = element_text(angle = 90))
    filename = paste0('~/Dropbox/ENIGMA/Boxplot2/Thickness/',
                      colnames(ENIGMA_thick_cov)[i],
                      '.png')
    ggsave(
      filename = filename,
      plot = plot,
      width = 16,
      height = 9
    )
  }
}

# split into training and test set
# nwe try with caret
train_controls <- ENIGMA_thick_cov[ENIGMA_thick_cov$Dx == 0, ]
test_MDD <- ENIGMA_thick_cov[ENIGMA_thick_cov$Dx == 1, ]

#make sure that Sex is a factor
#train_controls$Sex<- as.factor(train_controls$Sex)
#test_MDD$Sex<- as.factor(test_MDD$Sex)

set.seed(1)
trainIndex <- createDataPartition(train_controls$Sex, p = .6,
                                  list = FALSE, 
                                  times = 1)

train_cont <- train_controls[ trainIndex,]
test_cont  <- train_controls[-trainIndex,]


table(test_cont$Site)
table(train_cont$Site)
table(test_MDD$Site)

train_cont$Set<-"train"
test_cont$Set<-"test"
test_MDD$Set<-"MDD"

g<-rbind(train_cont, test_cont, test_MDD)
#this is the cool overview
if(MAKE_PLOTS) {
  ggplot(g, aes(
    x = Age,
    y = MThickness,
    group = Set,
    colour = Set,
    fill = Set
  )) + geom_point(alpha = 0.5) + geom_smooth() + ylim(c(2, 5)) +
    theme_classic() + facet_wrap( ~ Site, nrow = 7)
  ggsave(
    file = "MThickness_caret_06.png",
    height = 15,
    width = 15,
    unit = "cm",
    dpi = 300
  )

  ggplot(g, aes(
    x = Age,
    group = Set,
    colour = Set,
    fill = Set
  )) + geom_histogram(position = "stack", alpha = 0.5) +
    scale_color_jama(palette = "default") + scale_fill_jama(palette = "default") +
    theme_classic()
  ggsave(
    file = "Age_histogram.png",
    height = 7,
    width = 15,
    unit = "cm",
    dpi = 300
  )
  
  ggplot(g, aes(
    x = Age,
    y = MThickness,
    group = Set,
    colour = Set,
    fill = Set
  )) + geom_point(alpha = 0.5) + geom_smooth() +
    scale_color_jama(palette = "default") + scale_fill_jama(palette = "default") +
    theme_classic()
  ggsave(
    file = "MThickness.png",
    height = 15,
    width = 15,
    unit = "cm",
    dpi = 300
  )
  
  ggplot(g, aes(
    x = Site,
    group = Set,
    colour = Set,
    fill = Set
  )) + geom_histogram(position = "stack", alpha = 0.5) +
    scale_color_jama(palette = "default") + scale_fill_jama(palette = "default") +
    theme_classic()
  
  ggplot(g,
         aes(
           x = Age,
           y = MThickness,
           group = Site_name,
           colour = Site_name,
           fill = Site_name
         )) + geom_point(alpha = 0.5) + geom_smooth() +
    theme_classic() + facet_wrap( ~ Set, nrow = 3)
}

range(train_cont$MThickness)
range(test_cont$MThickness)
range(test_MDD$MThickness)


preProcValues <-
  preProcess(train_cont[, c(1:35, 39)], method = c("knnImpute"), k = 10)
imp_train_cont <- train_cont
imp_train_cont[, c(1:35, 39)] <-
  predict(preProcValues, train_cont[, c(1:35, 39)])
imp_test_cont <- test_cont
imp_test_cont[, c(1:35, 39)] <-
  predict(preProcValues, test_cont[, c(1:35, 39)])
imp_test_MDD <- test_MDD
imp_test_MDD[, c(1:35, 39)] <-
  predict(preProcValues, test_MDD[, c(1:35, 39)])

if(MAKE_PLOTS) {
  p1 <-
    ggplot(imp_train_cont, aes(
      x = Age,
      color = Sex,
      fill = Sex,
      group = Sex
    )) + geom_histogram(alpha = 0.5,
                        position = "identity",
                        binwidth = 0.1) +
    ylim(c(0, 200)) + xlim(c(-3, 3))
  p2 <-
    ggplot(imp_test_cont, aes(
      x = Age,
      color = Sex,
      fill = Sex,
      group = Sex
    )) + geom_histogram(alpha = 0.5,
                        position = "identity",
                        binwidth = 0.1) +
    ylim(c(0, 200)) + xlim(c(-3, 3))
  p3 <-
    ggplot(imp_test_MDD, aes(
      x = Age,
      color = Sex,
      fill = Sex,
      group = Sex
    )) + geom_histogram(alpha = 0.5,
                        position = "identity",
                        binwidth = 0.1) +
    ylim(c(0, 200)) + xlim(c(-3, 3))
  
  plot <- (p1) / (p2) / (p3)
  
  p_dml <- rvg::dml(ggobj = plot)
  
  # initialize PowerPoint slide
  officer::read_pptx() %>%
    # add slide
    officer::add_slide() %>%
    # specify object and location of object
    officer::ph_with(p_dml, location = ph_location_type(type = "body")) %>%
    # export slide
    base::print(target = here::here("Demographics.pptx"))
}

if(!file.exists(file.path("imp_random_06_caret.RData"))){
  save(imp_test_cont,
       imp_test_MDD,
       imp_train_cont,
       preProcValues,
       file = "imp_random_06_caret.RData")
}
