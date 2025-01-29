library(stringr)
library(ggpubr)
library(officer)
install.packages("nnet")
library(nnet)
library(plotly)
library(htmlwidgets)


## Raw data ENIGMA

### Load data

setwd("~/Dropbox/ENIGMA")

ENIGMA_full <-
  read.csv("raw_data/Subcort_Cort_lifespan.csv") #read dataset
covariates <- read.csv("raw_data/Covariates_lifespan.csv")
covariates <-
  covariates[!(is.na(covariates$Age) | is.na(covariates$Sex)), ]


### Load updates in data

#these are the updates
MODECT_SubCort_Cort <-
  read.csv("raw_data/MODECT_SubCort_Cort.csv") # replace with existing variables
CSAN_Basic_Covariates <-
  read.csv("raw_data/CSAN_Basic_Covariates.csv")
CSAN_SubCort_Cort <- read.csv("raw_data/CSAN_SubCort_Cort.csv")

### Merge

#merge covaraites
covariates <-
  rbind(covariates[, 2:14], CSAN_Basic_Covariates[, c(1:4, 6:14)])

ENIGMA_full[grepl("MODECT" , ENIGMA_full$SubjID),][, c(2:237)] <-
  MODECT_SubCort_Cort
ENIGMA_full <- ENIGMA_full[, c(2:238)]
CSAN_SubCort_Cort$Site <- 65

colnames(ENIGMA_full) <- colnames(CSAN_SubCort_Cort)

ENIGMA_full <- rbind(ENIGMA_full, CSAN_SubCort_Cort)



### String split


ENIGMA_full$SubjID <- as.character(ENIGMA_full$SubjID)
levels(ENIGMA_full$SubjID)
ENIGMA_full$Site_name <-
  str_split(ENIGMA_full$SubjID, "\\_", simplify = TRUE, n = 2)[, 1]

ENIGMA_full$Site_name[grepl("Episca*" , ENIGMA_full$SubjID)] <-
  "Episca"
ENIGMA_full$Site_name[grepl("AdF*" , ENIGMA_full$SubjID)] <- "AdF"

ENIGMA_full$Site_name <-
  paste0(ENIGMA_full$Site, "_", ENIGMA_full$Site_name)

ENIGMA_full$Site_name <- as.factor(ENIGMA_full$Site_name)
#unique(ENIGMA_full$Site_name)

ENIGMA_full[grepl("AdF" , ENIGMA_full$SubjID),]



### Sites and sex in the full data set

unique(ENIGMA_full$Site)


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
ENIGMA_cov <- ENIGMA_cov[!is.na(ENIGMA_cov$Dx),]


ENIGMA_thick <- ENIGMA_cov[, grep("hick", colnames(ENIGMA_cov))]
cov_thick <-
  subset(ENIGMA_cov, select = c("SubjID", "Site", "Dx", "Age", "Sex"))


colSums(is.na(ENIGMA_thick))

#exclude all individuals with many values missing
cov_thick <- cov_thick[!rowSums(is.na(ENIGMA_thick)) > 4,]
ENIGMA_thick <- ENIGMA_thick[!rowSums(is.na(ENIGMA_thick)) > 4,]




ENIGMA_thick_cov <- cbind(ENIGMA_thick, cov_thick)

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

ENIGMA_thick_cov[grepl("AdF" , ENIGMA_thick_cov$SubjID), ]


#unique(ENIGMA_thick_cov$Site_name)
## Imputed data ENIGMA, training set

# Display all the sites in the imputed training set:

load("~/Dropbox/ENIGMA/derivatives/imp_random_06_caret.RData")
sort(unique(imp_train_cont$Site_name))

# The number of scanners that the model was trained on is 40.
imp_train_cont$Site_name2 <-
  str_split(imp_train_cont$Site_name,
            "\\_",
            simplify = TRUE,
            n = 2)[, 2]
unique(imp_train_cont$Site_name2)


# The number of sites that the analysis was done on is 27.

## Figure showing distribution of age and sex between training and test set
gghistogram(
  imp_train_cont,
  x = "Age",
  fill = "Sex",
  binwidth = 1,
  position = "stack",
  title = "Training set"
)


gghistogram(
  imp_test_cont,
  x = "Age",
  fill = "Sex",
  binwidth = 1,
  position = "stack",
  title = "Training set"
)



gghistogram(
  imp_test_MDD,
  x = "Age",
  fill = "Sex",
  binwidth = 1,
  position = "stack",
  title = "Training set"
)

# Get individuals from ENIGMA that are inlcuded in the sample in order to make the plots

imp_train_ENIGMA <-
  merge(x = ENIGMA_thick_cov,
        y = imp_train_cont,
        by = "SubjID",
        all.y = TRUE)

imp_train_ENIGMA <-
  imp_train_ENIGMA[, c("Age.x", "Sex.x", "Dx.x", "Site.x", "Site_name.x")]
colnames(imp_train_ENIGMA) <-
  c("Age", "Sex", "Dx", "Site", "Site_name")
imp_train_ENIGMA$Sex <- as.factor(imp_train_ENIGMA$Sex)

imp_train_ENIGMA <- imp_train_ENIGMA %>%
  mutate(Sex = factor(Sex, levels = c("1", "2"),
                      labels = c("M", "F")))

p <-
  gghistogram(
    imp_train_ENIGMA,
    x = "Age",
    fill = "Sex",
    color = "Sex",
    binwidth = 1,
    position = "stack",
    title = "Healthy Controls: Training set",
    palette = c("#00AFBB", "#E7B800")
  )

# p_dml <- rvg::dml(ggobj = p)
#
# # initialize PowerPoint slide ----
# officer::read_pptx() %>%
#   # add slide ----
# officer::add_slide() %>%
#   # specify object and location of object ----
# officer::ph_with(p_dml, ph_location()) %>%
#   # export slide -----
# base::print(target = here::here("control_train.pptx"))

# save to html - interactive plot
interactive_plot <- ggplotly(p)
saveWidget(interactive_plot,
           "figures/train_demographics.html",
           selfcontained = TRUE)

#%-------- test controls
imp_test_ENIGMA <-
  merge(x = ENIGMA_thick_cov,
        y = imp_test_cont,
        by = "SubjID",
        all.y = TRUE)

imp_test_ENIGMA <-
  imp_test_ENIGMA[, c("Age.x", "Sex.x", "Dx.x", "Site_name.x")]
colnames(imp_test_ENIGMA) <- c("Age", "Sex", "Dx", "Site_name")
imp_test_ENIGMA <- imp_test_ENIGMA %>%
  mutate(Sex = factor(Sex, levels = c("1", "2"),
                      labels = c("M", "F")))

imp_test_ENIGMA$Sex <- as.factor(imp_test_ENIGMA$Sex)

p <-
  gghistogram(
    imp_test_ENIGMA,
    x = "Age",
    fill = "Sex",
    color = "Sex",
    binwidth = 1,
    position = "stack",
    title = "Healthy Controls: Test set",
    palette = c("#00AFBB", "#E7B800")
  )

# p_dml <- rvg::dml(ggobj = p)
#
# # initialize PowerPoint slide ----
# officer::read_pptx() %>%
#   # add slide ----
# officer::add_slide() %>%
#   # specify object and location of object ----
# officer::ph_with(p_dml, ph_location()) %>%
#   # export slide -----
# base::print(target = here::here("control_test.pptx"))

interactive_plot <- ggplotly(p)
saveWidget(interactive_plot,
           "figures/test_demographics.html",
           selfcontained = TRUE)

#%----

# test MDD
imp_test_MDD_ENIGMA <-
  merge(x = ENIGMA_thick_cov,
        y = imp_test_MDD,
        by = "SubjID",
        all.y = TRUE)

imp_test_MDD_ENIGMA <-
  imp_test_MDD_ENIGMA[, c("Age.x", "Sex.x", "Dx.x", "Site_name.x")]
colnames(imp_test_MDD_ENIGMA) <- c("Age", "Sex", "Dx", "Site_name")
imp_test_MDD_ENIGMA <- imp_test_MDD_ENIGMA %>%
  mutate(Sex = factor(Sex, levels = c("1", "2"),
                      labels = c("M", "F")))

imp_test_MDD_ENIGMA$Sex <- as.factor(imp_test_MDD_ENIGMA$Sex)

p <-
  gghistogram(
    imp_test_MDD_ENIGMA,
    x = "Age",
    fill = "Sex",
    color = "Sex",
    binwidth = 1,
    position = "stack",
    title = "Depression: Test Set",
    palette = c("#00AFBB", "#E7B800")
  )

# p_dml <- rvg::dml(ggobj = p)
#
# # initialize PowerPoint slide ----
# officer::read_pptx() %>%
#   # add slide ----
# officer::add_slide() %>%
#   # specify object and location of object ----
# officer::ph_with(p_dml, ph_location()) %>%
#   # export slide -----
# base::print(target = here::here("MDD_test.pptx"))

interactive_plot <- ggplotly(p)
saveWidget(
  interactive_plot,
  "figures/test_MDD_demographics.html",
  selfcontained = TRUE,
  libdir = NULL
)

##

## age sex data set ----------
## tests
# sex
colnames(imp_test_MDD_ENIGMA)
imp_test_MDD_ENIGMA$Set <- "test_MDD"
imp_test_ENIGMA$Set <- "test_cont"
imp_train_ENIGMA$Set <- "train_cont"

df_demo <-
  rbind(imp_test_ENIGMA[, c("Sex", "Dx", "Age", "Site_name", "Set")], imp_train_ENIGMA[, c("Sex", "Dx", "Age", "Site_name", "Set")],
        imp_test_MDD_ENIGMA[, c("Sex", "Dx", "Age", "Site_name", "Set")])


df_demo$Set <- as.factor(df_demo$Set)
df_demo$Set <- relevel(df_demo$Set, ref = "train_cont")

df_demo$Sex <- as.factor(df_demo$Sex)
# Multinomial logistic regression with interaction
multi_model <- multinom(Set ~ Age * Sex, data = df_demo)
summary(multi_model)

exp(coef(multi_model))

# Extract coefficients and standard errors
z_values <-
  summary(multi_model)$coefficients / summary(multi_model)$standard.errors

# Compute two-tailed p-values
p_values <- 2 * (1 - pnorm(abs(z_values)))

# Display p-values
p_values

sex_table <- table(df_demo$Sex, df_demo$Set)
percent_table <- prop.table(sex_table) * 100
percent_table <- as.data.frame(percent_table)

# Column-wise percentages
col_percent <- prop.table(sex_table, margin = 2) * 100
print(round(col_percent, 2))

# age

# Create age groups
df_demo$age_group <-
  cut(df_demo$Age,
      breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90),
      right = FALSE)

# Contingency table
age_table <- table(df_demo$Set, df_demo$age_group)
perc_table_age <- prop.table(age_table)

age_table
perc_table_age <- round(perc_table_age * 100, 2)
perc_table_age
# Column-wise percentages
col_percent <- prop.table(age_table, margin = 2) * 100
print(round(col_percent, 2))
