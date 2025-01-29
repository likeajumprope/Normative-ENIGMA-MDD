## Spider plot and individual profiles
library(ggradar)
library(officer)

load("~/Dropbox/ENIGMA/derivatives/derivatives.RData")
source("~/Dropbox/ENIGMA/covariates.R")

# How many individuals do at least have one positive deviation
MDD_positive <- (as.matrix((MDD_zscore[, c(2:36)] > 1.96)) * 1)
MDD_negative <- (as.matrix((MDD_zscore[, c(2:36)] < (-1.96))) * -1)

sum(rowSums(MDD_positive) >= 1) / nrow(MDD_positive)

sum(rowSums(MDD_negative) <= (-1)) / nrow(MDD_negative)

MDD_pos <- sum(rowSums(MDD_positive) >= 1)
MDD_neg <- sum(rowSums(MDD_negative) <= (-1))

#controls

cont_positive <- (as.matrix((cont_zscore[, c(2:36)] > 1.96)) * 1)
cont_negative <- (as.matrix((cont_zscore[, c(2:36)] < (-1.96))) * -1)

sum(rowSums(cont_positive) >= 1) / nrow(cont_positive)

sum(rowSums(cont_negative) <= (-1)) / nrow(cont_negative)

#Correlation of load with clinical variables
MDD_positive_load <- rowSums(MDD_positive)
MDD_negative_load <- rowSums(MDD_negative)

MDD_SubjID <- MDD_zscore$SubjID
MDD_SubjID <- as.data.frame(MDD_SubjID)
colnames(MDD_SubjID) <- "SubjID"

MDD_load <-
  data.frame(MDD_positive_load, MDD_negative_load, MDD_SubjID)

MDD_load_cov <- merge(MDD_load, cov, by.x = "SubjID", all.x = TRUE)

# most negative average z-score
min(MDD_load_cov$MDD_negative_load)

which(MDD_load$MDD_negative_load == (-33))
MDD_load[3002, ]
Sub1 <- MDD_zscore[3002, ]
MDD_load_cov[3002,]

p<-ggradar(
  Sub1[c(2:36)],
  values.radar = c("-10", "0", "10"),
  grid.min = -10,
  grid.max = 10,
  grid.mid = 0,
  group.line.width = 1,
  group.point.size = 4,
  group.colours = c("#F8766D", "#00BFC4"),
  legend.text.size = 10,
  legend.position = "bottom"
)

p_dml <- rvg::dml(ggobj = p)
officer::read_pptx() %>%
officer::add_slide() %>%
officer::ph_with(p_dml, ph_location()) %>%
base::print(
  target = here::here(
    "Most_negative_load.pptx"
  )
)

#ggsave(file = "Most_negative_load.png")

## Severity

load("~/Dropbox/ENIGMA/test_tables.RData")
source("~/Dropbox/ENIGMA/scr/unPreProc.R")

MDD_zscore_df<-MDD_zscore %>% 
  data.frame() %>%
  select(!"SubjID")

cont_zscore_df<-cont_zscore %>% 
  data.frame() %>%
  select(!"SubjID")


severity_MDD_pos<-data.frame("severity" =do.call(`pmax`, MDD_zscore_df))
severity_MDD_neg<-data.frame("severity" =do.call(`pmin`, MDD_zscore_df))

severity_cont_pos<-data.frame("severity" =do.call(`pmax`, cont_zscore_df))
severity_cont_neg<-data.frame("severity"= do.call(`pmin`, cont_zscore_df))


min(severity_MDD_neg$severity)

which(severity_MDD_neg$severity == (-8.92688))
MDD_load[3002, ]
Sub1 <- MDD_zscore[3002, ]

p<-ggradar(
  Sub1[c(2:36)],
  values.radar = c("-10", "0", "10"),
  grid.min = -10,
  grid.max = 10,
  grid.mid = 0,
  group.line.width = 0.5,
  group.point.size = 1.5,
  group.colours = c("#F8766D", "#00BFC4"),
  legend.text.size = 12,
  legend.position = "bottom"
)

p_dml <- rvg::dml(ggobj = p)
officer::read_pptx() %>%
  officer::add_slide() %>%
  officer::ph_with(p_dml, ph_location()) %>%
  base::print(
    target = here::here(
      "Most_negative_load.pptx"
    )
  )

## Average negative z-score

MDD<-data.frame(zscore = MDD_zscore$MThickness, Age= un_imp_test_MDD$Age, Sex= un_imp_test_MDD$Sex)
cont<-data.frame(zscore = cont_zscore$MThickness, Age= un_imp_test_cont$Age, Sex= un_imp_test_cont$Sex)

which(MDD ==min(MDD$zscore))


Sub1 <- MDD_zscore[3473, ]
MDD_load_cov[3473, ]

p<-ggradar(
  Sub1[c(2:36)],
  values.radar = c("-10", "0", "10"),
  grid.min = -10,
  grid.max = 10,
  grid.mid = 0,
  group.line.width = 0.5,
  group.point.size = 1.5,
  group.colours = c("#F8766D", "#00BFC4"),
  legend.text.size = 10,
  legend.position = "bottom"
)

p_dml <- rvg::dml(ggobj = p)
officer::read_pptx() %>%
  officer::add_slide() %>%
  officer::ph_with(p_dml, ph_location()) %>%
  base::print(
    target = here::here(
      "Most_negative_avgz.pptx"
    )
  )

