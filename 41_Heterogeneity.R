devtools::install_github("AndreaCirilloAC/updateR")
library(updateR)
library(ggseg3d)
library(ggseg)
library(dplyr)
library(tidyr)
library(officer)
library(ggplot2)
library(plotly)
library(htmlwidgets)
library(readxl)
library(readr)
library(viridis)

source("./region_converter.R")
#run heterogeneity before

# read in percentage table
prc_table_old <- read_csv("./perc_table.csv")
prc_table <- read_xlsx("./40_analysis/perc_table.xlsx")

prc_table <- prc_table[, -c(2, 4, 6)] 
prc_table <- prc_table[order(prc_table$region_MDD_pos), ]

prc_table_old<- prc_table_old[order(prc_table_old$region), ]

p <- ggseg3d(atlas=dk_3d) %>% 
  remove_axes() %>% 
  pan_camera("right medial")

dk_3d

someData = dk_3d %>% 
  filter(surf == "inflated" & hemi == "right") %>% 
  unnest(ggseg_3d) %>% 
  ungroup() %>% 
  select(region) %>% 
  na.omit() %>% 
  mutate(p = sample(seq(0,.5, length.out = 100 ), nrow(.)) %>% 
           round(2)) 
# get actual order
order <- someData$region

data <-region_converter(prc_table, unique(prc_table$region_MDD_pos))

data <-merge(someData, data, by.x ="region", by.y="ROI")
data[35,]<-c("corpus callosum", 0,0,0,0,0)

#make sure that
data<-data[match(order, data$region),]


#Negative control
#perc_cont_neg<-perc_cont_neg[match(order, perc_cont_neg$X2),]
someData$p<-data$perc_HC_neg
someData$p<-as.numeric(someData$p)

p<- ggseg3d(.data = someData, 
           atlas = dk_3d,
           colour = "p", text = "p", palette = c("white" =0, "white" = 0.05, "red3"=0.1,  "red4" = 0.15)) %>%  
  remove_axes() %>% 
  pan_camera("right lateral")


myp<-ggplotly(p)
saveWidget(myp, file="perc_HC_neg.html", selfcontained = F, libdir = "lib" )

#Positive control
#perc_cont_pos<-perc_cont_pos[match(order, perc_cont_pos$X2),]
someData$p<-data$perc_HC_pos
someData$p<-as.numeric(someData$p)

p<-ggseg3d(.data = someData, 
        atlas = dk_3d,
        colour = "p", text = "p", palette = c("white" = 0, "white"= 0.05, "blue2"= 0.1, "blue4" = 0.15)) %>% 
  remove_axes() %>% 
  pan_camera("right lateral")

myp<-ggplotly(p)
saveWidget(myp, file="perc_HC_pos.html")

#Negative MDD
#perc_MDD_neg<-perc_MDD_neg[match(order, perc_MDD_neg$X2),]
someData$p<-data$perc_MDD_neg
someData$p<-as.numeric(someData$p)

p<-ggseg3d(.data = someData, 
        atlas = dk_3d,
        colour = "p", text = "p",palette = c("white" = 0,"white"=0.05,  "red3"=0.1, "red4" = 0.15)) %>% 
  remove_axes() %>% 
  pan_camera("right medial")
  #pan_camera("right lateral")

myp<-ggplotly(p)
saveWidget(myp, file="perc_MDD_neg.html")

#Positive MDD
#perc_MDD_pos<-perc_MDD_pos[match(order, perc_MDD_pos$X2),]
someData$p<-data$perc_MDD_pos
someData$p<-as.numeric(someData$p)

p<-ggseg3d(.data = someData, 
        atlas = dk_3d,
        colour = "p", text = "p", palette = c("white" = 0, "white"=0.05, "blue2"=0.1, "blue4" = 0.15)) %>% 
  remove_axes() %>% 
  #pan_camera("right lateral")
  pan_camera("right medial")

myp<-ggplotly(p)
saveWidget(myp, file="perc_MDD_pos.html")

#------
#Difference
# negative
data$neg_diff <- data$perc_MDD_neg - data$perc_HC_neg

someData$p<-data$neg_diff
someData$p<-as.numeric(someData$p)


ggseg3d(.data = someData, 
           atlas = dk_3d,
           colour = "p", text = "p", palette = c("red3" = -0.04, "white" =0.0, "blue4" = 0.04)) %>% 
  remove_axes() %>% 
  #pan_camera("right lateral")
  pan_camera("right medial")

# positive
data$pos_diff <- data$perc_MDD_pos - data$perc_HC_pos

someData$p<-data$pos_diff
someData$p<-as.numeric(someData$p)


ggseg3d(.data = someData, 
        atlas = dk_3d,
        colour = "p", text = "p", palette = c("red3" = -0.04, "white" =0.0, "blue4" = 0.04)) %>% 
  remove_axes() %>% 
  #pan_camera("right lateral")
  pan_camera("right medial")


ggseg3d(
  .data = someData,
  atlas = dk_3d,
  colour = "p",               # Map the "p" column for coloring
  text = "p",                 # Display "p" values as text
  palette = viridis_pal(option = "heat")(100)  # Apply the viridis palette
) %>%
  remove_axes() %>%
  pan_camera("right medial")

ggseg3d(
  .data = someData,
  atlas = dk_3d,
  colour = "p",                # Map the "p" column for coloring
  text = "p",                  # Display "p" values as text
  palette = heat.colors(100)   # Use the heat palette with 100 colors
) %>%
  remove_axes() %>%
  pan_camera("right medial")

# -------

# HC
data$neg_diff <- data$perc_MDD_neg - data$perc_HC_neg

someData$p<-data$neg_diff
someData$p<-as.numeric(someData$p)


ggseg3d(.data = someData, 
        atlas = dk_3d,
        colour = "p", text = "p", palette = c("red3" = -0.04, "white" =0.0, "blue4" = 0.04)) %>% 
  remove_axes() %>% 
  #pan_camera("right lateral")
  pan_camera("right medial")

# positive
data$pos_diff <- data$perc_MDD_pos - data$perc_HC_pos

someData$p<-data$pos_diff
someData$p<-as.numeric(someData$p)


ggseg3d(.data = someData, 
        atlas = dk_3d,
        colour = "p", text = "p", palette = c("red3" = -0.04, "white" =0.0, "blue4" = 0.04)) %>% 
  remove_axes() %>% 
  #pan_camera("right lateral")
  pan_camera("right medial")
