devtools::install_github("AndreaCirilloAC/updateR")
library(updateR)
library(ggseg3d)
#vignette("ggseg3d")
library(ggseg)
library(dplyr)
library(tidyr)
library(officer)
library(ggplot2)
library(plotly)
library(htmlwidgets)

#run heterogeneity before

# read in percentage table
prc_table <- read_csv("perc_table.csv")


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

data <-region_converter(perc_table, region)

data <-merge(someData, data, by="region")
data[35,]<-c("corpus callosum", 0,0,0,0,0)

#make sure tgat
data<-data[match(order, data$region),]


#Negative control
#perc_cont_neg<-perc_cont_neg[match(order, perc_cont_neg$X2),]
someData$p<-data$perc_cont_neg

p<-ggseg3d(.data = someData, 
        atlas = dk_3d,
        colour = "p", text = "p", palette = c("white" = 0, "red3"=0.1, "red3" = 0.15)) %>%  
  remove_axes() %>% 
  pan_camera("right lateral")

myp<-ggplotly(p)
saveWidget(myp, file="perc_cont_neg.html", selfcontained = F, libdir = "lib" )

#Positive control
#perc_cont_pos<-perc_cont_pos[match(order, perc_cont_pos$X2),]
someData$p<-data$perc_cont_pos

p<-ggseg3d(.data = someData, 
        atlas = dk_3d,
        colour = "p", text = "p", palette = c("white" = 0, "darkblue" = 0.15)) %>% 
  remove_axes() %>% 
  pan_camera("right lateral")

myp<-ggplotly(p)
saveWidget(myp, file="perc_cont_pos.html")

#Negative MDD
#perc_MDD_neg<-perc_MDD_neg[match(order, perc_MDD_neg$X2),]
someData$p<-data$perc_MDD_neg
someData$p<-as.numeric(someData$p)

p<-ggseg3d(.data = someData, 
        atlas = dk_3d,
        colour = "p", text = "p",palette = c("white" = 0, "red3"=0.1, "red3" = 0.15)) %>% 
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
        colour = "p", text = "p", palette = c("white" = 0, "darkblue" = 0.15)) %>% 
  remove_axes() %>% 
  #pan_camera("right lateral")
  pan_camera("right medial")

myp<-ggplotly(p)
saveWidget(myp, file="perc_MDD_pos2.html")