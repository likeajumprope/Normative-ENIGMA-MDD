# t-tests for z-scores of all regions ####

# This is the one in the paper
library(ggseg3d)
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(officer)
library(ggplot2)


MDD_zscore$label<-"MDD"
cont_zscore$label<-"cont"

cohen<-data.frame(matrix(nrow=35, ncol=2))
colnames(cohen)[1]<-"cohen"
zscore<-rbind(MDD_zscore, cont_zscore)

for (i in 2:36){
  co<-cohen.d(zscore[,i]~zscore$label, pooled=TRUE)
  cohen[i-1,1]<-co$estimate
  cohen[i-1,2]<-colnames(zscore)[i]
  #p<-aggregate(zscore[,i]~zscore$label, FUN="mean")
  #cohen[i-1,3]<-p[1,2]
  #cohen[i-1,4]<-p[2,2]
}

t_table<-data.frame(matrix(nrow=35, ncol=2))
colnames(t_table)[1]<-"t_table"
zscore<-rbind(MDD_zscore, cont_zscore)
for (i in 2:36){
  test<-t.test(zscore[,i]~zscore$label)
  t_table[i-1,1]<-test$p.value
  t_table[i-1,2]<-colnames(zscore)[i]
}
p<-t_table$t_table

p_adjust<-p.adjust(p, "fdr", n = length(p))
cohen$p_adjust<-p_adjust

colnames(cohen)[2]<-"ROI"

#cohen<-cohen[match(order, cohen$X2),]
write.csv(cohen, file="cohen_means_adjusted_wo_age_sex_dx.csv")

cohen[cohen$p_adjust<0.05,]

#### Images 

someData = dk_3d %>% 
  filter(surf == "inflated" & hemi == "right") %>% 
  unnest(ggseg_3d) %>% 
  ungroup() %>% 
  select(region) %>% 
  na.omit() %>% 
  mutate(p = sample(seq(0,.5, length.out = 100 ), nrow(.)) %>% 
           round(2)) 

cohen<- read.csv("cohen_means_adjusted_wo_age_sex_dx.csv")
#colnames(cohen)[2]<-c("cohen")
#colnames(cohen)[3]<-c("adjp")

library(dplyr)
  cohen <- cohen %>%
    mutate(
      ROI = recode(
        ROI,
        "M_fusiform_thickavg" = "fusiform",
        "M_inferiortemporal_thickavg" = "inferior temporal" ,
        "M_middletemporal_thickavg" =  "middle temporal" ,
        "M_medialorbitofrontal_thickavg" = "medial orbitofrontal",
        "M_insula_thickavg" = "insula",
        "M_precentral_thickavg" =  "precentral",
        "M_bankssts_thickavg" = "bankssts",
        "M_parsopercularis_thickavg" = "pars opercularis",
        "M_supramarginal_thickavg" = "supramarginal",
        "M_lateralorbitofrontal_thickavg" = "lateral orbitofrontal",
        "M_parsorbitalis_thickavg" = "pars orbitalis",
        "M_parahippocampal_thickavg"  = "parahippocampal",
        "M_transversetemporal_thickavg"   = "transverse temporal" ,
        "M_precuneus_thickavg"  =  "precuneus",
        "M_postcentral_thickavg"  = "postcentral" ,
        "M_isthmuscingulate_thickavg" = "isthmus cingulate" ,
        "M_lateraloccipital_thickavg"   = "lateral occipital" ,
        "M_superiorfrontal_thickavg"    = "superior frontal" ,
        "M_parstriangularis_thickavg"  = "pars triangularis",
        "M_rostralmiddlefrontal_thickavg"   = "rostral middle frontal",
        "M_caudalanteriorcingulate_thickavg" = "caudal anterior cingulate" ,
        "M_entorhinal_thickavg"   = "entorhinal",
        "M_pericalcarine_thickavg"   = "pericalcarine",
        "M_lingual_thickavg"    = "lingual",
        "M_superiorparietal_thickavg"  = "superior parietal",
        "M_caudalmiddlefrontal_thickavg" = "caudal middle frontal",
        "M_frontalpole_thickavg"   = "frontal pole" ,
        "M_temporalpole_thickavg"    = "temporal pole" ,
        "M_cuneus_thickavg" =  "cuneus", 
        "M_paracentral_thickavg"   = "paracentral",            
        "M_superiortemporal_thickavg"     = "superior temporal" ,  
        "M_posteriorcingulate_thickavg" = "posterior cingulate" ,
        "M_rostralanteriorcingulate_thickavg" = "rostral anterior cingulate",
        "M_inferiorparietal_thickavg"  = "inferior parietal"
      )
    )

# recode the names
outmat<-data.frame(outmat)
outmat$ROI <- rownames(outmat)
outmat<-region_converter(outmat, ROI)

someData<-merge(someData, cohen, by.x = "region", by.y = "ROI", all.x = TRUE)

someData <-
  someData %>%
  mutate(cohen = if_else (p_adjust > 0.05, 0, cohen))

p<-ggseg3d(.data = someData, 
           atlas = dk_3d,
           colour = "cohen", text = "cohen", palette = c("grey"=0, "red3" = 0.20)) %>%  
  remove_axes() %>% 
  pan_camera("right lateral")


# widget
myp<-ggplotly(p)
saveWidget(myp, file="cohen.html")

# Ppt.

p_dml <- rvg::dml(ggobj = myp)

# initialize PowerPoint slide ----
officer::read_pptx() %>%
  # add slide ----
officer::add_slide() %>%
  # specify object and location of object ----
officer::ph_with(p_dml, ph_location()) %>%
  # export slide -----
base::print(target = here::here("cohen_lateral_wo_age_sex_dx.pptx"))

#calucalte cohen average

mean(cohen$cohen)


