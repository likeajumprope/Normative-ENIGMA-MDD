# Spider plots for group differences
devtools::install_github("ricardo-bion/ggradar")

library(ggplot2)
library(tidyverse)
library(ggradar)
library(officer)
library(emmeans)
library(htmlwidgets)
library(plotly)

# add covs to z-scores

# laod data taken from server
load("~/Dropbox/ENIGMA/derivatives/derivatives.RData")

MDD_zscore$label<-"MDD"
cont_zscore$label<-"cont"

mean_frame<-data.frame(matrix(nrow=35, ncol=4))
z_cov<-rbind(MDD_zscore, cont_zscore)

z_cov<-z_cov%>%
  inner_join(cov, by= "SubjID")
  

for (i in 2:36){
  m<-lm(z_cov[,i]~z_cov$Age*z_cov$Sex*z_cov$Dx)
  emm<-summary(emmeans(m, specs= pairwise ~ Dx))
  mean_frame[i-1,1]<-emm$emmeans$emmean[1]
  mean_frame[i-1,2]<-emm$emmeans$emmean[2]
  mean_frame[i-1,3]<-colnames(MDD_cov)[i]
  mean_frame[i-1,4]<-emm$contrasts$p.value
}

means<-as.data.frame(t(mean_frame[c(1:35), c(1:2)]))
means<-data.frame("group"=c("cont","MDD"), means)


colnames(means)<-c("group","bank of the superior sulcus","caudal anterior cingulate cortex","cauda lmiddle frontal gyrus","cuneus","entorhinal",      
"frontal pole","fusiform","inferior parietal","inferior temporal ","insula",            
"isthmus cingulate cortex","lateral occipital ","lateral orbitofrontal ","lingual ","medial orbitofrontal", 
"middle temporal","paracentral ","parahippocampal ","pars opercularis","pars orbitalis",    
"pars triangularis","pericalcarine","postcentral","posteriorcingulate","precentral",     
"precuneus","rostral anteriorcingulate","rostral middlefrontal","superiorfrontal","superiorparietal",    
"superiortemporal","supramarginal","temporalpole","transversetemporal","average cortical thickness")                     

means$group<-as.character(means$group)

p<-ggradar(means, values.radar = c("-0.2", "0", "0.1"), grid.min = -0.2, grid.max = 0.1, grid.mid=0,
        group.line.width = 1, group.point.size = 2,  group.colours = c("#999999", "#56B4E9" ),
        legend.text.size = 10, legend.position = "bottom")

if(!file.exists("radar.png")){
ggsave(filename="radar.png", height=25, width=25, units="cm", dpi=300)
} 



if(!file.exists("Spiderplot.pptx")) {
  p_dml <- rvg::dml(ggobj = p)
  # initialize PowerPoint slide ----
  officer::read_pptx() %>%
    # add slide ----
  officer::add_slide() %>%
    # specify object and location of object ----
  officer::ph_with(p_dml, ph_location()) %>%
    # export slide -----
  base::print(target = here::here("Spiderplot.pptx"))
}


# make plot for website

p<-ggradar(means, values.radar = c("-0.2", "0", "0.1"), grid.min = -0.2, grid.max = 0.1, grid.mid=0,
           group.line.width = 2, group.point.size = 3,  group.colours = c("#999999", "#56B4E9" ),
           legend.text.size = 10, legend.position = "bottom")

# Customize the axis labels to move them further out
radar_plot <- p +
  theme(
    # Increase the margin of the text for labels
    plot.margin = unit(c(0.01, 0.01, 0.01, 0.01), "cm"),
    legend.position = "bottom",
    legend.text = element_text(size = 10)
    #axis.text.x = element_text(size = 10, angle = 90, vjust= 2, hjust = 1) # Rotate labels by 45 degrees
  )


interactive_plot <- ggplotly(radar_plot, tooltip = "group")
saveWidget(interactive_plot, "figures/spider_plot.html", selfcontained = TRUE)

# make plot for the website
library(plotly)

fig <- plot_ly(
  type = 'scatterpolar',
  mode  = 'lines+markers'
) 
fig <- fig %>%
  add_trace(
    r = mean_frame$X1,
    theta = mean_frame$X3,
    name = 'Healthy controls',
    line = list(color = '#999999', width = 6),  # Line color and thickness
    marker = list(color = '#999999', size = 12),
    text = paste("region:", mean_frame$X3, "<br>z:", mean_frame$X1),  # Custom hover text
    hoverinfo = "text"
  ) 
fig <- fig %>%
  add_trace(
    r = mean_frame$X2,
    theta = mean_frame$X3,
    name = 'Depression',
    line = list(color = '#56B4E9', width = 6),  # Line color and thickness
    marker = list(color = '#56B4E9', size = 12),
    text = paste("region:", mean_frame$X3, "<br>z:", mean_frame$X2),  # Custom hover text
    hoverinfo = "text"
  ) 
fig <- fig %>%
  layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(-0.2,0.1),
        title = list(text = "avergage z")
      ),
      angularaxis = list(
        title = list(text = "Brain Regions")  # Rename angular axis
    )
  )
)
fig
saveWidget(fig, "figures/spider_plot.html", selfcontained = TRUE)

