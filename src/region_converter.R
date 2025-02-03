library(dplyr)
region_converter <- function(data, ROI) {
  data <- data %>%
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
}
