#-----------------------------------------------------------------------------------------------
# Coaker Lab - Plant Pathology Department UC Davis
# Author: Danielle M. Stevens
# Last Updated: 4/20/2020
# Script Purpose: Hold information of colors for all plots/figures 
# Inputs Necessary: n/a
# Outputs: n/a
#-----------------------------------------------------------------------------------------------

##############################################
# Colors for Script "Plot_PAMP_responses"
##############################################

color_code_samples <- c('Yes','No','Variable','N/A')
color_code_samples_col <- RColorBrewer::brewer.pal(length(color_code_samples),"Set2")
names(color_code_samples_col) <- color_code_samples


# do not use these in complex heatmap package!!! the function will break
Subfamily_list <- c("Toddalioideae","Aurantioideae")
Subfamily_colors <- c("#FBBE4E","#273253")
names(Subfamily_colors) <- Subfamily_list


Tribe_list <- c("Balsamocitrinae","Citrinae","Clauseninae","Merrilliinae",
                "Micromelinae","Triphasiinae", "Zanthoxyloideae")
Tribe_colors <- c("#1F6768","#A8C653","#E45D50","#544275","#CAA2DD",
                  "#4EAEDF","#E3DECA")
names(Tribe_colors) <- Tribe_list

row_anno <- rowAnnotation(df = citrus_realtionship_info,
                          #border = TRUE,
                          gap = unit(1, "mm"),
                          show_legend = FALSE,
                          col = list(`Sub-family` =  c("Toddalioideae" = "#FBBE4E", "Aurantioideae" = "#273253"), 
                                     
                                     Tribe = c("Balsamocitrinae" = "#1F6768", "Citrinae" = "#A8C653", "Clauseninae" = "#E45D50",
                                               "Cusparieae" = "#FF7E30",
                                               "Merrilliinae" = "#544275", "Micromelinae" = "#CAA2DD", "Triphasiinae" = "#4EAEDF",
                                               "Toddalioideae" = "#FBBE4E", "Rutoideae" = "#E3DECA", "Aurantioideae" = "#273253",
                                               "Zanthoxyloideae" = "#E3DECA")))
