#-----------------------------------------------------------------------------------------------
# Coaker Lab - Plant Pathology Department UC Davis
# Author: Danielle M. Stevens
# Last Updated: 07/13/2022
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
Subfamily_list <- c("Zanthoxyloideae","Citreae","Clauseneae")
Subfamily_colors <- c("#E3DECA","#273253","#FFBF00")
names(Subfamily_colors) <- Subfamily_list


Tribe_list <- c("Balsamocitrinae","Citrinae","Clauseninae","Merrilliinae",
                "Micromelinae","Triphasiinae", "Zanthoxyloideae")
Tribe_colors <- c("#1F6768","#A8C653","#E45D50","#544275",
                  "#CAA2DD","#4EAEDF","#E3DECA")
names(Tribe_colors) <- Tribe_list


