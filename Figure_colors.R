#-----------------------------------------------------------------------------------------------
# Coaker Lab - Plant Pathology Department UC Davis
# Author: Danielle M. Stevens
# Last Updated: 4/20/2020
# Script Purpose: Hold information of colors for all plots/figures 
# Inputs Necessary: n/a
# Outputs: n/a
#-----------------------------------------------------------------------------------------------

######################################################################
#library packages need to load
######################################################################

library(RColorBrewer)

##############################################
# Colors for Script "Plot_PAMP_responses"
##############################################

color_code_samples <- c('Yes','No','Variable','N/A')
color_code_samples_col <- RColorBrewer::brewer.pal(length(color_code_samples),"Set2")
names(color_code_samples_col) <- color_code_samples


# do not use these in complex heatmap package!!! the function will break
Subfamily_list <- c("Toddalioideae" ="#273253", "Rutoideae" = "#ED5C4D", "Aurantioideae" = "#FBBE4E")

Tribe_colors <- c("Balsamocitrinae" = "#1F6768", "Citrinae" = "#7DC2BF", "Clauseninae" = "#EE2737",
                     "Merrilliinae" = "#862983", "Micromelinae" = "#A8C653", "Triphasiinae" = "#5BC2E7")


