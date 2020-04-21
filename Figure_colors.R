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
