#-----------------------------------------------------------------------------------------------
# Coaker Lab - Plant Pathology Department UC Davis
# Author: Danielle M. Stevens
# Last Updated: 4/15/2020
# Script Purpose: Master Script Controlling all Subscripts
# Inputs Necessary: n/a
# Outputs: n/a
#-----------------------------------------------------------------------------------------------



##############################################
# Load Processed Data and Colors
##############################################

#make sure to set path to the same place where the figure 
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#getSrcDirectory(function(x) {x})


#### Loading libraries, color codes, and processing data
# Load libraries to run scripts
source("./Libraries_to_load.R")

# load processed data
source("./Process_PAMP_responses.R")

# load figure colors
source("./Figure_colors.R")

# Load custom ggplot
source("./Theme_ggplot.R")


#### Run to create all the figures use in these scripts
# run complex heatmap script figures
source("./Plot_Responses_ComplexHeatmap.R")

# run inital ggplot script figure
source("./Plot_Responses_ggplot_only.R")

# run ggplot script for assessing responses in relationship with disease


####################################################
# NOTE: If you have issues loading packages, I reccomend restarting R. This fixed the issues most times in my experience
#####################################################