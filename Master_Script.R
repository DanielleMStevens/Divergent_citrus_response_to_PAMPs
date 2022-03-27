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

# set path to location of where repository is located/downloaded
library(rstudioapi)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


#### Loading libraries, color codes, and processing data
# Load libraries to run scripts
source("./R_scripts/01_Libraries_to_load.R")

# load processed data
source("./R_scripts/02_Process_PAMP_responses.R")

# load figure colors
source("./R_scripts/03_Figure_colors.R")

# Load custom ggplot
source("./R_scripts/04_Theme_ggplot.R")


##############################################
# Plot data post processing
##############################################


#### Run to create all the figures use in these scripts
# run complex heatmap script figures
source("./R_scripts/05_Plot_Responses_ComplexHeatmap.R")


# run ggplot script for assessing responses in relationship with disease
source("./R_scripts/06_Assess_ROS_to_disease_assay.R")

# run inital ggplot script figure
#source("./R_scripts/Plot_Responses_ggplot_only.R")


####################################################
# NOTE: If you have issues loading packages, I reccomend restarting R. This fixed the issues most times in my experience
#####################################################

