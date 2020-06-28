#-----------------------------------------------------------------------------------------------
# Coaker Lab - Plant Pathology Department UC Davis
# Author: Danielle M. Stevens
# Last Updated: 5/7/2020
# Script Purpose: Loading Libraries for Processing data
# Inputs Necessary: 
# Outputs: 
#-----------------------------------------------------------------------------------------------


######################################################################
#library packages need to load
######################################################################

library(RColorBrewer)
library(devtools)
library(ComplexHeatmap)
library(readxl)
library(circlize)
library(treemap)
library(data.tree)
library(stringr)
library(ggplot2)
library(patchwork)
library(reshape2)
library(rstudioapi)
library(dplyr)

if(!require(pbkrtest)) install_version("pbkrtest", version = "0.4-7", repos = "http://cran.us.r-project.org")


# Install
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
library(ggpubr)


# NOTE: I have had issues sometimes loading the complex heatmap package (not sure why), 
# Try one of the many ways to download the package and if still running into troubles,
# consult google or contact me

#install_github("jokergoo/ComplexHeatmap")
#devtools::install_github("jokergoo/ComplexHeatmap")

# second method, not recommended although an option
# make sure you have a new enough version of this package otherwise it will not run properly
#if (!requireNamespace("BiocManager", quietly=TRUE))
#install.packages("BiocManager")
#BiocManager::install("ComplexHeatmap")



