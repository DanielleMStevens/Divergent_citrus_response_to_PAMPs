#-----------------------------------------------------------------------------------------------
# Coaker Lab - Plant Pathology Department UC Davis
# Author: Danielle M. Stevens
# Last Updated: 07/13/2022
# Script Purpose: Loading Libraries for Processing data + general functions used
# Inputs Necessary: N/A
# Outputs: N/A
#-----------------------------------------------------------------------------------------------


######################################################################
#library packages need to load
######################################################################

# Packages for importing data and cleaning ut up
library(readxl)
library(stringr)
library(reshape2)
library(dplyr)
library(xlsx)

# General packages for plotting and athetics
library(ggplot2)
library(RColorBrewer)
library(extrafont) 
library(ggrepel)
library(scales)
library(forcats)
library(ggbeeswarm)

# R packages for plotting heatmaps
library(ComplexHeatmap)
library(circlize)
#library(magick) broken?

# R packages for plotting and managing tree data
library(treeio)
library(phangorn)
library(ggtree)
library(cluster)


#########################################################
# funciton - convert AAstringset attribute to dataframe
#########################################################

# turning AAmultiplesequence alignment into dataframe
aa2df <- function(dss){
  return(data.frame(names = rownames(dss), seq = as.character(dss), stringsAsFactors = FALSE))
}

# turning AAsequences (fasta) into dataframe
dss2df <- function(dss){
  return(data.frame(width = BiocGenerics::width(dss), names = names(dss), seq = as.character(dss), stringsAsFactors = FALSE))
}

######################################################################
#function to adjust size - adjust size in plot plane 
######################################################################

plot_adjust_size <- function(desired_dpi){
  orginal_width <- dev.size('px')[1]
  orginal_height <- dev.size('px')[2]
  
  adjusted_width <- (orginal_width*desired_dpi)/72
  adjusted_height <- (orginal_height*desired_dpi)/72
  return(list(adjusted_width, adjusted_height))
}

# NOTE: I have had issues sometimes loading the complex heatmap package (not sure why), 
# Try one of the many ways to download the package and if still running into troubles,
# consult google or contact me

# Method One - try installing github version via Devtools
#--------------------------------------------------------

#library(devtools)
#install_github("jokergoo/ComplexHeatmap")
#devtools::install_github("jokergoo/ComplexHeatmap")

# Method Two - not recommended although an option, load via BioConductor
# make sure you have a new enough version of this package otherwise it will not run properly
#--------------------------------------------------------------------------------------------

#if (!requireNamespace("BiocManager", quietly=TRUE))
#install.packages("BiocManager")
#BiocManager::install("ComplexHeatmap")








