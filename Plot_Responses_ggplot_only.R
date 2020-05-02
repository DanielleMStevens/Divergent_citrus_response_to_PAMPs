#-----------------------------------------------------------------------------------------------
# Coaker Lab - Plant Pathology Department UC Davis
# Author: Danielle M. Stevens
# Last Updated: 4/15/2020
# Script Purpose: Plotting ROS data from divergent cirtus responding to different PAMPS
# Inputs Necessary: 
# Outputs: 
#-----------------------------------------------------------------------------------------------


######################################################################
#library packages need to load
######################################################################

library(RColorBrewer)
library(devtools)
library(treemap)
library(data.tree)
library(stringr)
library(ggplot2)
library(patchwork)
library(reshape2)
library(rstudioapi)



##############################################
# Load Processed Data and Colors
##############################################

#make sure to set path to the same place where the figure 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load processed data
source("./Process_PAMP_responses.R")

# load figure colors
source("./Figure_colors.R")

# Load custom ggplot
source("./Theme_ggplot.R")

######################################################################
#row name settings
######################################################################

#collect row nameinformation annotation - botanical name ONLY
row.names(melted_filtered_avg_PAMP_responses) <- filtered_avg_PAMP_response$`Botanical name`


#collect row nameinformation annotation - common name + botanical name
#row_names_to_apply <- filtered_avg_PAMP_response$`Common name`
#for (i in 1:length(row_names_to_apply)){
#  if(row_names_to_apply[i] == "0"){
#    row_names_to_apply[i] <- filtered_avg_PAMP_response$`Botanical name`[i]
#  }
#}
#row.names(melted_filtered_avg_PAMP_responses) <- row_names_to_apply


######################################################################
#plot indivisdial values - boxplot? for each tribe data set
######################################################################

Toddalioideae_melt <- reshape2::melt(Toddalioideae)
Toddalioideae_box <- ggplot(Toddalioideae_melt, aes(x = variable, y = value, fill = `Sub-family`)) +
  geom_boxplot() +
  geom_jitter(size = 1.5, alpha = 0.8, width= 0.2, colour = "black") +
  my_ggplot_theme +
  ylab("Max RLUs") + 
  xlab("") +
  scale_y_log10() +
  theme(legend.position = "none")+
  scale_fill_manual("Sub-Family", values = Subfamily_colors)+
  scale_colour_manual("Sub-Family", values = Subfamily_colors) +
  labs(subtitle = c(as.character(unique(Toddalioideae_melt$Tribe))))



box_plot_tribe_plot <- function(df_in){
  ggplot(df_in, aes(x = variable, y = value, fill = Tribe)) +
    geom_boxplot(alpha = 0.5) +
    geom_jitter(size = 1.2, alpha = 0.8, width= 0.2, colour = "black") +
    my_ggplot_theme +
    ylab("Max RLUs") +
    xlab("") +
    scale_y_log10() +
    theme(legend.position = "none")+
    scale_fill_manual("Tribe", values = Tribe_colors) +
    scale_colour_manual("Tribe", values = Tribe_colors) +
    labs(subtitle = c(as.character(unique(df_in$Tribe))))
}

Balsamocitrinae_melt <- reshape2::melt(Balsamocitrinae)
Balsamocitrinae_box <- box_plot_tribe_plot(Balsamocitrinae_melt)



Citrinae_melt <- reshape2::melt(Citrinae)
Citrinae_box <- box_plot_tribe_plot(Citrinae_melt)


Clauseninae_melt <- reshape2::melt(Clauseninae)
Clauseninae_box <- box_plot_tribe_plot(Clauseninae_melt)


Triphasiinae_melt <- reshape2::melt(Triphasiinae)
Triphasiinae_box <-box_plot_tribe_plot(Triphasiinae_melt)


png("Comparison_of_Max_RLUs_across_Tribes.png", height = 8, width = 3.5, units = "in", res = 800)
Toddalioideae_box/Balsamocitrinae_box/Citrinae_box/Clauseninae_box/Triphasiinae_box
dev.off()


######################################################################
#plot degress of variation
######################################################################


degree_of_variation_bargraph <- function(data_frame_in){
  reset_data <- reshape2::melt(data_frame_in, id=c("Botanical name",'Sub-family','Tribe','Common name'))
  reset_data$value <- as.factor(reset_data$value)
  ggplot(reset_data, aes(x= variable, fill = value))+
    geom_bar(position = "fill")+
    xlab("")+
    theme_bw() +
    scale_fill_manual("Response", values= color_code_samples_col) +
    labs(subtitle = c(as.character(unique(reset_data$Tribe))))
}

png("Degress_of_variation_Comparison_of_Max_RLUs_across_Tribes.png", height = 8, width = 3.5, units = "in", res = 800)

(degree_of_variation_bargraph(Toddalioideae_alt)/
    degree_of_variation_bargraph(Rutoideae_alt)/
    
    degree_of_variation_bargraph(Balsamocitrinae_alt)/
    degree_of_variation_bargraph(Citrinae_alt)/
    degree_of_variation_bargraph(Clauseninae_alt)/
    degree_of_variation_bargraph(Merrilliinae_alt)/
    degree_of_variation_bargraph(Micromelinae_alt)/
    degree_of_variation_bargraph(Triphasiinae_alt))

dev.off()

######################################################################
#plot degress of variation - as facets
######################################################################

reset_data <- reshape2::melt(alternate_maping_data, id=c("Botanical name",'Sub-family','Tribe','Common name'))
reset_data$value <- as.factor(reset_data$value)

png("Degress_of_variation_Comparison_of_Max_RLUs_across_Tribes_as_facets.png", height = 9, width = 3.5, units = "in", res = 800)

ggplot(reset_data, aes(x= variable, fill = value))+
  geom_bar(position = "fill")+
  facet_grid(Tribe ~ .)+
  xlab("")+
  ylab("Proportion of Response\n") +
  scale_y_continuous(breaks = c(0.5,1))+
  my_ggplot_theme +
  theme(axis.text.x = element_text(angle = 90))+
  scale_fill_manual("Response", values= color_code_samples_col) 

dev.off()
