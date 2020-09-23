#-----------------------------------------------------------------------------------------------
# Coaker Lab - Plant Pathology Department UC Davis
# Author: Danielle M. Stevens
# Last Updated: 4/15/2020
# Script Purpose: Plotting ROS data from divergent cirtus responding to different PAMPS
# Inputs Necessary: 
# Outputs: 
#-----------------------------------------------------------------------------------------------
  

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
#function to adjust size 
######################################################################
# adjust size in plot plane 
plot_adjust_size <- function(desired_dpi){
  orginal_width <- dev.size('px')[1]
  orginal_height <- dev.size('px')[2]
  
  adjusted_width <- (orginal_width*desired_dpi)/72
  adjusted_height <- (orginal_height*desired_dpi)/72
  return(list(adjusted_width, adjusted_height))
}

######################################################################
#subset CLas data 
######################################################################


disease_index <- subset(disease_index, disease_index$Disease_category != 'N/A')
disease_index <- disease_index[order(disease_index$Disease_category, decreasing = F),]
disease_index$Disease_category <- as.numeric(disease_index$Disease_category)


disease_subset_RLUs <- melted_filtered_avg_PAMP_responses[row.names(melted_filtered_avg_PAMP_responses) %in% disease_index$`Botanical name`,]
disease_subset_RLUs <- disease_subset_RLUs[disease_index$`Botanical name`,]
disease_subset_RLUs <- as.data.frame(disease_subset_RLUs, stringsAsFactors = F)
disease_subset_RLUs <- cbind(disease_subset_RLUs, as.character(rownames(disease_subset_RLUs)))

rownames(disease_subset_RLUs) <- NULL
disease_subset_RLUs <- cbind(disease_subset_RLUs, disease_index$Disease_category, disease_index$Tribe)
colnames(disease_subset_RLUs) <- c("Chitin","Flg22","Csp22","CLas csp22","Botanical_Name","Disease_category","Tribe")
disease_subset_RLUs <- disease_subset_RLUs[,c(5,1,2,3,4,6,7)]


disease_subset_RLUs <- reshape2::melt(disease_subset_RLUs, id = c("Botanical_Name","Tribe","Disease_category"))
# remove this code later but for now, remove missing data points
disease_subset_RLUs <- na.omit(disease_subset_RLUs)


############################################################
#Define function to plot data is there a linear corelation between ROS production and disease catagory
############################################################

correlation_curves <- function(df_in){
  colors_tribe <- c("Balsamocitrinae" = "#1F6768", "Citrinae" = "#A8C653", "Clauseninae" = "#E45D50",
                   "Merrilliinae" = "#544275", "Micromelinae" = "#CAA2DD", "Triphasiinae" = "#4EAEDF",
                  "Toddalioideae" = "#FBBE4E", "Rutoideae" = "#E3DECA", "Aurantioideae" = "#273253")
  colors_tribe_filtered <- colors_tribe[names(colors_tribe) %in% as.character(unique(df_in$Tribe))]

  ggplot(df_in, aes(x = Disease_category, y = value)) +
    geom_smooth(method = 'lm', se = T, color = 'black', alpha = 0.25) +
    geom_point(aes(color = Tribe)) +
    ylab("ROS Burst (Max RLUs)\n") +
    xlab("\n Disease Catagory") +
    my_ggplot_theme +
    scale_color_manual(values = colors_tribe_filtered) +
    theme(axis.text = element_text(size = 11)) +
    stat_cor(method="spearman")
    
}


############################################################
#Plot figures
############################################################

tiff("Correlation_curves_all_MAMPs.tiff", height = 2816, width = 5116, units='px', compression = "lzw", res = 600)
correlation_curves(disease_subset_RLUs) + ggtitle("All MAMPs")
dev.off()



tiff("Correlation_curves_by_MAMP.tiff", height = 2816, width = 8383, units='px', compression = "lzw", res = 600)

ggpubr::ggarrange(correlation_curves(subset(disease_subset_RLUs, variable == "Chitin")) + ggtitle("Chitin"),
                  correlation_curves(subset(disease_subset_RLUs, variable == "Flg22")) + ylab("") + ggtitle("Flg22"), 
                  correlation_curves(subset(disease_subset_RLUs, variable == "Csp22")) + ylab("") + ggtitle("Csp22"), 
                  correlation_curves(subset(disease_subset_RLUs, variable == "CLas csp22")) + ylab("") + ggtitle("CLas Csp22"),
                  ncol = 4, nrow = 1, common.legend = TRUE, legend = "bottom") + 
  theme(legend.text = element_text(size = 11), legend.direction = "vertical", legend.box = "vertical")

dev.off()
  

