#-----------------------------------------------------------------------------------------------
# Coaker Lab - Plant Pathology Department UC Davis
# Author: Danielle M. Stevens
# Last Updated: 07/13/2022
# Script Purpose: Plotting ROS data from divergent cirtus responding to different PAMPS
# Inputs Necessary: need to fix
# Outputs: 
#-----------------------------------------------------------------------------------------------


######################################################################
#row name settings
######################################################################

#collect row nameinformation annotation - botanical name ONLY
#row.names(melted_filtered_avg_PAMP_responses) <- filtered_avg_PAMP_response$`Botanical name`


#collect row nameinformation annotation - common name + botanical name
#row_names_to_apply <- filtered_avg_PAMP_response$`Common name`
#for (i in 1:length(row_names_to_apply)){
#  if(row_names_to_apply[i] == "0"){
#    row_names_to_apply[i] <- filtered_avg_PAMP_response$`Botanical name`[i]
#  }
#}
#row.names(melted_filtered_avg_PAMP_responses) <- row_names_to_apply



######################################################################
#subset CLas data 
######################################################################


disease_index <- subset(disease_index, disease_index$`Disease category` != 'N/A')
disease_index <- disease_index[order(disease_index$`Disease category`, decreasing = F),]
disease_index$`Disease category` <- as.numeric(disease_index$`Disease category`)


disease_subset_RLUs <- melted_filtered_avg_PAMP_responses[rownames(melted_filtered_avg_PAMP_responses) %in% disease_index$`Common name`,]
disease_subset_RLUs <- disease_subset_RLUs[disease_index$`Common name`,]
disease_subset_RLUs <- as.data.frame(disease_subset_RLUs, stringsAsFactors = F)
disease_subset_RLUs <- cbind(disease_subset_RLUs, as.character(rownames(disease_subset_RLUs)))

#rownames(disease_subset_RLUs) <- NULL
disease_subset_RLUs <- cbind(disease_subset_RLUs, disease_index$`Disease category`, disease_index$`Sub-tribe`)
colnames(disease_subset_RLUs) <- c("Chitin","Flg22","Csp22","CLas csp22","Common Name","Disease_category","Sub-tribe`")
#disease_subset_RLUs <- disease_subset_RLUs[,c(5,1,2,3,4,6,7)]
#disease_subset_RLUs <- reshape2::melt(disease_subset_RLUs, id = c("Common Name","Sub-tribe`","Disease_category"))




function(matrix_in, df_in){
  
  
  #determine coloring - row annotation
  
  colors_subfamily <- c("Toddalioideae" = "#FBBE4E", "Rutoideae" = "#E3DECA", "Aurantioideae" = "#273253")
  
  colors_tribe <- c("Balsamocitrinae" = "#1F6768", "Citrinae" = "#A8C653", "Clauseninae" = "#E45D50",
                    "Merrilliinae" = "#544275", "Micromelinae" = "#CAA2DD", "Triphasiinae" = "#4EAEDF",
                    "Toddalioideae" = "#FBBE4E", "Rutoideae" = "#E3DECA", "Aurantioideae" = "#273253")
  
  
  colors_subfamily_filtered <- colors_subfamily[names(colors_subfamily) %in% as.character(unique(df_in$`Sub-family`))]
  colors_tribe_filtered <- colors_tribe[names(colors_tribe) %in% as.character(unique(df_in$Tribe))]
  
  Taxon_df <- df_in[,2:3]
  Taxon_df$`Sub-family` <- as.character(Taxon_df$`Sub-family`)
  Taxon_df$Tribe <- as.character(Taxon_df$Tribe)
  
  row_anno <- rowAnnotation(df = Taxon_df,
                            border = TRUE,
                            show_legend =c(FALSE,FALSE),
                            col = list(`Sub-family` = colors_subfamily_filtered,
                                       Tribe = colors_tribe_filtered))
  
  #determine height of image
  number_of_samples <- nrow(matrix_in)
  height_of_heatmap <- number_of_samples*0.2
  if (number_of_samples > 20){
    height_of_heatmap <- number_of_samples*0.14
  }
  
  #determine width of image
  max_char_length <<- (max(nchar(row.names(matrix_in)))*0.25)
  
  # top annotation based on matrix info of RLUs in
  ha = HeatmapAnnotation('Avg. Max RLUs' = anno_boxplot(matrix_in), height = unit(3, "cm"))
  
  #right annotation 
  ra = rowAnnotation('Disease Index' = as.numeric(df_in$Disease_category))
  
  
  ht = ComplexHeatmap::Heatmap(matrix_in,
                               #cluster color modificaiton
                               col = my_color_scale_breaks,
                               show_heatmap_legend = c(col = FALSE),
                               
                               #column modifications
                               cluster_columns = F,
                               cluster_rows = F,
                               #row_km = 4,
                               
                               #row modifications + dendrogram
                               show_row_names = T,
                               row_names_gp = gpar(fontsize = 10),
                               
                               #annotate Taxnonomy Info
                               left_annotation = row_anno,
                               top_annotation = ha,
                               right_annotation = ra,
                               
                               #sizing and border
                               border = T,
                               width = unit(1.5, "in"),
                               height = unit(height_of_heatmap, "in"),
                               use_raster = TRUE, raster_quality = 2)
  
  draw(ht, heatmap_legend_side = "left")
  
}






############################################################
# Plot figures (old ignore)
# Define function to plot data is there a linear corelation between ROS production and disease catagory
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
  



correlation_curves(disease_subset_RLUs) + facet_wrap(Botanical_Name ~ .)


