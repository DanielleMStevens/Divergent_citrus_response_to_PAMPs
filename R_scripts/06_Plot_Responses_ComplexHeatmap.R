#-----------------------------------------------------------------------------------------------
# Coaker Lab - Plant Pathology Department UC Davis
# Author: Danielle M. Stevens
# Last Updated: 07/13/2022
# Script Purpose: Plotting ROS data from divergent cirtus responding to different PAMPS
# Inputs Necessary: N/A
# Outputs: Heatmaps of cleaned up accumulated ROS data
#-----------------------------------------------------------------------------------------------



#---------- To make Figure 1--------------------------------------------------------------------

######################################################################
# row name settings
######################################################################

#collect row name information annotation - first common name + then botanical name (if no common name)
row_names_to_apply <- filtered_avg_PAMP_response$`Common name`
for (i in 1:length(row_names_to_apply)){
  if(is.na(row_names_to_apply[i]) == TRUE){
    row_names_to_apply[i] <- filtered_avg_PAMP_response$`Botanical name`[i]
  }
}
row.names(melted_filtered_avg_PAMP_responses) <- row_names_to_apply
row.names(melted_alternate_maping_data) <- row_names_to_apply



######################################################################
# settings for inital response - color code
######################################################################


#create a customs scale to take in differential responses
my_scale_breaks <-  c(200, 500, 1000, 5000, 20000, 50000, 100000)
colors <- RColorBrewer::brewer.pal(length(my_scale_breaks), "Reds")
my_scale_breaks <-  c(0, 200, 500, 1000, 5000, 20000, 50000, 100000)
colors <- c("#FFFFFF",colors)
my_color_scale_breaks <- circlize::colorRamp2(my_scale_breaks, colors)



##pull out row names to annotate with
citrus_realtionship_info <- as.data.frame(filtered_avg_PAMP_response[,2:3], stringsAsFactors = FALSE)
citrus_realtionship_info$Tribe <- as.character(citrus_realtionship_info$Tribe)
citrus_realtionship_info$`Sub-tribe` <- as.character(citrus_realtionship_info$`Sub-tribe`)
row_anno <- rowAnnotation(df = citrus_realtionship_info,
                          gap = unit(1, "mm"),
                          show_legend = FALSE,
                          col = list(Tribe = c("Zanthoxyloideae" = "#E3DECA", "Citreae" = "#273253", "Clauseneae" = "#F7D57C"), 
                                     
                                     `Sub-tribe` = c("Balsamocitrinae" = "#1F6768", "Citrinae" = "#A8C653", "Clauseninae" = "#E45D50",
                                               "Merrilliinae" = "#544275", "Micromelinae" = "#CAA2DD", "Triphasiinae" = "#4EAEDF",
                                               "Citreae" = "#273253", "Clauseneae" = "#F7D57C", "Zanthoxyloideae" = "#E3DECA")))



######################################################################
# plot heatmap max rlu  - plot by taxonomy - Figure 1
######################################################################


##if this error pops up, try running the code below, restarting R, and rerunning all code.
#Error in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  : 
#invalid font type
#coomon error: https://github.com/stan-dev/rstan/issues/381

#code to run:
#extrafont::font_import()
#font_import(pattern="[A/a]rial", prompt=FALSE)


ht = ComplexHeatmap::Heatmap(melted_filtered_avg_PAMP_responses[,1:3],
                               
                               #cluster color modificaiton
                               col = my_color_scale_breaks,
                               
                               #row modifications + dendrogram
                               show_row_names = T,
                               row_names_gp = gpar(fontsize = 8),
                               
                               
                               row_split = factor(filtered_avg_PAMP_response$`Sub-tribe`,
                                                  levels = c("Zanthoxyloideae",
                                                             "Balsamocitrinae", "Citrinae",
                                                             "Clauseninae", "Merrilliinae",
                                                             "Micromelinae", "Triphasiinae")), #manual row split
                               cluster_row_slices = F,
                               column_dend_reorder = F,
                               row_dend_reorder = T,
                               show_parent_dend_line = FALSE,

                               # modifications to titles and dendrogram
                               row_title = NULL,
                               row_gap = unit(0, "mm"),
                               row_dend_width = unit(0, "mm"),
                               column_dend_height = unit(0, "mm"),
                             
                               
                               #sizing and border
                               width = unit(1.5, "in"),
                               height = unit(9, "in"),
                               
                               #annotate Taxnonomy Info
                               left_annotation = row_anno,
                              
                               
                               #details regarding modifying legend
                               heatmap_legend_param = list(col_fun = my_color_scale_breaks, #main MAX RLU annotation
                                                           legend_width = unit(30, "mm"),
                                                           legend_height = unit(48, "mm"),
                                                           title = "Max RLUs", 
                                                           border = "black",
                                                           at = my_scale_breaks,
                                                           title_gp = gpar(fontsize = 12, fontface = "bold", fontfamily = "Arial"),
                                                           labels_gp = gpar(fontsize = 10, fontfamily = "Arial"),
                                                           grid_width = unit(0.7, "cm"),
                                                           legend_label_gp = gpar(col = "black", fontsize = 10, fontfamily = "Arial")
                                                           ),
                               use_raster = TRUE, raster_quality = 4, raster_device = 'png')

  
lgd_list = list(
    # Sub-family annotation
    Legend(labels = c("Zanthoxyloideae", "Citreae", "Clauseneae"), title = "Tribe", 
           border = "black",
           gap = unit(2, "mm"),
           legend_gp = gpar(fill = c("#E3DECA", "#273253", "#F7D57C"))),
    
    # Tribe annotation
    Legend(labels = c("Zanthoxyloideae","Balsamocitrinae", "Citrinae", "Clauseninae",
                      "Merrilliinae", "Micromelinae", "Triphasiinae"), title = "Sub-tribe",
           border = "black",
           gap = unit(2, "mm"),
           legend_gp = gpar(fill = c("#E3DECA", "#1F6768", "#A8C653", "#E45D50", 
                                     "#544275", "#CAA2DD", "#4EAEDF")))
  )


draw(ht, adjust_annotation_extension = TRUE, heatmap_legend_side = "left", annotation_legend_side = "left", 
     annotation_legend_list = lgd_list)
  



#---------- To make final figure:-----------------------------------------------------------------------------------------
# use export button to export -> don't use pdf then dev off as issues with exporting image with font arise
# path to save image -> /Final_Figures/Heatmap_plot_all_values_organize_by_taxonomy.pdf
# save the following image size:width = 8, height = 10
# import pdf into vector based editing software such as inkscpae or adobe illustrator, remove edges/scaffold of higherachial 
# tree cluster, add basic black boarders on overall heatmap and annotations, and edit botanical names to carry only 
# genus + species info (the remianing can be look up in table) and italizie them. Finally, for those that were marked
# variable in ROS (sheet 2 of Summary_of_PAMP_response.xlsx), add in astricks.





#---------- To make Figure 7/Supplemental Figure 3---------------------------------------------------



######################################################################
# row name settings
######################################################################



#collect row name information annotation - common name + botanical name
#filtered_avg_PAMP_response <- filtered_avg_PAMP_response[rownames(alternate_maping_data),]
#row_names_to_apply <- filtered_avg_PAMP_response$`Common name`
#for (i in 1:length(row_names_to_apply)){
#  if(is.na(row_names_to_apply[i]) == TRUE){
#    row_names_to_apply[i] <- filtered_avg_PAMP_response$`Botanical name`[i]
#  }
#}
#row.names(csp22_filtered_data) <- row_names_to_apply



######################################################################
# subset both data sets for just csp22 consensus and clas variant response
######################################################################

# subset data frame where either csp22 or Clas csp22 elicites a response
csp22_filtered_data <- melted_filtered_avg_PAMP_responses[,c(3,4)]
#csp22_filtered_data <- csp22_filtered_data[rowSums(csp22_filtered_data) != 0, ] <--- can use to make F7, or remove extra rows manually
csp22_filtered_data <- as.data.frame(csp22_filtered_data)


# filter out alternate mappind data for just csp22s
alternate_maping_data <- melted_alternate_maping_data[rownames(melted_alternate_maping_data) %in% rownames(csp22_filtered_data),c(3,4)]
alternate_maping_data <- as.data.frame(alternate_maping_data)
alternate_maping_data <- alternate_maping_data %>% arrange(factor(`CLas csp22`, levels = c("Yes","Variable","No")))
csp22_filtered_data <- csp22_filtered_data[rownames(alternate_maping_data),]


##pull out row names to annotate with
citrus_realtionship_info <- as.data.frame(filtered_avg_PAMP_response[match(rownames(melted_filtered_avg_PAMP_responses), rownames(csp22_filtered_data)),] , stringsAsFactors = FALSE)
csp22_fix_filtered <- citrus_realtionship_info[,7:8]
csp22_fix_filtered <- csp22_fix_filtered[match(rownames(alternate_maping_data), rownames(csp22_fix_filtered)),]
citrus_realtionship_info <- citrus_realtionship_info[match(rownames(alternate_maping_data), rownames(citrus_realtionship_info)),]

for (i in 1:nrow(citrus_realtionship_info)){
  if (is.na(citrus_realtionship_info$`Common name`[i]) == TRUE){
    citrus_realtionship_info$`Common name`[i] <- citrus_realtionship_info$`Botanical name (CRC numbers in parenthesis)`[i]
  }
}




rownames(csp22_fix_filtered) <- citrus_realtionship_info$`Common name`


citrus_realtionship_info <- citrus_realtionship_info[,c(2,3)]
colnames(citrus_realtionship_info) <- c("Tribe","Sub-tribe")
citrus_realtionship_info$Tribe <- as.character(citrus_realtionship_info$Tribe)
citrus_realtionship_info$`Sub-tribe` <- as.character(citrus_realtionship_info$`Sub-tribe`)

row_anno <- rowAnnotation(df = citrus_realtionship_info,
                          gap = unit(1, "mm"),
                          show_legend = FALSE,
                          col = list(Tribe = c("Zanthoxyloideae" = "#E3DECA", "Citreae" = "#273253", "Clauseneae" = "#F7D57C"), 
                                     
                                     `Sub-tribe` = c("Balsamocitrinae" = "#1F6768", "Citrinae" = "#A8C653", "Clauseninae" = "#E45D50",
                                                     "Merrilliinae" = "#544275", "Micromelinae" = "#CAA2DD", "Triphasiinae" = "#4EAEDF",
                                                     "Citreae" = "#273253", "Clauseneae" = "#F7D57C", "Zanthoxyloideae" = "#E3DECA")))




######################################################################
# plot heatmap max rlu of just csp22 consensus and variants - plot by taxonomy - Figure 7
######################################################################


ht2 = ComplexHeatmap::Heatmap(csp22_fix_filtered,
                             
                             #cluster color modificaiton
                             col = my_color_scale_breaks,
                             
                             #row modifications + dendrogram
                             show_row_names = T,
                             row_names_gp = gpar(fontsize = 7.9),
                             
                             
                             row_split = factor(alternate_maping_data$`CLas csp22`,
                                                levels = c("Yes","Variable","No")), #manual row split
                             cluster_row_slices = F, 
                             column_dend_reorder = F,
                             row_dend_reorder = T,
                             show_parent_dend_line = FALSE,
                             
                             # modifications to titles and dendrogram
                             row_title = NULL,
                             row_gap = unit(1, "mm"),
                             row_dend_width = unit(0, "mm"),
                             column_dend_height = unit(0, "mm"),
                             
                             
                             #sizing and border
                             width = unit(1.1, "in"),
                             height = unit(9, "in"),
                             
                             #annotate Taxnonomy Info
                             left_annotation = row_anno,
                             
                             
                             #details regarding modifying legend
                             heatmap_legend_param = list(col_fun = my_color_scale_breaks, #main MAX RLU annotation
                                                         legend_width = unit(30, "mm"),
                                                         legend_height = unit(48, "mm"),
                                                         title = "Max RLUs", 
                                                         border = "black",
                                                         at = my_scale_breaks,
                                                         title_gp = gpar(fontsize = 12, fontface = "bold", fontfamily = "Arial"),
                                                         labels_gp = gpar(fontsize = 10, fontfamily = "Arial"),
                                                         grid_width = unit(0.7, "cm"),
                                                         legend_label_gp = gpar(col = "black", fontsize = 10, fontfamily = "Arial")
                             ),
                             use_raster = TRUE, raster_quality = 4, raster_device = 'png')





draw(ht2, adjust_annotation_extension = TRUE, heatmap_legend_side = "left", annotation_legend_side = "left", 
     annotation_legend_list = lgd_list)



#---------- To make final figure:-----------------------------------------------------------------------------------------
# use export button to export -> don't use pdf then dev off as issues with exporting image with font arise
# path to save image -> /Final_Figures/Heatmap_plot_only_csps_organize_by_CLas_csp22_response.pdf
# save the following image size:width = 6, height = 8
# import pdf into vector based editing software such as inkscpae or adobe illustrator, remove edges/scaffold of higherachial 
# tree cluster, add basic black boarders on overall heatmap and annotations, and edit botanical names to carry only 
# genus + species info (the remianing can be look up in table) and italizie them. For clariety, manually determined
# clustering within clas csp22 by tribe and manually reorder those samples based on that. Finally, for those that were marked
# variable in ROS (sheet 2 of Summary_of_PAMP_response.xlsx), add in astricks on Clas csp22.



