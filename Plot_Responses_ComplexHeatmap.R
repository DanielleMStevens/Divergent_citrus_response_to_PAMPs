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

# NOTE: I have had issues sometimes loading the complex heatmap package (not sure why), 
# Try one of the many ways to download the package and if still running into troubles,
# consult google or contact me

#install_github("jokergoo/ComplexHeatmap")
devtools::install_github("jokergoo/ComplexHeatmap")
#if (!requireNamespace("BiocManager", quietly=TRUE))
#install.packages("BiocManager")
#BiocManager::install("ComplexHeatmap")


##############################################
# Load Processed Data and Colors
##############################################

#make sure to set path to the same place where the figure 
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


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
row.names(melted_alternate_maping_data) <- filtered_avg_PAMP_response$`Botanical name`

#collect row nameinformation annotation - common name + botanical name
row_names_to_apply <- filtered_avg_PAMP_response$`Common name`
for (i in 1:length(row_names_to_apply)){
  if(row_names_to_apply[i] == "0"){
    row_names_to_apply[i] <- filtered_avg_PAMP_response$`Botanical name`[i]
  }
}
row.names(melted_filtered_avg_PAMP_responses) <- row_names_to_apply



######################################################################
#settings for inital response - color code
######################################################################


#create a customs scale to take in differential responses
my_scale_breaks <- c(0,200, 500, 1000, 5000, 20000, 50000, 100000)
colors <- RColorBrewer::brewer.pal(length(my_scale_breaks), "Reds")
my_color_scale_breaks <- circlize::colorRamp2(my_scale_breaks, colors)



##pull out row names to annotate with
citrus_realtionship_info <- as.data.frame(filtered_avg_PAMP_response[,2:3], stringsAsFactors = FALSE)
citrus_realtionship_info$`Sub-family` <- as.character(citrus_realtionship_info$`Sub-family`)
citrus_realtionship_info$Tribe <- as.character(citrus_realtionship_info$Tribe)
#citrus_realtionship_info_simplified <- citrus_realtionship_info[!duplicated(citrus_realtionship_info[,1:2]),]



######################################################################
#plot heatmap max rlu  - plot by taxonomy
######################################################################


png("Heatmap_plot_all_values_organize_by_taxonomy.png", units="in", width=8, height=10, res=800)




row_anno <- rowAnnotation(df = citrus_realtionship_info,
                          border = TRUE,
                          col = list(`Sub-family` =  c("Toddalioideae" = "#FBBE4E", "Rutoideae" = "#E3DECA", "Aurantioideae" = "#273253"), 
                                     
                                     Tribe = c("Balsamocitrinae" = "#1F6768", "Citrinae" = "#A8C653", "Clauseninae" = "#E45D50",
                                               "Merrilliinae" = "#544275", "Micromelinae" = "#CAA2DD", "Triphasiinae" = "#4EAEDF",
                                               "Toddalioideae" = "#FBBE4E", "Rutoideae" = "#E3DECA", "Aurantioideae" = "#273253")))
                          


ht = ComplexHeatmap::Heatmap(melted_filtered_avg_PAMP_responses,
                             
                             #cluster color modificaiton
                             col = my_color_scale_breaks,
                             
                             #column modifications
                             cluster_columns = F,
                             cluster_rows = F,
                             #row_km = 11,
                             #row_dend_width = unit(30, "mm"),
                             
                             #row modifications + dendrogram
                             show_row_names = T,
                             row_names_gp = gpar(fontsize = 8),
                             
                             
                             
                             #row_split = filtered_avg_PAMP_response$Tribe, #manual row split
                             #cluster_row_slices = T,
                             #row_dend_reorder = T,
                             
                             #sizing and border
                             border = T,
                             width = unit(1.5, "in"),
                             height = unit(8.5, "in"),
                             
                             #annotate Taxnonomy Info
                             left_annotation = row_anno,
                             
                             #details regarding modifying legend
                             heatmap_legend_param = list(col_fun = my_color_scale_breaks, 
                                                         legend_width = unit(35, "mm"),
                                                         legend_height = unit(38, "mm"),
                                                         title = "Max RLUs", 
                                                         border = "black",
                                                         at = my_scale_breaks,
                                                         title_gp = gpar(fontsize = 12, fontface = "bold", fontfamily = "Arial"),
                                                         labels_gp = gpar(fontsize = 10,fontfamily = "Arial"),
                                                         grid_width = unit(0.5, "cm"),
                                                         legend_label_gp = gpar(col = "black",fontsize = 10, fontfamily = "Arial")),
                             
                             use_raster = TRUE, raster_quality = 2) 


draw(ht, heatmap_legend_side = "left")

dev.off()

######################################################################
#plot heatmap max rlu  - plot by taxonomy with clustering within taxanomic group
######################################################################


sub_clustered_heatmap <- function(matrix_in, df_in){
  
  #determine height of image
  number_of_samples <- nrow(matrix_in)
  height_of_heatmap <- number_of_samples*0.16
  if (number_of_samples > 20){
    height_of_heatmap <- number_of_samples*0.14
  }
  
  #determine width of image
  max_char_length <<- (max(nchar(row.names(matrix_in)))*0.25)
  
  
  
  #determine coloring
  colnames(df_in) <- c("Botanical name","Sub-family","Tribe","Common name","Chitin","Flg22","Csp22","CLas csp22")
  
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
                           col = list(`Sub-family` = colors_subfamily_filtered,
                                    Tribe = colors_tribe_filtered))
  
  
  ComplexHeatmap::Heatmap(matrix_in,
                          #cluster color modificaiton
                          col = my_color_scale_breaks,
                          show_heatmap_legend = c(col = FALSE),
                          
                          #column modifications
                          cluster_columns = F,
                          cluster_rows = F,
                          #row_dend_width = unit(20, "mm"),
                          
                          #row modifications + dendrogram
                          show_row_names = T,
                          row_names_gp = gpar(fontsize = 10),
                          
                          #annotate Taxnonomy Info
                          left_annotation = row_anno,
                          
                          #sizing and border
                          border = T,
                          width = unit(1.5, "in"),
                          height = unit(height_of_heatmap, "in"),
                          use_raster = TRUE, raster_quality = 2)
  
}



Toddalioideae_mat <- as.matrix(Toddalioideae[,5:8])
row.names(Toddalioideae_mat) <- Toddalioideae$`Botanical name`                   
Tod <- sub_clustered_heatmap(Toddalioideae_mat, Toddalioideae)


Rutoideae_mat <- as.matrix(Rutoideae[,5:8])
row.names(Rutoideae_mat) <- Rutoideae$`Botanical name`                   
Rut <- sub_clustered_heatmap(Rutoideae_mat, Rutoideae)

Balsamocitrinae_mat  <- as.matrix(Balsamocitrinae[,5:8])
row.names(Balsamocitrinae_mat) <- Balsamocitrinae$`Botanical name`                   
Bal <- sub_clustered_heatmap(Balsamocitrinae_mat, Balsamocitrinae)


Citrinae_mat  <- as.matrix(Citrinae[,5:8])
row.names(Citrinae_mat) <- Citrinae$`Botanical name`                   
Cit <- sub_clustered_heatmap(Citrinae_mat, Citrinae)


Clauseninae_mat  <- as.matrix(Clauseninae[,5:8])
row.names(Clauseninae_mat) <- Clauseninae$`Botanical name`                   
Cla <- sub_clustered_heatmap(Clauseninae_mat, Clauseninae)

Merrilliinae_mat  <- as.matrix(Merrilliinae[,5:8])
row.names(Merrilliinae_mat) <- Merrilliinae$`Botanical name`                   
Mer <- sub_clustered_heatmap(Merrilliinae_mat, Merrilliinae)


Micromelinae_mat  <- as.matrix(Micromelinae[,5:8])
row.names(Micromelinae_mat) <- Micromelinae$`Botanical name`                   
Mic <- sub_clustered_heatmap(Micromelinae_mat, Micromelinae)


Triphasiinae_mat  <- as.matrix(Triphasiinae[,5:8])
row.names(Triphasiinae_mat) <- Triphasiinae$`Botanical name`                   
Tri <- sub_clustered_heatmap(Triphasiinae_mat, Triphasiinae)


ht_list = Tod %v% Rut %v% Bal %v% Cit %v% Cla %v% Mer %v% Mic %v% Tri

draw(ht_list, annotation_legend_side = "left")





######################################################################
#plot indivisdial values - heatmap
######################################################################

#setwd(paste0(getwd(),"/Figures/", sep = ""))

#row names just botanical information
row.names(melted_filtered_avg_PAMP_responses) <- filtered_avg_PAMP_response$`Botanical name`

###create function to create automated ploting
Small_subset_heatmap <- function(matrix_in, df_in){
  
  #determine height of image
  number_of_samples <- nrow(matrix_in)
  height_of_heatmap <- number_of_samples*0.2
  if (number_of_samples > 20){
    height_of_heatmap <- number_of_samples*0.14
  }
  
  #determine width of image
  max_char_length <<- (max(nchar(row.names(matrix_in)))*0.25)
  
  
  
  
  #determine coloring - row annotation
  colnames(df_in) <- c("Botanical name","Sub-family","Tribe","Common name","Chitin","Flg22","Csp22","CLas csp22")
  
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

  
  # top annotation
  ha = HeatmapAnnotation('Avg. Max RLUs' = anno_boxplot(matrix_in), height = unit(3, "cm"))
  
  plot_off <- ComplexHeatmap::Heatmap(matrix_in,
                                    #cluster color modificaiton
                                    col = my_color_scale_breaks,
                                    show_heatmap_legend = c(col = FALSE),
                                    
                                    #column modifications
                                    cluster_columns = F,
                                    cluster_rows = T,
                                    
                                    #row modifications + dendrogram
                                    show_row_names = T,
                                    row_names_gp = gpar(fontsize = 12),
                          
                                    #annotate Taxnonomy Info
                                    left_annotation = row_anno,
                                    top_annotation = ha,
                                    
                                    #sizing and border
                                    border = T,
                                    width = unit(1.5, "in"),
                                    height = unit(height_of_heatmap, "in"),
                                    
                                    
                                    use_raster = TRUE, raster_quality = 2)
  
  return(draw(plot_off, annotation_legend_side = "left"))

  
}





######################################################################
#subset data for tania
######################################################################

#subsetting Kumquats
Kumquats <- c("Fortunella margarita","Fortunella hindsii","Fortunella polyandra")
Kumquats_melt <- melted_filtered_avg_PAMP_responses[row.names(melted_filtered_avg_PAMP_responses) %in% Kumquats,]
Small_subset_heatmap(Kumquats_melt)

png("Kumquats_heatmap_subset.png", height = 3, width = max_char_length, units = "in", res = 1200)
dev.off()

#Subsetting Micrantha
Micrantha <- c("Citrus micrantha"," Citrus micrantha var. microcarpa")
Micrantha_melt <- melted_filtered_avg_PAMP_responses[row.names(melted_filtered_avg_PAMP_responses) %in% Micrantha,]

png("./Figures/Micrantha_heatmap_subset.png", height = 3, width = 3.5, units = "in", res = 1200)
Small_subset_heatmap(Micrantha_melt)
dev.off()

Lemon <- c("Citrus x limon var. limon (L.) Burm. f. (Lisbon lemon)","Citrus x limon var. limon (L.) Burm. f. (Eureka lemon)")
Lemon_melt <- melted_filtered_avg_PAMP_responses[row.names(melted_filtered_avg_PAMP_responses) %in% Lemon,]

Small_subset_heatmap(Lemon_melt)
png("/Figures/Lemon_heatmap_subset.png", height = 3, width = 7.5, units = "in", res = 1200)
dev.off()


Grapefruit <- c("Citrus paradisi Macfadyen (Marsh grapefruit)",
                "Citrus paradise Macfadyen (Rio red grapefruit)",
                "Citrus paradisi Macfadyen (New Zealand grapefruit)")

Grapefruit_melt <-  melted_filtered_avg_PAMP_responses[row.names(melted_filtered_avg_PAMP_responses) %in% Grapefruit,]

png("Grapefruit_heatmap_subset.png", height = 3, width = 7.5, units = "in", res = 1200)
Small_subset_heatmap(Grapefruit_melt)
dev.off()





######################################################################
#subset data which has a response for clas csp22
######################################################################


clascsp22_data <- subset(filtered_avg_PAMP_response, filtered_avg_PAMP_response$`CLas csp22` > 0)
clascsp22_data_asMatrix <- as.matrix(clascsp22_data[,c(5,6,7,8)])
row.names(clascsp22_data_asMatrix) <- clascsp22_data$`Botanical name`


png("ClasCps22_subset_heatmap.png", height = 6, width = 5, units = "in", res = 1200)
Small_subset_heatmap(clascsp22_data_asMatrix, clascsp22_data)
dev.off()




not_clascsp22_data <- subset(filtered_avg_PAMP_response, filtered_avg_PAMP_response$`CLas csp22` == 0)
not_clascsp22_data_asMatrix <- as.matrix(not_clascsp22_data[,c(5,6,7,8)])
row.names(not_clascsp22_data_asMatrix) <- not_clascsp22_data$`Botanical name`
Small_subset_heatmap(not_clascsp22_data_asMatrix,not_clascsp22_data)


######################################################################
#subset data which has a response for clas csp22
######################################################################


disease_index <- subset(disease_index, disease_index$Disease_category != 'N/A')


disease_index <- disease_index[order(disease_index$Disease_category, decreasing = F),]


###create function to create automated ploting
disease_index_heatmap <- function(matrix_in, df_in){

  
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
  ra = rowAnnotation('Disease Index' = as.numeric(df_in$Disease_category),
                     col = list('Disease index' =circlize::colorRamp2(c(0, 8), c("green", "red"))))
  
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

disease_subset_RLUs <- melted_filtered_avg_PAMP_responses[row.names(melted_filtered_avg_PAMP_responses) %in% disease_index$`Botanical name`,]


png("Disease_data_subset_heatmap.png", height = 9, width = 10, units = "in", res = 1200)

disease_index_heatmap(disease_subset_RLUs, disease_index)

dev.off()




