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


#need to determine color codes
#a <- unique(citrus_realtionship_info_simplified$`Sub-family`)
#b <- unique(citrus_realtionship_info_simplified$Tribe)
#b <- b[!b %in% a]


#col1_only <- RColorBrewer::brewer.pal(length(a), "Pastel1")
#col2_only <- RColorBrewer::brewer.pal(length(b), "Pastel2")

#col1 <- setNames(RColorBrewer::brewer.pal(length(a), "Pastel1"), a)
#col2 <- setNames(RColorBrewer::brewer.pal(length(b), "Pastel2"), b)
#col2 <- append(col2,  col1)
#col.list <- list(a = col1, b = col2)
    
#col_list_ht <- circlize::colorRamp2(append(a,b),append(col1_only,col2_only))


######################################################################
#plot heatmap max rlu 
######################################################################


png("Heatmap_plot_all_values_organize_by_taxonomy_no_chitin.png", units="in", width=8, height=10, res=800)




row_anno <- rowAnnotation(df = citrus_realtionship_info,
                          border = TRUE,
                          col = list(`Sub-family` =  c("Toddalioideae" = "#FBBE4E", "Rutoideae" = "#E3DECA", "Aurantioideae" = "#273253"), 
                                     
                                     Tribe = c("Balsamocitrinae" = "#1F6768", "Citrinae" = "#A8C653", "Clauseninae" = "#E45D50",
                                               "Merrilliinae" = "#544275", "Micromelinae" = "#CAA2DD", "Triphasiinae" = "#4EAEDF",
                                               "Toddalioideae" = "#FBBE4E", "Rutoideae" = "#E3DECA", "Aurantioideae" = "#273253")))
                          


ht = ComplexHeatmap::Heatmap(melted_filtered_avg_PAMP_responses[,2:4],
                             
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
#plot indivisdial values - heatmap
######################################################################

setwd(paste0(getwd(),"/Figures/", sep = ""))

#row names just botanical information
row.names(melted_filtered_avg_PAMP_responses) <- filtered_avg_PAMP_response$`Botanical name`

###create function to create automated ploting
Small_subset_heatmap <- function(matrix_in){
  
  #determine height of image
  number_of_samples <- nrow(matrix_in)
  height_of_heatmap <- number_of_samples*0.2
  if (number_of_samples > 20){
    height_of_heatmap <- number_of_samples*0.14
  }
  
  #determine width of image
  max_char_length <<- (max(nchar(row.names(matrix_in)))*0.25)

  
  ha = HeatmapAnnotation('Avg. Max RLUs' = anno_boxplot(matrix_in), height = unit(3, "cm"))
  
  ComplexHeatmap::Heatmap(matrix_in,
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
                                    #left_annotation = row_anno,
                                    top_annotation = ha,
                                    
                                    #sizing and border
                                    border = T,
                                    width = unit(1.5, "in"),
                                    height = unit(height_of_heatmap, "in"),
                                    use_raster = TRUE, raster_quality = 2)
  
}


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
Small_subset_heatmap(clascsp22_data_asMatrix)
dev.off()


not_clascsp22_data <- subset(filtered_avg_PAMP_response, filtered_avg_PAMP_response$`CLas csp22` == 0)
not_clascsp22_data_asMatrix <- as.matrix(not_clascsp22_data[,c(5,6,7,8)])
row.names(not_clascsp22_data_asMatrix) <- not_clascsp22_data$`Botanical name`
Small_subset_heatmap(not_clascsp22_data_asMatrix)


######################################################################
#subset data which has a response for clas csp22
######################################################################


disease_index <- subset(disease_index, disease_index$Disease_category != 'N/A')


disease_index <- disease_index[order(disease_index$Disease_category, decreasing = F),]


###create function to create automated ploting
disease_index_heatmap <- function(matrix_in, df_in){
  
  colnames(df_in) <- c("Botanical name","Sub-family","Tribe","Common name","Disease_category")
  Taxon_df <- cbind(df_in$`Sub-family`, df_in$Tribe)
  
  row_anno <- rowAnnotation(df = Taxon_df,
                            border = TRUE,
                            col = list(`Sub-family` =  c("Aurantioideae" = "#273253"), 
                                       
                                       Tribe = c("Balsamocitrinae" = "#1F6768", "Citrinae" = "#A8C653", "Clauseninae" = "#E45D50")))
  
  
  
  
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




