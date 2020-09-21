#-----------------------------------------------------------------------------------------------
# Coaker Lab - Plant Pathology Department UC Davis
# Author: Danielle M. Stevens
# Last Updated: 4/15/2020
# Script Purpose: Plotting ROS data from divergent cirtus responding to different PAMPS
# Inputs Necessary: 
# Outputs: 
#-----------------------------------------------------------------------------------------------



##############################################
# Load Processed Data and Colors
##############################################

#make sure to set path to the same place where the figure 
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# Load libraries to run scripts
source("./Libraries_to_load.R")

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
my_scale_breaks <-  c(200, 500, 1000, 5000, 20000, 50000, 100000)
colors <- RColorBrewer::brewer.pal(length(my_scale_breaks), "Reds")
my_scale_breaks <-  c(0, 200, 500, 1000, 5000, 20000, 50000, 100000)
colors <- c("#FFFFFF",colors)
my_color_scale_breaks <- circlize::colorRamp2(my_scale_breaks, colors)



##pull out row names to annotate with
citrus_realtionship_info <- as.data.frame(filtered_avg_PAMP_response[,2:3], stringsAsFactors = FALSE)
citrus_realtionship_info$`Sub-family` <- as.character(citrus_realtionship_info$`Sub-family`)
citrus_realtionship_info$Tribe <- as.character(citrus_realtionship_info$Tribe)
#citrus_realtionship_info_simplified <- citrus_realtionship_info[!duplicated(citrus_realtionship_info[,1:2]),]



######################################################################
#plot heatmap max rlu  - plot by taxonomy
######################################################################

png("Heatmap_plot_all_values_organize_by_taxonomy.png", units="in", width=9, height=11, res=1200)


#pdf("Heatmap_plot_all_values_organize_by_taxonomy.pdf", width = 8, height = 10)
##need to fix this error: 
#Error in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  : 
#invalid font type
#coomon error: https://github.com/stan-dev/rstan/issues/381
##

row_anno <- rowAnnotation(df = citrus_realtionship_info,
                          border = TRUE,
                          gap = unit(1, "mm"),
                          show_legend = FALSE,
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
                               height = unit(9, "in"),
                               
                               #annotate Taxnonomy Info
                               left_annotation = row_anno,
                               
                               
                               
                               #details regarding modifying legend
                               heatmap_legend_param = list(col_fun = my_color_scale_breaks, #main MAX RLU annotation
                                                           legend_width = unit(35, "mm"),
                                                           legend_height = unit(40, "mm"),
                                                           #direction = "horizontal",
                                                           #labels_rot = 90,
                                                           title = "Max RLUs", 
                                                           border = "black",
                                                           at = my_scale_breaks,
                                                           title_gp = gpar(fontsize = 12, fontface = "bold", fontfamily = "Arial"),
                                                           labels_gp = gpar(fontsize = 10, fontfamily = "Arial"),
                                                           grid_width = unit(0.7, "cm"),
                                                           legend_label_gp = gpar(col = "black", fontsize = 10, fontfamily = "Arial")
                                                           ),
                               use_raster = TRUE, raster_quality = 4, raster_device = 'png')
                               #use_raster = TRUE, raster_device = "CairoJPEG") 
                              #
  
lgd_list = list(
    # Sub-family annotation
    Legend(labels = c("Toddalioideae", "Rutoideae", "Aurantioideae"), title = "Sub-family", 
           border = "black",
           gap = unit(2, "mm"),
           legend_gp = gpar(fill = c("#FBBE4E","#E3DECA", "#273253"))),
    
    # Tribe annotation
    Legend(labels = c("Balsamocitrinae", "Citrinae", "Clauseninae", 
                      "Merrilliinae", "Micromelinae", "Triphasiinae"), title = "Tribe",
           border = "black",
           gap = unit(2, "mm"),
           legend_gp = gpar(fill = c("#1F6768", "#A8C653", "#E45D50",
                                     "#544275", "#CAA2DD", "#4EAEDF")))
    
    # Variable annottion
    #Legend(labels = c("Represents one positive ROS burst with several negatives"), title = "Variable ROS", type = "points", pch="*")
    
  )


draw(ht, adjust_annotation_extension = TRUE, heatmap_legend_side = "left", annotation_legend_side = "left", annotation_legend_list = lgd_list)
  
  
##### add 'variable samples' stars
# This function takes the input alternate data mapping, finds which points are labeled variable, 
# and plots * are coordinate points respective to which samples are smapled as variable
#####################
plot_variable_dots <- function(alternate_data_in){
    name_of_heatmap <- grepl("heatmap_body_1_1", list_components())
    name_of_heatmap <- list_components()[name_of_heatmap]
    name_of_heatmap <- str_remove(name_of_heatmap,"_heatmap_body_1_1")
    for (j in 1:nrow(alternate_data_in)){
      for (i in 1:ncol(alternate_data_in)){
        if (alternate_data_in[j,i] == 'Variable'){
          ComplexHeatmap::decorate_heatmap_body(name_of_heatmap, grid.text("*",
                                                                           #x coordinate 
                                                                           (i-0.5)/ncol(alternate_data_in), 
                                                                           #y coordinate
                                                                           (nrow(alternate_data_in)-j)/nrow(alternate_data_in), 
                                                                           just = "center",
                                                                           gp = gpar(fontsize = 16)))
        }      
      }
    }
  }
  
  
plot_variable_dots(melted_alternate_maping_data)
  

dev.off()
  



######################################################################
#plot heatmap max rlu  - plot by taxonomy with clustering within taxanomic group
######################################################################


sub_clustered_heatmap <- function(matrix_in, df_in){
  
  #determine height of image & width of image
  if (is.null(ncol(matrix_in)) == FALSE){
    number_of_samples <- nrow(matrix_in)
    height_of_heatmap <- number_of_samples*0.16
    if (number_of_samples > 20){
      height_of_heatmap <- number_of_samples*0.14
    }
    
    max_char_length <<- (max(nchar(row.names(matrix_in)))*0.25)
  }
  if (is.null(ncol(matrix_in)) == TRUE){
    number_of_samples <- length(matrix_in)
    height_of_heatmap <- number_of_samples*0.16
    if (number_of_samples > 20){
      height_of_heatmap <- number_of_samples*0.14
    }
    
    max_char_length <<- (max(nchar(names(matrix_in)))*0.25)
  }
  

  
  #determine coloring
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
  
  
  ht = ComplexHeatmap::Heatmap(matrix_in,
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
                          width = unit(1, "in"),
                          height = unit(height_of_heatmap, "in"),
                          use_raster = TRUE, raster_quality = 2)
  
  draw(ht, annotation_legend_side = "left", heatmap_legend_side = "left")
  
}

# plot different combination of inidivudal PAMPs
png("just_chitin_by_taxonomy.png", height = 14, width = 8, units = "in", res = 1000)
just_chitin <- sub_clustered_heatmap(melted_filtered_avg_PAMP_responses[,1], filtered_avg_PAMP_response[,1:5])
dev.off()

png("just_flg22_by_taxonomy.png", height = 14, width = 8,  units = "in", res = 1000)
just_flg22 <- sub_clustered_heatmap(melted_filtered_avg_PAMP_responses[,2], filtered_avg_PAMP_response[,c(1,2,3,4,6)])
dev.off()

png("just_csp22_by_taxonomy.png", height = 14, width = 8,  units = "in", res = 1000)
just_csp22 <- sub_clustered_heatmap(melted_filtered_avg_PAMP_responses[,3], filtered_avg_PAMP_response[,c(1,2,3,4,7)])
dev.off()

png("just_clascsp22_by_taxonomy.png", height = 14, width = 8,  units = "in", res = 1000)
just_clasCsp22 <- sub_clustered_heatmap(melted_filtered_avg_PAMP_responses[,4], filtered_avg_PAMP_response[,c(1,2,3,4,8)])
dev.off()


chitin_flg22 <- sub_clustered_heatmap(melted_filtered_avg_PAMP_responses[,1:2], filtered_avg_PAMP_response[,1:6])
chitin_csp22 <- sub_clustered_heatmap(melted_filtered_avg_PAMP_responses[,c(1,3)], filtered_avg_PAMP_response[,c(1,2,3,4,5,7)])


# cluster by subfamily/tribe individually -> then add together
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


box_and_bar_heatmap <- function(matrix_in, df_in){
  
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
  
  # row annotation on taxonomy
  row_anno <- rowAnnotation(df = Taxon_df,
                            border = TRUE,
                            show_legend =c(FALSE,FALSE),
                            col = list(`Sub-family` = colors_subfamily_filtered,
                                       Tribe = colors_tribe_filtered))
  
  
  # top annotation
  ha = HeatmapAnnotation('Avg. Max RLUs' = anno_boxplot(matrix_in), height = unit(3, "cm"))
  
  # row annotation on total response of all MAMPs
  ra = rowAnnotation('Total RLU Response' = anno_barplot(matrix_in), width = unit(2 , "cm"))
  
  
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
                                      right_annotation = ra,
                                      
                                      #sizing and border
                                      border = T,
                                      width = unit(1.5, "in"),
                                      height = unit(height_of_heatmap, "in"),
                                      
                                      
                                      use_raster = TRUE, raster_quality = 2)
  
  return(draw(plot_off, annotation_legend_side = "left"))
  
  
}


#subset by all data which is known to respond to clas csp22
clascsp22_data <- subset(filtered_avg_PAMP_response, filtered_avg_PAMP_response$`CLas csp22` > 0)
clascsp22_data_asMatrix <- as.matrix(clascsp22_data[,c(5,6,7,8)])
row.names(clascsp22_data_asMatrix) <- clascsp22_data$`Botanical name`

#png("ClasCps22_subset_heatmap.png", height = 6, width = 5, units = "in", res = 1200)
box_and_bar_heatmap(clascsp22_data_asMatrix, clascsp22_data)
#dev.off()

#subset by all data which is known not to respond to clas csp22
not_clascsp22_data <- subset(filtered_avg_PAMP_response, filtered_avg_PAMP_response$`CLas csp22` == 0)


## choose from "Chitin", "Flg22", "Csp22"
plot_top_nine <- function(MAMP_name){
  data_in <- dplyr::arrange(not_clascsp22_data, desc(MAMP_name))
  data_in_asMatrix <- as.matrix(data_in[,c(5,6,7,8)])
  row.names(data_in_asMatrix) <- not_clascsp22_data$`Botanical name`
  return(data_in_asMatrix[1:9,])
}

#arrrange and plot by top 9 hits of chitin
not_clascsp22_data <- dplyr::arrange(not_clascsp22_data, desc(Chitin))
not_clascsp22_data_asMatrix <- as.matrix(not_clascsp22_data[,c(5,6,7,8)])
row.names(not_clascsp22_data_asMatrix) <- not_clascsp22_data$`Botanical name`
box_and_bar_heatmap(not_clascsp22_data_asMatrix[1:9,],not_clascsp22_data[1:9,])

#arrrange and plot by top 9 hits of flg22
not_clascsp22_data <- dplyr::arrange(not_clascsp22_data, desc(Flg22))
not_clascsp22_data_asMatrix <- as.matrix(not_clascsp22_data[,c(5,6,7,8)])
row.names(not_clascsp22_data_asMatrix) <- not_clascsp22_data$`Botanical name`
box_and_bar_heatmap(not_clascsp22_data_asMatrix[1:9,],not_clascsp22_data[1:9,])

#arrrange and plot by top 9 hits of csp22
not_clascsp22_data <- dplyr::arrange(not_clascsp22_data, desc(Csp22))
not_clascsp22_data_asMatrix <- as.matrix(not_clascsp22_data[,c(5,6,7,8)])
row.names(not_clascsp22_data_asMatrix) <- not_clascsp22_data$`Botanical name`
box_and_bar_heatmap(not_clascsp22_data_asMatrix[1:9,],not_clascsp22_data[1:9,])

######################################################################
#subset data which has a response for clas csp22
######################################################################


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


disease_index <- subset(disease_index, disease_index$Disease_category != 'N/A')
disease_index <- disease_index[order(disease_index$Disease_category, decreasing = F),]
disease_index$Disease_category <- as.numeric(disease_index$Disease_category)

disease_subset_RLUs <- melted_filtered_avg_PAMP_responses[row.names(melted_filtered_avg_PAMP_responses) %in% disease_index$`Botanical name`,]
disease_subset_RLUs <- disease_subset_RLUs[disease_index$`Botanical name`,]

#png("Disease_data_subset_heatmap.png", height = 9, width = 10, units = "in", res = 1200)

disease_index_heatmap(disease_subset_RLUs, disease_index)

#dev.off()




