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

# NOTE: I have had issues sometimes loading the complex heatmap package (not sure why), 
# Try one of the many ways to download the package and if still running into troubles,
# consult google or contact me

#install_github("jokergoo/ComplexHeatmap")
#devtools::install_github("jokergoo/ComplexHeatmap")
#if (!requireNamespace("BiocManager", quietly=TRUE))
#install.packages("BiocManager")
#BiocManager::install("ComplexHeatmap")

######################################################################
#upload raw data 
######################################################################

#raw go-terms file to process - load in file
file_to_open <- file.choose() #choose the file: Summary_of_PAMP_response.xlsx
average_PAMP_response <- as.data.frame(read_excel(file_to_open, sheet=1, 
                                            col_names = TRUE), stringsAsFactors = F)


filtered_avg_PAMP_response <- average_PAMP_response[1:85,c(2:9)]

############this line is temporary - remove one all data is aquired################
filtered_avg_PAMP_response[is.na(filtered_avg_PAMP_response)] <- 0

######################################################################
#organize data for ploting
######################################################################

filtered_avg_PAMP_response$`Sub-family` <- as.factor(filtered_avg_PAMP_response$`Sub-family`)
filtered_avg_PAMP_response$Tribe <- as.factor(filtered_avg_PAMP_response$Tribe)
filtered_avg_PAMP_response <- filtered_avg_PAMP_response[order(filtered_avg_PAMP_response$`Sub-family`,
                                                               filtered_avg_PAMP_response$Tribe, 
                                                               decreasing = T),]

######################################################################
#need to subset each 'tribe'
######################################################################

Toddalioideae <- subset(filtered_avg_PAMP_response, Tribe == "Toddalioideae")
Rutoideae <- subset(filtered_avg_PAMP_response, Tribe =="Rutoideae")

Triphasiinae<- subset(filtered_avg_PAMP_response, Tribe == "Triphasiinae")
Micromelinae <- subset(filtered_avg_PAMP_response, Tribe == "Micromelinae")
Merrilliinae <- subset(filtered_avg_PAMP_response, Tribe == "Merrilliinae")
Clauseninae <- subset(filtered_avg_PAMP_response, Tribe == "Clauseninae")
Citrinae <- subset(filtered_avg_PAMP_response, Tribe == "Citrinae")
Balsamocitrinae <- subset(filtered_avg_PAMP_response, Tribe == "Balsamocitrinae")


######################################################################
#reorder by botanical name
######################################################################

Toddalioideae <- Toddalioideae[str_order(Toddalioideae$`Botanical name`),]
Rutoideae <- Rutoideae[str_order(Rutoideae$`Botanical name`),]


Triphasiinae <- Triphasiinae[str_order(Triphasiinae$`Botanical name`),]
Micromelinae <- Micromelinae[str_order(Micromelinae$`Botanical name`),]
Merrilliinae <- Merrilliinae[str_order(Merrilliinae$`Botanical name`),]
Clauseninae <- Clauseninae[str_order(Clauseninae$`Botanical name`),]
Citrinae <- Citrinae[str_order(Citrinae$`Botanical name`),]
Balsamocitrinae <- Balsamocitrinae[str_order(Balsamocitrinae$`Botanical name`),]


######################################################################
#then push back together
######################################################################

filtered_avg_PAMP_response <- rbind(Toddalioideae,Rutoideae,
                                    Balsamocitrinae,Citrinae,Clauseninae,Merrilliinae,Micromelinae,Triphasiinae)


melted_filtered_avg_PAMP_responses <- as.matrix(filtered_avg_PAMP_response[,5:8])



######################################################################
#settings for inital response - color code
######################################################################


#create a customs scale to take in differential responses
my_scale_breaks <- c(0,200, 500, 1000, 5000, 20000, 50000, 100000)
colors <- RColorBrewer::brewer.pal(length(my_scale_breaks), "Reds")
my_color_scale_breaks <- circlize::colorRamp2(my_scale_breaks, colors)


#collect row nameinformation annotation
row_names_to_apply <- filtered_avg_PAMP_response$`Common name`
for (i in 1:length(row_names_to_apply)){
  if(row_names_to_apply[i] == "0"){
    row_names_to_apply[i] <- filtered_avg_PAMP_response$`Botanical name`[i]
  }
}
row.names(melted_filtered_avg_PAMP_responses) <- row_names_to_apply

##pull out row names to annotate with
citrus_realtionship_info <- filtered_avg_PAMP_response[,2:3]
citrus_realtionship_info_simplified <- citrus_realtionship_info[!duplicated(citrus_realtionship_info[,1:2]),]


#need to determine color codes
a <- unique(citrus_realtionship_info_simplified$`Sub-family`)
b <- unique(citrus_realtionship_info_simplified$Tribe)
b <- b[!b %in% a]

col1 <- setNames(RColorBrewer::brewer.pal(length(a), "Pastel1"), a)
col2 <- setNames(RColorBrewer::brewer.pal(length(b), "Pastel2"), b)
#col2 <- append(col2,  col1)
col.list <- list(a = col1, b = col2)


######################################################################
#plot heatmap max rlu 
######################################################################



row_anno <- rowAnnotation(df = citrus_realtionship_info,
                          col = col.list, 
                          border = TRUE)
                          #heatmap_legend_param = list(
                          #title_gp = gpar(fontsize = 12, fontface = "bold", fontfamily = "Arial"),
                          #labels_gp = gpar(fontsize = 10,fontfamily = "Arial")))

anno <- anno_zoom(align_to = filtered_avg_PAMP_response$Tribe, which = "row", panel_fun = panel_fun_ggplot2, 
                  size = unit(3, "cm"), gap = unit(1, "cm"), width = unit(4, "cm"))
#pdf("Heatmap_plot_all_values_v1.pdf", width = 6, height = 10)
png("Heatmap_plot_all_values_v1.png", units="in", width=6, height=10, res=800)

ComplexHeatmap::Heatmap(melted_filtered_avg_PAMP_responses,
                        
                        #cluster color modificaiton
                        col = my_color_scale_breaks,
                        
                        #column modifications
                        cluster_columns = F,
                        cluster_rows = F,
                        
                        #row modifications + dendrogram
                        show_row_names = T,
                        row_names_gp = gpar(fontsize = 8),
                        #row_labels = row_labels[rownames(mat)],
                        
                        #right_annotation = rowAnnotation(foo = anno),
                        
                        #row_dend_width = unit(30, "mm"),
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
  
dev.off()



  
panel_fun_ggplot2 = function(index, nm) {
  df = melt(m[index, , drop = FALSE], varnames = c("row_index", "col_index"))
  g = ggplot(df, aes(x = factor(col_index), y = value)) + geom_boxplot()
  g = grid.grabExpr(print(g))
  pushViewport(viewport())
  grid.rect()
  grid.draw(g)
  popViewport()
}


                                              





#first_row_anno <- data.frame(filtered_avg_PAMP_response$Tribe)
#colnames(first_row_anno) <- c("Tribe")
#color_first_row <- RColorBrewer::brewer.pal(nrow(unique(first_row_anno)), "Paired")
#names(color_first_row) <- as.character(unlist(unique(first_row_anno)))



##second_row_anno <- data.frame(filtered_avg_PAMP_response$`Sub-family`)
#colnames(second_row_anno) <- c("Sub-Family")

# need to cluster data to apply to heatmap
#cluster_data <- as.data.frame(filtered_avg_PAMP_response[,1:3], stringsAsFactors = T)
#cluster_data <- cluster_data[,c(2,3,1)]


#cluster_data$`Sub-family` <- as.factor(cluster_data$`Sub-family`)
#cluster_data$Tribe <- as.factor(cluster_data$Tribe)
#cluster_data$`Botanical name` <- as.factor(cluster_data$`Botanical name`)

#cluster_data <- with(cluster_data, cluster_data[order(`Sub-family`, Tribe, decreasing = T)])

#cluster_data$pathString <- paste("Family", cluster_data$`Sub-family`, cluster_data$Tribe, sep = "/")
#cluster_tree <- data.tree::as.Node(cluster_data$pathString)

                                             
ComplexHeatmap::draw(Heatmap_everything,    
                     heatmap_legend_side = "right", padding = unit(c(5,5,5,5), "mm"))






