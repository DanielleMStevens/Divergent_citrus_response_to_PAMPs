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



#load in alterante data set for mapping on NA data and variable data
alternate_maping_data <- as.data.frame(read_excel(file.choose(), sheet = 2, col_names = TRUE), stringsAsFactors =F)
alternate_maping_data <- alternate_maping_data[1:85,2:9]


############this line is temporary - remove one all data is aquired################
filtered_avg_PAMP_response[is.na(filtered_avg_PAMP_response)] <- 0

######################################################################
#organize data for ploting
######################################################################

#organize raw data
filtered_avg_PAMP_response$`Sub-family` <- as.factor(filtered_avg_PAMP_response$`Sub-family`)
filtered_avg_PAMP_response$Tribe <- as.factor(filtered_avg_PAMP_response$Tribe)
filtered_avg_PAMP_response <- filtered_avg_PAMP_response[order(filtered_avg_PAMP_response$`Sub-family`,
                                                               filtered_avg_PAMP_response$Tribe, 
                                                               decreasing = T),]


#organize alterante data
alternate_maping_data$`Sub-family` <- as.factor(alternate_maping_data$`Sub-family`)
alternate_maping_data$Tribe <- as.factor(alternate_maping_data$Tribe)
alternate_maping_data <- alternate_maping_data[order(alternate_maping_data$`Sub-family`,
                                                     alternate_maping_data$Tribe,
                                                     decreasing = T),]

######################################################################
#need to subset each 'tribe'
######################################################################

#subset raw data
Toddalioideae <- subset(filtered_avg_PAMP_response, Tribe == "Toddalioideae")
Rutoideae <- subset(filtered_avg_PAMP_response, Tribe =="Rutoideae")

Triphasiinae<- subset(filtered_avg_PAMP_response, Tribe == "Triphasiinae")
Micromelinae <- subset(filtered_avg_PAMP_response, Tribe == "Micromelinae")
Merrilliinae <- subset(filtered_avg_PAMP_response, Tribe == "Merrilliinae")
Clauseninae <- subset(filtered_avg_PAMP_response, Tribe == "Clauseninae")
Citrinae <- subset(filtered_avg_PAMP_response, Tribe == "Citrinae")
Balsamocitrinae <- subset(filtered_avg_PAMP_response, Tribe == "Balsamocitrinae")

#subset alternate data
Toddalioideae_alt <- subset(alternate_maping_data, Tribe == "Toddalioideae")
Rutoideae_alt <- subset(alternate_maping_data, Tribe =="Rutoideae")

Triphasiinae_alt <- subset(alternate_maping_data, Tribe == "Triphasiinae")
Micromelinae_alt <- subset(alternate_maping_data, Tribe == "Micromelinae")
Merrilliinae_alt <- subset(alternate_maping_data, Tribe == "Merrilliinae")
Clauseninae_alt <- subset(alternate_maping_data, Tribe == "Clauseninae")
Citrinae_alt <- subset(alternate_maping_data, Tribe == "Citrinae")
Balsamocitrinae_alt <- subset(alternate_maping_data, Tribe == "Balsamocitrinae")

######################################################################
#reorder by botanical name
######################################################################

#reorder raw data
Toddalioideae <- Toddalioideae[str_order(Toddalioideae$`Botanical name`),]
Rutoideae <- Rutoideae[str_order(Rutoideae$`Botanical name`),]


Triphasiinae <- Triphasiinae[str_order(Triphasiinae$`Botanical name`),]
Micromelinae <- Micromelinae[str_order(Micromelinae$`Botanical name`),]
Merrilliinae <- Merrilliinae[str_order(Merrilliinae$`Botanical name`),]
Clauseninae <- Clauseninae[str_order(Clauseninae$`Botanical name`),]
Citrinae <- Citrinae[str_order(Citrinae$`Botanical name`),]
Balsamocitrinae <- Balsamocitrinae[str_order(Balsamocitrinae$`Botanical name`),]

#reorder alternate data
Toddalioideae_alt <- Toddalioideae_alt[str_order(Toddalioideae_alt$`Botanical name`),]
Rutoideae_alt <- Rutoideae_alt[str_order(Rutoideae_alt$`Botanical name`),]


Triphasiinae_alt <- Triphasiinae_alt[str_order(Triphasiinae_alt$`Botanical name`),]
Micromelinae_alt <- Micromelinae_alt[str_order(Micromelinae_alt$`Botanical name`),]
Merrilliinae_alt <- Merrilliinae_alt[str_order(Merrilliinae_alt$`Botanical name`),]
Clauseninae_alt <- Clauseninae_alt[str_order(Clauseninae_alt$`Botanical name`),]
Citrinae_alt <- Citrinae_alt[str_order(Citrinae_alt$`Botanical name`),]
Balsamocitrinae_alt <- Balsamocitrinae_alt[str_order(Balsamocitrinae_alt$`Botanical name`),]

######################################################################
#then push back together
######################################################################

#push back together raw data
filtered_avg_PAMP_response <- rbind(Toddalioideae,Rutoideae,
                                    Balsamocitrinae,Citrinae,Clauseninae,Merrilliinae,Micromelinae,Triphasiinae)

melted_filtered_avg_PAMP_responses <- as.matrix(filtered_avg_PAMP_response[,5:8])


#push back together alternate data
alternate_maping_data <- rbind(Toddalioideae_alt,Rutoideae_alt,
                              Balsamocitrinae_alt, Citrinae_alt, Clauseninae_alt, Merrilliinae_alt, Micromelinae_alt, Triphasiinae_alt)

######################################################################
#row name settings
######################################################################

#collect row nameinformation annotation - common name + botanical name
row_names_to_apply <- filtered_avg_PAMP_response$`Common name`
for (i in 1:length(row_names_to_apply)){
  if(row_names_to_apply[i] == "0"){
    row_names_to_apply[i] <- filtered_avg_PAMP_response$`Botanical name`[i]
  }
}
row.names(melted_filtered_avg_PAMP_responses) <- row_names_to_apply


#row names just botanical information
row.names(melted_filtered_avg_PAMP_responses) <- filtered_avg_PAMP_response$`Botanical name`

######################################################################
#settings for inital response - color code
######################################################################


#create a customs scale to take in differential responses
my_scale_breaks <- c(0,200, 500, 1000, 5000, 20000, 50000, 100000)
colors <- RColorBrewer::brewer.pal(length(my_scale_breaks), "Reds")
my_color_scale_breaks <- circlize::colorRamp2(my_scale_breaks, colors)



##pull out row names to annotate with
citrus_realtionship_info <- filtered_avg_PAMP_response[,2:3]
citrus_realtionship_info_simplified <- citrus_realtionship_info[!duplicated(citrus_realtionship_info[,1:2]),]


#need to determine color codes
a <- unique(citrus_realtionship_info_simplified$`Sub-family`)
b <- unique(citrus_realtionship_info_simplified$Tribe)
b <- b[!b %in% a]

col1_only <- RColorBrewer::brewer.pal(length(a), "Pastel1")
col2_only <- RColorBrewer::brewer.pal(length(b), "Pastel2")

col1 <- setNames(RColorBrewer::brewer.pal(length(a), "Pastel1"), a)
col2 <- setNames(RColorBrewer::brewer.pal(length(b), "Pastel2"), b)
col2 <- append(col2,  col1)
col.list <- list(a = col1, b = col2)

col_list_ht <- circlize::colorRamp2(append(a,b),append(col1_only,col2_only))


######################################################################
#plot heatmap max rlu 
######################################################################


ht_opt(legend_boarder = "black")

row_anno <- rowAnnotation(df = citrus_realtionship_info,
                          col = col_list_ht, 
                          border = TRUE)


png("Heatmap_plot_all_values_v2.png", units="in", width=8, height=10, res=800)

ht = ComplexHeatmap::Heatmap(melted_filtered_avg_PAMP_responses,
                        
                        #cluster color modificaiton
                        col = my_color_scale_breaks,
                        
                        #column modifications
                        cluster_columns = F,
                        cluster_rows = F,
                        
                        #row modifications + dendrogram
                        show_row_names = T,
                        row_names_gp = gpar(fontsize = 8),

                        
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


draw(ht, heatmap_legend_side = "left")

dev.off()




#right_annotation = rowAnnotation(foo = anno),
anno <- anno_zoom(align_to = filtered_avg_PAMP_response$Tribe, which = "row", panel_fun = panel_fun_ggplot2, 
                  size = unit(3, "cm"), gap = unit(1, "cm"), width = unit(4, "cm"))

  
panel_fun_ggplot2 = function(index, nm) {
  df = melt(m[index, , drop = FALSE], varnames = c("row_index", "col_index"))
  g = ggplot(df, aes(x = factor(col_index), y = value)) + geom_boxplot()
  g = grid.grabExpr(print(g))
  pushViewport(viewport())
  grid.rect()
  grid.draw(g)
  popViewport()
}


                                              
######################################################################
#plot indivisdial values - heatmap
######################################################################

###create function to create automated ploting

Small_subset_heatmap <- function(matrix_in){
  
  #determine height of image
  number_of_samples <- nrow(matrix_in)
  height_of_heatmap <- number_of_samples*0.25
  
  #determine width of image
  #max_char_length <- max(nchar(row.names(matrix_in)))
  
  hold_ht = ComplexHeatmap::Heatmap(matrix_in,
                          #cluster color modificaiton
                          col = my_color_scale_breaks,
                          show_heatmap_legend = c(col = FALSE),
                          
                          #column modifications
                          cluster_columns = F,
                          cluster_rows = F,
                          
                          #row modifications + dendrogram
                          show_row_names = T,
                          row_names_gp = gpar(fontsize = 12),
                          
                          #sizing and border
                          border = T,
                          width = unit(1.5, "in"),
                          height = unit(height_of_heatmap, "in"),
                          use_raster = TRUE, raster_quality = 2)
  
}


#subsetting Kumquats
Kumquats <- c("Fortunella margarita","Fortunella hindsii","Fortunella polyandra")
Kumquats_melt <- melted_filtered_avg_PAMP_responses[row.names(melted_filtered_avg_PAMP_responses) %in% Kumquats,]

png("Kumquats_heatmap_subset.png", height = 3, width = 3.5, units = "in", res = 1200)
Small_subset_heatmap(Kumquats_melt)
dev.off()

#Subsetting Micrantha
Micrantha <- c("Citrus micrantha"," Citrus micrantha var. microcarpa")
Micrantha_melt <- melted_filtered_avg_PAMP_responses[row.names(melted_filtered_avg_PAMP_responses) %in% Micrantha,]

png("Micrantha_heatmap_subset.png", height = 3, width = 3.5, units = "in", res = 1200)
Small_subset_heatmap(Micrantha_melt)
dev.off()

Lemon <- c("Citrus x limon var. limon (L.) Burm. f. (Lisbon lemon)","Citrus x limon var. limon (L.) Burm. f. (Eureka lemon)")
Lemon_melt <- melted_filtered_avg_PAMP_responses[row.names(melted_filtered_avg_PAMP_responses) %in% Lemon,]

png("Lemon_heatmap_subset.png", height = 3, width = 7.5, units = "in", res = 1200)
Small_subset_heatmap(Lemon_melt)
dev.off()


Grapefruit <- c("Citrus paradisi Macfadyen (Marsh grapefruit)",
                "Citrus paradise Macfadyen (Rio red grapefruit)",
                "Citrus paradisi Macfadyen (New Zealand grapefruit)")

Grapefruit_melt <-  melted_filtered_avg_PAMP_responses[row.names(melted_filtered_avg_PAMP_responses) %in% Grapefruit,]

png("Grapefruit_heatmap_subset.png", height = 3, width = 7.5, units = "in", res = 1200)
Small_subset_heatmap(Grapefruit_melt)
dev.off()


######################################################################
#plot indivisdial values - boxplot? for each tribe data set
######################################################################

Toddalioideae_melt <- reshape2::melt(Toddalioideae)
Toddalioideae_box <- ggplot(Toddalioideae_melt, aes(x = variable, y = value, fill = `Sub-family`)) +
  geom_boxplot() +
  geom_jitter(aes(colour = `Sub-family`), size = 1.5, alpha = 0.8, width= 0.2, colour = "black") +
  theme_bw() +
  ylab("Max RLUs") + 
  xlab("") +
  scale_y_log10() +
  theme(legend.position = "none")+
  scale_fill_manual("Sub-Family", values = col.list$a)+
  scale_colour_manual("Sub-Family", values = col.list$a) +
  labs(subtitle = c(as.character(unique(Toddalioideae_melt$Tribe))))



Balsamocitrinae_melt <- reshape2::melt(Balsamocitrinae)
Balsamocitrinae_box <- ggplot(Balsamocitrinae_melt, aes(x = variable, y = value, fill = Tribe)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(aes(color = Tribe), size = 1.5, alpha = 0.8, width= 0.2, colour = "black") +
  theme_bw() +
  ylab("Max RLUs") +
  xlab("") +
  scale_y_log10() +
  theme(legend.position = "none")+
  scale_fill_manual("Tribe", values = col.list$b) +
  scale_colour_manual("Tribe", values = col.list$b) +
  labs(subtitle = c(as.character(unique(Balsamocitrinae_melt$Tribe))))



Citrinae_melt <- reshape2::melt(Citrinae)
Citrinae_box <- ggplot(Citrinae_melt, aes(x = variable, y = value, fill = Tribe)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(aes(color = Tribe), size = 1.5, alpha = 0.8, width= 0.2, colour = "black") +
  theme_bw() +
  ylab("Max RLUs") + 
  xlab("") +
  scale_y_log10() +
  theme(legend.position = "none")+
  scale_fill_manual("Tribe", values = col.list$b) +
  scale_colour_manual("Tribe", values = col.list$b) +
  labs(subtitle = c(as.character(unique(Citrinae_melt$Tribe))))



Clauseninae_melt <- reshape2::melt(Clauseninae)
Clauseninae_box <- ggplot(Clauseninae_melt, aes(x = variable, y = value, fill = Tribe)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(aes(color = Tribe), size = 1.5, alpha = 0.8, width= 0.2, colour = "black") +
  theme_bw() +
  ylab("Max RLUs") + 
  xlab("") +
  scale_y_log10() +
  theme(legend.position = "none")+
  scale_fill_manual("Tribe", values = col.list$b) +
  scale_colour_manual("Tribe", values = col.list$b) +
  labs(subtitle = c(as.character(unique(Clauseninae_melt$Tribe))))



Triphasiinae_melt <- reshape2::melt(Triphasiinae)
Triphasiinae_box <- ggplot(Triphasiinae_melt, aes(x = variable, y = value, fill = Tribe)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(aes(color = Tribe), size = 1.5, alpha = 0.8, width= 0.2, colour = "black") +
  theme_bw() +
  ylab("Max RLUs") + 
  xlab("") +
  scale_y_log10() +
  theme(legend.position = "none")+
  scale_fill_manual("Tribe", values = col.list$b) +  
  scale_colour_manual("Tribe", values = col.list$b) +
  labs(subtitle = c(as.character(unique(Triphasiinae_melt$Tribe))))


png("Comparison_of_Max_RLUs_across_Tribes.png", height = 8, width = 3.5, units = "in", res = 800)
Toddalioideae_box/Balsamocitrinae_box/Citrinae_box/Clauseninae_box/Triphasiinae_box
dev.off()


######################################################################
#plot degress of variation
######################################################################

color_code_samples <- c('Yes','No','Variable','N/A')
color_code_samples_col <- RColorBrewer::brewer.pal(length(color_code_samples),"Set2")
names(color_code_samples_col) <- color_code_samples

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
#plot heatmap max rlu 
######################################################################

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






