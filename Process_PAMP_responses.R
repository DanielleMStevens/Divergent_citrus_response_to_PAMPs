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
library(readxl)
library(stringr)
library(reshape2)
library(rstudioapi)

######################################################################
#upload raw data 
######################################################################

#setwd to where repo was cloned and maintained
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))

#raw go-terms file to process - load in file
average_PAMP_response <- as.data.frame(read_excel("./Raw_files/Summary_of_PAMP_response.xlsx", sheet=1, col_names = TRUE), stringsAsFactors = F)
filtered_avg_PAMP_response <- average_PAMP_response[1:85,c(2:9)]

filtered_avg_PAMP_response <- filtered_avg_PAMP_response[,c(1,2,3,4,7,8,6,5)]


#load in alterante data set for mapping on NA data and variable data
alternate_maping_data <- as.data.frame(read_excel("./Raw_files/Summary_of_PAMP_response.xlsx", sheet = 2, col_names = TRUE), stringsAsFactors =F)
alternate_maping_data <- alternate_maping_data[1:85,2:9]

alternate_maping_data <- alternate_maping_data[,c(1,2,3,4,7,8,6,5)]

############this line is temporary - remove one all data is aquired################
filtered_avg_PAMP_response[is.na(filtered_avg_PAMP_response)] <- 0



#raw go-terms file to process - load in file
disease_index <- as.data.frame(read_excel("./Raw_files/Summary_of_PAMP_response.xlsx", sheet=3, col_names = TRUE), stringsAsFactors = F)
disease_index[is.na(disease_index)] <- 'N/A'





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

                                             
#ComplexHeatmap::draw(Heatmap_everything,heatmap_legend_side = "right", padding = unit(c(5,5,5,5), "mm"))






