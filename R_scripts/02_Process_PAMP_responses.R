#-----------------------------------------------------------------------------------------------
# Coaker Lab - Plant Pathology Department UC Davis
# Author: Danielle M. Stevens
# Last Updated: 03/26/2022
# Script Purpose: Plotting ROS data from divergent cirtus responding to different PAMPS
# Inputs Necessary: Summary_of_PAMP_response.xlsx file from Raw_ROS_files folder
# Outputs: N/A, Leads to cleaned up data for plotting 
#-----------------------------------------------------------------------------------------------


######################################################################
#upload raw data 
######################################################################


#raw go-terms file to process - load in file
average_PAMP_response <- as.data.frame(readxl::read_excel("./Raw_ROS_files/Summary_of_PAMP_response.xlsx", sheet = 1, col_names = TRUE), stringsAsFactors = F)
filtered_avg_PAMP_response <- average_PAMP_response[,c(1,2,3,4,7,8,6,5)]
rm(average_PAMP_response)



#load in alterante data set for mapping on NA data and variable data
alternate_maping_data <- as.data.frame(readxl::read_excel("./Raw_ROS_files/Summary_of_PAMP_response.xlsx", sheet = 2, col_names = TRUE), stringsAsFactors =F)
alternate_maping_data <- alternate_maping_data[1:86,2:9]
alternate_maping_data <- alternate_maping_data[,c(1,2,3,4,7,8,6,5)]




#raw go-terms file to process - load in file
disease_index <- as.data.frame(readxl::read_excel("./Raw_ROS_files/Summary_of_PAMP_response.xlsx", sheet = 3, col_names = TRUE), stringsAsFactors = F)
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

########## subset raw data
# Break up by sub-family
Toddalioideae <- subset(filtered_avg_PAMP_response, `Sub-family` == "Toddalioideae")
Aurantioideae <- subset(filtered_avg_PAMP_response, `Sub-family` == "Aurantioideae")


# Break up by tribe for Aurantioideae Subfamily
Triphasiinae<- subset(filtered_avg_PAMP_response, Tribe == "Triphasiinae")
Micromelinae <- subset(filtered_avg_PAMP_response, Tribe == "Micromelinae")
Merrilliinae <- subset(filtered_avg_PAMP_response, Tribe == "Merrilliinae")
Clauseninae <- subset(filtered_avg_PAMP_response, Tribe == "Clauseninae")
Citrinae <- subset(filtered_avg_PAMP_response, Tribe == "Citrinae")
Balsamocitrinae <- subset(filtered_avg_PAMP_response, Tribe == "Balsamocitrinae")



######################################################################
#reorder by botanical name
######################################################################

#reorder raw data
Toddalioideae <- Toddalioideae[str_order(Toddalioideae$`Botanical name`),]
Triphasiinae <- Triphasiinae[str_order(Triphasiinae$`Botanical name`),]
Micromelinae <- Micromelinae[str_order(Micromelinae$`Botanical name`),]
Merrilliinae <- Merrilliinae[str_order(Merrilliinae$`Botanical name`),]
Clauseninae <- Clauseninae[str_order(Clauseninae$`Botanical name`),]
Citrinae <- Citrinae[str_order(Citrinae$`Botanical name`),]
Balsamocitrinae <- Balsamocitrinae[str_order(Balsamocitrinae$`Botanical name`),]

#reorder alternate data
Toddalioideae_alt <- Toddalioideae_alt[str_order(Toddalioideae_alt$`Botanical name`),]
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
filtered_avg_PAMP_response <- rbind(Toddalioideae, Balsamocitrinae, Citrinae,
                                    Clauseninae, Merrilliinae, Micromelinae, Triphasiinae)

melted_filtered_avg_PAMP_responses <- as.matrix(filtered_avg_PAMP_response[,5:8])


#push back together alternate data
alternate_maping_data <- rbind(Toddalioideae_alt, Balsamocitrinae_alt, Citrinae_alt, 
                               Clauseninae_alt, Merrilliinae_alt, Micromelinae_alt, Triphasiinae_alt)


melted_alternate_maping_data <- as.matrix(alternate_maping_data[,5:8])





