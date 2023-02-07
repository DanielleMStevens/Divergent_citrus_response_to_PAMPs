#-----------------------------------------------------------------------------------------------
# Coaker Lab - Plant Pathology Department UC Davis
# Author: Danielle M. Stevens
# Last Updated: 07/13/2022
# Script Purpose: Plotting ROS data from diverse cirtus to catagotize immune response to different MAMPs
# Inputs Necessary: Processed data from 02_Process_PAMP_responses.R
# Outputs: Figure 2A
#-----------------------------------------------------------------------------------------------


############################################################
# Plot figures - Figure 2A
############################################################

colnames(filtered_avg_PAMP_response) <- c("Botanical name (CRC numbers in parenthesis)","Sub-family","Sub-tribe","Common name",
                                          "Chitin","flg22","csp22","CLas csp22")

# reshape data for plotting ros to restablish high, medium, and low
ros_strength_cutoff <- reshape2::melt(filtered_avg_PAMP_response, idvar = c("Botanical name (CRC numbers in parenthesis)","Sub-tribe"))

#remove clas csp22 data from this analysis
ros_strength_cutoff <- subset(ros_strength_cutoff, ros_strength_cutoff$variable != "CLas csp22")

# remove 0 points and establish quartile cut offs
ros_strength_cutoff <- subset(ros_strength_cutoff, ros_strength_cutoff$value != 0)


quartile_comparisons <- data.frame("Chitin" = quantile(subset(ros_strength_cutoff, ros_strength_cutoff$variable == "Chitin")[[6]]))
quartile_comparisons <- cbind(quartile_comparisons, 
                              data.frame("flg22" = quantile(subset(ros_strength_cutoff, ros_strength_cutoff$variable == "flg22")[[6]])),
                              data.frame("csp22" = quantile(subset(ros_strength_cutoff, ros_strength_cutoff$variable == "csp22")[[6]]))
                              )



# labeled examples for cuttoffs - chitin
examples_chitin <- subset(ros_strength_cutoff, ros_strength_cutoff$variable == "Chitin") 
examples_chitin <- examples_chitin[grepl(paste("Trifoliate","Chevalier",sep="|"), examples_chitin$`Common name`),]

examples_flg22 <- subset(ros_strength_cutoff, ros_strength_cutoff$variable == "flg22") 
examples_flg22 <- examples_flg22[examples_flg22$`Common name` %in% c("'Tango' mandarin", "Swamp orange","'King' tangor"),]

examples_csp22 <- subset(ros_strength_cutoff, ros_strength_cutoff$variable == "csp22") 
examples_csp22 <- examples_csp22[examples_csp22$`Common name` %in% c("Sydney hybrid","Horsewood"),]


#filter 
examples <- rbind(examples_chitin,examples_flg22,examples_csp22)

# reoder plot to match remaining figures in Figure 2
ros_strength_cutoff$variable <- factor(ros_strength_cutoff$variable, levels = c("csp22","flg22","Chitin"))


#---------- To make Figure 2A--------------------------------------------------------------------

# Plot cut-off plot for Figure 2
ggplot(ros_strength_cutoff, aes(x = variable, y = value)) +
  
  geom_quasirandom(aes(fill = `Sub-tribe`), size = 2.2, shape = 21, width = 0.2, stroke = 0.5, colour = "grey18") +
  
  geom_label_repel(data = examples, aes(label = `Common name`, color = `Sub-tribe`),
                   hjust=0, box.padding = unit(0.25, "lines"), segment.color="grey18",
                   size=4, segment.size=0.6, nudge_x=0.4, nudge_y = 0.15, direction="y") +
  
  scale_fill_manual(values = Tribe_colors) +
  scale_color_manual(values = Tribe_colors) +
  
  my_ggplot_theme +
  theme(legend.position = "none",
        axis.text.y = element_text(color = "black", size = 14),
        axis.text.x = element_text(color = "black", size = 14),
        axis.title.y = element_text(color = "black", size = 16),
        axis.title.x = element_text(color = "black", size = 16)) +
  xlab("\nMAMP") +
  
  #chitin quartiles - size of segment was manually altered (by + and - values)
  annotate("rect", xmin = 0.7, xmax = 1.2, ymin = quartile_comparisons$Chitin[2] - 50, ymax = quartile_comparisons$Chitin[2] + 50, fill = "grey42", alpha = 0.5) +
  annotate("rect", xmin = 0.7, xmax = 1.2, ymin = quartile_comparisons$Chitin[4] - 700, ymax = quartile_comparisons$Chitin[4] + 700, fill = "grey42", alpha = 0.5) +
  
  #flg22 quartiles - size of segment was manually altered (by + and - values)
  annotate("rect", xmin = 1.7, xmax = 2.2, ymin = quartile_comparisons$flg22[2] - 15, ymax = quartile_comparisons$flg22[2] + 15, fill = "grey42", alpha = 0.5) +
  annotate("rect", xmin = 1.7, xmax = 2.2, ymin = quartile_comparisons$flg22[4] - 165, ymax = quartile_comparisons$flg22[4] + 165, fill = "grey42", alpha = 0.5) +
  
  #csp22 quartiles - size of segment was manually altered (by + and - values)
  annotate("rect", xmin = 2.7, xmax = 3.2, ymin = quartile_comparisons$csp22[2] - 55, ymax = quartile_comparisons$csp22[2] + 55, fill = "grey42", alpha = 0.5) +
  annotate("rect", xmin = 2.7, xmax = 3.2, ymin = quartile_comparisons$csp22[4] - 500, ymax = quartile_comparisons$csp22[4] + 500, fill = "grey42", alpha = 0.5) +
  
  
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                name = expression(paste(log[10], " (Max RLU)")),
                labels = trans_format('log10', math_format(.x))) +

  
  coord_flip()



#---------- To make final figure:-----------------------------------------------------------------------------------------
# use export button to export -> don't use pdf then dev off as issues with exporting image with font arise
# path to save image -> /Final_Figures/ROS_Strength_Cuttoff.pdf
# save the following image size:width = 3.5, height = 6.5
# import pdf into vector based editing software such as inkscpae or adobe illustrator, Make cuttoff blocks same px width
# Color outline of boxes for point as black





# weird error can't fix
#quartile_comparisons <- ros_strength_cutoff %>% group_by(variable) %>% summarize("0%" = quantile(value, 0),
#                                                                                 "25%" = quantile(value, 0.25),
#                                                                                "50%" = quantile(value, 0.5),
#                                                                                 "75%" = quantile(value, 0.75),
#                                                                                 "100%" = quantile(value, 1))






############################################################
# Plot figures - Supplemental Figure
############################################################

PAMP_response_individual_values <- as.data.frame(readxl::read_excel("./Raw_ROS_files/Summary_of_PAMP_response.xlsx", sheet = 5, col_names = TRUE), stringsAsFactors = F)
colnames(PAMP_response_individual_values) <- c("Botanical name","Tribe","Sub-tribe","Common name","Chitin - R1","Chitin - R2",
                                               "Chitin - R3","Chitin - R4","Chitin - R5","Chitin - R6","Chitin - R7","Chitin - R8",
                                               "Chitin - R9","Chitin - R10","Chitin - R11",
                                               "Flg22 - R1","Flg22 - R2","Flg22 - R3","Flg22 - R4",
                                               "Flg22 - R5","Flg22 - R6","Csp22 - R1","Csp22 - R2",
                                               "Csp22 - R3","Csp22 - R4","Csp22 - R5","Csp22 - R6",
                                               "Csp22 - R7","Csp22 - R8","Csp22 - R9","Csp22 - R10")



#collect row name information annotation - first common name + then botanical name (if no common name)
row_names_to_apply <- PAMP_response_individual_values$`Common name`
for (i in 1:length(row_names_to_apply)){
  if(is.na(row_names_to_apply[i]) == TRUE){
    row_names_to_apply[i] <- PAMP_response_individual_values$`Botanical name`[i]
  }
}
PAMP_response_individual_values["Row names"] <- row_names_to_apply


# reshape data
PAMP_response_individual_values <- reshape2::melt(PAMP_response_individual_values[,c(2,3,5:32)], id.vars = c("Row names","Tribe","Sub-tribe"))
PAMP_response_individual_values$variable <- as.character(PAMP_response_individual_values$variable)
PAMP_response_individual_values$value <- as.numeric(PAMP_response_individual_values$value)


# replace R1,2,3... into rep counts on datasheet
for (i in 1:nrow(PAMP_response_individual_values)){
  if(grepl("Chitin", PAMP_response_individual_values$variable[i]) == TRUE){
    PAMP_response_individual_values$variable[i] <- gsub("Chitin - R\\d+","Chitin", PAMP_response_individual_values$variable[i])
  }
  if(grepl("Flg22", PAMP_response_individual_values$variable[i]) == TRUE){
    PAMP_response_individual_values$variable[i] <- gsub("Flg22 - R\\d+", "flg22", PAMP_response_individual_values$variable[i])
  }
  if(grepl("Csp22", PAMP_response_individual_values$variable[i]) == TRUE){
    PAMP_response_individual_values$variable[i] <- gsub("Csp22 - R\\d+", "csp22", PAMP_response_individual_values$variable[i])
  }
}

#split each datafrmae
chitin_individual_values <- subset(PAMP_response_individual_values, PAMP_response_individual_values$variable == "Chitin")
flg22_individual_values <- subset(PAMP_response_individual_values, PAMP_response_individual_values$variable == "flg22")
csp22_individual_values <- subset(PAMP_response_individual_values, PAMP_response_individual_values$variable == "csp22")



# plot as split bar chart
(ggplot(chitin_individual_values[complete.cases(chitin_individual_values),], 
        aes (x = reorder(`Row names`, value, FUN = median), y = value, fill = `Sub-tribe`)) + 
  geom_boxplot(outlier.size = 0, alpha = 0.8) +
  geom_jitter(alpha = 0.4, size = 0.8) +
  my_ggplot_theme +
  scale_y_continuous(trans = scales::pseudo_log_trans(base = 10),
                     breaks = c(0, 10^(1:6)),
                     labels = math_format(.x)) +
  coord_flip() +
  ggtitle("Chitin") +
  xlab("") +
  ylab("Avg. Max RLU") +
  scale_fill_manual(values = Tribe_colors) +
  theme(title = element_text(face = "bold", hjust = 0.5, family = "Arial"),
        axis.text.y = element_text(color = "black", size = 6.5, family = "Arial"),
        axis.text.x = element_text(color = "black", size = 8, angle = 45, hjust = 1, family = "Arial"),
        axis.title.x = element_text(color = "black", size = 10, family = "Arial"),
        panel.grid.major.x = element_line(colour="grey87", size = 0.5),
        legend.position = "none")) +

(ggplot(flg22_individual_values[complete.cases(flg22_individual_values),], 
        aes (x = reorder(`Row names`, value, FUN = median), y = value, fill = `Sub-tribe`)) + 
   geom_boxplot(outlier.size = 0, alpha = 0.8) +
   geom_jitter(alpha = 0.4, size = 0.8) +
   my_ggplot_theme +
   scale_y_continuous(trans = scales::pseudo_log_trans(base = 10),
                      breaks = c(0, 10^(1:6)),
                      labels = math_format(.x)) +
   coord_flip() +
   ggtitle("flg22") +
   xlab("") +
   ylab("Avg. Max RLU") +
   scale_fill_manual(values = Tribe_colors) +
   theme(title = element_text(face = "bold", hjust = 0.5, family = "Arial"),
         axis.text.y = element_text(color = "black", size = 6.5, family = "Arial"),
         axis.text.x = element_text(color = "black", size = 8, angle = 45, hjust = 1, family = "Arial"),
         axis.title.x = element_text(color = "black", size = 10, family = "Arial"),
         panel.grid.major.x = element_line(colour="grey87", size = 0.5),
         legend.position = "none")) +
   
(ggplot(csp22_individual_values[complete.cases(csp22_individual_values),], 
        aes (x = reorder(`Row names`, value, FUN = median), y = value, fill = `Sub-tribe`)) + 
    geom_boxplot(outlier.size = 0, alpha = 0.8) +
    geom_jitter(alpha = 0.4, size = 0.8) +
    my_ggplot_theme +
    scale_y_continuous(trans = scales::pseudo_log_trans(base = 10),
                      breaks = c(0, 10^(1:6)),
                      labels = math_format(.x)) +
   coord_flip() +
   ggtitle("csp22") +
   xlab("") +
   ylab("Avg. Max RLU") +
   scale_fill_manual(values = Tribe_colors) +
   theme(title = element_text(face = "bold", hjust = 0.5, family = "Arial"),
         axis.text.y = element_text(color = "black", size = 6.5, family = "Arial"),
         axis.text.x = element_text(color = "black", size = 9, angle = 45, hjust = 1, family = "Arial"),
         axis.title.x = element_text(color = "black", size = 10, family = "Arial"),
         panel.grid.major.x = element_line(colour="grey87", size = 0.5),
         legend.position = "none"))




