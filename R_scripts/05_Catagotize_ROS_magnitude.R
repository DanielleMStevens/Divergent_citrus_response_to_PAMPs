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





