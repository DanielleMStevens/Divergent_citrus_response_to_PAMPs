#-----------------------------------------------------------------------------------------------
# Coaker Lab - Plant Pathology Department UC Davis
# Author: Danielle M. Stevens
# Last Updated: 4/15/2020
# Script Purpose: Plotting ROS data from divergent cirtus responding to different PAMPS
# Inputs Necessary: 
# Outputs: 
#-----------------------------------------------------------------------------------------------


############################################################
# Plot figures - Figure 2A?
############################################################

colnames(filtered_avg_PAMP_response) <- c("Botanical name (CRC numbers in parenthesis)","Sub-family","Tribe","Common name",
                                          "Chitin","flg22","csp22","CLas csp22")

# reshape data for plotting ros to restablish high, medium, and low
ros_strength_cutoff <- reshape2::melt(filtered_avg_PAMP_response, idvar = c("Botanical name (CRC numbers in parenthesis)","Tribe"))

#remove clas csp22 data from this analysis
ros_strength_cutoff <- subset(ros_strength_cutoff, ros_strength_cutoff$variable != "CLas csp22")

# remove 0 points and establish quartile cut offs
ros_strength_cutoff <- subset(ros_strength_cutoff, ros_strength_cutoff$value != 0)
quartile_comparisons <- as.data.frame(ros_strength_cutoff %>% group_by(variable) %>% summarize("0%" = quantile(value, 0),
                                                                                               "25%" = quantile(value, 0.25),
                                                                                               "50%" = quantile(value, 0.5),
                                                                                               "75%" = quantile(value, 0.75),
                                                                                               "100%" = quantile(value, 1)))


# labeled examples for cuttoffs - chitin
examples_chitin <- subset(ros_strength_cutoff, ros_strength_cutoff$variable == "Chitin") 
examples_chitin <- examples_chitin[grepl(paste("Trifoliate","Chevalier",sep="|"), examples_chitin$`Common name`),]

examples_flg22 <- subset(ros_strength_cutoff, ros_strength_cutoff$variable == "flg22") 
examples_flg22 <- examples_flg22[examples_flg22$`Common name` %in% c("Tango mandarin", "Swamp orange","King tangor"),]

examples_csp22 <- subset(ros_strength_cutoff, ros_strength_cutoff$variable == "csp22") 
examples_csp22 <- examples_csp22[examples_csp22$`Common name` %in% c("Sydney hybrid","Horsewood"),]


#filter 
examples <- rbind(examples_chitin,examples_flg22,examples_csp22)

# reoder plot to match remaining figures in Figure 2
ros_strength_cutoff$variable <- factor(ros_strength_cutoff$variable, levels = c("csp22","flg22","Chitin"))


# Plot cut-off plot for Figure 2
ggplot(ros_strength_cutoff, aes(x = variable, y = value)) +
  
  geom_quasirandom(aes(fill = Tribe), size = 2.2, shape = 21, width = 0.2, stroke = 0.5, colour = "grey18") +
  
  geom_label_repel(data = examples, aes(label = `Common name`, color = Tribe),
                   hjust=0, box.padding = unit(0.25, "lines"), segment.color="grey18",
                   size=4, segment.size=0.6, nudge_x=0.4, nudge_y = 0.15, direction="y") +
  
  scale_fill_manual(values = Tribe_colors) +
  scale_color_manual(values = Tribe_colors) +
  
  my_ggplot_theme +
  theme(legend.position = "none") +
  xlab("\nMAMP") +
  
  #chitin quartiles - size of segment was manually altered (by + and - values)
  annotate("rect", xmin = 0.7, xmax = 1.2, ymin = quartile_comparisons$`25%`[1] - 50, ymax = quartile_comparisons$`25%`[1] + 50, fill = "grey42", alpha = 0.5) +
  annotate("rect", xmin = 0.7, xmax = 1.2, ymin = quartile_comparisons$`75%`[1] - 700, ymax = quartile_comparisons$`75%`[1] + 700, fill = "grey42", alpha = 0.5) +
  
  #flg22 quartiles - size of segment was manually altered (by + and - values)
  annotate("rect", xmin = 1.7, xmax = 2.2, ymin = quartile_comparisons$`25%`[2] - 15, ymax = quartile_comparisons$`25%`[2] + 15, fill = "grey42", alpha = 0.5) +
  annotate("rect", xmin = 1.7, xmax = 2.2, ymin = quartile_comparisons$`75%`[2] - 165, ymax = quartile_comparisons$`75%`[2] + 165, fill = "grey42", alpha = 0.5) +
  
  #csp22 quartiles - size of segment was manually altered (by + and - values)
  annotate("rect", xmin = 2.7, xmax = 3.2, ymin = quartile_comparisons$`25%`[3] - 55, ymax = quartile_comparisons$`25%`[3] + 55, fill = "grey42", alpha = 0.5) +
  annotate("rect", xmin = 2.7, xmax = 3.2, ymin = quartile_comparisons$`75%`[3] - 500, ymax = quartile_comparisons$`75%`[3] + 500, fill = "grey42", alpha = 0.5) +
  
  
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                name = expression(paste(log[10], " (Average Max RLU)")),
                labels = trans_format('log10', math_format(.x))) +

  
  coord_flip()







#scale_y_continuous(trans = "log10") +
#scale_y_log10() +
#scale_y_log10(labels = function(x) x/10, name = "Average Max RLU") +
#scale_y_log10(breaks = scales::log_breaks(n = 5)) +






