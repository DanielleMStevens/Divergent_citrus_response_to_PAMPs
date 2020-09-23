#-----------------------------------------------------------------------------------------------
# Coaker Lab - Plant Pathology Department UC Davis
# Author: Danielle M. Stevens
# Last Updated: 4/15/2020
# Script Purpose: Plotting ROS data from divergent cirtus responding to different PAMPS
# Inputs Necessary: 
# Outputs: 
#-----------------------------------------------------------------------------------------------


######################################################################
#row name settings
######################################################################

#collect row nameinformation annotation - botanical name ONLY
row.names(melted_filtered_avg_PAMP_responses) <- filtered_avg_PAMP_response$`Botanical name`


#collect row nameinformation annotation - common name + botanical name
#row_names_to_apply <- filtered_avg_PAMP_response$`Common name`
#for (i in 1:length(row_names_to_apply)){
#  if(row_names_to_apply[i] == "0"){
#    row_names_to_apply[i] <- filtered_avg_PAMP_response$`Botanical name`[i]
#  }
#}
#row.names(melted_filtered_avg_PAMP_responses) <- row_names_to_apply


######################################################################
#plot indivisdial values - boxplot? for each tribe data set
######################################################################


box_plot_tribe_plot <- function(df_in){
  if(length(unique(df_in$`Botanical name`)) > 1){
    hold_plot <- ggplot(df_in, aes(x = variable, y = value, fill = Tribe)) +
      geom_boxplot(alpha = 0.5) +
      geom_jitter(size = 0.8, alpha = 0.8, width= 0.2, colour = "black") +
      my_ggplot_theme +
      ylab("Max RLUs") +
      xlab("") +
      
      #scale_y_continuous(breaks = c(1,200, 500, 1000, 5000, 20000, 50000, 100000), labels = c(0,200, 500, 1000, 5000, 20000, 50000, 100000),limits = c(1,100000)) +
      scale_y_log10(limits = c(1,100000), breaks =c(10,1000,10000,100000), labels = c(10,1000,10000,100000)) +
      #breaks = c(1, 1000, 10000, 100000), labels = c(0,1000,10000,100000)
      #scale_y_log10(breaks = c(1,200, 500, 1000, 5000, 20000, 50000, 100000), labels = c(0,200, 500, 1000, 5000, 20000, 50000, 100000),limits = c(1,100000)) +
      theme(legend.position = "none")+
      scale_fill_manual("Tribe", values = Tribe_colors) +
      scale_colour_manual("Tribe", values = Tribe_colors) +
      labs(subtitle = c(as.character(unique(df_in$Tribe))))
  }
  if(length(unique(df_in$`Botanical name`)) < 2){
    #max_y_value <- max(df_in$value)
    hold_plot <- ggplot(df_in, aes(x = variable, y = value, fill = Tribe)) +
      geom_point(aes(x = variable), shape = 21, size = 1,  position = position_jitterdodge(jitter.width = 0.5, jitter.height=0.4, dodge.width=0.9))+
      #geom_jitter(size = 0.8, alpha = 0.8, width= 0.2, colour = "black") +
      my_ggplot_theme +
      ylab("Max RLUs") +
      xlab("") +
      scale_y_log10(breaks = c(0,200, 500, 1000, 5000, 20000, 50000, 100000), labels = c(0,200, 500, 1000, 5000, 20000, 50000, 100000)) +
      theme(legend.position = "none")+
      scale_fill_manual("Tribe", values = Tribe_colors) +
      scale_colour_manual("Tribe", values = Tribe_colors) +
      labs(subtitle = c(as.character(unique(df_in$Tribe))))
  }
  return(hold_plot)
}

# Toddalioideae Subfamily 
Toddalioideae_melt <- reshape2::melt(Toddalioideae)
Toddalioideae_box <- box_plot_tribe_plot(Toddalioideae_melt)

# Rutoideae Subfamily
Rutoideae_melt <- reshape2::melt(Rutoideae)
Rutoideae_box <- box_plot_tribe_plot(Rutoideae_melt)

# Aurantioideae Subfamily
Aurantioideae_melt <- reshape2::melt(Aurantioideae)
Aurantioideae_box <- box_plot_tribe_plot(Aurantioideae_melt)


# Break up Aurantioideae Subfamily into Tribes
Balsamocitrinae_melt <- reshape2::melt(Balsamocitrinae)
Balsamocitrinae_box <- box_plot_tribe_plot(Balsamocitrinae_melt)

Citrinae_melt <- reshape2::melt(Citrinae)
Citrinae_box <- box_plot_tribe_plot(Citrinae_melt)

Clauseninae_melt <- reshape2::melt(Clauseninae)
Clauseninae_box <- box_plot_tribe_plot(Clauseninae_melt)

Merrilliinae_melt <- reshape2::melt(Merrilliinae)
Merrilliinae_box <- box_plot_tribe_plot(Merrilliinae_melt)

Micromelinae_melt <- reshape2::melt(Micromelinae)
Micromelinae_box <- box_plot_tribe_plot(Micromelinae_melt)

Triphasiinae_melt <- reshape2::melt(Triphasiinae)
Triphasiinae_box <-box_plot_tribe_plot(Triphasiinae_melt)


#png("Comparison_of_Max_RLUs_across_Tribes.png", height = 8, width = 3.5, units = "in", res = 800)
ggpubr::ggarrange(Toddalioideae_box, Balsamocitrinae_box, Citrinae_box, 
          Clauseninae_box, Merrilliinae_box, Micromelinae_box, Triphasiinae_box, 
          ncol = 1, nrow = 8, common.legend = TRUE, legend = "right")
#dev.off()




png("Comparison_of_Max_RLUs_across_Tribes_box_only.png", height = 9, width = 3, units = "in", res = 1000)
ggpubr::ggarrange(Toddalioideae_box, Balsamocitrinae_box, Citrinae_box, Clauseninae_box, Triphasiinae_box, 
                  ncol = 1, nrow = 5, legend = "none")

dev.off()

######################################################################
#plot degress of variation
######################################################################


degree_of_variation_bargraph <- function(data_frame_in){
  reset_data <- reshape2::melt(data_frame_in, id=c("Botanical name",'Sub-family','Tribe','Common name'))
  reset_data$value <- as.factor(reset_data$value)
  ggplot(reset_data, aes(x= variable, fill = value))+
    geom_bar(position = "fill")+
    xlab("")+
    my_ggplot_theme +
    scale_y_continuous(breaks = c(0.5,1))+
    scale_fill_manual("Response", values= color_code_samples_col) 
    #labs(subtitle = c(as.character(unique(reset_data$Tribe))))
}


Toddalioideae_var <- degree_of_variation_bargraph(Toddalioideae_alt)
Rutoideae_var <- degree_of_variation_bargraph(Rutoideae_alt)
Balsamocitrinae_var <- degree_of_variation_bargraph(Balsamocitrinae_alt)
Citrinae_var <- degree_of_variation_bargraph(Citrinae_alt) 
Clauseninae_var <- degree_of_variation_bargraph(Clauseninae_alt)
Merrilliinae_var <- degree_of_variation_bargraph(Merrilliinae_alt)
Micromelinae_var <- degree_of_variation_bargraph(Micromelinae_alt)
Triphasiinae_var <- degree_of_variation_bargraph(Triphasiinae_alt)


png("Degress_of_variation_Comparison_of_Max_RLUs_across_Tribes.png", height = 8, width = 3.5, units = "in", res = 800)

ggarrange(Toddalioideae_var, Rutoideae_var, Balsamocitrinae_var, Citrinae_var,
          Clauseninae_var, Merrilliinae_var, Micromelinae_var, Triphasiinae_var,
          ncol = 1, nrow = 8, common.legend = TRUE, legend = "bottom")

dev.off()

######################################################################
# combine the top two plots
######################################################################


png("combine_individual_data_with_degree.png", height = 11, width = 6, units = "in", res = 1000)


ggpubr::ggarrange(Toddalioideae_box, Toddalioideae_var,
          Rutoideae_box, Rutoideae_var,
          Balsamocitrinae_box, Balsamocitrinae_var,
          Citrinae_box, Citrinae_var,
          Clauseninae_box, Clauseninae_var,
          Merrilliinae_box, Merrilliinae_var,
          Micromelinae_box, Micromelinae_var,
          Triphasiinae_box, Triphasiinae_var,
          ncol = 2, nrow = 8, common.legend = TRUE, legend = "bottom")


dev.off()

######################################################################
#plot degress of variation - as facets
######################################################################

reset_data <- reshape2::melt(alternate_maping_data, id=c("Botanical name",'Sub-family','Tribe','Common name'))
reset_data$value <- as.factor(reset_data$value)

png("Degress_of_variation_Comparison_of_Max_RLUs_across_Tribes_as_facets.png", height = 9, width = 3.5, units = "in", res = 800)

ggplot(reset_data[reset_data$Tribe %in% Tribe_list,], aes(x= variable, fill = value)) +
  geom_bar(position = "fill") +
  facet_grid(Tribe ~ .) +
  xlab("") +
  ylab("") +
  scale_y_continuous(breaks = c(0.5,1)) +
  my_ggplot_theme +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2, hjust = 0.95),
        legend.position = "none") +
  scale_fill_manual("Response", values= color_code_samples_col) 

dev.off()


png("Degress_of_variation_Comparison_of_Max_RLUs_across_Subfamily_as_facets.png", height = 4, width = 3.5, units = "in", res = 800)

reset_data$subfamily_f = factor(reset_data$`Sub-family`, levels=c('Toddalioideae','Rutoideae','Aurantioideae'))

ggplot(reset_data, aes(x= variable, fill = value)) +
  geom_bar(position = "fill") +
  facet_grid(subfamily_f ~ .) +
  xlab("") +
  ylab("Proportion of Response\n") +
  scale_y_continuous(breaks = c(0.5,1)) +
  my_ggplot_theme +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2, hjust = 0.95),
        legend.title = element_text(face = "bold"),
        legend.position = "bottom", 
        legend.justification = "left",
        legend.direction = "vertical") +
        scale_fill_manual("Response", values= color_code_samples_col) 

dev.off()


######################################################################
#plot indivisdial values - boxplot? clas data + only
######################################################################


box_clas_plot <- function(df_in){
    ggplot(df_in, aes(x = variable, y = value, fill = Tribe)) +
      geom_boxplot(alpha = 0.5) +
      geom_point(aes(x = variable), shape = 21, size = 1,  position = position_jitterdodge(jitter.width = 0.5, jitter.height=0.4, dodge.width=0.9)) +
      my_ggplot_theme +
      ylab("Max RLUs") +
      xlab("") +
      scale_y_log10() +
      scale_fill_manual("Tribe", values = Tribe_colors) +
      scale_colour_manual("Tribe", values = Tribe_colors)
}
  


clascsp22_data <- subset(filtered_avg_PAMP_response, filtered_avg_PAMP_response$`CLas csp22` > 0)
clascsp22_data_melt <- reshape2::melt(clascsp22_data)


png("Clas_raw_data.png", height = 2.5, width = 6, units = "in", res = 800)

box_clas_plot(clascsp22_data_melt)

dev.off()


######################################################################
#plot degress of variation - inidividual data as violin plots 
######################################################################


retreive_n <- function(x){
  return(c(y = max(x)*1.2, label = length(x))) 
  # experiment with the multiplier to find the perfect position
}

#    


plot_by_PAMP <- function(df_in){
  ggplot(df_in, aes(x = Tribe, y = value, fill = Tribe)) +
    geom_boxplot(alpha = 0.5) +
    geom_point(aes(x = Tribe), shape = 21, size = 1,  position = position_jitterdodge(jitter.width = 0.8, jitter.height=0.4, dodge.width=0.9)) +
    my_ggplot_theme +
    #stat_summary(fun.data = retreive_n, geom ="text", fun.y = max, position = position_dodge(width = 0.75)) +
    facet_grid(variable ~ .) +
    ylab("Average Max RLUs\n") +
    xlab("") +
    scale_y_log10(limits = c(1,100000), breaks =c(10,1000,10000,100000), labels = c(10,1000,10000,100000)) +
    scale_fill_manual("Tribe", values = Tribe_colors) 
}



Aurantioideae_subset <- subset(filtered_avg_PAMP_response, filtered_avg_PAMP_response$`Sub-family` == "Aurantioideae")
Aurantioideae_subset <- Aurantioideae_subset[!grepl("Merrilliinae", Aurantioideae_subset$Tribe),]
Aurantioideae_subset <- Aurantioideae_subset[!grepl("Micromelinae", Aurantioideae_subset$Tribe),]

Aurantioideae_subset <- reshape2::melt(Aurantioideae_subset)


png("Plot_raw_data_by_PAMP.png", height = 9, width = 4.5, units = "in", res = 1000)
plot_by_PAMP(Aurantioideae_subset) + theme(axis.text.x = element_text(angle = 90), legend.position = "left")
dev.off()

