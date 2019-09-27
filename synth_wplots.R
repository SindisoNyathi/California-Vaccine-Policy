#*******************************************************************************************************#
#27/07/2018
#Sindiso Nyathi
#Goal: Plot the Weights of Covariate or Units.
#*******************************************************************************************************#

#*******************************************************************************************************#
synth_wplots <- function(plot_data, outcome_var, treated_unit, additional_label) {
  
  label_one <- colnames(plot_data)[1]
  label_two <- colnames(plot_data)[2]
  
  #Create the GG plots
  controls_plot <- ggplot(plot_data, aes(x = Name, y = Weight)) +
    geom_bar(stat = "Identity", fill = "steelblue4") +
    theme_bw() +
    labs(title = "Weights", subtitle = paste("Outcome: ", outcome_variable, ". Unit: ", treated_unit, sep = "")) +
    xlab("Name") +
    ylab("Weight") +
    theme(plot.title = element_text(hjust = 0.5, size = 22),
          legend.title = element_blank(), 
          axis.text.x = element_text(angle = 90, hjust = 1),
          plot.subtitle = element_text(hjust = 0.5, size = 18), 
          axis.text = element_text(size=16),
          axis.title.x = element_text(size = 18), 
          axis.title.y = element_text(size = 18), 
          legend.text = element_text(size=18))
  
  setEPS()
  postscript(paste("Plots/", additional_label, " Weights.", outcome_var, ".eps", sep = ""),  width = 15, height = 8)
  plot(controls_plot)
  dev.off()
}
####################################################################################################
#"There are seven words that will make a person love you. There are ten words that will break a strong 
#man's will"
#Patrick Rothfuss in "The Name of the Wind"
####################################################################################################

