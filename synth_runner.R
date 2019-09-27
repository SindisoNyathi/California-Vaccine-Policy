#*******************************************************************************************************#
#27/06/2019
#Sindiso Nyathi
#Goal: Run the placebo tests for the synthetic control analysis.
#Placebo tests essentially assign treatment to untreated units. We can recalculate the effect sizes
#for these. This would effectively give us an idea of what natural variation in the outcome would look 
#like.
#*******************************************************************************************************#
synth_runner <- function(vaccination_data, outcome_variable, 
                         intervention_year, covariate_combination, 
                         start_year, end_year, additional_label) {
  
  #The column number for our variable.
  outcome_number <- which(colnames(vaccination_data) == outcome_variable)
  
  #For the variable we are currently running.
  this_variable_column <- which(colnames(vaccination_data) == outcome_variable)
  
  units_final <- as.data.frame(matrix(ncol = 4, nrow = 0))
  colnames(units_final) <- c("Unit", "Year", "Treated", "Synthetic")
  
  #Run data_prep and synth for each state in the final NONA dataframe.
  for (i in 1:length(unique(vaccination_data$Units))) {
    
    #Some of the states produce an error, so we wil use try catch to take note of those states
    #but continue with the analysis anyway.
    no_states <- length(unique(vaccination_data$Units))
    
    #Which state are we running?
    this_unit <- as.character(unique(vaccination_data$Units[i]))
    
    #Print progress monitor.
    print(paste("Current State is ", this_unit, "  Processing . . . ", sep = ""))
    
    #Run the synthetic control with the second unit.
    unit_out <- synth_unit(vaccination_data, this_unit, #Treated unit is reassigned to the current unit.
                           outcome_variable, intervention_year,
                           covariate_combination, start_year, end_year)
    
    #Check if a SC was synthesized. For a few (1 or 2) of the placebo tests, the synthetic control fails.
    if(is.null(unit_out)) {
      print(paste(this_unit, " Not Synthesized", sep = ""))
      next}
    
    #Format the output.
    unit_values <- as.data.frame(unit_out[[1]])
    unit_values <- cbind(this_unit, unit_values)
    colnames(unit_values) <- c("Unit", "Year", "Treated", "Synthetic")
    units_final <- rbind(units_final, unit_values)
    
    #Print progress monitor.
    print(paste("State: ", this_unit, " Complete. Moving on.", sep = ""))
    
  }
  
  #Done with the individual plaecbo analyses.
  
  #Now create the files we need for the plots for the placebo using the weights matrix. 
  
  #Now we have synth values, which is basically, plabebo controls. Save this file.
  write.csv(units_final, paste("Data Files/Placebo Synthetic Controls. ", outcome_variable, ".csv", sep = ""))
  
  #Plot the placebo plot. 
  plot_data <- units_final
  
  #We are interested in the difference between the synthetic control and the placebo treated state.
  plot_data[,5] <- plot_data[,3] - plot_data[, 4]
  colnames(plot_data)[5] <- "Gap"
  
  #Set limits of plot for various possible outcomes.
  if (outcome_variable == "MMR") {ylim = c(-10, 10)}
  else if (outcome_variable == "Medical") {ylim = c(-1.5, 1.5)}
  else {ylim = c(-4, 4)}
  
  #Filter controls
  cali <- subset(plot_data, Unit == "California")
  
  #Plot the Output.
  controls_plot <- ggplot(plot_data, aes(x = Year, y = Gap, group = Unit)) +
    geom_point(color = "grey70") +
    geom_line(color = "grey70") +
    theme_bw() +
    labs(title = "Placebo Analysis", subtitle = paste("Outcome: ", outcome_variable, sep = "")) +
    xlab("Year") +
    ylab("Percentage (%)") +
    ylim(ylim) +
    scale_x_discrete(limits = c(start_year:end_year)) +
    theme(plot.title = element_text(hjust = 0.5, size = 22),
          legend.title = element_blank(), 
          plot.subtitle = element_text(hjust = 0.5, size = 18), 
          axis.text = element_text(size=16),
          axis.title.x = element_text(size = 18), 
          axis.title.y = element_text(size = 18), 
          legend.text = element_text(size=18)) +
    geom_vline(xintercept = 2015, linetype = 2, color = "red", size = 0.25) +
    geom_line(aes(Year, cali[,5], colour = Unit), data = cali, colour = "steelblue", size = 1.25) +
    geom_point(aes(Year, cali[,5], colour = Unit), data = cali, colour = alpha("steelblue", 1))
  
  #Save the output plot.
  setEPS()
  postscript(paste("Plots/(Main Analysis). Placebo Plots Gaps. ", outcome_variable, ".eps", sep = ""),  width = 15, height = 8)
  plot(controls_plot)
  dev.off()
  
  #Return the placebo tests output file
  return(units_final)
  
  #The END. 
}

####################################################################################################
#"It was said that the gods favored fools because they were entertaining to watch"
#N. K. Jemisin in "The Killing Moon"
####################################################################################################
