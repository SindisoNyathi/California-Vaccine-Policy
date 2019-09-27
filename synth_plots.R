#*******************************************************************************************************#
#27/06/2019
#Sindiso Nyathi
#Goal: Plot the Placebo files for the Synthetic Controls. 
#*******************************************************************************************************#

#*******************************************************************************************************#
synth_plots <- function(plot_data, outcome_var, treated_unit, start_year, end_year, additional_label = "Null") {
  
  plot_data[,c(1:3)] <- as.data.frame(lapply(plot_data[,c(1:3)], as.numeric))
  plot_data2 <- plot_data
  plot_data2[,2] <- plot_data[, 2] - plot_data[, 3]
  plot_data2[,3] <- plot_data[, 3] - plot_data[, 3]
  
  colnames(plot_data2)[2] <- treated_unit
  colnames(plot_data)[2] <- treated_unit
  
  plot_data <- melt(plot_data, id.vars = "Year", direction = "long")
  plot_data2 <- melt(plot_data2, id.vars = "Year", direction = "long")
 
  min_range = 70
  max_range = 100
  
  if (isTRUE(max(plot_data[,3]) < 40)){
    min_range = -2.5
    max_range = 5
  }
  
  #Create the GG plots
  controls_plot <- ggplot(plot_data, aes(x = Year, y = value, colour = variable)) +
    geom_point(aes(colour = plot_data$variable), size = 1.75) +
    geom_line(aes(colour = plot_data$variable), size = 1.25) +
    theme_bw() +
    labs(title = "California vs. Synthetic Control", subtitle = paste("Outcome: ", outcome_variable, sep = "")) +
    xlab("Year") +
    ylab("Percentage (%)") +
    ylim(c(min_range, max_range)) +
    scale_x_discrete(limits = c(start_year:end_year)) +
    theme(plot.title = element_text(hjust = 0.5, size = 22),
          legend.title = element_blank(), 
          plot.subtitle = element_text(hjust = 0.5, size = 18), 
          axis.text = element_text(size=16),
          axis.title.x = element_text(size = 18), 
          axis.title.y = element_text(size = 18), 
          legend.text = element_text(size=18)) +
    geom_vline(xintercept = 2015, linetype = 2, color = "blue", size = 0.5)
  
  setEPS()
  postscript(paste("Plots/(", additional_label, ") Synthetic Control vs. Treated Unit (Raw) ", outcome_var, ".eps", sep = ""),  width = 15, height = 8)
  plot(controls_plot)
  dev.off()
  
  #Create the GG plots
  diff_plot <- ggplot(plot_data2, aes(x = Year, y = value, colour = variable)) +
    geom_point(aes(colour = plot_data2$variable), size = 1.25) +
    geom_line(aes(colour = plot_data2$variable), size = 1.25) +
    theme_bw() +
    labs(title = "California vs. Synthetic Control Difference", subtitle = paste("Outcome: ", outcome_variable, sep = "")) +
    xlab("Year") +
    ylab("Difference (%)") +
    ylim(c(-5,5)) +
    scale_x_discrete(limits = c(start_year:end_year)) +
    theme(plot.title = element_text(hjust = 0.5, size = 22),
          legend.title = element_blank(), 
          plot.subtitle = element_text(hjust = 0.5, size = 18), 
          axis.text = element_text(size=16),
          axis.title.x = element_text(size = 18), 
          axis.title.y = element_text(size = 18), 
          legend.text = element_text(size=18)) +
    geom_vline(xintercept = 2015, linetype = 2, color = "red", size = 0.5)
  
  setEPS()
  postscript(paste("Plots/(Main Analysis) Synthetic Control vs. Treated Unit (Diff)  ", outcome_var, ".eps", sep = ""),  width = 15, height = 8)
  plot(diff_plot)
  dev.off()
  
}




