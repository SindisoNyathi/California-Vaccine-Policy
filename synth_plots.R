#*******************************************************************************************************#
#06/09/2018
#Sindiso Nyathi
#Goal: Plot the Placebo files for the Synthetic Controls. 
#
#
#*******************************************************************************************************#

#*******************************************************************************************************#
#Read in teh required files. 

synth_plots <- function(plot_data, rmspe) {
  
  plot_data2 <- plot_data
  plot_data2[,2] <- plot_data[,3] - plot_data[, 2]
  plot_data2[,3] <- plot_data[,3] - plot_data[, 3]
  
  plot_data <- melt(plot_data, id.vars = "Year", direction = "long")
  plot_data2 <- melt(plot_data2, id.vars = "Year", direction = "long")
 
  #Create the GG plots
  controls_plot <- ggplot(plot_data, aes(x = Year, y = value, group = variable)) +
    geom_point(aes(colour = plot_data$variable)) +
    geom_line(aes(colour = plot_data$variable)) +
    theme_bw() +
    ggtitle("California vs.\n Synthetic Control") +
    xlab("Year") +
    ylab("Percentage (%)") +
    ylim(c(80,100)) +
    scale_x_discrete(limits = c(2011, 2012, 2013, 2014, 2015, 2016, 2017)) +
    theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank()) +
    theme(axis.text=element_text(size=22),
          plot.title = element_text(size = 32), 
          axis.title.x = element_text(size = 32), 
          axis.title.y = element_text(size = 32), 
          legend.text=element_text(size=32)) +
    geom_vline(xintercept = 2015, linetype = 2, color = "red", size = 0.25)
    
  jpeg("Synthetic Control vs. Treated Unit (Raw).jpg", width = 1500, height = 1000)
  plot(controls_plot)
  dev.off()
  
  #Create the GG plots
  diff_plot <- ggplot(plot_data2, aes(x = Year, y = value, group = variable)) +
    geom_point(aes(colour = plot_data2$variable)) +
    geom_line(aes(colour = plot_data2$variable)) +
    theme_bw() +
    ggtitle("California vs. \n Synthetic Control") +
    xlab("Year") +
    ylab("Percentage Difference (%)") +
    ylim(c(-5,5)) +
    scale_x_discrete(limits = c(2011, 2012, 2013, 2014, 2015, 2016, 2017)) +
    theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank()) +
    theme(axis.text=element_text(size=22),
          plot.title = element_text(size = 32), 
          axis.title.x = element_text(size = 32), 
          axis.title.y = element_text(size = 32), 
          legend.text=element_text(size=32)) +
    geom_vline(xintercept = 2016, linetype = 2, color = "red", size = 0.25)
  
  jpeg("Synthetic Control vs. Treated Unit (Diff).jpg", width = 1500, height = 1000)
  plot(diff_plot)
  dev.off()
  
}


