#*******************************************************************************************************#
#27/06/2019
#Sindiso Nyathi
#Goal: Plot and save the Percent changes.
#*******************************************************************************************************#
synth_perchanges <- function(placebo, outcome_variable){
  
  #Format the placebo files
  placebo$No <- 1:nrow(placebo)
  placebo <- placebo[,c(5, 2:4)]
  placebo[,6] <- placebo[, 4] - placebo[, 5]
  placebo <- placebo[,c(-1, -4, -5)]
  library(reshape2)
  library(ggplot2)
  
  #Calculate the effect sizes.
  placebo2 <- reshape(placebo, idvar = c("Unit"), timevar = "Year", direction = "wide")
  placebo <- placebo2[,c(-2, -3, -4)]
  placebo[,6] <- (placebo[,2] + placebo[,3])/2
  placebo[,7] <- (placebo[,4] + placebo[,5])/2
  
  placebo <- placebo[,c(1, 6, 7)]
  placebo[,4] <- placebo[,3] - placebo[,2] #Post - Pre
  colnames(placebo) <- c("Unit", "Pre", "Post", "Dif")
  
  placebo <- placebo[order(placebo$Dif),]
  
  #Create the plot.
  placebo_plot <- ggplot(placebo, aes(x = reorder(Unit, -Dif), y = Dif )) +
    geom_bar(stat = "identity", fill = "steelblue4") +
    theme_bw() +
    labs(title = "Pre-Post Policy Difference", subtitle = paste("Outcome: ", outcome_variable, sep = "")) +
    xlab("State") +
    ylab("Difference (%)") +
    #ylim(c(0, 0.75)) + 
    theme(plot.title = element_text(hjust = 0.5, size = 22),
          legend.title = element_blank(), 
          plot.subtitle = element_text(hjust = 0.5, size = 18), 
          axis.text = element_text(size=16),
          axis.text.x = element_text(angle=90, hjust = 1),
          axis.title.x = element_text(size = 18), 
          axis.title.y = element_text(size = 18), 
          legend.text = element_text(size=18))
  
  #Save the plot.
  setEPS()
  postscript(paste("Plots/(Main Analysis) Synthetic Control vs. Treated Unit (PrePost Diff) ", outcome_variable, ".eps", sep = ""),  width = 15, height = 8)
  plot(placebo_plot)
  dev.off()
  
  #Fin. 
}



