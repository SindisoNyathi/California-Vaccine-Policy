#*******************************************************************************************************#
#27/06/2019
#Sindiso Nyathi
#Goal: Conduct the Synthethic Control Analysis Cross Validation and Variable Selection. 
#Focus: Cross Validation. Forward stepwise variable selction for each of MMR Coverage, Medical and Non-medical Exemptions. 
#*******************************************************************************************************#

#*******************************************************************************************************#
#Set working directory and load files and folders. 
#Load required datasets and packages. Mostly to run the analysis and various data manipulations.
these_packages <- c("Synth", "data.table", "dplyr", "stringr", "panelView", "reshape2", "ggplot2", "ggthemes")
lapply(these_packages, require, character.only = TRUE)

#Set the working directory and source all required files.
setwd("~/Box/Sindiso Nyathi's Files/California Vaccine Policy")
source('~/Box/Sindiso Nyathi\'s Files/California Vaccine Policy/Code/synth_unit.R')
source('~/Box/Sindiso Nyathi\'s Files/California Vaccine Policy/Code/synth_core.R')
source('~/Box/Sindiso Nyathi\'s Files/California Vaccine Policy/Code/synth_plots.R')

#Read in the input data.
vac_data <- read.csv("Data Files/AggSynthCon.csv")

#For cross validation adn variable selection we will use just hte pre-policy data. 
vac_data <- vac_data[which(vac_data$Year < 2016),]

#Read in the potential covariates
cov_data <- read.csv("Data Files/Covariates.csv")
cov_data <- as.vector(cov_data[,1])

#The actual function for variable selection.
#Input is the output variable.
variable_selection <- function(outcome_variable){
  
  #Create a sub-covariate dataframe to store RMSPE values during variable testing.
  covariate_frame <- as.data.frame(matrix(nrow = length(cov_data), ncol = 4))
  colnames(covariate_frame) <- c("Covariates", "Train", "Test", "Sum")
  
  #Create an overall covariate dataframe to store final RMSPE values and final variables selected.
  covariate_frame_fin <- as.data.frame(matrix(nrow = 0, ncol = 4))
  colnames(covariate_frame_fin) <- c("Covariates", "Train", "Test", "Sum")
  
  #Initialize an empty vector as the final covariate frame.
  final_cov <- c()
  
  #This was a little unusual, and the code is a little tough to read. But here goes nothing. 
  
  #Effectively while the cov_data frame is not empty.
  while (length(cov_data) != 0) 
  {
    
    #Retrieve the number of covariates we are testing.
    this_varno <- length(cov_data)
    
    #Empty the dataframe to store the resulting RMSE values. 
    covariate_frame[, c(1:4)] <- NA
    
    #Iterate through each of the variables and the ones already selected.
    for (i in 1:this_varno) 
    {
      cov_comb <- c(final_cov, cov_data[i])
      
      #Create a synthetic control
      syn_objects <- synth_unit(vac_data, "California", outcome_variable, 2013, cov_comb, 2011, 2015)
      
      #If the function above fails and returns null. 
      if(is.null(syn_objects)){
        next
        }
      
      #Retrieve the dataframe containing output values.
      cali_whole <- as.data.frame(syn_objects[[1]])
      
      #Get the RMSPE values for the test and train time periods.
      train_rmspe <- round(sqrt(((cali_whole[1, 2] - cali_whole[1, 3])^2) + 
                                  ((cali_whole[2, 2] - cali_whole[2, 3])^2) + 
                                  ((cali_whole[3, 2] - cali_whole[3, 3])^2)), digits = 4)
      test_rmspe <- round(sqrt(((cali_whole[4, 2] - cali_whole[4, 3])^2)  + 
                                 ((cali_whole[5, 2] - cali_whole[5, 3])^2)), digits = 4)
      
      #Save these values.
      covariate_frame[i, 1] <- cov_data[i]
      covariate_frame[i, 2] <- train_rmspe
      covariate_frame[i, 3] <- test_rmspe
      covariate_frame[i, 4] <- train_rmspe + test_rmspe
      
    }
    
    #Find the min test_mspe.
    min_row <- which(covariate_frame$Test == min(covariate_frame$Test, na.rm = TRUE))
    
    #Append the column to covariate_frame_fin
    covariate_frame_fin <- rbind(covariate_frame_fin, covariate_frame[min_row,])
    
    #Add the variable to final data and remove it from cov_data
    final_cov <- c(final_cov, covariate_frame[min_row, 1])
    cov_data <- cov_data[-which(cov_data %in% covariate_frame[min_row, 1])]
    
    #Print Progress Note. 
    print(paste(cat("Current Variable Set:\n ", final_cov, "\nProceeding . . . ")), sep = "")
    }
  
  #Print Progress Note. 
  print("Variable Selection Complete. Saving data files and plots")
  
  #To ensure that the order of the covariates is not changed.
  covariate_frame_fin$Number <- 1:15

  #Plot the resulting RMSPE Values for this outcome.
  rmspe_plot <- ggplot(covariate_frame_fin, aes(x = Number, y = Test)) +
    geom_point(color = "black") +
    geom_line(color = "black", aes(group = 1)) + 
    theme_bw() +
    labs(title = "Stepwise Variable Selection", subtitle = paste("Outcome: ", outcome_variable, sep = "")) +
    xlab("Covariate Added") +
    ylab("Test RMSPE Value") +
    theme(plot.title = element_text(hjust = 0.5, size = 22),
          legend.title = element_blank(), 
          plot.subtitle = element_text(hjust = 0.5, size = 18), 
          axis.text = element_text(size=16),
          axis.text.x = element_text(angle=90, hjust = 1),
          axis.title.x = element_text(size = 18), 
          axis.title.y = element_text(size = 18), 
          legend.text = element_text(size=18))
  
  #Save the plot and the data file.
  setEPS()
  postscript(paste("Plots/(Cross Validation) Stepwise Variable Selection. ", outcome_variable, ".eps", sep = ""), width = 15, height = 8)
  plot(rmspe_plot)
  dev.off()
  
  #Datafile.
  write.csv(covariate_frame_fin, 
            paste("Data Files/(Cross Validation) Stepwise Variable Selection. ", outcome_variable, ".csv", sep = ""))
}
#Fin. 
#*******************************************************************************************************#

#*******************************************************************************************************#
#Call the function for each outcome. 
variable_selection("MMR")
variable_selection("Non.Medical")
variable_selection("Medical")
#Fin. 
#*******************************************************************************************************#

#*******************************************************************************************************#