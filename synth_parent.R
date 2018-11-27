#*******************************************************************************************************#
#30/10/2018
#Sindiso Nyathi
#Goal: Synthehtic Control Project. Focus: Cross Validation. Forward stepwise variable selction. 
#*******************************************************************************************************#

#*******************************************************************************************************#
#Working set working directory and load files and folders. 
#Load required datasets and packages. Mostly to run the analysis and various data manipulations.
these_packages <- c("Synth", "data.table", "dplyr", "stringr", "panelView", "reshape2", "ggplot2", "ggthemes")
lapply(these_packages, require, character.only = TRUE)

#Set the dataframe and Read in our dataframe
setwd("~/Box/Sindiso Nyathi's Files/California Vaccine Policy Project/Preliminary Results. 25_10_18")
source('~/Box/Sindiso Nyathi\'s Files/California Vaccine Policy Project/Preliminary Results. 25_10_18/synth_core.R')

#Read in the input data. Check if normalized is false. If it is read in the non_normalized data. Default is true.
vac_data <- read.csv("AggSynthCon.csv")

#Run one with average covariates and one with all covariates to see if the resulting plots are different. 
cov_data <- read.csv("Covariates.csv")
#cov_data <- read.csv("CovariatesL.csv")
cov_data <- as.vector(cov_data[,1])

#Store the covariate dataframe
covariate_frame <- as.data.frame(matrix(nrow = length(cov_data), ncol = 4))
colnames(covariate_frame) <- c("Covariates", "Train", "Test", "Sum")

covariate_frame_fin <- as.data.frame(matrix(nrow = 0, ncol = 4))
colnames(covariate_frame_fin) <- c("Covariates", "Train", "Test", "Sum")

#Get California Values for this variable, whcih we will use in the loop.
cali_val <- vac_data[vac_data$States == "California",][4]

final_cov <- c()

while (length(cov_data) != 8) {
  
  this_varno <- length(cov_data)
  covariate_frame[, c(1:4)] <- NA
  
  for (i in 1:this_varno) {
    
    
    cov_comb <- c(final_cov, cov_data[i])
    
    syn_objects <- synth_core(vac_data, "California", "MMR", cov_comb, 2013)
    
    #Get the synth objects from the returned values.
    synth.tables <- syn_objects[[1]]
    synth.out <- syn_objects[[2]]
    incl_states <- syn_objects[[3]]
    
    #Make sure only the included states are used.
    vac_data2 <- vac_data[vac_data$States %in% incl_states, c(1, 3, 4)]
    vac_data2 <- reshape(vac_data2, timevar = "Year", idvar = "States", direction = "wide")
    synth_weights <- as.matrix(synth.tables$solution.w)
    
    synth_val <- lapply(vac_data2[,2:8], syn_values)
    synth_val <- t(synth_weights) %*% as.matrix(vac_data2[,2:8])
    
    cali_whole <- data.frame("Year" = numeric(7), "California" = numeric(7), "Synth" = numeric(7))
    
    cali_whole[1:7,1] <- 2011:2017
    cali_whole[1:7,2] <- cali_val
    cali_whole[1:7,3] <- t(as.data.frame(synth_val))
    
    #Get the RMSPE values. 
    #Now what are we interested in with this model specification. 
    train_rmspe <- round(synth.tables$loss.v, digits = 2)
    test_rmspe <- round(sqrt(((cali_whole[4, 2] - cali_whole[4, 3])^2)  + ((cali_whole[5, 2] - cali_whole[5, 3])^2)), digits = 2)
    
    #Save these values.
    covariate_frame[i, 1] <- cov_data[i]
    covariate_frame[i, 2] <- train_rmspe
    covariate_frame[i, 3] <- test_rmspe
    covariate_frame[i, 4] <- train_rmspe + test_rmspe
    
    
  }
  
  #Find the min test_mspe.
  min_row <- which(covariate_frame$Test == min(covariate_frame$Test, na.rm = TRUE))
  
  #if (length(min_row) > 1) {min_row <- min_row[1]}
  
  #Append the column to covariate_frame_fin
  covariate_frame_fin <- rbind(covariate_frame_fin, covariate_frame[min_row,])
  
  #Add the variable to final data and remove it from cov_data
  final_cov <- c(final_cov, covariate_frame[min_row, 1])
  
  cov_data <- cov_data[-which(cov_data == covariate_frame[min_row, 1])]
  
  #Fin. 
  
}

write.csv(covariate_frame_fin, "Forward. Stepwise Variable Selection. NoLag.csv")
