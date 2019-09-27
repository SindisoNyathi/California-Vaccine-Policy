#*******************************************************************************************************#
#27/06/2019
#Sindiso Nyathi
#Goal: Process the Synthetic Control Sensitivity Analysis.
#Comments: Three sets of sensitivity analysis (2 actual sensitivity analysis and 1 negative control
#analysis) will be run.
#1) Sens1: Will rerun the Synthetic Control Analysis but with different cutoffs for the covariates used. The 
#   resulting runs will have anything from 1 to 15 covariates included. 
#3) Sens2: Will rerun the synthetic control analysis leaving out each of the states in the control pool one at
#   a time (leave-one-out tests).
#3) Sens3: Will rerun the synthetic control analysis but place the treatment at a time before the actual
#   treatment.
#*******************************************************************************************************#

#*******************************************************************************************************#
#Preliminaries.
#Load required packages and read in required files. Also source all functions that will be used. 
these_packages <- c("Synth", "data.table", "dplyr", "stringr", "panelView", "reshape2", "ggplot2", "ggthemes")
lapply(these_packages, require, character.only = TRUE)

#Set the wd and source the R files that will be used.
setwd("~/Box/Sindiso Nyathi's Files/California Vaccine Policy")
source('~/Box/Sindiso Nyathi\'s Files/California Vaccine Policy/Code/synth_unit.R')
source('~/Box/Sindiso Nyathi\'s Files/California Vaccine Policy/Code/synth_core.R')
source('~/Box/Sindiso Nyathi\'s Files/California Vaccine Policy/Code/synth_plots.R')
source('~/Box/Sindiso Nyathi\'s Files/California Vaccine Policy/Code/synth_runner.R')
source('~/Box/Sindiso Nyathi\'s Files/California Vaccine Policy/Code/synth_perchange.R')

##Vacination_data can be the state data or the county data.
vaccination_data <- read.csv("Data Files/AggSynthCon.csv")
#*******************************************************************************************************#

#*******************************************************************************************************#
#1) Sens1: Covariate combination Sensitivity Analysis

#Set intervention year and treated unit.
intervention_year <- "2015"
treated_unit <- "California"
start_year <- 2011
end_year <- 2017

#Create the list of outcomes and their respective lags. This will allows us to iterate through them
#using a for loop or the apply family of functions. 
outcomes <- c("MMR", "Non.Medical", "Medical")
lags <- c("AveLagMMR", "AveLagNMed", "AveLagMed")

##Set Covariates. (Refer to the covariates file for the meanings of the various abbreviations)
covariates_mmr <- c("MedAge", "P_NoPrev", "P_UnInsured", "P_PrivIns", "Pop", "PerCapHealth",
                    "P_ChilNoIns", "MedInc", "P_NoCov", "P_BelPov", "P_White", "P_BS", "P_Mar",
                    "P_HS", "P_LivRur")

covariates_nme <- c("MedAge", "Pop", "P_BS", "P_NoCov", "P_LivRur", "PerCapHealth", "P_ChilNoIns",
                    "P_BelPov", "P_UnInsured", "P_Mar", "P_White", "P_NoPrev", "MedInc", "P_HS",
                    "P_PrivIns")

covariates_med <- c("P_PrivIns", "PerCapHealth", "MedAge", "MedInc", "P_ChilNoIns", "P_NoPrev",
                    "P_NoCov", "P_Mar", "P_White", "P_HS", "P_LivRur", "P_BelPov", "Pop", "P_UnInsured",
                    "P_BS")

covariates_all <- list(covariates_mmr, covariates_nme, covariates_med)

#Begin the for loop that will iterate through all the outcomes. 
for (i in 1:3){
  
  ###Set the current outcome. The outcomes list contains outcomes <- c("MMR", "Medical", "Non-medical")
  outcome_variable <- outcomes[i]
  
  #Retrieve the lag covariates.
  ###The Lag list contains. lags <- c("AveLagMMR", "AveLagNMed", "AveLagMed")
  this_lag <- lags[i]
  
  #Retrieve the covariate order for this outcome
  this_covariate_set <- covariates_all[[i]]
  
  #This dataframe will store the results for each covariate combination for the outcomes. 
  #Column 1 will be the covariate combination while column 2 is the resulting effect size. 
  cov_sens <- as.data.frame(matrix(ncol = 2, nrow = 0))
  colnames(cov_sens) <- c("Covariates", "EffectSize")
  
  for (j in c(0:8, 15)){
    
    #Set the covariates, and add the lag.
    these_cov <- this_covariate_set[0:j]
    these_cov <- c(this_lag, these_cov)
    
    #Call the Synth unit function.
    unit_out <- synth_unit(vaccination_data, treated_unit, outcome_variable, intervention_year, these_cov, start_year, end_year)
    
    ###Synth Unit
    ####This function creates a synthetic control for the defined unit, using the defined covariates for 
    ####the given year. The object returned, unit_out is a list with 2 items, 1) A dataframe with 3 columns, 
    #### Year, Unit, Synth, Year is the year, Unit is the value of the outcome for the treated unit, and 
    #### Synth is the value of the outcome from the resulting Synthetic Control. 
    
    #Further process the output from synth_unit to determine the effect size. 
    #We defined Effect Size as [[(Unit_16 + Unit_17)/2] - [(Unit_14 + Unit_15)/2]] - 
    #[[(Synth_16 + Synth_17)/2] - [(Synth_14 + Synth_15)/2]], i.e. How much larger was the change in the 
    #outcome from pre to post policy, in the Treated State than in the synthetic control.
    
    #If the function above fails and returns null. 
    if(is.null(unit_out)){
      next
    }
    
    unit_whole <- as.data.frame(unit_out[[1]])
    
    #Find the values indicated in the formula above to get the effect size.
    pre_synth <- (unit_whole[4, 3] + unit_whole[5, 3])/2
    pre_treat <- (unit_whole[4, 2] + unit_whole[5, 2])/2
    post_synth <- (unit_whole[6, 3] + unit_whole[7, 3])/2
    post_treat <- (unit_whole[6, 2] + unit_whole[7, 2])/2
    
    #Calculate the actual Effect Size Measure.
    eff_measure <- (post_treat - pre_treat) - (post_synth - pre_synth) 
    
    #Could not figure out an easier way to do this, so I just created a mini dataframe with 1 row to store 
    #the covariates and effect measure. This way I could use R bind.
    this_line <- as.data.frame(matrix(nrow = 1, ncol = 2))
    this_line[1, 1][[1]] <- list(these_cov)
    this_line[1, 2] <- eff_measure
    
    #Merge this to the dataframe with rbind. 
    cov_sens <- rbind(cov_sens, this_line)
  }
  
  #Label the cov_sense file.
  colnames(cov_sens) <- c("Covariates", "EffectSize")
  
  #Plot the resulting effect sizes. 
  cov_sense_plot <- ggplot(cov_sens, aes(x = c(1:10), y = EffectSize)) +
    geom_bar(stat = "identity", fill = "steelblue4") +
    theme_bw() +
    labs(title = "Sensitivity Analysis", subtitle = paste("Covariate Selection: ", outcome_variable, sep = "")) +
    xlab("Covariate Set") +
    ylab("Pre-Post Difference (%)") +
    scale_x_continuous(limits = c(0, 11), breaks = c(1:11)) +
    #ylim(0, 5) +
    theme(plot.title = element_text(hjust = 0.5, size = 22),
          legend.title = element_blank(), 
          plot.subtitle = element_text(hjust = 0.5, size = 18), 
          axis.text = element_text(size=16),
          axis.text.x = element_text(angle=90, hjust = 1),
          axis.title.x = element_text(size = 18), 
          axis.title.y = element_text(size = 18), 
          legend.text = element_text(size=18))
  
  #Save the plot for the Sensitivity Analysis for this outcome_variable.
  setEPS()
  postscript(paste("Plots/(Sensitivity Analysis) Covariates. PrePost Difference. ", outcome_variable, ".eps", sep = ""), width = 15, height = 8)
  plot(cov_sense_plot)
  dev.off()
  
  #This prevents the unimplemented type 'list in encode element error. 
  cov_sens <- apply(cov_sens, 2, as.character)
  
  #Save the csv with the results for this outcomes. 
  write.csv(cov_sens, paste("Data Files/(Sensitivity Analysis) Covariates. ", outcome_variable, ".csv", sep = ""))
}
#*******************************************************************************************************#

#*******************************************************************************************************#
##2) Sens2: Leave One Out Tests. (Some unneccesary code is replicated for clarity.)

#Set intervention year and treated unit.
intervention_year <- "2015"
treated_unit <- "California"
start_year <- 2011
end_year <- 2017

#Create the list of interventions and their respective lags. This will allows us to iterate through them
#using a for loop or the apply family of functions. 
#Interventions are "MMR", "Medical", "Any"
outcomes <- c("MMR", "Non.Medical", "Medical")
lags <- c("AveLagMMR", "AveLagNMed", "AveLagMed")
mmr_preds <- c('MedAge', 'P_NoPrev', 'P_UnInsured', 'P_PrivIns') 
nme_preds <- c('MedAge', 'Pop', 'P_BS', 'P_NoCov')
med_preds <- c('P_PrivIns', 'PerCapHealth')


covariates_all <- list(mmr_preds, nme_preds, med_preds)

#Begin the for loop that will iterate through all the outcomes. 
for (i in 1:3){
  
  ###The outcomes list contains outcomes <- c("MMR", "Medical", "NMed")
  outcome_variable <- outcomes[i]
  
  #Retrieve the lag covariates.
  ###The Lag list contains. lags <- c("AveLagMMR", "AveLagMed", "AveLagNMed")
  this_lag <- lags[i]
  covariates <- c(this_lag, covariates_all[[i]])
  
  #This dataframe will store the results for each covariate combination for the outcomes. 
  #Column 1 will be the covariate combination while column 2 is the effect size. 
  cov_sens <- as.data.frame(matrix(ncol = 2, nrow = 0))
  colnames(cov_sens) <- c("Unit Excluded", "EffectSize")
  
  #Set the control pool. 
  control_pool <- unique(vaccination_data$Units)
  control_pool <- control_pool[!(control_pool == treated_unit)]
  
  #Adds none so that the first run is basically the main run.
  control_pool <- c("None", as.character(control_pool))
  
  #Modify the vaccination data, dataset to remove 1 state at a time.
  for (j in c(1:50)){
    
    #Set the state to leave.
    leave_this_out <- control_pool[j]
    
    #Remove the state.
    mod_vaccination_data <- vaccination_data[!(vaccination_data$Units == leave_this_out),]
    
    #Call the Synth unit function, with the mod_vaccination_data file.
    unit_out <- synth_unit(mod_vaccination_data, treated_unit, outcome_variable, 
                           intervention_year, covariates, start_year, end_year)
    ###Synth Unit
    ####This function creates a synthetic control for the defined unit, using the defined covariates for 
    ####the given year. The object returned, unit_out is a list with 2 items, 1) A dataframe with 3 columns, 
    #### Year, Unit, Synth, Year is the year, Unit is the value of the outcome for the treated unit, and 
    #### Synth is the value of the outcome from the resulting Synthetic Control. 
    
    #Make sure hte output to synth is not NULL.
    if(is.null(unit_out)){
      next
    }
    
    #Further process the output from synth_unit to determine the effect size. 
    #We defined Effect Size as [[(Unit_16 + Unit_17)/2] - [(Unit_14 + Unit_15)/2]] - 
    #[[(Synth_16 + Synth_17)/2] - [(Synth_14 + Synth_15)/2]], i.e. How much larger was the change in the 
    #outcome from pre to post policy, in the Treated State than in the synthetic control.
    unit_whole <- as.data.frame(unit_out[[1]])
    
    #Find the values indicated in the formula above. 
    pre_synth <- (unit_whole[4, 3] + unit_whole[5, 3])/2
    pre_treat <- (unit_whole[4, 2] + unit_whole[5, 2])/2
    post_synth <- (unit_whole[6, 3] + unit_whole[7, 3])/2
    post_treat <- (unit_whole[6, 2] + unit_whole[7, 2])/2
    
    #Calculate the actual Effect Size Measure.
    eff_measure <- (post_treat - pre_treat) - (post_synth - pre_synth) 
    
    #Could not figure out an easier way to do this, so I just created a mini dataframe with 1 row to store 
    #the covariates and effect measure. This way I could use R bind.
    this_line <- as.data.frame(matrix(nrow = 1, ncol = 2))
    this_line[1, 1][[1]] <- as.character(leave_this_out)
    this_line[1, 2] <- eff_measure
    
    #Merge this to the dataframe with rbind. 
    cov_sens <- rbind(cov_sens, this_line)
  }

  #Label the cov_sense file.
  colnames(cov_sens) <- c("UnitExcluded", "EffectSize")
  
  #Plot the resulting effect sizes. 
  cov_sense_plot <- ggplot(cov_sens, aes(x = UnitExcluded, y = EffectSize)) +
    geom_bar(stat = "identity", fill = "steelblue4") +
    theme_bw() +
    labs(title = "Sensitivity Analysis", subtitle = paste("Units Included: ", outcome_variable, sep = "")) +
    xlab("Unit Excluded") +
    ylab("Pre-Post Difference (%)") +
    #scale_x_continuous(limits = c(0, 11), breaks = c(1:11)) +
    #ylim(0, 5) +
    theme(plot.title = element_text(hjust = 0.5, size = 22),
          legend.title = element_blank(), 
          plot.subtitle = element_text(hjust = 0.5, size = 18), 
          axis.text = element_text(size=16),
          axis.text.x = element_text(angle=90, hjust = 1),
          axis.title.x = element_text(size = 18), 
          axis.title.y = element_text(size = 18), 
          legend.text = element_text(size=18))
  
  #Save the plot for the Sensitivity Analysis for this outcome_variable.
  setEPS()
  postscript(paste("Plots/(Sensitivity Analysis) States. PrePost Difference. ", outcome_variable, ".eps", sep = ""), width = 15, height = 8)
  plot(cov_sense_plot)
  dev.off()
  
  #This prevents the unimplemented type 'list in encode element error.
  cov_sens <- apply(cov_sens, 2, as.character)
  
  #Save the csv with the results for this outcomes. 
  write.csv(as.data.frame(cov_sens), paste("Data Files/(Sensitivity Analysis) States. ", outcome_variable, ".csv", sep = ""))
}
#*******************************************************************************************************#

#*******************************************************************************************************#
##3) Sens3: Negative Control Tests. (Some unneccesary code replicated for clarity.)

#Set intervention year and treated unit.
treated_unit <- "California"
intervention_year <- c(2011:2015)
start_year <- 2011
end_year <- 2017

#Create the list of interventions and their respective lags. This will allows us to iterate through them
#using a for loop or the apply family of functions. 
outcomes <- c("MMR", "Non.Medical", "Medical")
lags <- c("AveLagMMR", "AveLagNMed", "AveLagMed")
mmr_preds <- c('MedAge', 'P_NoPrev', 'P_UnInsured', 'P_PrivIns') 
nme_preds <- c('MedAge', 'Pop', 'P_BS', 'P_NoCov')
med_preds <- c('P_PrivIns', 'PerCapHealth')

covariates_all <- list(mmr_preds, nme_preds, med_preds)

#Begin the for loop that will iterate through all the outcomes. 
for (i in 1:3){
  
  ###The outcomes list contains outcomes <- c("MMR", "Medical", "Any")
  outcome_variable <- outcomes[i]
  
  #Retrieve the lag covariates.
  this_lag <- lags[i]
  covariates <- c(this_lag, covariates_all[[i]])
  
  #This dataframe will store the results for each covariate combination for the outcomes. 
  #Column 1 will be the year being tested while column 2 is the effect size. 
  cov_sens <- as.data.frame(matrix(ncol = 2, nrow = 0))
  colnames(cov_sens) <- c("TreatmentYear", "EffectSize")
  
  for (j in c(1:5)){
    
    #Modify the vaccination data, dataset to remove 1 state at a time.
    this_intervention_year <- intervention_year[j]
    
    #Call the Synth unit function, with the mod_vaccination_data file..
    unit_out <- synth_unit(vaccination_data, treated_unit, outcome_variable, 
                           this_intervention_year, covariates, start_year, end_year)
    ###Synth Unit
    ####This function creates a synthetic control for the defined unit, using the defined covariates for 
    ####the given year. The object returned, unit_out is a list with 2 items, 1) A dataframe with 3 columns, 
    #### Year, Unit, Synth, Year is the year, Unit is the value of the outcome for the treated unit, and 
    #### Synth is the value of the outcome from the resulting Synthetic Control. 
    
    #Further process the output from synth_unit to determine the effect size. 
    #We defined Effect Size as [[(Unit_16 + Unit_17)/2] - [(Unit_14 + Unit_15)/2]] - 
    #[[(Synth_16 + Synth_17)/2] - [(Synth_14 + Synth_15)/2]], i.e. How much larger was the change in the 
    #outcome from pre to post policy, in the Treated State than in the synthetic control.
    unit_whole <- as.data.frame(unit_out[[1]])
    
    #Find the values indicated in the formula above. 
    if (j == 1) {    
      pre_synth <- unit_whole[j, 3]
      pre_treat <- unit_whole[j, 2]
    } else {
      pre_synth <- (unit_whole[j-1, 3] + unit_whole[j, 3])/2
      pre_treat <- (unit_whole[j-1, 2] + unit_whole[j, 2])/2
      }
    
    post_synth <- (unit_whole[j+2, 3] + unit_whole[j+1, 3])/2
    post_treat <- (unit_whole[j+2, 2] + unit_whole[j+1, 2])/2
    
    #Calculate the actual Effect Size Measure.
    eff_measure <- (post_treat - pre_treat) - (post_synth - pre_synth) 
    
    #Could not figure out an easier way to do this, so I just created a mini dataframe with 1 row to store 
    #the covariates and effect measure. This way I could use R bind.
    this_line <- as.data.frame(matrix(nrow = 1, ncol = 2))
    this_line[1, 1][[1]] <- as.character(this_intervention_year)
    this_line[1, 2] <- eff_measure
    
    #Merge this to the dataframe with rbind. 
    cov_sens <- rbind(cov_sens, this_line)
  }
  
  #Label the cov_sense file.
  colnames(cov_sens) <- c("TreatmentYear", "EffectSize")
  
  #Plot the resulting effect sizes. 
  cov_sense_plot <- ggplot(cov_sens, aes(x = TreatmentYear, y = EffectSize)) +
    geom_bar(stat = "identity", fill = "steelblue4") +
    theme_bw() +
    labs(title = "Sensitivity Analysis", subtitle = paste("Treatment Year: ", outcome_variable, sep = "")) +
    xlab("Treatment Year") +
    ylab("Pre-Post Difference (%)") +
    #scale_x_continuous(limits = c(0, 5), breaks = c(1:5)) +
    #ylim(0, 5) +
    theme(plot.title = element_text(hjust = 0.5, size = 22),
          legend.title = element_blank(), 
          plot.subtitle = element_text(hjust = 0.5, size = 18), 
          axis.text = element_text(size=16),
          axis.text.x = element_text(angle=90, hjust = 1),
          axis.title.x = element_text(size = 18), 
          axis.title.y = element_text(size = 18), 
          legend.text = element_text(size=18))
  
  #Save the plot for the Sensitivity Analysis for this outcome_variable.
  setEPS()
  postscript(paste("Plots/(Sensitivity Analysis) Years. PrePost Difference. ", outcome_variable, ".eps", sep = ""), width = 15, height = 8)
  plot(cov_sense_plot)
  dev.off()
  
  #This preventa the unimplemented type 'list in encode element error.
  cov_sens <- apply(cov_sens, 2, as.character)
  
  #Save the csv with the results for this outcomes. 
  write.csv(as.data.frame(cov_sens), paste("Data Files/(Sensitivity Analysis) Years. ", outcome_variable, ".csv", sep = ""))
}
#*******************************************************************************************************#
####Trickster Trickster, 
####Stole the sun for a prank. 
####Did he really ride it, 
####where did you hide it? 
####Down by the riverbank."
#N.K. Jemisin in The Kingdom of the Gods
#*******************************************************************************************************#

