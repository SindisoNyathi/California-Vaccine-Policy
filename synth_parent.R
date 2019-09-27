#*******************************************************************************************************#
#27/06/2019
#Sindiso Nyathi
#Goal: To run the main synthetic control analysis.

#This R file runs the main synthetic control analysis. The code is divided into 3 sections:
#Section 0: Load required Pacakges, set working directory, and source all required subfunctions.
#Section 1: Read in required data and set required variables.
#Section 2: Iterate through a loop that creates a synthetic control, runs placebo tests and 
#plots the required graphs for each outcome. 
#*******************************************************************************************************#

#*******************************************************************************************************#
#Section 0: Load required Packages and set working directory.

#Load required datasets and packages. Mostly to run the analysis and various data manipulations.
these_packages <- c("Synth", "data.table", "dplyr", "stringr", "panelView", "reshape2", "ggplot2", "ggthemes", 
                    "ggpubr", "gridExtra")
lapply(these_packages, require, character.only = TRUE)

#Set the dataframe and source all the required functions. 
setwd("~/Box/Sindiso Nyathi's Files/California Vaccine Policy")
source('~/Box/Sindiso Nyathi\'s Files/California Vaccine Policy/Code/synth_unit.R')
source('~/Box/Sindiso Nyathi\'s Files/California Vaccine Policy/Code/synth_core.R')
source('~/Box/Sindiso Nyathi\'s Files/California Vaccine Policy/Code/synth_plots.R')
source('~/Box/Sindiso Nyathi\'s Files/California Vaccine Policy/Code/synth_runner.R')
source('~/Box/Sindiso Nyathi\'s Files/California Vaccine Policy/Code/synth_perchange.R')
source('~/Box/Sindiso Nyathi\'s Files/California Vaccine Policy/Code/synth_wplots.R')
#*******************************************************************************************************#

#*******************************************************************************************************#
#Section 1:Define the required variables

#Read in the file containing the vaccination outcomes and covariate data for states.
vaccination_data <- read.csv("Data Files/AggSynthCon.csv")

#Define the intervention year. This should be the last year before the intervention (see Synth Documentation).
intervention_year <- "2015"

#Define the main treated unit.
treated_unit <- "California"

#Define the start and end years. The synthetic control will minimize the RMSPE over the start_year:intervention_year period.
start_year <- 2011
end_year <- 2017

#Define outcomes. Vaccination and exemptions outcomes are "MMR", "Medical", " and "Non-Medical".
outcomes <- c("MMR", "Non.Medical", "Medical")

#Specify the lag variable.
lags <- c("AveLagMMR", "AveLagNMed", "AveLagMed")

#Specify the characteristic covariates for each outcome's synthetic control.
#These are based on a stepwise variable selection procedure. See manuscript and appendix for further details.
mmr_preds <- c('MedAge', 'P_NoPrev', 'P_UnInsured', 'P_PrivIns') 
nmed_preds <- c('MedAge', 'Pop', 'P_BS', 'P_NoCov')
med_preds <- c('P_PrivIns', 'PerCapHealth')
#All the outcomes, covariates and lags should be defined columns in the vaccinaiton_data dataset. 
#*******************************************************************************************************#

#*******************************************************************************************************#
#Section 2: Iterate through the outcomes using the sourced files to run various aspects of the analysis.
covariates <- list(mmr_preds, nmed_preds, med_preds)

for (i in 1:3) {
  
  #Retrieve the outcome and the covariates.
  outcome_variable <- outcomes[[i]]
  covariate_combination <- c(lags[i], covariates[[i]])
  
  #Print a progress message.
  print(paste(cat(outcome_variable, " Outcome In Progress.\n"), sep = ""))
  print(paste(cat("Using the following covariates:\n", covariate_combination), sep = ""))
  
  #Call the synth_unit function. 
  #synth_unit creates a synthetic control and formats the output for use in other functions.
  state_out <- synth_unit(vaccination_data, treated_unit, outcome_variable,
                          intervention_year, covariate_combination, start_year, end_year)
  
  #Print progress message.
  print(paste("Synthetic control for ", outcome_variable, " created. Moving on to plots and placebo tests.", sep = ""))
  
  #Call the synth_plot function. 
  #synth_plot plots the output from synth unit. 
  synth_plots(state_out[[1]], state_out[[2]], treated_unit, 
              start_year, end_year,
              additional_label = "Main Analysis.")
  
  #Save the files containing the weight of the control states for this outcome.
  write.csv(state_out[[3]], paste("Data Files/Covariate Weights. ", outcome_variable, ".csv", sep = ""))
  write.csv(state_out[[4]], paste("Data Files/Control Weights. ", outcome_variable, ".csv", sep = ""))
  
  #Plot the control and covariate weights using synth_wplots. 
  synth_wplots(state_out[[3]], state_out[[2]], treated_unit, additional_label = "Covariate")
  synth_wplots(state_out[[4]], state_out[[2]], treated_unit, additional_label = "Control")
  
  #Run the Placebo Tests for this outcome variable by calling synth_runner.
  placebo_tests <- synth_runner(vaccination_data, outcome_variable, intervention_year, covariate_combination, 
               start_year, end_year)
  
  #Plot the percentage changes (effect sizes) from the placebo tests. 
  synth_perchanges(placebo_tests, outcome_variable)
  
  #Print progress message. 
  print(paste(outcome_variable, " is Complete.\n", sep = ""))
}
#FIN. 
#*******************************************************************************************************#

