#*******************************************************************************************************#
#06/09/2018
#Sindiso Nyathi
#Goal: Conduct a preliminary Synthetic Control Analysis on county kindergarten 
#vaccine adherence. 
#This should also help us to understand the current data as well as how we are 
#using it and its limitations.
#
#
#*******************************************************************************************************#

#Create a function that runs the Synthetic control analysis as well as placebo runs for each of the
#Variables we have. This can be done with normalized or non-normalized data, defined in the function
#call.

synth_runner <- function(outcome_variable, normalized = TRUE) {
  
  #*******************************************************************************************************#
  #Definitions, working directory, and required packages.
  
  #The function has two inputs
  #outcome_variable <- 1 of the six we have in the data file, MMR, DTP, Var1, Var2, HepB, Polio, 
  #Med, NMed, Any. 
  #normalized is a logical input TRUE indicates that we will use the normalized data while
  #FALSE indicates that we will use non-normalized data. 
  
  #Load required datasets and packages. Mostly to run the analysis and various data manipulations.
  these_packages <- c("Synth", "data.table", "dplyr", "stringr", "panelView", "reshape2", "ggplot2", "ggthemes")
  lapply(these_packages, require, character.only = TRUE)
  
  #Set the dataframe and Read in our dataframe
  setwd("~/Box/Sindiso Nyathi's Files/California Vaccine Policy Project")
  
  #Read in the input data. Check if normalized is false. If it is read in the non_normalized data. Default is true.
  vac_pnorm <- read.csv("AggSynthConNorm.csv")
  
  if (!normalized) {vac_pnorm <- read.csv("AggSynthConNotNorm.csv")}
  
  outcome_number <- which(colnames(vac_pnorm) == outcome_variable)
  
  #Correct some values, make sure they are characters or numeric, etc.
  vac_pnorm$States <- as.character(vac_pnorm$States)
  vac_pnorm$StateNo. <- as.numeric(vac_pnorm$StateNo.)
  vac_pnorm$Year <- as.numeric(vac_pnorm$Year)
  vac_pnorm[,outcome_number] <- as.numeric(vac_pnorm[,outcome_number])
  
  #For the variable we are currently running, remove NA values.
  this_variable_column <- which(colnames(vac_pnorm) == outcome_variable)
  
  #If this_variable_column is NA throw an error and indcate that the variable selected is not in the input file. 
  if (is.null(this_variable_column)) {stop("The selected variable does not exist in the input file. \n
                                           Please check the input file and select a variable that exists in the file.")}
  
  #The synth funciton does not take NAs so for the output variable of interest remove all the states with an NA value in
  #any year. For example if Arizona has values for 2010 - 2015 but no value for 2016, we have to remove all the Arizona years.
  remove_these_states <- vac_pnorm[,this_variable_column] %>% 
    is.na %>% 
    which
  remove_these_states <- vac_pnorm$States[remove_these_states] %>%
    unique
  
  #Initialize the data with no NA values. 
  vac_pnormNONA <- vac_pnorm
  
  #For each state in remove_these_states subset vac_pnormNONA and remove the state. 
  for (i in 1:length(remove_these_states)){
    vac_pnormNONA <- vac_pnormNONA[!(vac_pnormNONA$States == remove_these_states[i]),]
  }
  
  #Create a database to store the weights for each synth run. The database should be symetric since the synthetic control 
  #function will be run for every state in order to faciliate a placebo examinatino.
  weights <- as.data.frame(matrix(NA, ncol = length(unique(vac_pnormNONA$States)) + 1,
                                  nrow = length(unique(vac_pnormNONA$States))))
  #Format the dataframe
  weights[,1] <- unique(vac_pnormNONA$States)
  colnames(weights)[1] <- "States"
  colnames(weights)[2:ncol(weights)] <- unique(vac_pnormNONA$States)
  
  #Remove Hawai because of the svd error, check with Nathan
  #vac_pnormNONA <- vac_pnormNONA[!(vac_pnormNONA$States == "Hawaii"),]
  
  #Run data_prep and synth for each state in the final NONA dataframe.
  for (i in 1:length(unique(vac_pnormNONA$States))) {
    
    #Some of the states produce an error, so we wil use try catch to take note of those states
    #but continue with the analysis anyway.
    
    tryCatch({
      
      no_states <- length(unique(vac_pnormNONA$States))
      
      #Which state are we running?
      this_unit <- as.character(unique(vac_pnormNONA$States[i]))
      
      #Print progress monitor.
      print(paste("Current State is ", this_unit, "  Processing . . . ", sep = ""))
      
      #Use dataprep to set up the model.
      vac_object <- dataprep(foo = vac_pnormNONA, 
                             predictors = c("HS.Higher", "X16.Empl", 
                                            "MedInc", "Poverty", 
                                            "Uninsured", "Density"), 
                             predictors.op = "mean", 
                             time.predictors.prior = 2010, 
                             dependent = outcome_variable, 
                             unit.variable = "StateNo.",
                             unit.names.variable = "States", 
                             time.variable = "Year", 
                             treatment.identifier = this_unit, 
                             controls.identifier = c(unique(vac_pnormNONA$States)[-i]), 
                             time.optimize.ssr = 2010:2016, 
                             time.plot = 2010:2016)
      
      
      #Now that dataprep has been done, run the synthetic control.
      #Run the Synth control file
      synth.out <- synth(data.prep.obj = vac_object, method = "BFGS")
      
      #Retrieve the table with the weights.
      synth.tables <- synth.tab(dataprep.res = vac_object, synth.res = synth.out)
      
      #Retrieve the weights, which is what we are interested in.
      these_weights <- as.data.frame(synth.tables$tab.w)
      these_weights <- these_weights[,1]
      
      #Add these weights to these weights table.
      weights[,i+1][c(1:i-1)] <- these_weights[1:i-1]
      
      if (i == length(unique(vac_pnormNONA$States))) next
      
      weights[,i+1][c((i+1):no_states)] <- these_weights[(i+1):no_states-1]
      
      #Print progress monitor.
      print(paste("State ", this_unit, " Complete. Moving on.", sep = ""))
      
    }, 
    
    error = function(e) {cat("ERROR :",conditionMessage(e), "\n")}
    )
    
    #Done
  }
  
  #Save the weights. 
  write.csv(weights, paste(outcome_variable, " Weights.csv", sep = ""))
  
  #Now create the files we need for the plots that we need for the placebo using the weights matrix. 
  #The value at each time point is the weight of each matrix.
  
  #Our goal is to create the outcomes, i.e. %age with MMR vaccine for the synthetic controls for each 
  #state.
  #Fill palcebo_controls with the values of the synthetic controls for each placebo
  
  rownames(weights) <- weights[,1]
  weights <- weights[,-1]
  #Recode NAs to zero in the weights, in theory it should be the same.
  weights[is.na(weights)] <- 0
  these_weights <- as.matrix(weights)
  
  mmr_curr <- vac_pnormNONA[, c(1, 3, 4)]
  
  #Reshape and order mmr_curr, so that rows are just years, and columns are states. 
  mmr_curr <- reshape(mmr_curr, idvar = "Year", timevar = "States", direction = "wide", 
                      new.row.names = unique(mmr_curr$Year))
  mmr_curr <- mmr_curr[,-1]
  colnames(mmr_curr) <- unique(vac_pnormNONA$States)
  mmr_curr <- lapply(mmr_curr, as.numeric)
  mmr_curr <- as.matrix(as.data.frame(mmr_curr))
  
  
  #Now multiply to get the synthetic control for Alabama
  syn_values <- mmr_curr %*% these_weights
  
  #Now qw have syn values, which is basically, plabebo controls. Save this file.
  write.csv(syn_values, "Placebo Synthetic Controls.csv")
  
  #Now create the pacebo gaps files. 
  placebo_gaps <- mmr_curr - syn_values
  
  #Save the placeb gaps file. 
  write.csv(placebo_gaps, "Placebo Gaps.csv")
  
  #Source the plotting file.
  source('~/Box/Sindiso Nyathi\'s Files/California Vaccine Policy Project/CaliVac Plots.R')
  
  #Run the plotting function. 
  cali_vac_plots(placebo_gaps, syn_values, outcome_variable)
  
  #The END. 
  }