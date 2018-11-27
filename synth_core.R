#*******************************************************************************************************#
#30/10/2018
#Sindiso Nyathi
#Goal: Synthethic Control Core Fucntion.
#Inputs: state -> The state for which the Synthetic Control should be created. 
#        raw_data -> raw_data
#        outcome_var -> outcome variable.
#        cov_comb -> covariates to be used in model. 
#        year -> year to apply the intervention.
#Returns: A list object. The first object is synth.out and the second is the synth.tables object. 

#Unresolved: The structure of the iput file. 
#*******************************************************************************************************#

#*******************************************************************************************************#
synth_core <- function(raw_data, state, outcome_var, cov_comb, year) {
  
  
  this_variable_column <- which(colnames(raw_data) == outcome_var)
  
  #The synth funciton does not take NAs so for the output variable of interest remove all the states with an NA value in
  #any year. For example if Arizona has values for 2010 - 2015 but no value for 2016, we have to remove all the Arizona years.
  remove_these_states <- raw_data[,this_variable_column] %>% 
    is.na %>% 
    which
  remove_these_states <- raw_data$States[remove_these_states] %>%
    unique
  
  #Initialize the data with no NA values. 
  raw_dataNONA <- raw_data
  
  #For each state in remove_these_states subset vac_pnormNONA and remove the state. 
  for (j in 1:length(remove_these_states)){
    raw_dataNONA <- raw_dataNONA[!(raw_dataNONA$States == remove_these_states[j]),]
  }
  
  #Figure out which number california is in the states.
  state_no. <- which(unique(raw_dataNONA$States) == state)
  raw_data <- raw_dataNONA
  
  #Correct Variable type in the dataframe.
  raw_data[,1] <- as.character(raw_data[,1])
  raw_data[,c(2:ncol(raw_data))] <- as.data.frame(lapply(raw_data[,c(2:ncol(raw_data))], as.numeric))
  
  #Use dataprep to set up the vac_object dataframe that will be passed to the model.
  controls.id <- unique(raw_data$States) 
  idno <- which(controls.id == state)
  
  vac_object <- dataprep(foo = raw_data,
                         #We are using a specific set of predictor variables.
                         predictors = cov_comb, 
                         predictors.op = "mean",
                         time.predictors.prior = 2011, 
                         dependent = outcome_var, 
                         unit.variable = "StateNo.",
                         unit.names.variable = "States", 
                         time.variable = "Year", 
                         treatment.identifier = state, 
                         controls.identifier = controls.id[-idno], 
                         time.optimize.ssr = 2011:year, 
                         time.plot = 2011:2017)
  
  
  #Now that dataprep has been done, run the synthetic control.
  #Run the Synth control file
  synth.out <- synth(data.prep.obj = vac_object, method = "Nelder-Mead")
  #tryCatch({synth.out <- synth(data.prep.obj = vac_object, method = "Nelder-Mead", verbose = FALSE)},
   #        warning = function(cond) {
    #         message(paste(cov_comb, " Covariates did not run and generated the following message: ", sep = ""))
     #        message(cond)
      #     })
   
  #Retrieve the table with the weights.
  synth.tables <- synth.tab(dataprep.res = vac_object, synth.res = synth.out)
  
  #Remove California from this.
  controls.id <- controls.id[-state_no.]
  
  synth_objects <- list(synth.out, synth.tables, controls.id)
  
  return(synth_objects)
}

syn_values <- function(og_values) {
  
  synth_weights <- as.matrix(synth.tables$solution.w)
  #og_values <- as.matrix(og_values)
  
  #Use matrix Multiplication to find the product of the weights and the original values.
  syn_value <- og_values %*% synth_weights
  
  #Return the value
  return(syn_value)
}
