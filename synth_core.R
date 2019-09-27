#*******************************************************************************************************#
#27/06/2019
#Sindiso Nyathi
#Goal: Run the core Synthethic Control analysis. 
#Inputs: state -> The state for which the Synthetic Control should be created. 
#        raw_data -> raw_data, containing the covariates and pre and post intervention values
#        outcome_variable -> outcome variable, e.g. 'MMR', 'Med', 'NMed'
#        covariate_combination -> list of covariates to be used in model. 
#        year -> year to apply the intervention, e.g. 2014, 2015, etc.
#*******************************************************************************************************#

#*******************************************************************************************************#
synth_core <- function(raw_data, treated_unit, outcome_variable, 
                       covariate_combination, treated_year, start_year, end_year) {
  
  #Return the column the output variable of interest is in.
  this_variable_column <- which(colnames(raw_data) == outcome_variable)
  
  #The synth function does not take NAs, so for the output variable of interest remove all the states with an NA value in
  #any year. For example if Arizona has values for 2010 - 2015 but no value for 2016, we have to remove all the Arizona years.
  remove_these_units <- raw_data[,this_variable_column] %>% 
    is.na %>% 
    which 
  remove_these_units <- raw_data$Units[remove_these_units] %>%
    unique
  
  #Check if the treated unit contains NAs for any year of the outcome, by checking if it is in the removed units.
  if (isTRUE(treated_unit %in% remove_these_units)) {
    print(paste("The treated unit contains NA values for the outcome: ", outcome_variable, ". Please correct this error and rerun the 
          code", sep = ""))
    return(NULL)
  }
  
  else {
    
    #Initialize the data. 
    raw_dataNONA <- raw_data
    
    #For each state in remove_these_states, remove that state from the NONA dataset. 
    for (j in 1:length(remove_these_units)){
      raw_dataNONA <- raw_dataNONA[!(raw_dataNONA$Units == remove_these_units[j]),]
    }
    raw_data <- raw_dataNONA
    
    
    #Correct the Variable types in the dataframe.
    raw_data[,1] <- as.character(raw_data[,1])
    raw_data[,c(2:ncol(raw_data))] <- as.data.frame(lapply(raw_data[,c(2:ncol(raw_data))], as.numeric))
    
    #Use dataprep to set up the vac_object dataframe that will be passed to the synth function
    #Dataprep is a function that was defined by Abadie et al. (See Synth Documentation).
    controls.id <- unique(raw_data$Units) 
    unit_no. <- which(controls.id == treated_unit)
    
    #Print the number of states used for the synthetic control.
    control_pool = as.numeric(length(controls.id))
    excluded_units = as.numeric(length(remove_these_units))

    print(paste(cat(control_pool, " Units were used in creating the Synthetic Control.\n", 
                excluded_units, " were excluded: \n", as.character(remove_these_units)), sep = ""))
    
    vac_object <- dataprep(foo = raw_data,
                           #We are using a specific set of predictor variables.
                           predictors = covariate_combination, 
                           predictors.op = "mean",
                           time.predictors.prior = 2011, #This input is not very important 
                           dependent = outcome_variable, 
                           unit.variable = "UnitNo.",
                           unit.names.variable = "Units", 
                           time.variable = "Year", 
                           treatment.identifier = treated_unit, 
                           controls.identifier = controls.id[-unit_no.], #Control units 
                           time.optimize.ssr = start_year:treated_year, 
                           time.plot = start_year:end_year)
    
    
    #Now that dataprep has been done, run the synthetic control.
    #Run the Synth control file.
    
    #Trycatch deals with instances in the placebo tests where a synthetic control is not created.
    
    tryCatch({
      #Call Abadie's synth function.
      synth.out <- synth(data.prep.obj = vac_object,
                         verbose = FALSE)
      
      #Retrieve the table with the weights.
      synth.tables <- synth.tab(dataprep.res = vac_object, synth.res = synth.out)
      
      #Remove the treated state from the list of states so you have a list of potential controls. 
      controls.id <- controls.id[-unit_no.]
      
      #Retrieve the weights of the covariates.
      cov_weights <- as.data.frame(t(synth.out$solution.v))
      cov_names <- rownames(cov_weights)
      cov_weights <- cbind(cov_names,cov_weights)
      colnames(cov_weights) <- c("Name", "Weight")
      
      #Retrieve the weights of the control states.
      unit_weights <- as.data.frame(synth.out$solution.w)
      unit_names <- as.data.frame(controls.id)
      unit_weights <- cbind(unit_names, unit_weights)
      colnames(unit_weights) <- c("Name", "Weight")
      
      #Combine the 6 objects so that we have a single list object to return. 
      synth_objects <- list(synth.tables, synth.out, controls.id, this_variable_column, cov_weights, unit_weights)
      
      #Return. 
      return(synth_objects)
      
    }, 
    
    #In case of an error print an error message.
    error = function(e) {cat("ERROR :", treated_unit, " not synthesized. \n")
      return(NULL)}
    )
  }
}
#FIN.

################################################################################################
#"There are three things all wise men fear; the sea in storm, a night with no moon, and the anger 
#of a gentle man."
#Patrick Rothfuss in The Wise Man's Fear.
################################################################################################
