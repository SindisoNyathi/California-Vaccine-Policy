#*******************************************************************************************************#
#27/06/2019
#Sindiso Nyathi
#Goal: Create a synthetic control based on the inputs. Format and return the output.

#The code is in two sections:
#Section 1: Call the synth_core function to create a synthetic control.
#Section 2: Format the output returned by synth_core and return them to the parent file.

#For definitions of required inputs see the synth_parent function.
#*******************************************************************************************************#

#*******************************************************************************************************#
synth_unit <- function(vaccination_data, treated_unit, 
                        outcome_variable, intervention_year, 
                        covariate_combination, start_year, end_year) {
  
  #*******************************************************************************************************#
  #Section 1: Call the synth_core function. 
  #synth_core formats the inputs given, creates a synthetic control based on these inputs, and returns the output.
  
  synth_objects <- synth_core(vaccination_data, treated_unit, 
                              outcome_variable, covariate_combination, 
                              intervention_year, start_year, end_year)
  
  
  #*******************************************************************************************************#
  
  #*******************************************************************************************************#
  #Section 2: Format the output from synth_core above. 
  
  #If the function above fails, it returns NULL. This would happen if the synth function was unable to create
  #a synthetic control with the given inputs. This may happen with 1 or 2 states in the placebo analysis.
  if(is.null(synth_objects)){return(NULL)}
  
  #Get the synth objects components from the returned object
  synth.tables <- synth_objects[[1]] 
  synth.out <- synth_objects[[2]]
  included_units <- synth_objects[[3]]
  outcome_column <- synth_objects[[4]]
  
  #Make sure only the included states are used in calculating the synthetic control outcome values, i.e. exclude states
  #excluded from the synthetic control analysis due to missind data. 
  vac_data2 <- vaccination_data[(vaccination_data$Units %in% included_units), c(1, 3, outcome_column)]
  vac_data2 <- reshape(vac_data2, timevar = "Year", idvar = "Units", direction = "wide")
  synth_weights <- as.matrix(synth.out$solution.w)
  
  no_years = length(start_year:end_year)
  
  synth_values <- t(synth_weights) %*% as.matrix(vac_data2[,2:(no_years + 1)])
  
  #The California values.
  unit_values <- vaccination_data[vaccination_data$Units == treated_unit,][outcome_column]
  unit_values <- unit_values[c(1:no_years),]
  
  #Populate the final data frame.
  unit_whole <- data.frame("Year" = numeric(5), "Unit" = numeric(5), "Synth" = numeric(5))
  unit_whole[1:no_years,1] <- start_year:end_year
  unit_whole[1:no_years,2] <- unit_values
  unit_whole[1:no_years,3] <- t(as.data.frame(synth_values))
  
  #Return everything, including the covariate and unit_weights.
  return(list(unit_whole, outcome_variable, synth_objects[[5]], synth_objects[[6]]))
  #Done.
}

#################################################################################################
#"I have seen Steelheart bleed; and I will see him bleed again"
#Brandon Sanderson in Steelheart
#################################################################################################
