#*******************************************************************************************************#
#29/09/2019
#Hannah Karpel
#Goal: To run the main Difference-in-differences Analysis.

#Notes: This R file runs the main Difference-in-differences analysis for the California Vaccine Policy paper.
#*******************************************************************************************************#

#*******************************************************************************************************#
#Load required Packages and set working directory.
working_directory <- "" 
setwd(working_directory)

#*******************************************************************************************************#

#*******************************************************************************************************#
#Outcome 1: Vaccine Coverage
#Import vaccine overall coverage data 
overall_coverage <- read.csv("overall_coverage.csv")

#Examine the data.
str(overall_coverage)

#Correct data types.
overall_coverage$poverty = as.numeric(overall_coverage$poverty)

#Create DID model parameters 
overall_coverage$pre_post = ifelse(overall_coverage$Year >= 2016, 1, 0)
overall_coverage$not_cali = ifelse(overall_coverage$State_num == 7, 1, 0)
overall_coverage$did1 = overall_coverage$pre_post * overall_coverage$not_cali

#Full adjusted model for overall vaccination
didreg1 = lm(All ~ pre_post + not_cali + did1 + poverty + income + pop + White_percent + household_size + lessthanHS	+ some_college_orless	+ bachelors_orhigher + uninsured_children, data = overall_coverage)

#Process the cluster robust standard errors
#Load the required library.
library(clubSandwich)
coef_test(didreg1, vcov = "CR2", cluster = overall_coverage$Geography, test = "Satterthwaite")

#Sensitivity Analysis.
#Leave out one state in turn
for (i in c(1:6, 8:17)) {
  didreg1 = lm(All ~ pre_post + not_cali + did1 + income + pop + poverty + household_size + White_percent + lessthanHS	+ some_college_orless	+ bachelors_orhigher + uninsured_children, data = overall_coverage, subset=State_num!=i)
  print(paste(i, overall_coverage$State[overall_coverage$State_num==i][1]))
  print(summary(didreg1))
  cluster <- coef_test(didreg1, vcov = "CR2", cluster = overall_coverage[overall_coverage$State_num!=i,]$Geography, test = "Satterthwaite")
  print(cluster)
}

#Leave out states reporting MMR coverage 
overall_coverage_noMMR <- subset(overall_coverage, State=='Arizona' | State == 'New York' | 
                                  State == 'Kansas' | State == 'Connecticut' | State == 'Florida' | 
                                  State == 'Iowa' | State == 'California' | State == 'Virginia' |
                                  State == 'Oregon' | State == 'New Jersey' | 
                                  State == 'Massachusetts' | State == 'Washington')

#Create DID model parameters 
overall_coverage_noMMR$pre_post = ifelse(overall_coverage_noMMR$Year >= 2016, 1, 0)
overall_coverage_noMMR$not_cali = ifelse(overall_coverage_noMMR$State_num == 7, 1,0)
overall_coverage_noMMR$did1 = overall_coverage_noMMR$pre_post * overall_coverage_noMMR$not_cali

#The model. 
didreg1_noMMR = lm(All ~ pre_post + not_cali + did1 + poverty + income + pop + White_percent + household_size + lessthanHS	+ some_college_orless	+ bachelors_orhigher + uninsured_children, data = overall_coverage_noMMR)

#Process the cluster robust standard errors
library(clubSandwich)
coef_test(didreg1_noMMR, vcov = "CR2", cluster = overall_coverage_noMMR$Geography, test = "Satterthwaite")

#*******************************************************************************************************#

#*******************************************************************************************************#
#Outcomes 2 and 3: Medical Exemptions and Non-medical Exemptions.
#Import exemption data 
exemptions <- read.csv("Exemptions_Data.csv")

#Examine the data. 
str(exemptions)

#Correct the data type.
exemptions$poverty = as.numeric(exemptions$poverty)

#Create DID model parameters-- exemptions
exemptions$pre_post = ifelse(exemptions$Year >= 2016, 1, 0)
exemptions$not_cali = ifelse(exemptions$State_num == 7, 1,0)
exemptions$did1 = exemptions$pre_post * exemptions$not_cali

#Full adjusted model for medical exemptions
didreg2 = lm(Med ~ pre_post + not_cali + did1 + income + household_size + pop + poverty + White_percent + lessthanHS	+ some_college_orless	+ bachelors_orhigher + uninsured_children, data = exemptions)

#Cluster robust standard errors 
library(clubSandwich)
coef_test(didreg2, vcov = "CR2", cluster = exemptions$Geography, test = "Satterthwaite")

#Full adjusted model for non-medical exemptions
didreg3 = lm(Pers.Rel ~ pre_post + not_cali + did1 + income + household_size + pop + poverty + White_percent + lessthanHS	+ some_college_orless	+ bachelors_orhigher + uninsured_children, data = exemptions)
#Cluster robust standard errors 
coef_test(didreg3, vcov = "CR2", cluster = exemptions$Geography, test = "Satterthwaite")

#Sensitivity Analysis
#Leave out one state in turn--medical exemptions
for (i in c(1:6, 8:17)) {
  didreg2 = lm(Med ~ pre_post + not_cali + did1 + income + household_size + pop + poverty + White_percent + lessthanHS	+ some_college_orless	+ bachelors_orhigher + uninsured_children, data = exemptions, subset=State_num!=i)
  print(paste(i, exemptions$State[exemptions$State_num==i][1]))
  print(summary(didreg2))
  cluster <- coef_test(didreg2, vcov = "CR2", cluster = exemptions[exemptions$State_num!=i,]$Geography, test = "Satterthwaite")
  print(cluster)
}

#Sensitivity Analysis
#Leave out one state in turn -- nonmedical exemptions
for (i in c(1:6, 8:17)) {
  didreg3 = lm(Pers.Rel ~ pre_post + not_cali + did1 + income + household_size + pop + poverty + White_percent + lessthanHS	+ some_college_orless	+ bachelors_orhigher + uninsured_children, data = exemptions, subset=State_num!=i)
  print(paste(i, exemptions$State[exemptions$State_num==i][1]))
  print(summary(didreg3))
  cluster <- coef_test(didreg3, vcov = "CR2", cluster = exemptions[exemptions$State_num!=i,]$Geography, test = "Satterthwaite")
  print(cluster)
}

#Leave out states reporting MMR coverage for exemption data 
exemptions_noMMR <- subset(exemptions, State=='Arizona' | State == 'New York' | 
                             State == 'Kansas' | State == 'Connecticut' | State == 'Florida' | 
                             State == 'Iowa' | State == 'California' | State == 'Virginia' |
                             State == 'Oregon' | State == 'New Jersey' | 
                             State == 'Massachusetts' | State == 'Washington' | State == 'Arkansas')

#Create DID model parameters 
exemptions_noMMR$pre_post = ifelse(exemptions_noMMR$Year >= 2016, 1, 0)
exemptions_noMMR$not_cali = ifelse(exemptions_noMMR$State_num == 7, 1,0)
exemptions_noMMR$did1 = exemptions_noMMR$pre_post * exemptions_noMMR$not_cali

#Model for medical exemptions
didreg2_noMMR = lm(Med ~ pre_post + not_cali + did1 + income + household_size + pop + poverty + White_percent + lessthanHS	+ some_college_orless	+ bachelors_orhigher + uninsured_children, data = exemptions_noMMR)
#cluster robust standard errors 
library(clubSandwich)
coef_test(didreg2_noMMR, vcov = "CR2", cluster = exemptions_noMMR$Geography, test = "Satterthwaite")

#model for nonmedical exemptions
didreg3_noMMR = lm(Pers.Rel ~ pre_post + not_cali + did1 + income + household_size + pop + poverty + White_percent + lessthanHS	+ some_college_orless	+ bachelors_orhigher + uninsured_children, data = exemptions_noMMR)
coef_test(didreg3_noMMR, vcov = "CR2", cluster = exemptions_noMMR$Geography, test = "Satterthwaite")

#*******************************************************************************************************#

#*******************************************************************************************************#
