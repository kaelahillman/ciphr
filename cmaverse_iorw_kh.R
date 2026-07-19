##########################
# CIPHR SOFTWARE PROJECT
##########################

#load packages
if(!require("packman")) install.packages("pacman")
pacman::p_load(rio, here, devtools, tidyverse, skimr)

#import empirical example
hrs <- rio::import(here("r", "CIPHR","data", "HRS_software_project_v2.dta")) 

#define labels of categorical variables
hrs$hispanic <- factor(hrs$hispanic, c(0,1), labels=c('non-hispanic', 'hispanic'))
hrs$gender <- factor(hrs$gender, c(0,1), labels=c('men','women'))
hrs$race <- factor(hrs$race, c(0,1,2), labels=c('white', 'black', 'other'))
hrs$birth_place <- factor(hrs$birth_place, c(0,1), labels=c('born outside USA', 'born in USA'))
hrs$dementia <- factor(hrs$dementia, c(0,1), labels=c('No dementia','dementia'))

#remove missing
hrs <- hrs %>%
  select(-c(total_recall))

#turn off scientific notation
options(scipen=999)

##########################################
# CMAverse: inverse odds-ratio weighting
##########################################

devtools::install_github("BS1125/CMAverse")
library(CMAverse)

# ***********************************************************************************
# ***********************************************************************************
# Estimation of effects for a mediation model REGARDLESS exposure-mediator interaction
# ***********************************************************************************
# ***********************************************************************************

# --------------------------------
# ereg logistic, yreg logistic
# --------------------------------
est_iorw <- cmest(
  data = hrs, model = "iorw", 
  outcome = "dementia", exposure = "hispanic", mediator = c("eduyrs"), 
  basec = c("race", "gender", "byear_centered", "sq_byear_centered", 
            "birth_place", "parental_edu", "sq_parental_edu"), 
  ereg = "logistic", yreg = "logistic",
  astar = "non-hispanic", a = "hispanic",
  mval = list(0),
  estimation = "imputation", 
  inference = "bootstrap", nboot = 1000, boot.ci.type = "per")

summary(est_iorw)
beepr::beep(5)

# --------------------------------
# ereg logistic, yreg loglinear
# --------------------------------
hrs_y_num <- hrs %>%
  mutate(dementia = ifelse(dementia == "No dementia", 0, 1)) # create new dataset with numeric y 

est_iorw <- cmest(
  data = hrs_y_num, model = "iorw", 
  outcome = "dementia", exposure = "hispanic", mediator = c("eduyrs"), 
  basec = c("race", "gender", "byear_centered", "sq_byear_centered", 
            "birth_place", "parental_edu", "sq_parental_edu"), 
  ereg = "logistic", yreg = "loglinear", 
  astar = "non-hispanic", a = "hispanic",
  mval = list(0),
  estimation = "imputation", 
  inference = "bootstrap", nboot = 1000, boot.ci.type = "per")

summary(est_iorw)
beepr::beep(5)


# ***********************************************************************************
# ***********************************************************************************
# Estimation of effects for a mediation model with a CONTINUOUS outcome
# ***********************************************************************************
# ***********************************************************************************

# --------------------------------
# ereg logistic, yreg linear
# --------------------------------
est_iorw <- cmest(
  data = hrs, model = "iorw", 
  outcome = "total_recall_imp", exposure = "hispanic", mediator = c("eduyrs"), 
  basec = c("race", "gender", "byear_centered", "sq_byear_centered", 
            "birth_place", "parental_edu", "sq_parental_edu"), 
  ereg = "logistic", yreg = "linear",  
  astar = "non-hispanic", a = "hispanic",
  mval = list(0),
  estimation = "imputation", 
  inference = "bootstrap", nboot = 1000, boot.ci.type = "per")

summary(est_iorw)
beepr::beep(5)

