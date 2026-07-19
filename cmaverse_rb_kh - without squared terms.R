##########################
# CIPHR SOFTWARE PROJECT
##########################

#load packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(rio, here, tidyverse, skimr, beepr)

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

#summarize data
skimr::skim(hrs)

#turn off scientific notation
options(scipen=999)

##############################
# CMAverse: regression-based
##############################

install.packages('devtools')
devtools::install_github("BS1125/CMAverse")
library(CMAverse)

# ***********************************************************************************
# ***********************************************************************************
# Estimation of effects for a mediation model WITHOUT exposure-mediator interaction
# ***********************************************************************************
# ***********************************************************************************
# MARGINAL
# --------------------------------
# Closed-form parameter, logistic
# --------------------------------
est <- cmest(
  data = hrs, model = "rb",
  outcome = "dementia", exposure = "hispanic", mediator = c("eduyrs"), 
  basec = c("race", "gender", "byear_centered", 
            "birth_place", "parental_edu"), 
  EMint = FALSE, 
  yreg = "logistic", mreg = list("linear"), 
  astar = "non-hispanic", a = "hispanic", 
  mval = list(0),
  estimation = "paramfunc", 
  inference = "bootstrap", nboot = 1000, boot.ci.type = "per")

summary(est)
beepr::beep(5)

# MARGINAL
# --------------------------------
# Closed-form parameter, loglinear
# --------------------------------
hrs_y_num <- hrs %>%
  mutate(dementia = ifelse(dementia == "No dementia", 0, 1)) # create new dataset with numeric y

table(hrs_y_num$dementia) #check

est <- cmest(
  data = hrs_y_num, model = "rb", 
  outcome = "dementia", exposure = "hispanic", mediator = c("eduyrs"), 
  basec = c("race", "gender", "byear_centered", 
            "birth_place", "parental_edu"), 
  EMint = FALSE, 
  yreg = "loglinear", mreg = list("linear"), 
  astar = "non-hispanic", a = "hispanic",
  mval = list(0),
  estimation = "paramfunc", 
  inference = "bootstrap", nboot = 1000, boot.ci.type = "per")

summary(est)
beepr::beep(5)

# MARGINAL
# --------------------------------
# Imputation, logistic
# --------------------------------
est <- cmest(
  data = hrs, model = "rb", 
  outcome = "dementia", exposure = "hispanic", mediator = c("eduyrs"), 
  basec = c("race", "gender", "byear_centered", 
            "birth_place", "parental_edu"), 
  EMint = FALSE, 
  yreg = "logistic", mreg = list("linear"), 
  astar = "non-hispanic", a = "hispanic",
  mval = list(0),
  estimation = "imputation", 
  inference = "bootstrap", nboot = 1000, boot.ci.type = "per")

summary(est)
beepr::beep(5)

# MARGINAL
# --------------------------------
# Imputation, loglinear
# --------------------------------
est <- cmest(
  data = hrs_y_num, model = "rb", 
  outcome = "dementia", exposure = "hispanic", mediator = c("eduyrs"), 
  basec = c("race", "gender", "byear_centered", 
            "birth_place", "parental_edu"), 
  EMint = FALSE, 
  yreg = "loglinear", mreg = list("linear"), 
  astar = "non-hispanic", a = "hispanic",
  mval = list(0),
  estimation = "imputation", 
  inference = "bootstrap", nboot = 1000, boot.ci.type = "per")

summary(est)
beepr::beep(5)


# ***********************************************************************************
# ***********************************************************************************
# Estimation of effects for a mediation model WITH exposure-mediator interaction
# ***********************************************************************************
# ***********************************************************************************
# MARGINAL
# --------------------------------
# Closed-form parameter, logistic
# --------------------------------
est <- cmest(
  data = hrs, model = "rb", 
  outcome = "dementia", exposure = "hispanic", mediator = c("eduyrs"), 
  basec = c("race", "gender", "byear_centered", 
            "birth_place", "parental_edu"), 
  EMint = TRUE, 
  yreg = "logistic", mreg = list("linear"), 
  astar = "non-hispanic", a = "hispanic",
  mval = list(0),
  estimation = "paramfunc", 
  inference = "bootstrap", nboot = 1000, boot.ci.type = "per")

summary(est)
beepr::beep(5)

# MARGINAL
# --------------------------------
# Closed-form parameter, loglinear
# --------------------------------
est <- cmest(
  data = hrs_y_num, model = "rb", 
  outcome = "dementia", exposure = "hispanic", mediator = c("eduyrs"), 
  basec = c("race", "gender", "byear_centered", 
            "birth_place", "parental_edu"), 
  EMint = TRUE, 
  yreg = "loglinear", mreg = list("linear"), 
  astar = "non-hispanic", a = "hispanic",
  mval = list(0),
  estimation = "paramfunc", 
  inference = "bootstrap", nboot = 1000, boot.ci.type = "per")

summary(est)
beepr::beep(5)

# MARGINAL
# --------------------------------
# Imputation, logistic
# --------------------------------
est <- cmest(
  data = hrs, model = "rb", 
  outcome = "dementia", exposure = "hispanic", mediator = c("eduyrs"), 
  basec = c("race", "gender", "byear_centered", 
            "birth_place", "parental_edu"), 
  EMint = TRUE, 
  yreg = "logistic", mreg = list("linear"), 
  astar = "non-hispanic", a = "hispanic",
  mval = list(0),
  estimation = "imputation", 
  inference = "bootstrap", nboot = 1000, boot.ci.type = "per")

summary(est)
beepr::beep(5)

# MARGINAL
# --------------------------------
# Imputation, loglinear
# --------------------------------
est <- cmest(
  data = hrs_y_num, model = "rb", 
  outcome = "dementia", exposure = "hispanic", mediator = c("eduyrs"), 
  basec = c("race", "gender", "byear_centered", 
            "birth_place", "parental_edu"), 
  EMint = TRUE, 
  yreg = "loglinear", mreg = list("linear"), 
  astar = "non-hispanic", a = "hispanic",
  mval = list(0),
  estimation = "imputation", 
  inference = "bootstrap", nboot = 1000, boot.ci.type = "per")

summary(est)
beepr::beep(5)

# CONDITIONAL
# --------------------------------
# Closed-form parameter, logistic
# --------------------------------
est <- cmest(
  data = hrs, model = "rb", 
  outcome = "dementia", exposure = "hispanic", mediator = c("eduyrs"), 
  basec = c("race", "gender", "byear_centered", 
            "birth_place", "parental_edu"), 
  EMint = TRUE, 
  yreg = "logistic", mreg = list("linear"), 
  astar = "non-hispanic", a = "hispanic", 
  mval = list(0),
  basecval = list("race" = "white", "gender" = "men", "byear_centered" = 0,
                  "birth_place" = "born outside USA", "parental_edu" = 0), 
  estimation = "paramfunc", 
  inference = "bootstrap", nboot = 1000, boot.ci.type = "per")

summary(est)
beepr::beep(5)

# CONDITIONAL
# --------------------------------
# Closed-form parameter, loglinear
# --------------------------------
est <- cmest(
  data = hrs_y_num, model = "rb", 
  outcome = "dementia", exposure = "hispanic", mediator = c("eduyrs"), 
  basec = c("race", "gender", "byear_centered", 
            "birth_place", "parental_edu"), 
  EMint = TRUE, 
  yreg = "loglinear", mreg = list("linear"), 
  astar = "non-hispanic", a = "hispanic", 
  mval = list(0),
  basecval = list("race" = "white", "gender" = "men", "byear_centered" = 0,
                  "birth_place" = "born outside USA", "parental_edu" = 0), 
  estimation = "paramfunc", 
  inference = "bootstrap", nboot = 1000, boot.ci.type = "per")

summary(est)
beepr::beep(5)


# ***********************************************************************************
# ***********************************************************************************
# Estimation of effects for a mediation model with a CONTINUOUS outcome
# ***********************************************************************************
# ***********************************************************************************
# MARGINAL
# --------------------------------
# Closed-form parameter
# --------------------------------
est <- cmest(
  data = hrs, model = "rb", 
  outcome = "total_recall_imp", exposure = "hispanic", mediator = c("eduyrs"), 
  basec = c("race", "gender", "byear_centered", 
            "birth_place", "parental_edu"), 
  EMint = FALSE, 
  yreg = "linear", mreg = list("linear"), 
  astar = "non-hispanic", a = "hispanic",
  mval = list(0),
  estimation = "paramfunc", 
  inference = "bootstrap", nboot = 1000, boot.ci.type = "per")

summary(est)
beepr::beep(5)

# MARGINAL
# --------------------------------
# Imputation
# --------------------------------
est <- cmest(
  data = hrs, model = "rb", 
  outcome = "total_recall_imp", exposure = "hispanic", mediator = c("eduyrs"), 
  basec = c("race", "gender", "byear_centered", 
            "birth_place", "parental_edu"), 
  EMint = FALSE, 
  yreg = "linear", mreg = list("linear"), 
  astar = "non-hispanic", a = "hispanic",
  mval = list(0),
  estimation = "imputation", 
  inference = "bootstrap", nboot = 1000, boot.ci.type = "per")

summary(est)
beepr::beep(5)

