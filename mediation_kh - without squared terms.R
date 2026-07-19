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

#turn off scientific notation
options(scipen=999)

##########################
# MEDIATION
##########################
------------------------------
library(mediation) #loading package


# MARGINAL
# ------------------------------
# without exp-med interaction
# ------------------------------
# logit:
# ------------------------------
model1 <- glm(eduyrs ~ hispanic + race + gender + 
              byear_centered + birth_place + parental_edu, 
              family = gaussian, data = hrs) #mediator model

model2 <- glm(dementia ~ hispanic + eduyrs + race + gender + 
              byear_centered + birth_place + parental_edu, 
              family = binomial, data = hrs) #outcome model

set.seed(12345)
mediationmodel <- mediate(
  model1, model2, 
  treat = 'hispanic', mediator = 'eduyrs', 
  covariates = c('race', 'gender', 'byear_centered',
                 'birth_place', 'parental_edu'), 
  treat.value = 'hispanic', control.value = 'non-hispanic', 
  boot = TRUE, boot.ci.type = "perc", sims = 1000
)

summary(mediationmodel)
beepr::beep(5)
# ------------------------------
# probit:
# ------------------------------
model1 <- glm(eduyrs ~ hispanic + race + gender + 
              byear_centered + birth_place + parental_edu, 
              family = gaussian, data = hrs) #mediator model

model2 <- glm(dementia ~ hispanic + eduyrs + race + gender + 
              byear_centered + birth_place + parental_edu, 
              family = binomial('probit'), data = hrs) #outcome model

set.seed(12345)
mediationmodel <- mediate(
  model1, model2, 
  treat = 'hispanic', mediator = 'eduyrs', 
  covariates = c('race', 'gender', 'byear_centered',
                 'birth_place', 'parental_edu'), 
  treat.value = 'hispanic', control.value = 'non-hispanic', 
  boot = TRUE, boot.ci.type = "perc", sims = 1000
)

summary(mediationmodel)
beepr::beep(5)

# CONDITIONAL
# ------------------------------
# without exp-med interaction
# ------------------------------
# logit:
# ------------------------------
model1 <- glm(eduyrs ~ hispanic + race + gender + byear_centered +
              birth_place + parental_edu, 
              family = gaussian, data = hrs) #mediator model

model2 <- glm(dementia ~ hispanic + eduyrs + race + gender + byear_centered + 
              birth_place + parental_edu, 
              family = binomial, data = hrs) #outcome model

set.seed(12345)
mediationmodel <- mediate(
  model1, model2, 
  treat = 'hispanic', mediator = 'eduyrs', 
  covariates = list(
    'race' = 'white', 
    'gender' = 'men', 
    'byear_centered' = 0, 
    'birth_place' = 'born outside USA', 
    'parental_edu' = 0), 
  treat.value = 'hispanic', control.value = 'non-hispanic', 
  boot = TRUE, boot.ci.type = "perc", sims = 1000
)

summary(mediationmodel)
beepr::beep(5)
# ------------------------------
# probit:
# ------------------------------
model1 <- glm(eduyrs ~ hispanic + race + gender + byear_centered +
              birth_place + parental_edu, 
              family = gaussian, data = hrs) #mediator model

model2 <- glm(dementia ~ hispanic + eduyrs + race + gender + byear_centered + 
              birth_place + parental_edu, 
              family = binomial('probit'), data = hrs) #outcome model

set.seed(12345)
mediationmodel <- mediate(
  model1, model2, 
  treat = 'hispanic', mediator = 'eduyrs', 
  covariates = list(
    'race' = 'white', 
    'gender' = 'men', 
    'byear_centered' = 0, 
    'birth_place' = 'born outside USA', 
    'parental_edu' = 0), 
  treat.value = 'hispanic', control.value = 'non-hispanic', 
  boot = TRUE, boot.ci.type = "perc", sims = 1000
)

summary(mediationmodel)
beepr::beep(5)


# MARGINAL
# ---------------------------
# with exp-med interaction
# ---------------------------
# logit:
# ---------------------------
model1 <- glm(eduyrs ~ hispanic + race + gender + byear_centered +
              birth_place + parental_edu, 
              family = gaussian, data = hrs) #mediator model

model2 <- glm(dementia ~ hispanic + eduyrs + hispanic*eduyrs + race + gender + 
                byear_centered + birth_place + parental_edu, 
              family = binomial, data = hrs) #outcome model

set.seed(12345)
mediationmodel <- mediate(
  model1, model2, 
  treat = 'hispanic', mediator = 'eduyrs', 
  covariates = c('race', 'gender', 'byear_centered',
                 'birth_place', 'parental_edu'), 
  treat.value = 'hispanic', control.value = 'non-hispanic', 
  boot = TRUE, boot.ci.type = "perc", sims = 1000
)

summary(mediationmodel)
beepr::beep(5)
# ---------------------------
# probit:
# ---------------------------
model1 <- glm(eduyrs ~ hispanic + race + gender + byear_centered + 
                birth_place + parental_edu, 
              family = gaussian, data = hrs) #mediator model

model2 <- glm(dementia ~ hispanic + eduyrs + hispanic*eduyrs + race + gender + 
                byear_centered + birth_place + parental_edu, 
              family = binomial('probit'), data = hrs) #outcome model

set.seed(12345)
mediationmodel <- mediate(
  model1, model2, 
  treat = 'hispanic', mediator = 'eduyrs', 
  covariates = c('race', 'gender', 'byear_centered',
                 'birth_place', 'parental_edu'), 
  treat.value = 'hispanic', control.value = 'non-hispanic', 
  boot = TRUE, boot.ci.type = "perc", sims = 1000
)

summary(mediationmodel)
beepr::beep(5)

# CONDITIONAL
# ---------------------------
# with exp-med interaction
# ---------------------------
# logit:
# ---------------------------
model1 <- glm(eduyrs ~ hispanic + race + gender + byear_centered +
                birth_place + parental_edu, 
              family = gaussian, data = hrs) #mediator model

model2 <- glm(dementia ~ hispanic + eduyrs + hispanic*eduyrs + race + gender + 
                byear_centered + birth_place + parental_edu, 
              family = binomial, data = hrs) #outcome model

set.seed(12345)
mediationmodel <- mediate(
  model1, model2, 
  treat = 'hispanic', mediator = 'eduyrs', 
  covariates = list(
    'race' = 'white', 
    'gender' = 'men', 
    'byear_centered' = 0, 
    'birth_place' = 'born outside USA', 
    'parental_edu' = 0), 
  treat.value = 'hispanic', control.value = 'non-hispanic', 
  boot = TRUE, boot.ci.type = "perc", sims = 1000
)

summary(mediationmodel)
beepr::beep(5)
# ---------------------------
# probit:
# ---------------------------
model1 <- glm(eduyrs ~ hispanic + race + gender + byear_centered +
                birth_place + parental_edu, 
              family = gaussian, data = hrs) #mediator model

model2 <- glm(dementia ~ hispanic + eduyrs + hispanic*eduyrs + race + gender + 
                byear_centered + birth_place + parental_edu, 
              family = binomial('probit'), data = hrs) #outcome model

set.seed(12345)
mediationmodel <- mediate(
  model1, model2, 
  treat = 'hispanic', mediator = 'eduyrs', 
  covariates = list(
    'race' = 'white', 
    'gender' = 'men', 
    'byear_centered' = 0, 
    'birth_place' = 'born outside USA', 
    'parental_edu' = 0), 
  treat.value = 'hispanic', control.value = 'non-hispanic', 
  boot = TRUE, boot.ci.type = "perc", sims = 1000
)

summary(mediationmodel)
beepr::beep(5)


# --------------------------
# continuous outcome
# --------------------------
# MARGINAL
# --------------------------
model1 <- glm(eduyrs ~ hispanic + race + gender + byear_centered +
                birth_place + parental_edu, 
              family = gaussian, data = hrs) #mediator model

model2 <- glm(total_recall_imp ~ hispanic + eduyrs + race + gender + byear_centered + 
                birth_place + parental_edu, 
              family = gaussian, data = hrs) #outcome model

set.seed(12345)
mediationmodel <- mediate(
  model1, model2, 
  treat = 'hispanic', mediator = 'eduyrs', 
  covariates = c('race', 'gender', 'byear_centered', 
                 'birth_place', 'parental_edu'), 
  treat.value = 'hispanic', control.value = 'non-hispanic', 
  boot = TRUE, boot.ci.type = "perc", sims = 1000
)

summary(mediationmodel)
beepr::beep(5)

# ---------------------------
# CONDITIONAL
# ---------------------------
model1 <- glm(eduyrs ~ hispanic + race + gender + byear_centered +
                birth_place + parental_edu, 
              family = gaussian, data = hrs) #mediator model

model2 <- glm(total_recall_imp ~ hispanic + eduyrs + race + gender + byear_centered + 
                birth_place + parental_edu, 
              family = gaussian, data = hrs) #outcome model

set.seed(12345)
mediationmodel <- mediate(
  model1, model2, 
  treat = 'hispanic', mediator = 'eduyrs', 
  covariates = list(
    'race' = 'white',
    'gender' = 'men', 
    'byear_centered' = 0, 
    'birth_place' = 'born outside USA', 
    'parental_edu' = 0), 
  treat.value = 'hispanic', control.value = 'non-hispanic', 
  boot = TRUE, boot.ci.type = "perc", sims = 1000
)

summary(mediationmodel)
beepr::beep(5)
