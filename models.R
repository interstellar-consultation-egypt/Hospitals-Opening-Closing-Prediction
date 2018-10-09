library(tidyverse)
library(h2o)

#########################
# Base Model (no pca)
########################
train <- read.csv("1.train_dta_log_num.csv")
train$OCN <- as.numeric(train$OC)

# Feature Selection
base.mod <- lm( as.numeric(OC) ~ 1 , data = train)  # base intercept only model
all.mod <- lm( as.numeric(OC) ~ . , data = train) # full model with all predictors
stepMod <-
  step(
    base.mod,
    scope = list(lower = base.mod, upper = all.mod),
    direction = "both",
    trace = 0,
    steps = 1000
  )  # perform step-wise algorithm
shortlistedVars <-
  names(unlist(stepMod[[1]])) # get the shortlisted variable.
shortlistedVars <-
  shortlistedVars[!shortlistedVars %in% "(Intercept)"]  # remove intercept
print(shortlistedVars)
summary(stepMod)

localH2O = h2o.init()
trainPath = system.file("extdata", "1.train_dta_log_num.csv")
train.hex <- h2o.importFile(path = "1.train_dta_log_num.csv")
train.pca <-
  h2o.prcomp(training_frame = train.hex,
             k = 8,
             transform = "STANDARDIZE")
summary(train.pca)
# The model is extremely week we will have to try pca now

