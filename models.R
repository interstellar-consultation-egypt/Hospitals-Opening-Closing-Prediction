library(tidyverse)
library(h2o)
library(ggbiplot)

#########################
# Base Model (no pca)
########################
train <- read.csv("3.train_dta_log_num_min_max.csv")

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

# The model is extremely week we will have to try pca now

#############################################################
# PCA first model
#############################################################
glimpse(train)
debt3_idx <- grep("debt3", colnames(train))
train.pca <- prcomp(train[, -c(1:2, 4, 6, debt3_idx)], center = TRUE, scale. = TRUE)
summary(train.pca)
ggbiplot(train.pca)
train.new <- train.pca$x[, 1:26] # explains 100% of data
train.new <- as.tibble(train.new)

# add factor data
train.new$instkind <- as.numeric(train$instkind)
train.new$ownerChange <- as.numeric(train$ownerChange)
train.new$OC <- train$OC
# try to perform FS again
base.mod <- lm( as.numeric(OC) ~ 1 , data = train.new)  # base intercept only model
all.mod <- lm( as.numeric(OC) ~ . , data = train.new) # full model with all predictors
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

y.dep <- 29
x.indep <- c(7, 13, 24, 26, 28)
ntrees_opt <- c(400, 600, 800, 1000, 1200)
maxdepth_opt <- c(6, 8, 10, 12, 14, 16)
hyper_parameters <- list(ntrees=ntrees_opt,
                         max_depth=maxdepth_opt)

# Multiple Regression 
regression.model <- h2o.glm( y = y.dep, x = x.indep, training_frame = as.h2o(train.new), family = "binomial")
h2o.performance(regression.model)

# test date
test <- read.csv("2.test_dta_min_max.csv")
glimpse(test)
debt3_idx <- grep("debt3", colnames(test))
test.pca <- prcomp(test[, -c(1:2, 4, 6, debt3_idx)], center = TRUE, scale. = TRUE)
summary(test.pca)
ggbiplot(test.pca)
test.new <- test.pca$x[,1:26] # explains 90% of data
test.new <- as.tibble(test.new)

# add factor data
test.new$instkind <- as.numeric(test$instkind)
test.new$ownerChange <- as.numeric(test$ownerChange)
test.new$OC <- test$OC
h2o.init()
predict.reg <- as.data.frame(h2o.predict(regression.model, as.h2o(test.new[, x.indep])))
OC_reg <- data.frame(inst_id = test$inst_id, OC = as.numeric(predict.reg$predict)-1)
write.csv(OC_reg, file = "OC_reg_4.csv", quote = FALSE, row.names=FALSE)

##########################
## Random Forest
##########################
#  system.time(
# rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 1000, mtries = 3, max_depth = 4, seed = 1122)
# )
# h2o.performance(rforest.model)
# h2o.varimp(rforest.model)
train.h2o <- as.h2o(train.new)
test.h2o <- as.h2o(test.new)
r <- h2o.runif(train.h2o)
trainHex.split <- h2o.splitFrame(train.h2o, ratios=.75)

grid <- h2o.grid("randomForest", hyper_params = hyper_parameters,
                 y = y.dep, x = x.indep,
                 seed = 123,
                 training_frame = trainHex.split[[1]],
                 validation_frame = trainHex.split[[2]])
# print out all prediction errors and run times of the models
grid

# print out the mse for all of the models
model_ids <- grid@model_ids
mse <- vector(mode="numeric", length=0)
grid_models <- lapply(model_ids, function(model_id) { model = h2o.getModel(model_id) })
for (i in 1:length(grid_models)) {
  print(sprintf("mse: %f", h2o.mse(grid_models[[i]])))
  mse[i] <- h2o.mse(grid_models[[i]])
}

best_id <- model_ids[order(mse,decreasing=F)][1]
best_id

fit.best <- h2o.getModel(model_id = best_id[[1]])
h2o.varimp(fit.best)

system.time(predict.rforest <- as.data.frame(h2o.predict(fit.best, test.h2o)))
OC_rf <- data.frame(inst_id = test$inst_id, OC = predict.rforest$predict)
write.csv(OC_rf, file = "OC_rf_4.csv", quote = FALSE, row.names=FALSE)

 system.time(
rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o,
                                  ntrees = 800, mtries = 3, max_depth = 10, seed = 1122)
)
h2o.performance(rforest.model)
h2o.varimp(rforest.model)
predict.rforest <- as.data.frame(h2o.predict(rforest.model, test.h2o))
OC_rf <- data.frame(inst_id = test$inst_id, OC = as.numeric(predict.rforest$predict) - 1)
write.csv(OC_rf, file = "OC_rf_4.csv", quote = FALSE, row.names=FALSE)

###############################################
## GBM
#================================================
  # system.time(
  # gbm.model <- h2o.gbm(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 1100, max_depth = 6, learn_rate = 0.01, seed = 1122)
  # )
  # h2o.performance (gbm.model)
  grid <- h2o.grid("gbm", hyper_params = hyper_parameters,
                   y = y.dep, x = x.indep,
                   distribution="AUTO",
                   training_frame = trainHex.split[[1]],
                   validation_frame = trainHex.split[[2]])
grid

# print out the mse for all of the models
model_ids <- grid@model_ids
mse <- vector(mode="numeric", length=0)
grid_models <- lapply(model_ids, function(model_id) { model = h2o.getModel(model_id) })
for (i in 1:length(grid_models)) {
  print(sprintf("mse: %f", h2o.mse(grid_models[[i]])))
  mse[i] <- h2o.mse(grid_models[[i]])
}


best_id <- model_ids[order(mse,decreasing=F)][1]
best_id

fit.best <- h2o.getModel(model_id = best_id[[1]])
h2o.varimp(fit.best)
RMPSE<- function(predicted, actual) {
  rmpse <- sqrt(mean((actual/predicted-1)^2))
  return(list(metric = "RMPSE", value = rmpse))
}

gbm.model <- h2o.gbm(y=y.dep, x=x.indep, training_frame = train.h2o,
                     ntrees = 1600, max_depth = 12, learn_rate = 0.01, seed = 1122)

h2o.performance (gbm.model)
predict.gbm <- as.data.frame(h2o.predict(gbm.model, test.h2o))

OC_gbm <- data.frame(inst_id = test$inst_id, OC = as.numeric(predict.gbm$predict) - 1)
write.csv(OC_gbm, file = "OC_gbm_4.csv", quote = FALSE, row.names=FALSE)
