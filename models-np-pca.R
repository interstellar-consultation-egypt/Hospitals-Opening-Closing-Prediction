library(tidyverse)
library(h2o)
library(caret)

h2o.init()
train <- read.csv("9c.train_dta_z_transform.csv")
test <- read.csv("9c.test_dta_z_transform.csv")
########################################
## GLM
########################################
base.mod <- lm( as.numeric(OC) ~ 1 , data = train)  # base intercept only model
all.mod <- lm( as.numeric(OC) ~ . ,
               data = train[, -c(grep("debt3", colnames(train)))]) # full model with all predictors
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
#lm(formula = as.numeric(OC) ~ salescost3 + ownerChange_change + 
#instkind_traditional_clinic, data = train)
y.dep <- 2
x.indep <- c(grep("sga3", colnames(train)), 
             grep("ownerChange_change", colnames(train)),
             grep("revenue3", colnames(train)),
             grep("salary3", colnames(train)),
             grep("ctax3", colnames(train)),
             grep("noe3", colnames(train)),
             grep("netAsset3 ", colnames(train)),
             grep("instkind_traditiona_hospital", colnames(train)),
             grep("instkind_traditional_clinic", colnames(train)))

# Multiple Regression 
regression.model <- h2o.glm( y = y.dep, x = x.indep, training_frame = as.h2o(train), family = "binomial")
h2o.performance(regression.model)

predict.reg <- as.data.frame(h2o.predict(regression.model, as.h2o(test[, x.indep])))
OC_reg <- data.frame(inst_id = test$inst_id, OC = as.numeric(predict.reg$predict)-1)
write.csv(OC_reg, file = "OC_reg_9c.csv", quote = FALSE, row.names=FALSE)

##########################
## Random Forest
##########################
train.h2o <- as.h2o(train)
test.h2o <- as.h2o(test)
r <- h2o.runif(train.h2o)
trainHex.split <- h2o.splitFrame(train.h2o, ratios=.70)

grid <- h2o.grid("randomForest",
                 search_criteria = list(
                   strategy = "RandomDiscrete",
                   stopping_tolerance = 0.001,
                   stopping_rounds = 10,
                   max_runtime_secs = 120
                 ),
                 hyper_params = list(
                   ntrees = c(50, 100, 150, 200, 250),
                   #mtries = c(2, 3, 4, 5),
                   sample_rate = c(0.5, 0.632, 0.8, 0.95),
                   col_sample_rate_per_tree = c(0.5, 0.9, 1.0)
                 ),
                 y = y.dep, 
                 x = x.indep,
                 seed = 123,
                 training_frame = trainHex.split[[1]],
                 validation_frame = trainHex.split[[2]],
                 nfolds = 5, max_depth = 40,
                 stopping_metric = "AUC",
                 stopping_tolerance = 0,
                 stopping_rounds = 4,
                 score_tree_interval = 3,
                 binomial_double_trees = TRUE)
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
h2o.performance(fit.best)

system.time(predict.rforest <- as.data.frame(h2o.predict(fit.best, test.h2o)))
OC_rf <- data.frame(inst_id = test$inst_id, OC = as.numeric(predict.rforest$predict) - 1)
write.csv(OC_rf, file = "OC_rf_9c.csv", quote = FALSE, row.names=FALSE)
###############################################
## GBM
#================================================
# system.time(
# gbm.model <- h2o.gbm(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 1100, max_depth = 6, learn_rate = 0.01, seed = 1122)
# )
# h2o.performance (gbm.model)
grid <- h2o.grid("gbm",grid_id = "my_grid",
                 
                 hyper_params = list(
                   max_depth = c(5, 20, 50),
                   min_rows = c(2, 5, 10),
                   sample_rate = c(0.5, 0.8),# 0.95, 1.0),
                   #col_sample_rate = c(0.5, 0.8),# 0.95, 1.0),
                   #col_sample_rate_per_tree = c(0.8, 0.99, 1.0),
                   learn_rate = c(0.1),  #Placemarker
                   ntrees = c(50, 100, 150),# 200, 250),
                   seed = c(701)  #Placemarker
                 ),
                 y = y.dep, x = x.indep,
                 stopping_tolerance = 0.001,
                 stopping_rounds=3,
                 score_tree_interval = 10,
                 distribution="bernoulli",
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
h2o.performance(fit.best)
# RMPSE<- function(predicted, actual) {
#   rmpse <- sqrt(mean((actual/predicted-1)^2))
#   return(list(metric = "RMPSE", value = rmpse))
# }

# gbm.model <- h2o.gbm(y=y.dep, x=x.indep, training_frame = train.h2o,
#                      ntrees = 1600, max_depth = 12, learn_rate = 0.01, seed = 1122)

# h2o.performance (gbm.model)
predict.gbm <- as.data.frame(h2o.predict(fit.best, test.h2o))

OC_gbm <- data.frame(inst_id = test$inst_id, OC = as.numeric(predict.gbm$predict))
write.csv(OC_gbm, file = "OC_gbm_6.csv", quote = FALSE, row.names=FALSE)

#==============================================
# SVM
#==============================================
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)

svm_Linear <- train(OC ~ salescost3 + ownerChange_change + 
                      instkind_traditional_clinic,
                    data = train, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svm_pred <- predict(svm_Linear, newdata = test)
svm_pred

