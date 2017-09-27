###################################
#                                 #
#             SETTINGS            #
#                                 #
###################################

# clearing the memory
rm(list = ls())

# setting directory depending on a user
if (Sys.info()[8] == "lizzzi111")           {setwd("~/DSG_2017_Finals/")} 
if (Sys.info()[8] == "kozodoi")             {setwd("~/Documents/Competitions/DSG_2017_Finals/")}
if (Sys.info()[8] == "nataliasverchkova")   {setwd("~/Documents/DSG/DSG_2017_Finals/")}
if (Sys.info()[8] == "oleksiyostapenko")    {setwd("/Users/oleksiyostapenko/Documents/HU_Berlin/ML/DSG/DSG_2017_Finals")}

# setting inner folders
code.folder <- "codes"
data.folder <- "data"
func.folder <- "functions"
subm.folder <- "submissions"

# loading libraries
if (require(pacman) == FALSE) install.packages("pacman")
library(pacman)
p_load(dplyr, data.table, caret, Metrics, xgboost, vtreat)

# loading all functions
source(file.path(code.folder, "code_0_helper_functions.R"))
source(file.path(code.folder, "code_0_parameters.R"))


###################################
#                                 #
#        XGBOOST - RAW DATA       #
#                                 #
###################################

# loading the data
load(file.path(data.folder, "data_partitioned.rda"))
load(file.path(data.folder, "data_unknown.rda"))

# data partitioning
data_train <- data_known[data_known$part == "train", ]
data_valid <- data_known[data_known$part == "valid", ]


# train xgboost model
#! handles only numerical values, therefor vtreat as encoder
treatmentPlan <-  create_vtreat_treatPlan(data_train = data_train, label = dv, binary_class = TRUE)
data_train_treat <-  vtreat_vars(data = data_train, label = dv, treatplan = treatmentPlan, pruneLevel = NULL)
data_valid_treat <- vtreat_vars(data = data_valid, label = dv, treatplan = treatmentPlan, pruneLevel = NULL)
data_unknown_treat <- vtreat_vars(data = data_unknown, label = dv, treatplan = treatmentPlan, pruneLevel = NULL)

# model
model1 <- base_line_model(data_train = data_train_treat, train_label = data_train_treat[[dv]], data_valid = data_valid_treat, valid_label = data_valid_treat[[dv]], target = dv, nrounds = 500, type = "c")

# predict validation data
pred <-  predict(model1, as.matrix( data_valid_treat[, !names(data_valid_treat) %in% c(dv)] ))
prediction <- as.numeric(pred > 0.5)
auc(data_valid_treat[[dv]], prediction)

# predict unknown data
pred1 <- predict(model1, as.matrix( data_unknown_treat[, !names(data_valid_treat) %in% c(dv)] ))
submit(pred1, data = data_unknown, id.var = "PassengerId", target.var = "Survived", folder = subm.folder, file = "test_no_features.csv", binary = T)


###################################
#                                 #
#   XGBOOST - WITH AUTO FEATURES  #
#                                 #
###################################

# loading the data
load(file.path(data.folder, "data_partitioned.rda"))
load(file.path(data.folder, "data_unknown.rda"))

# data partitioning
data_train <- data_known[data_known$part == "train", ]
data_valid <- data_known[data_known$part == "valid", ]

# adding factor features (Nikita)
data_unknown <- add_factor_features(data_train, data_unknown, target = dv, smooth = 10)
data_known   <- add_factor_features(data_train, data_valid,   target = dv, smooth = 10)
data_train   <- data_known$train
data_valid   <- data_known$valid
data_unknown <- data_unknown$valid




#adding moments per groups for correlated variabel (Alex) 
#don't use target as "corelated_real_var" since function does no smoothing
data_unknown <-  moments_per_group_on_real_corelated_var(data_train, data_unknown, corelated_real_var = "Age", list(c("PassengerId", "Pclass")))
data_known <-  moments_per_group_on_real_corelated_var(data_train, data_valid, corelated_real_var = "Age", list(c("PassengerId", "Pclass")))
data_train <- data_known$train
data_valid <- data_known$valid
data_unknown <- data_unknown$valid

#adding smoothed mean per groups (Alex)
# here we calculate the mean only for the target variable per groups 
# data_known <-  smoothed_mean_per_group(data_train = data_train, data_valid = data_valid, target_name = dv, var_groups = list(c("Age", "Pclass")), alpha = 10)
# data_unknown <-  smoothed_mean_per_group(data_train = data_train, data_valid = data_unknown, target_name = dv, var_groups = list( c("Age", "Pclass")), alpha = 10)
# data_train <- data_known$train
# data_valid <- data_known$valid
# data_unknown <- data_unknown$valid


# train xgboost model
iv <- colnames(data_train)[!(colnames(data_train)%in%c(dv, ign_vars))]
treatmentPlan <- designTreatmentsC(data_train, varlist = iv, outcomename = dv, outcometarget = TRUE)
#treatmentPlan <- designTreatmentsN(data_train, varlist = iv, outcomename = dv, outcometarget = TRUE)

# vtreat
data_train_treat   <- vtreat_vars(data = data_train,   label = dv, treatplan = treatmentPlan, pruneLevel = NULL)
data_valid_treat   <- vtreat_vars(data = data_valid,   label = dv, treatplan = treatmentPlan, pruneLevel = NULL)
data_unknown_treat <- vtreat_vars(data = data_unknown, label = dv, treatplan = treatmentPlan, pruneLevel = NULL)

# model
model2 <- base_line_model(data_train = data_train_treat, train_label = data_train_treat[[dv]], data_valid = data_valid_treat, valid_label = data_valid_treat[[dv]], target = dv, nrounds = 500, type = "c")

# predict validation data
pred2 <-  predict(model2, as.matrix( data_valid_treat[, !names(data_valid_treat) %in% c(dv)] ))
prediction2 <- as.numeric(pred2 > 0.5)
auc(data_valid_treat[[dv]], prediction2)

varimp <- xgb.importance(colnames(data_train_treat), model2)
xgb.plot.importance(importance_matrix = varimp)

# predict unknown data
pred2 <- predict(model2, as.matrix( data_unknown_treat[, !names(data_valid_treat) %in% c(dv)] ))
submit(pred2, data = data_unknown, id.var = "PassengerId", target.var = "Survived", folder = subm.folder, file = "test_fare.csv", binary = T)




###################################
#                                 #
#   XGBOOST - WITH ALL FEATURES   #
#                                 #
###################################

# loading the prepared data
load(file.path(data.folder, "data_train_prepared.rda"))
load(file.path(data.folder, "data_valid_prepared.rda"))


# train xgboost model
# predict validation data
# compute accuracy
# predict unknown data
# submit predictions