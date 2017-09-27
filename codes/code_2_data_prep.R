###################################
#                                 #
#             SETTINGS            #
#                                 #
###################################

# clearing the memory
rm(list = ls())

# setting directory depending on a user
if (Sys.info()[8] == "lizzzi111")           {setwd("~/Documents/DSG_2017_Finals/")} 
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
p_load(dplyr, data.table, caret, Metrics, xgboost, vtreat, plyr)

# loading all functions
source(file.path(code.folder, "code_0_helper_functions.R"))
source(file.path(code.folder, "code_0_parameters.R"))


###################################
#                                 #
#         CREATING FEATURES       #
#                                 #
###################################

# loading data
load(file.path(data.folder, "data_partitioned.rda"))
load(file.path(data.folder, "data_unknown.rda"))



# Comment Alex: need store the labels of the test subset in a seperate vector first and set the label column to NAs#
#==================================================================================================================#


##### CODES FOR NEW FEATURES: LIZA


##### CODES FOR NEW FEATURES: NATALIA


##### CODES FOR NEW FEATURES: NIKITA


##### CODES FOR NEW FEATURES: OLEKS



###################################
#                                 #
#        AUTOMATIC FEATURES       #
#                                 #
###################################

# data partitioning
data_train <- data_known[data_known$part == "train", ]
data_valid <- data_known[data_known$part == "valid", ]

# adding factor features (Nikita)
data_unknown <- add_factor_features(data_train, data_unknown, target = dv, smooth = 10)
data_known   <- add_factor_features(data_train, data_valid,   target = dv, smooth = 10)
data_train   <- data_known$train
data_valid   <- data_known$valid
data_unknown <- data_unknown$valid

# scaling data
data_unknown <- scale_data(data_train, data_unknown, type = "minmax", except = c(dv, ign_vars))
data_known   <- scale_data(data_train, data_valid,   type = "minmax", except = c(dv, ign_vars))
data_unknown <- data_unknown$valid
data_train   <- data_known$train
data_valid   <- data_known$valid

#adding moments per groups for correlated variabel (Alex) 
#don't use target as "corelated_real_var" since function does no smoothing
data_known <-  moments_per_group_on_real_corelated_var(data_train, data_valid, corelated_real_var = "Age", list(c("PassengerId", "Survived"), c("PassengerId", "Pclass")))
data_train <- data_known$train
data_valid <- data_known$valid

#adding smoothed mean per groups (Alex)
# here we calculate the mean only for the target variable per groups 
data_known <-  smoothed_mean_per_group(data_train = data_train, data_valid = data_valid, target_name = dv, var_groups = list(c("PassengerId", "Survived"), c("PassengerId", "Pclass")), alpha = 10)
data_train <- data_known$train
data_valid <- data_known$valid

# saving data as .RDA
save(data_train, file <-  file.path(data.folder, "data_train_prepared.rda"))
save(data_valid, file <-  file.path(data.folder, "data_valid_prepared.rda"))



