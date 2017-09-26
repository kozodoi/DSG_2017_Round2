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
#         CREATING FEATURES       #
#                                 #
###################################

# loading the data
load(file.path(data.folder, "data_partitioned.rda"))

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
train <- train_data_full[train_data_full$part == "train", ]
valid <- train_data_full[train_data_full$part == "valid", ]

label_valid = valid$Survived
valid$Survived="NA"

# adding factor features (Nikita)
data <- add_factor_features(train, valid, target = dv, smooth = 10)
train <- data$train
valid <- data$valid

# scaling data
data <- scale_data(train, valid, type = "minmax", except = c(dv, id))
train <- data$train
valid <- data$valid

#adding moments per groups for correlated variabel (Alex) 
#don't use target as "corelated_real_var" since function does no smoothing
data = moments_per_group_on_real_corelated_var(train, valid, corelated_real_var = "Age", list(c("PassengerId", "Survived"), c("PassengerId", "Pclass")))
train <- data$train
valid <- data$valid

#adding smoothed mean per groups (Alex)
# here we calculate the mean only for the target variable per groups
data = smoothed_mean_per_group(data_train = train, data_valid = valid, target_name = "Survived", var_groups = list(c("PassengerId", "Survived"), c("PassengerId", "Pclass")), alpha = 10)
train <- data$train
valid <- data$valid

# saving data as .RDA
save(train, file = file.path(data.folder, "data_train_prepared.rda"))
save(valid, file = file.path(data.folder, "data_valid_prepared.rda"))