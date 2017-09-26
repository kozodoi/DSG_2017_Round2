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
#     MODEL LIBRARY - RAW DATA    #
#                                 #
###################################

# loading the data
load(file.path(data.folder, "data_partitioned.rda"))

# data partitioning
train <- train_data_full[train_data_full$part == "train", ]
valid <- train_data_full[train_data_full$part == "valid", ]


# train some models
# predict validation and unknown data
# compute accuracy
# compare everything


#########################################
#                                       #
#   MODEL LIBRARY - WITH AUTO FEATURES  #
#                                       #
#########################################

# loading the data
load(file.path(data.folder, "data_partitioned.rda"))

# data partitioning
train <- train_data_full[train_data_full$part == "train", ]
valid <- train_data_full[train_data_full$part == "valid", ]

# adding factor features (Nikita)
data <- add_factor_features(train, valid, target = dv, smooth = 10)
train <- data$train
valid <- data$valid

# train some models
# predict validation and unknown data
# compute accuracy
# compare everything