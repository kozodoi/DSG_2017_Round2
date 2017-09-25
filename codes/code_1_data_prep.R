


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
p_load(dplyr, data.table, caret, Metrics, xgboost, titanic, vtreat)

# loading all functions
source(file.path(code.folder, "code_0_helper_functions.R"))

train_data_full =  fread(file.path(data.folder, "train.csv"), sep = ",", dec = ".", header = TRUE) 
to_execute_first(train_data_full, 0.8)



###################################
#                                 #
#         DATA PREPARATION        #
#                                 #
###################################

# loading exapmle data set
data("Sacramento")
summary(Sacramento)
train <- Sacramento[1:700,]
valid <- Sacramento[701:932,]

# scaling numeric features
data <- scale_data(train, valid, type = "minmax", except = "beds")
train <- data$train
valid <- data$valid

# adding factor features
data <- add_factor_features(train, valid, target = "price", all_factors = T, all_stats = T, smooth = 10)
train <- data$train
valid <- data$valid

# submiting simple predictions
prediction <- rep(0, nrow(valid))
submit(prediction, data = valid, target.var = "target", id.var = "zip", folder = subm.folder, file = "test.csv")