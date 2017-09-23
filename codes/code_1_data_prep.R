###################################
#                                 #
#             SETTINGS            #
#                                 #
###################################

# clearing the memory
rm(list = ls())

# setting directory depending on a user
if (Sys.info()[8] == "lizzzi111") {setwd("~/Documents/DSG_2017_Finals/")} 
if (Sys.info()[8] == "kozodoi")   {setwd("~/Documents/Competitions/DSG_2017_Finals/")}
if (Sys.info()[8] == "")          {setwd("")}
if (Sys.info()[8] == "")          {setwd("")}

# setting inner folders
code.folder <- "codes"
data.folder <- "data"
func.folder <- "functions"
subm.folder <- "submissions"

# loading libraries
if (require(pacman) == FALSE) install.packages("pacman")
library(pacman)
p_load(data.table, ggplot2, caret, Metrics, xgboost)

# loading all functions
source(file.path(code.folder, "code_0_helper_functions.R"))


###################################
#                                 #
#         DATA PREPARATION        #
#                                 #
###################################

# loading exapmle data set
data("Sacramento")
summary(Sacramento)

# scaling numeric features
data <- scale_data(data = data, type = "minmax", except = "beds")
summary(data)

# adding factor features
data <- add_factor_features(data = data, target = "price", all_factors = T, all_stats = T, smooth = 10)
summary(data)

# submiting simple predictions
prediction <- rep(0, nrow(data))
submit(prediction, data = data, target.var = "target", id.var = "zip", folder = subm.folder, file = "test.csv")