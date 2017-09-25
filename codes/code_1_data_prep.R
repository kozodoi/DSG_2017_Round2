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
source(file.path(code.folder, "code_0_parameters.R"))


###################################
#                                 #
#         DATA PREPARATION        #
#                                 #
###################################

train_data_full =  as.data.frame(fread(file.path(data.folder, "train.csv"), sep = ",", dec = ".", header = TRUE) )

train_data_full[, num_vars] <- sapply(train_data_full[, num_vars], function(x) as.numeric(as.character(x))) 
train_data_full[, fac_vars] <- sapply(train_data_full[, fac_vars], as.factor)
# train_data_full[, dat_vars] <- sapply(train_data_full[, dat_vars], function(x) as.Date(x, origin = '1971-01-01'))

to_execute_first(train_data_full, 0.8)

