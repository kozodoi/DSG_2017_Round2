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
#         DATA PREPARATION        #
#                                 #
###################################

# loading data and creating IDs
data_known =  read.csv(file.path(data.folder, "known.csv"), sep = ",", dec = ".", header = TRUE)
data_known$id = seq(1,nrow(data_known))

# converting features
data_known[num_vars] <- lapply(data_known[num_vars], function(x) as.numeric(as.character(x))) 
data_known[fac_vars] <- lapply(data_known[fac_vars], function(x) factor(x))
#data_known[dat_vars] <- lapply(data_known[dat_vars], function(x) as.Date(x, origin = '1971-01-01'))

# random data partitioning
idx <-  caret::createDataPartition(data_known$id, p = 0.8, list = FALSE)
data_known[idx, "part"] <-  "train" 
data_known[-idx,"part"] <-  "valid" 

# saving data as .RDA
save(data_known, file = file.path(data.folder, "data_partitioned.rda"))