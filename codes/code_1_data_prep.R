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

# additional folders
modl.folder <- 'models'
knwn.folder <- 'known_predictions'
smpl.folder <- 'sample'

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

# loading known data and creating IDs
data_known = fread(file.path(data.folder, "demand_anonymized_20170802.csv"), sep = ";", dec = ".", header = TRUE, stringsAsFactors = F)
data_known <- as.data.frame(data_known)

# loading unknown data
data_unknown <- fread(file.path(data.folder, "eval_correct.csv"), sep = ",", dec = ".", header = TRUE, stringsAsFactors = F)
data_unknown <- as.data.frame(data_unknown)

# converting features
data_known[num_vars]    <- lapply(data_known[num_vars],   function(x) as.numeric(as.character(x))) 
data_unknown[num_vars]  <- lapply(data_unknown[num_vars], function(x) as.numeric(as.character(x))) 
data_known[fac_vars]    <- lapply(data_known[fac_vars],   function(x) factor(x))
data_unknown[fac_vars]  <- lapply(data_unknown[fac_vars], function(x) factor(x))
data_known[dat_vars]   <- lapply(data_known[dat_vars],   function(x) as.Date.character(x, format = '%Y-%m-%d'))
data_unknown[dat_vars] <- lapply(data_unknown[dat_vars], function(x) as.Date.character(x, format = '%Y-%m-%d'))

# saving data as .RDA
save(data_known,   file = file.path(data.folder, "data_known.rda"))
save(data_unknown, file = file.path(data.folder, "data_unknown.rda"))

# random data partitioning
idx <-  caret::createDataPartition(data_known[, dv], p = 0.8, list = FALSE)
data_known[idx, "part"] <- "train" 
data_known[-idx,"part"] <- "valid" 