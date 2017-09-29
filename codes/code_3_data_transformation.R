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
#           TRANSFORMATION        #
#                                 #
###################################

# loading data
load(file.path(data.folder, "data_known_prepared.rda"))

# finding all numeric features
numeric.vars <- names(sapply(data_known, class)[sapply(data_known, class) %in% c("numeric", "integer")])
numeric.vars <- numeric.vars[!numeric.vars %in% "ID"]
known_num <- data_known[, c(numeric.vars, "Month", "SalOrg", "Material")]

# aggregating sums and means
sums  <- aggregate(OrderQty ~ Month + SalOrg + Material, data = known_num, function(x) sum(x, na.rm = T)) 
means <- aggregate(.        ~ Month + SalOrg + Material, data = known_num, function(x) sum(x, na.rm = T)) 

# renaming demand
means <- means[, !(colnames(means) %in% "OrderQty")]
colnames(sums)[4] <- "demand"

# merging demand
known <- merge(means, sums, by = c("Month", "SalOrg", "Material"), all.x = F, all.y = T, sort = T)
rm(list = "sums", "means")

# add missing combinations
setDT(known, key = c("Month", "SalOrg", "Material"))
combs <- as.data.frame(known[CJ(known_num$Month, known_num$SalOrg, known_num$Material, unique = TRUE), .N, by=.EACHI])
combs[combs$N == 0, "N"] <- NA
combs <- combs[is.na(combs$N), ]
colnames(combs)[4] <- "N"
known <- merge(known, combs, all.x = T, all.y = T)
known <- as.data.frame(known)
known <- known[, !(colnames(known) %in% "N")]

# imputing zeros for demand
known$demand[is.na(known$demand)] <- 0

# saving data as .RDA
save(known, file <- file.path(data.folder, "known_structured.rda"))