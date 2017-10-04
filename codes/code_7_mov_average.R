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
p_load(dplyr, data.table, caret, Metrics, xgboost, vtreat, plyr, matrixStats)

# loading all functions
source(file.path(code.folder, "code_0_helper_functions.R"))
source(file.path(code.folder, "code_0_parameters.R"))


###################################
#                                 #
#           TRANSFORMATION        #
#                                 #
###################################

# loading known data
known <- load_rda(file.path(data.folder, "known_with_depvar_20_00.rda"))
known <- as.data.frame(known)
known <- known[, c("Month", "SalOrg", "Material", "demand")]
known$ID <- NA
known$date <- NA

# loading unknown data
unknown <- load_rda(file.path(data.folder, "data_unknown.rda"))
unknown$demand <- NA
unknown$Month <- unknown$date

# merging
known <- rbind(known, unknown)

# add moving average
known <- as.data.table(known)
variable <- "demand"
var_name <- paste0(variable, "_mean_", c(2:10))
known[, (var_name[1])  := rowMeans(mapply(cbind, shift(get(variable), 1:2)),   na.rm = T), by = .(SalOrg, Material)]
known[, (var_name[2])  := rowMeans(mapply(cbind, shift(get(variable), 1:3)),   na.rm = T), by = .(SalOrg, Material)]
known[, (var_name[3])  := rowMeans(mapply(cbind, shift(get(variable), 1:4)),   na.rm = T), by = .(SalOrg, Material)]
known[, (var_name[4])  := rowMeans(mapply(cbind, shift(get(variable), 1:5)),   na.rm = T), by = .(SalOrg, Material)]
known[, (var_name[5])  := rowMeans(mapply(cbind, shift(get(variable), 1:6)),   na.rm = T), by = .(SalOrg, Material)]
known[, (var_name[6])  := rowMeans(mapply(cbind, shift(get(variable), 1:7)),   na.rm = T), by = .(SalOrg, Material)]
known[, (var_name[7])  := rowMeans(mapply(cbind, shift(get(variable), 1:8)),   na.rm = T), by = .(SalOrg, Material)]
known[, (var_name[8])  := rowMeans(mapply(cbind, shift(get(variable), 1:9)),   na.rm = T), by = .(SalOrg, Material)]
known[, (var_name[9])  := rowMeans(mapply(cbind, shift(get(variable), 1:10)),  na.rm = T), by = .(SalOrg, Material)]
known <- as.data.frame(known)

# predicting unknown as moving average
unknown.months <- c("2017-04", "2017-05", "2017-06")
data_unknown <- as.data.frame(known[known$Month %in% unknown.months, ])

# check correlation with the best submission
prev <- read.csv(file.path(subm.folder, "median.csv"))
apply(data_unknown[, c(7:ncol(data_unknown))], 2, function(x) cor(x, prev$demand))

# creating submission files
subm5  <- data_unknown[, c("ID", "demand_mean_5")]
subm6  <- data_unknown[, c("ID", "demand_mean_6")]
subm7  <- data_unknown[, c("ID", "demand_mean_7")]
subm8  <- data_unknown[, c("ID", "demand_mean_8")]
subm9  <- data_unknown[, c("ID", "demand_mean_9")]
subm10 <- data_unknown[, c("ID", "demand_mean_10")]
subm5$demand  <- subm5$demand_mean_5
subm6$demand  <- subm6$demand_mean_6
subm7$demand  <- subm7$demand_mean_7
subm8$demand  <- subm8$demand_mean_8
subm9$demand  <- subm9$demand_mean_9
subm10$demand <- subm10$demand_mean_10
write.table(subm5[,  c("ID", "demand")], file.path(subm.folder, "mov_mean_5.csv"),  sep = ",", row.names = F, quote = F)
write.table(subm6[,  c("ID", "demand")], file.path(subm.folder, "mov_mean_6.csv"),  sep = ",", row.names = F, quote = F)
write.table(subm7[,  c("ID", "demand")], file.path(subm.folder, "mov_mean_7.csv"),  sep = ",", row.names = F, quote = F)
write.table(subm8[,  c("ID", "demand")], file.path(subm.folder, "mov_mean_8.csv"),  sep = ",", row.names = F, quote = F)
write.table(subm9[,  c("ID", "demand")], file.path(subm.folder, "mov_mean_9.csv"),  sep = ",", row.names = F, quote = F)
write.table(subm10[, c("ID", "demand")], file.path(subm.folder, "mov_mean_10.csv"), sep = ",", row.names = F, quote = F)
