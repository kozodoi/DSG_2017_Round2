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

load("./data/data_known.rda")
known <- as.data.table(data_known) 
library(lubridate)
known[, Month_c := parse_date_time(Month, 'ym')]
known = known[order(Month_c)]
known[, demand := sum(OrderQty), by = .(Month_c, Material, SalOrg)]


kk = known[,.(ID, Month, Month_c, Material, SalOrg, demand)]
# outliers
summary(kk[, demand])
kk[, q25demand_Material := quantile(demand, 0.25), by = .(Material)]
kk[, q75demand_Material := quantile(demand, 0.75), by = .(Material)]
kk[, IQR_15 := 1.5*IQR(demand, na.rm = T), by = .(Material)]

kk[,demand_n := demand]
kk[demand > q75demand_Material + IQR_15,  `:=` (outlier = 1), by = .(Material) ]

kk[outlier==1, demand_n := NA ]


kk = kk[order(Month_c)]
#known[, PRICE := na.locf(PRICE, na.rm = F), by=list(SalOrg, Material)]


kk[, demand_n :=  na.locf(demand_n, na.rm = F), by=list(SalOrg, Material)]
summary(kk[, demand_n])

kk[is.na(demand_n), demand_n :=  demand, by=list(SalOrg, Material)]
summary(kk[, demand_n])

kk[,demand:=demand_n]
data_known = kk[, .(Month, Material, SalOrg, demand)]
data_known = unique(data_known)
known  = data_known

# add missing combinations
setDT(known, key = c("Month", "SalOrg", "Material"))
combs <- as.data.frame(known[CJ(known$Month, known$SalOrg, known$Material, unique = TRUE), .N, by=.EACHI])
combs[combs$N == 0, "N"] <- NA
combs <- combs[is.na(combs$N), ]
colnames(combs)[4] <- "N"
known <- merge(known, combs, all.x = T, all.y = T)
known <- as.data.frame(known)
known <- known[, !(colnames(known) %in% "N")]
# imputing zeros for demand
known$demand[is.na(known$demand)] <- 0

save(known, file = "./data/imputed_demand.rda")
