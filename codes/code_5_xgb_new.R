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

# loading data
known <- load_rda(file.path(data.folder, "known_with_depvar_20_00.rda"))
#known <- load_rda(file.path(data.folder, "imputed_demand.rda"))
#known <- load_rda(file.path(data.folder, "agg_data_all_features_9lags_dep_vars_21_00.rda"))
#known[["demand_lead1_imp"]] <- NULL
#known[["demand_lead2_imp"]] <- NULL
known <- as.data.frame(known)

known <- known[, c("Month", "SalOrg", "Material", "demand", "demand_lead3")]

# adding lags
known <- as.data.table(known)
known[, .demand_lag_1  := shift(demand, 1),  by = .(SalOrg, Material)]
known[, .demand_lag_2  := shift(demand, 2),  by = .(SalOrg, Material)]
known[, .demand_lag_3  := shift(demand, 3),  by = .(SalOrg, Material)]
known[, .demand_lag_4  := shift(demand, 4),  by = .(SalOrg, Material)]
known[, .demand_lag_5  := shift(demand, 5),  by = .(SalOrg, Material)]
known[, .demand_lag_6  := shift(demand, 6),  by = .(SalOrg, Material)]
known[, .demand_lag_7  := shift(demand, 7),  by = .(SalOrg, Material)]
known[, .demand_lag_8  := shift(demand, 8),  by = .(SalOrg, Material)]
known[, .demand_lag_9  := shift(demand, 9),  by = .(SalOrg, Material)]
known[, .demand_lag_10  := shift(demand, 10),  by = .(SalOrg, Material)]
known[, .demand_lag_11  := shift(demand, 11),  by = .(SalOrg, Material)]
known <- as.data.frame(known)

# remove irrelevant vars
know <- known[, !(colnames(known) %in% c("demand_lead2", "demand_lead1", "demand_lead123", "Month_c"))]
know$country_material <- interaction(know$SalOrg, know$Material)

# add movштп averфпу
know <- as.data.table(know)
variable <- "demand"
var_name <- paste0(variable, "_mean_", c(2:9))
know[, (var_name[2])  := rowMeans(mapply(cbind, shift(get(variable), 1:2)),   na.rm = T), by = .(SalOrg, Material)]
know[, (var_name[3])  := rowMeans(mapply(cbind, shift(get(variable), 1:3)),   na.rm = T), by = .(SalOrg, Material)]
know[, (var_name[4])  := rowMeans(mapply(cbind, shift(get(variable), 1:4)),   na.rm = T), by = .(SalOrg, Material)]
know[, (var_name[5])  := rowMeans(mapply(cbind, shift(get(variable), 1:5)),   na.rm = T), by = .(SalOrg, Material)]
know[, (var_name[6])  := rowMeans(mapply(cbind, shift(get(variable), 1:6)),   na.rm = T), by = .(SalOrg, Material)]
know[, (var_name[7])  := rowMeans(mapply(cbind, shift(get(variable), 1:7)),   na.rm = T), by = .(SalOrg, Material)]
know[, (var_name[8])  := rowMeans(mapply(cbind, shift(get(variable), 1:8)),   na.rm = T), by = .(SalOrg, Material)]
know[, (var_name[9])  := rowMeans(mapply(cbind, shift(get(variable), 1:9)),   na.rm = T), by = .(SalOrg, Material)]
var_name <- paste0(variable, "_sm_", c(1:10))
know[, (var_name[2])  := rowSds(mapply(cbind, shift(get(variable), 1:2)),   na.rm = T), by = .(SalOrg, Material)]
know[, (var_name[3])  := rowSds(mapply(cbind, shift(get(variable), 1:3)),   na.rm = T), by = .(SalOrg, Material)]
know[, (var_name[4])  := rowSds(mapply(cbind, shift(get(variable), 1:4)),   na.rm = T), by = .(SalOrg, Material)]
know[, (var_name[5])  := rowSds(mapply(cbind, shift(get(variable), 1:5)),   na.rm = T), by = .(SalOrg, Material)]
know[, (var_name[6])  := rowSds(mapply(cbind, shift(get(variable), 1:6)),   na.rm = T), by = .(SalOrg, Material)]
know[, (var_name[7])  := rowSds(mapply(cbind, shift(get(variable), 1:7)),   na.rm = T), by = .(SalOrg, Material)]
know[, (var_name[8])  := rowSds(mapply(cbind, shift(get(variable), 1:8)),   na.rm = T), by = .(SalOrg, Material)]
know[, (var_name[9])  := rowSds(mapply(cbind, shift(get(variable), 1:9)),   na.rm = T), by = .(SalOrg, Material)]
know <- as.data.frame(know)

# data partitioning
valid.months <- c("2016-12", "2016-11", "2016-10")
data_train <- na.omit(as.data.frame(know[!(know$Month %in% valid.months), ]))
data_valid <- na.omit(as.data.frame(know[ (know$Month %in% valid.months), ]))

# add factor features
data <- add_factor_features(data_train, data_valid, smooth = 10, all_factors = F, all_stats = F,
                            factors = c("country_material"), stats = c("sd", "max", "mean", "median"), 
                            targets = c("demand_lead3"))
data_train <- data$train
data_valid <- data$valid

know <- rbind(data_train, data_valid)
data_train <- na.omit(data_train)
data_valid <- na.omit(data_valid)


# XGB parameters
obje = "reg:linear"
metrics = list(eval_metric = "mae", eval_metric = "mae")
param <- c(list(booster = "gbtree", nthread = 4, objective = obje), metrics, nrounds = 10)
dv <- "demand_lead3"
ign_vars <- c("ID", "country_material", "country_material_year", "country_material_month", "month", "year", "Quater", "demandQu_LEAD",
              "Month", "SalOrg", "Material", "demand_lead1", "demand_lead2", "demand_lead3", "demand_lead123", "price_group",
              "LogABC_imp", "PL_imp", "MktABC_imp", "Gamma_imp", "SalOrg_imp", "Volume", "Gamma_imp_Material_max",
              "LogABC_imp_Material_max", "PRICE", "revenue")


# vtreat as encoder
#iv <- colnames(data_train)[!(colnames(data_train)%in%c(dv, ign_vars))]
#treatmentPlan <- designTreatmentsN(data_train, varlist = iv, outcomename = dv)
#data_train_treat   <- vtreat_vars(data = data_train,   label = dv, treatplan = treatmentPlan, pruneLevel = NULL)
#data_valid_treat   <- vtreat_vars(data = data_valid,   label = dv, treatplan = treatmentPlan, pruneLevel = NULL)
#data_unknown_treat <- vtreat_vars(data = data_unknown, label = dv, treatplan = treatmentPlan, pruneLevel = NULL)

data_train <- as.data.frame(data_train)
data_valid <- as.data.frame(data_valid)

# creating matrices
DMMatrixTrain <- xgb.DMatrix(data = as.matrix(data_train[, !(colnames(data_train) %in% ign_vars)]), label = data_train[[dv]])
DMMatrixTest  <- xgb.DMatrix(data = as.matrix(data_valid[, !(colnames(data_valid) %in% ign_vars)]), label = data_valid[[dv]])

# training XGB
watchlist <- list(train = DMMatrixTrain, test = DMMatrixTest)
xgb <- xgb.train(data = DMMatrixTrain, params = param, watchlist=watchlist, nrounds = param$nrounds)

# feature importance
varimp <- xgb.importance(colnames(data_train[, !(colnames(data_train) %in% ign_vars)]), xgb)
xgb.plot.importance(importance_matrix = varimp)

# predicting validation
valid.pred <- predict(xgb, as.matrix(data_valid[, !(colnames(data_valid) %in% ign_vars)]))

# computing score on validation
valid <- data.frame(pred = valid.pred, demand = data_valid$demand_lead3)
valid$pred[valid$pred < 0] <- 0
apply(valid, 2, function(x) mean(abs((x - valid$demand))))

# predicting unknown
unknown.months <- c("2017-01", "2017-02", "2017-03")
data_unknown <- as.data.frame(known[known$Month %in% unknown.months, ])
data_unknown$pred <- predict(xgb, as.matrix(data_unknown[, !(colnames(data_unknown) %in% ign_vars)]))

# merging predictions with unknown data
data_unknown <- data_unknown[, c("SalOrg", "Material", "Month", "pred")]
unknown <- load_rda(file.path(data.folder, "data_unknown.rda"))
unknown$Month <- unknown$date
levels(unknown$Month) <- c("2017-01", "2017-02", "2017-03")
unknown <- merge(unknown, data_unknown, by = c("Month", "SalOrg", "Material"), all.x = T, all.y = F, sort = F)

# creating submission file
subm <- unknown[, c("ID", "pred")]
subm$demand <- subm$pred
subm <- subm[, c("ID", "demand")]
subm <- subm[order(subm$ID), ]
subm$demand[subm$demand < 0] <- 0
write.table(as.data.frame(subm), file.path(subm.folder, "xgb_newww.csv"), sep = ",", row.names = F, quote = F)

# chwck correlation with the best submission
prev <- read.csv(file.path(subm.folder, "median.csv"))
cor(prev$demand, subm$demand)

subm$demand = (subm$demand+prev$demand)/2
