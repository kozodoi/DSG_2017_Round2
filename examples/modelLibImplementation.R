#use indices
m(list = ls())

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
#         CREATING FEATURES       #
#                                 #
###################################

# loading data
load(file.path(data.folder, "data_partitioned.rda"))
load(file.path(data.folder, "data_unknown.rda"))

# data_unknown$part = "final" 
# data_unknown$Survived = "NA"
# 
# data_full = rbind(data_known, data_unknown)

data_full = data_known
data_full$Age[is.na(data_full$Age)] = mean(data_full$Age, na.rm = T)
data_full$Survived = as.factor(data_full$Survived)

#we need to change names of levels, than it works
levels(data_full$Survived) = c("survived", "not")
train_idx = which(data_full$part == "train")
valid_idx = which(data_full$part == "valid")
final_idx = which(data_full$part == "final")

#data_full$Survived[data_full$Survived=="NA"] = NA

source("./examples/createModelLib.R")
model_setup = list("NN" = list(tuneGrid = expand.grid(decay = 10^seq(-4, 0, 0.5), size = seq(3, 13, 2)), maxit = 100, method = "nnet"), 
                   "RF" = list(tuneGrid = expand.grid(mtry = c(5, 8, 10, 12, 15, 20)), ntree = 100, method = "rf"), 
                   "rpart" = list(tuneGrid = expand.grid(cp = seq(0.001, 0.1, 0.01)), method = "rpart"), 
                   "SVMradial" = list(tuneGrid = expand.grid(sigma = 2^seq(-12, -1), C = 2^seq(-12, 12)), method = "svmRadial"), 
                   "SVMlinear" = list(tuneGrid = expand.grid(C = 2^seq(-12, 12)), method = "svmLinear"))

modellist = c("rpart")

# it does not work with final_idx
mat_bin_fitA = modelsLib(data = data_full[,c("Survived", "Pclass", "Sex", "Age", "SibSp")], 
                         train_idx = list(train_idx), 
                         valid_idx = list(valid_idx),
                       #  final_idx = final_idx,
                         formula = Survived~., 
                         modellist = modellist, 
                         metric = "Accuracy",
                         model_setup = model_setup )

predict(mat_bin_fitA$rpart, newdata = data_unknown, type = "prob")
