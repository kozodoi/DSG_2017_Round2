###################################
#                                 #
#             SETTINGS            #
#                                 #
###################################

# clearing the memory
rm(list = ls())

test_on_sample <- T

# setting directory depending on a user
if (Sys.info()[8] == "lizzzi111")           {setwd("~/DSG_2017_Finals/")} 
if (Sys.info()[8] == "kozodoi")             {setwd("~/Documents/Competitions/DSG_2017_Finals/")}
if (Sys.info()[8] == "nataliasverchkova")   {setwd("~/Documents/DSG/DSG_2017_Finals/")}
if (Sys.info()[8] == "oleksiyostapenko")    {setwd("/Users/oleksiyostapenko/Documents/HU_Berlin/ML/DSG/DSG_2017_Finals")}

# setting inner folders
code.folder <- "codes"
data.folder <- if(!test_on_sample){"data"}else{"sample"}
func.folder <- "functions"
subm.folder <- "submissions"

modl.folder <- 'models'
knwn.folder <- 'known_predictions'
smpl.folder <- 'sample'

# loading all functions
source(file.path(code.folder, "code_0_helper_functions.R"))
source(file.path(code.folder, "code_0_parameters.R"))
# ------------------------------------------------------------------------------
## Seed
set.seed(213)

## Dependent variable name
dv <- "Survived"

## ID variable
id <- "id"

## Variables types
num_vars <- c("Age")  # numeric
fac_vars <- c("Pclass", "Sex", "Embarked")  # factors
dat_vars <- c("")  # dates
ign_vars <- c("")  # ignore variables

# ------------------------------------------------------------------------------
###################################
#                                 #
#         DATA PREPARATION        #
#                                 #
###################################

# loading data and creating IDs
data_known =  read.csv(file.path(data.folder, "known.csv"), sep = ",", dec = ".", header = TRUE, stringsAsFactors = F)
data_known$id = seq(1,nrow(data_known))

if(test_on_sample){
  # 10% of data goes to sample
  s <- sample(2, nrow(data_known), replace=TRUE, prob=c(0.9,0.1))
  data_known <- data_known[s==2,]
}

# converting features
colnames(data_known)
num_vars <- c("SepalLengthCm", "SepalWidthCm",  "PetalLengthCm", "PetalWidthCm" )
fac_vars <- c('Species')
ign_vars <- c('Id', 'part')
data_known[num_vars] <- lapply(data_known[num_vars], function(x) as.numeric(as.character(x))) 
data_known[fac_vars] <- lapply(data_known[fac_vars], function(x) factor(x))
#data_known[dat_vars] <- lapply(data_known[dat_vars], function(x) as.Date(x, origin = '1971-01-01'))

str(data_known)

# random data partitioning
# Also, for createDataPartition, very small class sizes (<= 3) 
# the classes may not show up in both the training and test data
dv <- 'Species'
idx <-  caret::createDataPartition(data_known[,dv], p = 0.8, list = FALSE)
data_known[idx, "part"] <-  "train" 
data_known[-idx,"part"] <-  "valid" 

# saving data as .RDA
save(data_known, file = file.path(data.folder, "data_partitioned.rda"))


###################################
#                                 #
#         CREATING FEATURES       #
#                                 #
###################################

# loading the data
load(file.path(data.folder, "data_partitioned.rda"))

# Comment Alex: need store the labels of the test subset in a seperate vector first and set the label column to NAs#
#==================================================================================================================#


##### CODES FOR NEW FEATURES: LIZA


##### CODES FOR NEW FEATURES: NATALIA


##### CODES FOR NEW FEATURES: NIKITA


##### CODES FOR NEW FEATURES: OLEKS



###################################
#                                 #
#        AUTOMATIC FEATURES       #
#                                 #
###################################

# data partitioning
data_train <- data_known[data_known$part == "train", ]
data_valid <- data_known[data_known$part == "valid", ]

# adding factor features (Nikita)
data_known <- add_factor_features(data_train, data_valid, target = dv, smooth = 10)
str(data_known)
data_train <- data_known$train
data_valid <- data_known$valid

# scaling data
data_known <- scale_data(data_train, data_valid, type = "minmax", except = c(dv, id))
data_train <- data_known$train
data_valid <- data_known$valid

#adding moments per groups for correlated variabel (Alex) 
#don't use target as "corelated_real_var" since function does no smoothing
data_known <-  moments_per_group_on_real_corelated_var(data_train, data_valid, corelated_real_var = "Age", list(c("PassengerId", "Survived"), c("PassengerId", "Pclass")))
data_train <- data_known$train
data_valid <- data_known$valid

#adding smoothed mean per groups (Alex)
# here we calculate the mean only for the target variable per groups 
data_known <-  smoothed_mean_per_group(data_train = data_train, data_valid = data_valid, target_name = dv, var_groups = list(c("PassengerId", "Survived"), c("PassengerId", "Pclass")), alpha = 10)
data_train <- data_known$train
data_valid <- data_known$valid


# saving data as .RDA
save(data_train, file =  file.path(data.folder, "data_train_prepared.rda"))
save(data_valid, file =  file.path(data.folder, "data_valid_prepared.rda"))



# ------------------------------------------------------------------------------
all_vars <- colnames(data_train)
iv <- setdiff(all_vars, c(dv, id, ign_vars))
equation <- paste0(dv, ' ~ ', paste0(iv, collapse = ' + '))
equation

# minimum.setup 
model_control <- trainControl(
  method = "cv", # 'cv' for cross validation
  number = 2, # number of folds in cross validation
  #repeats = 3, # number for repeated cross validation
  savePredictions = FALSE,
  classProbs = TRUE,
  allowParallel = F, # enable parallelization
  returnData = FALSE)



