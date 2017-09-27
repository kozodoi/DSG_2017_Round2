### PARAMETERS FOR EVERYONE

## Seed
set.seed(213)

## Dependent variable name
dv <- "Survived"

## Variables types
num_vars <- c("Age")  # numeric
fac_vars <- c("Pclass", "Sex", "Embarked")  # factors
dat_vars <- c("")  # dates
ign_vars <- c("part", 'id')  # ignore variables of id and partitioning

## Variables which we drop from the xgboost
drop_vars <- c("")

## Xgboost optimal parameters
par1 <- 0.3
par2 <- 500