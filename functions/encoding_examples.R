library(titanic)
library(data.table)
# Check with data frame and with data table
# data.frame
train <- titanic::titanic_train
# data.table
test <- as.data.table(titanic::titanic_test)

### ONE_HOT ENCODING
# -1 without intercept
tr <- model.matrix(~.-1,train)
ts <- model.matrix(~.-1,test)

# label encoding 
to_factor <- c("Pclass", "Sex", "Embarked")

# LABEL ENCODING
tr_lab <- train
tr_lab[,names(tr_lab)%in%to_factor] <- sapply(tr_lab[,names(tr_lab)%in%to_factor], factor)

ts_lab <- test
ts_lab[, (to_factor) := lapply(.SD, as.factor), .SDcols = to_factor]
sapply(ts_lab,class)