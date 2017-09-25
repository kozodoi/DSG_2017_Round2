############################################################
#                                                                                               
#  Function scales all numeric features in the data set
#   - data = data set as data.frame or data.table                                               
#   - type = scaling type (z or minmax)                                     
#   - except = variable names which should not be scaled    
#                                                                                               
############################################################

# introducing the function 
scale_data <- function(train, valid, type = c("minmax", "z"), except = NA) {
  
  # finding all numeric features
  numeric.vars <- names(sapply(train, class)[sapply(train, class) %in% c("numeric", "integer")])
  
  # removing features that should not be scaled
  numeric.vars <- numeric.vars[!(numeric.vars %in% except)]
  
  # looping through features
  for (var in numeric.vars) {
    
    # calculating moments
    mean <- mean(train[[var]])
    sd   <- sd(train[[var]])
    min  <- min(train[[var]])
    max  <- max(train[[var]])
      
    # applying scaling method
    if (type == "z")      {train[[var]] <- (train[[var]] - mean) / sd}
    if (type == "minmax") {train[[var]] <- (train[[var]] - min) / (max - min)}
    
    # applying scaling method
    if (type == "z")      {valid[[var]] <- (valid[[var]] - mean) / sd}
    if (type == "minmax") {valid[[var]] <- (valid[[var]] - min) / (max - min)}
  }
  
  # returning the train set
  return(list(train = train, valid = valid))
}