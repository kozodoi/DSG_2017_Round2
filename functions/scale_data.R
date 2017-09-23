############################################################
#                                                                                               
#  Function scales all numeric features in the data set
#   - data = data set as data.frame or data.table                                               
#   - type = scaling type (z or minmax)                                     
#   - except = variable names which should not be scaled    
#                                                                                               
############################################################

# introducing the function 
scale_data <- function(data, type = c("minmax", "z"), except = NA) {
  
  # finding all numeric features
  numeric.vars <- names(sapply(data, class)[sapply(data, class) %in% c("numeric", "integer")])
  
  # removing features that should not be scaled
  numeric.vars <- numeric.vars[!(numeric.vars %in% except)]
  
  # looping through features
  for (var in numeric.vars) {
    
    # calculating moments
    mean <- mean(data[[var]])
    sd   <- sd(data[[var]])
    min  <- min(data[[var]])
    max  <- max(data[[var]])
      
    # applying scaling method
    if (type == "z")      {data[[var]] <- (data[[var]] - mean) / sd}
    if (type == "minmax") {data[[var]] <- (data[[var]] - min) / (max - min)}
  }
  
  # returning the data set
  return(data)
}