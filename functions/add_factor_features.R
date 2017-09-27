################################################################################################
#                                                                                               
#  Function creates new features: moments of a target variable across different factor levels   
#   - data = data set as data.frame or data.table                                               
#   - target = name of the target variable (numeric)                                            
#   - factors = list of factor names (or set all_factors = T)                                   
#   - stats = list of staistics (min, max, mean, median, size or set all_stats = T)  
#   - smooth = smoothing parameter according to Daria Soboleva (0 = no smoothing)
#                                                                                               
################################################################################################

# introducing the function 
add_factor_features <- function(train, valid, target, factors = NULL, stats = NULL, all_factors = T, all_stats = T, smooth = 10) {
  
  
  ##### PREPARATIONS
  
  # saving colnames
  initial.vars <- colnames(train)
  
  # checking if target is factor
  if (class(train[[target]]) == "factor") {
    train[[target]] <- as.numeric(train[[target]])-1
  }
  
  # selecting factors if not sepcified
  if (all_factors == T) {
    factors <- names(Filter(is.factor, train))
  }
  
  # selecting stats if not sepcified
  if (all_stats == T) {
    stats <- c("min", "max", "mean", "median", "size")
  }
  
  # loading libraries
  if (require(pacman) == FALSE) install.packages("pacman")
  library(pacman)
  p_load(data.table, plyr)
  
  # converting to train table
  train <- as.data.table(train)
  
  
  
  ##### ERRORS
  
  # checking if target is a numeric variable
  if (!class(train[[target]]) %in% c("numeric", "integer")) {
    stop("Target variable is not numeric")
  }
  
  # checking if factors are indeed factors 
  if (sum(factors %in% names(Filter(is.factor, train))) < length(factors)) {
    stop("Not all factors are factor variables")
  }
  
  
  ##### COMPUTING AGGREGATED STATISTICS
  global_mean   <- mean(train[[target]],   na.rm = T)
  global_median <- median(train[[target]], na.rm = T)
  global_min    <- min(train[[target]],    na.rm = T)
  global_max    <- max(train[[target]],    na.rm = T)
  
  
  ##### COMPUTING FACTOR-LEVEL STATISTICS
  for (variable in factors) {
    for (stat in stats) {
      
      # computing statistics: min
      if (stat == "min") {
        var_name <- paste0(target, "_", variable, "_", stat)
        train[, (var_name) := (min(get(target), na.rm = T)*.N  + global_min*smooth)/(.N + smooth), by = get(variable)]
      }
      
      # computing statistics: max
      if (stat == "max") {
        var_name <- paste0(target, "_", variable, "_", stat)
        train[, (var_name) := (max(get(target), na.rm = T)*.N + global_max*smooth)/(.N + smooth), by = get(variable)]
      }
      
      # computing statistics: mean
      if (stat == "mean") {
        var_name <- paste0(target, "_", variable, "_", stat)
        train[, (var_name) := (mean(get(target), na.rm = T)*.N + global_mean*smooth)/(.N + smooth), by = get(variable)]
      }
      
      # computing statistics: median
      if (stat == "median") {
        var_name <- paste0(target, "_", variable, "_", stat)
        train[, (var_name) := (median(get(target), na.rm = T)*.N + global_median*smooth)/(.N + smooth), by = get(variable)]
      }
      
      # computing statistics: size
      if (stat == "size") {
        var_name <- paste0(target, "_", variable, "_", stat)
        train[, (var_name) := as.numeric(.N), by = get(variable)]
      }
    }
  }
  
  # saving colnames
  current.vars  <- colnames(train)
  current.stats <- current.vars[!current.vars %in% initial.vars]
  
  # converting to data.frame
  train <- as.data.frame(train)
  valid <- as.data.frame(valid)
  
  ### merging stats with validation
  if (length(c(factors, current.stats)) > 0) {
    for (factor in factors) {
      factor_levels <- train[, c(factor, current.stats[grepl(factor, current.stats)])]
      factor_levels <- factor_levels[!duplicated(factor_levels), ]
      valid <- join(valid, factor_levels, type = "left")
    }
  }
  
  
  ##### RETURNING THE DATA SET
  return(list(train = train, valid = valid))
}