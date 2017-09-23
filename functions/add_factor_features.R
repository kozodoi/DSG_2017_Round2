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
add_factor_features <- function(data, target, factors, stats, all_factors = F, all_stats = F, smooth = 10) {
  
  
  ##### PREPARATIONS
  
  # checking if target is factor
  if (class(data[[target]]) == "factor") {
    data[[target]] <- as.numeric(data[[target]])-1
  }
  
  # selecting factors if not sepcified
  if (all_factors == T) {
    factors <- names(Filter(is.factor, data))
  }
  
  # selecting stats if not sepcified
  if (all_stats == T) {
    stats <- c("min", "max", "mean", "median", "size")
  }

  # loading libraries
  if (require(pacman) == FALSE) install.packages("pacman")
  library(pacman)
  p_load(data.table)
  
  # converting to data table
  data <- as.data.table(data)
  

  
  ##### ERRORS
  
  # checking if target is a numeric variable
  if (!class(data[[target]]) %in% c("numeric", "integer")) {
    stop("Target variable is not numeric")
    }
  
  # checking if factors are indeed factors 
  if (sum(factors %in% names(Filter(is.factor, data))) < length(factors)) {
    stop("Not all factors are factor variables")
  }
  
  
  ##### COMPUTING AGGREGATED STATISTICS
  global_mean   <- mean(data[[target]],   na.rm = T)
  global_median <- median(data[[target]], na.rm = T)
  global_min    <- min(data[[target]],    na.rm = T)
  global_max    <- max(data[[target]],    na.rm = T)

  
  ##### COMPUTING FACTOR-LEVEL STATISTICS
  for (variable in factors) {
    for (stat in stats) {
      
      # computing statistics: min
      if (stat == "min") {
        var_name <- paste0(target, "_", variable, "_", stat)
        data[, (var_name) := (min(get(target), na.rm = T)*.N  + global_min*smooth)/(.N + smooth), by = get(variable)]
      }
  
      # computing statistics: max
      if (stat == "max") {
        var_name <- paste0(target, "_", variable, "_", stat)
        data[, (var_name) := (max(get(target), na.rm = T)*.N + global_max*smooth)/(.N + smooth), by = get(variable)]
      }
      
      # computing statistics: mean
      if (stat == "mean") {
        var_name <- paste0(target, "_", variable, "_", stat)
        data[, (var_name) := (mean(get(target), na.rm = T)*.N + global_mean*smooth)/(.N + smooth), by = get(variable)]
      }
      
      # computing statistics: median
      if (stat == "median") {
        var_name <- paste0(target, "_", variable, "_", stat)
        data[, (var_name) := (median(get(target), na.rm = T)*.N + global_median*smooth)/(.N + smooth), by = get(variable)]
      }

      # computing statistics: size
      if (stat == "size") {
        var_name <- paste0(target, "_", variable, "_", stat)
        data[, (var_name) := as.numeric(.N), by = get(variable)]
      }
    }
  }
  
  
  ##### RETURNING THE DATA SET
  return(data)
}
