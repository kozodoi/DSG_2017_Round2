# loading the data
load(file.path(smpl.folder, "data_partitioned.rda"))

# data partitioning
data_train <- data_known[data_known$part == "train", ]
data_valid <- data_known[data_known$part == "valid", ]

train <- data_train
valid <- data_valid
target <- dv
factors = NULL
stats = NULL
all_factors = T 
all_stats = T
smooth = 10

# introducing the function 
#add_factor_features <- function(train, valid, target, factors = NULL, stats = NULL, all_factors = T, all_stats = T, smooth = 10) {
  
  
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
  p_load(data.table)
  
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
  
  # merging stats with validation
  
  # for(var in factors){
  #   factor <- factors[1]
  #   calc_stats <- paste0(target, '_', var, "_", stats)
  #   valid <- merge(valid, train[, c(var, calc_stats)], by = var, all.x = T, all.y = F, sort = F)
  # }
  
  if (length(c(factors, current.stats)) > 0) {
    for (factor in factors) {
      
      calc_stats <- paste0(target, '_', factor, "_", stats)
      factor_levels <- train[, c(factor, calc_stats)] 
      factor_levels <- factor_levels[!duplicated(factor_levels), ]
      valid <- plyr::join(valid, factor_levels, by = factor, type = "left")
    }
  }
  
  ##### RETURNING THE DATA SET
#  return(list(train = train, valid = valid))
#}
