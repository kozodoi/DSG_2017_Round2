makeID <- function(df){
  
  df$ID <- paste0('id', 1:nrow(df))
  return(df)
  
}

# test <- makeID(test)
