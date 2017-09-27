###################################################################
#                                                                                               
#  Function to create a submission CSV file
#   - data = unknown data sample                                            
#   - prediciton = vector with predictions                                        
#   - id.var = name of id variable in the data sample                             
#   - target.var = name of target variable in the data sample
#   - folder = folder to save the file
#   - file = name of the file
#                                                                                               
###################################################################

# introducing the function 
submit <- function(prediction, data, id.var, target.var, folder, file = "submission_test.csv", binary = F) {
  
  # converting to data frame
  data <- as.data.frame(data)
  
  # displaying error messages
  if (length(prediction) != nrow(data)) {stop("Predictions and dataset are not the same length")}
  
  if (binary == T) {
    prediction <- as.numeric(prediction > 0.5)
  }
  
  
  # adding predictions to the data
  data[[target.var]] <- prediction
  
  # creating dataset with relevant coloumns
  data <- data[, c(id.var, target.var)]
  

  # exporting predictions
  write.table(data, row.names = F, col.names = T, quote = F, sep = ",", file = file.path(folder, file))
}