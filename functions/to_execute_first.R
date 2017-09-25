
to_execute_first <- function(data_train_full, percent){
  
  data_train_full = data.table(data_train_full)
  data_train_full[, id:= .I, ]
  split = split_dataset(data_train_full, percent )
  
  train = split$train
  valid = split$test
  
  fwrite(train, file = "./data/train.csv")
  fwrite(valid, file = "./data/valid.csv")
  
}