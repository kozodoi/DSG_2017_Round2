
base_line_model_xgboost_with_parameter_search = function(data_train, train_label, data_test, test_label, target, best_model=FALSE, nrounds = 100, type = "r"){
  
  
  obje= "reg:linear"
  metrics = list(eval_metric="rmse",
                 eval_metric="logloss" 
  )
  #best_model_parameter = "rmse"
  optimizer =  "which.min(ErrorsHyperparameters$rmse)"
  
  if(type == "c"){
    obje= "binary:logistic"
    metrics = list(eval_metric="error",
                   #eval_metric="rmse",
                   eval_metric="auc"
    )
    #best_model_parameter = "auc"
    optimizer =  "which.max(ErrorsHyperparameters$auc)"
  }
  
  
  size_train = dim(data_train)[1]
  
  data_train[[target]]=NULL
  data_test[[target]]=NULL
  
  
  
  #subset for now
  #data.train.xgboost_1 = dtrain[sample.int(n = nrow(dtrain), size = floor(.05*nrow(dtrain)), replace = F), ]
  
  searchGridSubCol <- expand.grid(subsample = c(0.7, 1), 
                                  colsample_bytree = c(0.6, 0.8, 1),
                                  eta = c(0.01, 0.001, 0.0001),
                                  max_depth = c(100, 500) 
  )
  
  #subset for now
  #dtrain_custsearch_subset = dtrain_custsearch[sample.int(n = nrow(dtrain_custsearch), size = floor(.01*nrow(dtrain_custsearch)), replace = F), ]
  
  
  
  #Build a xgb.DMatrix object
  DMMatrixTrain <- xgb.DMatrix(data = as.matrix(data_train), label = train_label)
  DMMatrixTest <- xgb.DMatrix(data = as.matrix(data_test), label = test_label)
  
  watchlist <- list(train=DMMatrixTrain, test=DMMatrixTest)
  
  
  ErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
    
    #Extract Parameters to test
    currentColsampleRate <- parameterList[["colsample_bytree"]]
    currentMax_depth <- parameterList[["max_depth"]]
    currentEta <- parameterList[["eta"]]
    currentSubsample <- parameterList[["subsample"]]
    
    
    param <- c(list(booster="gbtree", 
                    objective=obje,
                    #eval_metric="mlogloss",
                    #showsd = TRUE, 
                    #metrics=metrics
                    
                    #nthread = 3,      # parallel computing
                    #num_class=2,
                    eta = currentEta,
                    colsample_bytree = currentColsampleRate,
                    #gamma = 1,
                    max_depth = currentMax_depth, 
                    #min_child_weight = 1,
                    subsample = currentSubsample
                    #scale_pos_weight=scale_pos_weight),
    ),
    metrics
    )
    
    
    xgb2 <- xgb.train(data = DMMatrixTrain,
                      params = param,
                      watchlist=watchlist,
                      nrounds = nrounds
    )
    
    
    if(type == "c"){
      
      test_auc = xgb2$evaluation_log[nrounds]$test_auc
      test_error = xgb2$evaluation_log[nrounds]$test_error
      return(c(test_error, test_auc, currentSubsample, currentColsampleRate, currentEta, currentMax_depth))
      
    }else if (type == "r"){
      
      test_rmse = xgb2$evaluation_log[nrounds]$test_rmse
      test_logloss = xgb2$evaluation_log[nrounds]$test_logloss
      return(c(test_rmse, test_logloss, currentSubsample, currentColsampleRate, currentEta, currentMax_depth))
      
    }
    
  })
  
  ErrorsHyperparameters=transpose(as.data.frame(ErrorsHyperparameters))
  names(ErrorsHyperparameters)=c(unlist(metrics),names(searchGridSubCol))
  best.parameters= ErrorsHyperparameters[  eval(parse(text = optimizer)) ,]
  
  fin_best_model=NULL
  if (best_model==TRUE){
    param <- c(list(booster="gbtree", 
                    objective=obje,
                    #num_class=2,
                    eta = best.parameters$eta,
                    colsample_bytree = best.parameters$colsample_bytree,
                    #gamma = 1,
                    max_depth = best.parameters$max_depth, 
                    #min_child_weight = 1,
                    # nthread = 4, # multithreding
                    subsample = best.parameters$subsample
    ), 
    metrics)
    
    
    fin_best_model <- xgb.train(data = DMMatrixTrain,
                                params = param,
                                watchlist=watchlist,
                                nrounds = nrounds
    )
    
  }
  
  
  return( list(best_parameters = best.parameters, result = ErrorsHyperparameters, fin_best_model = fin_best_model  ))
  
}







base_line_model = function(data_train, train_label, data_valid, valid_label, target, nrounds = 100, type = "r", vtreat=TRUE){
  
  
  obje= "reg:linear"
  metrics = list(eval_metric="rmse",  eval_metric="logloss" )
  
  # in case of classification
  if(type == "c"){
    obje= "binary:logistic"
    metrics = list(eval_metric="error", eval_metric="auc")
  }
  
  data_train[[target]]=NULL
  data_valid[[target]]=NULL

  
  #subset for now
  #dtrain_custsearch_subset = dtrain_custsearch[sample.int(n = nrow(dtrain_custsearch), size = floor(.01*nrow(dtrain_custsearch)), replace = F), ]
  
  
  #Build a xgb.DMatrix object
  DMMatrixTrain <- xgb.DMatrix(data = as.matrix(data_train), label = train_label)
  DMMatrixTest <- xgb.DMatrix(data = as.matrix(data_valid), label = valid_label)
  
  watchlist <- list(train=DMMatrixTrain, test=DMMatrixTest)
  
  param <- c(list( booster="gbtree", 
                   #nthread = 4,
                objective=obje 
                #scale_pos_weight=scale_pos_weight,,
                ),
                metrics)
  
  xgb2 <- xgb.train(data = DMMatrixTrain,
                    params = param,
                    watchlist=watchlist,
                    # nrounds = xgb2cv$best_ntreelimit
                    nrounds = nrounds
  )
  
  return( xgb2 )
  
}

