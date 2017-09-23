
base_line_model_xgboost = function(data_train, train_label, data_test, test_label, target, best_model=FALSE, nrounds = 100){
  
  size_train = dim(data_train)[1]
  data = rbind(data_train, data_test)
  data_train[[target]]=NULL
  data_test[[target]]=NULL
  dframe_treat = vtreat_vars(data)
  
  dframe_treat_train = dframe_treat[1:size_train,] #train
  dframe_treat_test = dframe_treat[- c(1:size_train),] #test

  
  #subset for now
  #data.train.xgboost_1 = dtrain[sample.int(n = nrow(dtrain), size = floor(.05*nrow(dtrain)), replace = F), ]
  
  searchGridSubCol <- expand.grid(subsample = c(0.5, 1), 
                                  colsample_bytree = c(0.6, 0.8, 1),
                                  eta = c(0.01, 0.001, 0.0001),
                                  max_depth = c(100, 500) 
  )
  
  #subset for now
  #dtrain_custsearch_subset = dtrain_custsearch[sample.int(n = nrow(dtrain_custsearch), size = floor(.01*nrow(dtrain_custsearch)), replace = F), ]
  
  
  
  #Build a xgb.DMatrix object
  DMMatrixTrain <- xgb.DMatrix(data = as.matrix(dframe_treat_train), label = train_label)
  DMMatrixTest <- xgb.DMatrix(data = as.matrix(dframe_treat_test), label = test_label)
  
  watchlist <- list(train=DMMatrixTrain, test=DMMatrixTest)
  
  rmseErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
    
    #Extract Parameters to test
    currentColsampleRate <- parameterList[["colsample_bytree"]]
    currentMax_depth <- parameterList[["max_depth"]]
    currentEta <- parameterList[["eta"]]
    currentSubsample <- parameterList[["subsample"]]
    
    
    param <- list(booster="gbtree", 
                  objective="binary:logistic", 
                  #eval_metric="mlogloss",
                  #showsd = TRUE, 
                  #metrics=metrics
                  
                  eval_metric="error",
                  eval_metric="rmse", 
                  eval_metric="auc",
                  #num_class=2,
                  eta = currentEta,
                  colsample_bytree = currentColsampleRate,
                  #gamma = 1,
                  max_depth = currentMax_depth, 
                  #min_child_weight = 1,
                  subsample = currentSubsample
                  #scale_pos_weight=scale_pos_weight
    )
    
    
    xgb2 <- xgb.train(data = DMMatrixTrain,
                      params = param,
                      watchlist=watchlist,
                      nrounds = nrounds
    )
    
    
    
    
    #Save eval_metric of the best iteration
    test_error = xgb2$evaluation_log[nrounds]$test_error
    test_auc = xgb2$evaluation_log[nrounds]$test_auc
    test_rmse = xgb2$evaluation_log[nrounds]$test_rmse
    
    
    return(c(test_error, test_auc, test_rmse, currentSubsample, currentColsampleRate, currentEta, currentMax_depth))
    
  })
  
  rmseErrorsHyperparameters=transpose(as.data.frame(rmseErrorsHyperparameters))
  names(rmseErrorsHyperparameters)=c(unlist(param[names(param)=="eval_metric"]),names(searchGridSubCol))
  best.parameters= rmseErrorsHyperparameters[ which.max(rmseErrorsHyperparameters$auc),]
  
  fin_best_model=NULL
  if (best_model==TRUE){
    param <- list(booster="gbtree", 
                  objective="binary:logistic", 
                  #eval_metric="mlogloss",
                  #showsd = TRUE, 
                  #metrics=metrics,
                  eval_metric="error",
                  eval_metric="rmse", 
                  eval_metric="auc",
                  #num_class=2,
                  eta = best.parameters$eta,
                  colsample_bytree = best.parameters$colsample_bytree,
                  #gamma = 1,
                  max_depth = best.parameters$max_depth, 
                  #min_child_weight = 1,
                  subsample = best.parameters$subsample
    )
    
    
    fin_best_model <- xgb.train(data = DMMatrixTrain,
                                params = param,
                                watchlist=watchlist,
                                nrounds = nrounds
    )
    
  }
  
  
  return( list(best_parameters = best.parameters, result = rmseErrorsHyperparameters, fin_best_model = fin_best_model  ))
  
}