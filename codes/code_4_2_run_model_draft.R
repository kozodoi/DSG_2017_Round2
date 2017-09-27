# Insert the model type here and parameters in code_4_1
model_type <- 'RF'
quick <- T

m_name <- paste0(model_type, 
                 ifelse(quick, 'simple_training', 'tuning'),
                 model_setup[[model_type]],
                 collapse = '_')

training_param <- c(simple_training,
                    model_setup[[model_type]])

message(paste("Start training model", model_type, "at", Sys.time()))
modelObject <- try(do.call(caret::train, training_param))

# save model  
save(modelObject, file = file.path(modl.folder, paste0(m_name, '.rda')))

# # save train predictions
# data_train_predict <- predict(modelObject, data_train)
# save(data_train_predict, 
#      file = file.path(knwn.folder, paste0(m_name, '.rda')))
# 
# # save test predictions
# data_known <- predict(modelObject, data_known)
# save(m1, file = file.path(knwn.folder, paste0(m_name, '.rda')))