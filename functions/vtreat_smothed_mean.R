
split_dataset<-function(data, prozent_train){
  #set.seed(101) # Set Seed so that same sample can be reproduced in future also
  # Now Selecting 75% of data as sample from total 'n' rows of the data  
  sample <- sample.int(n = nrow(data), size = floor(prozent_train*nrow(data)), replace = F)
  train <- data[sample, ]
  test  <- data[-sample, ]
  train_test=list("train"=train, "test"=test)
  return(train_test)
}


#'vtreat' prepares variables so that data has 
#fewer exceptional cases, making it easier to safely 
#use models in production. Common problems 'vtreat' defends against: 'Inf', 'NA', 
#too many categorical levels, rare categorical levels, and new categorical levels 
#(levels seen during application, but not during training). 'vtreat::prepare' should be used as you would use 'model.matrix'.

create_vtreat_treatPlan = function(data_train, label, binary_class=TRUE){
  features = names(data_train)[ !names(data_train) %in% c(label) ]
  if(binary_class){
    treatmentPlan = designTreatmentsC(data_train, varlist = features, outcomename = label, outcometarget = TRUE)
  }else{
    treatmentPlan = designTreatmentsN(data_train, varlist = features, outcomename = label)
  }
  return(treatmentPlan)
}


vtreat_vars = function(data, label, treatplan, pruneLevel=NULL){
  #EXAMPLE:
  #library(vtreat)
  #label = data.table.full$is_listened
  #dframe_treat = vtreat_vars(data.table.full[,-"is_listened"])
  #dframe_treat$is_listened=label
  
  features = names(data_train)[ !names(data_train) %in% c(label) ]
  #target_var = data[,vars_to_save]
  
  #treatplan <- designTreatmentsZ(data, features)
  
  # Examine the scoreFrame
  scoreFrame <- treatplan$scoreFrame %>% select(varName, origName, code)
  
  # We only want the rows with codes "clean" or "lev"
  newvars <- (scoreFrame %>% filter(code %in% c("clean", "lev")))$varName
  # Create the treated training data
  dframe.treat <- prepare(treatplan, data, varRestriction = newvars)
  #str(dframe.treat)
  #dframe.treat[,vars_to_save]=target_var
  
  return(dframe.treat)
}








#compute smoothed mean of the targetvariable per defined group and stoes it in new variables, smoothed to avoid overfiting.
#var_groups is list(c ( ,), c( , ) ...)
# target values of the test set should be set to "NA". 

smoothed_mean_per_group <- function(data_train, data_valid, target_name, var_groups, alpha){
  
  data = rbind(data_train, data_valid)
  data_in = data.table(data)
  length_train = dim(data_train)[1]
  
  global_target_mean=mean( data_in[ , get(target_name), ], na.rm = TRUE )
  
  
  for (group in var_groups){
    group_in = unlist(group)
    if(length(group_in)==2){    
      
      a =group_in[1]
      b = group_in[2]
      print("Group:")
      print(a)
      print(b)
      new_var_name = paste(a,b, sep = "_")
      
      data_in[!(is.na((get(target_name)))), eval(new_var_name) := ((mean((get(target_name)))*.N)+(global_target_mean*alpha))/(.N+alpha), by=c(eval(a), eval(b))]
      data_in[is.na(get(target_name)), eval(new_var_name):=0]
      data_in[, eval(new_var_name) := max(get(new_var_name)), by=c(eval(a), eval(b))]
      
      
    } else if(length(group_in)==3){  
      print("Group:")
      a =group_in[1]
      b = group_in[2]
      c = group_in[3]
      print(a)
      print(b)
      new_var_name = paste(a,b,c, sep = "_")
      
      #data_in[, eval(new_var_name) = ((mean(get(corelated_real_var), na.rm = TRUE)*.N)+(global_target_mean*alpha))/(.N+alpha) , by=c(eval(a), eval(b), eval(c))]
      
      data_in[!(is.na((get(target_name)))), eval(new_var_name) := ((mean((get(target_name)))*.N)+(global_target_mean*alpha))/(.N+alpha), by=c(eval(a), eval(b), eval(c))]
      data_in[is.na(get(target_name)), eval(new_var_name):=0]
      data_in[, eval(new_var_name) := max(get(new_var_name)), by=c(eval(a), eval(b), eval(c))]
      
    } else
    {
      print("Only works for 2 or 3 combinations")
    }
    
    
    
  }
  
  
  train = data_in[c(1:length_train),]
  valid = data_in[-c(1:length_train),]
  
  return(list(train = train, valid = valid))
  
  
}


smoothed_mean_per_group_by_pos <- function(data_in, target_name, var_groups, alpha){
  #data_in = data.table(data)
  
  global_target_mean=mean( data_in[!(is.na(get(target_name))), get(target_name), ])
  
  
  for (group in var_groups){
    group_in = unlist(group)
    if(length(group_in)==2){  
      
      a =group_in[1]
      b = group_in[2]
      print("Group:")
      print(a)
      print(b)
      
      new_var_name = paste(a,b, "_by_pos", sep = "_")
      
      
      
      data_in[!(is.na((get(target_name)))), eval(new_var_name) := (mean((get(target_name)))+(global_target_mean*alpha))/(sum(get(target_name))+alpha), by=c(eval(a), eval(b))]
      data_in[is.na(get(target_name)), eval(new_var_name):=0]
      data_in[, eval(new_var_name) := max(get(new_var_name)), by=c(eval(a), eval(b))]
      
      
      
    } else if (length(group_in)==3)
    {
      print("Group:")
      a =group_in[1]
      b = group_in[2]
      c = group_in[3]
      print(a)
      print(b)
      new_var_name = paste(a,b,c, "_by_pos", sep = "_")
      
      data_in[!(is.na((get(target_name)))), eval(new_var_name) := (mean((get(target_name)))+(global_target_mean*alpha))/(sum(get(target_name))+alpha), by=c(eval(a), eval(b), eval(c))]
      data_in[is.na(get(target_name)), eval(new_var_name):=0]
      data_in[, eval(new_var_name) := max(get(new_var_name)), by=c(eval(a), eval(b), eval(c))]
      
    } else
    {
      print("Only works for 2 or 3 combinations")
    }
    
    
  }
  return(data_in)
  
}

#calculate moments of a fature (that correlates with target) pe defined groups
moments_per_group_on_real_corelated_var <- function(data_train, data_valid, corelated_real_var, var_groups){
  data = rbind(data_train, data_valid)
  data_in = data.table(data)
  length_train = dim(data_train)[1]
  
  for (group in var_groups){
    group_in = unlist(group)
    if(length(group_in)==2){  
      
      a =group_in[1]
      b = group_in[2]
      print("Group:")
      print(a)
      print(b)
      mean = paste(a,b,corelated_real_var, "_mean", sep = "_")
      #median = paste(a,b,corelated_real_var, "median", sep = "_")
      min = paste(a,b,corelated_real_var, "_min", sep = "_")
      lower = paste(a,b,corelated_real_var, "_lower", sep = "_")
      middle = paste(a,b,corelated_real_var, "_middle", sep = "_")
      upper = paste(a,b,corelated_real_var, "_upper", sep = "_")
      max = paste(a,b,corelated_real_var, "_max", sep = "_")
      
      data_in[, c(mean,min,lower, middle,upper,max ):= list(
        mean(get(corelated_real_var), na.rm = TRUE),
        min(get(corelated_real_var) , na.rm = TRUE),
        quantile(get(corelated_real_var), .25, na.rm=TRUE),
        quantile(get(corelated_real_var), .50, na.rm=TRUE),
        quantile(get(corelated_real_var), .75, na.rm=TRUE),
        max(get(corelated_real_var), na.rm = TRUE )),
        by=c(eval(a), eval(b))]
      
      
      
    } else if (length(group_in)==3)
    {
      print("Group:")
      a =group_in[1]
      b = group_in[2]
      c = group_in[3]
      
      print("Group:")
      print(a)
      print(b)
      print(c)
      mean = paste(a,b,corelated_real_var, "_mean", sep = "_")
      #median = paste(a,b,corelated_real_var, "median", sep = "_")
      min = paste(a,b,corelated_real_var, "_min", sep = "_")
      middle = paste(a,b,corelated_real_var, "_middle", sep = "_")
      upped = paste(a,b,corelated_real_var, "_upper", sep = "_")
      max = paste(a,b,corelated_real_var, "_max", sep = "_")
      
      data_in[,c(mean,min,middle,upper,max ):= list(
        
        mean(get(corelated_real_var)),
        min(get(corelated_real_var)),
        quantile(get(corelated_real_var), .25, na.rm=TRUE),
        quantile(get(corelated_real_var), .50, na.rm=TRUE),
        quantile(get(corelated_real_var), .75, na.rm=TRUE),
        max(get(corelated_real_var))),
        
        by=c(eval(a), eval(b), eval(c))]
      
      
    } else
    {
      print("Only works for 2 or 3 combinations")
    }
    
    
  }
  
  
  train = data_in[c(1:length_train),]
  valid = data_in[-c(1:length_train),]
  
  return(list(train = train, valid = valid))
  
}







