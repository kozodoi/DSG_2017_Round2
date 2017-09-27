# function to make formula from dv and independent vars

make_formula <- function(dep_var, ind_vars){
  
  equation <- paste0(dep_var, ' ~ ', paste0(ind_vars, collapse = ' + '))
  print(equation)
  
  return(as.formula(equation))
}
