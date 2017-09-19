##########################################################################
#
#  This is a code for loading all other functions.
#  Requires func.folder to be set as a directory with function files.
#
##########################################################################

# setting working directory
load_functions <- function(path) {
  
  # getting a file list
  file.list <- list.files(path)
  
  # processing functions
  for (i in 1:length(file.list)) {
    source(file.path(path, file.list[i]))
    print(file.path("Loading ", file.list[i]))
  }
}
 
# loading all functions
load_functions(func.folder)