##########################################################################
#
# This is a function for loading .Rda files and saving it
# in R environment using a custom object name.
#
# Input arguments:
#  - file = name of the .Rda file
#
##########################################################################

# introducing the function
load_rda <- function(file) {
  
  # loading the file
  load(file)
  
  # reading the name
  get(ls()[ls() != "file"])

}
