# This function concatenates the different columns of X's
# The inputs:
#   Input = the raw .csv file,
#   Intervals = the number of columns for each filming period (if
#               there are two columns until the next filming period,
#               e.g. area and x, then the Intervals value is 2).
# The output: 
#   An array of normalized distance values (units are in pixels) 
#   that are concatinated together.

source("~/Documents/MyFolders/DaphniaLab/Functions/ForceFilterFunction.R")

Concat <- function(Inputs,Intervals)
{
  final   <- NULL
  Max     <- dim(Inputs)[2]/2
  elementSequence <- seq(1,Max,1)
  for (i in elementSequence)
  {
    final <- c(final, 
               ForceFilter(Inputs, elementSequence[i], Intervals))
  }
  return(final)
}

