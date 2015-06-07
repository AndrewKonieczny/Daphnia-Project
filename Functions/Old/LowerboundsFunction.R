# Takes in input data, and goes therough a series of possible lower
# bounds to create an array of means. The rate of change of the 
# array of means is then taken. The mean of this array is found and 
# that mean is then compared to the rest of the array to find the 
# number of points in the array are less than the mean. This is the 
# best lower bound for the data set.
# Input: data and a number of lower bounds you want to test
# Output: the best lower bound

source("~/Documents/MyFolders/DaphniaLab/Functions/DxFunction.R")
source("~/Documents/MyFolders/DaphniaLab/Functions/ConcatFunction.R")
source("~/Documents/MyFolders/DaphniaLab/Functions/ForceFilterFunction.R")

Lowerbounds <- function(input)
{
  interval <- 0.01
  MIN <- min(input, na.rm = T)
  MAX <- mean(input, na.rm = T)
  itterations <- MAX/interval
  v     <- seq(MIN,MAX,interval)
  out   <- numeric(itterations)
  for(i in 1:length(v))
  {
    out[i] <- mean(input[input > v[i]], na.rm = T)
  }
  M <- match(min(Dx(out),na.rm=TRUE), Dx(out), nomatch = NA_integer_, incomparables = NULL)
  best <- v[M]
  return(best)
}