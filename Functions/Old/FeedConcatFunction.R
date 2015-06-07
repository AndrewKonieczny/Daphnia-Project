source("~/Documents/MyFolders/DaphniaLab/Functions/ZeroFunction.R")

FeedConcat <- function(Inputs,
                       Intervals)
{
  final   <- NULL
  Max     <- dim(Inputs)[2]/2
  elementSequence <- seq(2,Max,2)
  for (i in 1:(Max/2))
  {
    index <- i + 1
    final <- c(final,Zero(Inputs, index))
  }
  return(final)
}