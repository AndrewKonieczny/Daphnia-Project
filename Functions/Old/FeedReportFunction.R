FeedReport <- function(InputData,DataInterval)
{
  zero <- function(zeroInput, zeroColumn)
  {
    numbers <- 640 - data.matrix(zeroInput[zeroColumn])
    central <- mean(numbers[abs(dx(numbers)) < 10], na.rm = TRUE,
                    trim = 0.2)
    out <- as.vector(numbers - central, mode = "numeric")
    return(out)
  }
  concat <- function(Inputs,Intervals)
  {
    final   <- NULL
    Max     <- dim(Inputs)[2]/2
    elementSequence <- seq(1,Max,1)
    for (i in elementSequence)
    {
      final <- c(final, 
                 zero(Inputs, elementSequence[i], Intervals))
    }
    return(final)
  }
  
  ForceData <- ForceFilter(concat(InputData, DataInterval), 
                           lowerbounds(InputData,100))
  return(ForceData)
}