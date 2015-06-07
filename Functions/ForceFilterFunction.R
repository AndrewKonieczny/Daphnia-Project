# This function finds the max force values by removing values around 
# a local maximum value. The input is the data to be filtered 
# (preferably after being concatinated) with a bound that is simply 
# the lower limit of which you want no values below it. The output 
# is an array of the filtered data.

source("~/Documents/MyFolders/DaphniaLab/Functions/ZeroFunction.R")
source("~/Documents/MyFolders/DaphniaLab/Functions/AreaDevFunction.R")

ForceFilter <- function(data,element,interval)
{
  columnZero <- element * interval
  columnArea <- element * interval - 1
  vectorZero <- Zero(data, columnZero)
  vectorArea <- AreaDev(data, columnArea)
  cleanedData <- vectorZero * vectorArea
  cleanedDataA <- c(NA,cleanedData[1:(length(cleanedData)-1)])
  cleanedDataB <- c(NA,cleanedData[2:length(cleanedData)])
  cleanedDataC <- c(NA,cleanedData[3:length(cleanedData)],NA)
  final <- rep(NA,length(cleanedData))
  out <- rep(NA,length(cleanedData))
  for (i in 2:length(final))
  {
    compare <- c(cleanedDataA[i],cleanedDataB[i],cleanedDataC[i],final[i-1])
    if (sum(is.na(compare)) < length(compare))
    {
      out[i] <- max(compare,na.rm = T)
    }
    else {out[i] <- NA}
  }
  for (i in 1:length(cleanedData))
  {
    final[i] <- ifelse(out[i] == cleanedData[i],cleanedData[i],NA)
  }
  return(final)
}

