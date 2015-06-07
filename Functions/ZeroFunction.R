source("~/Documents/MyFolders/DaphniaLab/Functions/DxFunction.R")

Zero <- function(zeroInput, zeroColumn)
{
  # Conversion is done to correct the x-y coordinate system.
  ImageWidth <- 640
  # Number of bootstraps to be done
  N <- 10000
  rawdata <- data.matrix(zeroInput[zeroColumn])
  data <- ImageWidth - rawdata
  Simdata <- data[!is.na(data)]
  Simdata <- Simdata[Simdata > quantile(Simdata,0.25) ]
  Simdata <- Simdata[Simdata < quantile(Simdata,0.75) ]
  index <- seq(1,length(Simdata),1)
  FitLine <- lsfit(index, Simdata)
  #print(FitLine)
  out <- vector(mode = "numeric", length = length(data))
  for (i in 1:length(data))
  {
    AdjustValue <- i*coef(FitLine)[2] + coef(FitLine)[1]
    out[i] <- as.numeric(data[i]) - AdjustValue
  }
  myboot<-numeric(N)
  for (i in 1:N)
  {
    x<-sample(out, length(out), replace=TRUE)
    myboot[i] <- median(x, na.rm = T)
  }
  final <- out -mean(myboot)
  return(final)
}