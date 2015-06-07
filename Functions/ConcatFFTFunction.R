
ConcatFFT <- function(Data)
{
  DataIntervals <- 2
  FFTinterval <- 30
  elementSequence <- seq(2,dim(Data)[2],DataIntervals)
  toAnalyze <- NULL
  breaker <- rep(NA,30)
  for (i in elementSequence)
  {    
      examined <- GetFFT(Data[,elementSequence])
      print(class(examined))
      toAnalyze <- c(toAnalyze, examined)    
  }
#  ShuderSpeed <- 30
#  L <- 499
#  HzConvert <- ShuderSpeed/length(toAnalyze)
#  fftVector <- NULL
 # Max <- length(toAnalyze)-FFTinterval
#  series <- seq(1,length(toAnalyze),FFTinterval)
#  for(i in 1:Max)
#  {
   # print(class(toAnalyze))
#    toExamine <- data.matrix(toAnalyze[i:(i+FFTinterval)])
#    examined <- as.numeric(toExamine)
    #print(class(examined))
#    wave <- abs(Re(fft(examined))) #*HzConvert
#    fftVector <- c(fftVector,wave)
#  }
#  out <- fftVector[!is.na(fftVector)]
  return(out)
}

