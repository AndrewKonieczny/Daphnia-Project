GetFFT <- function(data)
{
  interval <- 30
  fftVector <- NULL
  data <- data[!is.na(data)]
  Max <- length(data)-interval
  series <- seq(1,length(data),interval)
  for(i in 1:Max)
  {
    examined <- data[i:as.numeric(i+interval)]
    if(sum(is.na(examined))!=interval)
      {
      wave <- abs(Re(fft(examined)))
      fftVector <- c(fftVector,wave)
      }

  }
  out <- fftVector[!is.na(fftVector)]
  return(out)
}