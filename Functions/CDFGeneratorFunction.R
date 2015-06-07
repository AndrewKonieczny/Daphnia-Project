cdfGenerator <- function(data, accuracy)
{
  data <- data[!is.na(data)]
  accuracy <- 10^accuracy
  S <- seq(0,max(data),accuracy)
  cdf <- numeric(length(S))
  for(i in S)
  {
    stat <- sum(i > data)
    cdf[i] <- stat
  }
  cdf <- cdf[cdf!=0]
  cdf <- cdf/length(S)
  return(cdf)
}