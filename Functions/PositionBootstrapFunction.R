PositionBooting <- function (sample,N)
{
  p <- 0.1
  bound <- quantile(sample, p, na.rm = T)
  myboot<-numeric(N)
  sample <- sample[sample>=bound]
  for (i in 1:N)
  {
    x<-sample(sample, length(sample), replace=TRUE)
    myboot[i] <- mean(x, na.rm = T)
  }
  plot(mean(myboot)-sample)
  return(mean(myboot))
}