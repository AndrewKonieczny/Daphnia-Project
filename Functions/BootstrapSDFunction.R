BootingSD <- function (X, N, B)
{
  sample <- X[abs(X)<B]
  myboot<-numeric(N)
  for (i in 1:N)
  {
    x<-sample(sample, length(sample), replace=TRUE)
    myboot[i] <- sd(x, na.rm = T)
  }
  return(mean(myboot))
}
