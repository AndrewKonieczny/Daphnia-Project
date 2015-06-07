# Permutation Test
N<-10000            # number of permutations
PValue <- function(A,B,N)
{
  combined <- c(A,B)
  result <- NULL
  observed <- mean(A) - mean(b)
  for (i in 1:N)
  {
    shuffled <- sample(combined, length(combined), replace = FALSE)
    randomA <- shuffled[1:length(A)]
    randomB <- shuffled[(length(A)+1):length(combined)]
    result[i] <- mean(randomA) - mean(randomB)
  }
  pTest <- result
  pvalue <- sum(abs(pTest) >= abs(observed))/N
  return(pvalue)
}