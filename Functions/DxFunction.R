# A simple difference function taking in an array and outputs an
# array of differences (final - initial)
Dx <- function(x)
{
  y   <- c(x[1:length(x)-1])
  z   <- c(x[2:length(x)])
  out <- y-z
  out <- c(out,NA)
  return(out)
}