# 'z_area' uses the trimmed mean to find the absolute value of 
# the z-score for the fiber's area values, the z-score is then 
# used to remove any valuses that deviate too far from the 
# area's trimmed mean.

AreaDev <- function(areaInput, areaColumn)
{
  g <- data.matrix(areaInput[areaColumn])
  trimMean <- mean(g, na.rm = TRUE)
  standardDeviation <- sd(g, na.rm=TRUE)
  zArea <- numeric(length(g))
  for (i in 1:length(g))
  {
    zScore <- abs(g[i] - trimMean) / standardDeviation
    zArea[i] <- ifelse(zScore > 2, NA, 1)
  }
  return(zArea)
}