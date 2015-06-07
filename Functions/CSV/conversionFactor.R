addForce <- function(
  columnNames = c("","Area","X","Y","Slice")
)
{
  if(names(data) != columnNames) {return}
  
  Time <- data$Slice/frameRate + index * 60
  x <- 640-data$X
  y <- data$Slice
  trendline <- lm()
  trendSet <- setdiff(seq(0,length(data$X),5),
                      c(0,length(data$X)))
  Xo <- detrend(x = 640-data$X, 
                tt = 'linear',
                bp = trendSet)
  if (filterWithArea)
  {
    if(notifications)
    {cat("\nYou are filtering the force data with ",
         "the area's deviation outside the 95th percentile ",
         "deviation range.\n")}
    Ao <- data$Area
    Ao[Ao < quantile(Ao,0.025)] <- NA
    Ao[Ao > quantile(Ao,(1-0.025),na.rm = TRUE)] <- NA
    Xo <- Xo * Ao/Ao
  }
  MaxForce <- Filter(Xo)
  MaxForce[MaxForce <= feedingBound] <- NA
  MaxForce[MaxForce > max(DisplacementData, na.rm=T)] <- NA
  forceConvertion <- ConversionFactor(DisplacementData,
                                      ForceData,
                                      UnitConversion)
  MaxForce <- MaxForce * forceConvertion
  data <- cbind(data[1],Time,data[-1],MaxForce)
  
  return()
}





#########
data <- Control12_Time00_Fiber$Area
N <- 10000
out <- numeric(N)
S <- seq(1,length(data),1)
for(i in 1:N)
{
  index <- sample(S,length(data),replace = T)
  out[i] <- max(data[index],na.rm=T)
}

hist(out)
