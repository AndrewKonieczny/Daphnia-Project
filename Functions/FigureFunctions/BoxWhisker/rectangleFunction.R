# Constructs a rectangle to tack on a 
# box-whisker plot to better distinguish
# between drug concentrations
rectangle <- function (input, 
                       orderNumber, 
                       densityValue, 
                       isVert = TRUE)
{
  if(isVert){
    yDown <- as.numeric(quantile(input, 
                                 0.25, 
                                 na.rm=T))
    yUp <- as.numeric(quantile(input, 
                               0.75, 
                               na.rm=T))
    xLeft <- 0.6 + (orderNumber - 1)
    xRight <- 1.4 + (orderNumber - 1)
  }
  else{
    yDown <- 1.4 + (orderNumber - 1)
    yUp <- 0.6 + (orderNumber - 1)
    xLeft <- as.numeric(quantile(input, 
                                 0.75, 
                                 na.rm=T))
    xRight <- as.numeric(quantile(input, 
                                  0.25, 
                                  na.rm=T))
  }
  rect(xLeft, 
       yDown, 
       xRight, 
       yUp, 
       col = rgb(red = 0, 
                 green = 0, 
                 blue = 0, 
                 alpha = 0.8),
       density = densityValue,
       angle = 45)
  if(floor(orderNumber / 2) != 
       orderNumber / 2)
  {
    rect(xLeft, 
         yDown, 
         xRight, 
         yUp, 
         col = rgb(red = 0, 
                   green = 0, 
                   blue = 0, 
                   alpha = 0.8),
         density = densityValue,
         angle = 45 + 90)
  }
}