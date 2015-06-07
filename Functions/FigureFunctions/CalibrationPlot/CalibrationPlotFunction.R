# This function outputs a png of the calibration plot with the
# inputted displacement and force data from calibrating the fiber.

CalibrationPlot <- function(Displacement,         # displacement 
                            # data.
                            Force,                # force data.
                            PPI = 300,            # dpi value.
                            interceptZero = TRUE, # is the 
                            # intercept zero?
                            showRsquared = TRUE,  # display R^2.
                            showXYCorrelation = FALSE, 
                            # display the
                            # correlation 
                            # between the 
                            # displacement 
                            # and force data.
                            showSlope = FALSE,    # display slope.
                            showCIRegion = TRUE)  # display the 
# 95% confidence 
# interval for 
# fitted line.
{
  x <- Displacement
  y <- Force
  if(interceptZero){
    mylm <- lm(Force ~ Displacement + 0)
  }
  else{
    mylm <- lm(Force ~ Displacement)
  }
  fittedForce <- mylm$fitted.values
  ppi <- PPI
  png("Calibration_Plot.png", 
      width = 6*ppi, 
      height = 6*ppi, 
      res = ppi, 
      pointsize = 12, 
      type = "cairo")
  plot(x = Displacement, 
       y = Force,
       xlab = "",
       ylab = "",
       xlim = c(0, max(Displacement)), 
       ylim = c(0, max(Force)),
       axes = FALSE)
  title(main = NULL,
        xlab = "Displacement (pixels)",
        ylab = expression(paste("Force (",mu,N,")")),
        cex.lab = 1.5,
        line = 2,
        family = "Times")
  axis(side = 1, 
       pos = 0, 
       lwd = 2, 
       cex.axis = 1.25,
       family = "Times")
  axis(side = 2, 
       pos = 0, 
       lwd = 2, 
       cex.axis = 1.25,
       family = "Times")
  abline(mylm, col="black")
  if(showRsquared){
    text(55, 20,
         bquote(italic(R)^2 == .(format(cor(Force, 
                                            fittedForce)^2, 
                                        digits = 3))))
  }
  if(showXYCorrelation){
    text(83, 19,
         bquote(italic(Cor(X,Y)) == .(format(cor(Displacement, 
                                                 Force), 
                                             digits = 3))))
  }
  if(showSlope){
    slope <- mylm$coefficients["(Intercept)"]
    if(!is.na(slope)){
      text(75, 18,
           bquote(italic(Slope) == .(format(slope, 
                                            digits = 3))))
    }
    else{
      text(75, 18,
           bquote(italic(Slope) == .(format(coef(mylm), 
                                            digits = 3))))
    }
  }
  dev.off()
}