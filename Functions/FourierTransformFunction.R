
FFT_Function <- function(data,interval)
{
  
  FourierTransform <- X[!is.na(X)]
  ShuderSpeed <- 30
  HzConvert <- ShuderSpeed/length(FourierTransform)
  HzAxis <- seq(0, (length(FourierTransform) - 1), 1) * HzConvert
  plot(x = HzAxis[1:length(HzAxis)], 
       density(FourierTransform)$y[1:length(FourierTransform)], 
       type = "l", 
       main = "FFT of Feeding Behavior", 
       sub = NULL, 
       xlab = "Frequency (Hz)", 
       ylab = "Magnitude")
}