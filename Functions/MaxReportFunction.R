MaxReport <- function(inData,CalibrationData_DispVariable)
{
  x <- inData
  x[x > max(CalibrationData_DispVariable)] <- NA
  x[x == 0] <- NA
  return(x)
}