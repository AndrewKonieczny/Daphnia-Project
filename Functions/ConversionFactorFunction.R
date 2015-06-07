ConversionFactor <- function (DisplacementData,
                              ForceData,
                              UnitConversion)
{
  FitLine <- lm(ForceData~DisplacementData)
  Conversion <- coef(FitLine)
  return(Conversion)
}