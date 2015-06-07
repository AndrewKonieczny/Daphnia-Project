source("~/Documents/MyFolders/DaphniaLab/Functions/ConcatFunction.R")
source("~/Documents/MyFolders/DaphniaLab/Functions/MaxReportFunction.R")


Swimming <- function(InputData,
                       DataInterval,
                       CalibrationDisp)
{
  maximumForceReportable <- max(CalibrationDisp)
  concatination <- Concat(InputData, DataInterval)
  outData <- MaxReport(concatination,maximumForceReportable)
  outData <-concatination
  outData <- outData[!is.na(outData)]
  #outData <- outData[outData > BootingSD(outData,1000,50)]
  return(outData)
}