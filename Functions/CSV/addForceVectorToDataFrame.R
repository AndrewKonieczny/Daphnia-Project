
addForceVectorToDataFrame <- function(directory, 
                                      where, 
                                      ID,
                                      animalNumber,
                                      columnNames = c("", 
                                                      "Area", 
                                                      "X", 
                                                      "Y", 
                                                      "Slice"), 
                                      separator = ",",
                                      frameRate = 30, 
                                      secondsOfRecording = 15,
                                      minThreshold = mean,
                                      range = 3, 
                                      filterArea = TRUE)
{
  source( paste0(where, "readTable.R") )
  source( paste0(where, "forceVector.R") )
  if( !file.exists(directory)) {return}
  data <- readTable(directory, 
                    ID,
                    animalNumber,
                    timeStamp,
                    frameRate,
                    columnNames, 
                    separator) 
  MaxForce <- forceVector(where, 
                          data,
                          minThreshold,
                          range, 
                          filterArea)
  colnames(data)[1] <- 
    paste0(animalID, sprintf("%02.0f", animalNumber))
  data <- cbind(data[1],Time,data[-1],MaxForce)
  return(data)  
}