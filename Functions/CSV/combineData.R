# This code will take in several individual measurement periods of 
# a single animal and put it into a single csv file in a specified 
# directory.
combineData <- function(whereIsTheFunction,
                        mainDirectory,
                        animalID = "Animal",
                        animalCount = 20,
                        whichCamera = "Fiber",
                        columnNames = c("","Area","X","Y","Slice"), 
                        frameRate = 30, 
                        separator = ",",
                        outputFileName = "", 
                        totalTimeFilmed = 6, 
                        filmingTimeInterval = 2,
                        addForceVector = TRUE,
                        filterWithArea = TRUE,
                        extremeAreaFilter = TRUE,
                        framesBetweenForces = 3)
{
  source( paste0(whereIsTheFunction, "readTable.R"))
  source( paste0(whereIsTheFunction, "columnBind.R"))
  source( paste0(whereIsTheFunction, "forceVector.R"))
  source( paste0(whereIsTheFunction, "fileNaming.R"))
  
  final <- NULL
  if(nchar(outputFileName) < 1) {
    break
  }
  if(!is.character(mainDirectory) || nchar(mainDirectory) == 0) {
    break
  }
  for(animalNumber in 1:animalCount) {
    cat("\nThe animal number",animalNumber,"\n")
    animal <- NULL
    #if(getwd() == path.expand(mainDirectory)) {
    timeSequence <- seq(0, 
                        totalTimeFilmed, 
                        filmingTimeInterval)
    for(index in timeSequence) {
      cat("\ntime stamp ",index,"\n")
      file <- fileNaming(directory        = mainDirectory, 
                         ID               = animalID, 
                         animalNumber     = animalNumber,  
                         timeIndex        = index, 
                         cameraID         = whichCamera) 
      if(!file.exists(file)) break
      if( file.exists(file)) {
        cat("\n", file, "\n", file.exists(file))
        data <- readTable(directory       = file, 
                          ID              = animalID,
                          animalNumber    = animalNumber,
                          timeStamp       = index,
                          frameRate       = frameRate,
                          columnNames     = columnNames, 
                          separator       = separator)
      }
      if(addForceVector && is.data.frame(data)) {
        force <- forceVector(where             = whereIsTheFunction, 
                             input             = data,
                             minThreshold      = mean,
                             range             = framesBetweenForces, 
                             filterArea        = filterWithArea,
                             isExtremeOutliers = extremeAreaFilter)
        #force <- as.numeric(force)
        cat("\nforce vector ", class(force))
        #data <- cbind(data, force, deparse.level = 1)
      }
      animal <- rbind(animal, data)
    }
    final <- columnBind(finalFrame = final,
                        addedFrame = animal)
    #}
  }
  write.csv(final, file = outputFileName)
  print("CSV was created")
}


