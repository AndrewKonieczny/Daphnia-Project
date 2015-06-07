# writes out the directory of the files to be found.

fileNaming <- function(directory, 
                       ID, 
                       animalNumber, 
                       timeIndex,
                       cameraID) 
{
  temp <- c(directory, "/")
  temp <- c(temp, ID, sprintf("%02.0f", animalNumber))
  temp <- c(temp,"_Time",sprintf("%02.0f", timeIndex))
  temp <- c(temp, "_", cameraID, ".csv")
  out <- paste0(temp, collapse = "")
  return(out)
}
