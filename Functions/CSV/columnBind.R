# This function concatinates all the 'rowBound' animal recording periods for a 
# given drug concentration. The data is formatted like so:
#
#   Animal_1  var_1a  var_1b  ... Animal_2  var_2a  ... etc.
#
# Where each thing listed above is a column vector and each row is an 
# observation at the given time (specified by the time or "Slice" which is  
# the ID for the spacific frame).
# 
# For this function to read the .csv files correctly, they need to be named 
# something along the lines of:  Control01, Control02, Control03, ...
# Where "Control" is the animalID ( see below ) and the animal number has a 
# zero place holder ( if you have more than 99 animals or start counting your 
# animals at zero this function might not work ).

# I also padded each data set with NA values so that the data.frame has the 
# same demensions between animals. So when you come accross a long string of
# NA rows, that means the recording data for that animal is done.
# 
# Inputs:
#   whereIsTheData:  A character string (i.e. a string of characters in double 
#                    quotes like: "Hello World") that specifies where the .csv 
#                    data is so the function can grab it.
#             
#   animalID:  A character string that identifies the animal group (i.e.
#              "GroupA", or "Control").
#             
#   separator:  A character string that indicates what the separates the  
#               columns in your data file. Typically, the .csv uses the "," 
#               which is the default value for the variable. So if your data
#               is separated by "," you can not include that variable when you 
#               call the function.
#             
#   returnCSV:  A boolean value (TRUE or FALSE in capital letters) that 
#               indicates if you want the funtion to output a .csv file or use
#               this function to output a data.frame object. Default for this
#               variable is TRUE.
#             
#   csvName:  A character string which, if returnCSV = TRUE, will be the name  
#             of the output .csv file. This file will contain all the data from
#             your individual animal .csv files. The default setting for this 
#             variable is NULL, which will result in the file being named after
#             the folder it is in with the "All_" phrase in front ( i.e. 
#             "All_folderName" ). So if you don't like the default name, change
#             it!
#             
# Outputs:
#   If returnCSV is FALSE, the output will be a data.frame object, if returnCSV
#   is TRUE then the output will be a .csv file. If a .csv file is produced, 
#   the file will be dropped where your working directory is pointed at the 
#   time of running ( you can see what your WD is by typing getwd() into the 
#   console and you can set it by setwd() ). 
# 
# Below is an example of what I did to run this function where *** denotes 
# notes:
#
# *** ids is the animalID, eachanimal in this folder was named like: 
# *** "Drug01", "Drug02", ...
# ids = "Drug"
#    
# *** dose is the name of the folder that held the animals with the same dose.
# dose = "1E2"
#
# *** whereData is the directory listing, using paste0() to save space. You 
# *** can also see I used the dose variable in the directory for convenience.
# whereData = 
#   paste0("~/Documents/MyFolders/DaphniaLab/Raw Data/A68930/Dose_uM_CSV/", 
#          dose, "/AllAnimals")       
#
# *** newfile is the file name for the output .csv
# newfile <-paste0("All_",ids,"_",dose,".csv")
#
# *** this sets the working directory to the folder whereData is pointing to
# setwd(file.path(whereData))
#
# *** Here is where the function is implemented using the above parameters
# columnBind(whereIsTheData = whereData, 
#            animalID = ids, separator = ",",
#            csvName = newfile) 
# *** You might also notice I'm taking advantage of the default settings for
# *** the returnCSV and csvName variables.

columnBind <- function(whereIsTheData,
                       animalID,
                       separator = ",",
                       returnCSV = TRUE,
                       csvName = NULL)
{
  # the variable where the file paths go
  files <- NULL
  int <- 1
  tempPath <- function(id,i) {paste0(id,sprintf("%02.0f.csv",i))}
  while(file.exists(file.path(whereIsTheData,tempPath(animalID,int))))
  {
    files <- c(files, tempPath(animalID,int))
    int <- int + 1
  }
  fileNames <- file.path(whereIsTheData, files)
  unpaddedData <- lapply(fileNames,  
                         read.delim, 
                         sep = separator)
  maxDim <- max(unlist(lapply(unpaddedData,nrow)))
  for(i in 1:length(unpaddedData))
  {
    if(nrow(unpaddedData[[i]]) < maxDim)
    {
      diff <- maxDim - nrow(unpaddedData[[i]])
      NArows <- matrix(data = NA, 
                       nrow = diff, 
                       ncol = ncol(unpaddedData[[i]]))
      NArows <- data.frame(NArows)
      names(NArows) <- names(unpaddedData[[i]])
      unpaddedData[[i]] <- rbind(unpaddedData[[i]],NArows)
    }
  }
  paddedData <- do.call(cbind, unpaddedData)
  finalData <- paddedData[names(paddedData) != "X"]
  if(returnCSV) {
    if( is.null(csvName) ) {
      newName <- paste0("All_", 
                        tail(unlist(strsplit(
                          x = whereIsTheData,
                          split = "/")),1),".csv")
      write.csv(finalData, file = newName)
    }
    else{
      if(grepl(".csv",csvName)) write.csv(finalData, file = csvName)
      
      else write.csv(finalData, file = paste0(csvName,".csv"))
    }
  }
  else{finalData}
}
