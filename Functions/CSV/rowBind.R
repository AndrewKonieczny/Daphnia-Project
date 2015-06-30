# This function combines .csv files that are of the same animal. If you record 
# an animal 4 times over 6 minutes (at 0,2,4,6 min), this function will combine
# those 4 .csv files into one. It will bind them by row, meaning it will attach
# time 2 to time 0 where the time 0 file will be on top of the time 2 file. I 
# found this useful in easing the data analysis process. This fucntion will 
# also find the local max force produced and create a new column to record that
# data point. This row is left un-converted to force. 
# 
# The file naming convension I used was: "Control01_Time00_Fiber.csv". Where 
# "Control" is the animal ID I used, it can be anything you feel is useful for
# identification of the file, which is followed by two numbers "01" which 
# signify the first animal. I used two digets for consistancy, and I doublt you
# will test more than 99 animals in a given period. Then "Time00" signifies the
# time stamp for when the recording happened over the length of the experiement.
# Lastly, "Fiber" indicates which camera the recording came from. I named each
# camera Fiber and Animal to distinguish the camera focused on fiber movement 
# from the camera focused on animal movement. You can name your cameras whatever
# you want. Use this file naming convension in order to utilize this function.
# 
# Inputs:
#   whereIsTheData: This is a string surrounded by double quotes with the file 
#                   path to where all of your ImageJ .csv outputs are.
#   whereIsTheFunction: This is a string surrounded by double quotes with the
#                       file path to where the forceVector.R file is. rowBind.R
#                       requires the use of that function.
#   animalID: This is a string identifier for the animal set. I typically used
#             the word "Drug" or "Control" based on the case of the animal set.
#             The animalID variable is a part of the naming convension I used 
#             for the data.
#   findForce: This is a boolean (T/F) value which, when true, will add on a 
#              force vector for you. When false, no force vector will be added.
#              The default value for this variable is true.
#   frameRate: This is a numeric value for the frame rate of the camera. The 
#              default value for this variable is 30 frames/s.
#   separator: This is a character variable which indicates how the .csv file is
#              structured. Comma-separated values (csv) is typically separated 
#              by commas but I left this adjustable because I needed to change it
#              to a tab ("\t") character for some of my data. The default 
#              character is the comma, ",".
#   timeInterval: This is a numeric value for the time interval between recording
#                 sessions in a single experiment. I recorded the animal at 2 
#                 minute intervals so the default is 2. This value is used to 
#                 find the next file in a sequence of recording sessions, (i.e. 
#                 "Control01_Time00_Fiber", "Control01_Time02_Fiber", 
#                 "Control01_Time04_Fiber", etc.).
#   returnAsList: This is a boolean (T/F) variable which asks you what output 
#                 you want from this function. It can output the data as a list 
#                 of data.frames or as a .csv file. The default value for this 
#                 variable is false.
# 
# Output:
#   This function by default (see returnAsList) outputs a series of .csv files 
#   to where the directary is currently set to. This fucntion can output a list 
#   of data.frames if you so choose.
# 
# The following is an example of how to use the code.
#
# dose <- "1E2"
# ids <- "Drug"
# whereData <- paste0("~/Documents/MyFolders/DaphniaLab/",
#                     "Raw Data/A68930/Dose_uM_CSV/",dose,"/")
# whereFun <- "~/Documents/MyFolders/DaphniaLab/Functions/CSV/"
# setwd(file.path(paste0(whereData,"AllAnimals")))
# t = rowBind(whereIsTheData = whereData,
#         whereIsTheFunction = whereFun,  
#         animalID = ids,
#         frameRate = 30,
#         separator = "\t", 
#         timeInterval = 2, 
#         returnAsList = T)



#####
rowBind <- function(whereIsTheData,
                    whereIsTheFunction, 
                    animalID,
                    findForce = TRUE,
                    frameRate = 30,
                    separator = ",",
                    timeInterval = 2,
                    returnAsList = FALSE)
{
  functionPath <- paste0(whereIsTheFunction, "forceVector.R")
  if(!file.exists( functionPath)){
    stop("file path \n",
         functionPath,
         "\n not found. The file for forceVector.R was not found at this path.")
    break
  }
  if(file.exists( functionPath)){
    source( functionPath)
    out <- NULL
    files <- NULL
    animalNumber <- 1
    timeStamp <- 0
    path <- function(where, id, index, time) {
      file.path(where, paste0(id, sprintf("%02.0f_", index), 
                              sprintf("Time%02.0f_Fiber.csv", time)))
    }
    if(!file.exists( path(where = whereIsTheData,
                          id = animalID, 
                          index = animalNumber, 
                          time = timeStamp))){
      stop("file path \n",
           path(where = whereIsTheData,
                id = animalID, 
                index = animalNumber, 
                time = timeStamp),
           "\n not found. Check your file naming format. \n
           Example: Drug01_Time02_Fiber.csv \n
           Where Drug is the animalID.")
    }
    while(file.exists( path(where = whereIsTheData,
                            id = animalID, 
                            index = animalNumber, 
                            time = timeStamp))) {  #changes animalnum
      files <- NULL
      while(file.exists( path(where = whereIsTheData, 
                              id = animalID, 
                              index = animalNumber, 
                              time = timeStamp))) { #changes time
        files <- c(files, path(where = whereIsTheData, 
                               id = animalID, 
                               index = animalNumber, 
                               time = timeStamp))
        timeStamp <- timeStamp + timeInterval
      }
      if(findForce) {
        tempData <- lapply(files, 
                           read.delim, 
                           sep = separator)
        appendedData <- lapply(tempData, 
                               forceVector, 
                               where = whereIsTheFunction,
                               timeRecorded = 0,
                               frameRate = 30,
                               range = 3, 
                               filterArea = FALSE)
      }
      else {
        appendedData <- lapply(files, 
                               read.delim, 
                               sep = separator)
      }
      finalData <- do.call(rbind, appendedData)
      ID <- paste0(animalID, 
                   sprintf("%02.0f", animalNumber))
      names(finalData) <- unlist(lapply(list(
        names(finalData)), paste0, sprintf("_%02.0f", animalNumber)))  
      names(finalData)[1] <- ID   
      if(!returnAsList) {
        dir.create(paste0(whereIsTheData,"/AllAnimals"),showWarnings=F)
        write.csv(finalData, file = paste0("AllAnimals/",ID,".csv")) 
      }
      else {
        out <- c(out, finalData)
      }
      animalNumber <- animalNumber + 1
      timeStamp <- 0
    }
    if(returnAsList){
      return(out)
    }
    else{
      return(paste("see",paste0(whereIsTheData,"/AllAnimals")))
    }
  }
  else{
    stop("functions are not in this path: ",functionPath)
  }
}

