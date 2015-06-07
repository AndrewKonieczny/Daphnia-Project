# This code will take in several individual measurement periods of 
# a single animal and put it into a single csv file in a specified 
# directory.
source("~/Documents/MyFolders/DaphniaLab/Functions/CSV/FilterFunction.R")
require(pracma)
csvCreater <- function(mainDirectory = getwd(),
                       animalID = "Animal",
                       animalCount = 20,
                       whichCamera, 
                       addForceVector = FALSE,
                       DisplacementData,
                       ForceData,
                       UnitConversion = 10^6,
                       filterWithArea = FALSE,
                       totalTimeFilmed = 6, 
                       filmingTimeInterval = 2,
                       columnNames = c("","Area","X","Y","Slice"), 
                       frameRate = 30, 
                       secondsOfRecording = 15, 
                       separator = ",",
                       ImageWidth = 640,
                       outputFileName = "",
                       feedingBound = 0.5,
                       notifications = FALSE)
{
  if(notifications)
  {cat("\nYou have entered the csv creater function.\n")}
  final <- NULL
  
  for(num in 1:animalCount)
  {
    animal <- NULL
    if(nchar(outputFileName) < 1)
    {
      cat("\nYou need to give an output file name!\n")
      break
    }
    if(getwd() == path.expand(mainDirectory)) 
    {
      if(notifications)
      {cat("\nYou entered the correct directory.\n")}
      counter <- 0
      timeSequence <- seq(0, totalTimeFilmed, filmingTimeInterval)
      fileNamingFormat <- c(
        paste0("/",animalID, sprintf("%02.0f", num),"_Time"), 
        paste0("_", whichCamera, ".csv"))
      paste0(fileNamingFormat[1],fileNamingFormat[2])
      for(index in timeSequence)
      {
        if(notifications)
        {cat("\nYou entered the time input for-loop.\n")}
        fileDirectory <- paste0(mainDirectory,
                                fileNamingFormat[1],
                                sprintf("%02.0f", index), 
                                fileNamingFormat[2])
        if(!file.exists(fileDirectory))
        {
          if(notifications)
          {cat("\nThe file directory\n", 
               fileDirectory,"\ndoes not exist.\n")}
          break
        }
        if(file.exists(fileDirectory))
        {
          if(notifications)
          {cat("\nThe file,\n",fileDirectory,"\ndoes exists\n")}
          data <- read.table(fileDirectory, 
                             header = TRUE, 
                             sep = separator, 
                             quote = "\"'", 
                             dec = ".", 
                             numerals = c("allow.loss", 
                                          "warn.loss", 
                                          "no.loss"), 
                             row.names = NULL, 
                             col.names = c(paste0(animalID, sprintf("%02.0f", num)),
                                           columnNames[-1]), 
                             as.is = TRUE, 
                             na.strings = "NA", 
                             colClasses = NA, 
                             nrows = frameRate * secondsOfRecording,
                             skip = 0, 
                             check.names = TRUE, 
                             fill = FALSE, 
                             strip.white = FALSE, 
                             blank.lines.skip = TRUE, 
                             comment.char = "#", 
                             allowEscapes = FALSE, 
                             flush = FALSE, 
                             stringsAsFactors = 
                               default.stringsAsFactors(),
                             fileEncoding = "", 
                             encoding = "unknown", 
                             text, 
                             skipNul = FALSE)
          Time <- data$Slice/frameRate + index * 60
          if(addForceVector)
          {
            if(notifications)
            {cat("\nYou are including a force column ",
                 "to the output data frame\n")}
            trendSet <- setdiff(seq(0,length(data$X),5),
                                c(0,length(data$X)))
            Xo <- detrend(x = 640-data$X, 
                          tt = 'linear',
                          bp = trendSet)
            if (filterWithArea)
            {
              if(notifications)
              {cat("\nYou are filtering the force data with ",
                   "the area's deviation outside the 95th percentile ",
                   "deviation range.\n")}
              Ao <- data$Area
              Ao[Ao < quantile(Ao,0.025)] <- NA
              Ao[Ao > quantile(Ao,(1-0.025),na.rm = TRUE)] <- NA
              Xo <- Xo * Ao/Ao
            }
            MaxForce <- Filter(Xo)
            #MaxForce[MaxForce <= feedingBound] <- NA
            #MaxForce[MaxForce > max(DisplacementData, na.rm=T)] <- NA
            #forceConvertion <- ConversionFactor(DisplacementData,
            #                                    ForceData,
            #                                    UnitConversion)
            #MaxForce <- MaxForce * forceConvertion
            data <- cbind(data[1],Time,data[-1],MaxForce)
          }
          else{data <- cbind(data[1],Time,data[-1])}
          
          line <- rep(NA, dim(data)[2])
          animal <- rbind(animal,data,line)
          if(notifications)
          {cat("\ndata has dimensions ",dim(data),"\n")}
          counter <- counter + 1
        }
      }
      #if(counter > 0)
      #{
      if(notifications)
      {cat("\nYou successfully went through the for-loop", 
           counter, "time(s)\n",names(animal))}
      if(class(final) == "NULL")
      {
        if(notifications){cat("\nfinal is a NULL object\n",
                              class(final),class(animal))}
        final <- rbind(final,animal)
      }
      if(class(animal) == "data.frame" && class(final) == "data.frame")
      {
        if(notifications){cat("\nboth data sets are data frames\n")}
        if(dim(animal)[1] < dim(final)[1])
        {
          diff <- dim(final)[1] - dim(animal)[1]        
          if(notifications)
          {cat("\nAdding ",diff," rows to animals data frame.")}
          for(n in 1:diff)
          {
            nullRow <- rep(NA, dim(animal)[2])
            animal <- rbind(animal,nullRow)
          }
        }
        if(dim(final)[1] < dim(animal)[1])
        {
          diff <- dim(animal)[1] - dim(final)[1]        
          if(notifications)
          {cat("\nAdding ",diff," rows to final data frame.")}
          for(n in 1:diff)
          {
            nullRow <- rep(NA, dim(final)[2])
            final <- rbind(final,nullRow)
          }
        }
        # if(file.exists(fileDirectory))
        # {
        final <- cbind(final,animal)
        if(notifications)
        {cat("\nFinal has dimensions ",dim(final),"\n")}
        #}
      }
      #}
      #else{cat("\nYou need to fix your working directory", 
      #         "Below are simple instructions to help you:", 
      #        "Click on the console (where this text is) and", 
      #         "type into the terminal the following code,", 
      #         "\nsetwd( enter where your data files are from here )\n", 
      #         "Then press enter and your wd should have changed,", 
      #         "unless an error is shown\n", fill = 3)}
      
    }
    
  }
  if(notifications){print(dim(final))}
  write.csv(final, file = outputFileName)
}

