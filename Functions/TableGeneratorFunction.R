source("~/Documents/MyFolders/DaphniaLab/Functions/FilterFunction.R")
require(pracma)



tableGenerator <- function(fileDirectory,
                           addForceVector = TRUE,
                           filterWithArea = TRUE,
                           columnNames = 
                             c("","Area","X","Y","Slice"), 
                           frameRate = 30, 
                           secondsOfRecording = 15, 
                           separator = ",",
                           notifications = FALSE)
{
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
                       col.names = columnNames, 
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
    print(names(data))
    Time <- data$Slice/frameRate
    if(addForceVector)
    {
      if(notifications) {cat("\nYou are including a force column ", "to the output data frame\n")}
      
      trendSet <- setdiff(seq(0,length(data$X),50),
                          c(0,length(data$X)))
     # print(coef(trendSet))
      Xo <- detrend(640-data$X, 
                    'linear',
                    trendSet)
      MaxForce <- Filter(Xo)
      MaxForce[MaxForce <= 0] <- NA
      animals <- cbind(data[1],Time,data[-1],MaxForce)
    }
    if (filterWithArea)
    {
      if(notifications)
      {cat("\nYou are filtering the force data with ",
           "the area's deviation outside the 95th percentile ",
           "deviation range.\n")}
      Ao <- data$Area
      Ao[Ao < quantile(Ao,0.025)] <- NA
      Ao[Ao > quantile(Ao,(1-0.025),na.rm = TRUE)] <- NA
      MaxForce <- MaxForce * Ao/Ao
    }
    else{animals <- cbind(data[1],Time,data[-1])}
    animals <- rbind(animals, rep(NA, dim(animals)[2]))
    return(animals)
  }
  if(!file.exists(fileDirectory))
  {
    if(notifications)
    {cat("\nThe file directory\n", 
         fileDirectory,"\ndoes not exist.\n")}
    break
  }
  else{cat("\nYou need to fix your working directory", 
           "Below are simple instructions to help you:", 
           "Click on the console (where this text is) and", 
           "type into the terminal the following code,", 
           "\nsetwd( enter where your data files are from here )\n", 
           "Then press enter and your wd should have changed,", 
           "unless an error is shown\n", fill = 3)}
}
