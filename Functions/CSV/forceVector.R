# The forceVector function takes in the individual data.frame of a single 
# recording session of an animal and adds on a force column and a time column. 
# The format of the data, aquired from ImageJ, should have at least the following
# column vectors: "Area", "X", "Y", "Slice". The local max is found from the data
# in the X variable. Slice is used to make the Time vector. Area can be used to
# to filter the X data but is not necessary and typically removes at most 8 out 
# a few hundred recorded forces. 
# 
# Inputs:
#   where: A string path to where the functions are kept. The forceVector  
#          function requires filter.R and areaFilter.R.
#   input: A data frame from a .csv file for a single animal over a single 
#          recording period.
#   timeRecorded: A numeric value which represents the time stamp of the data 
#                 set. The default value is 0, this number is considered to be a
#                 time in minutes and is then divided by 60 to get seconds. That
#                 value is then added to the time vector. This is not necessary 
#                 for the function to work just is a convenient option to have 
#                 if desired.
#   frameRate: A numeric value for the frame rate of the camera. This number is 
#              used to create the new time variable that is added to the data 
#              frame.
#   range: A numeric value (needs to be a whole number) that sets the range for 
#          deciding what the local max value is. The default value is 3, meaning 
#          the function will find the maximum value between 3 elements.
#   filterArea: A boolean value (T/F) when true will filter the local max values 
#               by area deviation. If a specific area value is an outlier of the 
#               area data set (from the input data frame) then the corresponding 
#               local max value, if there is one, is not included. The default 
#               setting is false, since this filter removes a negligable number 
#               of local maxima values.  
#   
# Outputs:
#   The output is a modified data frame base on the input. The function adds on 
#   two columns for time and "Force". The force column is not converted into 
#   micro Newton, it is a numeric vector of displacement of the fiber in pixels.
# 
# Below is an example of how one could implement the function:
# 
# whereFun <- "~/Documents/MyFolders/DaphniaLab/Functions/CSV/"
# 
# t <- forceVector(where = whereFun,
#                  input = Control01_Time00_Fiber,
#                  timeRecorded = 0,
#                  frameRate = 30,
#                  range = 3, 
#                  filterArea = F)

forceVector <- function(where, 
                        input,
                        timeRecorded = 0,
                        frameRate = 30,
                        range = 3, 
                        filterArea = FALSE){
  source( paste0(where, "filter.R") )
  source( paste0(where, "areaFilter.R") )
  if( class(input) == "data.frame" ) {
    colNames <- c( "Area", "X", "Y", "Slice")
    colNamesTest <- sum( names(input)[-1] == colNames)
    if( colNamesTest == length(colNames)) {
      if(filterArea) {
        theAreaFilter <- 
          areaFilter(input, isExtremeOutliers = TRUE)
        temp <- input$X * theAreaFilter
        temp[temp==0] <- NA
        Force <- filter(data = temp, range)
      }
      else {
        temp <- input$X
        Force <- filter(data = temp, range)
      }
      Force[Force < 20] <- NA
      Time <- input$Slice/frameRate + timeRecorded/60
      out <- cbind(input, Time, Force)
      out
    } else{return}
  } else{return}
}