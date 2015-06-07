# To use this function, you need to import a .csv file of the raw
# ImageJ data. To prepare the .csv file, you need to take the
# individual animal observation periods and collect them into a 
# single .csv file. Then, depending on how you concatinate the 
# collected data you take note of what the interval is between 
# individual animal observations is and put that value into the 
# DataInterval position of the function. For example, if you have 
# two observation periods and two animals in the same drug 
# concentration, you may have recorded, using ImageJ, the ID, Area, 
# X, Y, and Slice. If you recorded this information, in this order, 
# you have an interval of 5 since you have 5 categories of data. 
# Only the Area and X data are examined here. The ID has no meaning 
# here and the Y displacements are minimal and add very little to 
# the force measurements  being made. Slice only has importance when 
# the time is needed for a time series analysis, which this script 
# does not address. The output is an array of force measurements 
# that can be compared at various drug concentrations. This script 
# also removes the feeding oscilations (which is only important here
# because it skewes the mean force measuments).
SwimReport <- function(InputData,DataInterval,CalibrationData_DispVariable)
{
  # A simple difference function taking in an array and outputs an
  # array of differences (final - initial)
  dx <- function(x)
  {
    y   <- c(0,x[1:length(x)-1])
    z   <- c(0,x[2:length(x)])
    out <- z-y
    print("dx function used")
    return(out)
  }
  zero <- function(zeroInput, zeroColumn)
  {
    # 640 is the width of the image. This conversion is done to
    # correct the x-y coordinate system.
    numbers <- 640 - data.matrix(zeroInput[zeroColumn])
    # a 20% trimmed mean results in a better central fiber position
    # while removing values that deviate from the last more than 20
    # pixels will not be included.
    central <- mean(numbers[abs(dx(numbers)) < 10], na.rm = TRUE,
                    trim = 0.2)
    out <- as.vector(numbers - central, mode = "numeric")
    print("zero function used")
    return(out)
  }
  area_dev <- function(areaInput, areaColumn)
  {
    g <- data.matrix(areaInput[areaColumn])
    z_area <- abs(g - mean(g, na.rm = TRUE)) / sd(g, na.rm=TRUE)
    # 'z_area' uses the trimmed mean to find the absolute value of 
    # the z-score for the fiber's area values, the z-score is then 
    # used to remove any valuses that deviate too far from the 
    # area's trimmed mean.
    out <- as.vector(ifelse(z_area <= 2, 1, NA), mode = "numeric")
    print("area_dev function used")
    return(out)
  }
  filter <- function(Input, Element, Interval)
  {
    col_zero <- (Element) * Interval
    # The column to be entered into the zero() function
    col_area <- (Element) * Interval - 1
    # The column to be intered into the area_dev() function
    out <- as.vector(zero(Input, col_zero) *
                       area_dev(Input, col_area), mode = "numeric")
    print("filter function used")
    return(out)
  }
  # This function concatenates the different columns of X's
  # The inputs:
  #   Input = the raw .csv file,
  #   Intervals = the number of columns for each filming period (if
  #               there are two columns until the next filming period,
  #               e.g. area and x, then the Intervals value is 2).
  # The output: 
  #   An array of normalized distance values (units are in pixels) 
  #   that are concatinated together.
  concat <- function(Inputs,Intervals)
  {
    final   <- NULL
    Max     <- dim(Inputs)[2]/2
    elementSequence <- seq(1,Max,1)
    for (i in elementSequence)
    {
      final <- c(final, 
                 filter(Inputs, elementSequence[i], Intervals))
      print("concat function used: ")
      print(i)
    }
    return(final)
  }
  # This function finds the max force values by removing values around 
  # a local maximum value. The input is the data to be filtered 
  # (preferably after being concatinated) with a bound that is simply 
  # the lower limit of which you want no values below it. The output 
  # is an array of the filtered data.
  ForceFilter <- function(input,bound)
  {
    for(i in 1:length(input))
    {
      x <- ifelse(input[i] >= bound, input, NA)
    }
    y <- c(NA, x[1:length(x)-1])
    z <- c(x[2:length(x)], NA)
    out <- numeric(length(x))
    for (i in 1:length(x))
    {
      if (!is.na(x[i]))
      {
        if (is.na(y[i]))
        {
          if (is.na(z[i]) || x[i] > z[i])
          {
            out[i] <- x[i]
          }
        }
        else
        {
          if (x[i] > y[i])
          {
            if (is.na(z[i]) || x[i] > z[i])
            {
              out[i] <- x[i]
            }
          }
        }
      }
      else
      {
        out[i] <- NA
      }
    }
    print("forcefilter used")
    return(out)
  }
  # Takes in input data, and goes therough a series of possible lower
  # bounds to create an array of means. The rate of change of the 
  # array of means is then taken. The mean of this array is found and 
  # that mean is then compared to the rest of the array to find the 
  # number of points in the array are less than the mean. This is the 
  # best lower bound for the data set.
  # Input: data and a number of lower bounds you want to test
  # Output: the best lower bound
  lowerbounds <- function(input,MAX)
  {
    v     <- seq(0,MAX,1)
    out   <- NULL
    for(i in v)
    {
      data <- ForceFilter(concat(input,2),i)
      newMean <- mean(data, na.rm=TRUE)
      out <- c(out, newMean)
    }
    best <- sum(dx(out) < mean(dx(out)))
    print("lowerbound used")
    return(best)
  }
  
  maximumForceReportable <- max(CalibrationData_DispVariable)
  
  MaxReport <- function(inData,inMax)
  {
    x <- inData
    x[x>inMax] <- NA
    x[x==0] <- NA
    print("maxreport used")
    return(x)
  }
  
  ForceData <- ForceFilter(concat(InputData, DataInterval), 
                           lowerbounds(InputData,100))
  outData <- MaxReport(ForceData,maximumForceReportable)
  return(ForceData)
}
