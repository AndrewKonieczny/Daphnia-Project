# This function concatenates the different columns of X's
# The inputs:
# Input = the raw .csv file, Intervals = the number of columns for each
# filming period (if there are two columns until the next filming period,
# e.g. area and x, then the Intervals value is 2).
# The output: 
# An array of normalized distance values (units are in pixels) that are
# concatinated together.
concat <- function(Inputs,Intervals){
  filter <- function(Input, Element, Interval){
    zero <- function(zeroInput, zeroColumn){
      # 640 is the width of the image. This subtraction is done to
      # correct the x-y coordinate system.
      numbers <- 640-data.matrix(zeroInput[zeroColumn])
      central <- mean(numbers,na.rm=TRUE, trim = 0.2)
      # 'central' is the 30% trimmed mean to give a better value for the
      # central position of the fiber
      out <- as.vector((numbers-central), mode = "numeric")
      return(out)
    } # end of zero() function
    area_dev <- function(areaInput, areaColumn){
      g <- data.matrix(areaInput[areaColumn])
      z_area <- abs(mean(g,na.rm = TRUE, trim = 0.3)-g)/sd(g,na.rm=TRUE)
      # 'z_area' uses the trimmed mean to find the absolute value of the
      # z-score for the fiber's area values, the z-score is then used to
      # remove any valuses that deviate too far from the area's trimmed
      # mean.
      out <- as.vector(ifelse(z_area<=2,1,NA), mode = "numeric")
      return(out)
    } # end of area_dev() function
    col_zero <- (Element)*Interval
    # The column to be entered into the zero() function
    col_area <- (Element)*Interval-1
    # The column to be intered into the area_dev() function
    out <- as.vector(zero(Input, col_zero)*area_dev(Input, col_area), mode = "numeric")
    return(out)
  } # end of filter() function
  final <- NULL
  Max <- dim(Inputs)[2]/2
  elementSequence <- seq(1,Max,1)
  for (i in elementSequence){ 
    final <- c(final,filter(Inputs,elementSequence[i],Intervals))
  } # end of for-loop
  return(final)
} # end of concat() function
------------------------------------------------------------------------------------
  # This function finds the max force values by removing values around a
  # local maximum value. The input is the data to be filtered (preferably
  # after being concatinated) with a bound that is simply the lower limit
  # of which you want no values below it. The output is an array of the 
  # filtered data.
ForceFilter <- function(input,bound){
  data <- ifelse(input>=bound,input,0)
   x <- ifelse(is.na(data),0,data)
   y <- c(0,x[1:length(x)-1])
   z <- c(x[2:length(x)],0)
   one <- ifelse(x>y,1,NA)
   two <- ifelse(x>z,1,NA)
   three <- one*two*x
   four <- ifelse(three==0,NA,three)
   out <- four[!is.na(four)]
   return(out)
  }
lowerbounds <- function(input,MAX){
  v <- seq(0,MAX,1)
  out <- NULL
  for(i in v){
    out <- c(out,mean(ForceFilter(concat(input,2),i),na.rm=TRUE))
  }
  return(out)
}
dx <- function(x){
  y <- c(0,x[1:length(x)-1])
  z <- c(0,x[2:length(x)])
  out <- z-y
  return(out)
}
ReportedData <- function(data){
  BestBound <- sum(ifelse(abs(dx(dx(lowerbounds(data,50))))>1,1,0))+1
  ForceData <- ForceFilter(concat(data,2),BestBound)*0.0518
  return(ForceData)
}
-------------------------------------------------------------------
Final <- function(InputData){
  concat <- function(Inputs,Intervals){
    filter <- function(Input, Element, Interval){
      zero <- function(zeroInput, zeroColumn){
        # 640 is the width of the image. This subtraction is done to
        # correct the x-y coordinate system.
        numbers <- 640-data.matrix(zeroInput[zeroColumn])
        central <- mean(numbers,na.rm=TRUE, trim = 0.2)
        # 'central' is the 30% trimmed mean to give a better value for the
        # central position of the fiber
        out <- as.vector((numbers-central), mode = "numeric")
        return(out)
      } # end of zero() function
      area_dev <- function(areaInput, areaColumn){
        g <- data.matrix(areaInput[areaColumn])
        z_area <- abs(mean(g,na.rm = TRUE, trim = 0.3)-g)/sd(g,na.rm=TRUE)
        # 'z_area' uses the trimmed mean to find the absolute value of the
        # z-score for the fiber's area values, the z-score is then used to
        # remove any valuses that deviate too far from the area's trimmed
        # mean.
        out <- as.vector(ifelse(z_area<=2,1,NA), mode = "numeric")
        return(out)
      } # end of area_dev() function
      col_zero <- (Element)*Interval
      # The column to be entered into the zero() function
      col_area <- (Element)*Interval-1
      # The column to be intered into the area_dev() function
      out <- as.vector(zero(Input, col_zero)*area_dev(Input, col_area), mode = "numeric")
      return(out)
    } # end of filter() function
    final <- NULL
    Max <- dim(Inputs)[2]/2
    elementSequence <- seq(1,Max,1)
    for (i in elementSequence){ 
      final <- c(final,filter(Inputs,elementSequence[i],Intervals))
    } # end of for-loop
    return(final)
  } # end of concat() function
  ForceFilter <- function(input,bound){
    data <- ifelse(input>=bound,input,0)
    x <- ifelse(is.na(data),0,data)
    y <- c(0,x[1:length(x)-1])
    z <- c(x[2:length(x)],0)
    one <- ifelse(x>y,1,NA)
    two <- ifelse(x>z,1,NA)
    three <- one*two*x
    four <- ifelse(three==0,NA,three)
    out <- four[!is.na(four)]
    return(out)
  }
  lowerbounds <- function(input,MAX){
    v <- seq(0,MAX,1)
    out <- NULL
    for(i in v){
      out <- c(out,mean(ForceFilter(concat(input,2),i),na.rm=TRUE))
    }
    return(out)
  }
  dx <- function(x){
    y <- c(0,x[1:length(x)-1])
    z <- c(0,x[2:length(x)])
    out <- z-y
    return(out)
  }
  ReportedData <- function(data){
    BestBound <- sum(ifelse(abs(dx(dx(lowerbounds(data,50))))>1,1,0))+1
    ForceData <- ForceFilter(concat(data,2),BestBound)*0.0518
    return(ForceData)
  }
  out <- ReportedData(InputData)
  return(out)
}
