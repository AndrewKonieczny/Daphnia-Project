ForceFilter <- function(input,interval,lower_bound){
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
        numbers <- data.matrix(zeroInput[zeroColumn])
        central <- mean(f,na.rm=TRUE, trim = 0.3)
        # 'central' is the 30% trimmed mean to give a better value for the
        # central position of the fiber
        out <- as.vector((central-numbers), mode = "numeric")
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
  
  first <- function(input,bound){
    data <- ifelse(input>=bound,input,0)
    x <- ifelse(is.na(data),0,data)
    y <- c(0,x[1:length(x)-1])
    z <- c(x[2:length(x)],0)
    one <- ifelse(x>y,1,NA)
    two <- ifelse(x>z,1,NA)
    three <- one*two*x
    out <- ifelse(three==0,NA,three)
    return(out)
  }
  final <- first(concat(input,interval),lower_bound)
  return(final)
}

