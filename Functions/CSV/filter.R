# This function goes through a vector and finds the local maxima for the 
# specified range of elements ( default being 3 elements ). By this I mean  
# given a list c(1,1,NA,3,1,2,NA) with range = 3, this function will go through
# this list and make a matrix :  NA  1 NA 3  1  2 NA
#                                NA NA  3 1  2 NA NA
#                                NA  3  1 2 NA NA NA
# So for this example the would find output c(NA,NA,3,NA,2,NA) which are the 
# local maxima within 3 spaces (the range).
# 
# Inputs:
#   data: A numeric vector, the X values, to be filtered.
#   range: A numeric value (needs to be a whole number) that sets the range for 
#          deciding what the local max value is. The default value is 3, meaning 
#          the function will find the maximum value between 3 elements.
#   
# Outputs:
#   A vector of numeric values based on the input. The new vector will have the 
#   local maximum values only and NA values where a max value was not recorded. 
#   The new vector will retain the time that the local max was recorded because 
#   the vector is "padded" with NAs.
  
filter <- function(data, 
                   range = 3)
{
  input <- median(data)-data
  cleaningRange <- NULL  
  localMax <- numeric(1)
  out <- rep(NA,length(data))
  if(range < 1) {range = 3}
  if(range > 20) {range = 3}
  # This loop creates a matrix with range number of columns. Each column 
  # removes the first input element to replace with NA, and continues depending
  # on the value of i. If i is greater than 1 then i NA's are added to the end 
  # while removing i elements from the begining of the input.
  
  for ( i in 1:range ) {
    if(i==1){newCol <- c(NA, input[-1])
             cleaningRange <- cbind(cleaningRange, newCol)
    }
    else{
      newCol <- c(NA, 
                  tail(input[-1], n = (length(input)-i)), 
                  rep(NA, i-1))
      
      cleaningRange <- cbind(cleaningRange, newCol)
    }
  }
  
  # This for loop goes through the matrix created above,
  # cleaningRange, and goes down the i'th column to decide 
  # if the first row (the actual input data) is the maximum
  # value over the next range number of elements. 
  
  for ( i in 2:length(input) ) {
    if( sum(is.na(cleaningRange[i-1,])) < range ) {
      localMax <- max(cleaningRange[i-1,], na.rm = T)
    }
    if ( sum(is.na(c(cleaningRange[i,], localMax))) < range ) {
      test <- max(cleaningRange[i,], localMax, na.rm = T)
      if ( is.infinite(test) ) {out[i] <- NA}
      if ( test == cleaningRange[i,1]   && 
             !is.infinite(test) && 
             !is.na(cleaningRange[i,1]) ) { out[i] <- cleaningRange[i,1] }
    }
  }
  # The threshold I used to remove the values that are not true max force events
  # This is done using the kmeans cluster analysis algorithm. I initiated the
  # algorithm with two clusters with initial means being the min and max of the
  # whole data set. The algorithm separates the initially filtered data into two
  # clusters, of which I select the cluster with the largest mean. This finally
  # results in the data we call the max force.
  temp <- out[ !is.na(out) ]
  theThreshold <- ( kmeans( x = temp, 
                                centers = c( min(temp), max(temp) )
                                )$cluster
                        )==2
  out <- ifelse( (out %in% temp[theThreshold]), out, NA)
  out
}
