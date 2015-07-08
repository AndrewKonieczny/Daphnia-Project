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
#  data:  A data frame that has at least the following variables: 
#         Slice, X, Y
#         These variables are aquired from the processing done by ImageJ.
#  range: A numeric value (needs to be a whole number) that sets the range for 
#         deciding what the local max value is. The default value is 3, meaning 
#         the function will find the maximum value between 3 elements.
# A note on the data input variable:
#    ImageJ by default has the origin of the image being in the top left corner.
#    This function requires that this is maintained presently. To normalize the
#    data, the X variable needed to to be subtracted from the "normal" line.
#   
# Outputs:
#  A data frame with all of the same input variables plus additional variables:
#  A vector of numeric values based on the input. The new vector will have the 
#  local maximum values only and NA values where a max value was not recorded. 
#  The new vector will retain the time that the local max was recorded because 
#  the vector is "padded" with NAs.
#  ID:  
#     The value of the input ID is the name of the variable, but the numeric
#     data that follows is integers from 1 to N (the number of observations in
#     the data set).
#  Time:
#     The time corresponding to the observation, based on the Slice variable.
#     The time also includes the time you filmed the animal via the naming
#     format “Drug01_Time00_Fiber.csv” where the number from “…Time00…” is
#     extracted and interpreted as the number of minutes after starting the
#     experiment.
#  Slice:
#     A variable given to you by the ImageJ processing of the series of images.
#  Area:   
#     A variable given to you by the ImageJ processing of the series of images.
#  X:
#     A variable given to you by the ImageJ processing of the series of images.
#  Y:
#     A variable given to you by the ImageJ processing of the series of images.
#  R:
#     The radial distance from the origin, calculated by sqrt(X^2 + Y^2).
#  detrend: 
#     A variable based on X, calculated by median(X) - X. This calculation is
#     based on the assumption that ImageJ put the origin in the top left corner
#     of the image which is the default. 

#I may change this a bit to figure out if your origin is in the top left corner 
#or not, to be continued. */

#  local_max:
#     The final variable that will be converted to a max force reading, based on
#     the detrend variable, finding the local maxima within a given range 
#     (default 3 observations).

filter <- function(file_name,
                   file_path,
                   ID,
                   sep = ",",
                   rate = 30,
                   range = 3)
{
  file_separators <- c(",","\t")
  data <- read.delim(file = paste0( file_path, file_name), 
                     sep = sep)
  if(length(data[1])==1){
    file_separators[-grep( pattern = sep, 
                           x = file_separators,
                           value = FALSE)]
    data <- read.delim(file = paste0( file_path, file_name), 
                       sep = file_separators[-grep( pattern = sep, 
                                                    x = file_separators,
                                                    value = FALSE)])
  }
  .names <- unlist( strsplit( x = file_name, 
                              split = "_"))
  names(data)[grep( pattern = "X\\.", 
                    x = names(data),
                    value = FALSE)] <- grep( pattern = ID, 
                                             x = .names, 
                                             value = TRUE)
  time_stamp <- as.numeric( sub( pattern = "Time",
                                 replacement = "" ,
                                 x = grep( pattern = "Time",
                                           x = .names,
                                           value = TRUE)))
  Slice <- as.vector( as.matrix( data["Slice"]))
  X <- as.vector( as.matrix( data["X"]))
  Y <- as.vector( as.matrix( data["Y"]))
  Time <- ( Slice - 1)/rate + time_stamp*60
  R <- sqrt( X^2 + Y^2)
  eval_matrix <- detrend <- median( X) - X
  local_max <- numeric( length = 0)
  if( range < 2){
    range <- 2
  }
  if( range > length( detrend) / 2){
    range <- length( detrend) / 2
  }
  # This loop creates a matrix with range number of columns. Each column 
  # removes the first input element to replace with NA, and continues depending
  # on the value of i. If i is greater than 1 then i NA's are added to the end 
  # while removing i elements from the begining of the input.
  
  for( i in 1:range){
    new_row <- c( tail( x = detrend, 
                        n = ( length( detrend) - i)),
                  rep( NA, i))
    eval_matrix <- rbind( eval_matrix, 
                          new_row)
  }
  
  # This for loop goes through the matrix created above,
  # cleaningRange, and goes down the i'th column to decide 
  # if the first row (the actual input data) is the maximum
  # value over the next range number of elements. 
  
  for( i in seq( 1, length( detrend))){
    if( all( is.na( eval_matrix[,i]))){
      local_max[i] <- NA
    }
    if( max( eval_matrix[,i], 
             na.rm = TRUE) == eval_matrix[1,i]){
      local_max[i] <- eval_matrix[1,i]
    } 
    if(i > ( length( detrend) - range)){
      local_max[i] <- NA
    }
  }
  
  # The threshold I used to remove the values that are not true max force events
  # This is done using the kmeans cluster analysis algorithm. I initiated the
  # algorithm with two clusters with initial means being the min and max of the
  # whole data set. The algorithm separates the initially filtered data into two
  # clusters, of which I select the cluster with the largest mean. This finally
  # results in the data we call the max force.
  
  k <- kmeans( x = local_max[!is.na( local_max)], 
               centers = c( min( local_max[!is.na( local_max)]), 
                            max( local_max[!is.na( local_max)])))
  threshold <- mean( k$centers)
  for( n in seq( 1, length( local_max))){
    if( !is.na( local_max[n])){
      if( local_max[n] < threshold){
        local_max[n] <- NA
      }
    }
  }
  return( cbind( data[grep( pattern = ID,
                            x = names( data),
                            value = FALSE)],
                 Time,
                 data[grep( pattern = "Slice",
                            x = names( data),
                            value = FALSE)],
                 data[-grep( pattern = "Slice",
                             x = names( data),
                             value = FALSE)][-grep( pattern = ID,
                                                    x = names( data),
                                                    value = FALSE)],
                 R,
                 detrend,
                 local_max))
}
