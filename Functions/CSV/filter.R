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
#  R_norm: 
#     A variable based on X, calculated by median(X) - X. This calculation is
#     based on the assumption that ImageJ put the origin in the top left corner
#     of the image which is the default. 

#I may change this a bit to figure out if your origin is in the top left corner 
#or not, to be continued. */

#  local_max:
#     The final variable that will be converted to a max force reading, based on
#     the R_norm variable, finding the local maxima within a given range 
#     (default 3 observations).

filter <- function(file_name,
                   file_directory,
                   ID,
                   rate = 30,
                   range = 3)
{
  # grabbing data
  if( substr(file_directory, 
             nchar(file_directory), nchar(file_directory))!="/"){
    file_directory <- paste0(file_directory, "/")
  }
  
  FILE_SEPARATORS <- c(",","\t")
  for(delimiter in FILE_SEPARATORS){
    if( file.exists(paste0( file_directory, file_name))){
      data <- read.delim(file = paste0( file_directory, file_name), 
                         sep = delimiter)
      if( ncol( data) > 1){ 
        break
      } 
    } else{
      warning( "file path does not exist\n", paste0( file_directory, file_name))
    }
  }
  
  # checking functional inputs
  MIN_RANGE <- 2
  MAX_RANGE <- length( nrow(data))/2
  if( !is.numeric(range) && !is.numeric(as.numeric(range))){
    warning("non-numeric range input, range = ", range, 
            "assigning range = 3 (default)")
    range <- 3
  } else{
    if( !is.numeric(range) && is.numeric(as.numeric(range))){
      if( range < MIN_RANGE){ range <- MIN_RANGE}
      if( range > MAX_RANGE){ range <- MAX_RANGE}
      if( range > MIN_RANGE && range < MAX_RANGE){ range <- as.numeric(range)}
    }
  }
  
  # setting variables
  split_file_name <- unlist( strsplit( x = file_name, 
                                       split = "_"))
  animal <- grep( pattern = ID, 
                  x = split_file_name, 
                  value = TRUE)
  names(data)[grep( pattern = "X\\.", 
                    x = names(data),
                    value = FALSE)] <- animal
  time_stamp <- as.numeric( sub( pattern = "Time",
                                 replacement = "" ,
                                 x = grep( pattern = "Time",
                                           x = split_file_name,
                                           value = TRUE)))
  tryCatch({
    assign("Slice", 
           value = as.vector( 
             as.matrix( data[grep("Slice", names(data))] )));
    assign("time", value = (Slice - 1)/rate + time_stamp*60)},
    error = function(cond){
      print("no variable named \"Slice\" can be found")
    }
  )
  X <- as.vector( as.matrix( data[grep("X",names(data))]))
  Y <- as.vector( as.matrix( data[grep("Y",names(data))]))
  if(any(grepl("R",names(data)))){
    R <- as.vector( as.matrix( data[grep("R",names(data))]))
  } else{
    R <- sqrt( X^2 + Y^2)
  }
  eval_matrix <- R_norm <- median( R) - R
  local_max <- numeric( length = 0)
  
  # This loop creates a matrix with range number of columns. Each column 
  # removes the first input element to replace with NA, and continues depending
  # on the value of i. If i is greater than 1 then i NA's are added to the end 
  # while removing i elements from the begining of the input.
  for( i in 1:range){
    new_row <- c( tail( x = R_norm, 
                        n = ( length( R_norm) - i)),
                  rep( NA, i))
    eval_matrix <- rbind( eval_matrix, 
                          new_row)
  }
  
  # This for loop goes through the matrix created above,
  # cleaningRange, and goes down the i'th column to decide 
  # if the first row (the actual input data) is the maximum
  # value over the next range number of elements. 
  for( i in seq( 1, length( R_norm))){
    if( all( is.na( eval_matrix[,i]))){
      local_max[i] <- NA
    }
    if( max( eval_matrix[,i], 
             na.rm = TRUE) == eval_matrix[1,i]){
      local_max[i] <- eval_matrix[1,i]
    } 
    if(i > ( length( R_norm) - range)){
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
  
  try(
    assign("data", value = cbind(data[grep(animal, names(data))],
                                 data[grep("Slice", names(data))], 
                                 time, 
                                 data[-grep("Slice", names(data))][-grep(animal, names(data))]))
  )
  output <- cbind( data,
                   R,
                   R_norm,
                   local_max)
  return( output)
}
