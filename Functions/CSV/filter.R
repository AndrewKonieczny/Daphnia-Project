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

filter <- function(file_name,
                   file_path,
                   ID,
                   sep = ",",
                   rate = 30,
                   range = 3)
{
  file_separators <- c(",","\t")
  data <- read.delim(file = paste0(file_path, file_name), 
                     sep = sep)
  if(length(data[1])==1){
    file_separators[-grep(sep, file_separators)]
    data <- read.delim(file = paste0(file_path, file_name), 
                       sep = file_separators[-grep(sep, file_separators)])
  }
  names(data)[grep("X\\.", names(data))] <- grep(ID, unlist(strsplit(file_name, split = "_")), value = TRUE)
  time_stamp <- as.numeric(sub(pattern = "Time",
                               replacement = "" ,
                               grep(pattern = "Time",
                                    x = unlist(strsplit(file_name, split = "_")),
                                    value = TRUE)))
  Slice <- as.vector(as.matrix(data["Slice"]))
  X <- as.vector(as.matrix(data["X"]))
  Y <- as.vector(as.matrix(data["Y"]))
  time <- (Slice-1)/rate + time_stamp*60
  R <- sqrt(X^2+Y^2)
  detrend <- median(X) - X
  eval_matrix <- detrend  
  local_max <- numeric(length = 0)
  if(range < 2) {range = 2}
  if(range > length(detrend)/2) {range = length(detrend)/2}
  # This loop creates a matrix with range number of columns. Each column 
  # removes the first input element to replace with NA, and continues depending
  # on the value of i. If i is greater than 1 then i NA's are added to the end 
  # while removing i elements from the begining of the input.
  
  for(i in 1:range){
    new_row <- c(tail(detrend, length(detrend)-i),rep(NA,i))
    eval_matrix <- rbind(eval_matrix, new_row)
  }
  
  # This for loop goes through the matrix created above,
  # cleaningRange, and goes down the i'th column to decide 
  # if the first row (the actual input data) is the maximum
  # value over the next range number of elements. 
  
  for(i in seq(1, length(detrend))){
    if(all(is.na(eval_matrix[,i]))){
      local_max[i] <- NA
    }
    if(max(eval_matrix[,i], na.rm = TRUE) == eval_matrix[1,i]){
      local_max[i] <- eval_matrix[1,i]
    } 
    if(i > length(detrend)-range){
      local_max[i] <- NA
    }
  }
  
  # The threshold I used to remove the values that are not true max force events
  # This is done using the kmeans cluster analysis algorithm. I initiated the
  # algorithm with two clusters with initial means being the min and max of the
  # whole data set. The algorithm separates the initially filtered data into two
  # clusters, of which I select the cluster with the largest mean. This finally
  # results in the data we call the max force.
  
  temp <- local_max[!is.na(local_max)]
  k <- kmeans( x = temp, centers = c(min(temp), max(temp)))
  threshold <- mean(k$centers)
  for(n in seq(1, length(local_max))){
    if(!is.na(local_max[n])){
      if(local_max[n] < threshold){
        local_max[n] <- NA
      }
    }
  }
  return(cbind(data[grep(ID,names(data))],
               time,
               data[grep("Slice",names(data))],
               data[-grep("Slice",names(data))][-grep(ID,names(data))],
               R,
               detrend,
               local_max))
}
