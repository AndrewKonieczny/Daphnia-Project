# This function concatinates all the 'rowBound' animal recording periods for a 
# given drug concentration. The data is formatted like so:
#
#   Animal_1  var_1a  var_1b  ... Animal_2  var_2a  ... etc.
#
# Where each thing listed above is a column vector and each row is an 
# observation at the given time (specified by the time or "Slice" which is  
# the ID for the spacific frame).
# 
# For this function to read the .csv files correctly, they need to be named 
# something along the lines of:  Control01, Control02, Control03, ...
# Where "Control" is the animal_ID ( see below ) and the animal number has a 
# zero place holder ( if you have more than 99 animals or start counting your 
# animals at zero this function might not work ).

# I also padded each data set with NA values so that the data.frame has the 
# same demensions between animals. So when you come accross a long string of
# NA rows, that means the recording data for that animal is done.
# 
# Inputs:
#   data_path:  A character string (i.e. a string of characters in double 
#                    quotes like: "Hello World") that specifies where the .csv 
#                    data is so the function can grab it.
#             
#   animal_ID:  A character string that identifies the animal group (i.e.
#              "GroupA", or "Control").
#             
#   separator:  A character string that indicates what the separates the  
#               columns in your data file. Typically, the .csv uses the "," 
#               which is the default value for the variable. So if your data
#               is separated by "," you can not include that variable when you 
#               call the function.
#             
#   return_file:  A boolean value (TRUE or FALSE in capital letters) that 
#               indicates if you want the funtion to output a .csv file or use
#               this function to output a data.frame object. Default for this
#               variable is TRUE.
#             
#   csv_filename:  A character string which, if return_file = TRUE, will be the 
#             name of the output .csv file. This file will contain all the data
#             from your individual animal .csv files. The default setting for 
#             this variable is NULL, which will result in the file being named
#             after the folder it is in with the "All_" phrase in front (i.e. 
#             "All_folderName"). So if you don't like the default name, change
#             it!
#             
# Outputs:
#   If return_file is FALSE, the output will be a data.frame object, if 
#   return_file is TRUE then the output will be a .csv file. If a .csv file is
#   produced, the file will be dropped where your working directory is pointed
#   at the time of running (you can see what your WD is by typing getwd() into
#   the console and you can set it by setwd()). 
# 
# Below is an example of what I did to run this function where *** denotes 
# notes:
#
# *** ids is the animal_ID, eachanimal in this folder was named like: 
# *** "Drug01", "Drug02", ...
# ids = "Drug"
#    
# *** dose is the name of the folder that held the animals with the same dose.
# dose = "1E2"
#
# *** whereData is the directory listing, using paste0() to save space. You 
# *** can also see I used the dose variable in the directory for convenience.
# whereData = 
#   paste0("~/Documents/MyFolders/DaphniaLab/Raw Data/A68930/Dose_uM_CSV/", 
#          dose, "/AllAnimals")       
#
# *** newfile is the file name for the output .csv
# newfile <-paste0("All_",ids,"_",dose,".csv")
#
# *** this sets the working directory to the folder whereData is pointing to
# setwd(file.path(whereData))
#
# *** Here is where the function is implemented using the above parameters
# columnBind(data_path = whereData, 
#            animal_ID = ids, separator = ",",
#            csv_filename = newfile) 
# *** You might also notice I'm taking advantage of the default settings for
# *** the return_file and csv_filename variables.

column_Bind <- function(data,
                        animal_ID,
                        separator = ",",
                        return_as_file = TRUE,
                        output_name = NULL)
{
  # padding is a short recursively defined function used to make all 
  # data frames have the same number of rows.
  padding <- function( input_data_frame, row_count){
    if(nrow( input_data_frame) < row_count){
      pad <- rep( NA, ncol( input_data_frame))
      input_data_frame <- rbind( input_data_frame,pad)
      padding( input_data_frame, row_count)
    } else{ return( input_data_frame)}}
  message("initiating processing...")
  if( inherits( data, "character")){
    message("\tclass(data) = \"", class(data),"\"")
    if( dir.exists( data)){
      message("\tdirectory exists: ", dir.exists( data))
      filenames <- list.files(path = data,
                              pattern = paste0( animal_ID, "\\d{2}.csv"))
      unpadded_data <- lapply(X = file.path( data, filenames),  
                              FUN = read.delim, 
                              sep = separator)
      message("\tpadding ", length(unpadded_data)," data.frame(s)...")
      max_row_count <- max( unlist( lapply( unpadded_data, nrow)))
      padded_data <- lapply( unpadded_data, padding, max_row_count)
      message("\t...padding complete \n\tconcatinating data.frame(s)...")
      column_bound_data <- do.call("cbind", padded_data)
      message("\t...concatination complete")
    } else{
      stop("the provided directory does not exist:\n", file.path(data))
    }
  }
  if( inherits( data, "environment")){
    message("\tclass(data) = \"", class(data),"\"")
    element_list <- ls( data)
    unpadded_data <- lapply(element_list, get, envir = data)
    message("\tpadding ", length(unpadded_data), "data.frame(s)...")
    max_row_count <- max( unlist( lapply( unpadded_data, nrow)))
    padded_data <- lapply( unpadded_data, padding, max_row_count)
    message("\t...padding complete \n\tconcatinating data.frames...")
    column_bound_data <- do.call("cbind", padded_data)
    message("\tconcatination complete")
  }
  message( "...processing complete\noutput:")
  if( return_as_file){
    if( is.null(output_name) ) {
      output_name <- paste0("All_", animal_ID,".csv")
    } else{
      if( !grepl( pattern = ".csv", x = output_name)){
        output_name <- paste0( output_name, ".csv")
      }
    }
    write.csv( x = column_bound_data, 
               file = file.path( data, output_name))
    message( "\tfilename:\t",
             output_name,
             "\n\tdirectory:\t",
             normalizePath( file.path( data)),
             "\n\tfile path:\t",
             normalizePath( file.path( data, output_name)),
             "\n\tfile path exists:\t",
             file.exists( file.path( data, output_name)))
  } else{
    message( "Returned data.frame")
    return( column_bound_data)
  }
}
