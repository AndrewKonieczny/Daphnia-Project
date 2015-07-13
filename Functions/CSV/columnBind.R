# This function concatinates the data from animals exposed to the same dose of drug into a single data.frame or .csv file. This function is for convienence purposes, there are no real calculations or analyses being preformed in this function. The method of concatination is by column, meaning the data sets of each animal will be side by side reading the table from left to right.
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
#####
# Below is an example of what I did to run this function where *** denotes 
# notes:
#
# column_bind( data = paste0("~/Documents/MyFolders/DaphniaLab/",
#                            "Raw Data/A68930/Dose_uM_CSV/", 
#                            "1E2/AllAnimals"),
#              animal_ID = "Drug",
#              separator = ",",
#              return_as_file = TRUE,
#              output_name = paste0("All_Drug_1E2.csv"))
#####
column_bind <- function( data,
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
      file_paths <- setNames(file.path( data, filenames), filenames)
      unpadded_data <- lapply(X = file_paths,  
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
    unpadded_data <- lapply( element_list, get, envir = data)
    print(lapply(lapply( unpadded_data,names),
          grep,pattern = animal_ID, value = T))
    message("\tpadding ", length(unpadded_data), "data.frame(s)...")
    max_row_count <- max( unlist( lapply( unpadded_data, nrow)))
    padded_data <- lapply( unpadded_data, padding, max_row_count)
    message("\t...padding complete \n\tconcatinating data.frames...")
    column_bound_data <- do.call("cbind", padded_data)
    message("\tconcatination complete")
  }
  message( "...processing complete\noutput:")
  if( is.null(output_name) ) {
    output_name <- paste0("All_", animal_ID,".csv")
  } else{
    if( !grepl( pattern = ".csv", x = output_name)){
      output_name <- paste0( output_name, ".csv")
    }
  }
  if( return_as_file){
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
    .name <- sub( pattern = ".csv", 
                  replacement = "", 
                  x = output_name)
    assign(x = .name, 
           value = column_bound_data,
           envir = globalenv(),
           inherits = TRUE)
    message( "Returned data.frame named: ", .name)
  }
}

####################
# scrap work TBC tomorrow...
c_bind <- function(path_to_data,
                   path_to_functions, 
                   animal_ID,
                   name_of_env,
                   frame_rate = 30,
                   separator = ",",
                   return_an_object = FALSE){
  padding <- function( input_data_frame, row_count){
    if(nrow( input_data_frame) < row_count){
      pad <- rep( NA, ncol( input_data_frame))
      input_data_frame <- rbind( input_data_frame,pad)
      padding( input_data_frame, row_count)
    } else{ return( input_data_frame)}
  }
  source(paste0(path_to_functions, "filter.R"))
  files <- list.files(path = path_to_data,
                      pattern = "Drug\\d{2}_Time\\d{2}_Fiber.csv",
                      full.names = FALSE,
                      ignore.case = TRUE)
  if(length( files) == 0){
    stop("no files found in the given directory")
  }
  animal_names <- grep(pattern = animal_ID,
                       x = unique(unlist(lapply(X = files, 
                                                FUN = strsplit, 
                                                split = "_"))),
                       value = TRUE)
  if(length( animal_names) == 0){
    stop("there is a problem with the naming format of the .csv files ",
         "or the value of the input 'animal_ID'")
  }
  message("There are ", length(files), " file(s) and ", length(animal_names),
          " animals in the following directory:\n\t", path_to_data)
  
  output_list <- list()
  for(animal in animal_names){
    eval_animals <- grep(pattern = animal, 
                         x = files, 
                         value = TRUE)
    message("Converting ", length(eval_animals), " file(s) for ", 
            animal, " to data frame")
    data_adding_force <- lapply(X = eval_animals, 
                                FUN = filter, 
                                file_path = path_to_data,
                                ID = animal_ID,
                                sep = separator,
                                rate = frame_rate,
                                range = 3)
    finalData <- do.call(what = "rbind", 
                         args = data_adding_force)
    tag <- grep(pattern = animal_ID, x = names(finalData), value = TRUE)
    finalData <- finalData[-grep(pattern = animal_ID, x = names(finalData), value = FALSE)]
    names(finalData) <- paste0(tag,"_",names(finalData))
    print(names(finalData))
    output_list[[animal]] <- as.data.frame(finalData)
  }
  # output_data_frame <- li
  #   for(thing in output_list){
  #     max_row_length <- max(nrow(output_data_frame),
  #                           nrow(thing))
  #     padding( input_data_frame = thing, row_count = max_row_length)
  #     padding( input_data_frame = output_data_frame, row_count = max_row_length)
  #     # output_data_frame <- cbind(output_data_frame,thing)
  #   }
  
  print(names(output_list))
  output_list <- lapply(output_list, as.data.frame)
  MAX <- max(unlist(lapply( X = output_list, FUN = nrow)))
  # print(unlist(lapply(output_list,names)))
  temp <- lapply(X = output_list, FUN = padding, row_count = MAX)
  output_data_frame <- do.call(what = "cbind", args = temp)
  print(dim(output_data_frame))
  # 
}
whereFun <- "~/Documents/MyFolders/DaphniaLab/Functions/CSV/"
data_folder <- "1E2"
ids <- "Drug"
whereData <- paste0("~/Documents/MyFolders/DaphniaLab/",
                    "Raw Data/A68930/Dose_uM_CSV/", 
                    data_folder, "/")

r <-c_bind(path_to_data = whereData,
           path_to_functions = whereFun, 
           animal_ID = ids,
           name_of_env = "thing",
           frame_rate = 30,
           separator = ",",
           return_an_object = FALSE)
