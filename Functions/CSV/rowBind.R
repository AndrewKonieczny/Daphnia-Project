# This function combines .csv files that are of the same animal. If you record 
# an animal 4 times over 6 minutes (at 0,2,4,6 min), this function will combine
# those 4 .csv files into one. It will bind them by row, meaning it will attach
# time 2 to time 0 where the time 0 file will be on top of the time 2 file. I 
# found this useful in easing the data analysis process. This fucntion will 
# also find the local max force produced and create a new column to record that
# data point. This row is left un-converted to force. 
# 
# The file naming convension I used was: "Control01_Time00_Fiber.csv". Where 
# "Control" is the animal ID I used, it can be anything you feel is useful for
# identification of the file, which is followed by two numbers "01" which 
# signify the first animal. I used two digets for consistancy, and I doublt you
# will test more than 99 animals in a given period. Then "Time00" signifies the
# time stamp for when the recording happened over the length of the experiement.
# Lastly, "Fiber" indicates which camera the recording came from. I named each
# camera Fiber and Animal to distinguish the camera focused on fiber movement 
# from the camera focused on animal movement. You can name your cameras whatever
# you want. Use this file naming convension in order to utilize this function.
# 
# Inputs:
#   path_to_data: This is a string surrounded by double quotes with the file 
#              path to where all of your ImageJ .csv outputs are.
#   path_to_functions: This is a string surrounded by double quotes with the
#              file path to where the forceVector.R file is. rowBind.R requires
#              the use of that function.
#   animal_ID:  This is a string identifier for the animal set. I typically used
#              the word "Drug" or "Control" based on the case of the animal set.
#              The animal_ID variable is a part of the naming convension I used 
#              for the data.
#   findForce: This is a boolean (T/F) value which, when true, will add on a 
#              force vector for you. When false, no force vector will be added.
#              The default value for this variable is true.
#   separator: This is a character variable which indicates how the .csv file is
#              structured. Comma-separated values (csv) is typically separated 
#              by commas but I left this adjustable because I needed to change
#              it to a tab ("\t") character for some of my data. The default 
#              character is the comma, ",".
#   return_an_object: This is a boolean (T/F) variable which asks you what output 
#              you want from this function. It can output the data as a list of
#              data.frames or as a .csv file. The default value for this 
#              variable is false.
# 
# Output:
#   This function by default (see return_an_object) outputs a series of .csv files 
#   to where the directary is currently set to. This fucntion can output a list 
#   of data.frames if you so choose.
#####
# 
# The following is an example of how to use the code.
#
# rowBind(path_to_data = paste0("~/Documents/MyFolders/DaphniaLab/",
#                               "Raw Data/A68930/Dose_uM_CSV/1E2/"),
#         path_to_functions = "~/Documents/MyFolders/DaphniaLab/Functions/CSV/",  
#         animal_ID = "Drug",
#         separator = "\t", 
#         return_an_object = TRUE)

#####
row_bind <- function(path_to_data,
                     path_to_functions, 
                     animal_ID,
                     camera_ID,
                     name_of_env,
                     frame_rate = 30,
                     return_an_object = FALSE){
  if( substr( path_to_data, 
             nchar( path_to_data), nchar( path_to_data)) != "/"){
    path_to_data <- paste0( path_to_data, "/")
  }
  if( substr( path_to_functions, 
             nchar( path_to_functions), nchar( path_to_functions)) != "/"){
    path_to_functions <- paste0( path_to_functions, "/")
  }
  source(paste0(path_to_functions,"filter.R"))
  files <- list.files(path = path_to_data,
                      pattern = paste0(animal_ID, "\\d{2}_Time\\d{2}_",camera_ID,".csv"),
                      full.names = FALSE,
                      ignore.case = TRUE)
  if(length( files) == 0){
    stop("no files found in the given directory:\n", path_to_data)
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
          " animal(s) in the following directory:\n\t", path_to_data)
  message("With the following naming format:\n\t.../", 
          animal_ID, "##_Time##_", camera_ID, ".csv")
  if(return_an_object){
    assign(name_of_env, 
           value = new.env(parent = globalenv()),
           inherits = TRUE)
  }
  for(animal in animal_names){
    eval_animals <- grep(pattern = animal, 
                         x = files, 
                         value = TRUE)
    message("\nConverting ", length(eval_animals), " file(s) for ", 
            animal, " to data frame...")
    data_adding_force <- lapply(X = eval_animals, 
                                FUN = filter, 
                                file_directory = path_to_data,
                                ID = animal_ID,
                                rate = frame_rate,
                                range = 3)
    finalData <- do.call(what = "rbind", 
                         args = data_adding_force)
    if(return_an_object) {
      assign(animal, 
             value = finalData, 
             inherits = FALSE,
             envir = eval(as.name(name_of_env)))
      message("Processing ", animal, " complete.\n", 
              animal, " variables:\n", paste(names(finalData), collapse = "\t"))
    } else{
      output_path <- paste0(path_to_data, "/AllAnimals")
      dir.create(output_path, 
                 showWarnings = FALSE)
      write.csv(finalData, 
                file = paste0(output_path, "/", animal_ID, ".csv")) 
      message("Output a .csv file named ", animal_ID,
              ".csv which is in the following directory:\n", output_path)
    } 
  }
  if(return_an_object){
    message("\noutput:\t", class(eval(as.name(name_of_env))),
            "\noutput name:\t", name_of_env)
  } 
}