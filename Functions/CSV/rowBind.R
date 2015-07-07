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
#   whereIsTheData: This is a string surrounded by double quotes with the file 
#              path to where all of your ImageJ .csv outputs are.
#   whereIsTheFunction: This is a string surrounded by double quotes with the
#              file path to where the forceVector.R file is. rowBind.R requires
#              the use of that function.
#   animalID:  This is a string identifier for the animal set. I typically used
#              the word "Drug" or "Control" based on the case of the animal set.
#              The animalID variable is a part of the naming convension I used 
#              for the data.
#   findForce: This is a boolean (T/F) value which, when true, will add on a 
#              force vector for you. When false, no force vector will be added.
#              The default value for this variable is true.
#   separator: This is a character variable which indicates how the .csv file is
#              structured. Comma-separated values (csv) is typically separated 
#              by commas but I left this adjustable because I needed to change
#              it to a tab ("\t") character for some of my data. The default 
#              character is the comma, ",".
#   returnAsList: This is a boolean (T/F) variable which asks you what output 
#              you want from this function. It can output the data as a list of
#              data.frames or as a .csv file. The default value for this 
#              variable is false.
# 
# Output:
#   This function by default (see returnAsList) outputs a series of .csv files 
#   to where the directary is currently set to. This fucntion can output a list 
#   of data.frames if you so choose.
#####
# 
# The following is an example of how to use the code.
#
# dose <- "1E2"
# ids <- "Drug"
# whereData <- paste0("~/Documents/MyFolders/DaphniaLab/",
#                     "Raw Data/A68930/Dose_uM_CSV/",dose,"/")
# whereFun <- "~/Documents/MyFolders/DaphniaLab/Functions/CSV/"
# setwd(file.path(paste0(whereData,"AllAnimals")))
# t = rowBind(whereIsTheData = whereData,
#         whereIsTheFunction = whereFun,  
#         animalID = ids,
#         separator = "\t", 
#         returnAsList = T)

#####
rowBind <- function(whereIsTheData,
                    whereIsTheFunction, 
                    animalID,
                    separator = ",",
                    returnAsList = FALSE,
                    name_of_env = animalID){
  requiredFunctions <- c("filter","forceVector")
  functionPath <- paste0(whereIsTheFunction, requiredFunctions)
  #   try(!file.exists( functionPath)){
  #     stop("file path \n",
  #          functionPath,
  #          "\n not found.",
  #          "The file for forceVector.R was not found at this path.")
  #   } else{
  #     source(functionPath)
  #   }

  sapply(paste0(whereIsTheFunction, requiredFunctions, ".R"),source,.GlobalEnv)
  print(requiredFunctions %in% ls())
  files <- list.files(path = whereIsTheData,
                      pattern = "Drug\\d{2}_Time\\d{2}_Fiber.csv",
                      full.names = FALSE,
                      ignore.case = TRUE)
  if(length( files) == 0){
    stop("no files found in the given directory")
  }
  animal_names <- grep( animalID,
                        unique(
                          unlist(lapply(files, strsplit, split = "_"))),
                        value = TRUE)
  if(length( animal_names) == 0){
    stop("there is a problem with the naming format of the .csv files ",
         "or the value of the input 'animalID'")
  }
  message("There are ", length(files), " file(s) and ", length(animal_names),
          " animals in the following directory:\n\t", whereIsTheData)
  if(returnAsList){
    assign(name_of_env, 
           value = new.env(parent = globalenv()),
           inherits = TRUE)
  }
  for(animal in animal_names){
    eval_animals <- grep(animal, files, value = TRUE)
    message("Converting ", length(eval_animals), " file(s) for ", 
            animal, " to data frame")
    csv_data <- lapply(paste0(whereIsTheData, eval_animals), 
                       read.delim, 
                       sep = separator)
    data_adding_force <- lapply(csv_data, 
                                filter, 
                                range = 3)
    finalData <- do.call("rbind", data_adding_force)
    names(finalData) <- paste0(names(finalData), "_", animal)
    finalData <- finalData[-grep("X\\.", names(finalData))]
    if(!returnAsList) {
      output_path <- paste0(whereIsTheData, "/AllAnimals")
      dir.create(output_path, showWarnings = FALSE)
      write.csv(finalData, 
                file = paste0(output_path,"/",ID,".csv")) 
      message("Output a .csv file named ", ID,
              ".csv which is in the following directory:\n", output_path)
    } 
    if(returnAsList) {
      assign(animal, 
             value = finalData, inherits = F,
             envir = eval(as.name(name_of_env)))
    }
  }
  if(returnAsList){
    message("Returned a list of data frames")
  } 
}