
data_folder <- "1E2"
ids <- "Drug"
whereData <- paste0("~/Documents/MyFolders/DaphniaLab/",
                    "Raw Data/A68930/Dose_uM_CSV/",data_folder, "/")
whereFun <- "~/Documents/MyFolders/DaphniaLab/Functions/CSV/"
requiredFunctions <- c("rowBind.R","columnBind.R")
source( paste0(whereFun, "rowBind.R"))
source( paste0(whereFun, "columnBind.R"))

rowBind(path_to_data = whereData,
        path_to_functions = whereFun, 
        animal_ID = ids,
        name_of_env = "bar",
        frame_rate = 30,
        separator = ",",
        return_an_object = TRUE)

# columnBind(whereIsTheData = paste0(whereData,"/AllAnimals"), 
#            animalID = ids, separator = ",",
#            csvName = newfile) 