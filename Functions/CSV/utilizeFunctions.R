
source( "~/Documents/MyFolders/DaphniaLab/Functions/CSV/rowBind.R")  # accesses the row_bind function from a separate file
# this is to organize the code and make things less cluttered.
row_bind(path_to_data = "~/Documents/MyFolders/DaphniaLab/Raw Data/A68930/Dose_uM_CSV/0E1/",
         path_to_functions = "~/Documents/MyFolders/DaphniaLab/Functions/CSV/", 
         animal_ID = "Control",
         camera_ID = "Fiber",
         name_of_env = "bar",
         frame_rate = 30,
         return_an_object = TRUE)
# 
# whereFun <- "~/Documents/MyFolders/DaphniaLab/Functions/CSV/"
# data_folder <- "0E1"
# ids <- "Control"
# whereData <- paste0("~/Documents/MyFolders/DaphniaLab/",
#                     "Raw Data/A68930/Dose_uM_CSV/", 
#                     data_folder, "/")
# source( paste0(whereFun, "columnBind.R"))
# column_bind(data = bar,
#             animal_ID = ids,
#             separator = ",",
#             return_as_file = FALSE,
#             output_name = NULL)