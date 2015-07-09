whereFun <- "~/Documents/MyFolders/DaphniaLab/Functions/CSV/"
data_folder <- "1E2"
ids <- "Drug"
whereData <- paste0("~/Documents/MyFolders/DaphniaLab/",
                    "Raw Data/A68930/Dose_uM_CSV/", 
                    data_folder, "/")
source( paste0(whereFun, "rowBind.R"))
row_bind(path_to_data = whereData,
         path_to_functions = whereFun, 
         animal_ID = ids,
         name_of_env = "bar",
         frame_rate = 30,
         separator = ",",
         return_an_object = TRUE)

source( paste0(whereFun, "columnBind.R"))
column_bind(data = bar,
            animal_ID = ids,
            separator = ",",
            return_as_file = FALSE,
            output_name = NULL)