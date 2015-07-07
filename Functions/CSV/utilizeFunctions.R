
dose <- "1E2"
ids <- "Drug"
whereData <- paste0("~/Documents/MyFolders/DaphniaLab/",
                    "Raw Data/A68930/Dose_uM_CSV/",dose, "/")
whereFun <- "~/Documents/MyFolders/DaphniaLab/Functions/CSV/"
requiredFunctions <- c("rowBind.R","columnBind.R")
source( paste0(whereFun, "rowBind.R"))
source( paste0(whereFun, "columnBind.R"))
rowBind(whereIsTheData = whereData,
        whereIsTheFunction = whereFun,  
        animalID = ids,
        separator = "\t", 
        returnAsList = T,
        name_of_env = "bar")

# columnBind(whereIsTheData = paste0(whereData,"/AllAnimals"), 
#            animalID = ids, separator = ",",
#            csvName = newfile) 
'Done'
head(bar$Drug01)
hist(bar$Drug01$local_max_Drug01)
