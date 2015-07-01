
dose <- "1E2"
ids <- "Drug"
whereData <- paste0("~/Documents/MyFolders/DaphniaLab/",
                    "Raw Data/A68930/Dose_uM_CSV/",dose)
whereFun <- "~/Documents/MyFolders/DaphniaLab/Functions/CSV/"
newfile <- paste0("All_",ids,"_",dose,".csv")
requiredFunctions <- c("rowBind.R","columnBind.R")
source( paste0(whereFun, "rowBind.R"))
source( paste0(whereFun, "columnBind.R"))
rowBind(whereIsTheData = whereData,
        whereIsTheFunction = whereFun,  
        animalID = ids,
        frameRate = 30,
        separator = "\t", 
        timeInterval = 2, 
        returnAsList = F)
columnBind(whereIsTheData = paste0(whereData,"/AllAnimals"), 
           animalID = ids, separator = ",",
           csvName = newfile) 
'Done'