
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
        separator = ",", 
        returnAsList = T,
        name_of_env = "bar")

# columnBind(whereIsTheData = paste0(whereData,"/AllAnimals"), 
#            animalID = ids, separator = ",",
#            csvName = newfile) 

head(bar$Drug01)
hist(bar$Drug01$local_max,50)
plot(x = bar$Drug01$time,
     y = bar$Drug01$detrend,
     xlim = c(0,15),
     type="l")
abline(h = 0, col = "red")
points(x = bar$Drug01$time,
       y = bar$Drug01$local_max,
       cex = 0.5,
       col = "blue")
summary(bar$Drug01$local_max)