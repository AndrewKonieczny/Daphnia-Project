
dose <- "1E2"
ids <- "Drug"
whereData <- paste0("~/Documents/MyFolders/DaphniaLab/",
                    "Raw Data/A68930/Dose_uM_CSV/",dose,"/")
whereFun <- "~/Documents/MyFolders/DaphniaLab/Functions/CSV/"
newfile <- paste0("All_",ids,"_",dose,".csv")

analyzeData <- function(whereIsTheData,
                        whereIsTheFunction, 
                        animalID,
                        separator = ",",
                        timeInterval = 2,
                        returnAsList = FALSE)
{ 
  # Importing the required functions   
  requiredFunctions <- c("forceVector.R")
  functionfiles <- grep(".R",
                        list.files(whereIsTheFunction), value=TRUE)
  if(requiredFunctions %in% functionfiles){
    for(element in requiredFunctions){
      source(paste0(whereIsTheFunction,element))
    }
  }
  out <- NULL
  # Importing the data
  cameraID <- c("Fiber","Animal")
  csvfiles <- grep(".csv",list.files(whereIsTheData),value=TRUE)
  for(identifier in cameraID){
    temp <- grep(identifier,csvfiles,value=TRUE)
    assign(identifier, temp)
  }
  temp <- lapply(strsplit(get(cameraID[1]),"_"), 
                 grep, pattern=animalID, value=T)
  animalList <- as.vector(unique(unlist(temp)))
  count <- 0
  for(animal in animalList){
    animalDataName <- grep(animal,get("Fiber"),value=TRUE)
    animalDataPath <- paste0(whereIsTheData,animalDataName)
    csvdata <- lapply(animalDataPath, 
                      read.delim, 
                      sep = separator)
    asdataframe <- lapply(csvdata, as.data.frame)
    appendedData <- lapply(csvdata, 
                           forceVector, 
                           where = whereIsTheFunction,
                           timeRecorded = 0,
                           frameRate = 30,
                           range = 3, 
                           filterArea = FALSE)
    boundData <- do.call("rbind", appendedData)
    remove <- c("Time",grep("X.",names(boundData),value=T))
    #     print(remove)
    boundData <- boundData[is.na(pmatch(names(boundData),remove))]
    names(boundData) <- paste0(animal,"_",names(boundData))
    dir.create(paste0(whereIsTheData,"/AllAnimals"),showWarnings=F)
    write.csv(boundData, file = paste0("AllAnimals/",animal,".csv")) 
    count <- count +1
  }
  padding <- function(x,y){
    if(is.null(x)){
      output <- y
    }
    else{
      myname <- list(NULL,names(x))
      test <- nrow(x)-nrow(y)
      diff <- abs(test)
      while(nrow(x)>nrow(y)){
        y <- rbind(y,rep(NA,ncol(y)))
      }
      while(nrow(x)<nrow(y)){
        x <- rbind(x,rep(NA,ncol(x)))
      }
      output <- cbind(x,y)
    }
    return(output)
  }
  newcsvfiles <- grep(".csv",
                      list.files(paste0(whereIsTheData,"AllAnimals")),
                      value=TRUE)
  out <- lapply(paste0(whereIsTheData,"AllAnimals/",newcsvfiles),read.csv)
  final <- NULL 
  for(el in out){
    final <- padding(final,el)
  }
  final <- subset(final,select=-c(X))
  final <- final[, -which(names(final) %in% 
                                c("X",grep("X.",names(final),value=T)))]
  print(names(final))
  if(!returnAsList) {
    newName <- paste0("All_", 
                      tail(unlist(strsplit(
                        x = whereIsTheData,
                        split = "/")),1),".csv")
    print(paste("hi",newName))
    write.csv(final, file = paste0("AllAnimals/",newName))
  }
  else{final}
}

analyzeData(whereIsTheData = whereData,
            whereIsTheFunction = whereFun,  
            animalID = ids,
            separator = "\t", 
            returnAsList = F)