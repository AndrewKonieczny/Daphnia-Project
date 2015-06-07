readTable <- function(directory, 
                      ID,
                      animalNumber,
                      timeStamp,
                      frameRate = 30,
                      columnNames = c("","Area","X","Y","Slice"), 
                      separator = ",") 
{
  if(separator == ","){
    data <- read.csv(file = directory, 
                     header = TRUE, 
                     sep = ",", 
                     quote = "\"",
                     dec = ".", 
                     fill = TRUE, 
                     comment.char = "")
  }
  if(separator == "\t"){
    data <- read.delim(file = directory, 
                       header = TRUE, 
                       sep = "\t", 
                       quote = "\"",
                       dec = ".", 
                       fill = TRUE, 
                       comment.char = "")
  }
  names(data) <- c(paste0(ID, sprintf("%02.0f", animalNumber)), 
                   names(data)[-1])
  #Time <- data$Slice
  #Time <- Time/frameRate + timeStamp * 60
  #data <- cbind(data,Time)
  return(data)
}

