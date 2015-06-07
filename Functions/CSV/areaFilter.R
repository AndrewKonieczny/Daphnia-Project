



areaFilter <- function(input,
                       isExtremeOutliers = TRUE)
{
  if( class(input) == "data.frame") {
    
    colNames <- c( "Area", "X", "Y", "Slice")
    colNamesTest <- sum( names(input)[-1] == colNames)
    
    if( colNamesTest == length(colNames)){

      quantiles <- data.frame(t(matrix(quantile(input$Area))))
      colnames(quantiles) <- c("0%", "25%", "50%", "75%", "100%")
      interquartileRange <- quantiles$`75%` - quantiles$`25%`
      interquartileRange <- interquartileRange * 
        ifelse(isExtremeOutliers, 3, 1.5)
      
      lowerbound <- quantiles$`25%` - interquartileRange
      upperbound <- quantiles$`75%` + interquartileRange
      
      area <- rep(1, length(input$Area))
      area[input$Area < lowerbound] <- 0
      area[input$Area > upperbound] <- 0
      
    } else {return}
  } else {return}
  return(area)
}