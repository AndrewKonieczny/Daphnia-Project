Binder <- function(animals,final,
                   notifications = FALSE)
{
  if(class(final) == "NULL")
  {
    if(notifications) {cat("\nfinal is a NULL object\n", class(final),class(animals))}
    
    final <- rbind(final,animals)
  }
  if(class(animals) == "data.frame" && class(final) == "data.frame")
  {
    if(notifications) {cat("\nboth data sets are data frames\n")}
    
    diff <- abs(dim(final)[1] - dim(animals)[1])
    if(dim(animals)[1] < dim(final)[1])
    {
      if(notifications) {cat("\nAdding ",diff," rows to animals data frame.")}
      
      for(n in 1:diff)
      {
        nullRow <- rep(NA, dim(animals)[2])
        animals <- rbind(animals,nullRow)
      }
    }
    if(dim(final)[1] < dim(animals)[1])
    {
      if(notifications) {cat("\nAdding ",diff," rows to final data frame.")}
      
      for(n in 1:diff)
      {
        nullRow <- rep(NA, dim(final)[2])
        final <- rbind(final,nullRow)
      }
    }
    final <- cbind(final,animals)
  }
  return(final)
}