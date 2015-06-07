BulkExportToCSV <- function(a,b,c,d,e,location)
{
  activity <- c(a,b,c,d,e)
  doses <- c(rep(0, length(a)),
             rep(0.1, length(b)),
             rep(1, length(c)),
             rep(10, length(d)),
             rep(100, length(e)))
  final <- as.data.frame(cbind(doses,activity))
  final$doses <- as.factor(final$doses)
  write.csv(final, location)
}