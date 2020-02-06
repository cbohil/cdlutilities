# summarize matrix values
#

matrixSummary = function(data) {
  columnMean <- colMeans(data)
  columnSD <- apply(data, 2, sd)
  columnSE <-columnSD / sqrt(nrow(data))
  returnList <- list(columnMean, columnSD, columnSE)
  names(returnList) <- c("colMeans", "colSD", "colSE")
  return(returnList)
}
