# compute mean values for each level of a grouping variable (e.g., block)
meansByBlock = function(data) {
  data <- as.data.frame(data)
  meansOut <- aggregate(data, list(data$block), mean)
  return(meansOut)
}
