# compute mean values for each level of a grouping variable (e.g., block)
meansByBlock = function(data) {
  data <- as.data.frame(data)
  meansOut <- aggregate(data, list(data$block), mean)
  return(meansOut)
}

# compute accuracy by block (any number of categories)
catAccuracy = function(data) {
  # data needs to be a list of data frames
  # each data set needs columns labeled "block", "cat", & "resp"
  warning("catAccuracy needs to be generalized to send in labels for block, cat & resp")
  accuracy <- lapply(data, function(data) {lapply(split(data, data$block), function(data) {length(which(data$cat == data$resp))/nrow(data)})})
  accuracy <- matrix( unlist(accuracy), nrow=length(accuracy) , byrow = TRUE)
  return(accuracy)
}
