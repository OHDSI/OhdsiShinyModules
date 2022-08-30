
getEstimationColumnsToBlind <- function(results) {
  columnsToBlind <- c("rr", "ci95Ub", "ci95Lb",
                      "logRr", "seLogRr", "p",
                      "calibratedRr", "calibratedCi95Ub",
                      "calibratedCi95Lb", "calibratedLogRr",
                      "calibratedSeLogRr",
                      "calibratedP")
  
  return(intersect(columnsToBlind, colnames(results)))
  
}

getEstimationSelectNamedChoices <- function(v1, v2) {
  l <- as.list(v1)
  names(l) <- as.vector(v2)
  return(l)
}


filterEstimationEmptyNullValues <- function(v, includeNull=TRUE) {
  valsToFilter <- c('')
  if (includeNull)
    valsToFilter <- c(valsToFilter, NULL)
  return(v[! v %in% valsToFilter])
}
