
getCohortMethodColumnsToBlind <- function(results) {
  columnsToBlind <- c("rr", "ci95Ub", "ci95Lb",
                      "logRr", "seLogRr", "p",
                      "calibratedRr", "calibratedCi95Ub",
                      "calibratedCi95Lb", "calibratedLogRr",
                      "calibratedSeLogRr",
                      "calibratedP")
  
  return(intersect(columnsToBlind, colnames(results)))
  
}

getCohortMethodSelectNamedChoices <- function(v1, v2) {
  l <- as.list(v1)
  names(l) <- as.vector(v2)
  return(l)
}


filterCohortMethodEmptyNullValues <- function(v, includeNull=TRUE) {
  valsToFilter <- c('')
  if (includeNull)
    valsToFilter <- c(valsToFilter, NULL)
  return(v[! v %in% valsToFilter])
}
