getPredictionHelp <- function(file){
  fileLoc <- system.file(
    'prediction-www', 
    file, 
    package = "OhdsiShinyModules"
  )
  return(fileLoc)
}
