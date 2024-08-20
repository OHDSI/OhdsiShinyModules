createPredictionProtocol <- function(
    connectionHandler, 
    resultDatabaseSettings,
    modelDesignId,
    output,
    intermediatesDir = file.path(tempdir(), 'plp-prot')
){
  
  #require('CirceR')
  # get the data
  #protocolLoc <- 'modules/prediction/documents/main.Rmd'
  protocolLoc <- system.file('patient-level-prediction-document', "main.Rmd", package = "OhdsiShinyModules")
  
  if(!dir.exists(intermediatesDir)){
    dir.create(intermediatesDir)
  }
  
  rmarkdown::render(
    input = protocolLoc, 
    intermediates_dir = intermediatesDir,
    output_dir = output, 
    params = list(
      connectionHandler = connectionHandler,
      resultSchema = resultDatabaseSettings$schema, 
      myTableAppend = resultDatabaseSettings$plpTablePrefix,
      modelDesignIds = modelDesignId,
      databaseTableAppend = resultDatabaseSettings$databaseTablePrefix,
      cohortTableAppend = resultDatabaseSettings$cgTablePrefix
    )
  )
  
  
}
