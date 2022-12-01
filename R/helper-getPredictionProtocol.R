createPredictionProtocol <- function(
  con, 
  mySchema, 
  targetDialect,
  myTableAppend,
  databaseTableAppend,
  cohortTableAppend,
  modelDesignId,
  output,
  intermediatesDir = file.path(tempdir(), 'plp-prot')
){
  
  #require('CirceR')
  # get the data
  #protocolLoc <- 'modules/prediction/documents/main.Rmd'
  protocolLoc <- system.file('prediction-document', "main.Rmd", package = "OhdsiShinyModules")
  
  if(!dir.exists(intermediatesDir)){
    dir.create(intermediatesDir)
  }
  
  rmarkdown::render(
    input = protocolLoc, 
    intermediates_dir = intermediatesDir,
    output_dir = output, 
    params = list(
      connection = con,
      resultSchema = mySchema, 
      targetDialect = targetDialect,
      myTableAppend = myTableAppend,
      modelDesignIds = modelDesignId,
      databaseTableAppend = databaseTableAppend,
      cohortTableAppend = cohortTableAppend
    )
  )
  
  
}