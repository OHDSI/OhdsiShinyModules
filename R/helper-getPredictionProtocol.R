createPredictionProtocol <- function(
  con, 
  mySchema, 
  targetDialect,
  myTableAppend,
  databaseTableAppend,
  cohortTableAppend,
  modelDesignId,
  output
){
  
  #require('CirceR')
  # get the data
  #protocolLoc <- 'modules/prediction/documents/main.Rmd'
  protocolLoc <- system.file('prediction-document', "main.Rmd", package = "OhdsiShinyModules")
  
  rmarkdown::render(
    input = protocolLoc, 
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