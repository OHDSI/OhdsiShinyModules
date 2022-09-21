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
  
  if(!dir.exists(file.path(tempdir(), 'plp-prot'))){
    dir.create(file.path(tempdir(), 'plp-prot'))
  }
  
  rmarkdown::render(
    input = protocolLoc, 
    intermediates_dir = file.path(tempdir(), 'plp-prot'),
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