getPredictionResult <- function(
    connectionHandler, 
    tableName, 
    performanceId, 
    schema
    ){
  
  shiny::withProgress(message = paste('Extracting PLP results from', tableName), value = 0, {

  shiny::incProgress(1/3, detail = paste("Translating and rendering "))

  sql <- "SELECT * FROM @my_schema.@table_name WHERE performance_id = @performance_id"

  shiny::incProgress(2/3, detail = paste("Extracting data "))
  
  result <- connectionHandler$queryDb(
    sql = sql, 
    my_schema = schema,
    table_name = tableName,
    performance_id = performanceId()
  )
  
  if('evaluation' %in% colnames(result)){
    result$evaluation <- trimws(result$evaluation)
  }
  
  shiny::incProgress(3/3, detail = paste("Finished "))
  
  })

  return(result)
}


