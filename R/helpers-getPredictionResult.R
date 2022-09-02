getPredictionResult <- function(con, tableName, performanceId, mySchema, targetDialect){
  
  shiny::withProgress(message = paste('Extracting PLP results from', tableName), value = 0, {

  shiny::incProgress(1/3, detail = paste("Translating and rendering "))

  sql <- "SELECT * FROM @my_schema.@table_name WHERE performance_id = @performance_id"
  sql <- SqlRender::render(
    sql = sql, 
    my_schema = mySchema,
    table_name = tableName,
    performance_id = performanceId()
  )
  sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)
  
  shiny::incProgress(2/3, detail = paste("Extracting data "))
  
  result <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
  colnames(result) <- SqlRender::snakeCaseToCamelCase(colnames(result))
  
  if('evaluation' %in% colnames(result)){
    result$evaluation <- trimws(result$evaluation)
  }
  
  shiny::incProgress(3/3, detail = paste("Finished "))
  
  })

  return(result)
}


