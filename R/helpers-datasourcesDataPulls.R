#pull database meta data table
getDatasourcesData <- function(
    connectionHandler, 
    resultsSchema,
    databaseMetaData
) {
  
  sql <- "SELECT * from @resultsSchema.@databaseMetaData
  ;"
  return(
    connectionHandler$queryDb(
      sql = sql,
      resultsSchema = resultsSchema,
      databaseMetaData = databaseMetaData
    )
  )
}
