getPhevalAlgorithmPerformance <- function(
  connectionHandler, 
  resultsSchema,
  tablePrefix = 'pv_'
) {
  
  sql <- "SELECT * FROM @results_schema.@table_prefixALGORITHM_PERFORMANCE_RESULTS
  ;"
  return(
    connectionHandler$queryDb(
      sql = sql,
      results_schema = resultsSchema,
      table_prefix = tablePrefix
    )
  )
}

#test it
d <- getPhevalAlgorithmPerformance(connectionHandler = connection,
                          resultsSchema = resultDatabaseDetails$schema,
                          tablePrefix = resultDatabaseDetails$tablePrefix)
