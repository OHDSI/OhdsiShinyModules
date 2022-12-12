

getCohortGeneratorCohortCounts <- function(
  connection, 
  resultsSchema,
  tablePrefix = 'cg_'
  ) {
  
  sql <- "SELECT * FROM @results_schema.@table_prefixCOHORT_COUNT;"
  return(
    DatabaseConnector::renderTranslateQuerySql(
      connection, sql,
      snakeCaseToCamelCase = TRUE,
      results_schema = resultsSchema,
      table_prefix = tablePrefix
    )
  )
}

getCohortGeneratorCohortMeta <- function(
  connection, 
  resultsSchema,
  tablePrefix = 'cg_'
  ) {
  
  sql <- "SELECT * FROM @results_schema.@table_prefixCOHORT_GENERATION;"
  return(
    DatabaseConnector::renderTranslateQuerySql(
      connection, sql,
      snakeCaseToCamelCase = TRUE,
      results_schema = resultsSchema,
      table_prefix = tablePrefix
    )
  )
}




getCohortGeneratorCohortInclusionStats <- function(
  connection, 
  resultsSchema,
  tablePrefix = 'cg_'
  ) {
  
  sql <- "SELECT * FROM @results_schema.@table_prefixCOHORT_SUMMARY_STATS;"
  return(
    DatabaseConnector::renderTranslateQuerySql(
      connection, sql,
      snakeCaseToCamelCase = TRUE,
      results_schema = resultsSchema,
      table_prefix = tablePrefix
    )
  )
}
