

getCohortGeneratorCohortCounts <- function(
  connectionHandler, 
  resultsSchema,
  tablePrefix = 'cg_'
  ) {
  
  sql <- "SELECT * FROM @results_schema.@table_prefixCOHORT_COUNT;"
  return(
    connectionHandler$queryDb(
      sql = sql,
      results_schema = resultsSchema,
      table_prefix = tablePrefix
    )
  )
}

getCohortGeneratorCohortMeta <- function(
    connectionHandler, 
  resultsSchema,
  tablePrefix = 'cg_'
  ) {
  
  sql <- "SELECT * FROM @results_schema.@table_prefixCOHORT_GENERATION;"
  return(
    connectionHandler$queryDb(
      sql = sql,
      results_schema = resultsSchema,
      table_prefix = tablePrefix
    )
  )
}




getCohortGeneratorCohortInclusionStats <- function(
    connectionHandler, 
  resultsSchema,
  tablePrefix = 'cg_'
  ) {
  
  sql <- "SELECT * FROM @results_schema.@table_prefixCOHORT_SUMMARY_STATS;"
  return(
    connectionHandler$queryDb(
      sql = sql,
      results_schema = resultsSchema,
      table_prefix = tablePrefix
    )
  )
}
