

getCohortGeneratorCohortCounts <- function(
  connectionHandler, 
  resultsSchema,
  tablePrefix = 'cg_',
  databaseTable,
  databaseTablePrefix
  ) {
  
  sql <- "SELECT cc.cohort_id, cc.cohort_entries, cc.cohort_subjects,
  dt.cdm_source_name, cd.cohort_name FROM @results_schema.@table_prefixCOHORT_COUNT cc
  join @results_schema.@database_table_prefix@database_table dt
  on cc.database_id = dt.database_id
  join @results_schema.@table_prefixCOHORT_DEFINITION cd
  on cd.cohort_definition_id = cc.cohort_id
  ;"
  return(
    connectionHandler$queryDb(
      sql = sql,
      results_schema = resultsSchema,
      table_prefix = tablePrefix,
      database_table = databaseTable,
      database_table_prefix = databaseTablePrefix
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
