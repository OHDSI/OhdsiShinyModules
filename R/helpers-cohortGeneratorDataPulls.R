

getCohortGeneratorCohortCounts <- function(connection, resultsSchema) {
  
  sql <- "SELECT * FROM @results_schema.CG_COHORT_COUNT"
  return(
    DatabaseConnector::renderTranslateQuerySql(connection, sql,
                                               snakeCaseToCamelCase = TRUE,
                                               results_schema = resultsSchema)
  )
}

getCohortGeneratorCohortMeta <- function(connection, resultsSchema) {
  
  sql <- "SELECT * FROM @results_schema.CG_COHORT_GENERATION"
  return(
    DatabaseConnector::renderTranslateQuerySql(connection, sql,
                                               snakeCaseToCamelCase = TRUE,
                                               results_schema = resultsSchema)
  )
}




getCohortGeneratorCohortInclusionStats <- function(connection, resultsSchema) {
  
  sql <- "SELECT * FROM @results_schema.CG_COHORT_SUMMARY_STATS"
  return(
    DatabaseConnector::renderTranslateQuerySql(connection, sql,
                                               snakeCaseToCamelCase = TRUE,
                                               results_schema = resultsSchema)
  )
}