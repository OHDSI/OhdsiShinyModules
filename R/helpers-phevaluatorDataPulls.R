#add databaseId and phenotype as args into the function
#pass these into the sql code with 'where'

getPhevalAlgorithmPerformance <- function(
  connectionHandler, 
  resultsSchema,
  tablePrefix = 'pv_',
  databaseIds,
  phenotypes
) {
  
  if(!is.null(databaseIds) & !is.null(phenotypes)){
    
    sql <- "SELECT * FROM @results_schema.@table_prefixALGORITHM_PERFORMANCE_RESULTS
            WHERE( @results_schema.@table_prefixALGORITHM_PERFORMANCE_RESULTS.DATABASE_ID IN (@databaseIds)
              AND @results_schema.@table_prefixALGORITHM_PERFORMANCE_RESULTS.PHENOTYPE IN (@phenotypes)
            )
  ;"
  
  return(
    connectionHandler$queryDb(
      sql = sql,
      results_schema = resultsSchema,
      table_prefix = tablePrefix,
      databaseIds = paste0("'", databaseIds, "'", collapse = ", "),
      phenotypes = paste0("'", phenotypes, "'", collapse = ", ")
      # ,
      # snakeCaseToCamelCase = F
    )
  )
  }
  
  else {
    
    sql <- "SELECT * FROM @results_schema.@table_prefixALGORITHM_PERFORMANCE_RESULTS
  ;"
    
    return(
      connectionHandler$queryDb(
        sql = sql,
        results_schema = resultsSchema,
        table_prefix = tablePrefix,
        databaseIds = paste0("'", databaseIds, "'", collapse = ", "),
        phenotypes = paste0("'", phenotypes, "'", collapse = ", ")
        # ,
        # snakeCaseToCamelCase = F
      )
    )
  }
}

#test it

# databaseIds = c("CCAE_RS", "Germany_RS")
# phenotypes = c("hyperprolactinemia")
# 
# d <- getPhevalAlgorithmPerformance(connectionHandler = connection,
#                           resultsSchema = resultDatabaseDetails$schema,
#                           tablePrefix = resultDatabaseDetails$tablePrefix,
#                           databaseIds = databaseIds,
#                           phenotypes = phenotypes
#                           )

getPhevalCohortDefinitionSet <- function(
  connectionHandler, 
  resultsSchema,
  tablePrefix = 'pv_',
  phenotypes
) {
  
  if(!is.null(phenotypes)){
  sql <- "SELECT * FROM @results_schema.@table_prefixCOHORT_DEFINITION_SET
            WHERE( @results_schema.@table_prefixCOHORT_DEFINITION_SET.PHENOTYPE IN (@phenotypes)
            )
  ;"
  return(
    connectionHandler$queryDb(
      sql = sql,
      results_schema = resultsSchema,
      table_prefix = tablePrefix,
      phenotypes = paste0("'", phenotypes, "'", collapse = ", ")
    )
  )
  }
  
  else {
    sql <- "SELECT * FROM @results_schema.@table_prefixCOHORT_DEFINITION_SET
  ;"
    return(
      connectionHandler$queryDb(
        sql = sql,
        results_schema = resultsSchema,
        table_prefix = tablePrefix,
        phenotypes = paste0("'", phenotypes, "'", collapse = ", ")
        # ,
        # snakeCaseToCamelCase = F
      )
    )
  }
}

getPhevalDiagnostics <- function(
  connectionHandler, 
  resultsSchema,
  tablePrefix = 'pv_',
  databaseIds,
  phenotypes
) {
  
  if(!is.null(databaseIds) & !is.null(phenotypes)){
  sql <- "SELECT * FROM @results_schema.@table_prefixDIAGNOSTICS
            WHERE( @results_schema.@table_prefixDIAGNOSTICS.DATABASE_ID IN (@databaseIds)
              AND @results_schema.@table_prefixDIAGNOSTICS.PHENOTYPE IN (@phenotypes)
            )
  ;"
  return(
    connectionHandler$queryDb(
      sql = sql,
      results_schema = resultsSchema,
      table_prefix = tablePrefix,
      databaseIds = paste0("'", databaseIds, "'", collapse = ", "),
      phenotypes = paste0("'", phenotypes, "'", collapse = ", ")
    )
  )
  }
  
  else{
    
    sql <- "SELECT * FROM @results_schema.@table_prefixDIAGNOSTICS
  ;"
    return(
      connectionHandler$queryDb(
        sql = sql,
        results_schema = resultsSchema,
        table_prefix = tablePrefix,
        databaseIds = paste0("'", databaseIds, "'", collapse = ", "),
        phenotypes = paste0("'", phenotypes, "'", collapse = ", ")
        # ,
        # snakeCaseToCamelCase = F
      )
    )
  }
}

getPhevalEvalInputParams <- function(
  connectionHandler, 
  resultsSchema,
  tablePrefix = 'pv_',
  databaseIds,
  phenotypes
) {
  
  if(!is.null(databaseIds) & !is.null(phenotypes)){
  sql <- "SELECT * FROM @results_schema.@table_prefixEVALUATION_INPUT_PARAMETERS
            WHERE( @results_schema.@table_prefixEVALUATION_INPUT_PARAMETERS.DATABASE_ID IN (@databaseIds)
              AND @results_schema.@table_prefixEVALUATION_INPUT_PARAMETERS.PHENOTYPE IN (@phenotypes)
            )
  ;"
  return(
    connectionHandler$queryDb(
      sql = sql,
      results_schema = resultsSchema,
      table_prefix = tablePrefix,
      databaseIds = paste0("'", databaseIds, "'", collapse = ", "),
      phenotypes = paste0("'", phenotypes, "'", collapse = ", ")
    )
  )
  }
  
  else{
    sql <- "SELECT * FROM @results_schema.@table_prefixEVALUATION_INPUT_PARAMETERS
  ;"
    return(
      connectionHandler$queryDb(
        sql = sql,
        results_schema = resultsSchema,
        table_prefix = tablePrefix,
        databaseIds = paste0("'", databaseIds, "'", collapse = ", "),
        phenotypes = paste0("'", phenotypes, "'", collapse = ", ")
        # ,
        # snakeCaseToCamelCase = F
      )
    )
  }
}

getPhevalModelCovars <- function(
  connectionHandler, 
  resultsSchema,
  tablePrefix = 'pv_',
  databaseIds,
  phenotypes
) {
  
  if(!is.null(databaseIds) & !is.null(phenotypes)){
  sql <- "SELECT * FROM @results_schema.@table_prefixMODEL_COVARIATES
            WHERE( @results_schema.@table_prefixMODEL_COVARIATES.DATABASE_ID IN (@databaseIds)
              AND @results_schema.@table_prefixMODEL_COVARIATES.PHENOTYPE IN (@phenotypes)
            )
  ;"
  return(
    connectionHandler$queryDb(
      sql = sql,
      results_schema = resultsSchema,
      table_prefix = tablePrefix,
      databaseIds = paste0("'", databaseIds, "'", collapse = ", "),
      phenotypes = paste0("'", phenotypes, "'", collapse = ", ")
    )
  )
  }
  
  else{
    sql <- "SELECT * FROM @results_schema.@table_prefixMODEL_COVARIATES
  ;"
    return(
      connectionHandler$queryDb(
        sql = sql,
        results_schema = resultsSchema,
        table_prefix = tablePrefix,
        databaseIds = paste0("'", databaseIds, "'", collapse = ", "),
        phenotypes = paste0("'", phenotypes, "'", collapse = ", ")
        # ,
        # snakeCaseToCamelCase = F
      )
    )
  }
}

getPhevalModelInputParams <- function(
  connectionHandler, 
  resultsSchema,
  tablePrefix = 'pv_',
  databaseIds,
  phenotypes
) {
  
  if(!is.null(databaseIds) & !is.null(phenotypes)){
  sql <- "SELECT * FROM @results_schema.@table_prefixMODEL_INPUT_PARAMETERS
            WHERE( @results_schema.@table_prefixMODEL_INPUT_PARAMETERS.DATABASE_ID IN (@databaseIds)
              AND @results_schema.@table_prefixMODEL_INPUT_PARAMETERS.PHENOTYPE IN (@phenotypes)
            )
  ;"
  return(
    connectionHandler$queryDb(
      sql = sql,
      results_schema = resultsSchema,
      table_prefix = tablePrefix,
      databaseIds = paste0("'", databaseIds, "'", collapse = ", "),
      phenotypes = paste0("'", phenotypes, "'", collapse = ", ")
    )
  )
  }
  
  else{
    sql <- "SELECT * FROM @results_schema.@table_prefixMODEL_INPUT_PARAMETERS
  ;"
    return(
      connectionHandler$queryDb(
        sql = sql,
        results_schema = resultsSchema,
        table_prefix = tablePrefix,
        databaseIds = paste0("'", databaseIds, "'", collapse = ", "),
        phenotypes = paste0("'", phenotypes, "'", collapse = ", ")
        # ,
        # snakeCaseToCamelCase = F
      )
    )
  }
}

getPhevalModelPerformance <- function(
  connectionHandler, 
  resultsSchema,
  tablePrefix = 'pv_',
  databaseIds,
  phenotypes
) {
  
  if(!is.null(databaseIds) & !is.null(phenotypes)){
  sql <- "SELECT * FROM @results_schema.@table_prefixMODEL_PERFORMANCE
            WHERE( @results_schema.@table_prefixMODEL_PERFORMANCE.DATABASE_ID IN (@databaseIds)
              AND @results_schema.@table_prefixMODEL_PERFORMANCE.PHENOTYPE IN (@phenotypes)
            )
  ;"
  return(
    connectionHandler$queryDb(
      sql = sql,
      results_schema = resultsSchema,
      table_prefix = tablePrefix,
      databaseIds = paste0("'", databaseIds, "'", collapse = ", "),
      phenotypes = paste0("'", phenotypes, "'", collapse = ", ")
    )
  )
  }
  
  else{
    sql <- "SELECT * FROM @results_schema.@table_prefixMODEL_PERFORMANCE
  ;"
    return(
      connectionHandler$queryDb(
        sql = sql,
        results_schema = resultsSchema,
        table_prefix = tablePrefix,
        databaseIds = paste0("'", databaseIds, "'", collapse = ", "),
        phenotypes = paste0("'", phenotypes, "'", collapse = ", ")
        # ,
        # snakeCaseToCamelCase = F
      )
    )
  }
}

getPhevalTestSubjects <- function(
  connectionHandler, 
  resultsSchema,
  tablePrefix = 'pv_',
  databaseIds,
  phenotypes
) {
  
  if(!is.null(databaseIds) & !is.null(phenotypes)){
  sql <- "SELECT * FROM @results_schema.@table_prefixTEST_SUBJECTS
            WHERE( @results_schema.@table_prefixTEST_SUBJECTS.DATABASE_ID IN (@databaseIds)
              AND @results_schema.@table_prefixTEST_SUBJECTS.PHENOTYPE IN (@phenotypes)
            )
  ;"
  return(
    connectionHandler$queryDb(
      sql = sql,
      results_schema = resultsSchema,
      table_prefix = tablePrefix,
      databaseIds = paste0("'", databaseIds, "'", collapse = ", "),
      phenotypes = paste0("'", phenotypes, "'", collapse = ", ")
    )
  )
  }
  
  else{
    sql <- "SELECT * FROM @results_schema.@table_prefixTEST_SUBJECTS
  ;"
    return(
      connectionHandler$queryDb(
        sql = sql,
        results_schema = resultsSchema,
        table_prefix = tablePrefix,
        databaseIds = paste0("'", databaseIds, "'", collapse = ", "),
        phenotypes = paste0("'", phenotypes, "'", collapse = ", ")
        # ,
        # snakeCaseToCamelCase = F
      )
    )
  }
}

getPhevalTestSubjectsCovars <- function(
  connectionHandler, 
  resultsSchema,
  tablePrefix = 'pv_',
  databaseIds,
  phenotypes
) {
  
  if(!is.null(databaseIds) & !is.null(phenotypes)){
  sql <- "SELECT * FROM @results_schema.@table_prefixTEST_SUBJECTS_COVARIATES
            WHERE( @results_schema.@table_prefixTEST_SUBJECTS_COVARIATES.DATABASE_ID IN (@databaseIds)
              AND @results_schema.@table_prefixTEST_SUBJECTS_COVARIATES.PHENOTYPE IN (@phenotypes)
            )
  ;"
  return(
    connectionHandler$queryDb(
      sql = sql,
      results_schema = resultsSchema,
      table_prefix = tablePrefix,
      databaseIds = paste0("'", databaseIds, "'", collapse = ", "),
      phenotypes = paste0("'", phenotypes, "'", collapse = ", ")
    )
  )
  }
  
  else{
    sql <- "SELECT * FROM @results_schema.@table_prefixTEST_SUBJECTS_COVARIATES
  ;"
    return(
      connectionHandler$queryDb(
        sql = sql,
        results_schema = resultsSchema,
        table_prefix = tablePrefix,
        databaseIds = paste0("'", databaseIds, "'", collapse = ", "),
        phenotypes = paste0("'", phenotypes, "'", collapse = ", ")
        # ,
        # snakeCaseToCamelCase = F
      )
    )
  }
}


#test it
 # t <- getPhevalTestSubjectsCovars(connectionHandler = connection,
 #                           resultsSchema = resultDatabaseDetails$schema,
 #                           tablePrefix = resultDatabaseDetails$tablePrefix)

