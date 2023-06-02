#add databaseId and phenotype as args into the function
#pass these into the sql code with 'where'

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

# databaseIds = c("CCAE_RS", "Germany_RS")
# phenotypes = c("hyperprolactinemia", )
# 
# getPhevalAlgorithmPerformance(connectionHandler = connectionHandler,
#                           resultsSchema = resultDatabaseDetails$schema,
#                           tablePrefix = resultDatabaseDetails$tablePrefix
#                           )


getPhevalCohortDefinitionSet <- function(
  connectionHandler, 
  resultsSchema,
  tablePrefix = 'pv_'
) {
  
    sql <- "SELECT * FROM @results_schema.@table_prefixCOHORT_DEFINITION_SET
  ;"
    
    return(
      connectionHandler$queryDb(
        sql = sql,
        results_schema = resultsSchema,
        table_prefix = tablePrefix
     )
    )
}

getPhevalDiagnostics <- function(
  connectionHandler, 
  resultsSchema,
  tablePrefix = 'pv_'
) {

    sql <- "SELECT * FROM @results_schema.@table_prefixDIAGNOSTICS
  ;"
    return(
      connectionHandler$queryDb(
        sql = sql,
        results_schema = resultsSchema,
        table_prefix = tablePrefix
      )
    )
}

getPhevalEvalInputParams <- function(
  connectionHandler, 
  resultsSchema,
  tablePrefix = 'pv_'
) {
  
    sql <- "SELECT * FROM @results_schema.@table_prefixEVALUATION_INPUT_PARAMETERS
  ;"
    return(
      connectionHandler$queryDb(
        sql = sql,
        results_schema = resultsSchema,
        table_prefix = tablePrefix
      )
    )
}

getPhevalModelCovars <- function(
  connectionHandler, 
  resultsSchema,
  tablePrefix = 'pv_'
) {

    sql <- "SELECT * FROM @results_schema.@table_prefixMODEL_COVARIATES
  ;"
  
    df <-  connectionHandler$queryDb(
      sql = sql,
      results_schema = resultsSchema,
      table_prefix = tablePrefix
    )
    
    df$databaseId = stringi::stri_trans_general(df$databaseId, "latin-ascii")
    df$phenotype = stringi::stri_trans_general(df$phenotype, "latin-ascii")
    df$analysisName = stringi::stri_trans_general(df$analysisName, "latin-ascii")
    df$covariateName = stringi::stri_trans_general(df$covariateName, "latin-ascii")
    
    return(
      df
      )
}

# d <- getPhevalModelCovars(connectionHandler = connectionHandler,
#                                    resultsSchema = resultDatabaseDetails$schema,
#                                    tablePrefix = resultDatabaseDetails$tablePrefix,
#                                    databaseIds = databaseIds,
#                                    phenotypes = phenotypes
# )



getPhevalModelInputParams <- function(
  connectionHandler, 
  resultsSchema,
  tablePrefix = 'pv_'
) {
  
    sql <- "SELECT * FROM @results_schema.@table_prefixMODEL_INPUT_PARAMETERS
  ;"
    return(
      connectionHandler$queryDb(
        sql = sql,
        results_schema = resultsSchema,
        table_prefix = tablePrefix
      )
    )
}

getPhevalModelPerformance <- function(
  connectionHandler, 
  resultsSchema,
  tablePrefix = 'pv_'
) {

    sql <- "SELECT * FROM @results_schema.@table_prefixMODEL_PERFORMANCE
  ;"
    return(
      connectionHandler$queryDb(
        sql = sql,
        results_schema = resultsSchema,
        table_prefix = tablePrefix
      )
    )
}

getPhevalTestSubjects <- function(
  connectionHandler, 
  resultsSchema,
  tablePrefix = 'pv_'
) {

    sql <- "SELECT * FROM @results_schema.@table_prefixTEST_SUBJECTS
  ;"
    return(
      connectionHandler$queryDb(
        sql = sql,
        results_schema = resultsSchema,
        table_prefix = tablePrefix
      )
    )
}

getPhevalTestSubjectsCovars <- function(
  connectionHandler, 
  resultsSchema,
  tablePrefix = 'pv_'
) {

    sql <- "SELECT * FROM @results_schema.@table_prefixTEST_SUBJECTS_COVARIATES
  ;"
    
    df <- connectionHandler$queryDb(
      sql = sql,
      results_schema = resultsSchema,
      table_prefix = tablePrefix
    )
    
    df$databaseId = stringi::stri_trans_general(df$databaseId, "latin-ascii")
    df$phenotype = stringi::stri_trans_general(df$phenotype, "latin-ascii")
    df$analysisName = stringi::stri_trans_general(df$analysisName, "latin-ascii")
    df$type = stringi::stri_trans_general(df$type, "latin-ascii")
    df$covariateName = stringi::stri_trans_general(df$covariateName, "latin-ascii")
    
    return(
      df
    )

}


