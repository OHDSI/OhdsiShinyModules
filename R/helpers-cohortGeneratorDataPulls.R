

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




getCohortGeneratorCohortInclusionSummary <- function(
    connectionHandler, 
  resultsSchema,
  tablePrefix = 'cg_',
  databaseTable,
  databaseTablePrefix
  ) {
  
  sql <- "SELECT css.cohort_definition_id, css.base_count, css.final_count, css.mode_id,
  dt.cdm_source_name, cd.cohort_name FROM @results_schema.@table_prefixCOHORT_SUMMARY_STATS css
  join @results_schema.@database_table_prefix@database_table dt
  on css.database_id = dt.database_id
  join @results_schema.@table_prefixCOHORT_DEFINITION cd
  on cd.cohort_definition_id = css.cohort_definition_id
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



getCohortGeneratorInclusionRules <- function(
  connectionHandler, 
  resultsSchema,
  tablePrefix = 'cg_'
) {
  
  sql <- "SELECT ci.cohort_definition_id, ci.rule_sequence, ci.name as rule_name,
  cd.cohort_name FROM @results_schema.@table_prefixCOHORT_INCLUSION ci
  join @results_schema.@table_prefixCOHORT_DEFINITION cd
  on cd.cohort_definition_id = ci.cohort_definition_id
  ;"
  return(
    connectionHandler$queryDb(
      sql = sql,
      results_schema = resultsSchema,
      table_prefix = tablePrefix
    )
  )
}

getCohortGeneratorInclusionStats <- function(
  connectionHandler, 
  resultsSchema,
  tablePrefix = 'cg_',
  databaseTable,
  databaseTablePrefix
) {
  
  sql <- "SELECT cir.database_id, cir.cohort_definition_id, cir.inclusion_rule_mask, cir.person_count, cir.mode_id,
  dt.cdm_source_name FROM @results_schema.@table_prefixCOHORT_INC_RESULT cir
  join @results_schema.@database_table_prefix@database_table dt
  on cir.database_id = dt.database_id
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

getCohortGenerationAttritionTable <- function(
  rules,
  stats
){
  
  uniqueCohortIDs <- unique(rules$cohortDefinitionId)
  
  attritionTable <- data.frame()
  
  for(cohortId in uniqueCohortIDs){
    
    cohortRules <- rules %>% 
      dplyr::filter(.data$cohortDefinitionId==cohortId) %>%
      dplyr::select("ruleSequence", "ruleName", "cohortName") %>%
      dplyr::arrange("ruleSequence")
    
    testMask = 0
    
    for(i in 1:nrow(cohortRules)){
      
      rule = cohortRules[i,]
      
      testMask = testMask + 2^(rule$ruleSequence)
      
      attritionRows <- stats %>%
        dplyr::filter((.data$cohortDefinitionId == !!cohortId) &
                        (bitwAnd(.data$inclusionRuleMask, !!testMask) == !!testMask)
        ) %>% 
        dplyr::select(-c("databaseId")) %>%
        dplyr::group_by(.data$cdmSourceName, .data$cohortDefinitionId, .data$modeId) %>%
        dplyr::summarise(personCount = sum(.data$personCount))
      
      attritionRowsFull <- cbind(attritionRows, rule)
      
      attritionTable <- rbind(attritionTable, attritionRowsFull)  
      
    }
    
  }
  
  return(attritionTable)
  
}









