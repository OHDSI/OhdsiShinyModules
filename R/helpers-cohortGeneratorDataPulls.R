

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
        dplyr::summarise(personCount = sum(.data$personCount),
        )
      
      startingCounts <- stats %>%
        dplyr::select(-c("databaseId")) %>%
        dplyr::group_by(.data$cdmSourceName, .data$cohortDefinitionId, .data$modeId) %>%
        dplyr::summarise(personCount = sum(.data$personCount),
        ) %>%
        dplyr::mutate(ruleSequence = -1,
                      ruleName = "Before any inclusion criteria",
        )
      
      attritionRowsFull <- cbind(attritionRows, rule)
      
      startingCountsFull <- cbind(startingCounts, rule %>% dplyr::select("cohortName")) %>%
        dplyr::filter(.data$cohortDefinitionId %in% !!attritionRows$cohortDefinitionId)
      
      attritionTable <- rbind(attritionTable, attritionRowsFull, startingCountsFull)
      
    }
    
  }
  
  attritionTableDistinct <- dplyr::distinct(attritionTable)
  
  #adding drop counts
  attritionTableFinal <- attritionTableDistinct %>%
    dplyr::group_by(
      .data$cdmSourceName, 
      .data$cohortDefinitionId, 
      .data$modeId) %>%
    dplyr::mutate(
      dropCount = dplyr::case_when(
        is.na(dplyr::lag(.data$personCount, order_by = .data$ruleSequence)) ~ 0,
        TRUE ~ dplyr::lag(.data$personCount, order_by = .data$ruleSequence) - .data$personCount
      ),
      dropPerc = dplyr::case_when(
        is.na(dplyr::lag(.data$personCount, order_by = .data$ruleSequence)) ~ "0.00%",
        TRUE ~  paste(
          round(
            (.data$dropCount/(dplyr::lag(.data$personCount, order_by = .data$ruleSequence)) * 100), 
            digits = 2
          ),
          "%",
          sep="")
      ),
      retainPerc = dplyr::case_when(
        is.na(dplyr::lag(.data$personCount, order_by = .data$ruleSequence)) ~ "100.00%",
        TRUE ~ paste(
          round(
            (.data$personCount/(dplyr::lag(.data$personCount, order_by = .data$ruleSequence)) * 100), 
            digits = 2
          ),
          "%",
          sep="")
        
      )
    )
  #newdata <- mtcars[order(mpg, -cyl),]
  return(attritionTableFinal[order(attritionTableFinal$ruleSequence),])
  
}

 # test <- inputValsClean %>%
 #   dplyr::filter(cohortDefinitionId == 11057 & cdmSourceName == "Optum EHR" & 
 #            modeId == "Subject")

getCohortAttritionPlot <- function(data) {
  
  #colorPal <- colorRampPalette(c("darkgreen", "green", "yellow", "orange", "red"))
  
  fig <- plotly::plot_ly() 
  fig %>%
    plotly::add_trace(
      type = "funnel",
      y = data$ruleName,
      x = data$personCount,
      texttemplate = "N: %{value:,d}<br>Number Lost: %{text:,d}",
      marker = list(color = RColorBrewer::brewer.pal(length(unique(data$ruleName)),
                                                     "Greens"
                                                     )
                   ),
      connector = list(fillcolor = "#e9e9bf"),
      text = data$dropCount,
      hoverinfo = "percent initial+percent previous" ,
      hovertemplate='% of Previous: %{percentPrevious:.2%}<br> % of Initial: %{percentInitial:.2%}</b><extra></extra>'
      ) %>%
    plotly::layout(title = "Cohort Attrition by Inclusion Rules",
                   yaxis = list(categoryarray = c(order(data$personCount, decreasing = T)))
           )
  
}









