# @file characterization-aggregateFeatures.R
#
# Copyright 2024 Observational Health Data Sciences and Informatics
#
# This file is part of OhdsiShinyModules
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.



characterizationCaseSeriesViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    
    # module that does input selection for a single row DF
    shiny::uiOutput(ns("inputs")),
    
    shiny::conditionalPanel(
      condition = 'input.generate != 0',
      ns = ns,
      shinydashboard::tabBox(
        width = "100%",
        # Title can include an icon
        title = shiny::tagList(shiny::icon("gear"), "Case Series"),
        shiny::tabPanel("Binary Feature Table", 
                        resultTableViewer(ns('binaryTable'))
        ),
        shiny::tabPanel("Continuous Feature Table", 
                        resultTableViewer(ns('continuousTable'))
        )
      )
    )
  )

}



characterizationCaseSeriesServer <- function(
    id, 
    connectionHandler,
    resultDatabaseSettings,
    targetId, #reactive 
    outcomeId  #reactive 
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # get databases
      options <- shiny::reactive({
        characterizationGetCaseSeriesOptions(
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings,
          targetId = targetId(),
          outcomeId = outcomeId()
        )
      })
      
      output$inputs <- shiny::renderUI({ # need to make reactive?
        
        shiny::div(
          shiny::selectInput(
            inputId = session$ns('databaseId'),
            label = 'Database: ',
            choices = options()$databaseIds,
            selected = options()$databaseIds[1],
            multiple = F
          ),
          
          shiny::selectInput(
            inputId = session$ns('tarId'),
            label = 'Time-at-risk: ',
            choices = options()$tarIds,
            selected = options()$tarIds[1],
            multiple = F
          ),
          
          shiny::actionButton(
            inputId = session$ns('generate'), 
            label = 'Generate'
          )
        )
        
      })
      
      shiny::observeEvent(input$generate, {
        
        allData <- characterizationGetCaseSeriesData(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            targetId = targetId(),
            outcomeId = outcomeId(),
            databaseId = input$databaseId,
            timeAtRiskId = input$tarId
          )
        
        binTableOutputs <- resultTableServer(
          id = "binaryTable", 
          df = allData$binary,
          colDefsInput = colDefsBinary(), # function below
          addActions = NULL
        )
        
        conTableOutputs <- resultTableServer(
          id = "continuousTable", 
          df = allData$continuous,
          colDefsInput = colDefsContinuous(), # function below
          addActions = NULL
        )
        
      })
   
  return(invisible(NULL))
    }
  )
}


characterizationGetCaseSeriesOptions <- function(
  connectionHandler,
  resultDatabaseSettings,
  targetId,
  outcomeId
){
  
  sql <- "SELECT distinct cd.database_id, d.CDM_SOURCE_ABBREVIATION as database_name,
          cd.time_at_risk_id, 
          t.RISK_WINDOW_START,	t.RISK_WINDOW_END,	
          t.START_ANCHOR, t.END_ANCHOR
          from
          @schema.@c_table_prefixcohort_details cd
          inner join @schema.@database_meta_table d 
          on cd.database_id = d.database_id
          inner join @schema.@c_table_prefixtime_at_risk t
          on t.database_id = cd.database_id
          and t.run_id = cd.run_id
          and t.time_at_risk_id = cd.time_at_risk_id
          
          where cd.target_cohort_id = @target_id
          and cd.outcome_cohort_id = @outcome_id
          and cd.time_at_risk_id != 0
  ;"
  
  options <- connectionHandler$queryDb(
    sql = sql, 
    schema = resultDatabaseSettings$schema,
    c_table_prefix = resultDatabaseSettings$cTablePrefix,
    database_meta_table = resultDatabaseSettings$databaseTable,
    target_id = targetId,
    outcome_id = outcomeId
  )
  
  db <- unique(options$databaseId)
  names(db) <- unique(options$databaseName)
  
  tar <- unique(options$timeAtRiskId)
  names(tar) <- unique(paste0('(', options$startAnchor, ' + ', options$riskWindowStart, ') - (', 
                       options$endAnchor, ' + ', options$riskWindowEnd, ')'
                       ))
  
  return(
    list(
      databaseIds = db,
      tarIds = tar
    )
  )
  
}


characterizationGetCaseSeriesData <- function(
  connectionHandler,
  resultDatabaseSettings,
  targetId,
  outcomeId,
  databaseId,
  timeAtRiskId
){
  
  ids <- 
    
  shiny::withProgress(message = 'Getting case series data', value = 0, {
    shiny::incProgress(1/4, detail = paste("Extracting ids"))
    
    sql <- "SELECT distinct cohort_definition_id, cohort_type
          from
          @schema.@c_table_prefixcohort_details
          where target_cohort_id = @target_id
          and outcome_cohort_id = @outcome_id
          and database_id = '@database_id'
          and time_at_risk_id = @time_at_risk_id
          and cohort_type in ('TnObetween','OnT','TnO')
  ;"
    
    ids <- connectionHandler$queryDb(
      sql = sql, 
      schema = resultDatabaseSettings$schema,
      c_table_prefix = resultDatabaseSettings$cTablePrefix,
      target_id = targetId,
      outcome_id = outcomeId,
      database_id = databaseId,
      time_at_risk_id = timeAtRiskId
    )
    
    shiny::incProgress(2/4, detail = paste("Extracting binary"))
    
  sql <- "SELECT cov.cohort_definition_id, cr.covariate_name, 
  s.min_prior_observation, s.outcome_washout_days,
  s.case_post_outcome_duration, s.case_pre_target_duration,
  cov.covariate_id, cov.sum_value, cov.average_value 
          from
          @schema.@c_table_prefixcovariates cov
          inner join  @schema.@c_table_prefixcovariate_ref cr
          on cov.run_id = cr.run_id and 
          cov.database_id = cr.database_id and 
          cov.covariate_id = cr.covariate_id
          
          inner join @schema.@c_table_prefixsettings s
          on cov.run_id = s.run_id
          and cov.database_id = s.database_id
          
          where cov.cohort_definition_id in (@ids)
          and cov.database_id = '@database_id'
          and cr.analysis_id in (109, 110, 217, 218, 305, 417, 418, 505, 605, 713, 805, 926, 927)
  ;"

  binary <- connectionHandler$queryDb(
    sql = sql, 
    schema = resultDatabaseSettings$schema,
    c_table_prefix = resultDatabaseSettings$cTablePrefix,
    ids = paste0(ids$cohortDefinitionId, collapse = ','),
    database_id = databaseId
  )
  
  # now process into table
  binary <- caseSeriesTable(
    data = binary,
    ids = ids
  )
  
  shiny::incProgress(3/4, detail = paste("Extracting continuous"))

  sql <- "SELECT cov.cohort_definition_id, cr.covariate_name, 
    s.min_prior_observation, s.outcome_washout_days, 
    s.case_post_outcome_duration, s.case_pre_target_duration,
    cov.covariate_id, 
          cov.count_value, cov.min_value, cov.max_value, cov.average_value,
          cov.standard_deviation, cov.median_value, cov.p_10_value,
          cov.p_25_value, cov.p_75_value, cov.p_90_value
          from
          @schema.@c_table_prefixcovariates_continuous cov
          inner join  @schema.@c_table_prefixcovariate_ref cr
          on cov.run_id = cr.run_id and 
          cov.database_id = cr.database_id and 
          cov.covariate_id = cr.covariate_id
          
          inner join @schema.@c_table_prefixsettings s
          on cov.run_id = s.run_id
          and cov.database_id = s.database_id
          
          where cov.cohort_definition_id in (@ids)
          and cov.database_id = '@database_id'
          and cr.analysis_id in (109, 110, 217, 218, 305, 417, 418, 505, 605, 713, 805, 926, 927)
  ;"
  
  # TODO - how to remove prior outcomes??
  continuous <- connectionHandler$queryDb(
    sql = sql, 
    schema = resultDatabaseSettings$schema,
    c_table_prefix = resultDatabaseSettings$cTablePrefix,
    ids = paste0(ids$cohortDefinitionId, collapse = ','),
    database_id = databaseId
  )
  
  # add type
  ids <- merge(ids, data.frame(
    cohortType = c('TnObetween','OnT','TnO'),
    type = c('During', "After", 'Before')
  ), by = 'cohortType') %>%
    dplyr::select("cohortDefinitionId","type")
  continuous <- merge(ids, continuous, by = 'cohortDefinitionId')
  
  shiny::incProgress(4/4, detail = paste("Done"))
  
  })
  
  return(
    list(
      binary = binary,
      continuous = continuous
    )
  )
}


# now process into table
caseSeriesTable <- function(
  data,
  ids
){
  
  timeBeforeId <- ids$cohortDefinitionId[ids$cohortType == 'TnO']
  timeDuringId <- ids$cohortDefinitionId[ids$cohortType == 'TnObetween']
  timeAfterId <- ids$cohortDefinitionId[ids$cohortType == 'OnT']
  

  # Before Index Cases
  beforeData <- data %>% 
    dplyr::filter(.data$cohortDefinitionId == !!timeBeforeId) %>%
    dplyr::select(-"cohortDefinitionId") 
  Nbefore <- beforeData$sumValue[1]/beforeData$averageValue[1]
  
  # After Index Cases
  afterData <- data %>% 
    dplyr::filter(.data$cohortDefinitionId == !!timeAfterId) %>%
    dplyr::select(-"cohortDefinitionId") 
  Nafter <- afterData$sumValue[1]/afterData$averageValue[1]
  
  # During Index Cases
  duringData <- data %>% 
    dplyr::filter(.data$cohortDefinitionId == !!timeDuringId) %>%
    dplyr::select(-"cohortDefinitionId") 
  Nduring <- duringData$sumValue[1]/duringData$averageValue[1]
  
  
  beforeData <- beforeData %>%
    dplyr::mutate(
      sumValueBefore = .data$sumValue,
      averageValueBefore = .data$averageValue,
    ) %>%
    dplyr::select("covariateName", "covariateId", 'minPriorObservation', 'outcomeWashoutDays','casePostOutcomeDuration', 'casePreTargetDuration', "sumValueBefore", "averageValueBefore")
  
  afterData <-afterData %>%
    dplyr::mutate(
      sumValueAfter = .data$sumValue,
      averageValueAfter = .data$averageValue,
    ) %>%
    dplyr::select("covariateName", "covariateId", 'minPriorObservation', 'outcomeWashoutDays','casePostOutcomeDuration', 'casePreTargetDuration', "sumValueAfter", "averageValueAfter")
  
  duringData <- duringData %>%
    dplyr::mutate(
      sumValueDuring = .data$sumValue,
      averageValueDuring = .data$averageValue,
    ) %>%
    dplyr::select("covariateName", "covariateId", 'minPriorObservation', 'outcomeWashoutDays','casePostOutcomeDuration', 'casePreTargetDuration', "sumValueDuring", "averageValueDuring")
  
  
  
  allResults <- beforeData %>% 
    dplyr::full_join(
      y = duringData, 
      by = c("covariateName", "covariateId", 'minPriorObservation', 'outcomeWashoutDays','casePostOutcomeDuration', 'casePreTargetDuration')
    ) %>% 
    dplyr::full_join(
      y = afterData, 
      by = c("covariateName", "covariateId", 'minPriorObservation', 'outcomeWashoutDays','casePostOutcomeDuration', 'casePreTargetDuration')
    ) 
  
  return(allResults)
}

colDefsBinary <- function(){
  result <- list(
    covariateName = reactable::colDef(
      header = withTooltip("Covariate Name",
                           "Name of the covariate"),
      filterable = T
    ),
    covariateId = reactable::colDef(
      show = F
    ),
    minPriorObservation = reactable::colDef(
      header = withTooltip("Min Prior Observation",
                           "Minimum prior observation time (days)"),
      filterable = T
    ), 
    outcomeWashoutDays = reactable::colDef(
      header = withTooltip("Outcome Washout Days",
                           "Number of days for the outcome washout"),
      filterable = T
    ),
    casePostOutcomeDuration = reactable::colDef(
      header = withTooltip("Days Post-outcome Covariate Window",
                           "Number of days after the outcome we look for the covariate"),
      filterable = T
    ), 
    casePreTargetDuration = reactable::colDef(
      header = withTooltip("Days Pre-exposure Covariate Window",
                           "Number of days before the exposure we look for the covariate"),
      filterable = T
    ),
    sumValueBefore = reactable::colDef(
      header = withTooltip("# Cases with Feature Pre-exposure",
                           "Number of cases with the covariate prior to exposure"),
      filterable = T,
      format = reactable::colFormat(digits = 2, percent = F),
      cell = function(value) {
        if(is.null(value)){return('< min threshold')}
        if(is.na(value)){return('< min threshold')}
        if (value != -1) value else '< min threshold'
      }
    ), 
    averageValueBefore = reactable::colDef(
      header = withTooltip("% of Cases with Feature Pre-exposure",
                           "Percent of cases with the covariate prior to exposure"),
      filterable = T,
      format = reactable::colFormat(digits = 2, percent = T)
    ), 
    sumValueDuring = reactable::colDef(
      header = withTooltip("# of Cases with Feature Between Exposure & Outcome",
                           "Number of cases with the covariate between the exposure and outcome"),
      filterable = T,
      format = reactable::colFormat(digits = 2, percent = F),
      cell = function(value) {
        if(is.null(value)){return('< min threshold')}
        if(is.na(value)){return('< min threshold')}
        if (value != -1) value else '< min threshold'
      }
    ), 
    averageValueDuring = reactable::colDef(
      header = withTooltip("% of Cases with Feature Between Exposure & Outcome",
                           "Percent of cases with the covariate between the exposure and outcome"),
      filterable = T,
      format = reactable::colFormat(digits = 2, percent = T)
    ), 
    sumValueAfter = reactable::colDef(
      header = withTooltip("# of Cases with Feautre Post-outcome",
                           "Number of cases with the covariate after the outcome"),
      filterable = T,
      format = reactable::colFormat(digits = 2, percent = F),
      cell = function(value) {
        if(is.null(value)){return('< min threshold')}
        if(is.na(value)){return('< min threshold')}
        if (value != -1) value else '< min threshold'
      }
    ), 
    averageValueAfter = reactable::colDef(
      header = withTooltip("% of Cases with Feature Post-outcome",
                           "Percent of cases with the covariate after the outcome"),
      filterable = T,
      format = reactable::colFormat(digits = 2, percent = T)
    ), 
    
    analysisName = reactable::colDef(
      filterInput = function(values, name) {
        shiny::tags$select(
          # Set to undefined to clear the filter
          onchange = sprintf("Reactable.setFilter('desc-cont-select', '%s', event.target.value || undefined)", name),
          # "All" has an empty value to clear the filter, and is the default option
          shiny::tags$option(value = "", "All"),
          lapply(unique(values), shiny::tags$option),
          "aria-label" = sprintf("Filter %s", name),
          style = "width: 100%; height: 28px;"
        )
      }
    )
  )
  return(result)
}

colDefsContinuous <- function(){
  result <- list(
    cohortDefinitionId = reactable::colDef(
      header = withTooltip("Cohort ID",
                           "Unique identifier of the cohort"),
      filterable = T
    ),
    type = reactable::colDef(
      header = withTooltip("Time of Cases Relative to Index",
                           "Time period relative to index date for cases for the covariate"),
      filterable = T
    ),
    covariateName = reactable::colDef(
      header = withTooltip("Covariate Name",
                           "Name of the covariate"),
      filterable = T
    ),
    covariateId = reactable::colDef(
      show = F
    ),
    minPriorObservation = reactable::colDef(
      header = withTooltip("Min Prior Observation",
                           "Minimum prior observation time (days)"),
      filterable = T
    ), 
    outcomeWashoutDays = reactable::colDef(
      header = withTooltip("Outcome Washout Days",
                           "Number of days for the outcome washout"),
      filterable = T
    ),
    casePostOutcomeDuration = reactable::colDef(
      header = withTooltip("Days Post-outcome Covariate Window",
                           "Number of days after the outcome we look for the covariate"),
      filterable = T
    ), 
    casePreTargetDuration = reactable::colDef(
      header = withTooltip("Days Pre-exposure Covariate Window",
                           "Number of days before the exposure we look for the covariate"),
      filterable = T
    ),
    countValue = reactable::colDef(
      header = withTooltip("# Cases with Feature",
                           "Number of cases with the covariate"),
      filterable = T,
      format = reactable::colFormat(digits = 2, percent = F)
    ), 
    minValue = reactable::colDef(
      header = withTooltip("Min Value",
                           "Minimum value of the covariate"),
      filterable = T,
      format = reactable::colFormat(digits = 2, percent = F)
    ), 
    maxValue = reactable::colDef(
      header = withTooltip("Max Value",
                           "Maximum value of the covariate"),
      filterable = T,
      format = reactable::colFormat(digits = 2, percent = F)
    ), 
    averageValue = reactable::colDef(
      header = withTooltip("Average Value",
                           "Average value of the covariate"),
      filterable = T,
      format = reactable::colFormat(digits = 2, percent = F)
    ), 
    standardDeviation = reactable::colDef(
      header = withTooltip("SD",
                           "Standard deviation of the covariate"),
      filterable = T,
      format = reactable::colFormat(digits = 2, percent = F)
    ), 
    medianValue = reactable::colDef(
      header = withTooltip("Median Value",
                           "Median value of the covariate"),
      filterable = T,
      format = reactable::colFormat(digits = 2, percent = F)
    ), 
    p10Value = reactable::colDef(
      header = withTooltip("10th %tile",
                           "10th percentile value of the covariate"),
      filterable = T,
      format = reactable::colFormat(digits = 2, percent = F)
    ), 
    p25Value = reactable::colDef(
      header = withTooltip("25th %tile",
                           "25th percentile value of the covariate"),
      filterable = T,
      format = reactable::colFormat(digits = 2, percent = F)
    ), 
    p75Value = reactable::colDef(
      header = withTooltip("75th %tile",
                           "75th percentile value of the covariate"),
      filterable = T,
      format = reactable::colFormat(digits = 2, percent = F)
    ), 
    p90Value = reactable::colDef(
      header = withTooltip("90th %tile",
                           "90th percentile value of the covariate"),
      filterable = T,
      format = reactable::colFormat(digits = 2, percent = F)
    )
    )
  return(result)
}

