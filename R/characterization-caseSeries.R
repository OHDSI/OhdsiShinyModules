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
            inputId = session$ns('tarInd'),
            label = 'Time-at-risk: ',
            choices = options()$tarInds,
            selected = options()$tarInds[1],
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
            tar = options()$tarList[[which(options()$tarInds == input$tarInd)]]
          )
        
        binTableOutputs <- resultTableServer(
          id = "binaryTable", 
          df = allData$binary,
          colDefsInput = colDefs(), # function below
          addActions = NULL
        )
        
        conTableOutputs <- resultTableServer(
          id = "continuousTable", 
          df = allData$continuous,
          colDefsInput = colDefs(), # function below
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
  
  sql <- "SELECT distinct s.database_id, d.CDM_SOURCE_ABBREVIATION as database_name,
          s.setting_id, 
          s.RISK_WINDOW_START,	s.RISK_WINDOW_END,	
          s.START_ANCHOR, s.END_ANCHOR
          from
          @schema.@c_table_prefixsettings s
          inner join @schema.@database_meta_table d 
          on s.database_id = d.database_id
          inner join @schema.@c_table_prefixcohort_details cd
          on s.setting_id = cd.setting_id
          and s.database_id = cd.database_id
          and cd.target_cohort_id = @target_id
          and cd.outcome_cohort_id = @outcome_id
          and cd.cohort_type = 'TnO'
  ;"
  
  options <- connectionHandler$queryDb(
    sql = sql, 
    schema = resultDatabaseSettings$schema,
    c_table_prefix = resultDatabaseSettings$cTablePrefix,
    target_id = targetId,
    outcome_id = outcomeId,
    database_meta_table = resultDatabaseSettings$databaseTable
  )
  
  db <- unique(options$databaseId)
  names(db) <- unique(options$databaseName)
  
  tar <- unique(options[,c('startAnchor','riskWindowStart', 'endAnchor', 'riskWindowEnd')])
  tarList <- lapply(1:nrow(tar), function(i) as.list(tar[i,]))
  #tar <- unique(options$settingId)
  tarInds <- 1:nrow(tar)
  names(tarInds) <- unique(paste0('(', tar$startAnchor, ' + ', tar$riskWindowStart, ') - (', 
                       tar$endAnchor, ' + ', tar$riskWindowEnd, ')'
                       ))
  
  return(
    list(
      databaseIds = db,
      tarInds = tarInds,
      tarList = tarList
    )
  )
  
}


characterizationGetCaseSeriesData <- function(
  connectionHandler,
  resultDatabaseSettings,
  targetId,
  outcomeId,
  databaseId,
  tar
){
  
  shiny::withProgress(message = 'Getting case series data', value = 0, {
    shiny::incProgress(1/4, detail = paste("Extracting binary"))
    
  sql <- "SELECT 
  case 
  when cov.cohort_type = 'TnO' then 'Before'
  when cov.cohort_type = 'TnObetween' then 'During'
  when cov.cohort_type = 'OnT' then 'After'
  end as type, 
  cr.covariate_name, 
  s.min_prior_observation, s.outcome_washout_days,
  s.case_post_outcome_duration, s.case_pre_target_duration,
  cov.covariate_id, cov.sum_value, cov.average_value 
          from
          @schema.@c_table_prefixcovariates cov
          inner join  @schema.@c_table_prefixcovariate_ref cr
          on cov.setting_id = cr.setting_id and 
          cov.database_id = cr.database_id and 
          cov.covariate_id = cr.covariate_id
          
          inner join @schema.@c_table_prefixsettings s
          on cov.setting_id = s.setting_id
          and cov.database_id = s.database_id

          where cov.target_cohort_id = @target_id
          and cov.outcome_cohort_id = @outcome_id
          and cov.cohort_type in ('TnObetween','OnT','TnO')
          --and cov.setting_id = @setting_id
          and s.risk_window_start = @risk_window_start
          and s.risk_window_end = @risk_window_end
          and s.start_anchor = '@start_anchor'
          and s.end_anchor = '@end_anchor'
          and cov.database_id = '@database_id'
          and cr.analysis_id in (109, 110, 217, 218, 305, 417, 418, 505, 605, 713, 805, 926, 927)
  ;"

  binary <- connectionHandler$queryDb(
    sql = sql, 
    schema = resultDatabaseSettings$schema,
    c_table_prefix = resultDatabaseSettings$cTablePrefix,
    target_id = targetId,
    outcome_id = outcomeId,
    risk_window_start = tar$riskWindowStart,
    risk_window_end = tar$riskWindowEnd,
    start_anchor = tar$startAnchor,
    end_anchor = tar$endAnchor,
    database_id = databaseId
  )
  
  # now process into table
  binary <- caseSeriesTable(
    data = binary
  )
  
  shiny::incProgress(3/4, detail = paste("Extracting continuous"))

  sql <- "SELECT
    case 
  when cov.cohort_type = 'TnO' then 'Before'
  when cov.cohort_type = 'TnObetween' then 'During'
  when cov.cohort_type = 'OnT' then 'After'
  end as type, 
  cr.covariate_name, 
    s.min_prior_observation, s.outcome_washout_days, 
    s.case_post_outcome_duration, s.case_pre_target_duration,
    cov.covariate_id, 
          cov.count_value, cov.min_value, cov.max_value, cov.average_value,
          cov.standard_deviation, cov.median_value, cov.p_10_value,
          cov.p_25_value, cov.p_75_value, cov.p_90_value
          from
          @schema.@c_table_prefixcovariates_continuous cov
          inner join  @schema.@c_table_prefixcovariate_ref cr
          on cov.setting_id = cr.setting_id and 
          cov.database_id = cr.database_id and 
          cov.covariate_id = cr.covariate_id
          
          inner join @schema.@c_table_prefixsettings s
          on cov.setting_id = s.setting_id
          and cov.database_id = s.database_id
          
          where cov.target_cohort_id = @target_id
          and cov.outcome_cohort_id = @outcome_id
          and cov.cohort_type in ('TnObetween','OnT','TnO')
          and s.risk_window_start = @risk_window_start
          and s.risk_window_end = @risk_window_end
          and s.start_anchor = '@start_anchor'
          and s.end_anchor = '@end_anchor'
          and cov.database_id = '@database_id'
          and cr.analysis_id in (109, 110, 217, 218, 305, 417, 418, 505, 605, 713, 805, 926, 927)
  ;"
  
  # TODO - how to remove prior outcomes??
  continuous <- connectionHandler$queryDb(
    sql = sql, 
    schema = resultDatabaseSettings$schema,
    c_table_prefix = resultDatabaseSettings$cTablePrefix,
    target_id = targetId,
    outcome_id = outcomeId,
    risk_window_start = tar$riskWindowStart,
    risk_window_end = tar$riskWindowEnd,
    start_anchor = tar$startAnchor,
    end_anchor = tar$endAnchor,
    database_id = databaseId
  )
  
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
  data
){
  
  # Before Index Cases
  beforeData <- data %>% 
    dplyr::filter(.data$type == 'Before') 
  Nbefore <- beforeData$sumValue[1]/beforeData$averageValue[1]
  
  # After Index Cases
  afterData <- data %>% 
    dplyr::filter(.data$type == 'After') 
  Nafter <- afterData$sumValue[1]/afterData$averageValue[1]
  
  # During Index Cases
  duringData <- data %>% 
    dplyr::filter(.data$type == 'During') 
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

colDefs <- function(){
  result <- list(
    covariateName = reactable::colDef(
      name = "Covariate Name", 
      filterable = T
    ),
    covariateId = reactable::colDef(
      show = F
    ),
    minPriorObservation = reactable::colDef(
      name = "Minimum prior observation"
    ), 
    outcomeWashoutDays = reactable::colDef(
      name = "outcome washout days"
    ),
    casePostOutcomeDuration = reactable::colDef(
      name = "Time after outcome we look for covariate"
    ), 
    casePreTargetDuration = reactable::colDef(
      name = "Time before exposure we look for covariate"
    ),
    sumValueBefore = reactable::colDef(
      name = "Number of cases with feature before exposure", 
      format = reactable::colFormat(digits = 2, percent = F),
      cell = function(value) {
        if(is.null(value)){return('< min threshold')}
        if(is.na(value)){return('< min threshold')}
        if (value != -1) value else '< min threshold'
      }
    ), 
    averageValueBefore = reactable::colDef(
      name = "% of cases with feature before exposure", 
      format = reactable::colFormat(digits = 2, percent = T)
    ), 
    sumValueDuring = reactable::colDef(
      name = "Number of cases with feature between exposure and outcome", 
      format = reactable::colFormat(digits = 2, percent = F),
      cell = function(value) {
        if(is.null(value)){return('< min threshold')}
        if(is.na(value)){return('< min threshold')}
        if (value != -1) value else '< min threshold'
      }
    ), 
    averageValueDuring = reactable::colDef(
      name = "% of cases with feature between exposure and outcome", 
      format = reactable::colFormat(digits = 2, percent = T)
    ), 
    sumValueAfter = reactable::colDef(
      name = "Number of cases with feature after outcome", 
      format = reactable::colFormat(digits = 2, percent = F),
      cell = function(value) {
        if(is.null(value)){return('< min threshold')}
        if(is.na(value)){return('< min threshold')}
        if (value != -1) value else '< min threshold'
      }
    ), 
    averageValueAfter = reactable::colDef(
      name = "% of cases with feature after outcome", 
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

