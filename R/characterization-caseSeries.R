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
      
      inputSelectionDfViewer(id = ns('inputSelected'), title = 'Selected'),
      
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
          shinyWidgets::pickerInput(
            inputId = session$ns('databaseId'),
            label = 'Database: ',
            choices = options()$databaseIds,
            selected = options()$databaseIds[1],
            multiple = F,
            options = shinyWidgets::pickerOptions(
              actionsBox = TRUE,
              liveSearch = TRUE,
              size = 10,
              dropupAuto = TRUE,
              liveSearchStyle = "contains",
              liveSearchPlaceholder = "Type here to search",
              virtualScroll = 50
            )
          ),
          
          shinyWidgets::pickerInput(
            inputId = session$ns('tarInd'),
            label = 'Time-at-risk: ',
            choices = options()$tarInds,
            selected = options()$tarInds[1],
            multiple = F,
            options = shinyWidgets::pickerOptions(
              actionsBox = TRUE,
              liveSearch = TRUE,
              size = 10,
              dropupAuto = TRUE,
              liveSearchStyle = "contains",
              liveSearchPlaceholder = "Type here to search",
              virtualScroll = 50
            )
          ),
          
          shiny::actionButton(
            inputId = session$ns('generate'), 
            label = 'Generate'
          )
        )
        
      })
      
      # save the selections
      selected <- shiny::reactiveVal(NULL)
      
      shiny::observeEvent(input$generate, {
        
        selected(data.frame(
          database = names(options()$databaseIds)[which(input$databaseId == options()$databaseIds)],
          time_at_risk = names(options()$tarInds)[which(input$tarInd == options()$tarInds)]
        ))
        
        inputSelectionDfServer(
          id = 'inputSelected', 
          dataFrameRow = selected,
          ncol = 1
        )
        
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
          details = data.frame(
            database = names(options()$databaseIds)[which(input$databaseId == options()$databaseIds)],
            tar = names(options()$tarInds)[which(input$tarInd == options()$tarInds)],
            target = options()$targetName,
            outcome = options()$outcomeName,
            description = "Case series binary features before target index, during exposure and after outcome index"
          ),
          downloadedFileName = 'case_series_binary',
          colDefsInput = colDefsBinary(
            elementId = session$ns('binary-table-filter')
          ), # function below
          addActions = NULL,
          elementId = session$ns('binary-table-filter')
        )
        
        conTableOutputs <- resultTableServer(
          id = "continuousTable", 
          df = allData$continuous,
          details = data.frame(
            database = names(options()$databaseIds)[which(input$databaseId == options()$databaseIds)],
            tar = names(options()$tarInds)[which(input$tarInd == options()$tarInds)],
            target = options()$targetName,
            outcome = options()$outcomeName,
            description = "Case series continuous features before target index, during exposure and after outcome index"
          ),
          downloadedFileName = 'case_series_continuous',
          colDefsInput = colDefsContinuous(
            elementId = session$ns('continuous-table-filter')
          ), # function below
          addActions = NULL,
          elementId = session$ns('continuous-table-filter')
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
          s.START_ANCHOR, s.END_ANCHOR,
          ct1.cohort_name as target_name,
          ct2.cohort_name as outcome_name
          
          from
          @schema.@c_table_prefixsettings s
          inner join @schema.@database_meta_table d 
          on s.database_id = d.database_id
          inner join @schema.@c_table_prefixcohort_details cd
          on s.setting_id = cd.setting_id
          and s.database_id = cd.database_id
          and cd.target_cohort_id = @target_id
          and cd.outcome_cohort_id = @outcome_id
          and cd.cohort_type = 'Cases'
          
          inner join
          @schema.@cg_table_prefixcohort_definition ct1
          on 
          ct1.cohort_definition_id = cd.target_cohort_id
          
          inner join
          @schema.@cg_table_prefixcohort_definition ct2
          on 
          ct2.cohort_definition_id = cd.outcome_cohort_id
          
          
  ;"
  
  options <- connectionHandler$queryDb(
    sql = sql, 
    schema = resultDatabaseSettings$schema,
    c_table_prefix = resultDatabaseSettings$cTablePrefix,
    target_id = targetId,
    outcome_id = outcomeId,
    database_meta_table = resultDatabaseSettings$databaseTable,
    cg_table_prefix = resultDatabaseSettings$cgTablePrefix
  )
  
  outcomeName <- unique(options$outcomeName)
  targetName <- unique(options$targetName)
  
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
      tarList = tarList,
      outcomeName = outcomeName,
      targetName = targetName
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
  when cov.cohort_type = 'CasesBefore' then 'Before'
  when cov.cohort_type = 'CasesBetween' then 'During'
  when cov.cohort_type = 'CaseAfter' then 'After'
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
          and cov.cohort_type in ('CasesBetween','CasesAfter','CasesBefore')
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
  when cov.cohort_type = 'CasesBefore' then 'Before'
  when cov.cohort_type = 'CasesBetween' then 'During'
  when cov.cohort_type = 'CasesAfter' then 'After'
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
          and cov.cohort_type in ('CasesBetween','CasesAfter','CasesBefore')
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
  Nbefore <- getCountFromFE(
    sumValue = beforeData$sumValue, 
    averageValue = beforeData$averageValue
    )

  # After Index Cases
  afterData <- data %>% 
    dplyr::filter(.data$type == 'After') 
  Nafter <- getCountFromFE(
    sumValue = afterData$sumValue, 
    averageValue = afterData$averageValue
  )
  
  # During Index Cases
  duringData <- data %>% 
    dplyr::filter(.data$type == 'During') 
  Nduring <- getCountFromFE(
    sumValue = duringData$sumValue, 
    averageValue = duringData$averageValue
  )
  
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

colDefsBinary <- function(
    elementId
    ){
  result <- list(
    covariateName = reactable::colDef(
      header = withTooltip("Covariate Name",
                           "Name of the covariate"),
      filterable = T,
      minWidth = 300
    ),
    covariateId = reactable::colDef(
      show = F
    ),
    minPriorObservation = reactable::colDef(
      header = withTooltip("Min Prior Observation",
                           "Minimum prior observation time (days)"),
      filterable = T,
      filterInput = function(values, name) {
        shiny::tags$select(
          # Set to undefined to clear the filter
          onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", elementId, name),
          # "All" has an empty value to clear the filter, and is the default option
          shiny::tags$option(value = "", "All"),
          lapply(unique(values), shiny::tags$option),
          "aria-label" = sprintf("Filter %s", name),
          style = "width: 100%; height: 28px;"
        )
      }
    ), 
    outcomeWashoutDays = reactable::colDef(
      header = withTooltip("Outcome Washout Days",
                           "Number of days for the outcome washout"),
      filterable = T,
      filterInput = function(values, name) {
        shiny::tags$select(
          # Set to undefined to clear the filter
          onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", elementId, name),
          # "All" has an empty value to clear the filter, and is the default option
          shiny::tags$option(value = "", "All"),
          lapply(unique(values), shiny::tags$option),
          "aria-label" = sprintf("Filter %s", name),
          style = "width: 100%; height: 28px;"
        )
      }
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
        if (value >= 0) value else paste0('<', abs(value))
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
        if (value >= 0) value else paste0('<', abs(value))
      }
    ), 
    averageValueDuring = reactable::colDef(
      header = withTooltip("% of Cases with Feature Between Exposure & Outcome",
                           "Percent of cases with the covariate between the exposure and outcome"),
      filterable = T,
      format = reactable::colFormat(digits = 2, percent = T)
    ), 
    sumValueAfter = reactable::colDef(
      header = withTooltip("# of Cases with Feature Post-outcome",
                           "Number of cases with the covariate after the outcome"),
      filterable = T,
      format = reactable::colFormat(digits = 2, percent = F),
      cell = function(value) {
        if(is.null(value)){return('< min threshold')}
        if(is.na(value)){return('< min threshold')}
        if (value >= 0) value else paste0('<', abs(value))
      }
    ), 
    averageValueAfter = reactable::colDef(
      header = withTooltip("% of Cases with Feature Post-outcome",
                           "Percent of cases with the covariate after the outcome"),
      filterable = T,
      format = reactable::colFormat(digits = 2, percent = T)
    ), 
    
    analysisName = reactable::colDef(
      filterable = T,
      filterInput = function(values, name) {
        shiny::tags$select(
          # Set to undefined to clear the filter
          onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", elementId, name),
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

colDefsContinuous <- function(
    elementId
    ){
  result <- list(
    cohortDefinitionId = reactable::colDef(
      header = withTooltip("Cohort ID",
                           "Unique identifier of the cohort"),
      filterable = T
    ),
    type = reactable::colDef(
      header = withTooltip("Time of Cases Relative to Index",
                           "Time period relative to index date for cases for the covariate"),
      filterable = T,
      filterInput = function(values, name) {
        shiny::tags$select(
          # Set to undefined to clear the filter
          onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", elementId, name),
          # "All" has an empty value to clear the filter, and is the default option
          shiny::tags$option(value = "", "All"),
          lapply(unique(values), shiny::tags$option),
          "aria-label" = sprintf("Filter %s", name),
          style = "width: 100%; height: 28px;"
        )
      }
    ),
    covariateName = reactable::colDef(
      header = withTooltip("Covariate Name",
                           "Name of the covariate"),
      filterable = T,
      minWidth = 300
    ),
    covariateId = reactable::colDef(
      show = F
    ),
    minPriorObservation = reactable::colDef(
      header = withTooltip("Min Prior Observation",
                           "Minimum prior observation time (days)"),
      filterable = T,
      filterInput = function(values, name) {
        shiny::tags$select(
          # Set to undefined to clear the filter
          onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", elementId, name),
          # "All" has an empty value to clear the filter, and is the default option
          shiny::tags$option(value = "", "All"),
          lapply(unique(values), shiny::tags$option),
          "aria-label" = sprintf("Filter %s", name),
          style = "width: 100%; height: 28px;"
        )
      }
    ), 
    outcomeWashoutDays = reactable::colDef(
      header = withTooltip("Outcome Washout Days",
                           "Number of days for the outcome washout"),
      filterable = T,
      filterInput = function(values, name) {
        shiny::tags$select(
          # Set to undefined to clear the filter
          onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", elementId, name),
          # "All" has an empty value to clear the filter, and is the default option
          shiny::tags$option(value = "", "All"),
          lapply(unique(values), shiny::tags$option),
          "aria-label" = sprintf("Filter %s", name),
          style = "width: 100%; height: 28px;"
        )
      }
    ),
    casePostOutcomeDuration = reactable::colDef(
      header = withTooltip("Days Post-outcome Covariate Window",
                           "Number of days after the outcome we look for the covariate"),
      filterable = T,
      filterInput = function(values, name) {
        shiny::tags$select(
          # Set to undefined to clear the filter
          onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", elementId, name),
          # "All" has an empty value to clear the filter, and is the default option
          shiny::tags$option(value = "", "All"),
          lapply(unique(values), shiny::tags$option),
          "aria-label" = sprintf("Filter %s", name),
          style = "width: 100%; height: 28px;"
        )
      }
    ), 
    casePreTargetDuration = reactable::colDef(
      header = withTooltip("Days Pre-exposure Covariate Window",
                           "Number of days before the exposure we look for the covariate"),
      filterable = T,
      filterInput = function(values, name) {
        shiny::tags$select(
          # Set to undefined to clear the filter
          onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", elementId, name),
          # "All" has an empty value to clear the filter, and is the default option
          shiny::tags$option(value = "", "All"),
          lapply(unique(values), shiny::tags$option),
          "aria-label" = sprintf("Filter %s", name),
          style = "width: 100%; height: 28px;"
        )
      }
    ),
    countValue = reactable::colDef(
      header = withTooltip("# Cases with Feature",
                           "Number of cases with the covariate"),
      filterable = T,
      format = reactable::colFormat(digits = 2, percent = F),
      cell = function(value) {
        # Add < if cencored
        if (value < 0 ) paste("<", abs(value)) else abs(value)
      }
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



getCountFromFE <- function(
  sumValue, 
  averageValue
){
  
  Ns <- sumValue/averageValue
  if(sum(is.finite(Ns)) > 0 ){
    maxN <- max(Ns[is.finite(Ns)])
  } else{
    message('Issue calculating N')
    maxN <- 0
  }
  return(maxN)
}
