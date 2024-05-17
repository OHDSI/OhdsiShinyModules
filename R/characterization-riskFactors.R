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



characterizationRiskFactorViewer <- function(id) {
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
        title = shiny::tagList(shiny::icon("gear"), "Risk Factors"),
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



characterizationRiskFactorServer <- function(
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
        
        allData <- characterizationGetRiskFactorData(
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
          colDefsInput = characteriationRiskFactorColDefs(), # function below
          addActions = NULL
        )
        
        conTableOutputs <- resultTableServer(
          id = "continuousTable", 
          df = allData$continuous,
          colDefsInput = characteriationRiskFactorColDefs(), # function below
          addActions = NULL
        )
        
      })
   
  return(invisible(NULL))
    }
  )
}


characterizationGetRiskFactorData <- function(
  connectionHandler,
  resultDatabaseSettings,
  targetId,
  outcomeId,
  databaseId,
  timeAtRiskId
){
  
  shiny::withProgress(message = 'Getting risk factor data', value = 0, {
    shiny::incProgress(1/4, detail = paste("Extracting ids"))
    
    sql <- "SELECT cohort_definition_id, cohort_type
          from
          @schema.@c_table_prefixcohort_details
          where target_cohort_id = @target_id
          and outcome_cohort_id in (0, @outcome_id)
          and database_id = '@database_id'
          and time_at_risk_id in (0, @time_at_risk_id)
          and cohort_type in ('T','TnO', 'TnOprior')
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
    
  sql <- "SELECT cov.cohort_definition_id, cr.covariate_name, cov.covariate_id, cov.sum_value, cov.average_value 
          from
          @schema.@c_table_prefixcovariates cov
          inner join  @schema.@c_table_prefixcovariate_ref cr
          on cov.run_id = cr.run_id and 
          cov.database_id = cr.database_id and 
          cov.covariate_id = cr.covariate_id
          where cov.cohort_definition_id in (@ids)
          and cov.database_id = '@database_id'
          and cr.analysis_id not in (109, 110, 217, 218, 305, 417, 418, 505, 605, 713, 805, 926, 927)
  ;"

  binary <- connectionHandler$queryDb(
    sql = sql, 
    schema = resultDatabaseSettings$schema,
    c_table_prefix = resultDatabaseSettings$cTablePrefix,
    ids = paste0(ids$cohortDefinitionId, collapse = ','),
    database_id = databaseId
  )
  
  # now process into table
  binary <- riskFactorTable(
    data = binary,
    ids = ids
  )
  
  shiny::incProgress(3/4, detail = paste("Extracting continuous"))

  sql <- "SELECT cov.cohort_definition_id, cr.covariate_name, cov.covariate_id, 
          cov.count_value, cov.min_value, cov.max_value, cov.average_value,
          cov.standard_deviation, cov.median_value, cov.p_10_value,
          cov.p_25_value, cov.p_75_value, cov.p_90_value
          from
          @schema.@c_table_prefixcovariates_continuous cov
          inner join  @schema.@c_table_prefixcovariate_ref cr
          on cov.run_id = cr.run_id and 
          cov.database_id = cr.database_id and 
          cov.covariate_id = cr.covariate_id
          where cov.cohort_definition_id in (@ids)
          and cov.database_id = '@database_id'
          and cr.analysis_id not in (109, 110, 217, 218, 305, 417, 418, 505, 605, 713, 805, 926, 927)
  ;"
  
  # TODO - how to remove prior outcomes??
  continuous <- connectionHandler$queryDb(
    sql = sql, 
    schema = resultDatabaseSettings$schema,
    c_table_prefix = resultDatabaseSettings$cTablePrefix,
    ids = paste0(ids$cohortDefinitionId, collapse = ','),
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
riskFactorTable <- function(
  data,
  ids
){

  caseId <- ids$cohortDefinitionId[ids$cohortType == 'TnO']
  if(length(caseId ) == 0){
    caseId <- -1
  }
  caseData <- data %>% 
    dplyr::filter(.data$cohortDefinitionId == !!caseId) %>%
    dplyr::select(-"cohortDefinitionId")
  
  allId <- ids$cohortDefinitionId[ids$cohortType == 'T']
  allData <- data %>% 
    dplyr::filter(.data$cohortDefinitionId == !!allId) %>%
    dplyr::select(-"cohortDefinitionId")
  allData$N <- allData$sumValue[1]/allData$averageValue[1]
  
  excludeId <- ids$cohortDefinitionId[ids$cohortType == 'TnOprior']
  if(length(excludeId) == 0){
    excludeId <- -1
  }
  excludeData <- data %>% 
    dplyr::filter(.data$cohortDefinitionId == !!excludeId) %>%
    dplyr::select(-"cohortDefinitionId")
  
  if(nrow(excludeData) > 0 ){
    excludeData$exclude_N <- excludeData$sumValue[1]/excludeData$averageValue[1]
    colnames(excludeData)[!colnames(excludeData) %in% c('covariateId', 'covariateName')] <- paste0('exclude_',colnames(excludeData))
  
    # if prior Os then exclude from T
    allData <- allData %>% 
      dplyr::full_join(excludeData, by = c('covariateId', 'covariateName')) %>%
      dplyr::mutate(
        sumValue = .data$sumValue - .data$prior_sumValue,
        averageValue = (.data$sumValue - .data$prior_sumValue)/(.data$N-.data$exclude_N)
      ) %>%
      dplyr::mutate(
        N = .data$N-.data$exclude_N
      ) %>%
      dplyr::select("covariateId","covariateName","sumValue","averageValue", "N")
    
    }
  
  if(nrow(caseData) > 0){
    caseData$caseN <- caseData$sumValue[1]/caseData$averageValue[1]
    caseData <- caseData %>%
      dplyr::mutate(
      caseSumValue = .data$sumValue,
      caseAverageValue = .data$averageValue
    ) %>%
      dplyr::select("covariateId","covariateName","caseSumValue","caseAverageValue", "caseN")
    
    # join with cases
    allData <- allData %>% 
      dplyr::full_join(caseData, by = c('covariateId', 'covariateName')) %>%
      dplyr::mutate(
        nonCaseSumValue = .data$sumValue - .data$caseSumValue,
        nonCaseAverageValue = (.data$sumValue - .data$caseSumValue)/(.data$N-.data$caseN)
      ) %>%
      dplyr::mutate(
        nonCaseN = .data$N-.data$caseN
      ) %>%
      dplyr::select(
        "covariateId","covariateName",
        "caseSumValue","caseAverageValue", 
        "nonCaseSumValue","nonCaseAverageValue"
        )
    
    # add abs smd
    allData <- allData %>% 
      dplyr::mutate(
        meanDiff = (.data$caseAverageValue - .data$nonCaseAverageValue),
        std1 = .data$caseAverageValue*(1-.data$caseAverageValue),
        std2 = .data$nonCaseAverageValue*(1-.data$nonCaseAverageValue)
      ) %>% 
      dplyr::mutate(
        SMD = .data$meanDiff/sqrt((.data$std1^2 + .data$std2^2)/2),
        absSMD = abs(.data$meanDiff/sqrt((.data$std1^2 + .data$std2^2)/2))
      )
  
    
  } else{
    allData <- allData %>% 
      dplyr::mutate(
        nonCaseSumValue = .data$sumValue,
        nonCaseAverageValue = .data$averageValue
      ) %>%
      dplyr::select(
        "covariateId","covariateName",
        "nonCaseSumValue","nonCaseAverageValue"
      )
  }
  
  
  return(allData)
}

characteriationRiskFactorColDefs <- function(){
  result <- list(
    covariateName = reactable::colDef(
      name = "Covariate Name", 
      filterable = T
    ),
    covariateId = reactable::colDef(
      show = F
    ),
    nonCaseSumValue = reactable::colDef(
      name = "Number of non-cases with feature before exposure", 
      format = reactable::colFormat(digits = 2, percent = F),
      cell = function(value) {
        if(is.null(value)){return('< min threshold')}
        if(is.na(value)){return('< min threshold')}
        if (value != -1) value else '< min threshold'
      }
    ), 
    caseSumValue = reactable::colDef(
      name = "Number of cases with feature before exposure", 
      format = reactable::colFormat(digits = 2, percent = F),
      cell = function(value) {
        if(is.null(value)){return('< min threshold')}
        if(is.na(value)){return('< min threshold')}
        if (value != -1) value else '< min threshold'
      }
    ), 
    nonCaseAverageValue = reactable::colDef(
      name = "% of non-cases with feature before exposure", 
      format = reactable::colFormat(digits = 2, percent = T)
    ), 
    caseAverageValue = reactable::colDef(
      name = "% of cases with feature before exposure", 
      format = reactable::colFormat(digits = 2, percent = T)
    ), 
    
    SMD = reactable::colDef(
      name = "Standardized mean difference", 
      format = reactable::colFormat(digits = 2, percent = F)
    ), 
    
    absSMD = reactable::colDef(
      name = "Absolute standardized mean difference", 
      format = reactable::colFormat(digits = 2, percent = F)
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

