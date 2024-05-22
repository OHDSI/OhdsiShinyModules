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
          colDefsInput = characteriationRiskFactorContColDefs(), # function below
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
    
    sql <- "SELECT distinct cd.cohort_definition_id, cd.cohort_type, cc.person_count as N
          from
          @schema.@c_table_prefixcohort_details cd
          left join 
          @schema.@c_table_prefixcohort_counts cc
          on cd.run_id = cc.run_id
          and cd.database_id = cc.database_id
          and cd.cohort_definition_id = cc.cohort_definition_id
          
          where cd.target_cohort_id = @target_id
          and cd.outcome_cohort_id in (0, @outcome_id)
          and cd.database_id = '@database_id'
          and cd.time_at_risk_id in (0, @time_at_risk_id)
          and cd.cohort_type in ('T','TnO', 'TnOprior')
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
    
  sql <- "SELECT distinct cov.cohort_definition_id, cr.covariate_name, 
  s.min_prior_observation, s.outcome_washout_days,
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

  sql <- "SELECT distinct cov.cohort_definition_id, cr.covariate_name, 
   s.min_prior_observation, s.outcome_washout_days,cov.covariate_id, 
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
  
  continuous <- riskFactorContinuousTable(
    data = continuous,
    ids = ids
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
  
  data <- unique(data)

  caseId <- ids$cohortDefinitionId[ids$cohortType == 'TnO']
  if(length(caseId ) == 0){
    caseId <- -1
  }
  caseData <- data %>% 
    dplyr::filter(.data$cohortDefinitionId == !!caseId) %>%
    dplyr::select(-"cohortDefinitionId")
  #write.csv(caseData, '/Users/jreps/Documents/caseData.csv')
  
  allId <- ids$cohortDefinitionId[ids$cohortType == 'T']
  allData <- data %>% 
    dplyr::filter(.data$cohortDefinitionId == !!allId) %>%
    dplyr::select(-"cohortDefinitionId")
  allData$N <- allData$sumValue[1]/allData$averageValue[1]
  
  #write.csv(allData, '/Users/jreps/Documents/allData.csv')
  
  excludeId <- ids$cohortDefinitionId[ids$cohortType == 'TnOprior']
  if(length(excludeId) == 0){
    excludeId <- -1
  }
  excludeData <- data %>% 
    dplyr::filter(.data$cohortDefinitionId == !!excludeId) %>%
    dplyr::select(-"cohortDefinitionId")
  
  if(nrow(excludeData) > 0 ){
    excludeData$N <- excludeData$sumValue[1]/excludeData$averageValue[1]
    excludeN <- excludeData$sumValue[1]/excludeData$averageValue[1]
    colnamesInclude <- !colnames(excludeData) %in% c('covariateId', 'covariateName', 'minPriorObservation', 'outcomeWashoutDays')
    colnames(excludeData)[colnamesInclude] <- paste0('exclude_',colnames(excludeData)[colnamesInclude])
    
    # if prior Os then exclude from T
    allData <- allData %>% 
      dplyr::full_join(excludeData, by = c('covariateId', 'covariateName', 'minPriorObservation', 'outcomeWashoutDays')) %>%
      dplyr::mutate(
        sumValue = .data$sumValue - .data$exclude_sumValue,
        averageValue = (.data$sumValue - .data$exclude_sumValue)/(.data$N-!!excludeN)
      ) %>%
      dplyr::mutate(
        N = .data$N-!!excludeN
      ) %>%
      dplyr::select("covariateId","covariateName","sumValue","averageValue", "N", 'minPriorObservation', 'outcomeWashoutDays')
    
    }
  
  if(nrow(caseData) > 0){
    caseData$caseN <- caseData$sumValue[1]/caseData$averageValue[1]
    caseData <- caseData %>%
      dplyr::mutate(
      caseSumValue = .data$sumValue,
      caseAverageValue = .data$averageValue
    ) %>%
      dplyr::select("covariateId","covariateName","caseSumValue","caseAverageValue", "caseN", 'minPriorObservation', 'outcomeWashoutDays')
    
    # join with cases
    allData <- allData %>% 
      dplyr::full_join(caseData, by = c('covariateId', 'covariateName', 'minPriorObservation', 'outcomeWashoutDays')) %>%
      dplyr::mutate_if(is.numeric,dplyr::coalesce,0) %>%
      dplyr::mutate(
        nonCaseSumValue = .data$sumValue - .data$caseSumValue,
        nonCaseAverageValue = (.data$sumValue - .data$caseSumValue)/(.data$N-.data$caseN)
      ) %>%
      dplyr::mutate(
        nonCaseN = .data$N-.data$caseN
      ) %>%
      dplyr::select(
        "covariateId","covariateName",
        'minPriorObservation', 'outcomeWashoutDays',
        "caseSumValue","caseAverageValue", 
        "nonCaseSumValue","nonCaseAverageValue",
        "N","caseN"
        )
    
    # add abs smd
    allData <- allData %>% 
      dplyr::mutate(
        meanDiff = .data$caseAverageValue - .data$nonCaseAverageValue,
        std1 =  sqrt(((1-.data$caseAverageValue)^2*.data$caseSumValue + (-.data$caseAverageValue)^2*(.data$caseN - .data$caseSumValue))/.data$caseN),
        std2 =  sqrt(((1-.data$nonCaseAverageValue)^2*.data$nonCaseSumValue + (-.data$nonCaseAverageValue)^2*(.data$N - .data$nonCaseSumValue))/.data$N)
      ) %>% 
      dplyr::mutate(
        SMD = .data$meanDiff/sqrt((.data$std1^2 + .data$std2^2)/2),
        absSMD = abs(.data$meanDiff/sqrt((.data$std1^2 + .data$std2^2)/2))
      ) %>%
      dplyr::select(-"meanDiff",-"std1", -"std2",-"N",-"caseN")
  
    #write.csv(allData, '/Users/jreps/Documents/finalData.csv')
    
    
  } else{
    allData <- allData %>% 
      dplyr::mutate(
        nonCaseSumValue = .data$sumValue,
        nonCaseAverageValue = .data$averageValue
      ) %>%
      dplyr::select(
        "covariateId","covariateName",
        'minPriorObservation', 'outcomeWashoutDays',
        "nonCaseSumValue","nonCaseAverageValue"
      )
  }
  
  
  return(unique(allData))
}


riskFactorContinuousTable <- function(
  data,
  ids # have N in them
){
  
  data <- unique(data)
  
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
  
  if(nrow(caseData) > 0){

    caseData <- caseData %>%
      dplyr::mutate(
        caseCountValue = .data$countValue,
        caseAverageValue = .data$averageValue,
        caseStandardDeviation = .data$standardDeviation,
        caseMedianValue = .data$medianValue,
        caseMinValue = .data$minValue,
        caseMaxValue = .data$maxValue,
        caseP10Value = .data$p10Value,
        caseP25Value = .data$p25Value,
        caseP75Value = .data$p75Value,
        caseP90Value = .data$p90Value
      ) %>%
      dplyr::select("covariateId","covariateName",
                    'minPriorObservation', 'outcomeWashoutDays',
                    "caseCountValue","caseAverageValue", 
                    "caseStandardDeviation", "caseMedianValue", "caseP10Value", "caseP25Value",
                    "caseP75Value", "caseP90Value", "caseMaxValue", "caseMinValue")
    
    # join with cases
    allData <- allData %>% 
      dplyr::full_join(caseData, by = c('covariateId', 'covariateName', 'minPriorObservation', 'outcomeWashoutDays')) %>%
      dplyr::mutate(
        targetCountValue = .data$countValue,
        targetAverageValue = .data$averageValue,
        targetStandardDeviation = .data$standardDeviation,
        targetMedianValue = .data$medianValue,
        targetMinValue = .data$minValue,
        targetMaxValue = .data$maxValue,
        targetP10Value = .data$p10Value,
        targetP25Value = .data$p25Value,
        targetP75Value = .data$p75Value,
        targetP90Value = .data$p90Value
      )  %>%
      dplyr::select("covariateId","covariateName",
                    'minPriorObservation', 'outcomeWashoutDays',
                    "caseCountValue","caseAverageValue",
                    "caseStandardDeviation", "caseMedianValue", "caseP10Value", "caseP25Value",
                    "caseP75Value", "caseP90Value", "caseMaxValue", "caseMinValue",
                    
                    "targetCountValue","targetAverageValue",
                    "targetStandardDeviation", "targetMedianValue", "targetP10Value", "targetP25Value",
                    "targetP75Value", "targetP90Value","targetMaxValue", "targetMinValue",)
    
    # add abs smd
    allData <- allData %>% 
      dplyr::mutate(
        SMD = (.data$caseAverageValue - .data$targetAverageValue)/sqrt((.data$caseStandardDeviation^2 + .data$targetStandardDeviation^2)/2),
        absSMD = abs((.data$caseAverageValue - .data$targetAverageValue)/sqrt((.data$caseStandardDeviation^2 + .data$targetStandardDeviation^2)/2)),
        targetBoxPlot = 0,
        caseBoxPlot = 0
      ) 
    
    
  }
  
  
  return(unique(allData))
  
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
    minPriorObservation = reactable::colDef(
      name = "Minimum prior observation"
      ), 
    outcomeWashoutDays = reactable::colDef(
      name = "outcome washout days"
    ),
    nonCaseSumValue = reactable::colDef(
      name = "Number of non-cases with feature before exposure", 
      format = reactable::colFormat(
        percent = F,
        separators = TRUE
        ),
      cell = function(value) {
        if(is.null(value)){return('< min threshold')}
        if(is.na(value)){return('< min threshold')}
        if (value != -1) value else '< min threshold'
      }
    ), 
    caseSumValue = reactable::colDef(
      name = "Number of cases with feature before exposure", 
      format = reactable::colFormat(
        separators = TRUE, 
        percent = F
        ),
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
    )
  )
  return(result)
}



characteriationRiskFactorContColDefs <- function(){
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
    targetCountValue = reactable::colDef(
      name = "Number of target population with feature", 
      format = reactable::colFormat(
        percent = F,
        separators = TRUE
      ),
      cell = function(value) {
        if(is.null(value)){return('< min threshold')}
        if(is.na(value)){return('< min threshold')}
        if (value != -1) value else '< min threshold'
      }
    ), 
    caseCountValue = reactable::colDef(
      name = "Number of cases with feature", 
      format = reactable::colFormat(
        separators = TRUE, 
        percent = F
      ),
      cell = function(value) {
        if(is.null(value)){return('< min threshold')}
        if(is.na(value)){return('< min threshold')}
        if (value != -1) value else '< min threshold'
      }
    ), 
    targetAverageValue = reactable::colDef(
      name = "Target mean feature value", 
      format = reactable::colFormat(digits = 2, percent = F)
    ), 
    caseAverageValue = reactable::colDef(
      name = "Cases mean feature value", 
      format = reactable::colFormat(digits = 2, percent = F)
    ), 
    
    targetStandardDeviation = reactable::colDef(
      name = "Target standard deviation", 
      format = reactable::colFormat(digits = 2, percent = F)
    ), 
    caseStandardDeviation  = reactable::colDef(
      name = "Cases standard deviation", 
      format = reactable::colFormat(digits = 2, percent = F)
    ), 
    
    #targetBoxPlot = reactable::colDef(cell = function(value, index) {
    #  ggplot2::ggplot() +
    #        ggplot2::geom_boxplot(
    #              ggplot2::aes(
    #                x = 1, 
    #                ymin = data$targetMinValue[index], 
    #                lower = data$targetP10Value[index], 
    #                middle = data$targetMedianValue[index], 
    #                upper = data$targetP90Value[index], 
    #                ymax = data$targetMaxValue[index]
    #                ),
    #              stat = "identity"
    #          )
    #}), 
    #caseBoxPlot  = reactable::colDef(cell = function(value, index) {
    #    sparkline(vcs_boxp_data$em_red_per_th[[index]], type = "box")
    #  }), 
    #caseBoxPlot  = reactable::colDef(cell = function(value, index) {
    #  sparkline::sparkline(vcs_boxp_data$em_red_per_th[[index]], type = "box")
    #  }), 
    
    # low_outlier, low_whisker, q1, median, q3, high_whisker, high_outlier
    #sparkline::spk_chr(c(data$targetMinValue[index], data$targetP10Value[index], data$targetP25Value[index], data$targetMedianValue[index], 3, 6, 6), type="box", raw = TRUE, width = 200)
    
    SMD = reactable::colDef(
      name = "SMD", 
      format = reactable::colFormat(digits = 2, percent = F)
    ), 
    
    absSMD = reactable::colDef(
      name = "Absolute SMD", 
      format = reactable::colFormat(digits = 2, percent = F)
    )
  )
  return(result)
}

