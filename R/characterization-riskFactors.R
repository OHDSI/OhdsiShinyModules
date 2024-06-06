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
      
      inputSelectionDfViewer(id = ns('inputSelected'), title = 'Selected'),
      
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
      
      # save the selections
      selected <- shiny::reactive({
        data.frame(
        database = names(options()$databaseIds)[which(input$databaseId == options()$databaseIds)],
        time_at_risk = names(options()$tarInds)[which(input$tarInd == options()$tarInds)]
      )})
      
      shiny::observeEvent(input$generate, {
        
        inputSelectionDfServer(
          id = 'inputSelected', 
          dataFrameRow = selected,
          ncol = 1
        )
        
        allData <- characterizationGetRiskFactorData(
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
  tar
){
  
  shiny::withProgress(message = 'Getting risk factor data', value = 0, {
    shiny::incProgress(1/4, detail = paste("Extracting ids"))
    
    sql <- "SELECT distinct setting_id
          from
          @schema.@c_table_prefixsettings
          where database_id = '@database_id'
          and risk_window_start = @risk_window_start
          and risk_window_end = @risk_window_end
          and start_anchor = '@start_anchor'
          and end_anchor = '@end_anchor'
          
          union
          
          SELECT distinct setting_id
          from
          @schema.@c_table_prefixsettings
          where database_id = '@database_id'
          and risk_window_start is NULL
  ;"
    
    ids <- connectionHandler$queryDb(
      sql = sql, 
      schema = resultDatabaseSettings$schema,
      c_table_prefix = resultDatabaseSettings$cTablePrefix,
      database_id = databaseId,
      risk_window_start = tar$riskWindowStart,
      start_anchor = tar$startAnchor,
      risk_window_end = tar$riskWindowEnd,
      end_anchor = tar$endAnchor
    )
    
    shiny::incProgress(2/4, detail = paste("Extracting binary"))
    
  sql <- "SELECT distinct cov.cohort_type, cr.covariate_name, 
  s.min_prior_observation, s.outcome_washout_days,
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
          
          where 
          cov.target_cohort_id = @target_id
          and cov.outcome_cohort_id in (0,@outcome_id)
          and cov.cohort_type in ('Target','TnO', 'TnOprior')
          and cov.database_id = '@database_id'
          and cov.setting_id in (@setting_ids)
          and cr.analysis_id not in (109, 110, 217, 218, 305, 417, 418, 505, 605, 713, 805, 926, 927)
  ;"

  binary <- connectionHandler$queryDb(
    sql = sql, 
    schema = resultDatabaseSettings$schema,
    c_table_prefix = resultDatabaseSettings$cTablePrefix,
    target_id = targetId,
    outcome_id = outcomeId,
    database_id = databaseId,
    setting_ids = paste0(ids$settingId, collapse=',')
  )
  message(paste0('Extracted ',nrow(binary),' binary RF rows'))
  
  # now process into table
  binary <- riskFactorTable(
    data = binary
  )
  
  shiny::incProgress(3/4, detail = paste("Extracting continuous"))

  sql <- "SELECT distinct cov.cohort_type, cr.covariate_name, 
   s.min_prior_observation, s.outcome_washout_days,cov.covariate_id, 
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
          and cov.outcome_cohort_id in (0,@outcome_id)
          and cov.cohort_type in ('Target','TnO', 'TnOprior')
          and cov.database_id = '@database_id'
          and cov.setting_id in (@setting_ids)
          and cr.analysis_id not in (109, 110, 217, 218, 305, 417, 418, 505, 605, 713, 805, 926, 927)
  ;"
  
  # TODO - how to remove prior outcomes??
  continuous <- connectionHandler$queryDb(
    sql = sql, 
    schema = resultDatabaseSettings$schema,
    c_table_prefix = resultDatabaseSettings$cTablePrefix,
    target_id = targetId,
    outcome_id = outcomeId,
    database_id = databaseId,
    setting_ids = paste0(ids$settingId, collapse=',')
  ) 
  
  message(paste0('Extracted ',nrow(binary),' continuous RF rows'))
  
  continuous <- riskFactorContinuousTable(
    data = continuous
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
  data
){
  
  if(is.null(data)){
    return(data)
  }
  
  data <- unique(data)
  if(nrow(data) == 0){
    return(data)
  }
  
  outcomeWashoutDays <- unique(data$outcomeWashoutDays)
  outcomeWashoutDays <- outcomeWashoutDays[!is.na(outcomeWashoutDays)]
  if(length(outcomeWashoutDays) == 0){
    shiny::showNotification('No cases')
    data <- data %>% 
      dplyr::filter(.data$cohortType == 'Target') %>%
      dplyr::select(-"cohortType", -"outcomeWashoutDays") %>% 
      dplyr::mutate(
        nonCaseSumValue = .data$sumValue,
        nonCaseAverageValue = .data$averageValue
      ) %>%
      dplyr::select(
        "covariateId","covariateName",
        'minPriorObservation',
        "nonCaseSumValue","nonCaseAverageValue"
      )
    return(data)
  }
  
  allData <- data %>% 
    dplyr::filter(.data$cohortType == 'Target') %>%
    dplyr::select(-"cohortType", -"outcomeWashoutDays")
  
  allcounts <- allData %>% 
    dplyr::mutate(N = .data$sumValue/.data$averageValue) %>%
    dplyr::select('minPriorObservation', 'N') %>%
    dplyr::group_by(.data$minPriorObservation) %>%
    dplyr::summarise(N = round(max(.data$N)))
  
  getN <- function(priorObs){
    sapply(priorObs, function(x){
      allcounts$N[allcounts$minPriorObservation == x]
    })
  }
  
  completeData <- c()
  for(outcomeWashoutDay in outcomeWashoutDays){
    
  caseData <- data %>% 
    dplyr::filter(
      .data$cohortType == 'TnO' &
      .data$outcomeWashoutDays == !!outcomeWashoutDay
        ) %>%
    dplyr::select(-"cohortType")
  
  casecounts <- caseData %>% 
    dplyr::mutate(N = .data$sumValue/.data$averageValue) %>%
    dplyr::select('minPriorObservation', 'N') %>%
    dplyr::group_by(.data$minPriorObservation) %>%
    dplyr::summarise(caseN = round(max(.data$N)))
  
  excludeData <- data %>% 
    dplyr::filter(
      .data$cohortType == 'TnOprior' & 
        .data$outcomeWashoutDays == !!outcomeWashoutDay
      ) %>%
    dplyr::select(-"cohortType")
  
  excludecounts <- excludeData %>% 
    dplyr::mutate(N = .data$sumValue/.data$averageValue) %>%
    dplyr::select('minPriorObservation', 'N') %>%
    dplyr::group_by(.data$minPriorObservation) %>%
    dplyr::summarise(exclude_N = round(max(.data$N, na.rm = T)))
  

  if(nrow(excludeData) > 0 ){
    colnamesInclude <- !colnames(excludeData) %in% c('covariateId', 'covariateName', 'minPriorObservation')
    colnames(excludeData)[colnamesInclude] <- paste0('exclude_',colnames(excludeData)[colnamesInclude])
    
    # if prior Os then exclude from T
    allData <- allData %>% 
      dplyr::left_join(
        excludeData, 
        by = c('covariateId', 'covariateName', 'minPriorObservation')) %>%
      dplyr::inner_join( # add N per washout/min obs
        allcounts, 
        by = c('minPriorObservation')
      )  %>%
      dplyr::inner_join( # add N per washout/min obs
        excludecounts, 
        by = c('minPriorObservation')
      ) %>%
      dplyr::mutate_if(is.numeric,dplyr::coalesce,0) %>%
      dplyr::mutate( # add exclude N per washout/min obs
        sumValue = .data$sumValue - .data$exclude_sumValue,
        averageValue = (.data$sumValue - .data$exclude_sumValue)/(.data$N-.data$exclude_N)
      ) %>%
      dplyr::mutate(
        N = .data$N-.data$exclude_N
      ) %>%
      dplyr::select("covariateId","covariateName","sumValue","averageValue", "N", 'minPriorObservation')
    
    allcounts <- allData %>% 
      dplyr::select('minPriorObservation', 'N') %>%
      dplyr::group_by(.data$minPriorObservation) %>%
      dplyr::summarise(N = round(max(.data$N)))
    
    getN <- function(priorObs){
      sapply(priorObs, function(x){
        allcounts$N[allcounts$minPriorObservation == x]
      })
    }
    
    }

  if(nrow(caseData) > 0){
    caseData <- caseData %>%
      dplyr::mutate(
      caseSumValue = .data$sumValue,
      caseAverageValue = .data$averageValue
    ) %>%
      dplyr::select("covariateId","covariateName","caseSumValue","caseAverageValue", 'minPriorObservation')
    
    # join with cases
    allData <- allData %>% 
      dplyr::full_join(caseData, 
                       by = c('covariateId', 'covariateName', 'minPriorObservation')) %>%
      dplyr::left_join(
        casecounts, 
        by = c('minPriorObservation')
      ) %>%
      dplyr::mutate(
        N = ifelse(
          is.na(.data$N),
          getN(.data$minPriorObservation),
          .data$N
        )
      ) %>%
      dplyr::mutate_if(is.numeric,dplyr::coalesce,0) %>%
      dplyr::mutate(
        nonCaseSumValue = ifelse(
          .data$sumValue > 0,
          .data$sumValue - .data$caseSumValue,
          0
          )
        ,
        nonCaseAverageValue = ifelse(
          .data$sumValue > 0,
          (.data$sumValue - .data$caseSumValue)/(.data$N-.data$caseN),
          0
          )
      ) %>%
      dplyr::mutate(
        nonCaseN = .data$N-.data$caseN
      ) %>%
      dplyr::select(
        "covariateId","covariateName",
        'minPriorObservation',
        "caseSumValue","caseAverageValue", 
        "nonCaseSumValue","nonCaseAverageValue"
        ,"nonCaseN", "caseN", "N"
        )
    

    # add abs smd
    allData <- allData %>% 
      dplyr::mutate(
        meanDiff = .data$caseAverageValue - .data$nonCaseAverageValue,
        std1 =  sqrt(((1-.data$caseAverageValue)^2*.data$caseSumValue + (-.data$caseAverageValue)^2*(.data$caseN - .data$caseSumValue))/.data$caseN),
        std2 =  sqrt(((1-.data$nonCaseAverageValue)^2*.data$nonCaseSumValue + (-.data$nonCaseAverageValue)^2*(.data$nonCaseN - .data$nonCaseSumValue))/.data$nonCaseN)
      ) %>% 
      dplyr::mutate(
        SMD = .data$meanDiff/sqrt((.data$std1^2 + .data$std2^2)/2),
        absSMD = abs(.data$meanDiff/sqrt((.data$std1^2 + .data$std2^2)/2))
      ) %>%
      dplyr::select(-"meanDiff",-"std1", -"std2", -"N",-"caseN", -"nonCaseN")
  

    # add outcomewashout back here
    allData <- allData %>% 
      dplyr::mutate(
        outcomeWashoutDays = !!outcomeWashoutDay
      ) %>%
      dplyr::relocate(.data$outcomeWashoutDays, 
                      .after = .data$minPriorObservation)
    
    completeData <- rbind(allData, completeData)

  } 
  
  } # end outcomeWashoutDays loop
  
  if(nrow(completeData) == 0){
    completeData <- allData %>% 
      dplyr::mutate(
        nonCaseSumValue = .data$sumValue,
        nonCaseAverageValue = .data$averageValue
      ) %>%
      dplyr::select(
        "covariateId","covariateName",
        'minPriorObservation',
        "nonCaseSumValue","nonCaseAverageValue"
      )
  }
  

  return(unique(completeData))
}


riskFactorContinuousTable <- function(
  data
){
  

  data <- unique(data)
  
  caseData <- data %>% 
    dplyr::filter(.data$cohortType == 'TnO') %>%
    dplyr::select(-"cohortType")
  
  allData <- data %>% 
    dplyr::filter(.data$cohortType == 'Target') %>%
    dplyr::select(-"cohortType", -"outcomeWashoutDays")
  
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
      dplyr::full_join(caseData, by = c('covariateId', 'covariateName', 'minPriorObservation')) %>%
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
    covariateId = reactable::colDef(
      show = F
    ),
    covariateName = reactable::colDef(
      header = withTooltip("Covariate Name",
                           "Name of the covariate"),
      filterable = T,
      minWidth = 300
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
    nonCaseSumValue = reactable::colDef(
      header = withTooltip("# Non-cases with Feature Before Exposure",
                           "Number of non-cases for the outcome with the feature before exposure"),
      filterable = T, 
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
      header = withTooltip("# Cases with Feature Before Exposure",
                           "Number of cases for the outcome with the feature before exposure"),
      filterable = T, 
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
      header = withTooltip("% Non-cases with Feature Before Exposure",
                           "Percent of non-cases for the outcome with the feature before exposure"),
      filterable = T, 
      format = reactable::colFormat(digits = 2, percent = T)
    ), 
    caseAverageValue = reactable::colDef(
      header = withTooltip("% Cases with Feature Before Exposure",
                           "Percent of Cases for the outcome with the feature before exposure"),
      filterable = T, 
      format = reactable::colFormat(digits = 2, percent = T)
    ), 
    
    SMD = reactable::colDef(
      header = withTooltip("SMD",
                           "Standardized mean difference"),
      filterable = T, 
      format = reactable::colFormat(digits = 2, percent = F)
    ), 
    
    absSMD = reactable::colDef(
      header = withTooltip("absSMD",
                           "Absolute value of standardized mean difference"),
      filterable = T, 
      format = reactable::colFormat(digits = 2, percent = F)
    )
  )
  return(result)
}



characteriationRiskFactorContColDefs <- function(){
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
      filterable = T
    ), 
    outcomeWashoutDays = reactable::colDef(
      header = withTooltip("Outcome Washout Days",
                           "Number of days for the outcome washout"),
      filterable = T
    ),
    targetCountValue = reactable::colDef(
        header = withTooltip("# of Target with Feature",
                             "Number of the target population with feature"),
        filterable = T
      , 
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
      header = withTooltip("# of Cases with Feature",
                           "Number of the cases in the target population with feature"), 
      filterable = T,
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
      header = withTooltip("Target Mean Feature Value",
                           "Mean value of the feature in the target population"), 
      filterable = T,
      format = reactable::colFormat(digits = 2, percent = F)
    ), 
    caseAverageValue = reactable::colDef(
      header = withTooltip("Cases Mean Feature Value",
                           "Mean value of the feature in the cases"), 
      filterable = T,
      format = reactable::colFormat(digits = 2, percent = F)
    ), 
    
    targetStandardDeviation = reactable::colDef(
      header = withTooltip("Target SD Feature Value",
                           "Standard deviation of the feature value in the target population"), 
      filterable = T,
      format = reactable::colFormat(digits = 2, percent = F)
    ), 
    caseStandardDeviation  = reactable::colDef(
      header = withTooltip("Cases SD Feature Value",
                           "Standard deviation of the feature value in the cases"), 
      filterable = T, 
      format = reactable::colFormat(digits = 2, percent = F)
    ), 
    caseMedianValue  = reactable::colDef(
      header = withTooltip("Cases Median Feature Value",
                           "Median of the feature value in the cases"), 
      filterable = T, 
      format = reactable::colFormat(digits = 2, percent = F)
    ),
    caseP10Value  = reactable::colDef(
      header = withTooltip("Cases 10th %ile Feature Value",
                           "10th percentile of the feature value in the cases"), 
      filterable = T, 
      format = reactable::colFormat(digits = 2, percent = F)
    ),
    caseP25Value  = reactable::colDef(
      header = withTooltip("Cases 25th %tile Feature Value",
                           "25th percentile of the feature value in the cases"), 
      filterable = T, 
      format = reactable::colFormat(digits = 2, percent = F)
    ),
    caseP75Value  = reactable::colDef(
      header = withTooltip("Cases 75th %tile Feature Value",
                           "75th percentile of the feature value in the cases"), 
      filterable = T, 
      format = reactable::colFormat(digits = 2, percent = F)
    ),
    caseP90Value  = reactable::colDef(
      header = withTooltip("Cases 90th %tile Feature Value",
                           "90th percentile of the feature value in the cases"), 
      filterable = T, 
      format = reactable::colFormat(digits = 2, percent = F)
    ),
    caseMaxValue  = reactable::colDef(
      header = withTooltip("Cases Max Feature Value",
                           "Maximum of the feature value in the cases"), 
      filterable = T, 
      format = reactable::colFormat(digits = 2, percent = F)
    ),
    caseMinValue  = reactable::colDef(
      header = withTooltip("Cases Min Feature Value",
                           "Minimum of the feature value in the cases"), 
      filterable = T, 
      format = reactable::colFormat(digits = 2, percent = F)
    ),
    targetMedianValue  = reactable::colDef(
      header = withTooltip("Target Median Feature Value",
                           "Median of the feature value in the target population"), 
      filterable = T, 
      format = reactable::colFormat(digits = 2, percent = F)
    ),
    targetP10Value  = reactable::colDef(
      header = withTooltip("Target 10th %ile Feature Value",
                           "10th percentile of the feature value in the target population"), 
      filterable = T, 
      format = reactable::colFormat(digits = 2, percent = F)
    ),
    targetP25Value  = reactable::colDef(
      header = withTooltip("Target 25th %tile Feature Value",
                           "25th percentile of the feature value in the target population"), 
      filterable = T, 
      format = reactable::colFormat(digits = 2, percent = F)
    ),
    targetP75Value  = reactable::colDef(
      header = withTooltip("Target 75th %tile Feature Value",
                           "75th percentile of the feature value in the target population"), 
      filterable = T, 
      format = reactable::colFormat(digits = 2, percent = F)
    ),
    targetP90Value  = reactable::colDef(
      header = withTooltip("Target 90th %tile Feature Value",
                           "90th percentile of the feature value in the target population"), 
      filterable = T, 
      format = reactable::colFormat(digits = 2, percent = F)
    ),
    targetMaxValue  = reactable::colDef(
      header = withTooltip("Target Max Feature Value",
                           "Maximum of the feature value in the target population"), 
      filterable = T, 
      format = reactable::colFormat(digits = 2, percent = F)
    ),
    targetMinValue  = reactable::colDef(
      header = withTooltip("Target Min Feature Value",
                           "Minimum of the feature value in the target population"), 
      filterable = T, 
      format = reactable::colFormat(digits = 2, percent = F)
    ),
    targetBoxPlot = reactable::colDef(
      show = F
    ),
    caseBoxPlot = reactable::colDef(
      show = F
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
      header = withTooltip("SMD",
                           "Standardized mean difference"), 
      filterable = T, 
      format = reactable::colFormat(digits = 2, percent = F)
    ), 
    absSMD = reactable::colDef(
      header = withTooltip("absSMD",
                           "Absolute value of the standardized mean difference"), 
      filterable = T,  
      format = reactable::colFormat(digits = 2, percent = F)
    )
  )
  return(result)
}

