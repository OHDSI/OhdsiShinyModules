# @file characterization-aggregateFeatures.R
#
# Copyright 2025 Observational Health Data Sciences and Informatics
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
      condition = 'output.showRiskFactors != 0',
      ns = ns,
      
      inputSelectionDfViewer(id = ns('inputSelected'), title = 'Selected'),
      
      shinydashboard::box(
        title = 'Counts', 
        width = "100%", 
        collapsible = T, 
        resultTableViewer(ns('countTable'))
      ),
      
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
    reactiveTargetRow,
    reactiveOutcomeRow,
    reactiveOutcomeTar
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      output$showRiskFactors <- shiny::reactive(0)
      shiny::outputOptions(output, "showRiskFactors", suspendWhenHidden = FALSE)
      
      # if target or outcome changes hide results
      shiny::observeEvent(reactiveTargetRow(), {
        output$showRiskFactors <- shiny::reactive(0)
      })
      shiny::observeEvent(reactiveOutcomeRow(), {
        output$showRiskFactors <- shiny::reactive(0)
      })
      
      # get databases
      databaseNames <- shiny::reactive(unlist(strsplit(x = reactiveTargetRow()$databaseString, split = ', ')))
      databaseIds <- shiny::reactive(unlist(strsplit(x = reactiveTargetRow()$databaseIdString, split = ', ')))
        
      output$inputs <- shiny::renderUI({ # need to make reactive?
        
        shiny::div(
          shiny::selectInput(
            inputId = session$ns('databaseName'),
            label = 'Database: ',
            choices = databaseNames(),
            selected = databaseNames()[1],
            multiple = F
          ),
          
          shiny::selectInput(
            inputId = session$ns('tarInd'),
            label = 'Time-at-risk: ',
            choices = reactiveOutcomeTar()$tarInds,
            selected = reactiveOutcomeTar()$tarInds[1],
            multiple = F
          ),
          
          shiny::actionButton(
            inputId = session$ns('generate'), 
            label = 'Generate'
          )
        )
        
      })
      
      # save the selections
      selected <- shiny::reactiveVal(value = NULL)
      
      shiny::observeEvent(input$generate, {
        
        # add target, outcome, database and tar check
        
        if(is.null(reactiveTargetRow()) | is.null(reactiveOutcomeRow()) |
           is.null(reactiveOutcomeTar()$tarList[[1]]) | is.null(input$databaseName)){
          
          output$showRiskFactors <- shiny::reactive(0)
          shiny::showNotification('Need to set all inputs')
        } else{
          
          if(nrow(reactiveTargetRow()) == 0 | nrow(reactiveOutcomeRow()) == 0){
            output$showRiskFactors <- shiny::reactive(0)
            shiny::showNotification('Need to pick a target and outcome')
          } else{
            output$showRiskFactors <- shiny::reactive(1)
            
            selected(
              data.frame( #TODO add outcome and target here
                database = input$databaseName,
                time_at_risk = names(reactiveOutcomeTar()$tarInds)[which(input$tarInd == reactiveOutcomeTar()$tarInds)]
              )
            )
            
            inputSelectionDfServer(
              id = 'inputSelected', 
              dataFrameRow = selected,
              ncol = 1
            )
            
            counts <- characterizationGetRiskFactorCounts(
              connectionHandler = connectionHandler,
              resultDatabaseSettings = resultDatabaseSettings,
              targetId = reactiveTargetRow()$cohortId,
              outcomeId = reactiveOutcomeRow()$cohortId,
              databaseId = databaseIds()[input$databaseName == databaseNames()],
              tar = reactiveOutcomeTar()$tarList[[which(reactiveOutcomeTar()$tarInds == input$tarInd)]]
            )
            
            countTableOutput <- resultTableServer(
              id = "countTable", 
              df = counts,
              details = data.frame(
                target = reactiveTargetRow()$cohortName,
                outcome = reactiveOutcomeRow()$cohortName,
                Database = input$databaseName,
                TimeAtRisk = reactiveOutcomeTar()$tarList[[which(reactiveOutcomeTar()$tarInds == input$tarInd)]],
                Analysis = 'Counts - Risk Factor'
              ),
              downloadedFileName = 'risk_factor_counts',
              colDefsInput = characteriationCountsColDefs(
                elementId = session$ns('count-table-filter')
              ),
              addActions = NULL,
              elementId = session$ns('count-table-filter')
            )
            
            allData <- characterizationGetRiskFactorData(
              connectionHandler = connectionHandler,
              resultDatabaseSettings = resultDatabaseSettings,
              targetId = reactiveTargetRow()$cohortId,
              outcomeId = reactiveOutcomeRow()$cohortId,
              databaseId = databaseIds()[input$databaseName == databaseNames()],
              tar = reactiveOutcomeTar()$tarList[[which(reactiveOutcomeTar()$tarInds == input$tarInd)]]
            )
            
            binTableOutputs <- resultTableServer(
              id = "binaryTable", 
              df = allData$binary,
              details = data.frame(
                target = reactiveTargetRow()$cohortName,
                outcome = reactiveOutcomeRow()$cohortName,
                Database = input$databaseName,
                TimeAtRisk = reactiveOutcomeTar()$tarList[[which(reactiveOutcomeTar()$tarInds == input$tarInd)]],
                Analysis = 'Exposed Cases Summary - Risk Factor'
              ),
              downloadedFileName = 'risk_factor_binary',
              colDefsInput = characteriationRiskFactorColDefs(
                elementId = session$ns('binary-table-filter')
              ), # function below
              addActions = NULL,
              elementId = session$ns('binary-table-filter')
            )
            
            conTableOutputs <- resultTableServer(
              id = "continuousTable", 
              df = allData$continuous,
              colDefsInput = characteriationRiskFactorContColDefs(
                elementId = session$ns('continuous-table-filter')
              ), # function below
              addActions = NULL,
              elementId = session$ns('continuous-table-filter')
            )
            
          }
        }
        
      })
   
  return(invisible(NULL))
    }
  )
}


characterizationGetRiskFactorCounts <- function(
    connectionHandler,
    resultDatabaseSettings,
    targetId,
    outcomeId,
    databaseId,
    tar
){
  
  sql <- "SELECT 
          cohort_type,
          min_prior_observation,	
          outcome_washout_days,
          row_count,
          person_count 
          
          from
          @schema.@c_table_prefixcohort_counts
          where database_id = '@database_id'
          and target_cohort_id = @target_id
          and outcome_cohort_id in (@outcome_id, 0)
          and (risk_window_start = @risk_window_start OR risk_window_start is NULL)
          and (risk_window_end = @risk_window_end OR risk_window_end is NULL)
          and (start_anchor = '@start_anchor' OR start_anchor is NULL)
          and (end_anchor = '@end_anchor' OR end_anchor is NULL)
          and cohort_type in ('Cases','Exclude','Target')
  ;"
  
  counts <- connectionHandler$queryDb(
    sql = sql, 
    schema = resultDatabaseSettings$schema,
    c_table_prefix = resultDatabaseSettings$cTablePrefix,
    database_id = databaseId,
    target_id = targetId,
    outcome_id = outcomeId,
    risk_window_start = tar$riskWindowStart,
    start_anchor = tar$startAnchor,
    risk_window_end = tar$riskWindowEnd,
    end_anchor = tar$endAnchor
  )
  
  return(counts)
  
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
          and cov.cohort_type in ('Target','Cases', 'Exclude')
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
    setting_ids = paste0(paste0("'",ids$settingId, "'"), collapse=',')
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
          and cov.cohort_type in ('Target','Cases', 'Exclude')
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
    setting_ids = paste0(paste0("'",ids$settingId, "'"), collapse=',')
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
  
  targetData <- data %>% 
    dplyr::filter(.data$cohortType == 'Target') %>%
    dplyr::mutate(
      sumValue_Target = .data$sumValue,
      averageValue_Target = .data$averageValue
    ) %>%    
    dplyr::select(
      -"cohortType", 
      -"outcomeWashoutDays",
      -"sumValue",
      -"averageValue"
      ) 
  
  targetN <- targetData %>% 
    dplyr::mutate(N_Target = .data$sumValue_Target/.data$averageValue_Target) %>%
    dplyr::select('minPriorObservation', 'N_Target') %>%
    dplyr::group_by(.data$minPriorObservation) %>%
    dplyr::summarise(N_Target = round(max(.data$N_Target, na.rm = T)))
  
  completeData <- c()
  for(outcomeWashoutDay in outcomeWashoutDays){
    
    # add dummy Cases and Exclude to data so columns always exist
    data <- rbind(data, data.frame(
      cohortType = c('Cases','Exclude'),
      covariateName = rep('NA', 2),
      minPriorObservation = rep(unique(data$minPriorObservation)[1], 2),
      outcomeWashoutDays = rep(outcomeWashoutDay, 2),
      covariateId = rep(-1, 2),
      sumValue = rep(0,2),
      averageValue = rep(0,2)
    )[colnames(data)])
    
    #filter data to outcomeWashoutDays
    otherData <- data %>%
      dplyr::filter(
        .data$cohortType != 'Target' &
        .data$outcomeWashoutDays == !!outcomeWashoutDay 
        ) %>%
      tidyr::pivot_wider(
        id_cols = c(
          "minPriorObservation",
          "covariateId",
          "covariateName"
        ),
        names_from = "cohortType",
        values_from = c("sumValue","averageValue")
      ) 
    
    otherN <- otherData %>%
      dplyr::mutate(
        N_Cases = .data$sumValue_Cases/.data$averageValue_Cases,
        N_Exclude = .data$sumValue_Exclude/.data$averageValue_Exclude
      ) %>%
      dplyr::group_by(.data$minPriorObservation) %>%
      dplyr::summarise(
        N_Cases = max(.data$N_Cases, na.rm = T),
        N_Exclude = max(.data$N_Exclude, na.rm = T)
      )

    if(length(is.infinite(otherN$N_Cases))>0){
      otherN$N_Cases[is.infinite(otherN$N_Cases)] <- 0
    }
    if(length(is.infinite(otherN$N_Exclude))>0){
      otherN$N_Exclude[is.infinite(otherN$N_Exclude)] <- 0
    }
    
    # get all counts
    counts <- targetN %>%
      dplyr::left_join(otherN, by = c('minPriorObservation'))
    
    # get final data for minPriorObs
    finalData <- targetData %>%
      dplyr::left_join(
        otherData,
        by = c(
          "minPriorObservation",
          "covariateId",
          "covariateName"
        )
      ) %>%
      dplyr::inner_join(
        counts ,
        by = c(
          "minPriorObservation"
        )
      )
    if(length(is.na(finalData$sumValue_Cases))>0){
      finalData$sumValue_Cases[is.na(finalData$sumValue_Cases)] <- 0
    }
    if(length(is.na(finalData$sumValue_Target))>0){
      finalData$sumValue_Target[is.na(finalData$sumValue_Target)] <- 0
    }
    if(length(is.na(finalData$sumValue_Exclude))>0){
      finalData$sumValue_Exclude[is.na(finalData$sumValue_Exclude)] <- 0
    }
    if(length(is.na(finalData$N_Target))>0){
      finalData$N_Target[is.na(finalData$N_Target)] <- 0
    }
    if(length(is.na(finalData$N_Cases))>0){
      finalData$N_Cases[is.na(finalData$N_Cases)] <- 0
    }
    if(length(is.na(finalData$N_Exclude))>0){
      finalData$N_Exclude[is.na(finalData$N_Exclude)] <- 0
    }
    if(length(is.na(finalData$averageValue_Cases))>0){
      finalData$averageValue_Cases[is.na(finalData$averageValue_Cases)] <- 0
    }
    
    # removing censored counts as dont want to add due to negative
    if(length(finalData$N_Exclude < 0) > 0 ){
      finalData$N_Exclude[finalData$N_Exclude < 0] <- 0 
    }
    finalData$N_Cases_exclude <- finalData$N_Cases
    if(length(finalData$N_Cases_exclude < 0) > 0 ){
      finalData$N_Cases_exclude[finalData$N_Cases_exclude < 0] <- 0 
    }
    if(length(finalData$sumValue_Exclude < 0) > 0 ){
      finalData$sumValue_Exclude[finalData$sumValue_Exclude < 0] <- 0 
    }
    finalData$sumValue_Cases_exclude <- finalData$sumValue_Cases
    if(length(finalData$sumValue_Cases_exclude < 0) > 0 ){
      finalData$sumValue_Cases_exclude[finalData$sumValue_Cases_exclude < 0] <- 0 
    }
    
    finalData <- finalData %>%
      dplyr::mutate(
        nonCaseN = round(.data$N_Target-.data$N_Exclude-.data$N_Cases_exclude),
        caseN = .data$N_Cases,
        N = .data$N_Target,
        nonCaseSumValue = .data$sumValue_Target-.data$sumValue_Exclude-.data$sumValue_Cases_exclude,
        caseSumValue = .data$sumValue_Cases, 
        nonCaseAverageValue = (.data$sumValue_Target-.data$sumValue_Exclude-.data$sumValue_Cases_exclude)/(.data$N_Target-.data$N_Exclude-.data$N_Cases_exclude),
        caseAverageValue = .data$averageValue_Cases
      ) %>%
      dplyr::select(
        "covariateId", "covariateName", "minPriorObservation",
        "caseSumValue","caseAverageValue", 
        "nonCaseSumValue","nonCaseAverageValue",
        "nonCaseN", "caseN", "N"
      ) %>% 
      dplyr::mutate(
        meanDiff = .data$caseAverageValue - .data$nonCaseAverageValue,
        std1 =  ifelse(.data$caseN == 0, 0 ,sqrt(((1-.data$caseAverageValue)^2*.data$caseSumValue + (-.data$caseAverageValue)^2*(.data$caseN - .data$caseSumValue))/.data$caseN)),
        std2 =  ifelse(.data$nonCaseN == 0, 0, sqrt(((1-.data$nonCaseAverageValue)^2*.data$nonCaseSumValue + (-.data$nonCaseAverageValue)^2*(.data$nonCaseN - .data$nonCaseSumValue))/.data$nonCaseN))
      ) %>% 
      dplyr::mutate(
        SMD = .data$meanDiff/sqrt((.data$std1^2 + .data$std2^2)/2),
        absSMD = abs(.data$meanDiff/sqrt((.data$std1^2 + .data$std2^2)/2))
      ) %>%
      dplyr::select(-"meanDiff",-"std1", -"std2", -"N",-"caseN", -"nonCaseN")
  

    # add outcomewashout back here
    finalData <- finalData %>% 
      dplyr::mutate(
        outcomeWashoutDays = !!outcomeWashoutDay
      ) %>%
      dplyr::relocate("outcomeWashoutDays", 
                      .after = "minPriorObservation")
    
    completeData <- rbind(finalData, completeData)

  } # end outcomeWashoutDays loop
  
  if(nrow(completeData) == 0){
    completeData <- data %>% 
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
  }
  
  return(unique(completeData))
}


riskFactorContinuousTable <- function(
  data
){
  

  data <- unique(data)
  
  caseData <- data %>% 
    dplyr::filter(.data$cohortType == 'Cases') %>%
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

characteriationCountsColDefs <- function(
    elementId
){
  result <- list(
    cohortType = reactable::colDef(
      header = withTooltip("Cohort Type",
                           "The target popualtion, exclusions from target or cases"),
      filterable = T
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
    
    rowCount = reactable::colDef(
      header = withTooltip("# Rows",
                           "Number of exposures in the cohort (people can be in more than once)"),
      cell = function(value) {
        if(is.null(value)){return('< min threshold')}
        if(is.na(value)){return('< min threshold')}
        if (value >= 0) value else paste0('< ', abs(value))
      }
    ), 
    personCount = reactable::colDef(
      header = withTooltip("# Persons",
                           "Number of distinct people in the cohort"),
      cell = function(value) {
        if(is.null(value)){return('< min threshold')}
        if(is.na(value)){return('< min threshold')}
        if (value >= 0) value else paste0('< ', abs(value))
      }
    )
  )
  return(result)
}

characteriationRiskFactorColDefs <- function(
    elementId
    ){
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
        if (value >= 0) value else paste0('< ', abs(value))
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
        if (value >= 0) value else paste0('< ', abs(value))
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
      format = reactable::colFormat(digits = 2, percent = F),
      filterable = TRUE,
      filterMethod = reactable::JS("function(rows, columnId, filterValue) {
        return rows.filter(function(row) {
          return row.values[columnId] >= filterValue
        })
      }"),
      filterInput = function(values, name) {
        oninput <- sprintf("Reactable.setFilter('%s', '%s', this.value)", elementId, name)
        shiny::tags$input(
          type = "range",
          min = floor(min(values, na.rm = T)),
          max = ceiling(max(values, na.rm = T)),
          value = floor(min(values, na.rm = T)),
          oninput = oninput,
          onchange = oninput, # For IE11 support
          "aria-label" = sprintf("Filter by minimum %s", name)
        )
      }
    )
  )
  return(result)
}



characteriationRiskFactorContColDefs <- function(
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
    countValue = reactable::colDef(
        header = withTooltip("# with Feature",
                             "Number with feature"),
        filterable = T
      , 
      format = reactable::colFormat(
        percent = F,
        separators = TRUE
      ),
      cell = function(value) {
        if(is.null(value)){return('< min threshold')}
        if(is.na(value)){return('< min threshold')}
        if (value >=0) value else paste0('< ', abs(value))
      }
    ),
    averageValue = reactable::colDef(
      header = withTooltip("Mean Feature Value",
                           "Mean value of the feature in the population"), 
      filterable = T,
      format = reactable::colFormat(digits = 2, percent = F)
    ), 
    standardDeviation = reactable::colDef(
      header = withTooltip("SD Feature Value",
                           "Standard deviation of the feature value in the population"), 
      filterable = T,
      format = reactable::colFormat(digits = 2, percent = F)
    ), 
    medianValue  = reactable::colDef(
      header = withTooltip("Median Feature Value",
                           "Median of the feature value"), 
      filterable = T, 
      format = reactable::colFormat(digits = 2, percent = F)
    ),
    p10Value  = reactable::colDef(
      header = withTooltip("10th %ile Feature Value",
                           "10th percentile of the feature value"), 
      filterable = T, 
      format = reactable::colFormat(digits = 2, percent = F)
    ),
    p25Value  = reactable::colDef(
      header = withTooltip("25th %tile Feature Value",
                           "25th percentile of the feature value"), 
      filterable = T, 
      format = reactable::colFormat(digits = 2, percent = F)
    ),
    p75Value  = reactable::colDef(
      header = withTooltip("75th %tile Feature Value",
                           "75th percentile of the feature value"), 
      filterable = T, 
      format = reactable::colFormat(digits = 2, percent = F)
    ),
    p90Value  = reactable::colDef(
      header = withTooltip("90th %tile Feature Value",
                           "90th percentile of the feature value"), 
      filterable = T, 
      format = reactable::colFormat(digits = 2, percent = F)
    ),
    maxValue  = reactable::colDef(
      header = withTooltip("Max Feature Value",
                           "Maximum of the feature value"), 
      filterable = T, 
      format = reactable::colFormat(digits = 2, percent = F)
    ),
    minValue  = reactable::colDef(
      header = withTooltip("Min Feature Value",
                           "Minimum of the feature value"), 
      filterable = T, 
      format = reactable::colFormat(digits = 2, percent = F)
    ),
    boxPlot = reactable::colDef(
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
      format = reactable::colFormat(digits = 2, percent = F),
      filterable = TRUE,
      filterMethod = reactable::JS("function(rows, columnId, filterValue) {
        return rows.filter(function(row) {
          return row.values[columnId] >= filterValue
        })
      }"),
      filterInput = function(values, name) {
        oninput <- sprintf("Reactable.setFilter('%s', '%s', this.value)", elementId, name)
        shiny::tags$input(
          type = "range",
          min = floor(min(values, na.rm = T)),
          max = ceiling(max(values, na.rm = T)),
          value = floor(min(values, na.rm = T)),
          oninput = oninput,
          onchange = oninput, # For IE11 support
          "aria-label" = sprintf("Filter by minimum %s", name)
        )
      }
    )
  )
  return(result)
}

