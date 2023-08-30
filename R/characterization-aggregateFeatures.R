# @file characterization-aggregateFeatures.R
#
# Copyright 2022 Observational Health Data Sciences and Informatics
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


#' The module viewer for exploring aggregate feature results 
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' 
#' @return
#' The user interface to the description aggregate feature module
#'
#' @export
characterizationAggregateFeaturesViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    
    infoHelperViewer(
      id = "helper",
      helpLocation= system.file("characterization-www", "help-OutcomeStratified.html", package = utils::packageName())
    ),
    
    
    # module that does input selection for a single row DF
    inputSelectionViewer(
      id = ns("input-selection")
    ),
    
    # COV: RUN_ID	DATABASE_ID	COHORT_DEFINITION_ID	COVARIATE_ID	SUM_VALUE	AVERAGE_VALUE
    # COV REF: RUN_ID	DATABASE_ID	COVARIATE_ID	COVARIATE_NAME	ANALYSIS_ID	CONCEPT_ID
    # settings: RUN_ID	DATABASE_ID	COVARIATE_SETTING_JSON	RISK_WINDOW_START	START_ANCHOR	RISK_WINDOW_END	END_ANCHOR	
    # cohort_details: RUN_ID	DATABASE_ID COHORT_DEFINITION_ID	TARGET_COHORT_ID	OUTCOME_COHORT_ID	COHORT_TYPE
    # analysis_ref: RUN_ID	DATABASE_ID	ANALYSIS_ID	ANALYSIS_NAME	DOMAIN_ID	START_DAY	END_DAY	IS_BINARY	MISSING_MEANS_ZERO
    # cov cont: RUN_ID	DATABASE_ID	COHORT_DEFINITION_ID	COVARIATE_ID	COUNT_VALUE	MIN_VALUE	MAX_VALUE	AVERAGE_VALUE	STANDARD_DEVIATION	MEDIAN_VALUE	P_10_VALUE	P_25_VALUE	P_75_VALUE	P_90_VALUE
    # add table with options to select T, O and TAR
    
    # add UI to pick database/type 1 and database/type 2
    
    shiny::conditionalPanel(
      condition = 'input.generate != 0',
      ns = shiny::NS(ns("input-selection")),
      
      shinydashboard::tabBox(
        width = "100%",
        # Title can include an icon
        title = shiny::tagList(shiny::icon("gear"), "Table and Plots"),
        shiny::tabPanel("Binary Feature Table", 
                        resultTableViewer(ns('binaryTable'))
        ),
        shiny::tabPanel("Continuous Feature Table", 
                        resultTableViewer(ns('continuousTable'))
        ),
        shiny::tabPanel("Binary Feature Plot",
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput(ns("binaryPlot"))
                        )
        ),
        shiny::tabPanel("Continuous Feature Plot", 
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput(ns("continuousPlot"))
                        )
        )
      )
    )
  )
}


#' The module server for exploring aggregate features results 
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param connectionHandler the connection to the prediction result database
#' @param mainPanelTab the current tab 
#' @param resultDatabaseSettings a list containing the characterization result schema, dbms, tablePrefix, databaseTable and cgTablePrefix
#' 
#' @return
#' The server to the description aggregate features module
#'
#' @export
characterizationAggregateFeaturesServer <- function(
    id, 
    connectionHandler,
    mainPanelTab,
    resultDatabaseSettings
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      types <- c(
        'Target (first exposure)',
        'Outcome (all exposures)',
        'Target with outcome (all exposures) during TAR (T index)',
        'Target with outcome (all exposures) during TAR (O index)',
        'Target without outcome (all exposures) during TAR',
        'Target (all exposures)',
        'Outcome (first exposure)',
        'Target with outcome (first exposure) during TAR (T index)',
        'Target without outcome (first exposure) during TAR',
        'Target with outcome (first exposure) during TAR (O index)'
      )
      typesTranslate <- c(
        'T',
        'O',
        'TnO',
        'OnT',
        'TnOc',
        'allT',
        'firstO',
        'TnfirstO',
        'TnfirstOc',
        'firstOnT'
      )
      
      # get the possible options
      options <- getAggregateFeatureOptions(
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings
      )
      
      # get databases
      databases <- getAggregateFeatureDatabases(
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings
      )
      
      # input selection component
      inputSelected <- inputSelectionServer(
        id = "input-selection", 
        inputSettingList = list(
          createInputSetting(
            rowNumber = 1,                           
            columnWidth = 4,
            varName = 'targetIds',
            uiFunction = 'shinyWidgets::pickerInput',
            uiInputs = list(
              label = 'Target: ',
              choices = options$targets,
              selected = options$targets[1],
              multiple = F,
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                size = 10,
                liveSearchStyle = "contains",
                liveSearchPlaceholder = "Type here to search",
                virtualScroll = 50
              )
            )
          ),
          createInputSetting(
            rowNumber = 1,                           
            columnWidth = 4,
            varName = 'outcomeIds',
            uiFunction = 'shinyWidgets::pickerInput',
            uiInputs = list(
              label = 'Outcome: ',
              choices = options$outcomes,
              selected = options$outcomes[1],
              multiple = F,
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                size = 10,
                liveSearchStyle = "contains",
                liveSearchPlaceholder = "Type here to search",
                virtualScroll = 50
              )
            )
          )
          ,
          createInputSetting(
            rowNumber = 1,                           
            columnWidth = 4,
            varName = 'tarIds',
            uiFunction = 'shinyWidgets::pickerInput',
            uiInputs = list(
              label = 'Time at risk: ',
              choices = options$tars,
              selected = options$tars[1],
              multiple = F,
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                size = 10,
                liveSearchStyle = "contains",
                liveSearchPlaceholder = "Type here to search",
                virtualScroll = 50
              )
            )
          )
          ,
          createInputSetting(
            rowNumber = 2,                           
            columnWidth = 3,
            varName = 'database1',
            uiFunction = 'shinyWidgets::pickerInput',
            uiInputs = list(
              label = 'Database 1: ',
              choices = databases,
              selected = databases[1],
              multiple = F,
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                size = 10,
                liveSearchStyle = "contains",
                liveSearchPlaceholder = "Type here to search",
                virtualScroll = 50
              )
            )
          )
          ,
          createInputSetting(
            rowNumber = 2,                           
            columnWidth = 3,
            varName = 'type1',
            uiFunction = 'shinyWidgets::pickerInput',
            uiInputs = list(
              label = 'Type 1: ',
              choices = types,
              selected = types[1],
              multiple = F,
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                size = 10,
                liveSearchStyle = "contains",
                liveSearchPlaceholder = "Type here to search",
                virtualScroll = 50
              )
            )
          )
          ,
          createInputSetting(
            rowNumber = 2,                           
            columnWidth = 3,
            varName = 'database2',
            uiFunction = 'shinyWidgets::pickerInput',
            uiInputs = list(
              label = 'Database 2: ',
              choices = databases,
              selected = databases[1],
              multiple = F,
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                size = 10,
                liveSearchStyle = "contains",
                liveSearchPlaceholder = "Type here to search",
                virtualScroll = 50
              )
            )
          )
          ,
          createInputSetting(
            rowNumber = 2,                           
            columnWidth = 3,
            varName = 'type2',
            uiFunction = 'shinyWidgets::pickerInput',
            uiInputs = list(
              label = 'Type 2: ',
              choices = types,
              selected = types[2],
              multiple = F,
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                size = 10,
                liveSearchStyle = "contains",
                liveSearchPlaceholder = "Type here to search",
                virtualScroll = 50
              )
            )
          )
        )
      )
      
      allData <- shiny::reactive({
        characterizationGetAggregateData(
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings,
        targetId = inputSelected()$targetIds,
        outcomeId = inputSelected()$outcomeIds,
        riskWindowStart = options$tarList[[which(options$tars == ifelse(is.null(inputSelected()$tarIds),options$tars[1],inputSelected()$tarIds))]]$riskWindowStart, 
        riskWindowEnd = options$tarList[[which(options$tars == ifelse(is.null(inputSelected()$tarIds),options$tars[1],inputSelected()$tarIds))]]$riskWindowEnd,
        startAnchor = options$tarList[[which(options$tars == ifelse(is.null(inputSelected()$tarIds),options$tars[1],inputSelected()$tarIds))]]$startAnchor,
        endAnchor = options$tarList[[which(options$tars == ifelse(is.null(inputSelected()$tarIds),options$tars[1],inputSelected()$tarIds))]]$endAnchor,
        database1 = inputSelected()$database1,
        database2 = inputSelected()$database2,
        type1 = typesTranslate[types == inputSelected()$type1],
        type2 = typesTranslate[types == inputSelected()$type2]
      )
        })
      
      output$binaryPlot <- plotly::renderPlotly(
        characterizationFeaturePlot(
          data = allData()$binary,
          valueColumn = 'averageValue'
        )
      )
      output$continuousPlot <- plotly::renderPlotly(
        characterizationFeaturePlot(
          data = allData()$continuous,
          valueColumn = 'averageValue'
        )
      )
      
      binTableOutputs <- resultTableServer(
        id = "binaryTable", 
        df = characterizationFeatureTable(allData()$binary),
        colDefsInput = list(
          covariateName = reactable::colDef(
            name = "Covariate Name", 
            filterable = T
          ),
          comp1 = reactable::colDef(
            name = "Selection 1 mean", 
            format = reactable::colFormat(digits = 2, percent = T)
          ), 
          comp1sd = reactable::colDef(
            name = "Selection 1 stdev", 
            format = reactable::colFormat(digits = 2)
          ),
          comp2 = reactable::colDef(
            name = "Selection 2 mean",
            format = reactable::colFormat(digits = 2, percent = T)
          ), 
          comp2sd = reactable::colDef(
            name = "Selection 2 stdev",
            format = reactable::colFormat(digits = 2)
          ), 
          analysisName = reactable::colDef( # not sure this will work now
            filterInput = function(values, name) {
              shiny::tags$select(
                # Set to undefined to clear the filter
                onchange = sprintf("Reactable.setFilter('desc-bin-select', '%s', event.target.value || undefined)", name),
                # "All" has an empty value to clear the filter, and is the default option
                shiny::tags$option(value = "", "All"),
                lapply(unique(values), shiny::tags$option),
                "aria-label" = sprintf("Filter %s", name),
                style = "width: 100%; height: 28px;"
              )
            }
          ),
          standardizedMeanDiff = reactable::colDef(
            format = reactable::colFormat(digits = 2)
          )
        ),
        addActions = NULL
      )
      
      conTableOutputs <- resultTableServer(
        id = "continuousTable", 
        df = characterizationFeatureTable(allData()$continuous),
        colDefsInput = list(
          covariateName = reactable::colDef(
            name = "Covariate Name", 
            filterable = T
          ),
          comp1 = reactable::colDef(
            name = "Selection 1 mean", 
            format = reactable::colFormat(digits = 2)
          ),
          comp1sd = reactable::colDef(
            name = "Selection 1 stdev", 
            format = reactable::colFormat(digits = 2)
          ),
          comp2 = reactable::colDef(
            name = "Selection 2 mean",
            format = reactable::colFormat(digits = 2)
          ),
          comp2sd = reactable::colDef(
            name = "Selection 2 stdev",
            format = reactable::colFormat(digits = 2)
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
          ),
          standardizedMeanDiff = reactable::colDef(
            format = reactable::colFormat(digits = 2)
          )
        ),
        addActions = NULL
      )
   
     #elementId = "desc-cont-select"

      
  
  return(invisible(NULL))
    }
  )
}


getAggregateFeatureOptions <- function(
  connectionHandler,
  resultDatabaseSettings
){
  
 
  shiny::withProgress(message = 'Getting feature comparison options', value = 0, {
  
  sql <- "SELECT DISTINCT t.COHORT_NAME as TARGET, cd.TARGET_COHORT_ID, 
            o.COHORT_NAME as outcome, cd.OUTCOME_COHORT_ID, 
            s.RISK_WINDOW_START,	s.START_ANCHOR,	s.RISK_WINDOW_END,	s.END_ANCHOR  
          FROM @schema.@c_table_prefixCOHORT_DETAILS cd
          inner join @schema.@c_table_prefixSETTINGS s
          on cd.run_id = s.run_id and cd.database_id = s.database_id
          inner join @schema.@cg_table_prefixCOHORT_DEFINITION t
          on cd.TARGET_COHORT_ID = t.COHORT_DEFINITION_ID
          inner join @schema.@cg_table_prefixCOHORT_DEFINITION o
          on cd.OUTCOME_COHORT_ID = o.COHORT_DEFINITION_ID
          WHERE cd.TARGET_COHORT_ID != 0 AND cd.OUTCOME_COHORT_ID != 0;"

  shiny::incProgress(1/2, detail = paste("Extracting options"))
  
  options <- connectionHandler$queryDb(
    sql = sql, 
    schema = resultDatabaseSettings$schema,
    c_table_prefix = resultDatabaseSettings$cTablePrefix,
    cg_table_prefix = resultDatabaseSettings$cgTablePrefix
  )
  
  shiny::incProgress(2/2, detail = paste("Finished"))
  
  })
  
  targets <- unique(options$targetCohortId)
  names(targets) <- unique(options$target)
  
  outcomes <- unique(options$outcomeCohortId)
  names(outcomes) <- unique(options$outcome)
  
  options <- unique(
    options %>% 
      dplyr::select(
        "riskWindowStart",
        "riskWindowEnd",
        "startAnchor",
        "endAnchor"
      )
    )
  
  tarList <- lapply(
    1:nrow(options), 
    function(i){
    list(
      riskWindowStart = options$riskWindowStart[i],
      riskWindowEnd = options$riskWindowEnd[i],
      startAnchor = options$startAnchor[i],
      endAnchor = options$endAnchor[i] 
    )
  })
  
  tars <- unlist(
    lapply(
      1:nrow(options), 
      function(i){
        paste0(
          '(',options$startAnchor[i],' + ', options$riskWindowStart[i],
          ') - (', options$endAnchor[i],' + ', options$riskWindowEnd[i],
          ')'
        )
      })
  )
  
  return(
    list(
      targets = targets,
      outcomes = outcomes,
      tars = tars,
      tarList = tarList
    )
  )
}

getAggregateFeatureDatabases <- function(
    connectionHandler,
    resultDatabaseSettings
){
  
  shiny::withProgress(message = 'Finding databases', value = 0, {
    sql <- "SELECT DISTINCT s.DATABASE_ID, d.CDM_SOURCE_ABBREVIATION as database_name  
          FROM @schema.@c_table_prefixCOHORT_DETAILS cd
          inner join @schema.@database_table d
          on cd.database_id = d.database_id
          inner join @schema.@c_table_prefixSETTINGS s
          on s.database_id = d.database_id
          and s.run_id = cd.run_id;"
    
    shiny::incProgress(1/2, detail = paste("Extracting databases"))
    
    
    databases <- connectionHandler$queryDb(
      sql = sql, 
      schema = resultDatabaseSettings$schema,
      c_table_prefix = resultDatabaseSettings$cTablePrefix,
      database_table = resultDatabaseSettings$databaseTable
    )
    
    shiny::incProgress(2/2, detail = paste("Finished"))
    
  }
  )
  
  dbs <- databases$databaseId
  names(dbs) <- databases$databaseName
  
  return(dbs)
}

# pulls all data for a target and outcome
characterizationGetAggregateData <- function(
    connectionHandler,
    resultDatabaseSettings,
  targetId,
  outcomeId,
  riskWindowStart,
  riskWindowEnd,
  startAnchor,
  endAnchor,
  database1,
  database2,
  type1,
  type2
){
  
  if(is.null(targetId)){
    return(NULL)
  }
  
  shiny::withProgress(message = 'Getting Feature Comparison Data', value = 0, {
  sql <- "SELECT s.RUN_ID, cd.COHORT_DEFINITION_ID
          FROM @schema.@c_table_prefixSETTINGS s
          inner join 
          @schema.@c_table_prefixCOHORT_DETAILS cd
          on cd.database_id = s.database_id and
          cd.run_id = s.run_id
          WHERE cd.TARGET_COHORT_ID = @target_id and cd.OUTCOME_COHORT_ID = @outcome_id
          and s.RISK_WINDOW_START = @risk_window_start and s.START_ANCHOR = '@start_anchor'
          and s.RISK_WINDOW_END = @risk_window_end and	s.END_ANCHOR = '@end_anchor'
          and s.DATABASE_ID  = '@database_id' and cd.COHORT_TYPE = '@type';"

  settingsFirst <- connectionHandler$queryDb(
    sql = sql, 
    schema = resultDatabaseSettings$schema,
    c_table_prefix = resultDatabaseSettings$cTablePrefix,
    target_id = ifelse(type1 %in% c('firstO','O'), 0, targetId),
    outcome_id = ifelse(type1 %in% c('T', 'allT'), 0, outcomeId),
    risk_window_start = riskWindowStart,
    start_anchor = startAnchor,
    risk_window_end = riskWindowEnd,
    end_anchor = endAnchor,
    database_id = database1,
    type = type1
  )
  
  shiny::incProgress(1/5, detail = paste("Got first runId and cohortId"))
  
  
  sql <- "SELECT s.RUN_ID, cd.COHORT_DEFINITION_ID
          FROM @schema.@c_table_prefixSETTINGS s
          inner join 
          @schema.@c_table_prefixCOHORT_DETAILS cd
          on cd.database_id = s.database_id and
          cd.run_id = s.run_id
          WHERE cd.TARGET_COHORT_ID = @target_id and cd.OUTCOME_COHORT_ID = @outcome_id
          and s.RISK_WINDOW_START = @risk_window_start and s.START_ANCHOR = '@start_anchor'
          and s.RISK_WINDOW_END = @risk_window_end and	s.END_ANCHOR = '@end_anchor'
          and s.DATABASE_ID  = '@database_id' and cd.COHORT_TYPE = '@type';"
  

  settingsSecond <- connectionHandler$queryDb(
    sql = sql, 
    schema = resultDatabaseSettings$schema,
    c_table_prefix = resultDatabaseSettings$cTablePrefix,
    target_id = ifelse(type1 %in% c('firstO','O'), 0, targetId),
    outcome_id = ifelse(type1 %in% c('T', 'allT'), 0, outcomeId),
    risk_window_start = riskWindowStart,
    start_anchor = startAnchor,
    risk_window_end = riskWindowEnd,
    end_anchor = endAnchor,
    database_id = database2,
    type = type2
  )
  
  if(nrow(settingsSecond) == 0){
    print('no second setting')
    settingsSecond <- settingsFirst
  }
  
  shiny::incProgress(2/5, detail = paste("Got second runId and CohortId"))
  
  sql <- "SELECT cov.*, cov_ref.COVARIATE_NAME, an_ref.ANALYSIS_NAME,
  case when (cov.DATABASE_ID  = '@database_id1' and cov.COHORT_DEFINITION_ID = @cohortDef1 and cov.RUN_ID in (@run_id1)) then 'comp1' else 'comp2' end as label
          FROM @schema.@c_table_prefixCOVARIATES cov 
          INNER JOIN
          @schema.@c_table_prefixCOVARIATE_REF cov_ref
          ON cov.covariate_id = cov_ref.covariate_id 
          and cov.run_id = cov_ref.run_id
          and cov.database_id = cov_ref.database_id
          INNER JOIN
          @schema.@c_table_prefixANALYSIS_REF an_ref
          ON an_ref.analysis_id = cov_ref.analysis_id 
          and an_ref.run_id = cov_ref.run_id
          and an_ref.database_id = cov_ref.database_id
          WHERE 
          (
          (cov.DATABASE_ID  = '@database_id1' and cov.COHORT_DEFINITION_ID = @cohortDef1 and cov.RUN_ID in (@run_id1))
          OR
          (cov.DATABASE_ID  = '@database_id2' and cov.COHORT_DEFINITION_ID = @cohortDef2 and cov.RUN_ID in (@run_id2))
          );"

  shiny::incProgress(3/5, detail = paste("Getting binary data"))
  
  binary <- connectionHandler$queryDb(
    sql = sql, 
    schema = resultDatabaseSettings$schema,
    c_table_prefix = resultDatabaseSettings$cTablePrefix,
    cohortDef1 = settingsFirst$cohortDefinitionId[1],
    cohortDef2 = settingsSecond$cohortDefinitionId[1],
    database_id1 = database1,
    database_id2 = database2,
    run_id1 = paste(settingsFirst$runId, collapse = ','),
    run_id2 = paste(settingsSecond$runId, collapse = ',')
  )
  
  shiny::incProgress(4/5, detail = paste("Getting continuous data"))
  
  sql <- "SELECT cov.*, cov_ref.COVARIATE_NAME, an_ref.ANALYSIS_NAME,
  case when (cov.DATABASE_ID  = '@database_id1' and cov.COHORT_DEFINITION_ID = @cohortDef1 and cov.RUN_ID in (@run_id1)) then 'comp1' else 'comp2' end as label
          FROM @schema.@c_table_prefixCOVARIATES_CONTINUOUS cov 
          INNER JOIN
          @schema.@c_table_prefixCOVARIATE_REF cov_ref
          ON cov.covariate_id = cov_ref.covariate_id 
          and cov.run_id = cov_ref.run_id
          and cov.database_id = cov_ref.database_id
          INNER JOIN
          @schema.@c_table_prefixANALYSIS_REF an_ref
          ON an_ref.analysis_id = cov_ref.analysis_id 
          and an_ref.run_id = cov_ref.run_id
          and an_ref.database_id = cov_ref.database_id
          WHERE 
          (
          (cov.DATABASE_ID  = '@database_id1' and cov.COHORT_DEFINITION_ID = @cohortDef1 and cov.RUN_ID in (@run_id1))
          OR
          (cov.DATABASE_ID  = '@database_id2' and cov.COHORT_DEFINITION_ID = @cohortDef2 and cov.RUN_ID in (@run_id2))
          );"

  continuous <- connectionHandler$queryDb(
    sql = sql, 
    schema = resultDatabaseSettings$schema,
    c_table_prefix = resultDatabaseSettings$cTablePrefix,
    cohortDef1 = settingsFirst$cohortDefinitionId[1],
    cohortDef2 = settingsSecond$cohortDefinitionId[1],
    database_id1 = database1,
    database_id2 = database2,
    run_id1 = paste(settingsFirst$runId, collapse =  ','),
    run_id2 = paste(settingsSecond$runId, collapse =  ',')
  )

  shiny::incProgress(5/5, detail = paste("Finished"))
  }
  )
  
  return(list(
    binary = binary,
    continuous = continuous
  ))
}

characterizationFeaturePlot <- function(
  data,
  valueColumn = 'averageValue'
){
  
  if(is.null(data)){
    return(NULL)
  }
  
  valueColumns <- c("covariateName", "covariateId", valueColumn)
  
  
  shiny::withProgress(message = 'Generating plots', value = 0, {
  
  comp1 <- data %>% 
    dplyr::filter(.data$label == 'comp1') %>%
    dplyr::select(dplyr::all_of(valueColumns)) %>%
    dplyr::rename(comp1 = dplyr::all_of(valueColumn)) #.data[[valueColumn]])  # not sure how to do this ERROR?
  
  shiny::incProgress(1/5, detail = paste("Filtered comparision 1"))
    
  comp2 <- data %>% 
    dplyr::filter(.data$label == 'comp2') %>%
    dplyr::select(dplyr::all_of(valueColumns)) %>%
    dplyr::rename(comp2 = dplyr::all_of(valueColumn))  # not sure this will work ERROR?
  
  shiny::incProgress(2/5, detail = paste("Filtered comparision 2"))
  
  analysisIds <- data %>%
    dplyr::select(c("covariateName", "covariateId", "analysisName")) %>%
    dplyr::distinct()
  
  shiny::incProgress(3/5, detail = paste("Extracting analysisNames"))
  
  maxval <- max(max(comp1$comp1, na.rm = T),  max(comp2$comp2, na.rm = T))
  
  allData <- merge(comp1, comp2, by = c('covariateName','covariateId'), all = T)
  allData[is.na(allData)] <- 0
  allData <- merge(allData, analysisIds,  by = c('covariateName','covariateId') , all.x = T)
  
  shiny::incProgress(4/5, detail = paste("Merged data"))
  
  plot <- plotly::plot_ly(x = allData$comp1,
                  showlegend = F) %>%
    plotly::add_markers(y = allData$comp2,
                        color=factor(allData$analysisName),
                        hoverinfo = 'text',
                        text = ~paste(
                          '\n',descGetType(allData$covariateName),
                          '\n',descGetName(allData$covariateName),
                          '\n',descGetTime(allData$covariateName)
                          ),
                        showlegend = T
    ) %>%
    plotly::add_trace(x= c(0,maxval), y = c(0,maxval),mode = 'lines',
                      line = list(dash = "dash"), color = I('black'),
                      type='scatter', showlegend = FALSE) %>%
    plotly::layout(#title = 'Prevalance of baseline predictors in persons with and without outcome',
      xaxis = list(title = "Prevalance in selection 1"),
      yaxis = list(title = "Prevalance in selection 2"),
      #legend = l, showlegend = T,
      legend = list(orientation = 'h', y = -0.3), showlegend = T)
  
  shiny::incProgress(5/5, detail = paste("Finished"))
  
  })
  
    return(plot)
}

descGetType <- function(x){
  return(unlist(lapply(strsplit(x = x, split = ' during'), function(y){y[1]})))
}

descGetName <- function(x){
  return(unlist(lapply(strsplit(x = x, split = ': '), function(y){y[length(y)]})))
}

descGetTime <- function(x){
  part1 <- unlist(lapply(strsplit(x = x, split = ' during '), function(y){y[2]}))
  return(unlist(lapply(strsplit(x = part1, split = ': '), function(y){y[1]})))
}


characterizationFeatureTable <- function(
  data
){
  
  if(is.null(data)){
    return(NULL)
  }
  
  shiny::withProgress(message = 'Generating Table', value = 0, {
    
    if(!'standardDeviation' %in% colnames(data)){
      # adding standard dev for binary features
      data <- data %>% 
        dplyr::mutate(
          standardDeviation = sqrt(data$averageValue * (1-data$averageValue))
                        )
    }
    
    comp1 <- data %>% 
      dplyr::filter(.data$label == 'comp1') %>%
      dplyr::select(
        c(
        "covariateId",
        "covariateName", 
        "averageValue", 
        "standardDeviation"
        )
        ) %>%
      dplyr::rename(
        comp1 = "averageValue",
        comp1sd = "standardDeviation"
        )

    
    shiny::incProgress(1/4, detail = paste("Filtered comparision 1"))
    
    comp2 <- data %>% 
      dplyr::filter(.data$label == 'comp2') %>%
      dplyr::select(
        c(
        "covariateId",
        "covariateName", 
        "averageValue", 
        "standardDeviation"
        )
        ) %>%
      dplyr::rename(
        comp2 = "averageValue",
        comp2sd = "standardDeviation"
        )
    
    shiny::incProgress(2/4, detail = paste("Filtered comparision 2"))
    
    analysisIds <- data %>%
      dplyr::select(c("covariateName", "covariateId", "analysisName")) %>%
      dplyr::distinct()
    
    shiny::incProgress(3/4, detail = paste("Extracting analysisIds"))
    
    allData <- merge(
      comp1, 
      comp2, 
      by = c('covariateId', 'covariateName'), 
      all = T
      )
    allData[is.na(allData)] <- 0
    allData <- merge(allData, analysisIds,  by = c('covariateId', 'covariateName'), all.x = T)
    
    allData <- allData %>%
      dplyr::mutate(
        standardizedMeanDiff = (.data$comp1 - .data$comp2)/(sqrt((.data$comp1sd^2 + .data$comp2sd^2)/2))
        ) 
    
    # multiple binary by 100 and make to 2dp?
    
    shiny::incProgress(4/4, detail = paste("Finished"))
    
  })
  
  return(allData)
}
