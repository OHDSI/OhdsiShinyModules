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
#' @param resultDatabaseSettings a list containing the characterization result schema, dbms, tablePrefix, databaseTable and cgTablePrefix
#' 
#' @return
#' The server to the description aggregate features module
#'
#' @export
characterizationAggregateFeaturesServer <- function(
    id, 
    connectionHandler,
    resultDatabaseSettings
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
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
            columnWidth = 6,
            varName = 'database',
            uiFunction = 'shinyWidgets::pickerInput',
            uiInputs = list(
              label = 'Database: ',
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
            varName = 'firstO',
            uiFunction = 'shinyWidgets::pickerInput',
            uiInputs = list(
              label = 'Restrict to first O: ',
              choices = c(T,F),
              selected = T,
              multiple = F
            )
          )
          ,
          createInputSetting(
            rowNumber = 2,                           
            columnWidth = 3,
            varName = 'index',
            uiFunction = 'shinyWidgets::pickerInput',
            uiInputs = list(
              label = 'Index: ',
              choices = c('T', 'O'),
              selected = 'T',
              multiple = F
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
        database = inputSelected()$database,
        firstO = inputSelected()$firstO,
        index = inputSelected()$index
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
      
      binaryData <- shiny::reactive({
        characterizationFeatureTable(
          data = allData()$binary
        )
      })
      
      continuousData <- shiny::reactive({
        characterizationFeatureTable(
          data = allData()$continuous
        )
      })
      
      binTableOutputs <- resultTableServer(
        id = "binaryTable", 
        df = binaryData,
        colDefsInput = list(
          covariateName = reactable::colDef(
            name = "Covariate Name", 
            filterable = T
          ),
          comp1T = reactable::colDef(
            name = "T without O mean", 
            format = reactable::colFormat(digits = 2, percent = T)
          ), 
          comp1sdT = reactable::colDef(
            name = "T without O stdev", 
            format = reactable::colFormat(digits = 2)
          ),
          comp2T = reactable::colDef(
            name = "T with O mean",
            format = reactable::colFormat(digits = 2, percent = T)
          ), 
          comp2sdT = reactable::colDef(
            name = "T with O stdev",
            format = reactable::colFormat(digits = 2)
          ), 
          comp1O = reactable::colDef(
            name = "O without T mean", 
            format = reactable::colFormat(digits = 2, percent = T)
          ), 
          comp1sdO = reactable::colDef(
            name = "O without T stdev", 
            format = reactable::colFormat(digits = 2)
          ),
          comp2O = reactable::colDef(
            name = "O with T mean",
            format = reactable::colFormat(digits = 2, percent = T)
          ), 
          comp2sdO = reactable::colDef(
            name = "O with T stdev",
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
        df = continuousData,
        colDefsInput = list(
          covariateName = reactable::colDef(
            name = "Covariate Name", 
            filterable = T
          ),
          comp1T = reactable::colDef(
            name = "T without O mean", 
            format = reactable::colFormat(digits = 2)
          ), 
          comp1sdT = reactable::colDef(
            name = "T without O stdev", 
            format = reactable::colFormat(digits = 2)
          ),
          comp2T = reactable::colDef(
            name = "T with O mean",
            format = reactable::colFormat(digits = 2)
          ), 
          comp2sdT = reactable::colDef(
            name = "T with O stdev",
            format = reactable::colFormat(digits = 2)
          ), 
          comp1O = reactable::colDef(
            name = "O without T mean", 
            format = reactable::colFormat(digits = 2)
          ), 
          comp1sdO = reactable::colDef(
            name = "O without T stdev", 
            format = reactable::colFormat(digits = 2)
          ),
          comp2O = reactable::colDef(
            name = "O with T mean",
            format = reactable::colFormat(digits = 2)
          ), 
          comp2sdO = reactable::colDef(
            name = "O with T stdev",
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
# edited to only use Ts and TnOs
characterizationGetAggregateData <- function(
    connectionHandler,
    resultDatabaseSettings,
  targetId,
  outcomeId,
  riskWindowStart,
  riskWindowEnd,
  startAnchor,
  endAnchor,
  database,
  firstO,
  index
){
  
  if(is.null(targetId)){
    return(NULL)
  }
  
  #get types based on index and first
  outcomeType <- ifelse(firstO, 'firstO', 'O')
  firstPart <- ifelse(index == 'T', 'T', outcomeType)
  secondPart <- ifelse(index == 'T',outcomeType, 'T')
  
  type1 <- firstPart
  type2 <- paste0(firstPart, 'n', secondPart)
  
  # if type is TnOc TnfirstOc the extract T minus TnO / TnOfirst
  
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
    database_id = database,
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
    target_id = ifelse(type2 %in% c('firstO','O'), 0, targetId),
    outcome_id = ifelse(type2 %in% c('T', 'allT'), 0, outcomeId),
    risk_window_start = riskWindowStart,
    start_anchor = startAnchor,
    risk_window_end = riskWindowEnd,
    end_anchor = endAnchor,
    database_id = database,
    type = type2
  )
  
  if(nrow(settingsSecond) == 0){
    print('no second setting')
    settingsSecond <- settingsFirst
  }
  
  shiny::incProgress(2/5, detail = paste("Got second runId and CohortId"))
  
  sql <- "SELECT  
  case when t.covariate_id is NULL then tno.covariate_id else t.covariate_id end covariate_id,
  t.sum_value - tno.sum_value as comp1_count,
  tno.sum_value as comp2_count,
  case when (t.sum_value - tno.sum_value)*1.0/(cc.row_count - cctno.row_count) is NULL then 0 else (t.sum_value - tno.sum_value)*1.0/(cc.row_count - cctno.row_count) end as comp1_@index,
  case when tno.average_value is NULL then 0 else tno.average_value end as comp2_@index,
  sqrt( (t.sum_value - tno.sum_value)*1.0/(cc.row_count - cctno.row_count) * (1-( (t.sum_value - tno.sum_value)*1.0/(cc.row_count - cctno.row_count) )) ) as comp1sd_@index,
  sqrt( (tno.average_value)*(1-(tno.average_value))) as comp2sd_@index,
  cov_ref.COVARIATE_NAME, 
  an_ref.ANALYSIS_NAME
  
  FROM
  
  (select * FROM @schema.@c_table_prefixCOVARIATES
  where
  DATABASE_ID  = '@database_id' and 
  COHORT_DEFINITION_ID = @cohort_def_1 and 
  RUN_ID in (@run_id_1) 
  ) t 
  full join
  (select * FROM @schema.@c_table_prefixCOVARIATES
  where
  DATABASE_ID  = '@database_id' and 
  COHORT_DEFINITION_ID = @cohort_def_2 and 
  RUN_ID in (@run_id_2)
  ) tno
  
  on 
  t.covariate_id = tno.covariate_id 
  and t.run_id = tno.run_id
  
  INNER JOIN
  @schema.@c_table_prefixCOHORT_COUNTS cc
  on cc.cohort_definition_id = t.cohort_definition_id 
  and cc.run_id = t.run_id 
  and cc.database_id = t.database_id
  
  INNER JOIN
  @schema.@c_table_prefixCOHORT_COUNTS cctno
  on cctno.cohort_definition_id = tno.cohort_definition_id 
  and cctno.run_id = tno.run_id 
  and cctno.database_id = tno.database_id
  
  INNER JOIN
  @schema.@c_table_prefixCOVARIATE_REF cov_ref
  ON cov_ref.covariate_id = t.covariate_id 
  and cov_ref.run_id = case when t.run_id is NULL then tno.run_id else t.run_id end
  and cov_ref.database_id = t.database_id
  
  INNER JOIN
  @schema.@c_table_prefixANALYSIS_REF an_ref
  ON an_ref.analysis_id = cov_ref.analysis_id 
  and an_ref.run_id = cov_ref.run_id
  and an_ref.database_id = cov_ref.database_id
  
  ;"

  shiny::incProgress(3/5, detail = paste("Getting binary data"))
  
  binary <- connectionHandler$queryDb(
    sql = sql, 
    schema = resultDatabaseSettings$schema,
    c_table_prefix = resultDatabaseSettings$cTablePrefix,
    cohort_def_1 = settingsFirst$cohortDefinitionId[1],
    cohort_def_2 = settingsSecond$cohortDefinitionId[1],
    database_id = database,
    run_id_1 = paste(settingsFirst$runId, collapse = ','),
    run_id_2 = paste(settingsSecond$runId, collapse = ','),
    index = index
  )
  
  shiny::incProgress(4/5, detail = paste("Getting continuous data"))
  
  sql <- "SELECT  
  case when t.covariate_id is NULL then tno.covariate_id else t.covariate_id end covariate_id,
  t.count_value - tno.count_value as comp1_count,
  tno.count_value as comp2_count,
  case when (t.count_value*t.average_value - tno.count_value*tno.average_value)*1.0/(cc.row_count-tnocc.row_count) is NULL then 0 else (t.count_value*t.average_value - tno.count_value*tno.average_value)*1.0/(cc.row_count-tnocc.row_count) end as comp1_@index,
  case when tno.average_value is NULL then 0 else tno.average_value end  as comp2_@index,
  sqrt( (square(t.standard_deviation)*cc.row_count - square(tno.standard_deviation)*tnocc.row_count)/ (cc.row_count - tnocc.row_count)) as comp1sd_@index,
  tno.standard_deviation as comp2sd_@index,
  cov_ref.COVARIATE_NAME, 
  an_ref.ANALYSIS_NAME
  
  FROM
  
  (select * FROM @schema.@c_table_prefixCOVARIATES_continuous
  where
  DATABASE_ID  = '@database_id' and 
  COHORT_DEFINITION_ID = @cohort_def_1 and 
  RUN_ID in (@run_id_1) 
  ) t 
  full join
  (select * FROM @schema.@c_table_prefixCOVARIATES_continuous
  where
  DATABASE_ID  = '@database_id' and 
  COHORT_DEFINITION_ID = @cohort_def_2 and 
  RUN_ID in (@run_id_2) 
  ) tno
  
  on 
  t.covariate_id = tno.covariate_id 
  and t.run_id = tno.run_id
  
  INNER JOIN
  @schema.@c_table_prefixCOHORT_COUNTS cc
  on cc.cohort_definition_id = t.cohort_definition_id 
  and cc.run_id =  t.run_id 
  and cc.database_id = t.database_id
  
  INNER JOIN
  @schema.@c_table_prefixCOHORT_COUNTS tnocc
  on tnocc.cohort_definition_id = tno.cohort_definition_id 
  and tnocc.run_id =  tno.run_id 
  and tnocc.database_id = tno.database_id
  
  INNER JOIN
  @schema.@c_table_prefixCOVARIATE_REF cov_ref
  ON cov_ref.covariate_id = t.covariate_id 
  and cov_ref.run_id = case when t.run_id is NULL then tno.run_id else t.run_id end
  and cov_ref.database_id = t.database_id
  
  INNER JOIN
  @schema.@c_table_prefixANALYSIS_REF an_ref
  ON an_ref.analysis_id = cov_ref.analysis_id 
  and an_ref.run_id = cov_ref.run_id
  and an_ref.database_id = cov_ref.database_id
  
  ;"

  continuous <- connectionHandler$queryDb(
    sql = sql, 
    schema = resultDatabaseSettings$schema,
    c_table_prefix = resultDatabaseSettings$cTablePrefix,
    cohort_def_1 = settingsFirst$cohortDefinitionId[1],
    cohort_def_2 = settingsSecond$cohortDefinitionId[1],
    database_id = database,
    run_id_1 = paste(settingsFirst$runId, collapse =  ','),
    run_id_2 = paste(settingsSecond$runId, collapse =  ','),
    index = index
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
  
  # selecting the column anmes that has _index appended to it
  comp1Name <- paste0('comp1', c('O', 'T'))[paste0('comp1', c('O', 'T')) %in% colnames(data)]
  comp2Name <- paste0('comp2', c('O', 'T'))[paste0('comp2', c('O', 'T')) %in% colnames(data)]
  data$comp1 <- data[,comp1Name]
  data$comp2 <- data[,comp2Name]
  
  maxval <- max(max(data$comp1),max(data$comp2))
  
  plot <- plotly::plot_ly(
    data = data,
    x = ~.data$comp1,
    y = ~.data$comp2,
    showlegend = F
    ) %>%
    plotly::add_markers(color=factor(data$analysisName),
                        hoverinfo = 'text',
                        text = ~paste(
                          '\n',descGetType(data$covariateName),
                          '\n',descGetName(data$covariateName),
                          '\n',descGetTime(data$covariateName)
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
  
  # selecting the column that as _index appended to it
  comp1Name <- paste0('comp1', c('O', 'T'))[paste0('comp1', c('O', 'T')) %in% colnames(data)]
  comp2Name <- paste0('comp2', c('O', 'T'))[paste0('comp2', c('O', 'T')) %in% colnames(data)]
  comp1sdName <- paste0('comp1sd', c('O', 'T'))[paste0('comp1sd', c('O', 'T')) %in% colnames(data)]
  comp2sdName <- paste0('comp2sd', c('O', 'T'))[paste0('comp2sd', c('O', 'T')) %in% colnames(data)]
  
  if(sum(is.null(data[comp1sdName]))>0){
    data[comp1sdName][is.null(data[comp1sdName])] <- 0
  }
  if(sum(is.null(data[comp2sdName]))>0){
    data[comp2sdName][is.null(data[comp2sdName])] <- 0
  }

    data <- data %>%
      dplyr::mutate(
        standardizedMeanDiff = (.data[[comp1Name]] - .data[[comp2Name]])/(sqrt((.data[[comp1sdName]]^2 + .data[[comp2sdName]]^2)))
        ) %>%
      dplyr::select(
        "covariateName",
        "analysisName",
        comp1Name,
        comp1sdName,
        comp2Name,
        comp2sdName,
        "standardizedMeanDiff"
      )
    
    if(sum(is.null(data$standardizedMeanDiff))>0){
      data$standardizedMeanDiff[is.null(data$standardizedMeanDiff)] <- 0
    }
    
    if(sum(!is.finite(data$standardizedMeanDiff))>0){
      data$standardizedMeanDiff[!is.finite(data$standardizedMeanDiff)] <- 0
    }
    
  return(data)
}
