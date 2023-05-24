# @file estimation-diagnosticsSummary
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


#' The module viewer for rendering the PLE diagnostics results
#'
#' @param id the unique reference id for the module
#'
#' @return
#' The user interface to the estimation diagnostics viewer
#' 
#' @export
estimationDiagnosticsSummaryViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    
    inputSelectionViewer(ns("input-selection")),
    
    shiny::conditionalPanel(
      condition = 'input.generate != 0',
      ns = shiny::NS(ns("input-selection")),
      
      shiny::tabsetPanel(
        type = 'pills',
        id = ns('diagnosticsTablePanel'),
        shiny::tabPanel(
          title = 'Summary',
          resultTableViewer(ns("diagnosticsSummaryTable"))
        ),
        shiny::tabPanel(
          title = 'Full',
          resultTableViewer(ns("diagnosticsTable"))
        )
      )
    )
  )
}


#' The module server for rendering the PLE diagnostics summary
#'
#' @param id the unique reference id for the module
#' @param connectionHandler the connection to the PLE results database
#' @param resultsSchema the schema with the PLE results
#' @param tablePrefix tablePrefix
#' @param cohortTablePrefix cohortTablePrefix
#' @param databaseTable databaseTable
#'
#' @return
#' the PLE diagnostics summary results
#' 
#' @export
estimationDiagnosticsSummaryServer <- function(
    id,
    connectionHandler,
    resultsSchema,
    tablePrefix,
    cohortTablePrefix,
    databaseTable
) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      targetIds <- getCmDiagCohorts(
        connectionHandler = connectionHandler,
        resultsSchema = resultsSchema,
        tablePrefix = tablePrefix,
        cohortTablePrefix = cohortTablePrefix,
        type = 'target'
      )
      outcomeIds <- getCmDiagCohorts(
        connectionHandler = connectionHandler,
        resultsSchema = resultsSchema,
        tablePrefix = tablePrefix,
        cohortTablePrefix = cohortTablePrefix,
        type = 'outcome'
      )
      comparatorIds <- getCmDiagCohorts(
        connectionHandler = connectionHandler,
        resultsSchema = resultsSchema,
        tablePrefix = tablePrefix,
        cohortTablePrefix = cohortTablePrefix,
        type = 'comparator'
      )
      analysisIds <- getCmDiagAnalyses(
        connectionHandler = connectionHandler,
        resultsSchema = resultsSchema, 
        tablePrefix = tablePrefix
      )
      
      inputSelected <- inputSelectionServer(
        id = "input-selection", 
        inputSettingList = list(
          createInputSetting(
            rowNumber = 1,                           
            columnWidth = 6,
            varName = 'targetIds',
            uiFunction = 'shinyWidgets::pickerInput',
            uiInputs = list(
              label = 'Target: ',
              choices = targetIds,
              selected = targetIds[1],
              multiple = T,
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
            columnWidth = 6,
            varName = 'outcomeIds',
            uiFunction = 'shinyWidgets::pickerInput',
            uiInputs = list(
              label = 'Outcome: ',
              choices = outcomeIds,
              selected = outcomeIds[1],
              multiple = T,
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
            rowNumber = 2,                           
            columnWidth = 6,
            varName = 'comparatorIds',
            uiFunction = 'shinyWidgets::pickerInput',
            uiInputs = list(
              label = 'Comparator: ',
              choices = comparatorIds,
              selected = comparatorIds[1],
              multiple = T,
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
            rowNumber = 2,                           
            columnWidth = 6,
            varName = 'analysisIds',
            uiFunction = 'shinyWidgets::pickerInput',
            uiInputs = list(
              label = 'Analysis: ',
              choices = analysisIds,
              selected = analysisIds[1],
              multiple = T,
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
      
      data <- shiny::reactive({
        getCmDiagnosticsData(
          connectionHandler,
          resultsSchema,
          tablePrefix,
          cohortTablePrefix,
          databaseTable,
          targetIds = inputSelected()$targetIds,
          outcomeIds = inputSelected()$outcomeIds,
          comparatorIds = inputSelected()$comparatorIds,
          analysisIds = inputSelected()$analysisIds
        )
      })
      
      data2 <- shiny::reactive({
        diagnosticSummaryFormat(data)
      })
      
      customColDefs <- list(
        databaseName = reactable::colDef(
          header = withTooltip(
            "Database",
            "The database name"
          )
        ),
        target = reactable::colDef(
          header = withTooltip(
            "Target",
            "The target cohort of interest "
          )
        ),
        comparator = reactable::colDef(
          header = withTooltip(
            "Comparator",
            "The comparator cohort of interest "
          )
        ),
        outcome = reactable::colDef(
          header = withTooltip(
            "Outcome",
            "The outcome of interest "
          )
        ),
        analysis = reactable::colDef(
          header = withTooltip(
            "Analysis",
            "The analysis name "
          )
        ),
        
        mdrr = reactable::colDef(
          header = withTooltip(
            "mdrr",
            "The minimum detectible relative risk"
          )
        ),
        ease = reactable::colDef(
          header = withTooltip(
            "ease",
            "The ..."
          )
        ),
        timeTrendP = reactable::colDef(
          header = withTooltip(
            "timeTrendP",
            "The ..."
          )
        ),
        preExposureP = reactable::colDef(
          header = withTooltip(
            "preExposureP",
            "The ..."
          )
        ),
        mdrrDiagnostic = reactable::colDef(
          header = withTooltip(
            "mdrrDiagnostic",
            "The ..."
          )
        ),
        easeDiagnostic = reactable::colDef(
          header = withTooltip(
            "easeDiagnostic",
            "The ..."
          )
        ),
        timeTrendDiagnostic = reactable::colDef(
          header = withTooltip(
            "timeTrendDiagnostic",
            "The ..."
          )
        ),
        preExposureDiagnostic = reactable::colDef(
          header = withTooltip(
            "preExposureDiagnostic",
            "The ..."
          )
        ),
        
        unblind = reactable::colDef(
          header = withTooltip(
            "unblind",
            "If the value is 1 then the diagnostics passed and results can be unblinded"
          )
        )
        
      )
        
      resultTableServer(
        id = "diagnosticsTable",
        df = data,
        colDefsInput = customColDefs
      )
      
      customColDefs2 <- list(
        databaseName = reactable::colDef(
          header = withTooltip(
            "Database",
            "The database name"
          )
        ),
        target = reactable::colDef(
          header = withTooltip(
            "Target",
            "The target cohort of interest "
          )
        ),
        comparator = reactable::colDef(
          header = withTooltip(
            "Comparator",
            "The comparator cohort of interest "
          )
        )
      )
      
      resultTableServer(
        id = "diagnosticsSummaryTable",
        df = data2,
        colDefsInput = styleColumns(customColDefs2, outcomeIds, analysisIds)
      )
      
      
    }
  )
}

styleColumns <- function(
    customColDefs,
    outcomeIds, 
    analysisIds
){      
  
  colnameFormat <- merge(names(outcomeIds), names(analysisIds))
  colnameFormat <- apply(colnameFormat, 1, function(x){paste(x, collapse = '_', sep = '_')})
  
  styleList <- lapply(
    colnameFormat, 
    FUN = function(x){
      reactable::colDef(
        style = function(value) {
          color <- 'orange'
          if(is.na(value)){
            color <- 'black'
          }else if(value == 'Pass'){
            color <- '#AFE1AF'
          }else if(value == 'Fail'){
            color <- '#E97451'
          }
          list(background = color)
        }
      )
    }
  )
  names(styleList) <- colnameFormat
  result <- append(customColDefs, styleList)
  
  return(result)
}

diagnosticSummaryFormat <- function(
    data, 
    idCols = c('databaseName','target', 'comparator'),
    namesFrom = c('outcome','analysis')
    ){
  
  data2 <- tidyr::pivot_wider(
    data = data(), 
    id_cols = idCols, 
    names_from = namesFrom, 
    values_from = c('summaryValue')
    )
  
  return(data2)
}

getCmDiagCohorts <- function(
    connectionHandler,
    resultsSchema, 
    tablePrefix,
    cohortTablePrefix,
    type = 'target'
){
  
  sql <- "
    SELECT DISTINCT
      cgcd1.cohort_name as names,
      cgcd1.cohort_definition_id
    FROM
      @results_schema.@table_prefixdiagnostics_summary cmds
      INNER JOIN 
      @results_schema.@cohort_table_prefixcohort_definition cgcd1 
      ON cmds.@type_id = cgcd1.cohort_definition_id;
  "
  
  result <- connectionHandler$queryDb(
    sql = sql,
    results_schema = resultsSchema,
    table_prefix = tablePrefix,
    cohort_table_prefix = cohortTablePrefix,
    type = type
  )
  
  res <- result$cohortDefinitionId
  names(res) <- result$names
  
  return(
    res
  )
}

getCmDiagAnalyses <- function(
  connectionHandler,
  resultsSchema, 
  tablePrefix
){
  
  sql <- "
    SELECT DISTINCT
      cma.analysis_id,
      cma.description as names
    FROM
      @results_schema.@table_prefixdiagnostics_summary cmds
      INNER JOIN 
      @results_schema.@table_prefixanalysis cma 
      ON cmds.analysis_id = cma.analysis_id
      ;
  "
  
  result <-  connectionHandler$queryDb(
    sql = sql,
    results_schema = resultsSchema,
    table_prefix = tablePrefix
  )
  
  res <- result$analysisId
  names(res) <- result$names
  
  return(
   res
  )
  
}


getCmDiagnosticsData <- function(
    connectionHandler, 
    resultsSchema, 
    tablePrefix, 
    cohortTablePrefix, 
    databaseTable,
    targetIds,
    outcomeIds,
    comparatorIds,
    analysisIds
) {
  sql <- "
    SELECT DISTINCT
      dmd.cdm_source_abbreviation database_name,
      cma.description analysis,
      cgcd1.cohort_name target,
      cgcd2.cohort_name comparator,
      cgcd3.cohort_name outcome,
      cmds.max_sdm,
      cmds.shared_max_sdm,
      cmds.equipoise,
      cmds.mdrr,
      cmds.attrition_fraction,
      cmds.ease,
      cmds.balance_diagnostic,
      cmds.shared_balance_diagnostic,
      cmds.equipoise_diagnostic,
      cmds.mdrr_diagnostic,
      cmds.attrition_diagnostic,
      cmds.ease_diagnostic,
      cmds.unblind
    FROM
      @results_schema.@table_prefixdiagnostics_summary cmds
      INNER JOIN @results_schema.@table_prefixanalysis cma ON cmds.analysis_id = cma.analysis_id
      INNER JOIN @results_schema.@database_table dmd ON dmd.database_id = cmds.database_id
      INNER JOIN @results_schema.@cohort_table_prefixcohort_definition cgcd1 ON cmds.target_id = cgcd1.cohort_definition_id
      INNER JOIN @results_schema.@cohort_table_prefixcohort_definition cgcd2 ON cmds.comparator_id = cgcd2.cohort_definition_id
      INNER JOIN @results_schema.@cohort_table_prefixcohort_definition cgcd3 ON cmds.outcome_id = cgcd3.cohort_definition_id
      
      where cgcd1.cohort_definition_id in (@targets)
      and cgcd2.cohort_definition_id in (@comparators)
      and cgcd3.cohort_definition_id in (@outcomes)
      and cma.analysis_id in (@analyses)
      ;
  "
  
  result <- connectionHandler$queryDb(
    sql = sql,
    results_schema = resultsSchema,
    table_prefix = tablePrefix,
    cohort_table_prefix = cohortTablePrefix,
    database_table = databaseTable,
    
    targets = paste0(targetIds, collapse = ','),
    comparators = paste0(comparatorIds, collapse = ','),
    outcomes = paste0(outcomeIds, collapse = ','),
    analyses = paste0(analysisIds, collapse = ',')
  )
  
  # adding percent fail for summary
  result$summaryValue <- apply(
    X = result[, grep('Diagnostic', colnames(result))], 
    MARGIN = 1, 
    FUN = function(x){
      
      if(sum(x %in% c('FAIL'))>0){
        return('Fail')
      } else if(sum(x %in% c('WARNING')) >0){
        return(sum(x %in% c('WARNING')))
      } else{
        return('Pass')
      }
    }
  )
  
  return(
    result
  )
}