# @file patient-level-prediction-main.R
#
# Copyright 2025 Observational Health Data Sciences and Informatics
#
# This file is part of PatientLevelPrediction
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


#' The location of the prediction module helper file
#'
#' @details
#' Returns the location of the prediction helper file
#' @family PatientLevelPrediction
#' @return
#' string location of the prediction helper file
#'
#' @export
patientLevelPredictionHelperFile <- function(){
  fileLoc <- system.file('patient-level-prediction-www', "patient-level-prediction.html", package = "OhdsiShinyModules")
  return(fileLoc)
}

#' The module viewer for exploring PatientLevelPrediction
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @family PatientLevelPrediction
#' @return
#' The user interface to the PatientLevelPrediction viewer module
#'
#' @export
patientLevelPredictionViewer <- function(id=1) {
  ns <- shiny::NS(id)
  
  shinydashboard::box(
    status = 'info', width = 12,
    title =  shiny::span( shiny::icon("chart-line"), "Prediction Viewer"),
    solidHeader = TRUE,
    
    tableSelectionViewer(id = ns('performance-select')),
    
    shiny::conditionalPanel(
      condition = "output.showPredResults != 0", 
      ns = ns,
      
      # add drop down to select resultView (model, cal, disc, netben, threshold)
      
      shiny::uiOutput(ns('viewPredictionOptions')),
      
      shiny::tabsetPanel(
        type = "hidden",
        id = ns('resultTab'),
        
        shiny::tabPanel(
          title = 'View Models', 
          shiny::helpText('This lets users explore the models'),
          patientLevelPredictionModelViewer(id = ns('prediction-models'))
          ),
        shiny::tabPanel(
          title = 'Generate Plots', 
          shiny::helpText('This lets users compare common prediction plots (e.g., discrimination, calibration, net benefit) across one or more models'),
          patientLevelPredictionPlotViewer(id = ns('prediction-plots'))
          ),
        shiny::tabPanel(
          title = 'Generate Heatmap', 
          shiny::helpText('This lets users plot large scale prediction impacts'),
          patientLevelPredictionHeatmapViewer(id = ns('prediction-heatmap'))
        ),
        shiny::tabPanel(
          title = 'View Threshold Performances', 
          shiny::helpText("Explore the models' performances (e.g., precision, sensitivity, specificity) at different predictive thresholds."),
          patientLevelPredictionCutoffViewer(id = ns('prediction-cutoff'))
          ),
        shiny::tabPanel(
          title = 'View Diagnostics', 
          shiny::helpText('View model design and development diagnostics (based on PROBAST).'),
          patientLevelPredictionDiagnosticsViewer(id = ns('prediction-diagnostics'))
          )
      )
      
    )
        
  ) # end box
  
}

#' The module server for exploring PatientLevelPrediction
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param connectionHandler a connection to the database with the results
#' @param resultDatabaseSettings a list containing the prediction result schema and connection details
#' @family PatientLevelPrediction
#' @return
#' The server for the PatientLevelPrediction module
#'
#' @export
patientLevelPredictionServer <- function(
    id, 
    connectionHandler,
    resultDatabaseSettings = list(port = 1)
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      #   VIEW SETTINGS
      # =============================
      output$showPredResults <- shiny::reactive(0)
      shiny::outputOptions(output, "showPredResults", suspendWhenHidden = FALSE)
      
      performanceRowId <- shiny::reactiveVal(0)
      performances <- OhdsiReportGenerator::getFullPredictionPerformances(
        connectionHandler = connectionHandler, 
        schema = resultDatabaseSettings$schema,
        plpTablePrefix = resultDatabaseSettings$plpTablePrefix, 
        cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
        databaseTable = resultDatabaseSettings$databaseTable, 
        databaseTablePrefix = resultDatabaseSettings$databaseTablePrefix
      ) %>%
        dplyr::relocate("developmentTargetName") %>%
        dplyr::relocate("developmentOutcomeName", .after = "developmentTargetName") %>%
        dplyr::relocate("developmentTimeAtRisk", .after = "developmentOutcomeName") %>%
        dplyr::relocate("modelDesignId", .after = "developmentTimeAtRisk") %>%
        dplyr::arrange(.data$developmentTargetName, .data$developmentOutcomeName, .data$developmentTimeAtRisk)
      
      
      tableSelectionServer(
        id = 'performance-select', 
        table = shiny::reactive({performances}), # must be reactive
        selectedRowId = performanceRowId, # must be reactive
        helpText = 'Click the button to select model performances to explore',
        selectMultiple = TRUE,
        inputColumns = list(
          performanceId = reactable::colDef(name = 'Performance ID',show = TRUE),
          modelDesignId = reactable::colDef(name = 'Model ID'),
          developmentDatabaseId = reactable::colDef(show = FALSE),
          validationDatabaseId  = reactable::colDef(show = FALSE),
          developmentTargetId = reactable::colDef(show = FALSE),
          developmentTargetName = reactable::colDef(
            name = 'Dev. Target', 
            width = 200,
            filterInput = function(values, name) {
              shiny::tags$select(
                # Set to undefined to clear the filter
                onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", session$ns('performance-select-id'),name),
                # "All" has an empty value to clear the filter, and is the default option
                shiny::tags$option(value = "", "All"),
                lapply(unique(values), shiny::tags$option),
                "aria-label" = sprintf("Filter %s", name),
                style = "width: 100%; height: 28px;"
              )
            }
          ),
          developmentOutcomeId = reactable::colDef(show = FALSE),
          developmentOutcomeName = reactable::colDef(
            name = 'Dev. Outcome',
            width = 200,
            filterInput = function(values, name) {
              shiny::tags$select(
                # Set to undefined to clear the filter
                onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", session$ns('performance-select-id'),name),
                # "All" has an empty value to clear the filter, and is the default option
                shiny::tags$option(value = "", "All"),
                lapply(unique(values), shiny::tags$option),
                "aria-label" = sprintf("Filter %s", name),
                style = "width: 100%; height: 28px;"
              )
            }
          ),
          developmentDatabase = reactable::colDef(
            name = 'Dev. Database',
            filterInput = function(values, name) {
              shiny::tags$select(
                # Set to undefined to clear the filter
                onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", session$ns('performance-select-id'),name),
                # "All" has an empty value to clear the filter, and is the default option
                shiny::tags$option(value = "", "All"),
                lapply(unique(values), shiny::tags$option),
                "aria-label" = sprintf("Filter %s", name),
                style = "width: 100%; height: 28px;"
              )
            }),
          validationDatabase = reactable::colDef(name = 'Val. Database'),
          validationTargetName = reactable::colDef(name = 'Val. Target'),
          validationTargetId = reactable::colDef(show = FALSE),
          validationOutcomeName = reactable::colDef(name = 'Val. Outcome'),
          validationOutcomeId = reactable::colDef(show = FALSE),
          timeStamp = reactable::colDef(show = FALSE),
          validationTimeAtRisk = reactable::colDef(
            name = 'Val. TAR'
            ),
          developmentTimeAtRisk = reactable::colDef(
            name = 'Dev. TAR',
            filterInput = function(values, name) {
              shiny::tags$select(
                # Set to undefined to clear the filter
                onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", session$ns('performance-select-id'),name),
                # "All" has an empty value to clear the filter, and is the default option
                shiny::tags$option(value = "", "All"),
                lapply(unique(values), shiny::tags$option),
                "aria-label" = sprintf("Filter %s", name),
                style = "width: 100%; height: 28px;"
              )
            }),
          evaluation = reactable::colDef(
            name = 'Evaluation', 
            aggregate = "frequency",
            filterInput = function(values, name) {
              shiny::tags$select(
                # Set to undefined to clear the filter
                onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", session$ns('performance-select-id'),name),
                # "All" has an empty value to clear the filter, and is the default option
                shiny::tags$option(value = "", "All"),
                lapply(unique(values), shiny::tags$option),
                "aria-label" = sprintf("Filter %s", name),
                style = "width: 100%; height: 28px;"
              )
            }),
          populationSize = reactable::colDef(
            name = 'N', 
            aggregate = "sum",
            filterMethod = reactable::JS("function(rows, columnId, filterValue) {
        return rows.filter(function(row) {
          return row.values[columnId] >= filterValue
        })
      }")
            ),
          outcomeCount = reactable::colDef(name = 'Outcomes', aggregate = "sum"),
          AUROC = reactable::colDef(
            name = 'AUROC', 
            aggregate = 'mean',
            format = reactable::colFormat(digits = 3),
            filterMethod = reactable::JS("function(rows, columnId, filterValue) {
        return rows.filter(function(row) {
          return row.values[columnId] >= filterValue
        })
      }")
          ),
          `95% lower AUROC` = reactable::colDef(
            name = '95%LB', 
            aggregate = 'mean',
            format = reactable::colFormat(digits = 3),
            filterMethod = reactable::JS("function(rows, columnId, filterValue) {
        return rows.filter(function(row) {
          return row.values[columnId] >= filterValue
        })
      }")
          ),
          `95% upper AUROC` = reactable::colDef(
            name = '95%UB',
            aggregate = 'mean',
            format = reactable::colFormat(digits = 3),
            filterMethod = reactable::JS("function(rows, columnId, filterValue) {
        return rows.filter(function(row) {
          return row.values[columnId] >= filterValue
        })
      }")
          ),
          AUPRC = reactable::colDef(
            name = 'AUPRC', 
            aggregate = 'mean',
            format = reactable::colFormat(digits = 3),
            filterMethod = reactable::JS("function(rows, columnId, filterValue) {
        return rows.filter(function(row) {
          return row.values[columnId] >= filterValue
        })
      }")
          ),
          `brier score` = reactable::colDef(
            name = 'Brier', 
            aggregate = 'mean',
            format = reactable::colFormat(digits = 3)
          ),
          `brier score scaled` = reactable::colDef(show = FALSE),
          `Average Precision` = reactable::colDef(show = FALSE),
          
          `Eavg` = reactable::colDef(
            name = 'Eavg', 
            aggregate = 'mean',
            format = reactable::colFormat(digits = 3)
          ),
          `E90` = reactable::colDef(show = FALSE),
          `Emax` = reactable::colDef(show = FALSE),
          `calibrationInLarge mean prediction` = reactable::colDef(
            name = 'mean pred', 
            aggregate = 'mean',
            format = reactable::colFormat(digits = 3)
          ),
          `calibrationInLarge observed risk` = reactable::colDef(
            name = 'observed risk', 
            aggregate = 'mean',
            format = reactable::colFormat(digits = 3)
          ),
          `calibrationInLarge intercept` = reactable::colDef(show = FALSE),
          `weak calibration intercept` = reactable::colDef(
            name = 'Intercept', 
            aggregate = 'mean',
            format = reactable::colFormat(digits = 2)
          ),
          `weak calibration gradient` = reactable::colDef(
            name = 'Gradient', 
            aggregate = 'mean',
            format = reactable::colFormat(digits = 2)
          ),
          `Hosmer-Lemeshow calibration intercept` = reactable::colDef(show = FALSE),
          `Hosmer-Lemeshow calibration gradient` = reactable::colDef(show = FALSE)
        ),
        #displayColumns = inputColumns,
        groupBy = NULL, 
        columnGroups = list(
          reactable::colGroup(
            name = 'AUROC', 
            columns = c("AUROC","95% lower AUROC","95% upper AUROC")
          ),
          reactable::colGroup(
            name = 'Calibration In Large', 
            columns = c("calibrationInLarge observed risk","calibrationInLarge mean prediction")
          ),
          reactable::colGroup(
            name = 'Weak calibration', 
            columns = c("weak calibration gradient","weak calibration intercept")
          )
        ),
        elementId = session$ns('performance-select-id'),
        selectButtonText = 'Select Model Performance/s',
        tableReset = shiny::reactive(0)
      )
      
      output$viewPredictionOptions <- shiny::renderUI({
        
        shinyWidgets::pickerInput(
          inputId = session$ns('tabView'),
          label = 'Result: ',
          choices = c('View Models', 
                      'Generate Plots',
                      'Generate Heatmap',
                      'View Threshold Performances', 
                      'View Diagnostics'
                      ),
          selected = 'View Models',
          multiple = F,
          options = shinyWidgets::pickerOptions(
            actionsBox = TRUE,
            liveSearch = TRUE,
            dropupAuto = F,
            size = 10,
            liveSearchStyle = "contains",
            liveSearchPlaceholder = "Type here to search",
            virtualScroll = 500
          )
        )
        
      })
      
      
      # swap to view based on selected dropdown
      shiny::observeEvent(input$tabView, {
        shiny::updateTabsetPanel(
          session = session,
          inputId = 'resultTab', 
          selected = input$tabView
        )
        
      })
      
      shiny::observeEvent(performanceRowId(),{
        
        if(max(performanceRowId()) != 0){
          # show the prediction results viewer
          output$showPredResults <- shiny::reactive(1)
        } else{
          output$showPredResults <- shiny::reactive(0)
        }
        
      })
      
      # add all the servers here
      
      patientLevelPredictionPlotServer(
        id = 'prediction-plots',
        performances = shiny::reactive({performances}),
        performanceRowIds = performanceRowId,
        connectionHandler = connectionHandler, 
        resultDatabaseSettings = resultDatabaseSettings
        )
      
      patientLevelPredictionModelServer(
        id = 'prediction-models',
        performances = shiny::reactive({performances}),
        performanceRowIds = performanceRowId,
        connectionHandler = connectionHandler, 
        resultDatabaseSettings = resultDatabaseSettings
      )
      
      patientLevelPredictionHeatmapServer(
        id = 'prediction-heatmap',
        performances = shiny::reactive({performances}),
        performanceRowIds = performanceRowId,
        connectionHandler = connectionHandler, 
        resultDatabaseSettings = resultDatabaseSettings
      )
      
      patientLevelPredictionCutoffServer(
        id = 'prediction-cutoff',
        performances = shiny::reactive({performances}),
        performanceRowIds = performanceRowId,
        connectionHandler = connectionHandler, 
        resultDatabaseSettings = resultDatabaseSettings
      )
      
      patientLevelPredictionDiagnosticsServer(
        id = 'prediction-diagnostics',
        performances = shiny::reactive({performances}),
        performanceRowIds = performanceRowId,
        connectionHandler = connectionHandler, 
        resultDatabaseSettings = resultDatabaseSettings
      )
      
    }
  )
}



