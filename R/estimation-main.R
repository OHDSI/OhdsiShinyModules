# @file estimation-main.R
#
# Copyright 2022 Observational Health Data Sciences and Informatics
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




#' The viewer of the main estimation module
#'
#' @param id the unique reference id for the module
#'
#' @return
#' The user interface to the estimation results viewer
#' 
#' @export
estimationViewer <- function(id) {
  ns <- shiny::NS(id)
  
  fluidPage(style = "width:1500px;",
            estimationTitlePanelViewer(ns("titlePanel")),
            tags$head(tags$style(type = "text/css", "
             #loadmessage {
                                 position: fixed;
                                 top: 0px;
                                 left: 0px;
                                 width: 100%;
                                 padding: 5px 0px 5px 0px;
                                 text-align: center;
                                 font-weight: bold;
                                 font-size: 100%;
                                 color: #000000;
                                 background-color: #ADD8E6;
                                 z-index: 105;
                                 }
                                 ")),
            conditionalPanel(id = ns("loadmessage"),
                             condition = "$('html').hasClass('shiny-busy')",
                             tags$div("Processing...")),
            tabsetPanel(
              id = ns("mainTabsetPanel"),
              tabPanel(
                title = "Diagnostics",
                estimationDiagnosticsSummaryViewer(ns("estimationDiganostics"))
              ),
              tabPanel(
                title = "Results",
                fluidRow(
                  column(width = 3,
                         uiOutput(outputId = ns("targetWidget")),
                         uiOutput(outputId = ns("comparatorWidget")),
                         uiOutput(outputId = ns("outcomeWidget")),
                         uiOutput(outputId = ns("databaseWidget")),
                         uiOutput(outputId = ns("analysisWidget"))
                  ),
                  column(width = 9,
                         estimationResultsTableViewer(ns("resultsTable")),
                         conditionalPanel("output.rowIsSelected == true", ns = ns,
                                          tabsetPanel(id = ns("detailsTabsetPanel"),
                                                      tabPanel(title = "Power",
                                                               estimationPowerViewer(ns("power"))
                                                      ),
                                                      tabPanel(title = "Attrition",
                                                               estimationAttritionViewer(ns("attrition"))
                                                      ),
                                                      tabPanel(title = "Population characteristics",
                                                               estimationPopulationCharacteristicsViewer(ns("popCharacteristics"))
                                                      ),
                                                      tabPanel(title = "Propensity model",
                                                               estimationPropensityModelViewer(ns("propensityModel"))
                                                      ),
                                                      tabPanel(title = "Propensity scores",
                                                               estimationPropensityScoreDistViewer(ns("propensityScoreDist"))
                                                      ),
                                                      tabPanel(title = "Covariate balance",
                                                               estimationCovariateBalanceViewer(ns("covariateBalance"))
                                                      ),
                                                      tabPanel(title = "Systematic error",
                                                               estimationSystematicErrorViewer(ns("systematicError"))
                                                      ),
                                                      tabPanel(title = "Forest plot",
                                                               estimationForestPlotViewer(ns("forestPlot"))
                                                      ),
                                                      tabPanel(title = "Kaplan-Meier",
                                                               estimationKaplanMeierViewer(ns("kaplanMeier"))
                                                      ),
                                                      tabPanel(title = "Subgroups",
                                                               estimationSubgroupsViewer(ns("subgroups"))
                                                      )
                                                      
                                          ) # end tabsetPanel
                         ) # end conditionalPanel
                  )
                  
                ) 
              )
            )
  )
  
}


#' The module server for the main estimation module
#'
#' @param id the unique reference id for the module
#' @param resultDatabaseSettings a named list containing the PLE results database connection details
#' @param resultsSchema the schema with the PLE results
#'
#' @return
#' the PLE results viewer main module server
#' 
#' @export
estimationServer <- function(id,
                             resultDatabaseSettings,
                             resultsSchema = "poc") {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      estimationConnectionDetails <- resultDatabaseSettings
      
      estimationTitlePanelServer("titlePanel")
      
      connection <- NULL
      dataFolder <- NULL
      if (is.null(estimationConnectionDetails$server) ||
          (is.list(estimationConnectionDetails$server) && length(estimationConnectionDetails$server) == 0)) {
        assign("dataFolder", estimationConnectionDetails$dataFolder, envir = .GlobalEnv)
        
        loadEstimationData(estimationConnectionDetails$dataFolder)
      } else {
        connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = estimationConnectionDetails$dbms,
                                                                        user = estimationConnectionDetails$user,
                                                                        password = estimationConnectionDetails$password,
                                                                        server = sprintf("%s/%s", estimationConnectionDetails$server,
                                                                                         estimationConnectionDetails$database))
        
        
        connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
      }
      
      
      shiny::onStop(function() {
        if (DBI::dbIsValid(connection)) {
          DatabaseConnector::disconnect(connection)
        }
      })
      
      output$targetWidget <- shiny::renderUI({
        targets <- getEstimationTargetChoices(connection, resultsSchema)
        shiny::selectInput(inputId = session$ns("target"),
                           label = "Target",
                           choices = getEstimationSelectNamedChoices(targets$targetId,
                                                                     targets$cohortName))
      })
      
      output$comparatorWidget <- shiny::renderUI({
        comparators <- getEstimationComparatorChoices(connection, resultsSchema)
        shiny::selectInput(inputId = session$ns("comparator"),
                           label = "Comparator",
                           choices = getEstimationSelectNamedChoices(comparators$comparatorId,
                                                                     comparators$cohortName))
      })
      
      output$outcomeWidget <- shiny::renderUI({
        outcomes <- getEstimationOutcomeChoices(connection, resultsSchema)
        shiny::selectInput(inputId = session$ns("outcome"),
                           label = "Outcome",
                           choices = getEstimationSelectNamedChoices(outcomes$outcomeId,
                                                                     outcomes$cohortName))
      })
      output$databaseWidget<- shiny::renderUI({
        databases <- getEstimationDatabaseChoices(connection, resultsSchema)
        shiny::checkboxGroupInput(inputId = session$ns("database"),
                                  label = "Data source",
                                  choices =  getEstimationSelectNamedChoices(databases$databaseId,
                                                                             databases$cdmSourceAbbreviation),
                                  selected = unique(databases$databaseId))
      })
      output$analysisWidget <- shiny::renderUI({
        analyses <- getCmAnalysisOptions(connection, resultsSchema)
        shiny::checkboxGroupInput(inputId = session$ns("analysis"),
                                  label = "Analysis",
                                  choices =  getEstimationSelectNamedChoices(analyses$analysisId,
                                                                             analyses$description),
                                  selected = unique(analyses$analysisId))
      })
      
      
      inputParams <- reactive({
        t <- list()
        t$target <- input$target
        t$comparator <- input$comparator
        t$outcome <- input$outcome
        t$analysis <- input$analysis
        t$database <- input$database
        return(t)
      })
      
      
      estimationDiagnosticsSummaryServer("estimationDiganostics", connection, resultsSchema)
      
      
      selectedRow <- estimationResultsTableServer("resultsTable", connection, inputParams, resultsSchema)
      
      output$rowIsSelected <- shiny::reactive({
        return(!is.null(selectedRow()))
      })
      
      
      if (!exists("cmInteractionResult")) {
        #TODO: update for testing once subgroup analysis completed
        shiny::hideTab(inputId = "detailsTabsetPanel", target = "Subgroups",
                       session = session)
      }
      
      outputOptions(output, "rowIsSelected", suspendWhenHidden = FALSE)
      
      output$isMetaAnalysis <- shiny::reactive({
        #TODO: update once MA implemented
        row <- selectedRow()
        isMetaAnalysis <- FALSE # !is.null(row) && (row$databaseId %in% metaAnalysisDbIds)
        if (!is.null(row)) {
          if (isMetaAnalysis) {
            hideTab("detailsTabsetPanel", "Attrition", session = session)
            hideTab("detailsTabsetPanel", "Population characteristics", session = session)
            hideTab("detailsTabsetPanel", "Kaplan-Meier", session = session)
            hideTab("detailsTabsetPanel", "Propensity model", session = session)
            showTab("detailsTabsetPanel", "Forest plot", session = session)
          } else {
            showTab("detailsTabsetPanel", "Attrition", session = session)
            showTab("detailsTabsetPanel", "Population characteristics", session = session)
            if (row$unblind) {
              showTab("detailsTabsetPanel", "Kaplan-Meier", session = session)
            } else{
              shiny::hideTab("detailsTabsetPanel", "Kaplan-Meier", session = session)
            }
            showTab("detailsTabsetPanel", "Propensity model", session = session)
            hideTab("detailsTabsetPanel", "Forest plot", session = session)
          }
        }
        return(isMetaAnalysis)
      })
      shiny::outputOptions(output, "isMetaAnalysis", suspendWhenHidden = FALSE)
      
      
      estimationPowerServer("power", selectedRow, inputParams, connection, resultsSchema)
      
      estimationAttritionServer("attrition", selectedRow, inputParams, connection, resultsSchema)
      
      estimationPopulationCharacteristicsServer("popCharacteristics", selectedRow, inputParams, connection, resultsSchema)
      
      estimationPropensityModelServer("propensityModel", selectedRow, inputParams, connection, resultsSchema)
      
      estimationPropensityScoreDistServer("propensityScoreDist", selectedRow, inputParams, connection, resultsSchema)
      
      estimationCovariateBalanceServer("covariateBalance", selectedRow, inputParams, connection, resultsSchema)
      
      estimationSystematicErrorServer("systematicError", selectedRow, inputParams, connection, resultsSchema)
      
      estimationKaplanMeierServer("kaplanMeier", selectedRow, inputParams, connection, resultsSchema)
      
      #TODO: complete once MA implemented
      # estimationForestPlotServer("forestPlot", selectedRow, inputParams)
      
      #TODO: revisit once subgroup example conducted
      estimationSubgroupsServer("subgroups", selectedRow, inputParams)
      
    }
  )
}

