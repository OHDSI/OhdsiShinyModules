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


#' The location of the estimation module helper file
#'
#' @details
#' Returns the location of the estimation helper file
#' 
#' @return
#' string location of the estimation helper file
#'
#' @export
estimationHelperFile <- function(){
  fileLoc <- system.file('estimation-www', "estimation.html", package = "OhdsiShinyModules")
  return(fileLoc)
}

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
  
  shiny::fluidPage(style = "width:1500px;",
            estimationTitlePanelViewer(ns("titlePanel")),
            htmltools::tags$head(htmltools::tags$style(type = "text/css", "
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
            shiny::conditionalPanel(id = ns("loadmessage"),
                             condition = "$('html').hasClass('shiny-busy')",
                             htmltools::tags$div("Processing...")),
            shiny::tabsetPanel(
              id = ns("mainTabsetPanel"),
              shiny::tabPanel(
                title = "Diagnostics",
                estimationDiagnosticsSummaryViewer(ns("estimationDiganostics"))
              ),
              shiny::tabPanel(
                title = "Results",
                shiny::fluidRow(
                  shiny::column(width = 3,
                         shiny::uiOutput(outputId = ns("targetWidget")),
                         shiny::uiOutput(outputId = ns("comparatorWidget")),
                         shiny::uiOutput(outputId = ns("outcomeWidget")),
                         shiny::uiOutput(outputId = ns("databaseWidget")),
                         shiny::uiOutput(outputId = ns("analysisWidget"))
                  ),
                  shiny::column(width = 9,
                         estimationResultsTableViewer(ns("resultsTable")),
                         shiny::conditionalPanel("output.rowIsSelected == true", ns = ns,
                                                 shiny::tabsetPanel(id = ns("detailsTabsetPanel"),
                                                                    shiny::tabPanel(title = "Power",
                                                               estimationPowerViewer(ns("power"))
                                                      ),
                                                      shiny::tabPanel(title = "Attrition",
                                                               estimationAttritionViewer(ns("attrition"))
                                                      ),
                                                      shiny::tabPanel(title = "Population characteristics",
                                                               estimationPopulationCharacteristicsViewer(ns("popCharacteristics"))
                                                      ),
                                                      shiny::tabPanel(title = "Propensity model",
                                                               estimationPropensityModelViewer(ns("propensityModel"))
                                                      ),
                                                      shiny::tabPanel(title = "Propensity scores",
                                                               estimationPropensityScoreDistViewer(ns("propensityScoreDist"))
                                                      ),
                                                      shiny::tabPanel(title = "Covariate balance",
                                                               estimationCovariateBalanceViewer(ns("covariateBalance"))
                                                      ),
                                                      shiny::tabPanel(title = "Systematic error",
                                                               estimationSystematicErrorViewer(ns("systematicError"))
                                                      ),
                                                      shiny::tabPanel(title = "Forest plot",
                                                               estimationForestPlotViewer(ns("forestPlot"))
                                                      ),
                                                      shiny::tabPanel(title = "Kaplan-Meier",
                                                               estimationKaplanMeierViewer(ns("kaplanMeier"))
                                                      ),
                                                      shiny::tabPanel(title = "Subgroups",
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
#' @param connectionHandler a connection to the database with the results
#' @param resultDatabaseSettings a named list containing the PLE results database connection details
#'
#' @return
#' the PLE results viewer main module server
#' 
#' @export
estimationServer <- function(
    id, 
    connectionHandler, 
    resultDatabaseSettings
    ) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      estimationTitlePanelServer(id = "titlePanel")
      
      dataFolder <- NULL
      
      output$targetWidget <- shiny::renderUI({
        targets <- getEstimationTargetChoices(connectionHandler,
                                              resultDatabaseSettings$schema,
                                              resultDatabaseSettings$tablePrefix,
                                              resultDatabaseSettings$cohortTablePrefix)
        shiny::selectInput(inputId = session$ns("target"),
                           label = "Target",
                           choices = getEstimationSelectNamedChoices(targets$targetId,
                                                                     targets$cohortName))
      })
      
      output$comparatorWidget <- shiny::renderUI({
        comparators <- getEstimationComparatorChoices(connectionHandler,
                                                      resultDatabaseSettings$schema,
                                                      resultDatabaseSettings$tablePrefix,
                                                      resultDatabaseSettings$cohortTablePrefix)
        shiny::selectInput(inputId = session$ns("comparator"),
                           label = "Comparator",
                           choices = getEstimationSelectNamedChoices(comparators$comparatorId,
                                                                     comparators$cohortName))
      })
      
      output$outcomeWidget <- shiny::renderUI({
        outcomes <- getEstimationOutcomeChoices(connectionHandler,
                                                resultDatabaseSettings$schema,
                                                resultDatabaseSettings$tablePrefix,
                                                resultDatabaseSettings$cohortTablePrefix)
        shiny::selectInput(inputId = session$ns("outcome"),
                           label = "Outcome",
                           choices = getEstimationSelectNamedChoices(outcomes$outcomeId,
                                                                     outcomes$cohortName))
      })
      output$databaseWidget<- shiny::renderUI({
        databases <- getEstimationDatabaseChoices(connectionHandler,
                                                  resultDatabaseSettings$schema,
                                                  resultDatabaseSettings$tablePrefix,
                                                  resultDatabaseSettings$databaseTable)
        shiny::checkboxGroupInput(inputId = session$ns("database"),
                                  label = "Data source",
                                  choices =  getEstimationSelectNamedChoices(databases$databaseId,
                                                                             databases$cdmSourceAbbreviation),
                                  selected = unique(databases$databaseId))
      })
      output$analysisWidget <- shiny::renderUI({
        analyses <- getCmAnalysisOptions(connectionHandler,
                                         resultDatabaseSettings$schema,
                                         resultDatabaseSettings$tablePrefix)
        shiny::checkboxGroupInput(inputId = session$ns("analysis"),
                                  label = "Analysis",
                                  choices =  getEstimationSelectNamedChoices(analyses$analysisId,
                                                                             analyses$description),
                                  selected = unique(analyses$analysisId))
      })
      
      
      inputParams <- shiny::reactive({
        t <- list()
        t$target <- input$target
        t$comparator <- input$comparator
        t$outcome <- input$outcome
        t$analysis <- input$analysis
        t$database <- input$database
        return(t)
      })
      
      
      estimationDiagnosticsSummaryServer(id = "estimationDiganostics",
                                         connectionHandler = connectionHandler,
                                         resultsSchema = resultDatabaseSettings$schema,
                                         tablePrefix = resultDatabaseSettings$tablePrefix,
                                         cohortTablePrefix = resultDatabaseSettings$cohortTablePrefix,
                                         databaseTable = resultDatabaseSettings$databaseTable)
      
      
      selectedRow <- estimationResultsTableServer(id = "resultsTable",
                                                  connectionHandler = connectionHandler,
                                                  inputParams = inputParams,
                                                  resultsSchema = resultDatabaseSettings$schema,
                                                  tablePrefix = resultDatabaseSettings$tablePrefix,
                                                  databaseTable = resultDatabaseSettings$databaseTable)
      
      output$rowIsSelected <- shiny::reactive({
        return(!is.null(selectedRow()))
      })
      
      
      if (!exists("cmInteractionResult")) { # ISSUE: this should be an input resultDatabaseSettings$cmInteractionResult and not null check
        #TODO: update for testing once subgroup analysis completed
        shiny::hideTab(inputId = "detailsTabsetPanel", target = "Subgroups",
                       session = session)
      }
      
      shiny::outputOptions(output, "rowIsSelected", suspendWhenHidden = FALSE)
      
      output$isMetaAnalysis <- shiny::reactive({
        #TODO: update once MA implemented
        row <- selectedRow()
        isMetaAnalysis <- FALSE # !is.null(row) && (row$databaseId %in% metaAnalysisDbIds)
        if (!is.null(row)) {
          if (isMetaAnalysis) {
            shiny::hideTab("detailsTabsetPanel", "Attrition", session = session)
            shiny::hideTab("detailsTabsetPanel", "Population characteristics", session = session)
            shiny::hideTab("detailsTabsetPanel", "Kaplan-Meier", session = session)
            shiny::hideTab("detailsTabsetPanel", "Propensity model", session = session)
            shiny::showTab("detailsTabsetPanel", "Forest plot", session = session)
          } else {
            shiny::showTab("detailsTabsetPanel", "Attrition", session = session)
            shiny::showTab("detailsTabsetPanel", "Population characteristics", session = session)
            if (row$unblind) {
              shiny::showTab("detailsTabsetPanel", "Kaplan-Meier", session = session)
            } else{
              shiny::hideTab("detailsTabsetPanel", "Kaplan-Meier", session = session)
            }
            shiny::showTab("detailsTabsetPanel", "Propensity model", session = session)
            shiny::hideTab("detailsTabsetPanel", "Forest plot", session = session)
          }
        }
        return(isMetaAnalysis)
      })
      shiny::outputOptions(output, "isMetaAnalysis", suspendWhenHidden = FALSE)
      
      
      estimationPowerServer(id = "power",
                            selectedRow = selectedRow,
                            inputParams = inputParams,
                            connectionHandler = connectionHandler,
                            resultsSchema = resultDatabaseSettings$schema,
                            resultDatabaseSettings$tablePrefix)
      
      estimationAttritionServer(id = "attrition",
                                selectedRow = selectedRow,
                                inputParams = inputParams,
                                connectionHandler = connectionHandler,
                                resultsSchema = resultDatabaseSettings$schema,
                                tablePrefix = resultDatabaseSettings$tablePrefix,
                                databaseTable = resultDatabaseSettings$cohortTablePrefix)
      
      estimationPopulationCharacteristicsServer(id = "popCharacteristics",
                                                selectedRow = selectedRow,
                                                inputParams = inputParams,
                                                connectionHandler = connectionHandler,
                                                resultsSchema = resultDatabaseSettings$schema,
                                                tablePrefix = resultDatabaseSettings$tablePrefix)
      
      estimationPropensityModelServer(id = "propensityModel",
                                      selectedRow = selectedRow,
                                      inputParams = inputParams,
                                      connectionHandler = connectionHandler,
                                      resultsSchema = resultDatabaseSettings$schema,
                                      tablePrefix = resultDatabaseSettings$tablePrefix)
      
      estimationPropensityScoreDistServer(id = "propensityScoreDist",
                                          selectedRow = selectedRow,
                                          inputParams = inputParams,
                                          connectionHandler = connectionHandler,
                                          resultsSchema = resultDatabaseSettings$schema,
                                          tablePrefix = resultDatabaseSettings$tablePrefix,
                                          cohortTablePrefix = resultDatabaseSettings$cohortTablePrefix)
      
      estimationCovariateBalanceServer(id = "covariateBalance",
                                       selectedRow = selectedRow,
                                       inputParams = inputParams,
                                       connectionHandler = connectionHandler,
                                       resultsSchema = resultDatabaseSettings$schema,
                                       tablePrefix = resultDatabaseSettings$tablePrefix)
      
      estimationSystematicErrorServer(id = "systematicError",
                                      selectedRow = selectedRow,
                                      inputParams = inputParams,
                                      connectionHandler = connectionHandler,
                                      resultsSchema = resultDatabaseSettings$schema,
                                      tablePrefix = resultDatabaseSettings$tablePrefix)
      
      estimationKaplanMeierServer(id = "kaplanMeier",
                                  selectedRow = selectedRow,
                                  inputParams = inputParams,
                                  connectionHandler = connectionHandler,
                                  resultsSchema = resultDatabaseSettings$schema,
                                  tablePrefix = resultDatabaseSettings$tablePrefix,
                                  cohortTablePrefix = resultDatabaseSettings$cohortTablePrefix,
                                  databaseTable = resultDatabaseSettings$databaseTable)
      
      #TODO: complete once MA implemented
      # estimationForestPlotServer("forestPlot", connection, selectedRow, inputParams)
      
      #TODO: revisit once subgroup example conducted
      estimationSubgroupsServer(id = "subgroups",
                                selectedRow = selectedRow,
                                inputParams = inputParams)
      
    }
  )
}

