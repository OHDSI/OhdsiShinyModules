# @file cohort-method-main.R
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


#' The location of the cohort method module helper file
#'
#' @details
#' Returns the location of the cohort method helper file
#' 
#' @return
#' string location of the cohort method helper file
#'
#' @export
cohortMethodHelperFile <- function(){
  fileLoc <- system.file('cohort-method-www', "cohort-method.html", package = "OhdsiShinyModules")
  return(fileLoc)
}

#' The viewer of the main cohort method module
#'
#' @param id the unique reference id for the module
#'
#' @return
#' The user interface to the cohort method results viewer
#' 
#' @export
cohortMethodViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shinydashboard::box(
    status = 'info', 
    width = 12,
    title = shiny::span( shiny::icon("chart-column"), 'Cohort Method'),
    solidHeader = TRUE,
    
  #shiny::fluidPage(style = "width:1500px;",
  shinydashboard::box(
    collapsible = TRUE,
    collapsed = TRUE,
    title = "Cohort Method Evidence Explorer",
    width = "100%"#,
    #shiny::htmlTemplate(system.file("cohort-diagnostics-www", "cohortCounts.html", package = utils::packageName()))
  ),
  
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
              type = 'pills',
              id = ns("mainTabsetPanel"),
              shiny::tabPanel(
                title = "Diagnostics",
                cohortMethodDiagnosticsSummaryViewer(ns("estimationDiganostics"))
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
                                cohortMethodResultsTableViewer(ns("resultsTable")),
                         shiny::conditionalPanel("output.rowIsSelected == true", ns = ns,
                                                 shiny::tabsetPanel(id = ns("detailsTabsetPanel"),
                                                                    shiny::tabPanel(title = "Power",
                                                                                    cohortMethodPowerViewer(ns("power"))
                                                      ),
                                                      shiny::tabPanel(title = "Attrition",
                                                                      cohortMethodAttritionViewer(ns("attrition"))
                                                      ),
                                                      shiny::tabPanel(title = "Population characteristics",
                                                                      cohortMethodPopulationCharacteristicsViewer(ns("popCharacteristics"))
                                                      ),
                                                      shiny::tabPanel(title = "Propensity model",
                                                                      cohortMethodPropensityModelViewer(ns("propensityModel"))
                                                      ),
                                                      shiny::tabPanel(title = "Propensity scores",
                                                                      cohortMethodPropensityScoreDistViewer(ns("propensityScoreDist"))
                                                      ),
                                                      shiny::tabPanel(title = "Covariate balance",
                                                                      cohortMethodCovariateBalanceViewer(ns("covariateBalance"))
                                                      ),
                                                      shiny::tabPanel(title = "Systematic error",
                                                                      cohortMethodSystematicErrorViewer(ns("systematicError"))
                                                      ),
                                                      shiny::tabPanel(title = "Forest plot",
                                                                      cohortMethodForestPlotViewer(ns("forestPlot"))
                                                      ),
                                                      shiny::tabPanel(title = "Kaplan-Meier",
                                                                      cohortMethodKaplanMeierViewer(ns("kaplanMeier"))
                                                      ),
                                                      shiny::tabPanel(title = "Subgroups",
                                                                      cohortMethodSubgroupsViewer(ns("subgroups"))
                                                      )
                                                      
                                          ) # end tabsetPanel
                         ) # end conditionalPanel
                  )
                  
                ) 
              )
            )
  )
  
}


#' The module server for the main cohort method module
#'
#' @param id the unique reference id for the module
#' @param connectionHandler a connection to the database with the results
#' @param resultDatabaseSettings a named list containing the PLE results database connection details
#'
#' @return
#' the PLE results viewer main module server
#' 
#' @export
cohortMethodServer <- function(
    id, 
    connectionHandler, 
    resultDatabaseSettings
    ) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      dataFolder <- NULL
      
      output$targetWidget <- shiny::renderUI({
        targets <- getCohortMethodTargetChoices(connectionHandler,
                                              resultDatabaseSettings$schema,
                                              resultDatabaseSettings$tablePrefix,
                                              resultDatabaseSettings$cohortTablePrefix)
        shiny::selectInput(inputId = session$ns("target"),
                           label = "Target",
                           choices = getCohortMethodSelectNamedChoices(targets$targetId,
                                                                     targets$cohortName))
      })
      
      output$comparatorWidget <- shiny::renderUI({
        comparators <- getCohortMethodComparatorChoices(connectionHandler,
                                                      resultDatabaseSettings$schema,
                                                      resultDatabaseSettings$tablePrefix,
                                                      resultDatabaseSettings$cohortTablePrefix)
        shiny::selectInput(inputId = session$ns("comparator"),
                           label = "Comparator",
                           choices = getCohortMethodSelectNamedChoices(comparators$comparatorId,
                                                                     comparators$cohortName))
      })
      
      output$outcomeWidget <- shiny::renderUI({
        outcomes <- getCohortMethodOutcomeChoices(connectionHandler,
                                                resultDatabaseSettings$schema,
                                                resultDatabaseSettings$tablePrefix,
                                                resultDatabaseSettings$cohortTablePrefix)
        shiny::selectInput(inputId = session$ns("outcome"),
                           label = "Outcome",
                           choices = getCohortMethodSelectNamedChoices(outcomes$outcomeId,
                                                                     outcomes$cohortName))
      })
      output$databaseWidget<- shiny::renderUI({
        databases <- getCohortMethodDatabaseChoices(connectionHandler,
                                                  resultDatabaseSettings$schema,
                                                  resultDatabaseSettings$tablePrefix,
                                                  resultDatabaseSettings$databaseTable)
        shiny::checkboxGroupInput(inputId = session$ns("database"),
                                  label = "Data source",
                                  choices =  getCohortMethodSelectNamedChoices(databases$databaseId,
                                                                             databases$cdmSourceAbbreviation),
                                  selected = unique(databases$databaseId))
      })
      output$analysisWidget <- shiny::renderUI({
        analyses <- getCmAnalysisOptions(connectionHandler,
                                         resultDatabaseSettings$schema,
                                         resultDatabaseSettings$tablePrefix)
        shiny::checkboxGroupInput(inputId = session$ns("analysis"),
                                  label = "Analysis",
                                  choices =  getCohortMethodSelectNamedChoices(analyses$analysisId,
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
      
      
      cohortMethodDiagnosticsSummaryServer(id = "estimationDiganostics",
                                         connectionHandler = connectionHandler,
                                         resultsSchema = resultDatabaseSettings$schema,
                                         tablePrefix = resultDatabaseSettings$tablePrefix,
                                         cohortTablePrefix = resultDatabaseSettings$cohortTablePrefix,
                                         databaseTable = resultDatabaseSettings$databaseTable)
      
      
      selectedRow <- cohortMethodResultsTableServer(id = "resultsTable",
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
      
      
      cohortMethodPowerServer(id = "power",
                            selectedRow = selectedRow,
                            inputParams = inputParams,
                            connectionHandler = connectionHandler,
                            resultsSchema = resultDatabaseSettings$schema,
                            resultDatabaseSettings$tablePrefix)
      
      cohortMethodAttritionServer(id = "attrition",
                                selectedRow = selectedRow,
                                inputParams = inputParams,
                                connectionHandler = connectionHandler,
                                resultsSchema = resultDatabaseSettings$schema,
                                tablePrefix = resultDatabaseSettings$tablePrefix,
                                databaseTable = resultDatabaseSettings$cohortTablePrefix)
      
      cohortMethodPopulationCharacteristicsServer(id = "popCharacteristics",
                                                selectedRow = selectedRow,
                                                inputParams = inputParams,
                                                connectionHandler = connectionHandler,
                                                resultsSchema = resultDatabaseSettings$schema,
                                                tablePrefix = resultDatabaseSettings$tablePrefix)
      
      cohortMethodPropensityModelServer(id = "propensityModel",
                                      selectedRow = selectedRow,
                                      inputParams = inputParams,
                                      connectionHandler = connectionHandler,
                                      resultsSchema = resultDatabaseSettings$schema,
                                      tablePrefix = resultDatabaseSettings$tablePrefix)
      
      cohortMethodPropensityScoreDistServer(id = "propensityScoreDist",
                                          selectedRow = selectedRow,
                                          inputParams = inputParams,
                                          connectionHandler = connectionHandler,
                                          resultsSchema = resultDatabaseSettings$schema,
                                          tablePrefix = resultDatabaseSettings$tablePrefix,
                                          cohortTablePrefix = resultDatabaseSettings$cohortTablePrefix)
      
      cohortMethodCovariateBalanceServer(id = "covariateBalance",
                                       selectedRow = selectedRow,
                                       inputParams = inputParams,
                                       connectionHandler = connectionHandler,
                                       resultsSchema = resultDatabaseSettings$schema,
                                       tablePrefix = resultDatabaseSettings$tablePrefix)
      
      cohortMethodSystematicErrorServer(id = "systematicError",
                                      selectedRow = selectedRow,
                                      inputParams = inputParams,
                                      connectionHandler = connectionHandler,
                                      resultsSchema = resultDatabaseSettings$schema,
                                      tablePrefix = resultDatabaseSettings$tablePrefix)
      
      cohortMethodKaplanMeierServer(id = "kaplanMeier",
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
      cohortMethodSubgroupsServer(id = "subgroups",
                                selectedRow = selectedRow,
                                inputParams = inputParams)
      
    }
  )
}

