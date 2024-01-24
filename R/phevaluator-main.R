# @file phevaluator-main.R
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



#' The location of the phevaluator module helper file
#' 
#' @details Returns the location of the cohort-generator helper file
#' 
#' @return String location of the phevaluator helper file
#' 
#' @export 
#'
phevaluatorHelperFile <- function() {
  fileLoc <-
    system.file('phevaluator-www', "phevaluator.html", package = "OhdsiShinyModules")
  return(fileLoc)
}


#' The viewer of the phevaluator module
#'
#' @param id The unique reference id for the module
#'
#' @return The user interface to the phevaluator results viewer
#' 
#' @export
#'
phevaluatorViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shinydashboard::box(
    status = 'info',
    width = "100%",
    title =  shiny::span(shiny::icon("gauge"), "PheValuator"),
    solidHeader = TRUE,
    
    shinydashboard::box(
      collapsible = TRUE,
      collapsed = FALSE,
      title = shiny::span( shiny::icon("circle-question"), "Help & Information"),
      width = "100%",
      shiny::htmlTemplate(system.file("phevaluator-www", "phevaluator.html", package = utils::packageName()))
    ),
    
    shinydashboard::box(
      collapsible = TRUE,
      collapsed = FALSE,
      title = shiny::span( shiny::icon("gear"), "Options"),
      width = "100%",
      shiny::uiOutput(ns('phevalOptionsSelector'))
    ),
    
    shiny::conditionalPanel(
      condition = "input.generate != 0",
      ns = ns,
      
      shiny::uiOutput(ns("inputsText")), 
      
      shiny::tabsetPanel(
        type = 'pills',
        id = ns('mainPanel'),
        
        shiny::tabPanel(
          title = "Phenotypes",
          resultTableViewer(ns("cohortDefinitionSetTable"),
                            downloadedFileName = "cohortDefinitionSetTable-")
        ),
        shiny::tabPanel(
          title = "Phenotype Performance Characteristics",
          resultTableViewer(ns("algorithmPerformanceResultsTable"),
                            downloadedFileName = "algorithmPerformanceResultsTable-")
        ),
        shiny::tabPanel(
          title = "Model Covariates",
          resultTableViewer(ns("modelCovariatesTable"),
                            downloadedFileName = "modelCovariatesTable-")
        ),
        shiny::tabPanel(
          title = "Model Covariate Summary",
          resultTableViewer(ns("modelCovariateSummaryTable"),
                            downloadedFileName = "modelCovariateSummaryTable-")
        ),
        shiny::tabPanel(
          title = "Model Performance",
          resultTableViewer(ns("modelPerformanceTable"),
                            downloadedFileName = "modelPerformanceTable-")
        ),
        shiny::tabPanel(
          title = "Model Input Parameters",
          resultTableViewer(ns("modelInputParametersTable"),
                            downloadedFileName = "modelInputParametersTable-")
        ),
        shiny::tabPanel(
          title = "Evaluation Cohort Diagnostics",
          resultTableViewer(ns("diagnosticsTable"),
                            downloadedFileName = "diagnosticsTable-")
        ),
        shiny::tabPanel(
          title = "Evaluation Cohort Parameters",
          resultTableViewer(ns("evaluationInputParametersTable"),
                            downloadedFileName = "evaluationInputParametersTable-")
        ),
        shiny::tabPanel(
          title = "Test Subjects",
          resultTableViewer(ns("testSubjectsTable"),
                            downloadedFileName = "testSubjectsTable-")
        ),
        shiny::tabPanel(
          title = "Test Subjects Covariates",
          resultTableViewer(ns("testSubjectsCovariatesTable"),
                            downloadedFileName = "testSubjectsCovariatesTable-")
        )
      )
    )
  )
}


#' The module server for the main phevaluator module
#'
#' @param id The unique reference id for the module
#' @param connectionHandler A connection to the database with the results
#' @param resultDatabaseSettings A named list containing the cohort generator results database details (schema, table prefix) 
#'
#' @return The phevaluator main module server
#' 
#' @export
#'

phevaluatorServer <- function(
  id, 
  connectionHandler, 
  resultDatabaseSettings
) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      withTooltip <- function(value, tooltip, ...) {
        shiny::div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
                   tippy::tippy(value, tooltip, ...))
      }
      
      #use algorithm performance table to get "option columns",
      #which will be used to make choices before generating result(s)
      optionCols <- getPhevalAlgorithmPerformance(
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings
      ) %>%
        dplyr::select("databaseId", "phenotype")
      
      databaseIds = unique(optionCols$databaseId)
      phenotypeNames = unique(optionCols$phenotype)
      
      #build the selector
      output$phevalOptionsSelector <- shiny::renderUI({
        
        shiny::fluidPage(
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shinyWidgets::pickerInput(
                inputId = session$ns('selectedDatabaseIds'), 
                label = 'Database(s):', 
                choices = databaseIds, 
                selected = databaseIds,
                choicesOpt = list(style = rep_len("color: black;", 999)),
                multiple = T,
                options = shinyWidgets::pickerOptions(
                  actionsBox = TRUE,
                  liveSearch = TRUE,
                  size = 10,
                  liveSearchStyle = "contains",
                  liveSearchPlaceholder = "Type here to search",
                  virtualScroll = 50
                ),
                width = "100%"
              )
            ),
            shiny::column(
              width = 6,
              shinyWidgets::pickerInput(
                inputId = session$ns('selectedPhenotypes'), 
                label = 'Phenotype(s):', 
                choices = phenotypeNames, 
                selected = phenotypeNames,
                choicesOpt = list(style = rep_len("color: black;", 999)),
                multiple = T,
                options = shinyWidgets::pickerOptions(
                  actionsBox = TRUE,
                  liveSearch = TRUE,
                  size = 10,
                  liveSearchStyle = "contains",
                  liveSearchPlaceholder = "Type here to search",
                  virtualScroll = 50
                ),
                width = "100%"
              )
            )
          ),
          shiny::actionButton(
            inputId = session$ns('generate'),
            label = 'Generate Results'
          )
        )
      })
      
      #if generate is pushed, extract the data
      dataAlgorithmPerformance <- shiny::eventReactive(         #we care about returning this value, so we use eventReactive
        eventExpr = input$generate,                     #could add complexity to event if desired
        {
          if (is.null(input$selectedDatabaseIds) |
              is.null(input$selectedPhenotypes)) {
            data.frame()
          }
          
          getPhevalAlgorithmPerformance(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings
          ) %>%
            dplyr::filter(.data$databaseId %in% input$selectedDatabaseIds & 
                            .data$phenotype %in% input$selectedPhenotypes)
          # %>%
          #   dplyr::select("databaseId":"cohortId", "description", "sensitivity95Ci":"analysisId")
        }
      )
      
      dataCohortDefinitionSet <- shiny::eventReactive(
        eventExpr = input$generate,                     
        {
          if (is.null(input$selectedDatabaseIds) |
              is.null(input$selectedPhenotypes)) {
            data.frame()
          }
          
          getPhevalCohortDefinitionSet(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings
          ) %>%
            #dplyr::mutate(buttonSQL = makeButtonLabel("SQL"),
            #              buttonJSON = makeButtonLabel("JSON")) %>%
            dplyr::filter(.data$phenotype %in% input$selectedPhenotypes)
        }
      )
      
      dataDiagnostics <- shiny::eventReactive(
        eventExpr = input$generate,                     
        {
          if (is.null(input$selectedDatabaseIds) |
              is.null(input$selectedPhenotypes)) {
            data.frame()
          }
          
          getPhevalDiagnostics(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings
          ) %>%
            dplyr::filter(
              .data$databaseId %in% input$selectedDatabaseIds & 
              .data$phenotype %in% input$selectedPhenotypes
              )
        }
      )
      
      dataEvalInputParams <- shiny::eventReactive(
        eventExpr = input$generate,                     
        {
          if (is.null(input$selectedDatabaseIds) |
              is.null(input$selectedPhenotypes)) {
            data.frame()
          }
          
          getPhevalEvalInputParams(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings
          ) %>%
            dplyr::filter(
              .data$databaseId %in% input$selectedDatabaseIds & 
              .data$phenotype %in% input$selectedPhenotypes
              )
        }
      )
      
      dataModelCovarSummary <- shiny::eventReactive(
        eventExpr = input$generate,                     
        {
          if (is.null(input$selectedDatabaseIds) |
              is.null(input$selectedPhenotypes)) {
            data.frame()
          }
          
          getPhevalModelCovarSummary(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings
          ) %>%
            dplyr::filter(
              .data$databaseId %in% input$selectedDatabaseIds & 
                .data$phenotype %in% input$selectedPhenotypes
            )
        }
      )
      
      dataModelCovars <- shiny::eventReactive(
        eventExpr = input$generate,                     
        {
          if (is.null(input$selectedDatabaseIds) |
              is.null(input$selectedPhenotypes)) {
            data.frame()
          }
          
          getPhevalModelCovars(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings
          ) %>%
            dplyr::filter(
              .data$databaseId %in% input$selectedDatabaseIds & 
              .data$phenotype %in% input$selectedPhenotypes
              )
        }
      )
      
      dataModelInputParams <- shiny::eventReactive(
        eventExpr = input$generate,                     
        {
          if (is.null(input$selectedDatabaseIds) |
              is.null(input$selectedPhenotypes)) {
            data.frame()
          }
          
          getPhevalModelInputParams(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings
          ) %>%
            dplyr::filter(
              .data$databaseId %in% input$selectedDatabaseIds & 
              .data$phenotype %in% input$selectedPhenotypes
              )
        }
      )
      
      dataModelPerformance <- shiny::eventReactive(
        eventExpr = input$generate,                     
        {
          if (is.null(input$selectedDatabaseIds) |
              is.null(input$selectedPhenotypes)) {
            data.frame()
          }
          
          getPhevalModelPerformance(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings
          ) %>%
            dplyr::filter(
              .data$databaseId %in% input$selectedDatabaseIds & 
              .data$phenotype %in% input$selectedPhenotypes
              )
        }
      )
      
      dataTestSubjects <- shiny::eventReactive(
        eventExpr = input$generate,                     
        {
          if (is.null(input$selectedDatabaseIds) |
              is.null(input$selectedPhenotypes)) {
            data.frame()
          }
          
          getPhevalTestSubjects(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings
          ) %>%
            dplyr::filter(
              .data$databaseId %in% input$selectedDatabaseIds & 
              .data$phenotype %in% input$selectedPhenotypes
              )
        }
      )
      
      dataTestSubjectsCovars <- shiny::eventReactive(
        eventExpr = input$generate,                     
        {
          if (is.null(input$selectedDatabaseIds) |
              is.null(input$selectedPhenotypes)) {
            data.frame()
          }

          getPhevalTestSubjectsCovars(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings
          ) %>%
            dplyr::filter(
              .data$databaseId %in% input$selectedDatabaseIds & 
              .data$phenotype %in% input$selectedPhenotypes
              )
        }
      )
      
      
      
      
      selectedInputs <- shiny::reactiveVal()
      output$inputsText <- shiny::renderUI(selectedInputs())
      
      #when generate is pushed, return as text what was selected
      shiny::observeEvent(
        eventExpr = input$generate,
        {
          selectedInputs(
            shinydashboard::box(
              status = 'warning',
              width = "100%",
              title = 'Selected:',
              shiny::div(shiny::fluidRow(
                shiny::column(
                  width = 8,
                  shiny::tags$b("Phenotype(s):"),
                  
                  paste(unique(optionCols$databaseId[optionCols$databaseId %in% input$selectedDatabaseIds]),
                        collapse = ', ')
                  
                ),
                shiny::column(
                  width = 4,
                  shiny::tags$b("Database(s):"),
                  paste(unique(optionCols$phenotype[optionCols$phenotype %in% input$selectedPhenotypes]),
                        collapse = ', ')
                )
              ))
            )
          )
        }
      )
      
      #read in custom column name colDef list from rds file, generated by 
      #heplers-componentsCreateCustomColDefList.R
      
      phevalColList <- ParallelLogger::loadSettingsFromJson(system.file("components-columnInformation",
                                                                        "phevaluator-colDefs.json",
                                                                        package = "OhdsiShinyModules")
      )
      
      #define custom colDefs for SQL and JSON buttons
      buttonColDefs <- list(
        buttonSQL = reactable::colDef(header = withTooltip("SQL", "Downloads SQL code for the cohort"),
                                html = T
                                ),
        buttonJSON = reactable::colDef(header = withTooltip("JSON", "Downloads JSON code for the cohort"),
                                 html = T
                                 ),
        sql = reactable::colDef(show = F),
        json = reactable::colDef(show = F)
      )
      
      #define custom column definitions and render the result table
      customColDefs <- utils::modifyList(phevalColList, buttonColDefs)

      
      resultTableServer(id = "algorithmPerformanceResultsTable",
                        df = dataAlgorithmPerformance,
                        colDefsInput = customColDefs,
                        downloadedFileName = "algorithmPerformanceResultsTable-")
      
      resultTableServer(id = "cohortDefinitionSetTable",
                        df = dataCohortDefinitionSet,
                        colDefsInput = customColDefs,
                        downloadedFileName = "cohortDefinitionSetTable-")
      
      resultTableServer(id = "diagnosticsTable",
                        df = dataDiagnostics,
                        colDefsInput = customColDefs,
                        downloadedFileName = "diagnosticsTable-")
      
      resultTableServer(id = "evaluationInputParametersTable",
                        df = dataEvalInputParams,
                        colDefsInput = customColDefs,
                        downloadedFileName = "evaluationInputParametersTable-")
      
      resultTableServer(id = "modelCovariateSummaryTable",
                        df = dataModelCovarSummary,
                        colDefsInput = customColDefs,
                        downloadedFileName = "modelCovariateSummaryTable-")
      
      resultTableServer(id = "modelCovariatesTable",
                        df = dataModelCovars,
                        colDefsInput = customColDefs,
                        downloadedFileName = "modelCovariatesTable-")
      
      resultTableServer(id = "modelInputParametersTable",
                        df = dataModelInputParams,
                        colDefsInput = customColDefs,
                        downloadedFileName = "modelInputParametersTable-")
      
      resultTableServer(id = "modelPerformanceTable",
                        df = dataModelPerformance,
                        colDefsInput = customColDefs,
                        downloadedFileName = "modelPerformanceTable-")
      
      resultTableServer(id = "testSubjectsTable",
                        df = dataTestSubjects,
                        colDefsInput = customColDefs,
                        downloadedFileName = "testSubjectsTable-")
      
      resultTableServer(id = "testSubjectsCovariatesTable",
                        df = dataTestSubjectsCovars,
                        colDefsInput = customColDefs,
                        downloadedFileName = "testSubjectsCovariatesTable-")
      
      return(invisible(NULL))
      
    })
}

#add databaseId and phenotype as args into the function
#pass these into the sql code with 'where'

getPhevalAlgorithmPerformance <- function(
    connectionHandler, 
    resultDatabaseSettings
) {
  
  sql <- "SELECT * FROM @schema.@pv_table_prefixALGORITHM_PERFORMANCE_RESULTS
  ;"
  
  return(
    connectionHandler$queryDb(
      sql = sql,
      schema = resultDatabaseSettings$schema,
      pv_table_prefix = resultDatabaseSettings$pvTablePrefix
    )
  )
}

#test it

# databaseIds = c("CCAE_RS", "Germany_RS")
# phenotypes = c("hyperprolactinemia")
# 
# getPhevalAlgorithmPerformance(connectionHandler = connectionHandler,
#                           resultsSchema = resultDatabaseDetails$schema,
#                           tablePrefix = resultDatabaseDetails$tablePrefix
#                           )


getPhevalCohortDefinitionSet <- function(
    connectionHandler, 
    resultDatabaseSettings
) {
  
  sql <- "SELECT * FROM @schema.@pv_table_prefixCOHORT_DEFINITION_SET
  ;"
  
  return(
    connectionHandler$queryDb(
      sql = sql,
      schema = resultDatabaseSettings$schema,
      pv_table_prefix = resultDatabaseSettings$pvTablePrefix
    )
  )
}

getPhevalDiagnostics <- function(
    connectionHandler, 
    resultDatabaseSettings
) {
  
  sql <- "SELECT * FROM @schema.@pv_table_prefixDIAGNOSTICS
  ;"
  return(
    connectionHandler$queryDb(
      sql = sql,
      schema = resultDatabaseSettings$schema,
      pv_table_prefix = resultDatabaseSettings$pvTablePrefix
    )
  )
}

getPhevalEvalInputParams <- function(
    connectionHandler, 
    resultDatabaseSettings
) {
  
  sql <- "SELECT * FROM @schema.@pv_table_prefixEVALUATION_INPUT_PARAMETERS
  ;"
  return(
    connectionHandler$queryDb(
      sql = sql,
      schema = resultDatabaseSettings$schema,
      pv_table_prefix = resultDatabaseSettings$pvTablePrefix
    )
  )
}

getPhevalModelCovars <- function(
    connectionHandler, 
    resultDatabaseSettings
) {
  
  sql <- "SELECT * FROM @schema.@pv_table_prefixMODEL_COVARIATES
  ;"
  
  df <-  connectionHandler$queryDb(
    sql = sql,
    schema = resultDatabaseSettings$schema,
    pv_table_prefix = resultDatabaseSettings$pvTablePrefix
  )
  
  df$databaseId = stringi::stri_trans_general(df$databaseId, "latin-ascii")
  df$phenotype = stringi::stri_trans_general(df$phenotype, "latin-ascii")
  df$analysisName = stringi::stri_trans_general(df$analysisName, "latin-ascii")
  df$covariateName = stringi::stri_trans_general(df$covariateName, "latin-ascii")
  
  return(
    df
  )
}

getPhevalModelCovarSummary <- function(
    connectionHandler, 
    resultDatabaseSettings
) {
  
  sql <- "SELECT * FROM @schema.@pv_table_prefixMODEL_COVARIATE_SUMMARY
  ;"
  
  df <-  connectionHandler$queryDb(
    sql = sql, 
    schema = resultDatabaseSettings$schema,
    pv_table_prefix = resultDatabaseSettings$pvTablePrefix
  )
  
  df$databaseId = stringi::stri_trans_general(df$databaseId, "latin-ascii")
  df$phenotype = stringi::stri_trans_general(df$phenotype, "latin-ascii")
  df$analysisName = stringi::stri_trans_general(df$analysisName, "latin-ascii")
  df$covariateName = stringi::stri_trans_general(df$covariateName, "latin-ascii")
  
  return(
    df
  )
}

# d <- getPhevalModelCovars(connectionHandler = connectionHandler,
#                                    resultsSchema = resultDatabaseDetails$schema,
#                                    tablePrefix = resultDatabaseDetails$tablePrefix,
#                                    databaseIds = databaseIds,
#                                    phenotypes = phenotypes
# )



getPhevalModelInputParams <- function(
    connectionHandler, 
    resultDatabaseSettings
) {
  
  sql <- "SELECT * FROM @schema.@pv_table_prefixMODEL_INPUT_PARAMETERS
  ;"
  return(
    connectionHandler$queryDb(
      sql = sql,
      schema = resultDatabaseSettings$schema,
      pv_table_prefix = resultDatabaseSettings$pvTablePrefix
    )
  )
}

getPhevalModelPerformance <- function(
    connectionHandler, 
    resultDatabaseSettings
) {
  
  sql <- "SELECT * FROM @schema.@pv_table_prefixMODEL_PERFORMANCE
  ;"
  return(
    connectionHandler$queryDb(
      sql = sql,
      schema = resultDatabaseSettings$schema,
      pv_table_prefix = resultDatabaseSettings$pvTablePrefix
    )
  )
}

getPhevalTestSubjects <- function(
    connectionHandler, 
    resultDatabaseSettings
) {
  
  sql <- "SELECT * FROM @schema.@pv_table_prefixTEST_SUBJECTS
  ;"
  return(
    connectionHandler$queryDb(
      sql = sql,
      schema = resultDatabaseSettings$schema,
      pv_table_prefix = resultDatabaseSettings$pvTablePrefix
    )
  )
}

getPhevalTestSubjectsCovars <- function(
    connectionHandler, 
    resultDatabaseSettings
) {
  
  sql <- "SELECT * FROM @schema.@pv_table_prefixTEST_SUBJECTS_COVARIATES
  ;"
  
  df <- connectionHandler$queryDb(
    sql = sql,
    schema = resultDatabaseSettings$schema,
    pv_table_prefix = resultDatabaseSettings$pvTablePrefix
  )
  
  df$databaseId = stringi::stri_trans_general(df$databaseId, "latin-ascii")
  df$phenotype = stringi::stri_trans_general(df$phenotype, "latin-ascii")
  df$analysisName = stringi::stri_trans_general(df$analysisName, "latin-ascii")
  df$type = stringi::stri_trans_general(df$type, "latin-ascii")
  df$covariateName = stringi::stri_trans_general(df$covariateName, "latin-ascii")
  
  return(
    df
  )
  
}








