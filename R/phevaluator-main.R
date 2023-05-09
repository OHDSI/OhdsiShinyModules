# @file description-main.R
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
            
            resultTableViewer(ns("cohortDefinitionSetTable"))
          ),
        
        shiny::tabPanel(
          title = "Model Input Parameters",

            
            resultTableViewer(ns("modelInputParametersTable"))
          ),
        
        shiny::tabPanel(
          title = "Model Performance",

            resultTableViewer(ns("modelPerformanceTable"))
          ),
        
        shiny::tabPanel(
          title = "Model Covariates",
          
            resultTableViewer(ns("modelCovariatesTable"))
          ),
        
        shiny::tabPanel(
          title = "Evaluation Cohort Parameters",
          
            resultTableViewer(ns("evaluationCohortParametersTable"))
          ),
        
        shiny::tabPanel(
          title = "Test Subjects and Covariates",
          
            resultTableViewer(ns("testSubjectsCovariatesTable"))
          ),
        
        shiny::tabPanel(
          title = "Phenotype Performance Characteristics",
          
            resultTableViewer(ns("algorithmPerformanceResultsTable"))
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
      
      withTooltip <- function(value, tooltip, ...) {
        shiny::div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
                   tippy::tippy(value, tooltip, ...))
      }
      
      optionCols <- getPhevalAlgorithmPerformance(
        connectionHandler = connection,
        resultsSchema = resultDatabaseDetails$schema,
        tablePrefix = resultDatabaseDetails$tablePrefix
      ) %>%
        dplyr::select(databaseId, phenotype)
      
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
      
      
      dataAlgorithmPerformance <- shiny::eventReactive(         #we care about returning this value, so we use eventReactive
        eventExpr = input$generate,                     #could add complexity to event if desired
        {
          if (is.null(input$selectedDatabaseIds) |
              is.null(input$selectedPhenotypes)) {
            data.frame()
          }
          
          getPhevalAlgorithmPerformance(
            connectionHandler = connection,
            resultsSchema = resultDatabaseDetails$schema,
            tablePrefix = resultDatabaseDetails$tablePrefix
          ) %>%
            dplyr::filter(databaseId %in% input$selectedDatabaseIds &
                            phenotype %in% input$selectedPhenotypes
            )
        }
      )
      
      
      selectedInputs <- shiny::reactiveVal()
      output$inputsText <- shiny::renderUI(selectedInputs())
      
      shiny::observeEvent(
        eventExpr = input$generate,
        {
          
          # if(length(input$selectedCohortName) == 0 | is.null(input$selectedDatabaseId |
          #                                                    is.null(input$selectedModeId))){
          #   print('Null ids value')
          #   return(invisible(NULL))
          # }
          
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
                        collapse = ',')
                  
                ),
                shiny::column(
                  width = 4,
                  shiny::tags$b("Database(s):"),
                  paste(unique(optionCols$phenotype[optionCols$phenotype %in% input$selectedPhenotypes]),
                        collapse = ',')
                )
              ))
            )
          )
        }
      )
          
          
      custom_colDefs <- list(
        covariateId = reactable::colDef(
          header = withTooltip("Covariate ID",
                               "Unique identifier of the covariate"
        )),
        covariateName = reactable::colDef(
          header = withTooltip(
          "Covariate Name",
          "The name of the covariate"
        )),
        analysisName = reactable::colDef(
          header = withTooltip(
          "Covariate Class",
          "Class/type of the covariate"
        ))
      )
          
          
          resultTableServer(id = "algorithmPerformanceResultsTable",
                            df = dataAlgorithmPerformance,
                            colDefsInput = custom_colDefs)
          
          return(invisible(NULL))
          
})
}









