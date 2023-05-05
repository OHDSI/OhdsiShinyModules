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
phevaluatorViewer <- function(id = "phevaluatorViewer") {
  ns <- shiny::NS(id)
  
  shinydashboard::box(
    status = 'info',
    width = "100%",
    title =  shiny::span(shiny::icon("gauge"), "PheValuator"),
    solidHeader = TRUE,
    
    shiny::tabsetPanel(
      type = 'pills',
      id = ns('mainPanel'),
      
      shiny::tabPanel(
        title = "Phenotypes",
        shinydashboard::box(
          width = "100%",
          title = 'Options',
          collapsible = TRUE,
          collapsed = F,
          shiny::uiOutput(ns('cohortDefinitionSetInputs'))
        ),
        
        shiny::conditionalPanel(
          condition = "input.generate != 0",
          ns = ns,
          
          shiny::uiOutput(ns("cohortDefinitionSetInputsText")),
          
          reactable::reactableOutput(outputId = ns("cohortDefinitionSetTable"))
        ),
      ),

      shiny::tabPanel(
        title = "Model Input Parameters",
        shinydashboard::box(
          width = "100%",
          title = 'Options',
          collapsible = TRUE,
          collapsed = F,
          shiny::uiOutput(ns('modelInputParametersInputs'))
        ),
        
        shiny::conditionalPanel(
          condition = "input.generate != 0",
          ns = ns,
          
          shiny::uiOutput(ns("modelInputParametersInputsText")),
          
          reactable::reactableOutput(outputId = ns("modelInputParametersTable"))
        ),
      ),
      
      shiny::tabPanel(
        title = "Model Performance",
        shinydashboard::box(
          width = "100%",
          title = 'Options',
          collapsible = TRUE,
          collapsed = F,
          shiny::uiOutput(ns('modelPerformanceInputs'))
        ),
        
        shiny::conditionalPanel(
          condition = "input.generate != 0",
          ns = ns,
          
          shiny::uiOutput(ns("modelPerformanceInputsText")),
          
          reactable::reactableOutput(outputId = ns("modelPerformanceTable"))
        ),
      ),
      
      shiny::tabPanel(
        title = "Model Covariates",
        shinydashboard::box(
          width = "100%",
          title = 'Options',
          collapsible = TRUE,
          collapsed = F,
          shiny::uiOutput(ns('modelCovariatesInputs'))
        ),
        
        shiny::conditionalPanel(
          condition = "input.generate != 0",
          ns = ns,
          
          shiny::uiOutput(ns("modelCovariatesInputsText")),
          
          reactable::reactableOutput(outputId = ns("modelCovariatesTable"))
        ),
      ),
      
      shiny::tabPanel(
        title = "Evaluation Cohort Parameters",
        shinydashboard::box(
          width = "100%",
          title = 'Options',
          collapsible = TRUE,
          collapsed = F,
          shiny::uiOutput(ns('evaluationCohortParametersInputs'))
        ),
        
        shiny::conditionalPanel(
          condition = "input.generate != 0",
          ns = ns,
          
          shiny::uiOutput(ns("evaluationCohortParametersInputsText")),
          
          reactable::reactableOutput(outputId = ns("evaluationCohortParametersTable"))
        ),
      ),
      
      shiny::tabPanel(
        title = "Test Subjects and Covariates",
        shinydashboard::box(
          width = "100%",
          title = 'Options',
          collapsible = TRUE,
          collapsed = F,
          shiny::uiOutput(ns('testSubjectsCovariatesInputs'))
        ),
        
        shiny::conditionalPanel(
          condition = "input.generate != 0",
          ns = ns,
          
          shiny::uiOutput(ns("testSubjectsCovariatesInputsText")),
          
          reactable::reactableOutput(outputId = ns("testSubjectsCovariatesTable"))
        ),
      ),
      
      shiny::tabPanel(
        title = "Phenotype Performance Characteristics",
        shinydashboard::box(
          width = "100%",
          title = 'Options',
          collapsible = TRUE,
          collapsed = F,
          shiny::uiOutput(ns('algorithmPerformanceResultsInputs'))
        ),
        
        shiny::conditionalPanel(
          condition = "input.generate != 0",
          ns = ns,
          
          shiny::uiOutput(ns("algorithmPerformanceResultsInputsText")),
          
          reactable::reactableOutput(outputId = ns("algorithmPerformanceResultsTable"))
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
  id = "phevaluatorServer", 
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
    }
  )
}










