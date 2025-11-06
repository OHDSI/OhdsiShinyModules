# @file report-main.R
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


#' The location of the report module helper file
#'
#' @details
#' Returns the location of the report helper file
#' @family {Report}
#' @return
#' string location of the report helper file
#'
#' @export
reportHelperFile <- function(){
  fileLoc <- system.file('report-www', "report.html", package = "OhdsiShinyModules")
  return(fileLoc)
}

#' The module viewer for the shiny app report module
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @family Report
#' @return
#' The user interface to the home page module
#'
#' @export
reportViewer <- function(
    id = 'reportModule'
    ) {
  ns <- shiny::NS(id)
  
  shinydashboard::box(
    status = 'info', 
    width = 12,
    title =  shiny::span( shiny::icon('book'), "Report Generator"),
    solidHeader = TRUE,
    
    # Code to select the report 
    shiny::uiOutput(ns('reportSelection')),
    
    #==============
    # The full report with a conditional panel to only show if selected
    shiny::conditionalPanel(
      condition = 'output.showFullReport != 0',
      ns = ns,
      
      fullReportViewer(
        id = ns('fullReport')
      )
    ),
    #==============
    
    #==============
    # The prediction report with a conditional panel to only show if selected
    shiny::conditionalPanel(
      condition = 'output.showPredictionReport != 0',
      ns = ns,
      
      predictionReportViewer(
        id = ns('predictionReport')
      )
    )
    #==============  
    
  )
}


#' The module server for the shiny app report module
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param connectionHandler a connection to the database with the results
#' @param resultDatabaseSettings a list containing the characterization result schema, dbms, tablePrefix, databaseTable and cgTablePrefix
#' @param server server for the connection to the results for quarto
#' @param username username for the connection to the results for quarto
#' @param password password for the connection to the results for quarto
#' @param dbms dbms for the connection to the results for quarto
#' @family Report
#' @return
#' The server for the shiny app home
#'
#' @export
reportServer <- function(
    id = 'reportModule',
    connectionHandler = NULL,
    resultDatabaseSettings = NULL,
    server = Sys.getenv("RESULTS_SERVER"), 
    username = Sys.getenv("RESULTS_USER"), 
    password = Sys.getenv("RESULTS_PASSWORD"), 
    dbms = Sys.getenv("RESULTS_DBMS")
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # code to select the report
      output$reportSelection <- shiny::renderUI(
        shiny::shinyUI(
          shiny::selectInput(
            inputId = session$ns('reportToShow'), 
            label = "Pick a report", 
            choices = c("Full Report","Prediction Report"), 
            selected = "Full Report", 
            multiple = FALSE 
          )
        )
      )
      
      # update the outputs that conditionally show the report viewers
      output$showFullReport <- shiny::reactive(1) # defaults so shows
      output$showPredictionReport <- shiny::reactive(0)
      shiny::outputOptions(output, "showFullReport", suspendWhenHidden = FALSE)
      shiny::outputOptions(output, "showPredictionReport", suspendWhenHidden = FALSE)
      
      shiny::observeEvent(input$reportToShow,{
        
        if(input$reportToShow == "Full Report"){
          output$showFullReport <- shiny::reactive(1) 
          output$showPredictionReport <- shiny::reactive(0)
        } else if(input$reportToShow == "Prediction Report"){
          output$showFullReport <- shiny::reactive(0) 
          output$showPredictionReport <- shiny::reactive(1)
        }
          else{
            output$showFullReport <- shiny::reactive(0) 
            output$showPredictionReport <- shiny::reactive(0)
        }
        
      }
      )
      
      
      # Full report server
      #==================
      fullReportServer(
        id = 'fullReport',
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings,
        server = server, 
        username = username, 
        password = password, 
        dbms = dbms
        )
      #==================
      
      # Prediciton report server
      #==================
      predictionReportServer(
        id = 'predictionReport',
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings,
        server = server, 
        username = username, 
        password = password, 
        dbms = dbms
        )
      #==================
      
      
    }
  )
}


