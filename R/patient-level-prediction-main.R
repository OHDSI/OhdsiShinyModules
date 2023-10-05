# @file patient-level-prediction-main.R
#
# Copyright 2023 Observational Health Data Sciences and Informatics
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
#' 
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
#' 
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
    
    shiny::tabsetPanel(
      type = 'hidden',#'pills',
      id = ns('allView'),
      
      shiny::tabPanel(
        "Model Designs Summary",  
        
        
        infoHelperViewer(
          id = "helper",
          helpLocation= system.file("patient-level-prediction-www", "help-designSummary.html", package = utils::packageName())
        ),
        
        patientLevelPredictionDesignSummaryViewer(ns('designSummaryTab'))
      ),
      
      shiny::tabPanel(
        "Models Summary",  
        shiny::actionButton(
          inputId = ns("backToDesignSummary"), 
          label = "Back To Design Summary",
          shiny::icon("arrow-left"), 
          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
        ),
        patientLevelPredictionModelSummaryViewer(ns('modelSummaryTab'))
      ),
      
      shiny::tabPanel(
        "Diagnostic Summary",  
        shiny::actionButton(
          inputId = ns("backToDesignSummaryD"), 
          label = "Back To Design Summary",
          shiny::icon("arrow-left"), 
          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
        ),
        patientLevelPredictionDiagnosticsViewer(ns('diagnostics'))
      ),
      
      shiny::tabPanel(
        "Explore Selected Model",
        
        shiny::actionButton(
          inputId = ns("backToModelSummary"), 
          label = "Back To Models Summary",
          shiny::icon("arrow-left"), 
          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
        ),
        
        patientLevelPredictionResultsViewer(ns('prediction-results'))
        
        
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
#' 
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

      # go back button 
      shiny::observeEvent(input$backToModelSummary, {
        shiny::updateTabsetPanel(
          session = session,
          inputId = 'allView',
          selected = 'Models Summary'
        )

            })
      
      shiny::observeEvent(input$backToDesignSummary, {
        shiny::updateTabsetPanel(
          session = session,
          inputId = 'allView',
          selected = 'Model Designs Summary'
        )
      })
      
      shiny::observeEvent(input$backToDesignSummaryD, {
        shiny::updateTabsetPanel(
          session = session,
          inputId = 'allView',
          selected = 'Model Designs Summary'
        )
      })
      
      # =============================
      #   MODEL DESIGNS
      # =============================
      
      # the first server is the design server
      # this shows the number of developed models
      # per prediction model design and lets the user
      # select a model design id to explore
      modelDesignId <- shiny::reactiveVal()
      designSummary <- patientLevelPredictionDesignSummaryServer(
        id = 'designSummaryTab',
        connectionHandler = connectionHandler, 
        resultDatabaseSettings = resultDatabaseSettings
      )
      
      
      # change to model summary tab when 
      # a model design id is select that shows 
      # a summary table with all the models 
      # developed using the chosen model design
      shiny::observeEvent(designSummary$modelDesignId(), {
        modelDesignId(designSummary$modelDesignId())
        if(!is.null(designSummary$modelDesignId())){
          shiny::updateTabsetPanel(session, "allView", selected = "Models Summary")
          }
      })
      
      
      # =============================
      #   MODEL SUMMARY
      # =============================
      
      # this server lets the user select a model
      # of the chosen model design to explore in detail
      # this results a performance id and 
      # development database id
      performanceId <- shiny::reactiveVal()
      developmentDatabaseId <- shiny::reactiveVal()
      performance <- patientLevelPredictionModelSummaryServer(
          id = 'modelSummaryTab', 
          connectionHandler = connectionHandler,  
          modelDesignId = modelDesignId,
          resultDatabaseSettings = resultDatabaseSettings
        )

      
      # change to single model explore tab when 
      # a performance id is selected that shows 
      # the internal valdiation and external
      # validation details of the selected model
      shiny::observeEvent(performance$performanceId(), {
        performanceId(performance$performanceId())
        developmentDatabaseId(performance$developmentDatabaseId())
        if(!is.null(performance$performanceId())){
          shiny::updateTabsetPanel(session, "allView", selected = "Explore Selected Model")
          }
        
      })
      
      
      # =============================
      #   DIAGNOSTICS
      # =============================
      # diagnostic viewer - show model diagnostic results
      shiny::observeEvent(designSummary$diagnosticId(), {
        if(!is.null(designSummary$diagnosticId())){
          shiny::updateTabsetPanel(session, "allView", selected = "Diagnostic Summary")
        }
      })
      
      patientLevelPredictionDiagnosticsServer(
        id = 'diagnostics', 
        modelDesignId = designSummary$diagnosticId, 
        connectionHandler = connectionHandler, 
        resultDatabaseSettings = resultDatabaseSettings
      )
      
      # =============================
      #   PROTOCOL
      # =============================
      # protocol viewer - show protocol 
      shiny::observeEvent(designSummary$reportId(), {
        
        if(!is.null(designSummary$reportId())){
          
          #check protocol generator packages installed for this
          # could make this interactive in shiny
          if(!is_installed("CirceR")){
            shiny::showNotification("Need to install CirceR for this to work: remotes::install_github('OHDSI/CirceR')")
          }
          if(!is_installed("kableExtra")){
            shiny::showNotification("Need to install kableExtra for this to work: install.packages('kableExtra')")
          }
          if(!is_installed("knitr")){
            shiny::showNotification("Need to install knitr for this to work: install.packages('kableExtra')")
          }
          
          #protocolOutputLoc <- tempdir()
          protocolOutputLoc <- getwd()
          
          if(file.exists(file.path(protocolOutputLoc, 'main.html'))){
            file.remove(file.path(protocolOutputLoc, 'main.html'))
          }
          tryCatch(
            {createPredictionProtocol( 
              connectionHandler = connectionHandler, 
              resultDatabaseSettings = resultDatabaseSettings,
              modelDesignId = designSummary$reportId(),
              output = protocolOutputLoc,
              intermediatesDir = file.path(protocolOutputLoc, 'plp-prot')
            )
            }, error = function(e){
              shiny::showNotification(
                paste('error generating protocol:',e)
              )
            }
          )
             
          if(file.exists(file.path(protocolOutputLoc, 'main.html'))){
            # display the generated html report
            shiny::showModal(shiny::modalDialog(
              title = "Report",
              shiny::div(
                shiny::textInput(
                  inputId = session$ns('plpProtocolDownload'), 
                  label = 'Download Protocol Location:', 
                  placeholder = '/Users/jreps/Documents'
                ),
                shiny::actionButton(
                  inputId = session$ns('downloadButton'), 
                  label = 'Download'
                ),
                shiny::includeHTML(file.path(protocolOutputLoc, 'main.html'))
              ), 
              size = "l",
              easyClose = T
            ))
          }
        }
        
      })
      
      # add code to save protocol to file based on input$downloadButton
      shiny::observeEvent(input$downloadButton, {
        
        if(!dir.exists(input$plpProtocolDownload)){
          dir.create(input$plpProtocolDownload, recursive = T)
        }
        
        rmarkdown::render(
          input = system.file(
            'patient-level-prediction-document', 
            "export-main.Rmd", 
            package = "OhdsiShinyModules"
          ),  
          intermediates_dir = file.path(tempdir(), 'plp-prot'),
          output_dir = file.path(input$plpProtocolDownload, paste0('plp_report',designSummary$reportId())), 
          params = list( #TODO UPDATE DOC
            connectionHandler = connectionHandler,
            resultSchema = resultDatabaseSettings$schema,
            myTableAppend = resultDatabaseSettings$plpTablePrefix,
            databaseTableAppend = resultDatabaseSettings$databaseTablePrefix,
            cohortTableAppend = resultDatabaseSettings$cgTablePrefix,
            modelDesignIds = designSummary$reportId()
          )
        )
      }
      )
      
      # ===========================================
      #  Single Result Exploring Modules
      # ===========================================
      tracker <- shiny::reactiveVal(1)
      shiny::observeEvent(
        input$allView,
        {
          if(input$allView != 'Explore Selected Model'){
            tracker(tracker() + 1)
          }
        }
        )

      patientLevelPredictionResultsServer(
        id = 'prediction-results',
        modelDesignId = modelDesignId, 
        developmentDatabaseId = developmentDatabaseId ,
        performanceId = performanceId,
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings,
        performance = performance,
        tracker = tracker
        )
      
    }
  )
}



