# @file prediction-main.R
#
# Copyright 2021 Observational Health Data Sciences and Informatics
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
predictionHelperFile <- function(){
  fileLoc <- system.file('prediction-www', "prediction.html", package = "OhdsiShinyModules")
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
predictionViewer <- function(id=1) {
  ns <- shiny::NS(id)
  
  shiny::tabsetPanel(
    id = ns('allView'),
    
    shiny::tabPanel(
      "Model Designs Summary",  
      predictionDesignSummaryViewer(ns('designSummaryTab'))
    ),
    
    shiny::tabPanel(
      "Models Summary",  
      predictionModelSummaryViewer(ns('modelSummaryTab'))
    ),
    
    shiny::tabPanel(
      "Explore Selected Model",
      
      shiny::tabsetPanel(
        id = ns('singleView'),
        shiny::tabPanel(
          "Design Settings",
          predictionSettingsViewer(ns('settings'))
        ),
        
        shiny::tabPanel(
          "Model",
          predictionCovariateSummaryViewer(ns('covariateSummary'))
        ),
        
        shiny::tabPanel(
          "Threshold Dependant", 
          predictionCutoffViewer(ns('cutoff'))
        ), 
        
        shiny::tabPanel(
          "Discrimination",  
          predictionDiscriminationViewer(ns('discrimination'))
        ),
        
        shiny::tabPanel(
          "Calibration", 
          predictionCalibrationViewer(ns('calibration'))
        ),
        
        shiny::tabPanel(
          "Net Benefit", 
          predictionNbViewer(ns('netBenefit'))
        ),
        
        shiny::tabPanel(
          "Validation",
          predictionValidationViewer(ns('validation'))
        )
        
      )
    )
    
  )
  
}

#' The module server for exploring PatientLevelPrediction
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param resultDatabaseSettings a list containing the prediction result schema and connection details
#' 
#' @return
#' The server for the PatientLevelPrediction module
#'
#' @export
predictionServer <- function(
  id, 
  resultDatabaseSettings = list(port = 1)
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # =============================
      #   CONNECTION
      # =============================
      if(F){
        if(resultDatabaseSettings$port != ""){
          ParallelLogger::logInfo('Port')
          ParallelLogger::logInfo(paste(resultDatabaseSettings$port))
          con <- pool::dbPool(drv = DatabaseConnector::DatabaseConnectorDriver(),
                              dbms = resultDatabaseSettings$dbms,
                              server = resultDatabaseSettings$server,
                              user = resultDatabaseSettings$user,
                              password = resultDatabaseSettings$password,
                              port = resultDatabaseSettings$port)
          
        } else{
          ParallelLogger::logInfo('No Port')
          con <- pool::dbPool(drv = DatabaseConnector::DatabaseConnectorDriver(),
                              dbms = resultDatabaseSettings$dbms,
                              server = resultDatabaseSettings$server,
                              user = resultDatabaseSettings$user,
                              password = resultDatabaseSettings$password
          )
          
        }
      }
      
      # old connection 
      connectionDetails <- DatabaseConnector::createConnectionDetails(
        dbms = resultDatabaseSettings$dbms,
        server = resultDatabaseSettings$server,
        user = resultDatabaseSettings$user,
        password = resultDatabaseSettings$password,
        port = resultDatabaseSettings$port
        #pathToDriver =  '/Users/jreps/Documents/drivers'
      )

      # Get connection handler
      if (getOption("useNonPooledConnection", default = FALSE)) {
        connectionHandler <- ResultModelManager::ConnectionHandler$new(connectionDetails)
      } else {
        connectionHandler <- ResultModelManager::PooledConnectionHandler$new(connectionDetails)
      }

      con <- connectionHandler$con
      
      shiny::onStop(function() {
        connectionHandler$finalize()
      })
      
      # =============================
      #   VIEW SETTINGS
      # =============================
      # initially hide the models and selected model
      shiny::hideTab(inputId = "allView", session = session, target = "Models Summary")
      shiny::hideTab(inputId = "allView", session = session, target = "Explore Selected Model")
      
      # when going to the all model design hide tabs
      shiny::observeEvent(input$allView, {
        if(input$allView == 'Model Designs Summary'){
          shiny::hideTab(inputId = "allView", session = session, target = "Models Summary")
          shiny::hideTab(inputId = "allView", session = session, target = "Explore Selected Model")
        }
      })
      
      
      # keep a reactive variable tracking the active tab
      singleViewValue <- shiny::reactive({
        input$singleView
      })
      
      
      # =============================
      #   MODEL DESIGNS
      # =============================
      
      # the first server is the design server
      # this shows the number of developed models
      # per prediction model design and lets the user
      # select a model design id to explore
      modelDesignId <- shiny::reactiveVal()
      designSummary <- predictionDesignSummaryServer(
        id = 'designSummaryTab',
        con = con, 
        mySchema = resultDatabaseSettings$schema, 
        targetDialect = resultDatabaseSettings$dbms,
        myTableAppend = resultDatabaseSettings$tablePrefix
      )
      
      # change to model summary tab when 
      # a model design id is select that shows 
      # a summary table with all the models 
      # developed using the chosen model design
      shiny::observeEvent(designSummary$modelDesignId(), {
        modelDesignId(designSummary$modelDesignId())
        if(!is.null(designSummary$modelDesignId())){
          shiny::showTab(inputId = "allView", session = session, target = "Models Summary")
          shiny::updateTabsetPanel(session, "allView", selected = "Models Summary")
          shiny::hideTab(inputId = "allView", session = session, target = "Explore Selected Model")
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
      performance <- predictionModelSummaryServer(
          id = 'modelSummaryTab', 
          con = con, 
          mySchema = resultDatabaseSettings$schema, 
          targetDialect = resultDatabaseSettings$dbms,
          myTableAppend = resultDatabaseSettings$tablePrefix,
          modelDesignId = modelDesignId
        )

      
      # change to single model explore tab when 
      # a performance id is selected that shows 
      # the internal valdiation and external
      # validation details of the selected model
      shiny::observeEvent(performance$performanceId(), {
        performanceId(performance$performanceId())
        developmentDatabaseId(performance$developmentDatabaseId())
        if(!is.null(performance$performanceId())){
          shiny::showTab(inputId = "allView", session = session, target = "Explore Selected Model")
          shiny::updateTabsetPanel(session, "allView", selected = "Explore Selected Model")
          shiny::hideTab(inputId = "allView", session = session, target = "Models Summary")
        }
      })
      
      
      # =============================
      #   DIAGNOSTICS
      # =============================
      # diagnostic viewer - show model diagnostic results
      shiny::observeEvent(designSummary$diagnosticId(), {
        shiny::showModal(shiny::modalDialog(
          title = "Diagnostic",
          predictionDiagnosticsViewer(session$ns('diagnostics'))
        ))
      })
      predictionDiagnosticsServer(
        id = 'diagnostics', 
        modelDesignId = designSummary$diagnosticId, 
        mySchema = resultDatabaseSettings$schema, 
        con = con,
        myTableAppend = resultDatabaseSettings$tablePrefix, 
        targetDialect = resultDatabaseSettings$dbms
      )
      
      # =============================
      #   PROTOCOL
      # =============================
      # protocol viewer - show protocol 
      shiny::observeEvent(designSummary$reportId(), {
        
        if(!is.null(designSummary$reportId())){
          
              createPredictionProtocol(
                con = con, 
                mySchema = resultDatabaseSettings$schema, 
                targetDialect = resultDatabaseSettings$dbms,
                myTableAppend = resultDatabaseSettings$tablePrefix,
                modelDesignId = designSummary$reportId(),
                output = tempdir()
              )
             
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
              shiny::includeHTML(file.path(tempdir(), 'main.html'))
            ), 
            size = "l",
            easyClose = T
          ))
        }
        
      })
      
      # add code to save protocol to file based on input$downloadButton
      shiny::observeEvent(input$downloadButton, {
        
        if(!dir.exists(input$plpProtocolDownload)){
          dir.create(input$plpProtocolDownload, recursive = T)
        }
        
        rmarkdown::render(
          input = system.file(
            'prediction-document', 
            "export-main.Rmd", 
            package = "OhdsiShinyModules"
          ), 
          output_dir = file.path(input$plpProtocolDownload, paste0('plp_report',designSummary$reportId())), 
          params = list(
            connection = con, 
            resultSchema = resultDatabaseSettings$schema, 
            targetDialect = resultDatabaseSettings$dbms,
            myTableAppend = resultDatabaseSettings$tablePrefix,
            modelDesignIds = designSummary$reportId()
          )
        )
      }
      )
      
      # ===========================================
      #  Single Result Exploring Modules
      # ===========================================
      
      predictionCovariateSummaryServer(
        id = 'covariateSummary',
        modelDesignId = modelDesignId, # reactive
        developmentDatabaseId = developmentDatabaseId, # reactive
        performanceId = performanceId, # reactive
        mySchema = resultDatabaseSettings$schema, 
        connnectionHandler = connectionHandler,
        inputSingleView = singleViewValue,
        myTableAppend = resultDatabaseSettings$tablePrefix, 
        targetDialect = resultDatabaseSettings$dbms
      ) 
      
      predictionSettingsServer(
        id = 'settings', 
        modelDesignId = modelDesignId, # reactive
        developmentDatabaseId = developmentDatabaseId, # reactive
        performanceId = performanceId, # reactive
        mySchema = resultDatabaseSettings$schema, 
        con = con,
        inputSingleView = singleViewValue,
        myTableAppend = resultDatabaseSettings$tablePrefix, 
        targetDialect = resultDatabaseSettings$dbms
      )
      
      predictionCutoffServer(
        id = 'cutoff', 
        performanceId = performanceId, 
        mySchema = resultDatabaseSettings$schema, 
        con = con,
        inputSingleView = singleViewValue,
        myTableAppend = resultDatabaseSettings$tablePrefix, 
        targetDialect = resultDatabaseSettings$dbms
      )
      
      predictionDiscriminationServer(
        id = 'discrimination', 
        performanceId = performanceId, 
        mySchema = resultDatabaseSettings$schema, 
        con = con,
        inputSingleView = singleViewValue,
        myTableAppend = resultDatabaseSettings$tablePrefix, 
        targetDialect = resultDatabaseSettings$dbms
      )
      
      predictionCalibrationServer(
        id = 'calibration', 
        performanceId = performanceId, 
        mySchema = resultDatabaseSettings$schema, 
        con = con,
        inputSingleView = singleViewValue,
        myTableAppend = resultDatabaseSettings$tablePrefix, 
        targetDialect = resultDatabaseSettings$dbms
      ) 
      
      predictionNbServer(
        id = 'netBenefit', 
        performanceId = performanceId, 
        mySchema = resultDatabaseSettings$schema, 
        con = con,
        inputSingleView = singleViewValue,
        myTableAppend = resultDatabaseSettings$tablePrefix, 
        targetDialect = resultDatabaseSettings$dbms
      ) 
      
      predictionValidationServer(
        id = 'validation', 
        modelDesignId = modelDesignId, # reactive
        developmentDatabaseId = developmentDatabaseId, # reactive
        performanceId = performanceId, # reactive
        con = con, 
        inputSingleView = singleViewValue,
        mySchema = resultDatabaseSettings$schema,
        myTableAppend = resultDatabaseSettings$tablePrefix, 
        targetDialect = resultDatabaseSettings$dbms
      ) 
      
    }
  )
}
